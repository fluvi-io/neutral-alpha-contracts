/**
 * This provides an example for sending locked coins to recipients to be unlocked after a specific time.
 *
 * Locked coins flow:
 * 1. Deploy the lockup contract. Deployer can decide if the contract is upgradable or not.
 * 2. Sponsor accounts add locked APTs for custom expiration time + amount for recipients.
 * 3. Sponsor accounts can revoke a lock or change lockup (reduce or extend) anytime. This gives flexibility in case of
 * contract violation or special circumstances. If this is not desired, the deployer can remove these functionalities
 * before deploying.
 * 4. Once the lockup has expired, the recipient can call claim to get the unlocked tokens.
 **/
module alpha::neutral_alpha {
    use aptos_framework::account;
    use aptos_framework::resource_account;
    use aptos_framework::coin::{Self, Coin};
    use aptos_framework::event::{Self, EventHandle};
    use aptos_framework::timestamp;
    use aptos_std::table::{Self, Table};
    use aptos_std::type_info;
    use aptos_std::comparator;
    use std::error;
    use std::signer;
    use std::vector;
    use std::string;
    use pancake::swap::LPToken;
    use pancake_masterchef::masterchef;
    use pancake_oft::oft::{CakeOFT as Cake};
    use celer::celer_coin_manager::UsdcCoin;


    const EQUAL: u8 = 0;
    const SMALLER: u8 = 1;
    const GREATER: u8 = 2;

    const PRECISION: u128 = 1000000000000;

    struct LPData<phantom S1, phantom S2> has key {
        acc_cake_per_lp_fractional: u128,
        total_lp: u64,
    }

    struct PoolData<phantom S1, phantom S2, phantom V1, phantom V2> has key {
        epoch_snapshots: vector<EpochSnapshot>,
        last_epoch_time_sec: u64,
        epoch_interval_sec: u64,
        stable_balance: u64,
        stable_to_deposit: u64,
        stable_to_withdraw: u64,
        variable_balance: u64,
        variable_to_deposit: u64,
        variable_to_withdraw: u64,
        threshold: u128,
        apr_share: u128,
    }

    struct EpochSnapshot has store {
        stable_lp_change_fractional: u128,
        stable_lp_per_variable_lp: u128,
        last_stable_acc_cake_per_lp: u128,
        last_variable_acc_cake_per_lp: u128,
        variable_balance: u64,
        stable_balance: u64,
        v1_amount: u64,
        v2_amount: u64,
        coverage_fraction: u128,
        threshold: u128,
        apr_share: u128,
    }
    
    struct StablePair {}
    struct VariablePair {}

    struct ModuleData has key {
        resource_signer_cap: account::SignerCapability,
        admin: address,
    }

    struct UserData<phantom S1, phantom S2, phantom V1, phantom V2, phantom Side> has key {
        deposit_acc_cake_per_lp: u128,
        deposit_epoch: u64,
        withdraw_epoch: u64,
        amount: u64,
        claimed_cake: u64,
        claimed_lp: u64,
        deposit_events: EventHandle<DepositEvent>,
        withdraw_events: EventHandle<WithdrawEvent>,
        claim_events: EventHandle<ClaimEvent>,
    }
    
    struct DepositEvent has drop, store {
        epoch: u64,
        amount: u64,
    }
    
    struct WithdrawEvent has drop, store {
        epoch: u64,
        amount: u64,
    }

    struct ClaimEvent has drop, store {
        epoch: u64,
        amount: u64,
    }

    const THRESHOLD_BP: u64 = 9500;

    const EPOOL_DOES_NOT_EXIST: u64 = 1;
    const EALREADY_DEPOSITED: u64 = 2;
    const EZERO_AMOUNT: u64 = 3;
    const EPOOL_EPOCH_NOT_ENDED: u64 = 4;
    const EALREADY_WITHDRAWING: u64 = 5;

    const ENOT_ADMIN: u64 = 4;


    fun get_token_info<T>(): vector<u8> {
        let type_name = type_info::type_name<T>();
        *string::bytes(&type_name)
    }

    fun equals<X, Y>(): bool {
        comparator::is_equal(&comparator::compare_u8_vector(get_token_info<X>(), get_token_info<Y>()))
    }
    
    public entry fun remove_stable<S1, S2, V1, V2>(depositor: &signer) acquires UserData, PoolData, LPData {
        let (cake, lpbal, zer) = pending_balances<S1, S2, V1, V2, StablePair>(signer::address_of(depositor));

        let user_data = borrow_global_mut<UserData<S1, S2, V1, V2, StablePair>>(signer::address_of(depositor));
        assert!(exists<PoolData<S1, S2, V1, V2>>(@alpha), error::invalid_argument(EPOOL_DOES_NOT_EXIST));
        let pool_data = borrow_global_mut<PoolData<S1, S2, V1, V2>>(@alpha);
        assert!(user_data.amount > 0 && user_data.withdraw_epoch == 0, EALREADY_WITHDRAWING);

        pool_data.stable_to_withdraw = pool_data.stable_to_withdraw + lpbal;
        user_data.withdraw_epoch = vector::length(&pool_data.epoch_snapshots);
    }
        
    public entry fun remove_variable<S1, S2, V1, V2>(depositor: &signer) acquires UserData, PoolData {
        let user_data = borrow_global_mut<UserData<S1, S2, V1, V2, VariablePair>>(signer::address_of(depositor));
        assert!(exists<PoolData<S1, S2, V1, V2>>(@alpha), error::invalid_argument(EPOOL_DOES_NOT_EXIST));
        let pool_data = borrow_global_mut<PoolData<S1, S2, V1, V2>>(@alpha);
        assert!(user_data.amount > 0 && user_data.withdraw_epoch == 0, EALREADY_WITHDRAWING);

        pool_data.variable_to_withdraw = pool_data.variable_to_withdraw + user_data.amount;
        user_data.withdraw_epoch = vector::length(&pool_data.epoch_snapshots);
    }
        
    public entry fun add_stable<S1, S2, V1, V2>(depositor: &signer, amount: u64) acquires ModuleData, UserData, PoolData, LPData {
        let depositor_address = signer::address_of(depositor);
        if (!exists<UserData<S1, S2, V1, V2, StablePair>>(depositor_address)) {
            move_to<UserData<S1, S2, V1, V2, StablePair>>(depositor, UserData {
                deposit_acc_cake_per_lp: 0,
                deposit_epoch: 0,
                withdraw_epoch: 0,
                amount: 0,
                claimed_cake: 0,
                claimed_lp: 0,
                deposit_events: account::new_event_handle<DepositEvent>(depositor),
                withdraw_events: account::new_event_handle<WithdrawEvent>(depositor),
                claim_events: account::new_event_handle<ClaimEvent>(depositor),
            });
        };

        harvest<S1, S2>();
        
        let module_data = borrow_global_mut<ModuleData>(@alpha);
        let alpha_signer = account::create_signer_with_capability(&module_data.resource_signer_cap);
        let user_data = borrow_global_mut<UserData<S1, S2, V1, V2, StablePair>>(depositor_address);
        let lp_data = borrow_global_mut<LPData<S1, S2>>(@alpha);
        
        assert!(exists<PoolData<S1, S2, V1, V2>>(@alpha), error::invalid_argument(EPOOL_DOES_NOT_EXIST));
        let pool_data = borrow_global_mut<PoolData<S1, S2, V1, V2>>(@alpha);
        

        assert!(user_data.amount == 0, error::invalid_state(EALREADY_DEPOSITED));
        assert!(amount > 0, error::invalid_argument(EZERO_AMOUNT));

        coin::transfer<LPToken<S1, S2>>(depositor, @alpha, amount);
        

        masterchef::deposit<LPToken<S1, S2>>(&alpha_signer, amount);

        lp_data.total_lp = lp_data.total_lp + amount;

        pool_data.stable_to_deposit = pool_data.stable_to_deposit + amount;
        
        user_data.deposit_epoch = vector::length(&pool_data.epoch_snapshots);
        user_data.amount = amount;
        user_data.withdraw_epoch = 0;
        user_data.claimed_cake = 0;
        user_data.deposit_acc_cake_per_lp = lp_data.acc_cake_per_lp_fractional;
        
        event::emit_event(&mut user_data.deposit_events, DepositEvent {
            epoch: user_data.deposit_epoch,
            amount: amount,
        });
    }
    
    public entry fun add_variable<S1, S2, V1, V2>(depositor: &signer, amount: u64) acquires ModuleData, UserData, PoolData, LPData {
        let depositor_address = signer::address_of(depositor);
        if (!exists<UserData<S1, S2, V1, V2, VariablePair>>(depositor_address)) {
            move_to<UserData<S1, S2, V1, V2, VariablePair>>(depositor, UserData {
                deposit_acc_cake_per_lp: 0,
                deposit_epoch: 0,
                withdraw_epoch: 0,
                amount: 0,
                claimed_cake: 0,
                claimed_lp: 0,
                deposit_events: account::new_event_handle<DepositEvent>(depositor),
                withdraw_events: account::new_event_handle<WithdrawEvent>(depositor),
                claim_events: account::new_event_handle<ClaimEvent>(depositor),
            });
        };

        harvest<V1, V2>();

        let module_data = borrow_global_mut<ModuleData>(@alpha);
        let alpha_signer = account::create_signer_with_capability(&module_data.resource_signer_cap);
        let user_data = borrow_global_mut<UserData<S1, S2, V1, V2, VariablePair>>(depositor_address);
        let lp_data = borrow_global_mut<LPData<V1, V2>>(@alpha);
        
        assert!(exists<PoolData<S1, S2, V1, V2>>(@alpha), error::invalid_argument(EPOOL_DOES_NOT_EXIST));
        let pool_data = borrow_global_mut<PoolData<S1, S2, V1, V2>>(@alpha);
        

        assert!(user_data.amount == 0, error::invalid_state(EALREADY_DEPOSITED));
        assert!(amount > 0, error::invalid_argument(EZERO_AMOUNT));

        coin::transfer<LPToken<V1, V2>>(depositor, @alpha, amount);
        

        masterchef::deposit<LPToken<V1, V2>>(&alpha_signer, amount);

        lp_data.total_lp = lp_data.total_lp + amount;

        pool_data.variable_to_deposit = pool_data.variable_to_deposit + amount;
        
        user_data.deposit_epoch = vector::length(&pool_data.epoch_snapshots);
        user_data.amount = amount;
        user_data.withdraw_epoch = 0;
        user_data.claimed_cake = 0;
        user_data.claimed_lp = 0;
        user_data.deposit_acc_cake_per_lp = lp_data.acc_cake_per_lp_fractional;
        
        event::emit_event(&mut user_data.deposit_events, DepositEvent {
            epoch: user_data.deposit_epoch,
            amount: amount,
        });
    }
    
    /*
    aptos move run --function-id 0x9215fc192b61d9156a591c0e7471933653200068fd88c7e746db5086558a648d::neutral_alpha::add_pool<0x1::aptos_coin::AptosCoin,0xf22bede237a07e121b56d91a491eb7bcdfd1f5907926a9e58338f964a01b17fa::asset::USDC> --args str

    aptos move run --function-id 0xf363d09ccbfc63c0e3ca15fd2dee351dcffb2dbdf73c7dc200217691a4896d00::neutral_alpha::add_pool --type-args 0xf22bede237a07e121b56d91a491eb7bcdfd1f5907926a9e58338f964a01b17fa::asset::USDC 0xf22bede237a07e121b56d91a491eb7bcdfd1f5907926a9e58338f964a01b17fa::asset::USDT 0x1::aptos_coin::AptosCoin 0xf22bede237a07e121b56d91a491eb7bcdfd1f5907926a9e58338f964a01b17fa::asset::USDC --args u64:10 u128:0 u128:500000000000

    aptos move run --function-id 0x46b753416df325de5d81a26370249159792d783360a5f3e3731ace72ee14150e::neutral_alpha::add_stable --type-args 0xf22bede237a07e121b56d91a491eb7bcdfd1f5907926a9e58338f964a01b17fa::asset::USDC 0xf22bede237a07e121b56d91a491eb7bcdfd1f5907926a9e58338f964a01b17fa::asset::USDT 0x1::aptos_coin::AptosCoin 0xf22bede237a07e121b56d91a491eb7bcdfd1f5907926a9e58338f964a01b17fa::asset::USDC --args u64:100000

    aptos move run --function-id 0xf363d09ccbfc63c0e3ca15fd2dee351dcffb2dbdf73c7dc200217691a4896d00::neutral_alpha::tick --type-args 0xf22bede237a07e121b56d91a491eb7bcdfd1f5907926a9e58338f964a01b17fa::asset::USDC 0xf22bede237a07e121b56d91a491eb7bcdfd1f5907926a9e58338f964a01b17fa::asset::USDT 0x1::aptos_coin::AptosCoin 0xf22bede237a07e121b56d91a491eb7bcdfd1f5907926a9e58338f964a01b17fa::asset::USDC

    aptos move run --function-id 0x46b753416df325de5d81a26370249159792d783360a5f3e3731ace72ee14150e::neutral_alpha::remove_stable --type-args 0xf22bede237a07e121b56d91a491eb7bcdfd1f5907926a9e58338f964a01b17fa::asset::USDC 0xf22bede237a07e121b56d91a491eb7bcdfd1f5907926a9e58338f964a01b17fa::asset::USDT 0x1::aptos_coin::AptosCoin 0xf22bede237a07e121b56d91a491eb7bcdfd1f5907926a9e58338f964a01b17fa::asset::USDC

    aptos move run --function-id 0x46b753416df325de5d81a26370249159792d783360a5f3e3731ace72ee14150e::neutral_alpha::remove_variable --type-args 0xf22bede237a07e121b56d91a491eb7bcdfd1f5907926a9e58338f964a01b17fa::asset::USDC 0xf22bede237a07e121b56d91a491eb7bcdfd1f5907926a9e58338f964a01b17fa::asset::USDT 0x1::aptos_coin::AptosCoin 0xf22bede237a07e121b56d91a491eb7bcdfd1f5907926a9e58338f964a01b17fa::asset::USDC

    aptos move run --function-id 0x46b753416df325de5d81a26370249159792d783360a5f3e3731ace72ee14150e::neutral_alpha::reap_stable --type-args 0xf22bede237a07e121b56d91a491eb7bcdfd1f5907926a9e58338f964a01b17fa::asset::USDC 0xf22bede237a07e121b56d91a491eb7bcdfd1f5907926a9e58338f964a01b17fa::asset::USDT 0x1::aptos_coin::AptosCoin 0xf22bede237a07e121b56d91a491eb7bcdfd1f5907926a9e58338f964a01b17fa::asset::USDC

    aptos move run --function-id 0x46b753416df325de5d81a26370249159792d783360a5f3e3731ace72ee14150e::neutral_alpha::reap_variable --type-args 0xf22bede237a07e121b56d91a491eb7bcdfd1f5907926a9e58338f964a01b17fa::asset::USDC 0xf22bede237a07e121b56d91a491eb7bcdfd1f5907926a9e58338f964a01b17fa::asset::USDT 0x1::aptos_coin::AptosCoin 0xf22bede237a07e121b56d91a491eb7bcdfd1f5907926a9e58338f964a01b17fa::asset::USDC

    aptos move run --function-id 0x46b753416df325de5d81a26370249159792d783360a5f3e3731ace72ee14150e::neutral_alpha::add_variable --type-args 0xf22bede237a07e121b56d91a491eb7bcdfd1f5907926a9e58338f964a01b17fa::asset::USDC 0xf22bede237a07e121b56d91a491eb7bcdfd1f5907926a9e58338f964a01b17fa::asset::USDT 0x1::aptos_coin::AptosCoin 0xf22bede237a07e121b56d91a491eb7bcdfd1f5907926a9e58338f964a01b17fa::asset::USDC --args u64:50000
    */
    public entry fun add_pool<S1, S2, V1, V2>(sender: &signer, epoch_interval_sec: u64, threshold: u128, apr_share: u128) acquires ModuleData {
        
        let module_data = borrow_global_mut<ModuleData>(@alpha);
        assert!(signer::address_of(sender) == module_data.admin, ENOT_ADMIN);
        let alpha_signer = account::create_signer_with_capability(&module_data.resource_signer_cap);
        coin::register<LPToken<S1, S2>>(&alpha_signer);
        coin::register<LPToken<V1, V2>>(&alpha_signer);
        if (!exists<LPData<S1, S2>>(@alpha)) {
            move_to<LPData<S1, S2>>(&alpha_signer, LPData {
                acc_cake_per_lp_fractional: 0,
                total_lp: 0,
            });
        };
        if (!exists<LPData<V1, V2>>(@alpha)) {
            move_to<LPData<V1, V2>>(&alpha_signer, LPData {
                acc_cake_per_lp_fractional: 0,
                total_lp: 0,
            });
        };
        
        move_to<PoolData<S1, S2, V1, V2>>(&alpha_signer, PoolData {
            epoch_snapshots: vector::singleton(EpochSnapshot{
                stable_lp_change_fractional: 0,
                stable_lp_per_variable_lp: 0,
                last_stable_acc_cake_per_lp: 0,
                last_variable_acc_cake_per_lp: 0,
                v1_amount: 0,
                v2_amount: 0,
                variable_balance: 0,
                stable_balance: 0,
                coverage_fraction: 0,
                threshold: 0,
                apr_share: 0,
            }),
            last_epoch_time_sec: 0,
            epoch_interval_sec: epoch_interval_sec,
            stable_balance: 0,
            stable_to_deposit: 0,
            stable_to_withdraw: 0,
            variable_balance: 0,
            variable_to_deposit: 0,
            variable_to_withdraw: 0,
            threshold: threshold,
            apr_share: apr_share,
        });
    }
    public fun lp_value<T1, T2>(amount: u64): u64 {
        let (pt1, pt2) = (price_fractional<T1>(), price_fractional<T2>());
        let (t1, t2) = get_lp_contents<T1, T2>(amount);
        ((pt1 * (t1 as u128) / PRECISION
                               +   pt2 * (t2 as u128) / PRECISION) as u64)
    
    }
    public fun round_info<S1, S2, V1, V2>(): (u64, u64, u128, u128) acquires PoolData {
        let pool_data = borrow_global<PoolData<S1, S2, V1, V2>>(@alpha);
        let last_snapshot = vector::borrow(&pool_data.epoch_snapshots, vector::length(&pool_data.epoch_snapshots) - 1);

        (lp_value<S1, S2>(last_snapshot.stable_balance), lp_value<V1, V2>(last_snapshot.variable_balance), last_snapshot.coverage_fraction * last_snapshot.apr_share / PRECISION, last_snapshot.coverage_fraction)
    }

    public fun pool_info<S1, S2, V1, V2>(): (u64, u64, u128, u128) acquires PoolData {
        let pool_data = borrow_global<PoolData<S1, S2, V1, V2>>(@alpha);
        (pool_data.last_epoch_time_sec, pool_data.epoch_interval_sec, pool_data.threshold, pool_data.apr_share)
    }

    fun harvest<S1, S2>(): u128 acquires ModuleData, LPData {
        let module_data = borrow_global_mut<ModuleData>(@alpha);
        let alpha_signer = account::create_signer_with_capability(&module_data.resource_signer_cap);
        let lp_data = borrow_global_mut<LPData<S1, S2>>(@alpha);
        let cake_before = coin::balance<Cake>(@alpha);        
        masterchef::deposit<LPToken<S1, S2>>(&alpha_signer, 0);
        let cake_after = coin::balance<Cake>(@alpha);
        if (lp_data.total_lp > 0) {
            lp_data.acc_cake_per_lp_fractional = lp_data.acc_cake_per_lp_fractional + (((cake_after - cake_before) as u128) * PRECISION) / (lp_data.total_lp as u128);
        };
        lp_data.acc_cake_per_lp_fractional
    }
    
    fun price_fractional<T>(): u128  {
        if (equals<T, UsdcCoin>()) {
            PRECISION
        }
        else {
            let usdc;
            let t;
            if (pancake::swap_utils::sort_token_type<T, UsdcCoin>()) {
                (t, usdc) = pancake::swap::token_balances<T, UsdcCoin>();
            }
            else {
                (usdc, t) = pancake::swap::token_balances<UsdcCoin, T>();
            };
            (usdc as u128) * PRECISION / (t as u128)
        }
    }
    public fun pending_balances<S1, S2, V1, V2, Side>(acc: address): (u64, u64, u64) acquires PoolData, UserData, LPData {
        let user_data = borrow_global<UserData<S1, S2, V1, V2, Side>>(acc);
        let lp_data = borrow_global<LPData<V1, V2>>(@alpha);
        
        assert!(exists<PoolData<S1, S2, V1, V2>>(@alpha), error::invalid_argument(EPOOL_DOES_NOT_EXIST));
        let pool_data = borrow_global_mut<PoolData<S1, S2, V1, V2>>(@alpha);
        
        pending_cake<S1, S2, V1, V2, Side>(pool_data, user_data)
    }
    fun pending_cake<S1, S2, V1, V2, Side>(pool_data: &PoolData<S1, S2, V1, V2>, user_data: &UserData<S1, S2, V1, V2, Side>): (u64, u64, u64) acquires LPData {
        let stable_lp_data = borrow_global<LPData<S1, S2>>(@alpha);
        let variable_lp_data = borrow_global<LPData<V1, V2>>(@alpha);
        let epoch = user_data.deposit_epoch;
        let amount = (user_data.amount as u128);
        let total = 0u128;
        let total_lp = 0u128;
        let len = vector::length(&pool_data.epoch_snapshots);
        if (amount == 0) return (0, 0, 0);
        if (epoch >= len) {
            if (equals<Side, StablePair>()) {
                return ((((stable_lp_data.acc_cake_per_lp_fractional - user_data.deposit_acc_cake_per_lp) * amount / PRECISION) as u64), (amount as u64), 0);
            }
            else {
                return ((((variable_lp_data.acc_cake_per_lp_fractional - user_data.deposit_acc_cake_per_lp) * amount / PRECISION) as u64), (amount as u64), 0);
            }
        };

        let stable_acc = 0u128;
        let variable_acc = 0u128;
        let apr_share = 0u128;
        let coverage_fraction = 0u128;
        let variable_balance = 0u128;
        let stable_balance = 0u128;
        while (epoch < len) {
            let snapshot = vector::borrow(&pool_data.epoch_snapshots, epoch);
            if (epoch != user_data.deposit_epoch) {
                if (equals<Side, StablePair>()) {
                    total = total + (snapshot.last_stable_acc_cake_per_lp - stable_acc) * amount / PRECISION;

                    if (stable_balance > 0) {
                        total = total + (snapshot.last_variable_acc_cake_per_lp - variable_acc) * (variable_balance as u128) / PRECISION * apr_share / PRECISION * amount / (stable_balance as u128);
                        amount = amount * snapshot.stable_lp_change_fractional / PRECISION;
                    };
                }
                else {
                    total = total + (snapshot.last_variable_acc_cake_per_lp - variable_acc) * amount / PRECISION * (PRECISION - apr_share) / PRECISION;
                    total_lp = total_lp + snapshot.stable_lp_per_variable_lp * amount / PRECISION;
                }
            }
            else {
                if (equals<Side, StablePair>()) {
                    total = total + ((snapshot.last_stable_acc_cake_per_lp - user_data.deposit_acc_cake_per_lp) * amount / PRECISION);
                }
                else {
                    total = total + ((snapshot.last_variable_acc_cake_per_lp - user_data.deposit_acc_cake_per_lp) * amount / PRECISION);
                }
            };
            stable_acc = snapshot.last_stable_acc_cake_per_lp;
            variable_acc = snapshot.last_variable_acc_cake_per_lp;
            apr_share = snapshot.apr_share;
            coverage_fraction = snapshot.coverage_fraction;
            variable_balance = (snapshot.variable_balance as u128);
            stable_balance = (snapshot.stable_balance as u128);
            if (epoch == user_data.withdraw_epoch) break;
            epoch = epoch + 1;
        };

        if (epoch == user_data.withdraw_epoch && epoch < len) {
            if (equals<Side, StablePair>()) {
                total = total + ((stable_lp_data.acc_cake_per_lp_fractional - stable_acc) * amount / PRECISION);
            }
            else {
                total = total + ((variable_lp_data.acc_cake_per_lp_fractional - variable_acc) * amount / PRECISION);
            }
        }
        else {
            if (equals<Side, StablePair>()) {
                total = total + (stable_lp_data.acc_cake_per_lp_fractional - stable_acc) * amount / PRECISION;
                if (stable_balance > 0) {
                    total = total + (variable_lp_data.acc_cake_per_lp_fractional - variable_acc) * (variable_balance as u128) / PRECISION * apr_share / PRECISION * amount / (stable_balance as u128);
                };
            }
            else {
                total = total + (variable_lp_data.acc_cake_per_lp_fractional - variable_acc) * amount / PRECISION * (PRECISION - apr_share) / PRECISION;
            }
        };
        ((total as u64), (amount as u64), (total_lp as u64))
    }
    public entry fun reap_stable<S1, S2, V1, V2>(account: &signer) acquires ModuleData, LPData, UserData, PoolData {
        harvest<S1, S2>();
        let account_address = signer::address_of(account);
        let module_data = borrow_global_mut<ModuleData>(@alpha);
        let alpha_signer = account::create_signer_with_capability(&module_data.resource_signer_cap);
        let user_data = borrow_global_mut<UserData<S1, S2, V1, V2, StablePair>>(account_address);
        let pool_data = borrow_global_mut<PoolData<S1, S2, V1, V2>>(@alpha);
        
        let (cake, lp, unused) = pending_cake<S1, S2, V1, V2, StablePair>( pool_data, user_data);
        cake = cake - user_data.claimed_cake;

        let lp_data = borrow_global_mut<LPData<S1, S2>>(@alpha);
        if (cake > 0) {
            if (!coin::is_account_registered<Cake>(account_address)) {
                coin::register<Cake>(account);
            };
            coin::transfer<Cake>(&alpha_signer, account_address, cake);
            user_data.claimed_cake = user_data.claimed_cake + cake;
        };
        if (user_data.withdraw_epoch != 0 && vector::length(&pool_data.epoch_snapshots) > user_data.withdraw_epoch) {
            if (lp > 0) {
                masterchef::withdraw<LPToken<S1, S2>>(&alpha_signer, lp);
                coin::transfer<LPToken<S1, S2>>(&alpha_signer, account_address, lp);
                lp_data.total_lp = lp_data.total_lp - lp;
            };
            user_data.deposit_epoch = 0;
            user_data.withdraw_epoch = 0;
            user_data.amount = 0;
            user_data.claimed_cake = 0;
        };
    }

    public entry fun reap_variable<S1, S2, V1, V2>(account: &signer) acquires ModuleData, LPData, UserData, PoolData {
        harvest<V1, V2>();
        harvest<S1, S2>();
        let account_address = signer::address_of(account);
        let module_data = borrow_global_mut<ModuleData>(@alpha);
        let alpha_signer = account::create_signer_with_capability(&module_data.resource_signer_cap);
        let user_data = borrow_global_mut<UserData<S1, S2, V1, V2, VariablePair>>(account_address);
        let pool_data = borrow_global_mut<PoolData<S1, S2, V1, V2>>(@alpha);
        
        let (cake, lp, stablelp) = pending_cake<S1, S2, V1, V2, VariablePair>( pool_data, user_data);
        cake = cake - user_data.claimed_cake;
        stablelp = stablelp - user_data.claimed_lp;

        if (cake > 0) {
            let lp_data = borrow_global_mut<LPData<V1, V2>>(@alpha);
            if (!coin::is_account_registered<Cake>(account_address)) {
                coin::register<Cake>(account);
            };
            coin::transfer<Cake>(&alpha_signer, account_address, cake);
            user_data.claimed_cake = user_data.claimed_cake + cake;
        };
        if (stablelp > 0) {
            let stable_lp_data = borrow_global_mut<LPData<S1, S2>>(@alpha);
            masterchef::withdraw<LPToken<S1, S2>>(&alpha_signer, stablelp);
            coin::transfer<LPToken<S1, S2>>(&alpha_signer, account_address, stablelp);
            stable_lp_data.total_lp = stable_lp_data.total_lp - stablelp;
            user_data.claimed_lp = user_data.claimed_lp + stablelp;
        };
        if (user_data.withdraw_epoch != 0 && vector::length(&pool_data.epoch_snapshots) >= user_data.withdraw_epoch) {
            let lp_data = borrow_global_mut<LPData<V1, V2>>(@alpha);
            if (lp > 0) {
                masterchef::withdraw<LPToken<V1, V2>>(&alpha_signer, lp);
                coin::transfer<LPToken<V1, V2>>(&alpha_signer, account_address, lp);
                lp_data.total_lp = lp_data.total_lp - lp;
            };
            user_data.deposit_epoch = 0;
            user_data.withdraw_epoch = 0;
            user_data.amount = 0;
            user_data.claimed_cake = 0;
            user_data.claimed_lp = 0;
        };
    }


    fun get_lp_contents<T1, T2>(lp_amount: u64): (u64, u64) {
        let (t1, t2) = pancake::swap::token_balances<T1, T2>();
        let supply = pancake::swap::total_lp_supply<T1, T2>();
        ((((t1 as u128) * (lp_amount as u128) / supply) as u64), (((t2 as u128) * (lp_amount as u128) / supply) as u64))
    }

    public entry fun tick<S1, S2, V1, V2>() acquires PoolData, ModuleData, LPData {
        let stable_acc_cake_per_lp = harvest<S1, S2>();
        let variable_acc_cake_per_lp = harvest<V1, V2>();
        
        let module_data = borrow_global_mut<ModuleData>(@alpha);
        
        assert!(exists<PoolData<S1, S2, V1, V2>>(@alpha), error::invalid_argument(EPOOL_DOES_NOT_EXIST));
        let pool_data = borrow_global_mut<PoolData<S1, S2, V1, V2>>(@alpha);
        let now_secs = timestamp::now_seconds();
        let last_epoch = vector::borrow(&pool_data.epoch_snapshots, vector::length(&pool_data.epoch_snapshots) - 1);

        assert!(pool_data.epoch_interval_sec + pool_data.last_epoch_time_sec < now_secs, error::invalid_state(EPOOL_EPOCH_NOT_ENDED));
        
        let (pv1, pv2) = (price_fractional<V1>(), price_fractional<V2>());
        let (ps1, ps2) = (price_fractional<S1>(), price_fractional<S2>());
        let (v1, v2) = get_lp_contents<V1, V2>(pool_data.variable_balance);
        let (s1, s2) = get_lp_contents<S1, S2>(pool_data.stable_balance);
        let total_stable_value = ((ps1 * (s1 as u128) / PRECISION
                               +   ps2 * (s2 as u128) / PRECISION) as u64);

        let total_value = ((pv1 * (v1 as u128) / PRECISION
                        +   pv2 * (v2 as u128) / PRECISION) as u64);
        
        let virtual_value = ((pv1 * (last_epoch.v1_amount as u128) / PRECISION
                          +   pv2 * (last_epoch.v2_amount as u128) / PRECISION) as u64);
        
        let il = if (virtual_value < total_value) 0 else (virtual_value - total_value);
        
        let max_il = (((virtual_value as u128) * last_epoch.threshold / PRECISION) as u64);

        let coverage = if (il < max_il) 0 else ((((il - max_il) as u128) * last_epoch.coverage_fraction / PRECISION) as u64);

        let transfer_value = if (coverage > total_stable_value) total_stable_value else coverage;
        
        let transfer_lp = if (total_stable_value == 0) 0 else ((((pool_data.stable_balance as u128) * (transfer_value as u128)) / (total_stable_value as u128)) as u64);

        let stable_lp_change_fractional = if (total_stable_value == 0) 0 else ((((total_stable_value - transfer_value) as u128) * PRECISION / (total_stable_value as u128)));
        let stable_lp_per_variable_lp = if (pool_data.variable_balance == 0) 0 else (((transfer_lp as u128) * PRECISION / (pool_data.variable_balance as u128)));
        

        pool_data.stable_balance = (if (pool_data.stable_balance == 0) 0 else (((pool_data.stable_balance - transfer_lp) as u128) * (pool_data.stable_balance - pool_data.stable_to_withdraw as u128) / (pool_data.stable_balance as u128) as u64)) + pool_data.stable_to_deposit;
        pool_data.stable_to_deposit = 0;
        pool_data.stable_to_withdraw = 0;
        
        pool_data.variable_balance = pool_data.variable_balance + pool_data.variable_to_deposit - pool_data.variable_to_withdraw;
        pool_data.variable_to_deposit = 0;
        pool_data.variable_to_withdraw = 0;

        let (nv1, nv2) = get_lp_contents<V1, V2>(pool_data.variable_balance);
        let (ns1, ns2) = get_lp_contents<S1, S2>(pool_data.stable_balance);

        let new_total_stable_value = ((ps1 * (ns1 as u128) / PRECISION
                                   +   ps2 * (ns2 as u128) / PRECISION) as u64);
        let new_total_value = ((pv1 * (nv1 as u128) / PRECISION
                            +   pv2 * (nv2 as u128) / PRECISION) as u64);

        let new_coverage_fraction = if (new_total_value <= new_total_stable_value) PRECISION
                                    else (new_total_stable_value as u128) * PRECISION / (new_total_value as u128);
        
        pool_data.last_epoch_time_sec = now_secs;

        vector::push_back(&mut pool_data.epoch_snapshots, EpochSnapshot {
            stable_lp_change_fractional: stable_lp_change_fractional,
            stable_lp_per_variable_lp: stable_lp_per_variable_lp,
            last_stable_acc_cake_per_lp: stable_acc_cake_per_lp,
            last_variable_acc_cake_per_lp: variable_acc_cake_per_lp,
            v1_amount: nv1,
            v2_amount: nv2,
            variable_balance: pool_data.variable_balance,
            stable_balance: pool_data.stable_balance,
            coverage_fraction: new_coverage_fraction,
            threshold: pool_data.threshold,
            apr_share: pool_data.apr_share,
        });
    }
    fun init_module(account: &signer) {
        // store the capabilities within `ModuleData`
        let resource_signer_cap = resource_account::retrieve_resource_account_cap(account, @deployer);
        move_to(account, ModuleData {
            resource_signer_cap,
            admin: @deployer,
        });
        coin::register<Cake>(account);
    }
}
