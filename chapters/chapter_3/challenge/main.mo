import Result "mo:base/Result";
import Principal "mo:base/Principal";
import HashMap "mo:base/HashMap";
import Option "mo:base/Option";
import Debug "mo:base/Debug";
import Nat "mo:base/Nat";

import Types "types";

actor {

    type Result<Ok, Err> = Types.Result<Ok, Err>;
    type HashMap<K, V> = Types.HashMap<K, V>;

    let currencyName : Text = "JesperCoin";
    let currencySymbol : Text = "JES";

    let ledger = HashMap.HashMap<Principal, Nat>(0, Principal.equal, Principal.hash);

    public query func tokenName() : async Text {
        return currencyName;
    };

    public query func tokenSymbol() : async Text {
        return currencySymbol;
    };

    public func mint(owner : Principal, amount : Nat) : async Result<(), Text> {
        let balanceOwner = Option.get(ledger.get(owner), 0);
        ledger.put(owner, balanceOwner + amount);
        Debug.print(debug_show ("You have minted : " # Nat.toText(amount), currencyName # " to : " # Principal.toText(owner) # "."));
        return #ok();
    };
    
    public func burn(owner : Principal, amount : Nat) : async Result<(), Text> {
        let balanceOwner = Option.get(ledger.get(owner), 0);
        if(amount > balanceOwner) {
            return #err("This principal's balance doesn't have enough tokens to do that.");
        } else {
            ledger.put(owner, balanceOwner - amount);
            Debug.print(debug_show ("You have burned : " # Nat.toText(amount) # " from : " # Principal.toText(owner) # "."));
            return #ok;
        };
    };

    public shared func transfer(from : Principal, to : Principal, amount : Nat) : async Result<(), Text> {
        if (from == to) {
            return #err("Principals provided are the same, no transfer can be performed in this case.")
        } else {
            let fromBalance = Option.get(ledger.get(from), 0);
            let toBalance = Option.get(ledger.get(to), 0);
            if (amount > fromBalance) {
                return #err("This principal's balance doesn't have enough tokens to do that.")
            } else {
                ledger.put(from, fromBalance - amount);
                ledger.put(to, toBalance + amount);
                return #ok();
            };
        };
    };

    public query func balanceOf(account : Principal) : async Nat {
        let balanceOwner = Option.get(ledger.get(account), 0);
        switch(ledger.get(account)) {
            case(null) {
                return 0;
            };
            case(?ledger) {
                return balanceOwner;
            };
        };
    };

    public query func totalSupply() : async Nat {
        var totalCount : Nat = 0;
        for(balance in ledger.vals()){
            totalCount := totalCount + balance;
        };
        return totalCount;
    };

};