import Result "mo:base/Result";
import HashMap "mo:base/HashMap";
import Principal "mo:base/Principal";
import Array "mo:base/Array";
import Buffer "mo:base/Buffer";
import TrieMap "mo:base/TrieMap";
import Account "account";

actor {
    public type Subaccount = Blob;
    public type Account = {
        owner : Principal;
        subaccount : ?Subaccount;
    };

    let ledger = TrieMap.TrieMap<Account, Nat>(Account.accountsEqual, Account.accountsHash);

    public type Token = {
        name : Text;
        symbol : Text;
    };

    let token : Token = { name = "GardenVilleToken"; symbol = "GVT" };
    let total_supply : Nat = 100_000_000;

    public query func symbol() : async Text {
        return token.symbol;
    };

    public query func totalSupply() : async Nat {
        return total_supply;
    };
    
    public query func name() : async Text {
        return token.name;
    };

    public func mint(principal : Principal, amount : Nat) : async () {
        let account = {
            owner = principal;
            subaccount = null;
        };
        let currentBalance = switch (ledger.get(account)) {
            case (?balance) balance;
            case null 0;
        };
        ledger.put(account, currentBalance + amount);
    };

    public query func balanceOf(account : Account): async Nat {
        switch (ledger.get(account)) {
            case (?balance) balance;
            case null 0;
        };
    };

    public func transfer(from: Account, to: Account, amount: Nat) : async Result.Result<(), Text> {
        let fromBalance = switch(ledger.get(from)) {
            case (?balance) balance;
            case null 0;
        };
        let toBalance = switch(ledger.get(to)) {
            case (?balance) balance;
            case null 0;
        };
        if (fromBalance < amount) {
            return #err("Caller does not have enough tokens in its main account");
        };
        ledger.put(from, fromBalance - amount);
        ledger.put(to, toBalance + amount);
        return #ok(());
    };

};
