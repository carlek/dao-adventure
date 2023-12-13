import Account "account";
import TrieMap "mo:base/TrieMap";
import Result "mo:base/Result";

actor {

    type Result<A, B> = Result.Result<A, B>;
    type Account = Account.Account;

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

    public func mint(principal : Principal, amount : Nat) : async () {
        // ... (rest of the mint function)
    };

    public query func balanceOf(account : Account.Account) : async Nat {
        switch (ledger.get(account)) {
            case (?balance) balance;
            case null 0;
        };
    };

    public func transfer(from : Account.Account, to : Account.Account, amount : Nat) : async Result<(), Text> {        
        let fromBalance = await balanceOf(from);
        let toBalance = await balanceOf(to);
        if (fromBalance < amount) {
            return #err("Caller does not have enough tokens in its main account");
        };
        ledger.put(from, fromBalance - amount);
        ledger.put(to, toBalance + amount);
        return #ok(());
    };
};
