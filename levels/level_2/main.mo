import Result "mo:base/Result";
import HashMap "mo:base/HashMap";
import Principal "mo:base/Principal";
import Array "mo:base/Array";
import Buffer "mo:base/Buffer";
import TrieMap "mo:base/TrieMap";

actor {

    type Member = {
        name : Text;
        age : Nat;
    };

    type Result<A,B> = Result.Result<A,B>;
    type HashMap<K,V> = HashMap.HashMap<K,V>; 
    
    var members : HashMap<Principal, Member> = HashMap.HashMap<Principal, Member>
                                               (100, Principal.equal, Principal.hash);

    public shared(msg) func addMember(member: Member) : async Result<(), Text> {
        let caller = msg.caller;
        switch (members.get(caller)) {
            case null {
                members.put(caller, member);
                return #ok(());
            };
            case _ {
                return #err("Caller is already a member");
            };
        };
    };

    public shared query func getMember(principal : Principal) : async Result<Member, Text> {
        switch (members.get(principal)) {
            case (?member) {
                return #ok(member);
            };
            case null {
                return #err("No member found for the given principal");
            };
        };
    };

    public shared(msg) func updateMember(member : Member) : async Result<(), Text> {
        let caller = msg.caller;
        switch (members.get(caller)) {
            case (?_) {
                members.put(caller, member);
                return #ok(());
            };
            case null {
                return #err("Caller is not a member");
            };
        };
    };

    public shared query func getAllMembers() : async [Member] {
        let res = Buffer.Buffer<Member>(members.size());
        for (m in members.vals()) {
            res.add(m);
        };
        return Buffer.toArray(res);
    };

    public shared query func numberOfMembers(): async Nat {
        return members.size();
    };

    public shared(msg) func removeMember(principal: Principal) : async Result<(), Text> {
        switch (members.get(msg.caller)) {
            case (?_) {
                ignore members.remove(msg.caller);
                return #ok(());
            };
            case _ {
                return #err("Caller is not a member");
            };
        };
    };
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
    

    // public query func getLedger() : async [(Text, Text)] {
    //     var res : [(Text, Text)] = [];
    //     for ((key, value) in ledger.entries()) {
    //         let entry : (Text, Text) = (Principal.toText(key.owner), Nat.toText(value));
    //         res := Array.append<((Text, Text))>(res, [entry]); 
    //     };
    //     return res;
    // };

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

    public func transfer(from: Account, to: Account, amount: Nat) : async Result<(), Text> {
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
