import HashMap "mo:base/HashMap";
import Result "mo:base/Result";
import Principal "mo:base/Principal";
import Array "mo:base/Array";
import Nat "mo:base/Nat";
import Buffer "mo:base/Buffer";

actor {
    type HashMap<K, V> = HashMap.HashMap<K, V>;
    type Result<A, B> = Result.Result<A, B>;

    type Member = {
        name : Text;
        age : Nat;
    };

    let members : HashMap<Principal, Member> = HashMap.HashMap<Principal, Member>(100, Principal.equal, Principal.hash);

    public shared (msg) func addMember(member : Member) : async Result<(), Text> {
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

    public shared (msg) func updateMember(member : Member) : async Result<(), Text> {
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

    public shared query func numberOfMembers() : async Nat {
        return members.size();
    };

    public shared (msg) func removeMember(principal : Principal) : async Result<(), Text> {
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
};
