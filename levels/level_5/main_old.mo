import Buffer "mo:base/Buffer";
import Text "mo:base/Text";
import Result "mo:base/Result";
import HashMap "mo:base/HashMap";
import Principal "mo:base/Principal";
import Array "mo:base/Array";
import TrieMap "mo:base/TrieMap";
import Hash "mo:base/Hash";
import Nat32 "mo:base/Nat32";
import Nat "mo:base/Nat";
import Iter "mo:base/Iter";
import Account "account";
import Http "http";
import Logo "Logo";
actor {

    let logo : Text = Logo.getLogo();

    // Define a name for you DAO
    let name : Text = "GardenVille";

    // Define a manifesto for you DAO
    var manifesto : Text = "The Gardening Community of the Future";

    // get name
    public query func getName() : async Text {
        return name;
    };

    // get manifesto
    public query func getManifesto() : async Text {
        return manifesto;
    };

    // set manifesto
    public func setManifesto(value : Text) : async () {
        manifesto := value;
    };
    // list of goals
    var goals = Buffer.Buffer<Text>(100);

    // add goal
    public func addGoal(goal : Text) : async () {
        goals.add(goal);
    };

    public query func getGoals() : async [Text] {
        Buffer.toArray(goals);
    };

    type Member = {
        name : Text;
        age : Nat;
    };

    type Result<A, B> = Result.Result<A, B>;
    type HashMap<K, V> = HashMap.HashMap<K, V>;

    var members : HashMap<Principal, Member> = HashMap.HashMap<Principal, Member>(100, Principal.equal, Principal.hash);

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

    public query func balanceOf(account : Account) : async Nat {
        switch (ledger.get(account)) {
            case (?balance) balance;
            case null 0;
        };
    };

    public func transfer(from : Account, to : Account, amount : Nat) : async Result<(), Text> {
        let fromBalance = await balanceOf(from);
        let toBalance = await balanceOf(to);
        if (fromBalance < amount) {
            return #err("Caller does not have enough tokens in its main account");
        };
        ledger.put(from, fromBalance - amount);
        ledger.put(to, toBalance + amount);
        return #ok(());
    };

    public type Status = {
        #Open;
        #Accepted;
        #Rejected;
    };

    private func hashNat(num : Nat) : Hash.Hash {
        var hash : Hash.Hash = 5381;
        var n : Nat32 = Nat32.fromNat(num);
        while (n > 0) {
            let c : Nat32 = n % 256;
            hash := Nat32.bitshiftLeft(hash, 5) +% hash +% c;
            n := Nat32.div(n, 256);
        };
        hash;
    };

    public type Proposal = {
        id : Nat;
        status : Status;
        manifest : Text;
        votes : Int;
        voters : [Principal];
    };

    var nextProposalId : Nat = 0;
    let proposalCost : Nat = 1;
    let proposals = TrieMap.TrieMap<Nat, Proposal>(Nat.equal, hashNat);

    public type createProposalOk = {
        #ProposalCreated : Nat;
    };

    public type createProposalErr = {
        #NotDAOMember;
        #NotEnoughTokens;
    };

    public type voteOk = {
        #ProposalAccepted;
        #ProposalRefused;
        #ProposalOpen;
    };

    public type voteErr = {
        #ProposalNotFound;
        #AlreadyVoted;
        #ProposalEnded;
    };

    public query func getProposal(id : Nat) : async ?Proposal {
        switch (proposals.get(id)) {
            case (?proposal) {
                return ?proposal;
            };
            case null {
                return null;
            };
        };
    };

    public query func getProposals() : async [(Nat, Proposal)] {
        return Iter.toArray(proposals.entries());
    };

    public type voteResult = Result.Result<voteOk, voteErr>;

    public type createProposalResult = Result.Result<createProposalOk, createProposalErr>;

    public shared ({ caller }) func createProposal(manifest : Text) : async createProposalResult {
        let balance = ledger.get({ owner = caller; subaccount = null });
        switch (balance) {
            case (?num) {
                if (num < proposalCost) {
                    return #err(#NotEnoughTokens);
                } else {
                    let proposal = {
                        id = nextProposalId;
                        status = #Open;
                        manifest = manifest;
                        votes = 0;
                        voters = [];
                    };
                    proposals.put(proposal.id, proposal);
                    ledger.put({ owner = caller; subaccount = null }, num - proposalCost);
                    nextProposalId += 1;
                    return #ok(#ProposalCreated(proposal.id));
                };
            };
            case null {
                return #err(#NotDAOMember);
            };
        };
    };

    public shared ({ caller }) func vote(id : Nat, vote : Bool) : async voteResult {
        switch (proposals.get(id)) {
            case null {
                return #err(#ProposalNotFound);
            };
            case (?proposal) {
                let voted = switch (Array.find<Principal>(proposal.voters, func(x) { x == caller })) {
                    case null { false };
                    case _ { true };
                };
                if voted return #err(#AlreadyVoted);
                if (proposal.status != #Open) return #err(#ProposalEnded);
                let power = await balanceOf({
                    owner = caller;
                    subaccount = null;
                });
                var votes = proposal.votes;
                if (vote) {
                    votes += power;
                } else votes -= power;
                let v = Array.append<Principal>(proposal.voters, [caller]);

                if (votes >= 100) {
                    let p = {
                        id = nextProposalId;
                        status = #Accepted;
                        manifest = proposal.manifest;
                        votes = votes;
                        voters = v;
                    };
                    proposals.put(proposal.id, p);
                    return #ok(#ProposalAccepted);
                };
                if (votes <= -100) {
                    let p = {
                        id = proposal.id;
                        status = #Rejected;
                        manifest = proposal.manifest;
                        votes = votes;
                        voters = v;
                    };
                    proposals.put(proposal.id, p);
                    return #ok(#ProposalRefused);
                };
                let p = {
                    id = proposal.id;
                    status = #Open;
                    manifest = proposal.manifest;
                    votes = votes;
                    voters = v;
                };
                proposals.put(proposal.id, p);
                return #ok(#ProposalOpen);
            };
        };
    };

    public shared query ({ caller }) func whoami() : async Principal {
        return caller;
    };

    type DAOInfo = {
        name : Text;
        manifesto : Text;
        goals : [Text];
        logo : Text;
    };

    public func getStats() : async DAOInfo {
        let info : DAOInfo = {
            name = await getName();
            goals = await getGoals();
            manifesto = await getManifesto();
            logo = logo;
        };
        return info;
    };

    func _getWebpage() : Text {
        var webpage = "<style>" #
        "body { text-align: center; font-family: Arial, sans-serif; background-color: #f0f8ff; color: #333; }" #
        "h1 { font-size: 3em; margin-bottom: 10px; }" #
        "hr { margin-top: 20px; margin-bottom: 20px; }" #
        "em { font-style: italic; display: block; margin-bottom: 20px; }" #
        "ul { list-style-type: none; padding: 0; }" #
        "li { margin: 10px 0; }" #
        "li:before { content: '👉 '; }" #
        "svg { max-width: 150px; height: auto; display: block; margin: 20px auto; }" #
        "h2 { text-decoration: underline; }" #
        "</style>";

        webpage := webpage # "<div><h1>" # name # "</h1></div>";
        webpage := webpage # "<em>" # manifesto # "</em>";
        webpage := webpage # "<div>" # logo # "</div>";
        webpage := webpage # "<hr>";
        webpage := webpage # "<h2>Our goals:</h2>";
        webpage := webpage # "<ul>";
        for (goal in goals.vals()) {
            webpage := webpage # "<li>" # goal # "</li>";
        };
        webpage := webpage # "</ul>";
        return webpage;
    };

    public query func http_request(request : Http.Request) : async Http.Response {
        
        let page = _getWebpage();
        let response = {
            body = Text.encodeUtf8(page);
            // body = Text.encodeUtf8("Hello world");
            headers = [("Content-Type", "text/html; charset=UTF-8")];
            status_code = 200 : Nat16;
            streaming_strategy = null;
        };
        return (response);
    };
};
