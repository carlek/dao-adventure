import Result "mo:base/Result";
import Hash "mo:base/Hash";
import TrieMap "mo:base/TrieMap";
import Nat "mo:base/Nat";
import Nat32 "mo:base/Nat32";
import Iter "mo:base/Iter";
import Token "token"; 
actor {

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
        let balance = Token.ledger.get({ owner = caller; subaccount = null });
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
                    Token.ledger.put({ owner = caller; subaccount = null }, num - proposalCost);
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
};
