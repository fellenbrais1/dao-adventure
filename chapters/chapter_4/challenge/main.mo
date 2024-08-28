import Result "mo:base/Result";
import Buffer "mo:base/Buffer";
import HashMap "mo:base/HashMap";
import Iter "mo:base/Iter";
import Principal "mo:base/Principal";
import Option "mo:base/Option";
import Nat64 "mo:base/Nat64";
import Debug "mo:base/Debug";
import Time "mo:base/Time";
import Array "mo:base/Array";

import Types "types";
actor {
    // For this level we need to make use of the code implemented in the previous projects.
    // The voting system will make use of previous data structures and functions.
    
    //////////////////
    ///   TYPES    ///
    //////////////////
    
    type Member = Types.Member;
    type Result<Ok, Err> = Types.Result<Ok, Err>;
    type HashMap<K, V> = Types.HashMap<K, V>;
    type Proposal = Types.Proposal;
    type ProposalContent = Types.ProposalContent;
    type ProposalId = Types.ProposalId;
    type Vote = Types.Vote;

    //////////////////
    /// PROJECT #1 ///
    //////////////////
    
    let goals = Buffer.Buffer<Text>(0);
    let name = "Motoko Bootcamp";
    var manifesto = "Empower the next generation of builders and make the DAO-revolution a reality";

    public shared query func getName() : async Text {
        return name;
    };

    public shared query func getManifesto() : async Text {
        return manifesto;
    };

    public func setManifesto(newManifesto : Text) : async () {
        manifesto := newManifesto;
        return;
    };

    public func addGoal(newGoal : Text) : async () {
        goals.add(newGoal);
        return;
    };

    public shared query func getGoals() : async [Text] {
        Buffer.toArray(goals);
    };

    //////////////////
    /// PROJECT #2 ///
    //////////////////

    let members = HashMap.HashMap<Principal, Member>(0, Principal.equal, Principal.hash);

    public shared ({ caller }) func addMember(member : Member) : async Result<(), Text> {
        switch (members.get(caller)) {
            case (null) {
                members.put(caller, member);
                return #ok();
            };
            case (?member) {
                return #err("Member already exists");
            };
        };
    };

    public shared ({ caller }) func updateMember(member : Member) : async Result<(), Text> {
        switch (members.get(caller)) {
            case (null) {
                return #err("Member does not exist");
            };
            case (?member) {
                members.put(caller, member);
                return #ok();
            };
        };
    };

    public shared ({ caller }) func removeMember() : async Result<(), Text> {
        switch (members.get(caller)) {
            case (null) {
                return #err("Member does not exist");
            };
            case (?member) {
                members.delete(caller);
                return #ok();
            };
        };
    };

    public query func getMember(p : Principal) : async Result<Member, Text> {
        switch (members.get(p)) {
            case (null) {
                return #err("Member does not exist");
            };
            case (?member) {
                return #ok(member);
            };
        };
    };

    public query func getAllMembers() : async [Member] {
        return Iter.toArray(members.vals());
    };

    public query func numberOfMembers() : async Nat {
        return members.size();
    };

    //////////////////
    /// PROJECT #3 ///
    //////////////////

    let ledger = HashMap.HashMap<Principal, Nat>(0, Principal.equal, Principal.hash);

    public query func tokenName() : async Text {
        return "Motoko Bootcamp Token";
    };

    public query func tokenSymbol() : async Text {
        return "MBT";
    };

    public func mint(owner : Principal, amount : Nat) : async Result<(), Text> {
        let balance = Option.get(ledger.get(owner), 0);
        ledger.put(owner, balance + amount);
        return #ok();
    };

    public func burn(owner : Principal, amount : Nat) : async Result<(), Text> {
        let balance = Option.get(ledger.get(owner), 0);
        if (balance < amount) {
            return #err("Insufficient balance to burn");
        };
        ledger.put(owner, balance - amount);
        return #ok();
    };

    public shared ({ caller }) func transfer(from : Principal, to : Principal, amount : Nat) : async Result<(), Text> {
        let balanceFrom = Option.get(ledger.get(from), 0);
        let balanceTo = Option.get(ledger.get(to), 0);
        if (balanceFrom < amount) {
            return #err("Insufficient balance to transfer");
        };
        ledger.put(from, balanceFrom - amount);
        ledger.put(to, balanceTo + amount);
        return #ok();
    };

    public query func balanceOf(owner : Principal) : async Nat {
        return (Option.get(ledger.get(owner), 0));
    };

    public query func totalSupply() : async Nat {
        var total = 0;
        for (balance in ledger.vals()) {
            total += balance;
        };
        return total;
    };

    //////////////////
    /// PROJECT #4 ///
    //////////////////

    stable var nextProposalId : Nat64 = 0;
    let proposals = HashMap.HashMap<ProposalId, Proposal>(0, Nat64.equal, Nat64.toNat32);

    public shared ({ caller }) func createProposal(proposalContent : ProposalContent) : async Result<ProposalId, Text> {
        let value = await balanceOf(caller);
        if (value == 0) {
            return #err("You cannot create a proposal without a balance.");
        } else {
            let burnResult = await burn(caller, 1);
            switch (burnResult) {
                case(#ok()) {
                    var newProposal : Proposal = {
                        id = nextProposalId;
                        content = proposalContent;
                        creator = caller;
                        created = Time.now();
                        executed = null;
                        votes = [];
                        voteScore = 0;
                        status = #Open;
                    };
                    let newProposalId = newProposal.id;
                    proposals.put(newProposalId, newProposal);
                    nextProposalId += 1;
                    return #ok(newProposal.id);
                };
                case(#err(_)) {
                    return #err("Something went wrong with the burning process.");
                };
            };
        };
    };

    public query func getProposal(proposalId : ProposalId) : async ?Proposal {
        switch (proposals.get(proposalId)) {
            case (null) {
                Debug.print(debug_show ("No proposal found with that id number"));
                return null;
            };
            case (?match) {
                return proposals.get(proposalId);
            };
        };
    };

    public shared ({ caller }) func voteProposal(proposalId : ProposalId, vote : Bool) : async Result<(), Text> {
        let value = await balanceOf(caller);
        if (value == 0) {
            return #err("You cannot vote on a proposal without a balance.");
        } else {
            let proposalExists = proposals.get(proposalId);
            switch (proposalExists) {
                case(null) {
                    return #err("No proposal with that id has been submitted.")
                };
                case(?match) {
                    let newVote : Vote = {
                        member = caller;
                        votingPower = value;
                        yesOrNo = vote;
                    };
                    let data : ?Proposal = proposals.get(proposalId);
                    switch (data) {
                        case(null) {
                            return #err("No data.");
                        };
                        case(?foundProposal) {
                            let copyProposal = {
                                id : Nat64 = foundProposal.id;
                                content : ProposalContent = foundProposal.content;
                                creator : Principal = foundProposal.creator;
                                created : Time.Time = foundProposal.created;
                                var executed : ?Time.Time = foundProposal.executed;
                                var votes : [Vote] = foundProposal.votes;
                                var voteScore : Int = foundProposal.voteScore;
                                var status = foundProposal.status;
                            };
                            copyProposal.votes := Array.append(foundProposal.votes, [newVote]);
                            if (newVote.yesOrNo == true) {
                                copyProposal.voteScore += newVote.votingPower;
                            } else {
                                copyProposal.voteScore -= newVote.votingPower;
                            };
                            if (copyProposal.voteScore <= -100) {
                                Debug.print(debug_show("Proposal fails, archiving proposal."));
                                copyProposal.status := #Rejected;
                                copyProposal.executed := null;
                                let writeProposal = {
                                    id : Nat64 = copyProposal.id;
                                    content : ProposalContent = copyProposal.content;
                                    creator : Principal = copyProposal.creator;
                                    created : Time.Time = copyProposal.created;
                                    executed : ?Time.Time = copyProposal.executed;
                                    votes : [Vote] = copyProposal.votes;
                                    voteScore : Int = copyProposal.voteScore;
                                    status = copyProposal.status;
                                };
                                proposals.delete(proposalId);
                                proposals.put(proposalId, writeProposal);
                                return #err("Proposal fails.");
                            } else if (copyProposal.voteScore >= 100) {
                                Debug.print(debug_show("Proposal passes, enacting now."));
                                copyProposal.status := #Accepted;
                                copyProposal.executed := ?Time.now();
                                switch (copyProposal.content) {
                                    case(#ChangeManifesto(text)) {
                                        await setManifesto(text);
                                    };
                                    case(#AddGoal(text)) {
                                        await addGoal(text);
                                    };
                                };
                                let writeProposal = {
                                    id : Nat64 = copyProposal.id;
                                    content : ProposalContent = copyProposal.content;
                                    creator : Principal = copyProposal.creator;
                                    created : Time.Time = copyProposal.created;
                                    executed : ?Time.Time = copyProposal.executed;
                                    votes : [Vote] = copyProposal.votes;
                                    voteScore : Int = copyProposal.voteScore;
                                    status = copyProposal.status;
                                };
                                proposals.delete(proposalId);
                                proposals.put(proposalId, writeProposal);
                                return #ok();
                            } else {
                                Debug.print(debug_show("Proposal still open for voting."));
                                let writeProposal = {
                                    id : Nat64 = copyProposal.id;
                                    content : ProposalContent = copyProposal.content;
                                    creator : Principal = copyProposal.creator;
                                    created : Time.Time = copyProposal.created;
                                    executed : ?Time.Time = copyProposal.executed;
                                    votes : [Vote] = copyProposal.votes;
                                    voteScore : Int = copyProposal.voteScore;
                                    status = copyProposal.status;
                                };
                                proposals.delete(proposalId);
                                proposals.put(proposalId, writeProposal);
                                return #ok;
                            };
                        };
                    };
                };
            };
        };
    };

    public query func getAllProposals() : async [Proposal] {
        return Iter.toArray(proposals.vals());
    };
};