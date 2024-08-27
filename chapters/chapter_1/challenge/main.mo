import Buffer "mo:base/Buffer";

actor {

    let daoName : Text = "Jelespar DAO";
    var manifesto : Text = "To allow others to transition from dead-end jobs into blockchain development.";
    var goals = Buffer.Buffer<Text>(0);

    public shared query func getName() : async Text {
        return daoName;
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
};