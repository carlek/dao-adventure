import Buffer "mo:base/Buffer";
import Text "mo:base/Text";
actor  {

    // Define a name for you DAO
    let name: Text = "GardenVille";

    // Define a manifesto for you DAO
    var manifesto: Text = "The Gardening Community of the Future";

    // get name
    public query func getName(): async Text {
        return name;
    };

    // get manifesto
    public query func getManifesto(): async Text {
        return manifesto;
    };

    // set manifesto
    public func setManifesto(value: Text): async (){
        manifesto := value;
    };
    // list of goals
    var goals = Buffer.Buffer<Text>(100);

    // add goal
    public func addGoal(goal: Text): async () {
        goals.add(goal);
    };

    public query func getGoals(): async [Text] {
        Buffer.toArray(goals);
    };
};
