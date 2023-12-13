import Buffer "mo:base/Buffer";
import Text "mo:base/Text";
import Http "http";
import Logo "Logo";

actor {

    let logo : Text = Logo.getLogo();

    // name
    let name : Text = "GardenVille";
    public query func getName() : async Text {
        return name;
    };

    // manifesto
    var manifesto : Text = "The Gardening Community of the Future";
    public query func getManifesto() : async Text {
        return manifesto;
    };
    public func setManifesto(value : Text) : async () {
        manifesto := value;
    };

    // goals
    var goals = Buffer.Buffer<Text>(100);
    public func addGoal(goal : Text) : async () {
        goals.add(goal);
    };
    public query func getGoals() : async [Text] {
        Buffer.toArray(goals);
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

    func _getWebpage() : Text {        var webpage = "<style>" #
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
            headers = [("Content-Type", "text/html; charset=UTF-8")];
            status_code = 200 : Nat16;
            streaming_strategy = null;
        };
        return (response);
    };
};
