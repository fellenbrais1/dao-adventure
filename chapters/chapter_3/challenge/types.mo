import Result "mo:base/Result";
import HashMap "mo:base/HashMap";
module {
    //////////////////
    /// PROJECT #3 ///
    //////////////////
    public type Result<Ok, Err> = Result.Result<Ok, Err>;
    public type HashMap<Ok, Err> = HashMap.HashMap<Ok, Err>;
};