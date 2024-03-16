model :TestTransitions do
    graph :Test do
        states :one, :two, init: :one
        transition :one => :two
    end
    specify "The state" do
        it "starts in one" => :"Test == one"
        it "transitions to two (LTL)" => :"X Test == two"
        it "transitions to two (CTL)" => :"AX Test == two"
    end
end
