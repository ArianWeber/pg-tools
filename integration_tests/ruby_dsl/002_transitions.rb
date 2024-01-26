model :TestTransitions do
    graph :Test do
        states :one, :two, init: :one
        transition :one => :two
    end
    specify "The state" do
        it "starts in one" => :"Test == one"
        it "transitions to two" => :"X Test == two"
    end
end
