model :TestTransitions do
    graph :Test do
        states :initial
    end
    specify "The state" do
        it "starts in initial" => :"Test == initial"
        it "stays in initial"  => :"G Test == initial"
        it "never leaves initial" => :"! F Test != initial"
    end
end
