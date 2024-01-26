model :TestAction do
    graph :Test do
        var position: (0..100), init: 0
        states :go
        transition :go => :go do
            guard "position < 10"
            action "position := position + 1"
        end
        transition :go => :go do
            guard "position == 10"
        end
    end
    specify "The position" do
        it "does not start at the limit" => :"! G position == 100"
        it "does not reaches the limit" => :"! F position == 100"
        it "does stay at the threshold" => :"F G position == 10"
    end
end
