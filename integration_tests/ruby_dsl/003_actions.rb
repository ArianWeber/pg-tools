model :TestAction do
    graph :Test do
        var position: (0..100), init: 0
        states :go
        transition :go => :go do
            guard "position < 100"
            action "position := position + 1"
        end
    end
    specify "The position" do
        it "does not start at the limit" => :"! G position == 100"
        it "reaches the limit" => :"F position == 100"
    end
end
