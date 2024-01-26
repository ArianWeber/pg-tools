model :TestAction do
    graph :Test do
        var position: (0..10), init: 0
        states :go
        transition :go => :go do
            guard "position < 10"
            action "position := position + 1"
        end
    end
    specify "The position" do
        it "starts at 0" => :"position == 0"
        it "goes to 1" => :"X position == 1"
        it "goes to 2" => :"X X position == 2"
        it "reaches 10" => :"F position == 10"
    end
end
