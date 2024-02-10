model :Test do

    graph :Graph1 do
        var counter: (0..5), init: 0
        states :counting
        transition :counting => :counting do
            precon "counter < 5"
            action "counter := counter + 1"
        end
    end

    graph :Graph2 do
        states :idle
    end

    specify "The counter" do
        it "Goes to 0" => :"counter == 0"
        it "Goes to 1" => :"X counter == 1"
        it "Goes to 2" => :"X X counter == 2"
        it "Goes to 3" => :"X X X counter == 3"
        it "Goes to 4" => :"X X X X counter == 4"
        it "Goes to 5" => :"X X X X X counter == 5"
        it "Stays at 5" => :"X X X X X X X X counter == 5"
        it "reaches 5" => :"F counter == 5"
    end
end
