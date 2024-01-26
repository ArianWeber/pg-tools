model :TestAction do
    graph :Test do
        var a: (0..10), init: 0
        var b: (0..10), init: 10

        states :initial, :second ,init: :initial
        transition :initial => :second do
            guard "b > 5 && a + 1 == 1 && a != b && Other == second"
        end
    end
    graph :Other do
        states :initial, :second, init: :initial
        transition :initial => :second
    end
    specify "The Test" do
        it "does reach the second state" => :"X X Test == second"
    end
end
