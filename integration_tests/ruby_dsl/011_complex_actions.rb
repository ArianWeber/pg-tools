model :TestAction do
    graph :Test do
        var a: (0..11), init: 0
        var b: (0..100), init: 0

        states :go
        transition :go => :go do
            guard "a < 11"
            action "a := a + 1 | b := a * a"
        end
    end
    specify "The variable" do
        it "a reaches 10" => :"F a == 10"
        it "b reaches 100" => :"F b == 100"
    end
end
