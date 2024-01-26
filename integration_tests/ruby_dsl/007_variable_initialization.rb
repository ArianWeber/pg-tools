model :TestAction do
    graph :Test do
        state :idle
        var a: (0..1), init: 0
        var b: (0..1), init: "b == 0"
        var c: (-1..2), init: "c > 0 && c < 2"
        var d: (0..10), init: "d == c + c + c"

        var u: (0..10)
        init "u == 2" 

        var x: (0..10), init: "x >= 5 && x <= 7"
        var y: (0..10), init: "y = x + 1"
        var z: (0..1)
    end
    specify "The variable" do
        it "a is initialized to 0" => :"a == 0"
        it "b is initialized to 0" => :"b == 0"
        it "c is initialized to 1" => :"c == 1"
        it "d is initialized to 3" => :"d == 3"

        it "u is initialized to 2" => :"u == 2"

        it "x is initialized to either 5, 6 or 7" => :"x == 5 || x == 6 || x == 7"
        it "y is initialized to either 6, 7 or 8" => :"y == 6 || y == 7 || y == 8"
        it "z is either 0 or 1" => :"z == 0 || z == 1"
    end
end
