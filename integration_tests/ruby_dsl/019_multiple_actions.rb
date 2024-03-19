model :MultipleActions do

    graph :GraphWithMultipleActions do
        states :one, :two, init: :one

        var a: (0..10), init: 0
        var b: (0..10), init: 0
        var c: (0..10), init: 0

        transition :one => :two do
           action "a := 1 + 1 | b := 0 | c := 25 - (2 * 10)"
        end
    end

    specify "The graph" do
        it "set a = 2" => :"X a == 2"
        it "set b = 0" => :"X b == 0"
        it "set c = 5" => :"X c == 5"
    end

end
