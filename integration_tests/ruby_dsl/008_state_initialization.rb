model :TestAction do

    graph :GraphZero do
        states :a, :b, :c
    end
    graph :GraphOne do
        states :a, :b, :c
        init "GraphOne == a || GraphOne == b"
    end
    graph :GraphTwo do
        states :a, :b, :c, init: :a
    end

    specify "The graph" do
        it "GraphZero starts in a or b or c" => :"GraphOne == a || GraphOne == b || GraphOne == c"
        it "GraphOne starts in a or b"       => :"GraphOne == a || GraphOne == b"
        it "GraphTwo starts in a"            => :"GraphTwo == a"
    end
end
