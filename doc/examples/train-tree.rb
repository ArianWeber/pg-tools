
MAX_POS = 20
TREE_POS = 13

transient error :BreakFailure
transient error :SensorFailure

component :Train do
    states :driving

    var :position => (0..MAX_POS), init: 0

    transition :driving => :driving do
        guard "Breaks == idle"
        action "position := position + 1"
        precon "position < #{MAX_POS}"
    end
end

component :TreeSensor do
    states :idle, :signal
    transition :idle => :signal do
        guard "position >= #{TREE_POS - 3} && SensorFailure == no"
    end
end

component :Breaks do
    states :idle, :active
    transition :idle => :active do
        guard "TreeSensor == signal && BreakFailure == no"
    end
end

specify "The train" do
    it "reaches the tree" => :"F position == #{TREE_POS - 1}"

    assuming "there are no errors" => :"G BreakFailure == no && G SensorFailure == no" do
        it "stops before the tree" => :"G position < #{TREE_POS}"
    end
end

hazard "The train runs into a tree" => :"position == #{TREE_POS}"
hazard "The train does not reach its destination" => :"F position == #{MAX_POS}"
