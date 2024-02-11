model :Test do

    persistent error :BreakFailure

    graph :Car do
        states :driving
        var distance_to_wall: (0..10), init: 9

        transition :driving => :driving do
            precon "distance_to_wall > 0"
            guard  "distance_to_wall > 5 || BreakFailure == Yes"
            action "distance_to_wall := distance_to_wall - 1"
        end

    end

    # Expected Cut Sets: { BreakFailure }
    hazard "The car crashes" => :"distance_to_wall == 0"

    specify "The car" do
        assuming "the breaks don't fail" => :"G BreakFailure = No" do
            it "does not crash" => :"G distance_to_wall > 0"
        end
    end
end
