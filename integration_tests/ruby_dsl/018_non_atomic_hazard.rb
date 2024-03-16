model :NonAtomicHazard do

    persistent error :KeysForgotten

    graph :Person do
        states :inside, :outside, init: :inside
        transition :inside => :outside
        transition :outside => :inside do
            guard "KeysForgotten == No"
        end
    end

    # This does not work as at any point the fault could not have occurred
    # (KeysForgotten == No) already but then it happens at a later point
    # Expected Cut Sets: { :"" }
    # hazard "The person can't get in" => :"G Person == outside"
end
