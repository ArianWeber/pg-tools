model :TestAction do

    graph :Car do
        var distance_to_wall: (0..10), init: 0

        states :DrivingRelentlessly

        transition :DrivingRelentlessly => :DrivingRelentlessly do
            precon "distance_to_wall > 0"
            action "distance_to_wall := distance_to_wall - 1"
        end
    end
    graph :CrashDetector do
        states :no_crash, :crash, init: :no_crash

        transition :no_crash => :crash do
            guard "distance_to_wall == 0"
        end

    end

    specify "The car" do
        it "starts out driving" => :"CrashDetector == no_crash && Car == DrivingRelentlessly"
        it "crashes horribly" => :"F CrashDetector == crash"
    end
end
