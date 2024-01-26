model :TestAction do
    graph :Engine do
        states :running, :stopped
    end
    graph :Car do
        states :accelerating, :decelerating

        transition :accelerating => :decelerating do
            guard "Engine == stopped"
        end
        transition :decelerating => :decelerating do
            guard "Engine == stopped"
        end
        transition :accelerating => :accelerating do
            guard "Engine == running"
        end
        transition :decelerating => :accelerating do
            guard "Engine == running"
        end

    end
    specify "The car" do
        it "accelerates when the engine is running" => :"G (Engine == running => X Car == accelerating)"
        it "decelerates when the engine isn't running" => :"G (Engine != running => X Car == decelerating)"
    end
end
