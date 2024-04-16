model :Robot do

    persistent error :Errorgraph

    graph :Robot do
        var num: (0..7), init: 0
        states :idle, :busy, :inactive, init: :idle
        transition :idle => :busy do
            precon "(num + 1) >= 0 && (num + 1) <= 7"
            guard "LightBarrier == signal"
            action "num := num + 1"
        end
        transition :busy => :idle do
            guard "num < 3"
        end
        transition :busy => :inactive do
            guard "num >= 3"
        end
        transition :busy => :busy
        
        transition :inactive => :idle do 
            guard "Errorgraph != No"
        end
    end

    graph :LightBarrier do 
        states :idle, :signal
        transition :idle => :idle 
        transition :idle => :signal 
        transition :signal => :idle 
        transition :signal => :signal
    end

    # Expected Cut Sets: { Errorgraph }
    hazard "num ist zu gross" => :"num == 5"
end
