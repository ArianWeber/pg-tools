model :TestTransitions do

    transient error :Err1
    transient error :Err2
    transient error :Err3
    transient error :Err4
    transient error :Err5
    
    graph :OhNo do
        states :Ok, :Bad, init: :Ok
        transition :Ok => :Bad do
            guard "(Err1 == Yes && Err2 == Yes && Err3 == Yes ) || (Err4 == Yes && Err5 == Yes)"
        end
    end

    # Expected Cut Sets: { Err1, Err2, Err3 } { Err4, Err5 }
    hazard "Bad" => :"OhNo == Bad"

end
