####################################################################
# Model definition
####################################################################

model :RailroadCrossing do

    ####################################################################
    # Train Components
    ####################################################################

    # Train environment representing the actual physical train
    graph :EnvTrain do
        # velocity & position
    end

    # Position sensor of the train
    graph :SensorTrainPosition do
        states :idle, :send_close_msg, :send_is_closed_msg

    end

    graph :SensorTrainRadioModule do

    end

    # Actuator to 
    graph :ActTrainBrake do
        states :idle, :active
    end

    graph :CtrlTrain do
    end

    ####################################################################
    # Crossing Components
    ####################################################################

    graph :ActBarrierMotor do
       var motor_speed: [-1, 0, 1], init: 0

       states :idle, :opening, :closing


    end

    graph :CtrlCrossing do
    end








    graph :WingControl do
        
    end


end
