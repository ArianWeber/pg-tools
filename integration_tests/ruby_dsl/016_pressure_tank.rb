model :PressureTank do

    transient  error :SwitchOperatedBadly
    persistent error :SensorDead
    # Relay is jammed if it does not open when wanted
    persistent error :Relay1Jammed
    persistent error :Relay2Jammed

    # S1
    graph :Switch do
        states :Start, :Closed, :Open, init: :Start
    
        # Randomly close at some point
        transition :Start => :Start
        transition :Start => :Closed

        # Open right after closing (unless error)
        transition :Closed => :Open do
            guard "SwitchOperatedBadly == No"
        end

        # Close again if error
        transition :Open => :Closed do
            guard "SwitchOperatedBadly == Yes"
        end
    end

    # T
    graph :Timer do
        states :Closed, :Open, :Countdown, init: :Closed

        var time: (0..60), init: 0

        countdown_requirement = "Sensor == Closed && ( Switch == Closed || Relay2 == Closed )"

        transition :Closed => :Countdown do
            guard countdown_requirement
        end
        transition :Countdown => :Countdown do
            guard "#{countdown_requirement} && time < 60"
            action "time := time + 1"
        end 

        transition :Countdown => :Open do
            guard "#{countdown_requirement} && time = 60"
            action "time := 0"
        end

        transition :Open => :Closed

    end

    # D
    graph :Tank do
        states :Functional, :Raptured, init: :Functional
        var pressure: (0..70), init: 0

        transition :Functional => :Functional do
            guard "Motor == On"
            action "pressure := pressure + 1"
        end
        transition :Functional => :Functional do
            guard "Motor == Off"
            action "pressure := 0"
        end
        transition :Functional => :Raptured do
            guard "Motor == On && pressure > 65"
            action "pressure := 0"
        end
    end

    # M
    graph :Motor do
        states :Off, :On, init: :Off

        # Motor turns on/off if relay 2 delivers power or doesn't
        transition :Off => :On do
            guard "Relay2 == Closed"
        end
        transition :On => :Off do
            guard "Relay2 == Open"
        end
    end

    # S
    graph :Sensor do
        states :Closed, :Open, init: :Closed

        # Trigger when tank is full (unless dead)
        transition :Closed => :Open do
            guard "pressure >= 60 && SensorDead == No"
        end
        # Stop triggering when tank is empty
        transition :Open => :Closed do
            guard "pressure < 60"
        end
    end

    # K1
    graph :Relay1 do
        states :Closed, :Open, init: :Open

        transition :Closed => :Open do
            guard "Timer == Open && Relay1Jammed == No"
        end
        transition :Open => :Closed do
            guard "Timer != Open && Switch == Closed"
        end
    end

    # K2
    graph :Relay2 do
        states :Closed, :Open, init: :Open

        close_requirement = "Switch == Closed || ( Relay1 == Closed && Sensor == Closed )"

        transition :Open => :Closed do
            guard close_requirement
        end

        transition :Closed => :Open do
            guard "! ( #{close_requirement} ) && Relay2Jammed == No"
        end
        
    end

    specify "The tank" do
        assuming no_errors do
            assuming "the switch is pressed" => :"F Switch == Closed" do
                it "will be pressured" => :"F pressure == 60"
                it "will be depressured again" => :"F (pressure == 60 && F pressure == 0)"
            end
            it "does not rapture" => :"! F Tank == Raptured"
        end
    end

    specify "The switch" do
        assuming "no fault" => :"G ( SwitchOperatedBadly == No )" do
            it "is only pressed once" => :"G ( Switch == Closed => X (! F Switch == Closed ))"
        end
    end

    # Expected Cut Sets: { SwitchOperatedBadly } { Relay2Jammed } { SensorDead, Relay1Jammed }
    hazard "The tank raptures" => :"Tank == Raptured"

end
