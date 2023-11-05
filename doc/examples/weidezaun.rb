####################################################################
# Model definition
####################################################################

# Define a component called 'Hand'.
component :Hand do
    # The hand can be touching the fence or be somewhere else
    # It starts in 'somewhere' as that state is listed first
    states :somewhere, :at_fence

    # Transition non-deterministically between those states
    transition :somewhere => :somewhere
    transition :somewhere => :at_fence
    transition :at_fence => :somewhere
    transition :at_fence => :at_fence
end

# Define another component called 'PowerSwitch', which also
# transitions non-deterministically.
component :PowerSwitch do
    states :off, :on
    transition :off => :off
    transition :off => :on
    transition :on  => :off
    transition :on  => :on
end

component :Fence do
    # The fence has no states we are interested in
    states :exists

    # The fence has a voltage which can go up to 15
    var :voltage => (0..15), init: 0

    # The voltage increases when the power switch is on
    transition :exists => :exists do
        guard "PowerSwitch == on"
        action "voltage := voltage + 1"
    end
    # ..and instantly stops when the switch is off
    transition :exists => :exists do
        guard "PowerSwitch == off"
        action "voltage := 0"
    end

end

component :Pain do
    # We can either be in pain or not
    states :No, :Yes
    # Using regular variables with string interpolation
    pain_threshold = 7
    transition :No => :Yes do
        guard "Hand == at_fence && voltage >= #{pain_threshold}"
    end
    transition :Yes => :No do
        guard "Hand == somewhere || voltage < #{pain_threshold}"
    end
end

####################################################################
# Validity tests
####################################################################

# Specification of validity characteristics regarding the hand.
# The 'specify' block serves as a namespace/container for the contained specifications
specify "The Hand" do
    # Define some simple specs using a description text and an LTL expression
    it "isn't always touching the fence"  => :"F Hand == somewhere"
    it "isn't always away form the fence" => :"F Hand == at_fence"
end

# Specification of validity characteristics regarding the pain
specify "The pain" do
    # Use a regular LTL Formula as the expression
    it "is felt at some point" => :"F Pain == yes"
    # Use a more declarative syntax. This becomes useful for complex expressions
    # as LTL patterns can be used very easily
    it "is always felt at some point" => ltl.globally.exists(:"Pain == yes")

    # Pattern: 'Universality', range: 'after q'
    it "is felt after the switch is activated" => ltl.after(:"PowerSwitch == on").exists(:"Pain == yes")
    # Pattern: 'Absence', range: 'before q'
    it "is never felt before the switch is activated" => ltl.before(:"PowerSwitch == on").never(:"Pain == yes")
    # Pattern: 'Reaction', range: 'global'
    it "always reacts to the switch being activated"  => ltl.globally.reacts(:"PowerSwitch == on", :"Pain == yes")

    # Define an assumption. That assumption must be true for all contained specs
    assuming "the switch is never activated" => :"G PowerSwitch == off" do
        it "is never felt " => :"G Pain == no"
    end

    # Assumptions can be nested and used with the declarative syntax.
    assuming "the switch is activated" => ltl.globally.exists(:"PowerSwitch == on") do
        assuming "the hand never touches the fence" => :"G Hand != at_fence" do
            it "is never felt " => :"G Pain == no"
        end
    end
end
