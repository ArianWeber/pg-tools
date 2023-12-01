module PgTools
    module Model

        class Variable

            # Name of this variable as a symbol
            attr_accessor :name

            # The name of the component which owns this variable as a symbol
            attr_accessor :owner_name

            # Range of this variable. This can either be a range like (0..2)
            # an array of numbers like [0, 1, 2] or an array of symbols representing
            # states of components like [:Idle, :Breaking]
            attr_accessor :range

            # Initial value of the variable as an integer
            # Can be nil to not provide a fixed initial value
            attr_accessor :initial_value

            # The location in source, where this variable was defined
            attr_accessor :source_location

            def initialize(name, range, owner_name, source_location, initial_value: nil)
                @name, @range, @owner_name, @initial_value = name, range, owner_name, initial_value
                @source_location = source_location
            end

            # Returns all possible values of this variable as an array
            def values()
                return @range if @range.is_a?(Array)
                return @range.to_a
            end

            # A state variable represents the states of a component.
            # e.g: Switch with range [:on, :off] for component switch.
            def state_variable?()
                return @name == @owner_name
            end

        end

    end

end
