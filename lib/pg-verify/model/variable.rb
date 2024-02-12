module PgVerify
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

            # An expression to be used to initialize this variable
            # While this can be something simple like "my_var = 10", it can
            # also be something more involved like "my_var > 10 && my_var < 15".
            attr_accessor :init_expression

            # The location in source, where this variable was defined
            attr_accessor :source_location

            def initialize(name, range, owner_name, source_location, init: nil)
                init = Model::ParsedExpression.new(init, Model::ParsedExpression::TYPE_PL) if init.is_a?(String)
                @name, @range, @owner_name, @init_expression = name, range, owner_name, init
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
