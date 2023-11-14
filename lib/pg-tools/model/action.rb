module PgTools
    module Model

        class Action
            
            # The expression of this action
            attr_accessor :expression
            # All resolved variable objects the expression needs
            attr_accessor :variables

            def initialize(expression, variables)
                @expression, @variables = expression, variables
            end

            def calc_illegal_allocations()
                @expression.calc_illegal_allocations(@variables)
            end

            def to_s()
                return @expression.to_s
            end


        end

    end
end