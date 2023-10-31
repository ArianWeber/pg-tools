module Progg
    module Model

        class Condition

            # The expression of this condition
            attr_accessor :expression
            
            # Resolved Variables objects which the expression uses
            attr_accessor :variables

            def initialize(expression, variables)
                @expression, @variables = expression, variables
            end

            def filter_allocation_set(allocation_set)
                @expression.filter_allocation_set(allocation_set)
            end

            def to_s()
                return @expression.to_s
            end

        end

    end
end