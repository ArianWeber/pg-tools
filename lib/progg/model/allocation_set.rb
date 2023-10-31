module Progg
    module Model

        class AllocationSet

            # An array of variables
            attr_accessor :variables

            # An array of allocations for those variables.
            # Each allocation is a an array like [0, 1] where indices match the variable array
            # to represent values for that variable
            attr_accessor :allocations

            def initialize(variables, allocations)
                if !allocations.empty? && variables.length != allocations.first.length
                    raise "Variables and allocations must match in length!" 
                end
                @variables, @allocations = variables, allocations
            end

            def length()
                allocations.length
            end

        end

    end
end