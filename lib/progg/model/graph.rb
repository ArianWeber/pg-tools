
module Progg
    module Model

        class Graph

            # All Model::Component's which are part of this graph
            attr_accessor :components

            # A Model::VariableSet of all integer variables declared by
            # components in this graph
            attr_accessor :variables

            attr_accessor :specification

            def initialize(components: [], variables: VariableSet.new(), specification: Specification.empty())
                raise "Not a variable set #{variables}" unless variables.is_a?(VariableSet)
                @components, @variables, @specification = components, variables, specification
            end

            # Returns a list of state variables for each component in this graph
            # Each state variable is named according to the component it represents
            # and has a range consisting of the states of that component 
            def state_variables()
                vars = @components.map { |cmp|
                    Variable.new(cmp.name, cmp.states, cmp.name, initial_value: cmp.initial_state)
                }
                return VariableSet.new(*vars)
            end

            # Returns all variables used in this graph including state variables
            def all_variables()
                return state_variables() + @variables
            end

            def validate()
                errors = []
                errors += UnkownVariableValidation.validate(self)
                return errors
            end
    
        end
        
    end
end