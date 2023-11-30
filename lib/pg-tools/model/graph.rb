
module PgTools
    module Model

        class Graph

            # The name of this graph
            attr_accessor :name

            # All Model::Component's which are part of this graph
            attr_accessor :components

            # A Model::VariableSet of all non-state variables declared by
            # components in this graph
            attr_accessor :variables

            # The Model::Specification of this graph
            attr_accessor :specification

            # An array of Model::Hazards for this graph
            attr_accessor :hazards

            def initialize(name, components: [], variables: VariableSet.new(), specification: Specification.empty(), hazards: [])
                raise "Not a variable set #{variables}" unless variables.is_a?(VariableSet)
                raise "Not a specification #{specification}" unless specification.is_a?(Specification)
                @name = name
                @components, @variables, @specification, @hazards = components, variables, specification, hazards
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

            def fault_components()
                return @components.select(&:represents_fault?)
            end

            def validate()
                errors = []
                errors += UnkownVariableValidation.validate(self)
                return errors
            end
    
        end
        
    end
end