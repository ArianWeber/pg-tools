
module Progg
    module Model

        class Component
            
            # The name of this component as a symbol
            attr_accessor :name
            # A list of symbols representing states
            attr_accessor :states
            # A list of transitions between states
            attr_accessor :transitions

            # Variables owned by this component
            # attr_accessor :owned_variables
            # # Variables which are imported from other components
            # attr_accessor :imported_variables

            def initialize(args = {})
                @name = ( args[:name] || raise('Blubb') )
                @states = ( args[:states] || [] )
                @transitions = ( args[:transitions] || [] )
                # @owned_variables = ( args[:owned_variables] || [] )
                # @imported_variables = ( args[:imported_variables] || [] )
            end

            def initial_state()
                return states.first
            end



        end

    end
end