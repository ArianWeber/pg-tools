
module PgTools
    module Model

        class Component
            
            # The name of this component as a symbol
            attr_accessor :name
            # A list of symbols representing states
            attr_accessor :states
            # A list of transitions between states
            attr_accessor :transitions
            # Flag on whether this component is used to represent a fault.
            attr_accessor :represents_fault
            # The source location of this components definition 
            attr_accessor :source_location

            def initialize(args = {})
                @name = ( args[:name] || raise('Blubb') )
                @states = ( args[:states] || [] )
                @transitions = ( args[:transitions] || [] )
                @represents_fault = args[:represents_fault].nil? ? false : args[:represents_fault]
                @source_location =  ( args[:source_location] )
            end

            def initial_state()
                return states.first
            end

            def represents_fault?()
                return @represents_fault
            end

        end

    end
end