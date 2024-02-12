
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

            # A Model::ParsedExpression used as the init expression.
            # This expression must hold true initially. It can be used
            # to restrict or set the initial state and variables.
            # Can be left as nil in order to not restrict anything
            attr_accessor :init_expression

            def initialize(args = {})
                @name = ( args[:name] || raise('Blubb') )
                @states = ( args[:states] || [] )
                @transitions = ( args[:transitions] || [] )
                @represents_fault = args[:represents_fault].nil? ? false : args[:represents_fault]
                @source_location =  ( args[:source_location] )
                @init_expression =  args[:init_expression]
            end

            def represents_fault?()
                return @represents_fault
            end

        end

    end
end
