# Require all module files
Dir[File.join(__dir__, "**", '*.rb')].sort.each { |file| require file }

module PgVerify

    module Interpret

        class InterpretError < PgVerify::Core::Error
            attr_accessor :script_file
            attr_accessor :line_number
            attr_accessor :cause

            def initialize(script_file, line_number, cause)
                @script_file, @line_number, @cause = script_file, line_number, cause
            end

            def formatted()
                title = "Error while interpreting script at #{@script_file}:#{@line_number}"
                cause_str = format_cause()

                sloc = Model::SourceLocation.new(@script_file, @line_number)
                code_block = sloc.render_code_block

                body = "#{code_block}\n\n#{cause_str}"
                return title, body
            end

            def format_cause()
                return "#{cause}" unless cause.is_a?(Core::Error)
                return cause.to_formatted()
            end

        end

        class InvalidDSL_var <  PgVerify::Core::Error
            def initialize(message)
                @message = message
            end
            def formatted()
                hint = [ "Define variables like this:" ]
                hint << "var <name>: <range> [, init: <condition>]"
                hint << "  where:"
                hint << "  <name>      is the name of the variable"
                hint << "  <range>     is a range of integers like (0..15) or an array of symbols like [ :yes, :no ]"
                hint << "  <condition> is either a value in that range or an expression"
                return @message, nil, hint.join("\n")
            end
        end

        class InvalidDSL_state <  PgVerify::Core::Error
            def initialize(message)
                @message = message
            end
            def formatted()
                hint = [ "Define states like this:" ]
                hint << "states <states>"
                hint << "  where:"
                hint << "  <states> is an array of symbols like :on, :off"
                return @message, nil, hint.join("\n")
            end
        end

        class InvalidDSL_graph <  PgVerify::Core::Error
            def initialize(message)
                @message = message
            end
            def formatted()
                hint = [ "Define graphs like this:" ]
                hint << "graph <name> do ... end"
                return @message, nil, hint.join("\n")
            end
        end

        class InvalidDSL_transition <  PgVerify::Core::Error
            def initialize(message)
                @message = message
            end
            def formatted()
                hint = [ "Define transitions like this:" ]
                hint << "transition <pre-state> => <post-state> do ... end"
                hint << "  where"
                hint << "  <pre-state> & <post-state> are states in this graph, like :on or :off"
                return @message, nil, hint.join("\n")
            end
        end

        class InvalidDSL_expression <  PgVerify::Core::Error
            def initialize(type, message)
                @type, @message = type, message
            end
            def formatted()
                hint = [ "Define #{@type}s like this:" ]
                hint << "#{@type} <expression>"
                hint << "  where"
                hint << "  <expression> is a valid expression as a string or symbol"
                return @message, nil, hint.join("\n")
            end
        end

        class InvalidDSL_model <  PgVerify::Core::Error
            def initialize(type, message)
                @type, @message = type, message
            end
            def formatted()
                hint = [ "Define models like this:" ]
                hint << "model <name> do ... end"
                hint << "  where"
                hint << "  <name> is a string or symbol"
                return @message, nil, hint.join("\n")
            end
        end
        
        class NoSuchStateError < PgVerify::Core::Error
            def initialize(state, component)
                @state, @component = state, component
            end 
            def formatted()
                title = "No such state #{@state} in component #{@component.name}"
                body = "The component #{@component.name.to_s.c_cmp} does not contain a state called #{@state.to_s.c_state}.\n"
                body += "Available states are: #{@component.states.map(&:to_s).map(&:c_state).join(', ')}"
                hint = "Make sure to define states before defining transitions."
                return title, body, hint
            end
        end

        class NoSuchScriptError < Core::Error
            def initialize(script_path)
                @script_path = script_path
            end

            def formatted()
                title = "Could not find script at #{@script_path}"
                body = "No script file at #{@script_path.c_error}!"
                hint = "Make sure to create a program graph script at \n#{File.expand_path(@script_path)}"
                return title, body, hint
            end

        end
        
    end

end
