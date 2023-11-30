# Require all module files
Dir[File.join(__dir__, "**", '*.rb')].sort.each { |file| require file }

module PgTools

    module Interpret

        class InterpretError < PgTools::Core::Error
            attr_accessor :script_file
            attr_accessor :line_number
            attr_accessor :cause

            def initialize(script_file, line_number, cause)
                @script_file, @line_number, @cause = script_file, line_number, cause
            end

            def formatted()
                title = "Error while interpreting script at #{@script_file}:#{@line_number}"
                cause_str = format_cause()

                lines = render_code_block(@script_file, @line_number - 1, 2)

                body = "#{lines}\n\n#{cause_str}"
                return title, body
            end

            def format_cause()
                return "#{cause}" unless cause.is_a?(Core::Error)
                return cause.to_formatted()
            end

            def render_code_block(file, index, num_lines)
                # Read lines and add line numbers
                lines = File.read(file).split("\n").each_with_index.map { |l, i| "#{i.to_s.c_blue}: #{l}" }

                # Get the line surrounding the error
                start_index = [0, index - num_lines].max
                end_index = [lines.length - 1, index + num_lines].min
                surrounding_lines = lines[start_index..end_index]
                error_line = lines[index]

                surrounding_lines = [ "...".c_sidenote ] + surrounding_lines + [ "...".c_sidenote ]

                surrounding_lines = surrounding_lines.map { |l|
                    if l == error_line
                        l.ljust(surrounding_lines.map(&:length).max) + " < Error".c_error
                    else
                        l
                    end
                }

                return surrounding_lines.join("\n")
            end

        end

        class InvalidDSL_var <  PgTools::Core::Error
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

        class InvalidDSL_state <  PgTools::Core::Error
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

        class InvalidDSL_graph <  PgTools::Core::Error
            def initialize(message)
                @message = message
            end
            def formatted()
                hint = [ "Define graphs like this:" ]
                hint << "graph <name> do ... end"
                return @message, nil, hint.join("\n")
            end
        end

        class InvalidDSL_transition <  PgTools::Core::Error
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

        class InvalidDSL_expression <  PgTools::Core::Error
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

        class InvalidDSL_model <  PgTools::Core::Error
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
        
        class NoSuchStateError < PgTools::Core::Error
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
