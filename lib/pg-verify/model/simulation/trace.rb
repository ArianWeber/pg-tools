module PgVerify
    module Model

        class Trace

            attr_accessor :model
            attr_accessor :states

            def initialize(model, states)
                @model, @states = model, states
            end

            def to_s(include_steps: true)
                return "No states in trace" if @states.empty?
                # Get all variables (TODO: Bring into sensible order)
                vars = @states.first.keys
                state_vars = @model.state_variables()

                parts = vars.map { |var| 
                    var_string = state_vars.varname?(var) ? var.to_s.c_state.c_bold : var.to_s.c_string
                    var_string + "\n" + @states.each_with_index.map{ |state, index| value_str(var, state[var], index) }.join("\n") 
                }
                str = "Step".c_num.c_bold + "\n" + (0...@states.length).map { |i| "#{i + 1}" } .join("\n")
                str = "" unless include_steps
                parts.each { |part| str = str.line_combine(part, separator: "  ") }
                
                return str
            end

            def value_str(key, value, index)
                return value.c_green if [ "On", "Yes", "Active" ].include?(value)
                return value.c_red if [ "Off", "No", "Idle" ].include?(value)

                settings_color = Settings.trace.colors[value.to_s]
                return value.send(:"c_#{settings_color}") unless settings_color.blank?

                return "#{value}" 

            end

        end

    end
end
