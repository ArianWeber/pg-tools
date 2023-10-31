

module Progg
    module Tranform

        class NuSmvTransformation

            def transform_graph(programm_graph)
                variables  = programm_graph.all_variables
                components = programm_graph.components

                var_s  = transform_variables(variables)
                cmp_s  = transform_components(components, variables)
                main_s = transform_main_module(components)
                return "#{var_s}\n\n#{cmp_s}\n\n#{main_s}"
            end

            def transform_variables(varset)
                vars_s = varset.to_a.map { |v| transform_variable(v) }.join("\n")
                return module_string("_VARS", var: vars_s)
            end

            def transform_components(components, varset)
                return components.map { |c| transform_component(c, varset) }.join("\n\n")
            end

            def transform_variable(variable)
                return "#{transform_varname(variable.name)} : #{transform_range(variable.range)}"
            end

            def transform_range(range)
                # Transform state variables
                return "{#{range.map { |e| transform_const(e) }.join(", ")}};" if range.is_a?(Array)
                # Transform small integer variables
                return "{#{range.join(", ")}};" if (range.last - range.first) < 5
                # Transform regular integer variables
                return "#{range.first}..#{range.last};"
            end

            def transform_component(component, varset)
                # Name of the component module
                name = transform_module_name(component.name)
                # The component takes all variables v
                name += "(v)"

                # Generate the INIT block. Each component initializes it's own variables
                cmp_vars = varset.select_by_owner(component.name).to_a
                init = cmp_vars.map { |var|
                    # Allow variables to omit the initital value as to choose an indeterministic value
                    next if var.initial_value.nil?
                    "v.#{transform_varname(var.name)} = #{transform_const(var.initial_value)}"
                }.compact.join(" & ")

                trans = component.transitions.map { |transition|
                    trans_s = transform_transition(varset, component, transition)
                    "( #{trans_s} )"
                }.join(" | \n")
                trans += ";"

                return module_string(name, init: init, trans: trans)
            end

            def transform_transition(varset, component, transition)
                expression = []

                cmp_varname = transform_varname(component.name)
                prev_state  = transform_const(transition.src_state)
                expression << "v.#{cmp_varname} = #{prev_state}"

                next_state = transform_const(transition.tgt_state)
                expression << "next(v.#{cmp_varname}) = #{next_state}"

                precon_s = transform_expression(transition.precon, varset)
                guard_s  = transform_expression(transition.guard, varset)
                action_s = transform_expression(transition.action, varset)
                expression << precon_s
                expression << guard_s
                expression << action_s

                # TODO: Actions, preconditions and guards
                # ( v.V_UmgebungPosition = L_Moving & next(v.V_UmgebungPosition) = L_Moving & ((v.V_Position < 90) & (v.V_Position < 90)) & (next(v.V_Position) = (v.V_Position + v.V_Speed)))  |
                # ( v.V_FailureSensor = L_No & next(v.V_FailureSensor) = L_Yes & TRUE & (TRUE))  |
                return expression.compact.join(' & ')
            end

            def transform_assignment()
                "Position := Velocity * Speed"
                "next(v.V_Position) = ()"


            end

            def transform_expression(expression, varset)
                return nil if expression.nil?

                expression_s = expression.to_s

                expression_s = expression_s.gsub(/(\w+)\s*:=/) {
                    assigned_var = Regexp.last_match[1]
                    "next(#{assigned_var}) ="
                }

                expression.used_variables.uniq.each { |symbol|

                    symbol_s = varset.varname?(symbol) \
                        ? "v.#{transform_varname(symbol)}" \
                        : transform_const(symbol)
                    puts "REPL #{symbol} by #{symbol_s} in '#{expression}'"


                    expression_s = expression_s.gsub(/\bword\b(?!\w)/, symbol_s) { |match|
                        puts "MATCH: '#{match}'"
                    }
                }

                expression_s = expression_s.gsub('&&', '&')
                expression_s = expression_s.gsub('||', '|')
                expression_s = expression_s.gsub('||', '|')
                expression_s = expression_s.gsub('==', '=')


                return expression_s
            end


            def transform_main_module(components)
                # Instantiate variable module as v
                var_s = "v : _VARS();\n"
                # Instantiate all component modules, passing in v
                var_s += components.map { |component|
                    instance = transform_module_instance_name(component.name)
                    modul    = transform_module_name(component.name)
                    "#{instance} : #{modul}(v);"
                }.join("\n")
                return module_string("main", var: var_s)
            end

            def module_string(name, hash = {})
                blocks = [ "MODULE #{name}" ]
                hash.each { |key, val|
                    blocks << key.to_s.upcase.indented
                    blocks << val.indented(num: 2)
                }
                return blocks.join("\n")
            end

            def transform_varname(name)
                "V_#{name}"
            end

            def transform_const(value)
                return value.to_s unless value.is_a?(Symbol) || value.is_a?(String)
                "L_#{value}"
            end

            def transform_module_name(name)
                "_P_#{name}"
            end

            def transform_module_instance_name(name)
                "p_#{name}"
            end

        end

    end
end
