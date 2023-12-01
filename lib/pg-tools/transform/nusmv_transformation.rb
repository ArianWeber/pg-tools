module PgTools
    module Transform

        class NuSmvTransformation

            def transform_graph(program_graph)
                variables  = program_graph.all_variables
                components = program_graph.components

                var_s  = transform_variables(variables)
                cmp_s  = transform_components(components, variables)
                main_s = transform_main_module(components)

                specs = transform_specification(program_graph.specification, variables)

                return "#{var_s}\n\n#{cmp_s}\n\n#{main_s}\n\n#{specs}\n"
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
                owned_vars = varset.select_by_owner(component.name).to_a
                # TODO: Implement init expressions as well
                init = owned_vars.map { |var|
                    # Allow variables to omit the initial value as to choose an indeterministic value
                    next if var.initial_value.nil?
                    "v.#{transform_varname(var.name)} = #{transform_const(var.initial_value)}"
                }.compact.join(" & ")
                init = "TRUE" if init.empty?

                # Transform all transitions which are defined in this component
                trans = component.transitions.map { |transition|
                    trans_s = transform_transition(varset, component, transition)
                    "( #{trans_s} )"
                }

                # Create a default transition which is taken whenever no other
                # transitions can.
                tau_transition = []
                # Combine the precondition, guard and source state conditions
                # For each transition of this component
                other_transitions = component.transitions.map { |transition| 
                    precon_s = transform_expression(transition.precon, varset)
                    guard_s  = transform_expression(transition.guard, varset)

                    cmp_varname = transform_varname(component.name)
                    this_state = transform_const(transition.src_state)
                    state_s = "v.#{cmp_varname} = #{this_state}"

                    [ state_s, precon_s, guard_s ].compact.reject(&:empty?).join(' & ')
                }.compact.reject(&:empty?)
                # Only take the tau transition when all other transitions do not match 
                tau_transition << "!(\n\t#{other_transitions.join(" | \n\t")})\n\t"

                # Keep this components state when using the default transition
                # Keep all owned variables equal when using the default transition
                tau_transition += owned_vars.map { |var|
                    varname = "v.#{transform_varname(var.name)}"
                    "next(#{varname}) = #{varname}"
                }
                # Add the tau transition
                trans << "(#{tau_transition.join(" & ")})"

                trans = trans.join(" | \n") + ";"

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

                action_s, assigned_var = transform_assignment(transition.action, component, varset)
                
                keep_vars_constant = varset.select_by_owner(component.name).names
                    .reject { |v| v.to_s == assigned_var.to_s }
                    .reject { |v| v.to_s == component.name.to_s }
                    .map { |v| "next(v.#{transform_varname(v)}) = v.#{transform_varname(v)}" }
                    .join(' & ')

                expression << precon_s
                expression << guard_s
                expression << action_s
                expression << keep_vars_constant

                return expression.compact.reject(&:empty?).join(' & ')
            end

            def transform_assignment(assignment_expression, component, varset)
                return nil if assignment_expression.nil?

                parts = assignment_expression.to_s.split(":=")

                assigned_variable = parts[0].strip

                unless varset.varname?(assigned_variable)
                    raise Model::Validation::UnknownVariableError.new(assigned_variable, assignment_expression, varset)
                end
                unless varset[assigned_variable].owner_name == component.name
                    raise Model::Validation::ForeignVariableAssignmentError.new(assigned_variable, assignment_expression, varset, component)
                end
                if varset[assigned_variable].state_variable?
                    raise Model::Validation::AssignmentToStateVariableError.new(assigned_variable, assignment_expression, varset)
                end

                assigned_variable_s = "next(v.#{transform_varname(assigned_variable)})"

                expression = parts[1].strip
                expression = transform_expression(Model::ParsedExpression.new(expression, Model::ParsedExpression::TYPE_TERM), varset)

                assignment_s = "#{assigned_variable_s} = #{expression}"

                return assignment_s, assigned_variable
            end

            def transform_expression(expression, varset)
                return nil if expression.nil?

                variables, constants = [], []
                expression.word_tokens.select { |w| 
                    if varset.names.include?(w)
                        variables << w
                    elsif  varset.values.include?(w)
                        constants << w
                    else
                        raise Model::Validation::UnknownTokenError.new(w, expression, varset)
                    end
                }
                variables, constants = variables.uniq, constants.uniq

                constants = expression.word_tokens.select { |w| varset.values.include?(w) }.uniq

                expression_s = expression.to_s

                variables.each { |v| expression_s = expression_s.gsub(v.to_s, "v." + transform_varname(v))  }
                constants.each { |c| expression_s = expression_s.gsub(c.to_s, transform_const(c))  }

                expression_s = expression_s.gsub('&&', '&')
                expression_s = expression_s.gsub('||', '|')
                expression_s = expression_s.gsub('||', '|')
                expression_s = expression_s.gsub('==', '=')
                expression_s = expression_s.gsub('=>', '->')
                expression_s = expression_s.gsub('<=>', '<->')

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

            def transform_specification(specification, varset)
                return specification.flatten().map { |s| transform_spec(s, varset) }.join("\n")
            end

            def transform_spec(spec, varset)
                expression_s  = "-- #{spec.text}\n"
                expression_s += "-- Defined in: #{spec.source_location.to_s}\n"
                expression_s += "LTLSPEC " + transform_expression(spec.expression, varset)
                return expression_s
            end

        end

    end
end
