require_relative 'validation_error.rb'

module PgTools
    module Model

        module UnkownVariableValidation

            class UnkownVariableError < ValidationError

                def initialize(variables, component, transition, variable, expression, type)
                    @variables, @component, @transition, @variable = variables, component, transition, variable 
                    @expression, @type = expression, type
                end

                def title()
                    return "Unknown variable: '#{@variable}'!"
                end

                def body()
                    trans_s = "(#{@transition.src_state} -> #{@transition.tgt_state})".c_trans
                    expression = @expression.to_s.gsub(@variable.to_s, @variable.to_s.c_warn)
                    return "The #{@type} defined in transition #{trans_s} of component #{@component.name.to_s.c_cmp}" \
                        " uses variable #{@variable.to_s.c_warn} which could not be found." \
                        "\n  Expression: #{expression}" \
                        "\n  Variables:  #{@variables.to_s}"
                end

            end

            def self.validate(graph)
                errors = []
                variables = graph.all_variables()
                constants = graph.state_variables().values()
                graph.components.each { |component|
                    component.transitions.each { |transition|
                        transition.guard&.used_variables()&.uniq&.each { |variable|
                            next if variables.include?(variable) || constants.include?(variable)
                            errors << UnkownVariableError.new(variables, component, transition, variable, transition.guard, "guard")
                        }
                        transition.precon&.used_variables()&.uniq&.each { |variable|
                            next if variables.include?(variable) || constants.include?(variable)
                            errors << UnkownVariableError.new(variables, component, transition, variable, transition.precon, "precondition")
                        }
                        transition.action&.used_variables()&.uniq&.each { |variable|
                            next if variables.include?(variable) || constants.include?(variable)
                            errors << UnkownVariableError.new(variables, component, transition, variable, transition.action, "action")
                        }
                    }
                }
                return errors
            end

        end

    end
end