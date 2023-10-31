module Progg
    module Interpret

        class ConditionContext

            attr_accessor :parent_transition
            attr_accessor :expression

            def initialize(parent_transition, expression)
                @parent_transition = parent_transition
                @expression = Model::Expression.new(expression)
            end

            def to_model()
                # Resolve variables used in the expression
                owned_vars = parent_transition.parent_component.variable_list
                imported_vars = parent_transition.parent_component.resolve_imported_variables
                resolved_imported_vars = @expression.term_variables.map { |varname|
                    resolved = (owned_vars + imported_vars).detect { |var| var.name == varname }
                    raise "Condition '#{@expression}' uses variable '#{varname}' which could not be resolved." if resolved.nil?
                    resolved
                }

                all_variables = resolved_imported_vars.uniq

                Model::Condition.new(@expression, all_variables)
            end

        end

    end
end