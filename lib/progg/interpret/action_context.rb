module Progg
    module Interpret

        class ActionContext

            attr_accessor :parent_transition
            attr_accessor :expression

            def initialize(parent_transition, expression)
                @parent_transition = parent_transition
                @expression = Model::Expression.new(expression)
                raise InterpretError.new("Could not find variable assignment in action '#{@expression}'") if @expression.assigned_variable.nil?
            end

            def to_model()
                # Resolve the assigned variable
                owned_vars = parent_transition.parent_component.variable_list
                resolved_assigned_var = owned_vars.detect { |var| var.name == @expression.assigned_variable }
                raise "Action '#{@expression}' assigns a value to #{@expression.assigned_variable} " \
                    "which isn't owned by the parent component #{parent_transition.parent_component.name}!" if resolved_assigned_var.nil?

                # Resolve variables used in the expression
                imported_vars = parent_transition.parent_component.resolve_imported_variables
                resolved_imported_vars = @expression.term_variables.map { |varname|
                    resolved = (owned_vars + imported_vars).detect { |var| var.name == varname }
                    raise "Action '#{@expression}' uses variable '#{varname}' which could not be resolved." if resolved.nil?
                    resolved
                }

                all_variables = ([ resolved_assigned_var ] + resolved_imported_vars ).uniq

                Model::Action.new(@expression, all_variables)
            end

        end

    end
end