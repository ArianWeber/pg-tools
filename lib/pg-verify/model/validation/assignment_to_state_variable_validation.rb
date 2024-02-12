module PgVerify
    module Model
        module Validation

            module AssignmentToStateVariableValidation

                def self.validate(model)
                    errors = []
                    varset = model.all_variables()
                    actions = model.components.map(&:transitions).flatten.map(&:action).compact
                    actions.each { |action|
                        action.assigned_variables().each { |var_string|
                            var = varset[var_string]
                            # Do not handle: Assignment to unknown variable
                            next if var.nil?
                            next unless var.state_variable?
                            errors << AssignmentToStateVariableError.new(var.name, action, varset)
                        }
                    }
                    return errors
                end
                
            end
        end
    end
end
