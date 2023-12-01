module PgTools
    module Model
        module Validation

            module ForeignAssignmentValidation

                def self.validate(model)
                    errors = []
                    varset = model.all_variables()

                    model.components.each { |component|
                        actions = component.transitions.map(&:action).compact
                        actions.each { |a|
                            var_strings = a.assigned_variables()
                            # Null values (variable not known is not an error for this validation)
                            vars = var_strings.map { |vs| varset[vs] }.compact
                            vars.map { |var|
                                next if var.owner_name == component.name
                                errors << ForeignVariableAssignmentError.new(var.name, a, varset, component)
                            }
                        }
                    }
                    return errors
                end

            end

        end
    end
end
