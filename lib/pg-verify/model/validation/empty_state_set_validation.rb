module PgTools
    module Model
        module Validation

            module EmptyStateSetValidation

                def self.validate(model)
                    errors = model.components.map { |cmp|
                        next if cmp.states.length > 0
                        EmptyStateSetError.new(cmp)
                    }.compact
                    return errors
                end
                
            end
        end
    end
end
