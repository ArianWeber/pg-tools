module PgTools
    module Model
        module Validation

            def self.validate(model)
                errors = []
                errors += UnknownTokenValidation.validate(model)
                errors += ForeignAssignmentValidation.validate(model)
                return errors
            end

            def self.validate!(model)
                errors = validate(model)
                return if errors.empty?
                raise ValidationError.new(model, errors)
            end

        end

    end
end
