module PgVerify
    module Model
        module Validation

            module UnknownTokenValidation

                def self.validate(model)
                    errors = []
                    varset = model.all_variables()
                    transitions = model.components.map(&:transitions).flatten
                    transitions.each { |tx|
                        errors += validate_expression(varset, tx.precon)
                        errors += validate_expression(varset, tx.guard)
                        errors += validate_expression(varset, tx.action)

                    }
                    return errors
                end

                def self.validate_expression(varset, expression)
                    return [] if expression.nil?
                    tokens = expression.word_tokens()
                    tokens = tokens.reject { |token|
                        varset.varname?(token) || varset.constname?(token)
                    }
                    return tokens.map { |token|
                        UnknownTokenError.new(token, expression, varset)
                    }
                end

            end

        end
    end
end
