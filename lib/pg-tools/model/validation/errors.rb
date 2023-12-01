module PgTools
    module Model
        module Validation

            class ValidationError < PgTools::Core::Error
                def initialize(model, errors)
                    @model, @errors = model, errors
                end

                def formatted()
                    title = "There are #{@errors.length} errors for model #{@model.name}"

                    summary = @errors.map{ |e| e.formatted.first }
                    summary = summary.each_with_index.map { |e, i| "#{i + 1}: #{e}" }.join("\n")

                    body = @errors.map(&:to_formatted).join("\n")
                    return title, "#{summary}\n\n#{body}"
                end

            end

            class UnknownTokenError < PgTools::Core::Error
                def initialize(token, expression, varset)
                    @token, @expression, @varset = token, expression, varset
                end
                def formatted()
                    title = "Unknown token"
                    
                    body = []
                    body << @expression.source_location.to_s.c_sidenote
                    body << @expression.source_location.render_code_block()
                    body << ""
                    body << "The expression '#{@expression.to_s.c_expression}' uses token #{@token.to_s.c_string}."
                    body << "This is neither a known variable, nor literal."
                    body << "Known variables: #{@varset.names.map(&:to_s).map(&:c_var).join(", ")}"
                    body << "Known literals: #{@varset.values.reject{ |v| v.is_a?(Numeric) }.map(&:to_s).map(&:c_literal).join(", ")}"
                    
                    return title, body.join("\n")
                end
            end

            class ForeignVariableAssignmentError <  PgTools::Core::Error
                def initialize(variable, expression, varset, component)
                    @variable, @expression, @varset = variable, expression, varset
                    @component = component
                end
                def formatted()
                    title = "Foreign variable assignment"
                    
                    body = []
                    body << @expression.source_location.to_s.c_sidenote
                    body << @expression.source_location.render_code_block()
                    body << ""
                    body << "Component #{@component.name.to_s.c_cmp} assigns a value to variable #{@variable.to_s.c_var},"
                    body << "using this expression '#{@expression.to_s.c_expression}'."
                    body << "However #{@variable.to_s.c_var} is owned by component #{@varset[@variable].owner_name.to_s.c_cmp}"
    
                    hint = "A variable can only be written to by the component which declared it"
    
                    return title, body.join("\n"), hint
                end
            end



        end
    end
end
