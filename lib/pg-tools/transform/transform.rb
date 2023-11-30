# Require all module files
Dir[File.join(__dir__, "**", '*.rb')].sort.each { |file| require file }

module PgTools
    module Transform

        class ForeignVariableAssignmentError <  PgTools::Core::Error
            def initialize(assigned_variable, assignment_expression, var_set, component)
                @assigned_variable, @assignment_expression, @var_set = assigned_variable, assignment_expression, var_set
                @component = component
            end
            def formatted()
                title = "Foreign variable assignment"
                
                body = []
                body << @assignment_expression.source_location.to_s.c_sidenote
                body << @assignment_expression.source_location.render_code_block()
                body << ""
                body << "Component #{@component.name.to_s.c_cmp} assigns a value to variable #{@assigned_variable.to_s.c_var},"
                body << "using this expression '#{@assignment_expression.to_s.c_expression}'."
                body << "However #{@assigned_variable.to_s.c_var} is owned by component #{@var_set[@assigned_variable].owner_name.to_s.c_cmp}"

                hint = "A variable can only be written to by the component which declared it"

                return title, body.join("\n"), hint
            end
        end

        class UnknownVariableError <  PgTools::Core::Error
            def initialize(assigned_variable, assignment_expression, var_set, component)
                @assigned_variable, @assignment_expression, @var_set = assigned_variable, assignment_expression, var_set
                @component = component
            end
            def formatted()
                title = "Unknown variable '#{@assigned_variable}'"
                
                body = []
                body << @assignment_expression.source_location.to_s.c_sidenote
                body << @assignment_expression.source_location.render_code_block()
                body << ""
                body << "The expression '#{@assignment_expression.to_s.c_expression}' uses variable #{@assigned_variable.to_s.c_var}."
                body << "However there is no such variable in the context of this model."
                body << "Known variables: #{@var_set.names.map(&:to_s).map(&:c_var).join(', ')}."

                hint = "Declare variables before using them in expressions"

                return title, body.join("\n"), hint
            end
        end

        class AssignmentToStateVariableError <  PgTools::Core::Error
            def initialize(assigned_variable, assignment_expression, var_set, component)
                @assigned_variable, @assignment_expression, @var_set = assigned_variable, assignment_expression, var_set
                @component = component
            end
            def formatted()
                title = "Unknown variable '#{@assigned_variable}'"
                
                body = []
                body << @assignment_expression.source_location.to_s.c_sidenote
                body << @assignment_expression.source_location.render_code_block()
                body << ""
                body << "The expression '#{@assignment_expression.to_s.c_expression}' assigns a value to variable #{@assigned_variable.to_s.c_var}."
                body << "However this variable is a state variable and can not be written to"

                hint = "Declare variables before using them in expressions"

                return title, body.join("\n"), hint
            end
        end

        class UnknownTokenError <  PgTools::Core::Error
            def initialize(token, expression, var_set)
                @token, @expression, @var_set = token, expression, var_set
            end
            def formatted()
                title = "Unknown token"
                
                body = []
                body << @expression.source_location.to_s.c_sidenote
                body << @expression.source_location.render_code_block()
                body << ""
                body << "The expression '#{@expression.to_s.c_expression}' uses token #{@token.to_s.c_string}."
                body << "This is neither a known variable, nor literal."
                body << "Known variables: #{@var_set.names.map(&:to_s).map(&:c_var).join(", ")}"
                body << "Known literals: #{@var_set.values.reject{ |v| v.is_a?(Numeric) }.map(&:to_s).map(&:c_literal).join(", ")}"
                
                return title, body.join("\n")
            end
        end

    end
end
