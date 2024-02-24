module PgVerify
    module Model
        module Validation

            class ValidationError < PgVerify::Core::Error
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

            class UnknownTokenError < PgVerify::Core::Error
                def initialize(token, expression, varset)
                    @token, @expression, @varset = token, expression, varset
                end
                def formatted()
                    title = "Unknown token"
                    
                    body = []
                    body << @expression.source_location.to_s.c_sidenote  unless @expression.source_location.blank?
                    body << @expression.source_location.render_code_block() unless @expression.source_location.blank?
                    body << ""
                    body << "The expression '#{@expression.to_s.c_expression}' uses token #{@token.to_s.c_string}."
                    body << "This is neither a known variable, nor literal."
                    body << "Known variables: #{@varset.names.map(&:to_s).map(&:c_var).join(", ")}"
                    body << "Known literals: #{@varset.values.reject{ |v| v.is_a?(Numeric) }.map(&:to_s).map(&:c_literal).join(", ")}"
                    
                    return title, body.join("\n")
                end
            end

            class UnknownVariableError <  PgVerify::Core::Error
                def initialize(assigned_variable, assignment_expression, var_set)
                    @assigned_variable, @assignment_expression, @var_set = assigned_variable, assignment_expression, var_set
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

            class ForeignVariableAssignmentError <  PgVerify::Core::Error
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

            class AssignmentToStateVariableError < PgVerify::Core::Error
                def initialize(variable, expression, varset)
                    @variable, @expression, @varset = variable, expression, varset
                end
                def formatted()
                    title = "Assignment to state variable"
                    
                    body = []
                    body << @expression.source_location.to_s.c_sidenote
                    body << @expression.source_location.render_code_block()
                    body << ""
                    body << "The expression '#{@expression.to_s.c_expression}' assigns a value to variable #{@variable.to_s.c_var}."
                    body << "However this variable is a state variable and can not be written to."
    
                    hint = "State variables are read only"
    
                    return title, body.join("\n"), hint
                end
            end

            class EmptyStateSetError < PgVerify::Core::Error
                def initialize(component)
                    @component = component
                end
                def formatted()
                    title = "Empty state set"
                    body = "The graph component '#{@component.name.to_s.c_cmp}' does not define any states."
                    hint = "Each graph component needs at least one state"
                    return title, body, hint
                end
            end

            class DeadlockInFSMError < PgVerify::Core::Error
                def initialize(program_graph, deadlock_state)
                    @program_graph, @deadlock_state = program_graph, deadlock_state
                end
                def formatted()
                    trace = Model::Trace.new(@program_graph, [@deadlock_state])

                    title = "Deadlock in program graph #{@program_graph.name}"
                    body = "The following state has no successor:\n\n"
                    body += trace.to_s(include_steps: false).indented(str: ">>  ".c_error)

                    hint = []
                    hint << "This indicates a range violation:"
                    hint << "Once your graph is in the mentioned state (which can happen),"
                    hint << "all outgoing transitions are blocked due to variable range violations."
                    return title, body, hint.join("\n")
                end
            end

        end
    end
end
