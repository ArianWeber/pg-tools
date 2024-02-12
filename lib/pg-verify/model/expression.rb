module PgVerify
    module Model

        # Class used to represent a mathematical expression
        class Expression

            ASSIGNMENT_OP = ':='
            MAX_ALLOCS_FOR_CHECK = 1000000

            # 3 * Position + x
            # >= 4

            # The entire expression as a string
            attr_accessor :expression_string

            def initialize(expression_string)
                @expression_string = expression_string
            end

            # Finds all variables in the specified this expression
            # and returns them as an unique array of symbols ordered by their occource
            def used_variables(expression=@expression_string)
                expression.scan(/[a-zA-Z_][a-zA-Z0-9_]*/).uniq.map(&:to_sym)
            end

            def assigned_variable()
                var, expression = isolate_assigned_variable()
                return var
            end

            def term_variables()
                var, expression = isolate_assigned_variable()
                return used_variables(expression)
            end

            def assignment?()
                return !assigned_variable().nil?
            end

            # Calcuates an AllocationSet which is populated with all variable allocations
            # which lead to an illegal output value for this expressions variable assignment
            def calc_illegal_allocations(resolved_variables)

                assigned_var, term = isolate_assigned_variable()
                raise("Term '#{self}' is not a variable assignment!") if assigned_var.nil?
                term_vars = used_variables(term)

                # Resolve the variables used in this expression to actual variable objects
                assigned_var = resolved_variables.detect {|var| var.name == assigned_var }
                term_vars = term_vars.map { |varname| resolved_variables.detect { |var| varname == var.name } }

                # Calculate the number of possible allocations, which is the product of the variable range length
                num_combinations = term_vars.map(&:range).map(&:count).reduce(&:*)

                # Set an upper limit to not calculate some huge range forever
                raise "Search space too large on validation of term '#{self}': #{num_combinations} > #{MAX_ALLOCS_FOR_CHECK}" \
                    if num_combinations > MAX_ALLOCS_FOR_CHECK

                # Calculate all combinations of possible variable assignments based on their range
                allocations = term_vars.map(&:range).map(&:to_a).reduce(&:product).map(&:flatten)

                # For each combination of variable allocations create an expression by replacing the variable
                # with its value in that allocation
                illegal_allocations = allocations.reject { |alloc|
                   values = term_vars.map(&:name).map(&:to_s).zip(alloc).to_h
                   expression = term.gsub(/#{term_vars.map(&:name).join("|")}/, values)

                   resolved_value = eval(expression)
                   assigned_var.range.include?(resolved_value)
                }

                puts "#{illegal_allocations}"

                return AllocationSet.new(term_vars, illegal_allocations)
            end

            # Only keeps allocations in the specified allocation set which are possible
            # based on this condition
            def filter_allocation_set(allocation_set)
                # Only keep allocations for which this condition evaluates to true
                allocation_set.allocations.select { |alloc|
                    values = allocation_set.variables.map(&:name).map(&:to_s).zip(alloc).to_h
                    term = @expression_string.gsub(/#{allocation_set.variables.map(&:name).join("|")}/, values)
                    result = eval(term)
                    raise "Expression #{self} for allocation #{alloc} (#{term}) evaluated to #{result} which is not a boolean" \
                        unless result == true || result == false
                    result == true
                }
            end

            # This method splits off a potential assigned variable and returns the variable
            # which is assigned as a symbol and the remaining expression as a string.
            def isolate_assigned_variable()
                var = used_variables().first
                match = @expression_string[/(#{var}\s*#{ASSIGNMENT_OP})/, 1]
                return nil, @expression_string if match.nil?
                return var, @expression_string.gsub(match, '').strip
            end

            def to_s()
                @expression_string
            end

        end
    end
end