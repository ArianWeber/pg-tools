module PgVerify
    module Simulation

        class Simulator

            attr_accessor :graph

            def initialize(graph)
                @graph = graph
            end

            def run(steps: 10, state: State.for_variable_set(@graph.all_variables))
                state_stack = [ state ]
                steps.times {
                    component_transitions = find_next_transitions(state)
                    state = run_transitions(component_transitions, state)
                    state_stack << state
                }
                return state_stack
            end

            # Returns a map from 'component' to 'transition' which should
            # be used for that component based in the state.
            # If there is no matching transition for a component, that
            # component is omitted as to use the "tau-transition".
            def find_next_transitions(state)
                 return @graph.components.map { |cmp|
                    matching_transitions = cmp.transitions.select { |trans|
                        transition_accepted?(trans, cmp, state)
                    }
                    # Do not change the state if no transitions match
                    next if matching_transitions.empty?

                    # TODO: Choose random one using seed
                    chosen_transition = matching_transitions.shuffle().first
                    [cmp, chosen_transition]
                }.compact.to_h
            end

            # Performs the specified transitions on the specified state
            # and returns a new updated state.
            def run_transitions(component_transitions, state)
                new_state = state.clone()
                component_transitions.each { |component, transition|
                    # Update the component state
                    new_state[component.name] = transition.tgt_state

                    # Perform the action
                    next if transition.action.nil?
                    parts = transition.action.to_s.split(":=")
                    assigned_variable = parts[0].strip
                    expression = parts[1].strip

                    new_state[assigned_variable.to_sym] = eval_expression(expression, state)
                }
                return new_state
            end

            def transition_accepted?(transition, component, state)
                # Transition can't be used unless the component is in the required
                # source state.
                return false unless transition.src_state == state[component.name]
                
                guard_expression = [ transition.guard, transition.precon ]
                    .map(&:to_s).reject(&:empty?).join(" && ")

                return false if eval_expression(guard_expression, state) == false

                return true
            end

            def eval_expression(expression, state)
                state.names.sort_by(&:length).reverse.each { |var|
                    value = state[var]
                    expression = expression.gsub(var.to_s, value.to_s)
                }
                
                expression = Model::ParsedExpression.new(expression, nil)
                words = expression.word_tokens.uniq
                expression = expression.to_s
                words.each { |word|
                    expression = expression.gsub(word.to_s, "'#{word}'")
                }
                return eval(expression)
            end

        end

    end
end
