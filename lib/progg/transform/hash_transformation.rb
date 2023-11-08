module Progg
    module Transform

        class HashTransformation

            def transform_graph(graph)
                return { "graph" => 
                    graph.components.map { |c| transform_component(graph, c) }
                }
            end

            def parse_graph(hash)
                graph = Model::Graph.new()
                components = hash["graph"].map { |c| parse_component(graph, c) }
                graph.components = components
                return graph
            end

            def transform_component(graph, component)
                variables = graph.variables.select_by_owner(component)
                return { component.name.to_s => {
                    "states" => component.states.map(&:to_s),
                    "variables" => variables.map { |v| transform_variable(v) },
                    "transitions" => component.transitions.map { |t| transform_transition(t) },
                    "represents_fault" => component.represents_fault
                }}
            end

            def parse_component(graph, hash)
                name = hash.keys.first
                states = hash[name]["states"].map(&:to_sym)
                variables = hash[name]["variables"].map { |v| parse_variable(name, v) }
                transitions = hash[name]["transitions"].map { |t| parse_transition(t) }
                represents_fault = hash[name]["represents_fault"]

                graph.variables += variables

                return Model::Component.new(name: name.to_sym, states: states, 
                    transitions: transitions, represents_fault: represents_fault)
            end

            def transform_variable(variable)
                return { variable.name.to_s => {
                    "range" => variable.range,
                    "init" => variable.initial_value
                }}
            end

            def parse_variable(owner_name, hash)
                name = hash.keys.first
                range = hash[name]["range"]
                initial_value = hash[name]["init"]
                return Model::Variable.new(name.to_sym, range, owner_name.to_sym, initial_value:  initial_value)
            end

            def transform_transition(transition)
                name = "#{transition.src_state} -> #{transition.tgt_state}"
                return { name => {
                    "precon" => transition.precon&.to_s,
                    "guard" => transition.guard&.to_s,
                    "action" => transition.action&.to_s,
                }}
            end

            def parse_transition(hash)
                src_state, tgt_state = hash.keys.first.split("->").map(&:strip).map(&:to_sym)
                precon = hash.values.first["precon"]
                guard  = hash.values.first["guard"] 
                action = hash.values.first["action"]
                return Model::Transition.new(src_state, tgt_state, precon: precon, guard: guard, action: action)
            end

        end
    end
end
