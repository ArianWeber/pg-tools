module PgVerify
    module Transform

        class HashTransformation

            def transform_graph(graph)
                components = graph.components.map { |c| transform_component(graph, c) }
                hazards    = graph.hazards.map { |h| transform_hazard(h) }
                sub_hash = {}
                sub_hash["components"] = components
                sub_hash["hazards"] = hazards unless hazards.empty?
                sub_hash["specification"] = transform_specification(graph.specification)
                return { graph.name.to_s => sub_hash }
            end

            def parse_graph(hash)
                name = hash.keys.first.to_sym
                graph = Model::Graph.new(name)
                components = hash[hash.keys.first]["components"].map { |c| parse_component(graph, c) }
                hazards = hash[hash.keys.first]["hazards"]&.map { |h| parse_hazard(h) } || []
                specification = parse_specification(hash[hash.keys.first]["specification"])
                graph.components = components
                graph.hazards = hazards
                graph.specification = specification
                return graph
            end

            def transform_component(graph, component)
                variables = graph.variables.select_by_owner(component)
                return { component.name.to_s => {
                    "states" => component.states.map(&:to_s),
                    "variables" => variables.map { |v| transform_variable(v) },
                    "init" => transform_expression(component.init_expression),
                    "transitions" => component.transitions.map { |t| transform_transition(t) },
                    "represents_fault" => component.represents_fault
                }}
            end

            def parse_component(graph, hash)
                name = hash.keys.first
                states = hash[name]["states"].map(&:to_sym)
                variables = hash[name]["variables"].map { |v| parse_variable(name, v) }
                init_expression = parse_expression(hash[name]["init"])
                transitions = hash[name]["transitions"].map { |t| parse_transition(t) }
                represents_fault = hash[name]["represents_fault"]

                graph.variables += variables

                return Model::Component.new(name: name.to_sym, states: states, 
                    transitions: transitions, represents_fault: represents_fault, init_expression: init_expression)
            end

            def transform_variable(variable)
                return { variable.name.to_s => {
                    "range" => transform_variable_range(variable.range),
                    "init" => transform_expression(variable.init_expression)
                }}
            end

            def parse_variable(owner_name, hash)
                name = hash.keys.first
                range = hash[name]["range"]
                init_expression = parse_expression(hash[name]["init"])
                return Model::Variable.new(name.to_sym, range, owner_name.to_sym, nil, init: init_expression)
            end

            def transform_variable_range(range)
                return "#{range.first}..#{range.last}"if range.is_a?(Range)
                return range
            end

            def parse_variable_range(range)
                if range.is_a?(String) && range.match(/\A\d+\.\.\d+\z/)
                    first, last = range.split("..")[0], range.split("..")[-1]
                    return Range.new(first, last)
                end
                return range
            end

            def transform_expression(expression)
                return nil if expression.nil?
                return { 
                    "string" => expression.expression_string, 
                    "type" => expression.type.to_s
                }
            end

            def parse_expression(expression)
                return nil if expression.nil?
                string = expression["string"]
                type = expression["type"].to_sym
                return Model::ParsedExpression.new(string, type)
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

            def transform_hazard(hazard)
                return { "label" => hazard.text, "expression" => hazard.expression }
            end

            def parse_hazard(hash)
                return Model::Hazard.new(hash["label"], hash["expression"])
            end

            def transform_specification(specification)
                return specification.flatten().map { |spec|
                    { "label" => spec.text.strip, "expression" => transform_expression(spec.expression) }
                }
            end

            def parse_specification(array)
                specs = array.map { |hash|
                    Model::Spec.new(hash["label"], parse_expression(hash["expression"]), nil)
                }
                return  Model::Specification.new([Model::SpecSet.wrap(specs)])
            end

        end
    end
end
