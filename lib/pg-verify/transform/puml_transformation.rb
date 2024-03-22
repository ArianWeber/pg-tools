

module PgVerify
    module Transform

        class PumlTransformation

            attr_accessor :render_labels
            attr_accessor :render_precons
            attr_accessor :render_guards
            attr_accessor :render_actions

            def initialize(render_options = {})
                @render_labels = render_options[:render_labels].nil? ? true : render_options[:render_labels]
                @render_precons = render_options[:render_precons].nil? ? true : render_options[:render_precons]
                @render_guards = render_options[:render_guards].nil? ? true : render_options[:render_guards]
                @render_actions = render_options[:render_actions].nil? ? true : render_options[:render_actions]
            end

            def transform_graph(graph, variable_state: nil, only: nil)

                components = graph.components
                components = components.select { |c| only.map(&:to_s).include?(c.name.to_s) } unless only.nil?

                parts = []
                parts << components.map { |c| transform_component(graph, c, variable_state) }.join("\n\n")
                parts << transform_variable_state(graph, variable_state) unless variable_state.nil?
                parts = parts.compact.join("\n\n")
                return "@startuml Programmgraph\n#{parts}\n@enduml\n"
            end

            def transform_variable_state(graph, variable_state)
                state_variables = graph.state_variables()
                return state_variables.map { |var|
                    state = variable_state[var.name]
                    "rectangle #{state} as #{var.name}_#{state} #{Settings.puml.active_state_color}"
                }.join("\n")
            end
        
            def transform_component(graph, component, variable_state)
                # Transform component states
                states_s  = component.states.map { |s| transform_state(component, s) }.join("\n")
                # Transform component transitions
                trans_s   = component.transitions.map { |t| transform_transition(component, t) }.join("\n")
                # Transform component variables
                vars = graph.variables.select_by_owner(component.name)
                vars_s    = transform_variables(component, vars, variable_state)
                # Transform the initial state 
                initial_s = transform_initial(graph, component)

                str = [ states_s, trans_s, vars_s, initial_s ].compact.join("\n")
                return "rectangle #{component.name} {\n#{indented(str)}\n}"
            end
        
            # Render an initial state when possible.
            # For the very common case, that the component defines a single initial state
            # detect that using a regex. When the component does not define a single initial state
            # which we optionally do not enforce (as it can be useful) we just omit this.
            def transform_initial(graph, component)
                return "" if component.init_expression.nil?
                candidates = component.init_expression.expression_string.scan(/#{component.name}\s+==\s+(\w+)/).flatten
                return "" unless candidates.length == 1
                initial_state = candidates.first

                initial_state_name = "#{component.name}_initial"
                str = "circle initial as #{initial_state_name} \n"
                str += "#{initial_state_name} --> #{component.name}_#{initial_state}"
        
                vars = graph.variables.select_by_owner(component.name)
                init_var_s = vars.map(&:init_expression).compact.join(' && ')
        
                str += ": #{init_var_s}" unless init_var_s.empty?
                str += "\n"
                return str
            end
        
            def transform_state(component, state)
                return "rectangle #{state} as #{component.name}_#{state}"
            end
        
            def transform_transition(component, transition)
                precon = @render_precons ? transition.precon : nil
                guard  = @render_guards  ? transition.guard : nil
                action = @render_actions ? transition.action : nil

                label = [ precon, guard ].map(&:to_s).reject(&:empty?).join(" && ")
                label += " / " + action.to_s unless action.nil?
                label = ": #{label}" unless label.strip.empty?
                label = "" unless @render_labels
                return "#{component.name}_#{transition.src_state} --> #{component.name}_#{transition.tgt_state} #{label}"
            end
        
            def indented(str)
                str.split("\n").map { |s| "\t#{s}" }.join("\n")
            end
        
            def transform_variables(component, variables, variable_state)
                return nil if variables.empty?
                body =  variables.map { |var| 
                    value = variable_state.nil? ? transform_range(var.range) : variable_state[var.name]
                    "#{var.name} => #{value}"
                }.join("\n")
                return "map #{component.name}Variables {\n#{body}\n}"
            end
        
            def transform_range(range)
                return range.to_s if range.is_a?(Range)
                return range.map(&:to_s).join(', ') if range.is_a?(Array)
                raise "Unknown type for range '#{range}::#{range.class}'"
            end

        end


    end
end