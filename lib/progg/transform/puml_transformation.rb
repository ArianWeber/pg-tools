

module Progg
    module Transform

        class PumlTransformation

            def transform_graph(graph)
                str = graph.components.map { |c| transform_component(graph, c) }.join("\n\n")
                # str += "\n#{transform_import_dependencies(programm_graph.components)}"
                
                # skinparam defaultFontName Helvetica
                return "@startuml Programmgraph\n#{str}\n@enduml"
            end
        
            # def transform_import_dependencies(components)
            #     dep_map = components.map { |c| [c.name, c.imported_variables.map(&:component_name).uniq] }.uniq
            #     return dep_map.map { |c, deps| deps.map { |d| "#{c} ..> #{d}" }}.flatten.join("\n")
            # end
        
            def transform_component(graph, component)
                # Transform component states
                states_s  = component.states.map { |s| transform_state(s) }.join("\n")
                # Transform component transitions
                trans_s   = component.transitions.map { |t| transform_transition(t) }.join("\n")
                # Transform component variables
                vars = graph.variables.select_by_owner(component.name)
                vars_s    = transform_variables(component, vars)
                # Transform the initial state 
                initial_s = transform_initial(graph, component)

                str = [ states_s, trans_s, vars_s, initial_s ].compact.join("\n")
                return "rectangle #{component.name} {\n#{indented(str)}\n}"
            end
        
            def transform_initial(graph, component)
                initial_state_name = "#{component.name}Initial"
                str = "circle #{initial_state_name}\n"
                str += "#{initial_state_name} --> #{component.initial_state}"
        
                vars = graph.variables.select_by_owner(component.name)
                init_var_s = vars.map { |v|
                    next if v.initial_value.nil?
                    "#{v.name} == #{v.initial_value}" 
                }.compact.join(' && ')
        
                str += ": #{init_var_s}" unless init_var_s.empty?
                str += "\n"
                return str
            end
        
            def transform_state(state)
                return "rectangle #{state}"
            end
        
            def transform_transition(transition)
                label = transition.precon.to_s 
                label += transition.guard.to_s
                label += "/ " + transition.action.to_s unless transition.action.nil?

                label = ": #{label}" unless label.strip.empty?
                return "#{transition.src_state} --> #{transition.tgt_state} #{label}"
            end
        
            def indented(str)
                str.split("\n").map { |s| "\t#{s}" }.join("\n")
            end
        
            def transform_variables(component, variables)
                return nil if variables.empty? 
                body =  variables.map { |v| "#{v.name} => #{transform_range(v.range)}" }.join("\n")
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