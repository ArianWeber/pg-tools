
module PgTools
    module Interpret

        class PgScript

            # List of components for this graph
            attr_accessor :components

            def initialize()
                @components = []
            end
        
            def interpret(file)
                file = File.expand_path(file)
                graph_ctx = Interpret::GraphContext.new
                Dir.chdir(File.dirname(file)) { eval(File.read(file), graph_ctx.get_binding(), file) }
                return graph_ctx.to_model()
            end
        
            def find_component(comp_name)
                @components.detect { |s| s.name == comp_name }
            end
        
            def find_component!(comp_name)
                component = find_component(comp_name)
                raise "No such component '#{comp_name}'" if component.nil?
                return component
            end
        
        end
    end
end