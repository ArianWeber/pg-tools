
module Progg
    module Interpret

        class GraphContext

            # The list of currently declared components
            attr_accessor :components

            attr_accessor :specs
    
            def initialize()
                @components = []
                @specs = []
            end
    
            # DSL method for declaring a new component in this graph
            def component(name, &blk)
                cmp = ComponentContext.new(name, self)
                cmp.instance_eval(&blk)
                @components << cmp
                return cmp
            end

            # DSL method for declaring a new specification in this graph
            def specify(text, &blk)
                specset = SpecSetContext.new(text, nil)
                specset.instance_eval(&blk)
                @specs << specset
            end

            def to_model()
                components = @components.map(&:to_model)
                variables = Model::VariableSet.new(*@components.map(&:owned_variables).flatten())
                specification = Model::Specification.new(@specs.map { |s| s.to_model(nil) })
                Model::Graph.new(components: components, variables: variables, specification: specification)
            end
    
            def get_binding()
                binding()
            end
    
        end

    end
end
