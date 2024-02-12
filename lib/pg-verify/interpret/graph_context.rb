
module PgTools
    module Interpret

        class GraphContext

            attr_accessor :parent_script
            # The list of currently declared components
            attr_accessor :components
            attr_accessor :specs
            attr_accessor :hazards
            # The name of this graph
            attr_accessor :name
    
            def initialize(name, parent_script)
                @parent_script = parent_script
                @name = name
                @components, @specs, @hazards = [], [], []
            end
    
            # DSL method for declaring a new component in this graph
            def graph(name, &blk)
                raise InvalidDSL_graph.new("Name '#{name}' is neither a symbol nor string") unless name.is_a?(Symbol) || name.is_a?(String)
                cmp = ComponentContext.new(name, self)
                cmp.instance_eval(&blk)
                @components << cmp
                return cmp
            end

            def transient(name)
                cmp = graph(name) do
                    all_states = [ :No, :Yes ]
                    states(*all_states)
                    all_states.product(all_states).each { |s1, s2| transition({ s1 => s2}) }
                end
                cmp.represents_fault = true
                return cmp
            end

            def persistent(name)
                cmp = graph(name) do
                    states(:No, :Yes)
                    transition :No => :No
                    transition :No => :Yes
                end
                cmp.represents_fault = true
                return cmp
            end

            def disable_error(name)
                cmp = @components.find { |c| c.name == name.to_sym }
                raise "No such component: #{name}" if cmp.nil?
                raise "Not an error component: #{name}" unless cmp.represents_fault
                cmp.transition_list = [ ]
                cmp.init_expressions = [ "#{name} == No" ]
            end

            def error(name)
                return name
            end

            # DSL method for declaring a new specification in this graph
            def specify(text, &blk)
                specset = SpecSetContext.new(text, self, nil)
                specset.instance_eval(&blk)
                @specs << specset
            end

            def hazard(hash)
                text = hash.keys.first
                expression = hash[text]
                @hazards << Model::Hazard.new(text, expression)
            end

            def to_model()
                components = @components.map(&:to_model)
                variables = Model::VariableSet.new(*@components.map(&:owned_variables).flatten())
                specification = Model::Specification.new(@specs.map { |s| s.to_model(nil) })
                Model::Graph.new(@name, components: components, variables: variables, specification: specification, hazards: @hazards)
            end
    
        end

    end
end
