
module Progg
    module Interpret

        class ComponentContext

            # Name of this component as a symbol
            attr_accessor :name

            # The list of states of this component as symbols
            attr_accessor :states_list

            # The list of transitions of this component
            attr_accessor :transition_list

            # The list of variables which is component has declared
            # and thus owns
            attr_accessor :owned_variables

            # Back reference to the parent graph
            attr_accessor :parent_graph

            # Flag on whether this component is used to represent a fault.
            attr_accessor :represents_fault

            def initialize(name, parent_graph)
                @name, @parent_graph = name, parent_graph
                @states_list, @transition_list = [], []
                @owned_variables = []
                @represents_fault = false
                # @imported_variables = []
            end

            # DSL method for declaring a variable to be owned by this component
            def var(hash)
                name = hash.keys.first
                range = hash[name]
                raise InterpretError.new("Variable name '#{name}' is not a symbol") unless name.is_a?(Symbol)
                raise InterpretError.new("Variable range '#{range}' is not a range of integers") unless range.is_a?(Range) && range.first.is_a?(Integer)
                variable = Model::Variable.new(name, range, @name)
                # puts variable.inspect
                variable.initial_value = hash[:init] if hash.key?(:init) 
                @owned_variables << variable
                return variable
            end

            # def use(varname)
            #     @imported_variables << varname
            # end

            # DSL method to define states which this component can have
            def states(*states)
                states.each { |state|
                    raise InterpretError.new("State '#{state}' is neither a string or symbol") unless state.is_a?(String) || state.is_a?(Symbol)
                    raise InterpretError.new("State '#{state}' was already declared for component '#{name}'") if @states_list.include?(state)
                }
                @states_list += states.map(&:to_sym)
            end

            # DSL method for the definition of one state
            def state(state)
                states(state)
            end

            # DSL method for the decleration of a transition between states of this component
            def transition(hash, &blk)
                from = hash.keys.first
                to =  hash[from]
                [ from, to ].each { |state| 
                    raise InterpretError.new("No such state '#{state}' in component '#{name}'") unless @states_list.include?(state)
                }
                transition = TransitionContext.new(self, from, to)
                transition.instance_eval(&blk) unless blk.nil?
                @transition_list << transition
                return transition
            end

            def to_model()
                params = {}
                # Use the name as is
                params[:name] = @name
                # Use states as is
                params[:states] = @states_list
                # Convert the transitions to their model representation
                params[:transitions] = @transition_list.map(&:to_model)
                params[:represents_fault] = @represents_fault
                # Create a model component using these params
                Model::Component.new(params)
            end

            # def resolve_imported_variables()
            #     other_cmp_vars = @parent_graph.components.map { |cmp|
            #         next if cmp == self
            #         cmp.variable_list
            #     }.compact.flatten
            #     return @imported_variables.map { |varname|
            #         var = other_cmp_vars.detect { |var| var.name == varname }
            #         raise "Can not find variable to import '#{varname}'. Variables: #{other_cmp_vars.map(&:name).join(', ')}" if var.nil?
            #         var
            #     }
            # end

        end

    end
end