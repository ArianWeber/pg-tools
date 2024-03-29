
module PgVerify
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

            # The source location of this components definition 
            attr_accessor :source_location

            attr_accessor :init_expressions

            def initialize(name, parent_graph)
                @name, @parent_graph = name, parent_graph
                @states_list, @transition_list = [], []
                @owned_variables = []
                @init_expressions = []
                @represents_fault = false
                @source_location = parent_graph.parent_script.find_source_location()
            end

            # DSL method for declaring a variable to be owned by this component
            def var(hash)
                raise InvalidDSL_var.new("Invalid argument '#{hash}'") unless hash.is_a?(Hash)
                name = hash.keys.first
                range = hash[name]
                raise "Variable name must be different to the component that owns it" if name.to_sym == @name
                raise InvalidDSL_var.new("Name '#{name}' is not a symbol") unless name.is_a?(Symbol)
                raise InvalidDSL_var.new("Range '#{range}' is not a range or array") unless (range.is_a?(Range) || range.is_a?(Array)) && range.first.is_a?(Integer)
                sloc = @parent_graph.parent_script.find_source_location()
                variable = Model::Variable.new(name, range, @name, sloc)

                # The "init" argument can be an expression, or a value out of the
                # range of that variable
                if hash.key?(:init)
                    init_expr = variable.values().include?(hash[:init]) \
                        ? "#{variable.name} == #{hash[:init]}" \
                        : hash[:init]
                    raise "Neither a string, nor a valid initial value: #{hash[:init]}" unless init_expr.is_a?(String)
                    variable.init_expression = Model::ParsedExpression.new(init_expr, Model::ParsedExpression::TYPE_PL)
                end

                @owned_variables << variable
                return variable
            end

            # DSL method to define states which this component can have
            def states(*states, init: nil)
                states.each { |state|
                    raise InvalidDSL_state.new("State '#{state}' is neither a string or symbol") unless state.is_a?(String) || state.is_a?(Symbol)
                    raise InvalidDSL_state.new("State '#{state}' was already declared for component '#{name}'") if @states_list.include?(state)
                    raise InvalidDSL_state.new("Initial state '#{init}' is none of #{state}") unless init.nil? || states.include?(init)
                }
                @states_list += states.map(&:to_sym)
                self.init("#{@name} == #{init}") unless init.nil?
            end

            # DSL method for the definition of one state
            def state(state)
                states(state)
            end

            # DSL method for the decleration of a transition between states of this component
            def transition(hash, &blk)
                raise InvalidDSL_transition.new("Invalid argument '#{hash}'") unless hash.is_a?(Hash)
                from = hash.keys.first
                to =  hash[from]
                [ from, to ].each { |state| 
                    next if @states_list.include?(state)
                    raise NoSuchStateError.new(state, self)
                }
                transition = TransitionContext.new(self, from, to)
                transition.instance_eval(&blk) unless blk.nil?
                @transition_list << transition
                return transition
            end

            def init(string)
                @init_expressions << string
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
                # Pass the source location
                params[:source_location] = @source_location
                # Pass the init expression
                unless @init_expressions.empty?
                    init_expression = @init_expressions.map { |e| "( #{e} )" }.join(" && ")
                    init_expression = Model::ParsedExpression.new(init_expression, Model::ParsedExpression::TYPE_PL) 
                    params[:init_expression] = init_expression
                end
                
                # Create a model component using these params
                Model::Component.new(params)
            end
            
        end

    end
end
