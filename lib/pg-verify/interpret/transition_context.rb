module PgVerify
    module Interpret

        class TransitionContext

            TempExpression = Struct.new(:string, :source_location)

            # Back-reference to the component which owns this transition
            attr_accessor :parent_component

            # Source and target states as symbols
            attr_accessor :src_state, :tgt_state

            # Strings for the guars, actions and preconditions
            attr_accessor :guard, :action, :precon

            # Source location for this transition
            attr_accessor :source_location

            def initialize(parent_component, src_state, tgt_state)
                @parent_component = parent_component
                @src_state, @tgt_state = src_state, tgt_state
                @source_location = @parent_component.parent_graph.parent_script.find_source_location()
            end

            def guard(str)
                raise "Guard was already defined!" unless @guard.nil?
                raise InvalidDSL_expression.new("guard", "Expression is neither a sting, nor symbol: #{str}") unless str.is_a?(String) | str.is_a?(Symbol)
                @guard = Model::ParsedExpression.new(str, Model::ParsedExpression::TYPE_GUARD)
                @guard.source_location = @parent_component.parent_graph.parent_script.find_source_location()
            end

            def action(str)
                raise "Action was already defined!" unless @action.nil?
                raise InvalidDSL_expression.new("action", "Expression is neither a sting, nor symbol: #{str}") unless str.is_a?(String) | str.is_a?(Symbol)
                @action = Model::ParsedExpression.new(str, Model::ParsedExpression::TYPE_ACTION)
                @action.source_location = @parent_component.parent_graph.parent_script.find_source_location()
            end

            def precon(str)
                raise "Precondition was already defined!" unless @guard.nil?
                raise InvalidDSL_expression.new("precon", "Expression is neither a sting, nor symbol: #{str}") unless str.is_a?(String) | str.is_a?(Symbol)
                @precon = Model::ParsedExpression.new(str, Model::ParsedExpression::TYPE_GUARD)
                @precon.source_location = @parent_component.parent_graph.parent_script.find_source_location()
            end

            def to_model()
                Model::Transition.new(@src_state, @tgt_state, action: @action, precon: @precon, guard: @guard)
            end


        end

    end
end
