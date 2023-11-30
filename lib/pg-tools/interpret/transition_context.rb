module PgTools
    module Interpret

        class TransitionContext

            TempExpression = Struct.new(:string, :source_location)

            # Back-reference to the component which owns this transition
            attr_accessor :parent_component

            # Source and target states as symbols
            attr_accessor :src_state, :tgt_state

            # Lists for the guars, actions and preconditions
            attr_accessor :guard_list, :action_list, :precon_list

            attr_accessor :source_location

            def initialize(parent_component, src_state, tgt_state)
                @parent_component = parent_component
                @src_state, @tgt_state = src_state, tgt_state
                @guard_list, @action_list, @precon_list = [], [], []
                @source_location = @parent_component.parent_graph.parent_script.find_source_location()
            end

            def guard(str)
                raise InvalidDSL_expression.new("guard", "Expression is neither a sting, nor symbol: #{str}") unless str.is_a?(String) | str.is_a?(Symbol)
                @guard_list << TempExpression.new(str, @parent_component.parent_graph.parent_script.find_source_location())
            end

            def action(str)
                raise InvalidDSL_expression.new("action", "Expression is neither a sting, nor symbol: #{str}") unless str.is_a?(String) | str.is_a?(Symbol)
                @action_list << TempExpression.new(str, @parent_component.parent_graph.parent_script.find_source_location())
            end

            def precon(str)
                raise InvalidDSL_expression.new("precon", "Expression is neither a sting, nor symbol: #{str}") unless str.is_a?(String) | str.is_a?(Symbol)
                @precon_list << TempExpression.new(str, @parent_component.parent_graph.parent_script.find_source_location())
            end

            def to_model()
                guard_str  = @guard_list.empty? ? nil : @guard_list.map(&:string).map { |s| "(#{s})"  }.join(' && ')
                precon = @precon_list.empty? ? nil : @precon_list.map(&:string).map { |s| "(#{s})"  }.join(' && ')
                action = @action_list.empty? ? nil : @action_list.map(&:string).join(' | ')

                precon = precon.nil? ? nil : Model::ParsedExpression.new(precon, Model::ParsedExpression::TYPE_GUARD)
                precon&.source_location = @precon_list.first.source_location
                guard  = guard.nil? ? nil : Model::ParsedExpression.new(guard, Model::ParsedExpression::TYPE_GUARD)
                guard&.source_location = @guard_list.first.source_location
                action = action.nil? ? nil : Model::ParsedExpression.new(action, Model::ParsedExpression::TYPE_ACTION)
                action&.source_location = @action_list.first.source_location
                
                Model::Transition.new(@src_state, @tgt_state, action: action, precon: precon, guard: guard)
            end


        end

    end
end
