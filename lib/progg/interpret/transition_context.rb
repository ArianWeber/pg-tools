module Progg
    module Interpret

        class TransitionContext

            # Backreference to the component which owns this transition
            attr_accessor :parent_component

            # Source and target states as symbols
            attr_accessor :src_state, :tgt_state

            # Lists for the guars, actions and preconditions
            attr_accessor :guard_list, :action_list, :precon_list

            def initialize(parent_component, src_state, tgt_state)
                @parent_component = parent_component
                @src_state, @tgt_state = src_state, tgt_state
                @guard_list, @action_list, @precon_list = [], [], []
            end

            def guard(str)
                @guard_list << str
            end

            def action(str)
                action_list << str
            end

            def precon(str)
                @precon_list << str
            end

            def to_model()
                guard  = @guard_list.empty? ? nil : @guard_list.map { |s| "(#{s})"  }.join(' && ')
                precon = @precon_list.empty? ? nil : @precon_list.map { |s| "(#{s})"  }.join(' && ')
                action = @action_list.empty? ? nil : @action_list.join(' | ')
                Model::Transition.new(@src_state, @tgt_state, action: action, precon: precon, guard: guard)
            end


        end

    end
end
