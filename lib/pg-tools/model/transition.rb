module PgTools
    module Model

        class Transition

            # The source and target state of this transition as symbols
            attr_accessor :src_state, :tgt_state

            attr_accessor :guard, :precon, :action

            def initialize(src_state, tgt_state, guard: nil, action: nil, precon: nil)
                @src_state, @tgt_state = src_state, tgt_state
                @guard, @precon, @action = guard, precon, action
            end

             def validate!()
                # @actions.each { |action|
                #     # Calculate all illegal allocations
                #     # An allocation is illegal if it evaluates to a number outside of the range
                #     # of the assigned variable
                #     illegal_allocations = action.calc_illegal_allocations()
                #     # Only keep allocations as illegal if guards do not prevent them 
                #     illegal_allocations = precon.filter_allocation_set(illegal_allocations)

                #     raise("ValidationError") if illegal_allocations.length != 0
                # }
             end

             
             def to_s()     
                label = [ @precon, @guard ].map(&:to_s).reject(&:empty?).join(" && ")
                label += "/ " + @action.to_s unless @action.nil?

                label = ": #{label}" unless label.strip.empty?
                return "#{@src_state} -> #{@tgt_state} #{label}"
             end

        end

    end
end
