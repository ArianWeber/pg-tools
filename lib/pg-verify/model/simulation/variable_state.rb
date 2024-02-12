module PgVerify
    module Model

        class VariableState

            attr_accessor :value_map

            def initialize(spec, success, trace)
                
            end

            def success?
                return @success
            end
            
            def failure?()
                return !@success
            end

        end

    end
end
