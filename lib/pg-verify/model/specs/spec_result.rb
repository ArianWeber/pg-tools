module PgVerify
    module Model

        class SpecResult

            attr_reader :spec
            attr_accessor :success
            attr_accessor :trace

            def initialize(spec, success, trace)
                @spec, @success, @trace = spec, success, trace
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
