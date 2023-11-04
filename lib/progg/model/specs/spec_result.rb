module Progg
    module Model

        class SpecResult

            attr_reader :spec
            attr_accessor :success
            attr_accessor :trace

            def initialize(spec, success, trace)
                @spec, @success, @trace = spec, success, trace
            end

        end

    end
end
