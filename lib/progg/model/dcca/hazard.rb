module Progg
    module Model

        class Hazard

            attr_accessor :text
            attr_accessor :expression

            def initialize(text, expression)
                @text, @expression = text, expression
            end

        end

    end
end
