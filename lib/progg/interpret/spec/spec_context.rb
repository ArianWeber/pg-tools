module Progg
    module Interpret

        class SpecContext
            # The text of this spec as a string.
            attr_accessor :text

            # The LTL/CTL expression of this spec
            attr_accessor :expression

            # The parent set of this spec 
            attr_accessor :parent

            def initialize(text, expression, parent)
                @text, @expression, @parent = text, expression, parent
            end

            def ltl(expression)
                @expression << expression
            end

            def to_model(parent)
                return Model::Spec.new(@text, @expression, parent)
            end

        end

    end
end
