module PgVerify
    module Interpret

        class SpecContext
            # The text of this spec as a string.
            attr_accessor :text

            # The LTL/CTL expression of this spec
            attr_accessor :expression

            # The parent set of this spec 
            attr_accessor :parent

            attr_accessor :source_location

            def initialize(text, expression, parent)
                @text, @expression, @parent = text, expression, parent
                @expression = Model::ParsedExpression.new(expression, Model::ParsedExpression::TYPE_TL)
                @expression.source_location = parent.parent_graph.parent_script.find_source_location()
                @source_location = parent.parent_graph.parent_script.find_source_location()
            end

            def to_model(parent)
                spec = Model::Spec.new(@text, @expression, parent)
                spec.source_location = @source_location
                return spec
            end

        end

    end
end
