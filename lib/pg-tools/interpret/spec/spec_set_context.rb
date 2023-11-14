module PgTools
    module Interpret

        class SpecSetContext

            # The text of this spec set as a string.
            attr_accessor :text

            # The sub-spec sets contained in this spec set
            attr_accessor :children

            attr_accessor :parent

            attr_accessor :assumption

            def initialize(text, parent, children = [])
                @text, @parent, @children = text, parent, children
            end

            def specify(text, &blk)
                subset = SpecSetContext.new(text, self)
                subset.instance_eval(&blk)
                children << subset
            end

            def assuming(hash, &blk)
                assumption_text = hash.keys.first
                assumption_expression = hash.values.first
                subset = SpecSetContext.new("", self)
                subset.assumption = { text: assumption_text, expression: assumption_expression }
                subset.instance_eval(&blk)
                children << subset
            end

            def it(text_or_hash, &blk)
                spec = nil
                if text_or_hash.is_a?(Hash)
                    text = text_or_hash.keys.first
                    expression = text_or_hash[text]
                    spec = SpecContext.new(text, expression.to_s, self)
                else
                    spec = SpecContext.new(text, nil, self)
                end
                spec.instance_eval(&blk) unless blk.nil?
                children << spec
            end

            def ltl()
                return LTLBuilder.new()
            end

            def to_model(parent)
                model = Model::SpecSet.new(@text, @assumption, parent, [])
                model.children = @children.map { |child| child.to_model(model) }
                return model
            end

        end
    end

end
