module PgTools
    module Interpret

        class SpecSetContext

            # The text of this spec set as a string.
            attr_accessor :text

            # The sub-spec sets contained in this spec set
            attr_accessor :children

            attr_accessor :parent

            attr_accessor :assumption

            attr_accessor :parent_graph

            attr_accessor :source_location

            def initialize(text, parent_graph, parent, children = [])
                @text, @parent, @children = text, parent, children
                @parent_graph = parent_graph
                @source_location = parent_graph.parent_script.find_source_location()
            end

            def specify(text, &blk)
                subset = SpecSetContext.new(text, @parent_graph, self)
                subset.instance_eval(&blk)
                children << subset
            end

            def assuming(hash, &blk)
                assumption_text = hash.keys.first
                assumption_expression = hash.values.first
                subset = SpecSetContext.new("", @parent_graph, self)
                subset.assumption = { text: assumption_text, expression: assumption_expression }
                subset.instance_eval(&blk)
                children << subset
            end

            def no_errors()
                err_graphs = @parent_graph.components.select(&:represents_fault)
                expression = err_graphs.map { |g| "#{g.name} == No" }.join(" && ")
                return { "there are no errors" => "G ( #{expression} )" }
            end

            def it(hash)
                # TODO: Handle errors
                text = hash.keys.first
                expression = hash[text]
                children << SpecContext.new(text, expression.to_s, self)
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
