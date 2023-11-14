module PgTools
    module Model

        # A single specification and a leave in the tree
        class Spec

            # The text of this spec as a string.
            attr_accessor :text

            # The LTL/CTL expression of this spec
            attr_accessor :expression

            # The parent specification set for this node
            attr_accessor :parent

            def initialize(text, expression, parent)
                @text, @expression, @parent = text, expression, parent
            end

            def parent?
                return !@parent.nil?
            end

            def linage()
                return parents() + [ self ]
            end

            def parents()
                array = []
                current = self
                while(current.parent?)
                    current = current.parent
                    array << current
                end
                return array.reverse()
            end

        end

    end
end
