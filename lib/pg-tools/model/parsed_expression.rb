
module PgTools
    module Model

        class ParsedExpression

            # Guards and preconditions
            TYPE_GUARD = :guard
            # Actions
            TYPE_ACTION = :action
            # Propositional logic
            TYPE_PL = :pl
            # Temporal logic (LTL or CTL)
            TYPE_TL = :tl
            # Linear temporal logic
            TYPE_LTL = :ltl
            # Computation tree logic
            TYPE_CTL = :ctl

            attr_accessor :expression_string

            attr_accessor :source_location

            def initialize(expression_string, type, source_location: nil)
                @expression_string = expression_string
                @source_location = source_location
            end

            def word_tokens()
                words = expression_string.scan(/[a-zA-Z_][a-zA-Z0-9_]*/).flatten.compact
                words = words.reject { |w| w.match(/\A[GFXRU]+\z/) }
                words = words.reject { |w| w == "TRUE" || w == "FALSE" }
                return words.map(&:to_sym)
            end

            def used_variables()
                return expression_string.scan(/[a-zA-Z_][a-zA-Z0-9_]*/).flatten.compact.map(&:to_sym)
            end

            def to_s()
                @expression_string
            end

        end

    end
end
