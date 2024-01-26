
module PgTools
    module Model

        class ParsedExpression

            # Guards and preconditions
            TYPE_GUARD = :guard
            # Actions
            TYPE_ACTION = :action
            # Term for the right part of an action
            TYPE_TERM = :term
            # Propositional logic
            TYPE_PL = :pl
            # Temporal logic (LTL or CTL)
            TYPE_TL = :tl
            # Linear temporal logic
            TYPE_LTL = :ltl
            # Computation tree logic
            TYPE_CTL = :ctl

            TYPES = [ TYPE_GUARD, TYPE_ACTION, TYPE_TERM, TYPE_PL, TYPE_TL, TYPE_LTL, TYPE_CTL ]

            attr_accessor :expression_string
            attr_accessor :source_location
            attr_accessor :type

            def initialize(expression_string, type, source_location: nil)
                expression_string = expression_string.to_s if expression_string.is_a?(Symbol)
                raise "Unknown expression type '#{type}'" unless TYPES.include?(type)
                raise "Not a string '#{expression_string}'::#{expression_string.class}" unless expression_string.is_a?(String)
                @expression_string = expression_string
                @source_location = source_location
                @type = type
            end

            def word_tokens()
                words = expression_string.scan(/[a-zA-Z_][a-zA-Z0-9_]*/).flatten.compact
                words = words.reject { |w| w.match(/\A[GFXRU]+\z/) }
                words = words.reject { |w| w == "TRUE" || w == "FALSE" }
                return words.map(&:to_sym)
            end

            # Splits the expression string into an array of tokens. e.g:
            # "(a == b) && 3 >= 2" becomes [ "(", "a", "==", "b", ")", "&&", "3", ">=", "2" ]
            # Note that this split method very much a hack at the moment
            def tokenize()
                return expression_string.split(/\s+/).map { |t|
                    t.end_with?(")") ? [ t.chop, ")" ] : t
                }.flatten.map { |t|
                    t.end_with?("(") ? [ t.chop, "(" ] : t
                }.flatten.map { |t|
                    t.start_with?(")") ? [ ")" , t.slice(1..-1)] : t
                }.flatten.map { |t|
                    t.start_with?("(") ? [ "(" , t.slice(1..-1)] : t
                }.flatten.reject(&:blank?)
            end

            def used_variables()
                return expression_string.scan(/[a-zA-Z_][a-zA-Z0-9_]*/).flatten.compact.map(&:to_sym)
            end

            def to_s()
                @expression_string
            end

            def assigned_variables()
                raise "Not an action" unless @type == TYPE_ACTION
                return expression_string.split("|").map { |sub_action|
                    sub_action.split(":=")[0].strip
                }
            end

        end

    end
end
