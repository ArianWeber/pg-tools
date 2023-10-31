
module Progg
    module Model

        class ParsedExpression

            attr_accessor :expression_string
            attr_accessor :type
            attr_accessor :ast

            def initialize(expression_string, type)
                # result = EbnfParser.parse_expression(expression_string, type: type)
                # TODO: Custom error
                # raise result.error if result.error?()

                @expression_string = expression_string
                # @type = type
                # @ast  = result.ast
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