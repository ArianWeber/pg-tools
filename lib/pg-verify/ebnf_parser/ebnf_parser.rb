require 'ebnf'
require 'ebnf/terminals'
require 'ebnf/peg/parser'
require 'sxp'
require 'logger'
require 'json'

# Require all module files
Dir[File.join(__dir__, "**", '*.rb')].sort.each { |file| require file }

module PgVerify
    module EbnfParser

        def self.parse_expression(expression, type: :Expression)
            parser = ExpressionParser.new(type: type)
            error, ast = nil, nil
            begin
                ast = parser.parse!(expression)
            rescue EBNF::PEG::Parser::Error => e 
                error = e
            end
            return ParserResult.new(ast, error)
        end

    end
end
