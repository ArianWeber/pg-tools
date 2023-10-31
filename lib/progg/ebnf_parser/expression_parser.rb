module Progg
    module EbnfParser

        class ExpressionParser
            include EBNF::PEG::Parser
        
            # Abstract syntax tree from parse
            attr_reader :ast
            attr_accessor :rules
            attr_accessor :options
            attr_accessor :type


            # production(:Assignment, clear_packrat: true) do |value|
            #     # puts"Assignment:".ljust(25) + " #{value}"
            #     value
            # end

            # production(:BoolExpr, clear_packrat: true) do |value|
            #     # puts"BoolExpr:".ljust(25) + " #{value}"
            #     value
            # end

            # production(:Equivalence, clear_packrat: true) do |value|
            #     # puts"Equivalence:".ljust(25) + " #{value}"
            #     value
            # end

            # production(:Implication, clear_packrat: true) do |value|
            #     # puts"Implication:".ljust(25) + " #{value}"
            #     value
            # end

            # production(:Disjunction, clear_packrat: true) do |value|
            #     # puts"Disjunction:".ljust(25) + " #{value}"
            #     value
            # end

            # production(:Konjunction, clear_packrat: true) do |value|
            #     # puts"Konjunction:".ljust(25) + " #{value}"
            #     value
            # end

            # production(:Negation, clear_packrat: true) do |value|
            #     puts"Negation:".ljust(25) + "#{value.class} #{value}"
            #     value
            # end

            # production(:Comparison, clear_packrat: true) do |value|
            #     # puts"Comparison:".ljust(25) + " #{value}"
            #     value
            # end

            # production(:IntExpr, clear_packrat: true) do |value|
            #     # puts"IntExpr:".ljust(25) + " #{value}"
            #     value
            # end

            # production(:Sum, clear_packrat: true) do |value|
            #     # puts"Sum:".ljust(25) + " #{value}"
            #     value
            # end

            # production(:Product, clear_packrat: true) do |value|
            #     # puts"Product:".ljust(25) + " #{value}"
            #     valuex
            # end



            # [{:Value=>"3"}, {:_Product_1=>"* 5 * 5"}]
            production(:Product) do |value|
                val = value.first[:Value]
                prod1 = value.last[:_Product_1]
                ret = "#{val} #{prod1}"
                puts"Product:".ljust(25) + " #{value} -> #{ret}"
                ret
            end

            # A list of product operations plus values ['+ 4', '+ 4']
            production(:_Product_1) do |value|
                ret = value.join(' ')
                # puts"Product1:".ljust(25) + " #{value} -> #{ret}"
                ret
            end

            # A product operation plus value '+ 4'
            production(:_Product_2) do |value|
                op  = value.first[:_Product_3]
                val = value.last[:Value]
                ret = "#{op} #{val}"
                puts "Product2:".ljust(25) + " #{value} -> #{ret}"
                ret
            end

            # A product operation '*' '/'
            production(:_Product_3) do |value|
                operation = value
                value
            end

            # An atomic value like '3', 'true' or 'variable'
            production(:Value) do |value|
                value
            end
            # An expression in braces 
            production(:_Value_1) do |value|
                value.is_a?(Hash) ? value[:Expression] : value
            end
            # A number like '42' or '-7'
            production(:NUMBER) do |value|
                value
            end
            # A boolean value like 'true' or 'false'
            production(:BOOL) do |value|
                value
            end
            # A boolean expression in braces like '(3 < 2 <-> true)'
            production(:_BOOL_1) do |value|
                value.is_a?(Hash) ? value[:BoolExpr] : value
                value
            end
            # Integer comparison operators like '>' and '!='
            production(:CMPOP) do |value|
                value
            end
            

            def initialize(type: :Expression)
                @type = type
                @options = {}
                # @options[:logger] = Logger.new(STDERR)
                # @options[:logger].level = :info
                # @options[:logger].formatter = lambda {|severity, datetime, progname, msg| "#{severity} #{msg}\n"}
        
                # Intantiate the grammar
                ebnf_file = File.expand_path("expressions.ebnf", __dir__)
                # Perform PEG-specific transformation to the associated rules, which will be passed directly to the parser.
                @rules = EBNF.parse(File.open(ebnf_file)).make_peg.ast

                skeletton = EBNF.parse(File.open(ebnf_file)).make_peg.to_s.split("\n").reverse.map { |line|
                    rule = line.split(/\s+/)[1]
                    comment = "# Rule: '#{line.split(/\s+/).join(" ")}'"
                    puts_line = 'puts "' + rule + ': \'#{input}\' -> \'#{output}\'"'
                    body    = "\toutput = input\n\t#{puts_line}\n\toutput"
                    method =  "production(:#{rule}) do |input|\n#{body}\nend"
                    [comment, method].join("\n")
                }.join("\n\n")
                File.write(File.expand_path("expressions.rb", __dir__), skeletton)

                # TODO: Remove 
                File.write(File.expand_path("expressions.peg", __dir__), EBNF.parse(File.open(ebnf_file)).make_peg)
            end

            def parse!(input)
                raise "Empty expression!" if input.nil? || input.empty?
                ast_map = parse(input, @type, @rules, **@options)
                Ast.new(ast_map)
            end
        
            # def evaluate!(input, type: :Expression)
            #     # TODO: Change this to actually eval (Also in specs)
            #     parse(input, type, @rules, **@options)
            # end

            # def accepts?(input, type: :Expression)
            #     begin
            #         evaluate!(input, type: type) 
            #         return true
            #     rescue EBNF::PEG::Parser::Error => e 
            #         return false
            #     end
            # end
        end

    end
end