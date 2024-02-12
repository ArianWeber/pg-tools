module PgTools
    module EbnfParser

        class ExpressionParser2
            include EBNF::PEG::Parser
        
            # Abstract syntax tree from parse
            attr_reader :ast
            attr_accessor :rules
            attr_accessor :options
            attr_accessor :type

            # Rule: '[20] CMPOP ::= "<=" | ">=" | "<" | ">" | "==" | "!="'
            production(:CMPOP) do |input|
                output = input
                puts "CMPOP: '#{input}' -> '#{output}'"
                output
            end

            # Rule: '[19.2] _VARIABLE_2 ::= [a-zA-Z_]'
            production(:_VARIABLE_2) do |input|
                output = input
                puts "_VARIABLE_2: '#{input}' -> '#{output}'"
                output
            end

            # Rule: '[19.6] _VARIABLE_6 ::= [a-zA-Z0-9_]'
            production(:_VARIABLE_6) do |input|
                output = input
                puts "_VARIABLE_6: '#{input}' -> '#{output}'"
                output
            end

            # Rule: '[19.5] _VARIABLE_5 ::= _VARIABLE_6*'
            production(:_VARIABLE_5) do |input|
                output = input
                puts "_VARIABLE_5: '#{input}' -> '#{output}'"
                output
            end

            # Rule: '[19.4] _VARIABLE_4 ::= [a-zA-Z_]'
            production(:_VARIABLE_4) do |input|
                output = input
                puts "_VARIABLE_4: '#{input}' -> '#{output}'"
                output
            end

            # Rule: '[19.3] _VARIABLE_3 ::= [^('true'#x20|#x20'false')]'
            production(:_VARIABLE_3) do |input|
                output = input
                puts "_VARIABLE_3: '#{input}' -> '#{output}'"
                output
            end

            # Rule: '[19.1] _VARIABLE_1 ::= _VARIABLE_3 _VARIABLE_4 _VARIABLE_5'
            production(:_VARIABLE_1) do |input|
                output = input
                puts "_VARIABLE_1: '#{input}' -> '#{output}'"
                output
            end

            # Rule: '[19] VARIABLE ::= _VARIABLE_1 | _VARIABLE_2'
            production(:VARIABLE) do |input|
                output = input
                puts "VARIABLE: '#{input}' -> '#{output}'"
                output
            end

            # Rule: '[18.1] _BOOL_1 ::= "(" BoolExpr ")"'
            production(:_BOOL_1) do |input|
                output = input
                puts "_BOOL_1: '#{input}' -> '#{output}'"
                output
            end

            # Rule: '[18] BOOL ::= "true" | "false" | _BOOL_1'
            production(:BOOL) do |input|
                output = input
                puts "BOOL: '#{input}' -> '#{output}'"
                output
            end

            # Rule: '[17.3] _NUMBER_3 ::= [0-9]'
            production(:_NUMBER_3) do |input|
                output = input
                puts "_NUMBER_3: '#{input}' -> '#{output}'"
                output
            end

            # Rule: '[17.2] _NUMBER_2 ::= _NUMBER_3+'
            production(:_NUMBER_2) do |input|
                output = input
                puts "_NUMBER_2: '#{input}' -> '#{output}'"
                output
            end

            # Rule: '[17.1] _NUMBER_1 ::= "-"?'
            production(:_NUMBER_1) do |input|
                output = input
                puts "_NUMBER_1: '#{input}' -> '#{output}'"
                output
            end

            # Rule: '[17] NUMBER ::= _NUMBER_1 _NUMBER_2'
            production(:NUMBER) do |input|
                output = input
                puts "NUMBER: '#{input}' -> '#{output}'"
                output
            end

            # Rule: '[16.1] _Value_1 ::= "(" Expression ")"'
            production(:_Value_1) do |input|
                output = input
                puts "_Value_1: '#{input}' -> '#{output}'"
                output
            end

            # Rule: '[16] Value ::= NUMBER | VARIABLE | _Value_1'
            production(:Value) do |input|
                output = input
                puts "Value: '#{input}' -> '#{output}'"
                output
            end

            # Rule: '[15.3] _Product_3 ::= "*" | "/"'
            production(:_Product_3) do |input|
                output = input
                puts "_Product_3: '#{input}' -> '#{output}'"
                output
            end

            # Rule: '[15.2] _Product_2 ::= _Product_3 Value'
            production(:_Product_2) do |input|
                operation = input.first[:_Product_3]
                value     = input.last[:Value]
                output = "#{operation} #{value}"
                puts "_Product_2: '#{input}' -> '#{output}'"
                output
            end

            # Rule: '[15.1] _Product_1 ::= _Product_2*'
            production(:_Product_1) do |input|
                output = input
                puts "_Product_1: '#{input}' -> '#{output}'"
                output
            end

            # Rule: '[15] Product ::= Value _Product_1'
            production(:Product) do |input|
                value             = input.first[:Value]
                product_1_results = input.last[:_Product_1]
                output = "#{value} #{product_1_results.join(' ')}"
                puts "Product: '#{input}' -> '#{output}'"
                output
            end

            # Rule: '[14.3] _Sum_3 ::= "+" | "-"'
            production(:_Sum_3) do |input|
                output = input
                puts "_Sum_3: '#{input}' -> '#{output}'"
                output
            end

            # Rule: '[14.2] _Sum_2 ::= _Sum_3 Product'
            production(:_Sum_2) do |input|
                operation = input.first[:_Sum_3]
                value     = input.last[:Product]
                output = "#{operation} #{value}"
                puts "_Sum_2: '#{input}' -> '#{output}'"
                output
            end

            # Rule: '[14.1] _Sum_1 ::= _Sum_2*'
            production(:_Sum_1) do |input|
                output = input
                puts "_Sum_1: '#{input}' -> '#{output}'"
                output
            end

            # Rule: '[14] Sum ::= Product _Sum_1'
            production(:Sum) do |input|
                value         = input.first[:Product]
                sum_1_results = input.last[:_Sum_1]
                output = "#{value} #{sum_1_results.join(' ')}"
                puts "Sum: '#{input}' -> '#{output}'"
                output
            end

            # Rule: '[13] IntExpr ::= Sum'
            production(:IntExpr) do |input|
                output = input
                puts "IntExpr: '#{input}' -> '#{output}'"
                output
            end

            # Rule: '[12.3] _Comparison_3 ::= CMPOP Sum'
            production(:_Comparison_3) do |input|
                output = input
                puts "_Comparison_3: '#{input}' -> '#{output}'"
                output
            end

            # Rule: '[12.2] _Comparison_2 ::= _Comparison_3+'
            production(:_Comparison_2) do |input|
                output = input
                puts "_Comparison_2: '#{input}' -> '#{output}'"
                output
            end

            # Rule: '[12.1] _Comparison_1 ::= Sum _Comparison_2'
            production(:_Comparison_1) do |input|
                output = input
                puts "_Comparison_1: '#{input}' -> '#{output}'"
                output
            end

            # Rule: '[12] Comparison ::= BOOL | _Comparison_1'
            production(:Comparison) do |input|
                output = input
                puts "Comparison: '#{input}' -> '#{output}'"
                output
            end

            # Rule: '[11.1] _Negation_1 ::= "!" Comparison'
            production(:_Negation_1) do |input|
                output = input
                puts "_Negation_1: '#{input}' -> '#{output}'"
                output
            end

            # Rule: '[11] Negation ::= _Negation_1 | Comparison'
            production(:Negation) do |input|
                output = input
                puts "Negation: '#{input}' -> '#{output}'"
                output
            end

            # Rule: '[10.2] _Konjunction_2 ::= "&&" Negation'
            production(:_Konjunction_2) do |input|
                output = input
                puts "_Konjunction_2: '#{input}' -> '#{output}'"
                output
            end

            # Rule: '[10.1] _Konjunction_1 ::= _Konjunction_2*'
            production(:_Konjunction_1) do |input|
                output = input
                puts "_Konjunction_1: '#{input}' -> '#{output}'"
                output
            end

            # Rule: '[10] Konjunction ::= Negation _Konjunction_1'
            production(:Konjunction) do |input|
                output = input
                puts "Konjunction: '#{input}' -> '#{output}'"
                output
            end

            # Rule: '[9.2] _Disjunction_2 ::= "||" Konjunction'
            production(:_Disjunction_2) do |input|
                output = input
                puts "_Disjunction_2: '#{input}' -> '#{output}'"
                output
            end

            # Rule: '[9.1] _Disjunction_1 ::= _Disjunction_2*'
            production(:_Disjunction_1) do |input|
                output = input
                puts "_Disjunction_1: '#{input}' -> '#{output}'"
                output
            end

            # Rule: '[9] Disjunction ::= Konjunction _Disjunction_1'
            production(:Disjunction) do |input|
                output = input
                puts "Disjunction: '#{input}' -> '#{output}'"
                output
            end

            # Rule: '[8.2] _Implication_2 ::= "->" Disjunction'
            production(:_Implication_2) do |input|
                output = input
                puts "_Implication_2: '#{input}' -> '#{output}'"
                output
            end

            # Rule: '[8.1] _Implication_1 ::= _Implication_2*'
            production(:_Implication_1) do |input|
                output = input
                puts "_Implication_1: '#{input}' -> '#{output}'"
                output
            end

            # Rule: '[8] Implication ::= Disjunction _Implication_1'
            production(:Implication) do |input|
                output = input
                puts "Implication: '#{input}' -> '#{output}'"
                output
            end

            # Rule: '[7.3] _Equivalence_3 ::= "<->" | "<!>"'
            production(:_Equivalence_3) do |input|
                output = input
                puts "_Equivalence_3: '#{input}' -> '#{output}'"
                output
            end

            # Rule: '[7.2] _Equivalence_2 ::= _Equivalence_3 Implication'
            production(:_Equivalence_2) do |input|
                output = input
                puts "_Equivalence_2: '#{input}' -> '#{output}'"
                output
            end

            # Rule: '[7.1] _Equivalence_1 ::= _Equivalence_2*'
            production(:_Equivalence_1) do |input|
                output = input
                puts "_Equivalence_1: '#{input}' -> '#{output}'"
                output
            end

            # Rule: '[7] Equivalence ::= Implication _Equivalence_1'
            production(:Equivalence) do |input|
                output = input
                puts "Equivalence: '#{input}' -> '#{output}'"
                output
            end

            # Rule: '[6] BoolExpr ::= Equivalence'
            production(:BoolExpr) do |input|
                output = input
                puts "BoolExpr: '#{input}' -> '#{output}'"
                output
            end

            # Rule: '[5] Expression ::= BoolExpr | IntExpr'
            production(:Expression) do |input|
                output = input
                puts "Expression: '#{input}' -> '#{output}'"
                output
            end

            # Rule: '[4] Assignment ::= VARIABLE ":=" IntExpr'
            production(:Assignment) do |input|
                output = input
                puts "Assignment: '#{input}' -> '#{output}'"
                output
            end

            # Rule: '[3] Precon ::= BoolExpr'
            production(:Precon) do |input|
                output = input
                puts "Precon: '#{input}' -> '#{output}'"
                output
            end

            # Rule: '[2] Guard ::= BoolExpr'
            production(:Guard) do |input|
                output = input
                puts "Guard: '#{input}' -> '#{output}'"
                output
            end

            # Rule: '[1.2] _Action_2 ::= "|" Assignment'
            production(:_Action_2) do |input|
                output = input
                puts "_Action_2: '#{input}' -> '#{output}'"
                output
            end

            # Rule: '[1.1] _Action_1 ::= _Action_2*'
            production(:_Action_1) do |input|
                output = input
                puts "_Action_1: '#{input}' -> '#{output}'"
                output
            end

            # Rule: '[1] Action ::= Assignment _Action_1'
            production(:Action) do |input|
                output = input
                puts "Action: '#{input}' -> '#{output}'"
                output
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