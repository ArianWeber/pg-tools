
RSpec.describe Progg::EbnfParser::ExpressionParser do

    def parser(type: :Expression)
        Progg::EbnfParser::ExpressionParser.new(type: type)
    end

    INT_ATOMS  = [ "1334", "-42", "variable" ]
    BOOL_ATOMS = [ "true", "false" ]
    ATOMS      = (INT_ATOMS + BOOL_ATOMS).uniq
    CMP_OPS    = [ '<', '>', '<=', '>=', '==' ]
    LOGIC_OPS  = [ '->', '<->', '<!>', '||', '&&' ]

    context "when evaluating atomic expressions" do
        ATOMS.each { |atom|
            it "accepts '#{atom}'" do
                parser().parse!(atom)
            end
        }
        INT_ATOMS.each { |atom|
            it "accepts '#{atom}' as an int expression" do
                parser(type: :IntExpr).parse!(atom)
            end
        }
        BOOL_ATOMS.each { |atom|
            it "accepts '#{atom}' as a bool expression" do
                parser(type: :BoolExpr).parse!(atom)
            end
        }
        INT_ATOMS.each { |atom|
            it "rejects '#{atom}' as a bool expression" do
                expect { parser(type: :BoolExpr).parse!(atom) }.to raise_error(EBNF::PEG::Parser::Error)
               
            end
        }
        BOOL_ATOMS.each { |atom|
            it "rejects '#{atom}' as an int expression" do
                expect { parser(type: :IntExpr).parse!(atom) }.to raise_error(EBNF::PEG::Parser::Error)
               
            end
        }
    end

    context "when using braces" do
        INT_ATOMS.each { |atom|
            expression = "(#{atom})"
            it "accepts '#{expression}' as an int expression" do
                parser(type: :IntExpr).parse!(expression)
            end
        }
        BOOL_ATOMS.each { |atom|
            expression = "(#{atom})"
            it "accepts '#{expression}' as an int expression" do
                parser(type: :BoolExpr).parse!(expression)
            end
        }
    end

    context "when evaluating comparison expressions" do
        INT_ATOMS.product(INT_ATOMS, CMP_OPS).map(&:flatten).each { |i1, i2, cmp| 
            expression = "#{i1} #{cmp} #{i2}"
            it "accepts '#{expression}'" do
                parser().parse!(expression)
            end
        }
        INT_ATOMS.product(BOOL_ATOMS, CMP_OPS).map(&:flatten).each { |i, b, cmp| 
            next if b == 'variable'
            expression = "#{i} #{cmp} #{b}"
            it "rejects '#{expression}'" do
                expect { parser().parse!(expression) }.to raise_error(EBNF::PEG::Parser::Error)
            end
            expression = "#{b} #{cmp} #{i}"
            it "rejects '#{expression}'" do
                expect { parser().parse!(expression) }.to raise_error(EBNF::PEG::Parser::Error)
            end
        }
    end

    context "when evaluating logic expressions" do
        BOOL_ATOMS.product(BOOL_ATOMS, LOGIC_OPS).map(&:flatten).each { |b1, b2, cmp| 
            expression = "#{b2} #{cmp} #{b2}"
            it "accepts '#{expression}'" do
                parser().parse!(expression)
            end
        }
    end

    context "when evaluating some common expressions" do

        to_accept = {
            BoolExpr: [
                "Position + Velocity < 500 && Velocity - Deceleration > 0",
                "Velocity - BrakePower > 0",
                "(Velocity - BrakePower > 0)",
                "((true -> true) && ((false) -> true) ) -> (false -> true)",
                "((true -> true) && ((false) -> true) ) -> (false -> true)".gsub("true", "(Position + Velocity < 500 && Velocity - Deceleration > 0)")
            ],
            IntExpr: [
                "5 + 8 * (X / 15) - 10",
            ],
            Assignment: [
                "Position := (Position + Velocity)"
            ],
            Action: [
                "Velocity := Velocity - deceleration | Position := Position + Velocity * 3"
            ]
        }
        
        to_accept.each { |type, expressions|
            expressions.each { |expression|
                it "accepts '#{expression}' as a #{type}" do
                    parser(type: type).parse!(expression)
                end
            }
        }

    end

end