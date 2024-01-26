
RSpec.describe PgTools::Model::ParsedExpression do

    describe "#tokenize" do

        examples = {
            "Switch == off" => [ "Switch", "==", "off" ],
            "(a == b) && 3 >= 2" => [ "(", "a", "==", "b", ")", "&&", "3", ">=", "2" ],
            "G (hello >= 2 => F blubb == 3)" => ["G", "(", "hello", ">=", "2", "=>", "F", "blubb", "==", "3", ")"]
        }

        examples.each { |string, tokens|
            it "tokenizes '#{string}'" do
                expression = PgTools::Model::ParsedExpression.new(string, :guard)
                expect(expression.tokenize).to eq(tokens)
            end
        }

    end


end
