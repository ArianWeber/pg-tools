
RSpec.describe PgTools::Model::ParsedExpression do

    describe "#tokenize" do

        examples = {
            "Switch == off" => [ "Switch", "==", "off" ],
        }

        examples.each { |string, tokens|
            it "tokenizes '#{string}'" do
                expression = PgTools::Model::ParsedExpression.new(string, nil)
                expect(expression).to eq(tokens)
            end
        }

    end


end
