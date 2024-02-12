
RSpec.describe PgVerify::Transform::HashTransformation do

    test_files = Dir[File.join(PgVerify.root, "integration_tests", "ruby_dsl", "*.rb")].sort

    test_files.each { |test_file|
        it "round trips for '#{File.basename(test_file)}'" do
            transformer = PgVerify::Transform::HashTransformation.new()

            model_1 = PgVerify::Interpret::PgScript.new.interpret(test_file).first
            yaml_1  = transformer.transform_graph(model_1).to_yaml

            model_2 = transformer.parse_graph(YAML.load(yaml_1))
            yaml_2  = transformer.transform_graph(model_2).to_yaml

            expect(yaml_1).to eq(yaml_2)
        end
    }

end
