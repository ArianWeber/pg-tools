
RSpec.describe PgTools::Transform::HashTransformation do

    test_files = Dir[File.join(PgTools.root, "integration_tests", "ruby_dsl", "*.rb")]

    test_files.each { |test_file|
        it "round trips for '#{File.basename(test_file)}'" do
            transformer = PgTools::Transform::HashTransformation.new()

            model_1 = PgTools::Interpret::PgScript.new.interpret(test_file).first
            yaml_1  = transformer.transform_graph(model_1).to_yaml

            model_2 = transformer.parse_graph(YAML.load(yaml_1))
            yaml_2  = transformer.transform_graph(model_2).to_yaml

            expect(yaml_1).to eq(yaml_2)
        end
    }

end
