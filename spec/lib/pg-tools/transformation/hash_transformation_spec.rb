
RSpec.describe PgTools::Transform::HashTransformation do

    it "round trips" do
        transformer = PgTools::Transform::HashTransformation.new()

        script_file = File.join(PgTools.root, 'spec', 'res', 'weidezaun.rbx')

        model_1 = PgTools::Interpret::PgScript.new.interpret(script_file)
        yaml_1  = transformer.transform_graph(model_1).to_yaml

        model_2 = transformer.parse_graph(YAML.load(yaml_1))
        yaml_2  = transformer.transform_graph(model_2).to_yaml

        expect(yaml_1).to eq(yaml_2)
    end
end
