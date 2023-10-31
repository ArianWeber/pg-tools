# Require all module files
Dir[File.join(__dir__, '*.rb')].sort.each { |file| require file }

require 'thor'
require 'plantuml_builder'

module Progg
    module Cli

        class BaseCommand < Thor

            desc "test", "Shows the ProgramGraph"
            def test()

                script = Interpret::ProggScript.new
                model = script.interpret('program-graph.rb')

                model.specification.flat_specs()

            end

            desc "show", "Shows the ProgramGraph"
            option :yell, :type => :boolean
            def show()

                puts options[:yell]

                # parser  = EbnfParser::ExpressionParser2.new(type: :Sum)
                # puts parser.parse!("1 + 2").map
                # return

                script = Interpret::ProggScript.new
                model = script.interpret('program-graph.rb')

                puml = Tranform::PumlTransformation.new.transform_graph(model)
                png = PlantumlBuilder::Formats::PNG.new(puml).load
                File.binwrite("diagram.png", png)
                
                return

                nusmv_t = Tranform::NuSmvTransformation.new
                nusmv_s = nusmv_t.transform_graph(model)

                puts "Validating Model..."
                errors = model.validate()
                puts errors.map(&:to_s).join("\n\n")

                nusmv_file = File.expand_path('nusmv.tmp', Settings.workdir)
                FileUtils.mkdir_p(File.dirname(nusmv_file))
                File.write(nusmv_file, nusmv_s)
                nurunner = NuSMV::NuSMVRunner.new()
                nurunner.load_file(nusmv_file)

                return


                desc_str = model.specs.map(&:text).join("\n")
                expr_str = model.specs.map(&:expression).map(&:to_s).join("\n")
                puts desc_str.line_combine(expr_str)

                # 
            end

        end

    end
end