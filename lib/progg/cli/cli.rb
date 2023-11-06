# Require all module files
Dir[File.join(__dir__, '*.rb')].sort.each { |file| require file }

require 'thor'
require 'plantuml_builder'

module Progg
    module Cli

        class BaseCommand < Thor

            desc "test", ""
            def test()
                script = Interpret::ProggScript.new
                model = script.interpret('program-graph.rb')

                runner = NuSMV::Runner.new()
                results = runner.run_specs(model)

                results.each { |result|
                    stat_string = result.success ? "PASSED".c_success : "FAILED".c_error
                    puts "[ #{stat_string} ] #{result.spec.text}"
                    puts "           #{result.spec.expression.c_blue}"
                    puts result.trace.map(&:c_error).join("\n") unless result.success
                }

                # File.write("pg.smv", nusmv_s)

                # output, err, status = Open3.capture3({}, "NuSMV-2.6.0-Darwin/bin/NuSMV pg.smv")

                # puts "Result: #{status}"
                # puts output
            end

            desc "dcca", ""
            def dcca()

                script = Interpret::ProggScript.new
                model = script.interpret('program-graph.rb')

                runner = NuSMV::Runner.new()
                
                dcca = Model::DCCA.new(model, runner)
                result = dcca.perform()

                result.each { |hazard, crit_sets|
                    puts hazard.text

                    crit_sets.each { |set|
                        puts "{ #{set.map(&:to_s).map(&:c_blue).join(', ')} }"
                    }
                }
            end

            desc "sim", ""
            def sim()
                script = Interpret::ProggScript.new
                model = script.interpret('program-graph.rb')

                simulator = Simulation::Simulator.new(model)
                states = simulator.run()

                puts states.map(&:to_s).join("\n---------\n")

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

                puml = Transform::PumlTransformation.new.transform_graph(model)
                File.write("diagram.puml", puml)
                # png = PlantumlBuilder::Formats::PNG.new(puml).load
                # File.binwrite("diagram.png", png)
                
                return

                nusmv_t = Transform::NuSmvTransformation.new
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