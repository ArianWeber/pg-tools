# Require all module files
Dir[File.join(__dir__, '*.rb')].sort.each { |file| require file }

require 'thor'
require 'plantuml_builder'

module PgTools
    module Cli

        class ShowCommand < Thor

            desc "puml", "Shows the ProgramGraph"
            method_option :only, :type => :array, repeatable: true
            method_option :hide, :type => :array, repeatable: true
            method_option :script, :type => :string
            def puml()
                script_file = options[:script] || Settings.ruby_dsl.default_script_name
                script = Interpret::PgScript.new
                model = script.interpret(script_file)
                components = self.class.select_components(options[:only], options[:hide], model)
                puml = Transform::PumlTransformation.new.transform_graph(model, only: components)
                puts puml
            end

            desc "png", "Shows the ProgramGraph"
            method_option :only, :type => :array, repeatable: true
            method_option :hide, :type => :array, repeatable: true
            method_option :script, :type => :string
            def png()
                script_file = options[:script] || Settings.ruby_dsl.default_script_name
                script = Interpret::PgScript.new
                model = script.interpret(script_file)
                components = self.class.select_components(options[:only], options[:hide], model)
                puml = Transform::PumlTransformation.new.transform_graph(model, only: components)
                png = PlantumlBuilder::Formats::PNG.new(puml).load
                File.binwrite("program-graph.png", png)
            end

            desc "yaml", "Shows the ProgramGraph"
            def yaml()
                script = Interpret::PgScript.new
                model = script.interpret('program-graph.rb')
                hash = Transform::HashTransformation.new.transform_graph(model)
                puts hash.to_yaml
            end

            desc "yaml", "Shows the ProgramGraph"
            def json()
                script = Interpret::PgScript.new
                model = script.interpret('program-graph.rb')
                hash = Transform::HashTransformation.new.transform_graph(model)
                puts JSON.pretty_generate(hash)
            end

            def self.select_components(only_arg, hide_arg, model)
                only = (only_arg || []).flatten.map(&:to_s).map(&:downcase)
                hide = (hide_arg || []).flatten.map(&:to_s).map(&:downcase)
                components = model.components.map(&:name)
                components = components.select { |c| only.include?(c.to_s.downcase) } unless only.empty?
                components = components.reject { |c| hide.include?(c.to_s.downcase) } unless hide.empty?
                return components
            end

        end

        class BaseCommand < Thor

            desc "test", ""
            def test()
                script = Interpret::PgScript.new
                model = script.interpret('program-graph.rb')

                results = NuSMV::Runner.new().run_specs(model)

                results.each { |result|
                    stat_string = result.success ? "PASSED".c_success : "FAILED".c_error
                    puts "[ #{stat_string} ] #{result.spec.text}"
                    puts "           #{result.spec.expression.c_blue}"
                    puts result.trace.map(&:c_error).join("\n") unless result.success
                }
            end

            desc "dcca", ""
            def dcca()
                script = Interpret::PgScript.new
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
                script = Interpret::PgScript.new
                model = script.interpret('program-graph.rb')

                simulator = Simulation::Simulator.new(model)
                states = simulator.run(steps: 5)

                FileUtils.mkdir_p("video")

                states.each_with_index.map { |state, index|
                    puml = Transform::PumlTransformation.new.transform_graph(model, variable_state: state)
                    puts puml
                    png = PlantumlBuilder::Formats::PNG.new(puml).load
                    File.binwrite("video/#{index}.png", png)
                }

                puts states.map(&:to_s).join("\n---------\n")

            end

            desc "show", ""
            subcommand 'show', ShowCommand

            # desc "show", "Shows the ProgramGraph"
            # def show()
            #     script = Interpret::PgScript.new
            #     model = script.interpret('program-graph.rb')

            #     puml = Transform::PumlTransformation.new.transform_graph(model)
            #     File.write("diagram.puml", puml)
            #     # png = PlantumlBuilder::Formats::PNG.new(puml).load
            #     # File.binwrite("diagram.png", png)
                
            #     return

            #     nusmv_t = Transform::NuSmvTransformation.new
            #     nusmv_s = nusmv_t.transform_graph(model)

            #     puts "Validating Model..."
            #     errors = model.validate()
            #     puts errors.map(&:to_s).join("\n\n")

            #     nusmv_file = File.expand_path('nusmv.tmp', Settings.workdir)
            #     FileUtils.mkdir_p(File.dirname(nusmv_file))
            #     File.write(nusmv_file, nusmv_s)
            #     nurunner = NuSMV::NuSMVRunner.new()
            #     nurunner.load_file(nusmv_file)

            #     return


            #     desc_str = model.specs.map(&:text).join("\n")
            #     expr_str = model.specs.map(&:expression).map(&:to_s).join("\n")
            #     puts desc_str.line_combine(expr_str)

            #     # 
            # end

            def self.exit_on_failure?()
                true
            end

        end

    end
end