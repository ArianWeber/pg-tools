# Require all module files
Dir[File.join(__dir__, '*.rb')].sort.each { |file| require file }

require 'thor'
require 'plantuml_builder'

module PgTools
    module Cli

        class ShowCommand < Thor

            desc "puml", "Shows the model in PlantUML format"
            method_option :only, :type => :array, repeatable: true
            method_option :hide, :type => :array, repeatable: true
            method_option :script, :type => :string
            def puml()
                script_file = options[:script] || Settings.ruby_dsl.default_script_name
                models = Interpret::PgScript.new.interpret(script_file)
                models.each { |model|
                    components = self.class.select_components(options[:only], options[:hide], model)
                    puml = Transform::PumlTransformation.new.transform_graph(model, only: components)
                    puts puml
                }
            end

            desc "png", "Shows the model as a PNG image"
            method_option :only, :type => :array, repeatable: true
            method_option :hide, :type => :array, repeatable: true
            method_option :script, :type => :string
            def png()
                script_file = options[:script] || Settings.ruby_dsl.default_script_name
                models = Interpret::PgScript.new.interpret(script_file)

                models.each { |model|
                    components = self.class.select_components(options[:only], options[:hide], model)
                    puml = Transform::PumlTransformation.new.transform_graph(model, only: components)
                    png = PlantumlBuilder::Formats::PNG.new(puml).load
                    out_name = File.basename(script_file, '.*')
                    out_name += "-" + model.name.to_s.gsub(/\W+/, '_')
                    out_name += ".png"
                    File.binwrite(out_name, png)
                }
            end

            desc "yaml", "Shows the model in YAML format"
            method_option :script, :type => :string
            def yaml()
                script_file = options[:script] || Settings.ruby_dsl.default_script_name
                models = Interpret::PgScript.new.interpret(script_file)

                models.each { |model| 
                    hash = Transform::HashTransformation.new.transform_graph(model)
                    puts hash.to_yaml
                }
            end

            desc "json", "Shows the model in Json format"
            method_option :script, :type => :string
            def json()
                script_file = options[:script] || Settings.ruby_dsl.default_script_name
                models = Interpret::PgScript.new.interpret(script_file)

                models.each { |model| 
                    hash = Transform::HashTransformation.new.transform_graph(model)
                    puts JSON.pretty_generate(hash)
                }
            end

            desc "nusmv", "Shows the model in NuSMV format"
            method_option :script, :type => :string
            def nusmv()
                script_file = options[:script] || Settings.ruby_dsl.default_script_name
                models = Interpret::PgScript.new.interpret(script_file)

                models.each { |model|
                    nusmv = Transform::NuSmvTransformation.new.transform_graph(model)
                    puts nusmv
                }
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

            desc "test", "Test the model specifications"
            method_option :script, :type => :string
            def test()
                script_file = options[:script] || Settings.ruby_dsl.default_script_name
                models = Interpret::PgScript.new.interpret(script_file)

                models.each { |model|
                    results = Shell::LoadingPrompt.while_loading("Running specifications") {
                        NuSMV::Runner.new().run_specs(model)
                    }

                    results.each { |result|
                        stat_string = result.success ? "PASSED".c_success : "FAILED".c_error
                        puts "[ #{stat_string} ] #{result.spec.text}"
                        puts "           #{result.spec.expression.to_s.c_blue}"
                        puts result.trace.map(&:c_error).join("\n") unless result.success
                    }
                }
            end

            desc "dcca", "Run the automatic DCCA for hazards of the model"
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

            desc "simulate", "Simulate the model and save each step as an image"
            method_option :script, :type => :string
            method_option :steps, :type => :numeric, default: 10
            method_option :force, :type => :numeric, default: 10
            def simulate()
                script_file = options[:script] || Settings.ruby_dsl.default_script_name
                models = Interpret::PgScript.new.interpret(script_file)
                runner = NuSMV::Runner.new

                models.each { |model|
                    trace = Shell::LoadingPrompt.while_loading("Simulating model #{model.name.to_s.c_string}") {
                        runner.run_simulation(model, options[:steps])
                    }

                    # Prepare output dir
                    out_dir = File.expand_path("simulate-" + model.name.to_s.gsub(/\W+/, '_').downcase, Settings.outdir)
                    FileUtils.mkdir_p(out_dir)

                    # Generate images
                    Shell::LoadingPrompt.while_loading("Rendering states") { |printer|
                        trace.each_with_index { |variable_state, index|

                            printer.printl("Step #{index + 1}/#{trace.length}")
                            puml = Transform::PumlTransformation.new.transform_graph(model, variable_state: variable_state)
                            png = PlantumlBuilder::Formats::PNG.new(puml).load
                            out_path = File.expand_path("step-#{index}.png", out_dir)
                            File.binwrite(out_path, png)
                        }
                    }
                    puts "Wrote #{trace.length.to_s.c_num} files to #{out_dir.c_file}"
                }

            end

            desc "init", "Initialize a new pg-tools project"
            method_option :directory, :type => :string
            def init()

                target = options[:directory].blank? ? Dir.pwd() : File.expand_path(options[:directory])
                if Dir.exist?(target) && Dir.entries(target).size > 2
                    puts "The target directory #{target.c_file} isn't empty!"
                    exit 1
                end

                # Create target dir
                FileUtils.mkdir_p(target)

                # Copy files to target
                template_dir = File.join(PgTools.root, "data", "project-template")
                files = Dir.glob(File.join(template_dir, '**', '*'), File::FNM_DOTMATCH).select { |f| File.file?(f) }
                files.each { |f| 
                    target_file = File.join(target, f.sub(template_dir, ""))
                    target_file = target_file.gsub(".resource", "")
                    FileUtils.mkdir_p(File.dirname(target))
                    FileUtils.cp(f, target) unless File.basename(f) == ".keep"
                }
                # Copy the actual default config into the project as that
                # will contain all keys and should be commented
                FileUtils.cp(File.join(PgTools.root, "data", "config", "pg-tools.yml"), File.join("template_dir", ".pg-tools.yml"))

                # Initialize git project
                Dir.chdir(target) { 
                    Core::CMDRunner.run_cmd("git init")
                    Core::CMDRunner.run_cmd("git add .")
                    Core::CMDRunner.run_cmd("git commit -m \"Initial Commit\"")
                }

                puts "Successfully initialized project at #{target.c_file}!"
                puts "You can read the #{'README.md'.c_file} to get started."
                puts "Run #{'pg-tools doctor'.c_blue} and follow the instructions to set up your environment!"
            end

            desc "doctor", "Check for common problems"
            def doctor()
                 Doctor.check()
            end

            desc "show", "Show the program graph multiple different ways"
            subcommand 'show', ShowCommand

            # Make Thor exit with non-0 in case of errors
            def self.exit_on_failure?(); true end

        end

    end
end