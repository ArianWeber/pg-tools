module PgVerify
    module Doctor

        Warning = Struct.new(:title, :text)

        class DoctorError < PgVerify::Core::Error
            def initialize(warnings)
                @warnings = warnings
            end
            def formatted()
                is_are, s = @warnings.length == 1 ? ["is", ""] : ["are", "s"]
                header = "There #{is_are} #{@warnings.length} warning#{s} for your installation:"
                string = @warnings.each_with_index.map { |w, i|
                    title = "#{i + 1}) #{w.title}".c_warn
                    body  = w.text.indented(str: "   ")
                    "#{title}\n#{body}"
                }.join("\n\n")
                return header, string
            end
        end

        def self.check()
            checks = Doctor.methods
                .select { |method| method.to_s.start_with?("check_") }
                .map { |sym| sym.to_s.sub("check_", "").to_sym }
                .sort
            warnings = checks.map { |check| run_check(check) }.flatten.compact
            raise DoctorError.new(warnings) unless warnings.empty?
        end

        def self.run_check(symbol)
            Shell::LoadingPrompt.while_loading("Checking #{symbol.to_s.gsub('_', ' ').c_string}") {
                warnings = ([self.send(:"check_#{symbol}")] || []).flatten.compact
                state = warnings.empty? ? :success : :error
                msg   = warnings.empty? ? "Ok" : "#{warnings.length} warning(s)!"
                # Allow returning :skip from the check to mark the check as skipped
                if warnings.length == 1 && warnings.first == :skip
                    state, msg, warnings = :empty, "Skipped!", []
                end
                Shell::LoadingPrompt::LoadingResult.new(warnings, msg, state: state)
            }
        end

        def self.check_01_Can_find_NuSMV()
            return [] unless PgVerify::NuSMV::Runner.new.find_nusmv_path.nil?
            return Warning.new("Unable to locate the NuSMV executable", 
                "Make sure to install NuSMV by unpacking it and placing the entire folder into\n" \
                "the #{'addon'.c_file} directory of your project. " \
                "(#{File.expand_path('addon').c_sidenote})\n" \
                "Alternatively you can set the #{'nusmv.path'.c_string} in the configuration."
            )
        end

        def self.check_02_Can_run_NuSMV()
            path = PgVerify::NuSMV::Runner.new.find_nusmv_path
            return :skip if path.nil?

            # Test by evaluating some example smv file
            example_file = File.join(PgVerify.root, "data", "nusmv.sample.smv")
            return [] if Core::CMDRunner.run_for_exit_code("#{path} #{example_file}") == 0

            return Warning.new("Unable to run the NuSMV executable", 
                "NuSMV could be found here: #{path.c_file}\n" \
                "However it could not be executed. Here are a few things to try:\n" \
                " - Make sure the file is executable\n" \
                " - Make sure you have the required permissions"
            )
        end

        def self.check_03_Run_integration_tests()
            return :skip if PgVerify::NuSMV::Runner.new.find_nusmv_path.nil?

            warnings = []

            test_files = Dir[File.join(PgVerify.root, "integration_tests", "ruby_dsl", "*.rb")].sort
            warnings += test_files.map { |test_file|
                model = Interpret::PgScript.new.interpret(test_file).first
                PgVerify::Model::Validation.validate!(model)
                results = NuSMV::Runner.new().run_specs(model)
                failures = results.reject(&:success)
                next if failures.empty?

                test_name = File.basename(test_file, '.*').gsub("_", "-")
                failures_s = failures.map { |f| "#{f.spec.text} (#{f.spec.expression.to_s.c_blue})" }
                show_command = "$ pg-verify show nusmv --script #{File.expand_path(test_file)}".c_cyan
                test_command = "$ pg-verify test --script #{File.expand_path(test_file)}".c_cyan
                Warning.new("Failed integration test in #{test_name}", 
                   "The test #{test_name.c_string} contains the following unsatisfied specifications:\n" \
                   "\t- #{failures_s.join("\n\t- ")}\n" \
                   "These specifications should be valid if pg-verify works as expected.\n" \
                   "You can use the following commands to debug this:\n" \
                   "  #{show_command}\n  #{test_command}"
                )
            }.compact

            warnings += test_files.map { |test_file|
                model = Interpret::PgScript.new.interpret(test_file).first
                next if model.hazards.empty?
                require 'set'

                dcca = Model::DCCA.new(model, NuSMV::Runner.new)
                result = dcca.perform()

                expected_cut_sets = File.read(test_file)
                    .split("\n")
                    .find { |l| l.include?("Expected Cut Sets") }
                    .scan(/\{([^}]+)\}/).flatten
                    .map { |string| string.split(",").map(&:strip).map(&:to_sym) }
                    .map { |cut_set| Set.new(cut_set) }

                actual_cut_sets = result.values.first.map { |cut_set| Set.new(cut_set) }
                
                next if Set.new(actual_cut_sets) == Set.new(expected_cut_sets)
                
                Warning.new("Failed DCCA for #{File.basename(test_file)}",
                   "The DCCA for hazard #{result.keys.first.expression.to_s.c_blue} yielded unexpected cut sets:\n" \
                   "Expected: #{expected_cut_sets}\n" \
                   "Got     : #{actual_cut_sets}\n"
                )
            }.compact


            return warnings
        end

        def self.check_04_Check_project_files()
            warnings = []
            script = Settings.ruby_dsl.default_script_name

            return Warning.new("Failed to find model definition at #{script}", 
                "If you run PG verify without any arguments it expects a program graph definition\n" \
                "at your working directory, which should be named #{script.c_file} by default.\n" \
                "Based on your current working directory this would be this full path:\n" \
                "\t#{File.expand_path(script).c_file}"
            ) unless File.file?(script)

            begin 
                Interpret::PgScript.new.interpret(script)
            rescue Exception => e
                return Warning.new("Failed to interpret model at #{script}", 
                    "Your default model definition at #{script.c_file}\n" \
                    "(Full path: #{File.expand_path(script).c_sidenote})\n" \
                    "Could not be interpreted. Make sure there are no syntax errors."
                ) 
            end

            return []
        end

        # def self.check_03_Can_find_PlantUML()
        #     return [] unless PgVerify::Puml.find_path.nil?
        #     return Warning.new("Unable to find the PlantUML executable", 
        #         "Make sure to install PlantUML by dropping the jar into\n" \
        #         "the #{'addon'.c_file} directory of your project. " \
        #         "(#{File.expand_path('addon').c_sidenote})\n" \
        #         "You can get it from here: #{"https://plantuml.com/download".c_string}\n" \
        #         "pg-verify will fall back to using the PlantUML web-server otherwise."
        #     )
        # end

        # def self.check_04_Can_run_PlantUML()
        #     path = PgVerify::Puml.find_path
        #     return :skip if path.blank?

        #     return [] if Core::CMDRunner.run_for_exit_code("java -jar #{path} -help") == 0
        #     puts "java -jar #{path} -help"

        #     return Warning.new("Unable to run the PlantUML executable",
        #         "PlantUML could be found here: #{path.c_file}\n" \
        #         "However it could not be executed. Here are a few things to try:\n" \
        #         " - Make sure the file is executable\n" \
        #         " - Make sure you have the required permissions"
        #     )
        # end

    end
end

# Require all module files
Dir[File.join(__dir__, "**", '*.rb')].sort.each { |file| require file }