
module PgTools
    module NuSMV

        class Runner

            def run_specs(program_graph)
                nusmv_s = Transform::NuSmvTransformation.new.transform_graph(program_graph)
                output = eval_nusmv(nusmv_s)
                specs = program_graph.specification.flatten()
                return parse_spec_results(specs, output)
            end

            def parse_spec_results(specs, nusmv_output)
                block = Struct.new(:success, :lines)

                # Split the output into blocks which describe the 
                # specifications
                blocks, current_block = [], nil
                nusmv_output.split("\n").each { |line|
                    result = line[/-- specification .* is (true|false)/, 1]
                    if !result.nil?
                        blocks << current_block unless current_block.nil?
                        current_block = block.new(result == "true", [])
                        next
                    end
                    current_block.lines << line unless current_block.nil?
                }
                blocks << current_block unless current_block.nil?

                return blocks.each_with_index.map { |block, index|
                    trace = block.success ? nil : block.lines.select { |l| l.start_with?(/\s+/) }
                    Model::SpecResult.new(specs[index], block.success, trace)
                }
            end

            # Calls NuSMV to calculate a possible state trace.
            # Returns an array of states, where each state is a hash from
            # variable to the value of that variable in that state
            def run_simulation(program_graph, steps, random: false)
                commands = []
                commands << "read_model"
                commands << "flatten_hierarchy"
                commands << "encode_variables"
                commands << "build_model"
                commands << "pick_state"
                commands << "simulate -k #{steps.to_s.to_i} -v #{random ? '-r' : ''}"
                commands << "quit"
                nusmv_s = Transform::NuSmvTransformation.new.transform_graph(program_graph)
                output = eval_nusmv(nusmv_s, commands: commands)
                return parse_simulation_output(output)
            end

            def parse_simulation_output(nusmv_output)
                var_states, current_var_state = [], nil

                nusmv_output.split("\n").each { |line|
                    # Wait for heading of state
                    if line.match(/\s*-> State: .+ <-/)
                        var_states << current_var_state unless current_var_state.nil?
                        current_var_state = {}
                        next
                    end
                    # Skip lines before the first state
                    next if current_var_state.nil?

                    key_val = line.split("=").map(&:strip)
                    key = key_val[0].gsub("v.V_", "").to_sym
                    val = key_val[1].gsub("L_", "")
                    current_var_state[key] = val
                }
                return var_states
            end

            def eval_nusmv(nusmv_string, commands: [])
                tmp_file = PgTools.tmp_file("pg.smv")
                File.write(tmp_file, nusmv_string)
                return eval_file(tmp_file, commands: commands)
            end

            def eval_file(file, commands: [])
                nusmv_path = find_nusmv_path()
                raise "NuSMV not integrated! (TODO: Make a better error message)" if nusmv_path.blank?
                if commands.blank?
                    nusmv_cmd = "#{nusmv_path} #{file}"
                else
                    tmp_cmd_file = PgTools.tmp_file("nusmv_commands")
                    File.write(tmp_cmd_file, commands.join("\n"))
                    nusmv_cmd = "#{nusmv_path} -source '#{tmp_cmd_file}' #{file}"
                end
                output, err, status = Open3.capture3({}, nusmv_cmd)
                raise RawNuSMVError.new(nusmv_cmd, output, err, status, file) unless status.success?
                return output
            end

            def find_nusmv_path
                # Return by settings path if that exists
                return Settings.nusmv.path if !Settings.nusmv.path.blank? && File.file?(Settings.nusmv.path)

                # Fall back to looking in the addon directory
                candidates = Dir[File.join(PgTools.addon_dir, "*", "bin", "NuSMV*")]
                return candidates.sort.first unless candidates.empty?
            end

        end
    end
end
