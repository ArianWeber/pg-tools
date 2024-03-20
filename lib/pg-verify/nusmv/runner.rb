
module PgVerify
    module NuSMV

        class Runner

            def run_specs(program_graph)
                transformer = Transform::NuSmvTransformation.new
                nusmv_s = transformer.transform_graph(program_graph, include_specs: false)
                commands = [ "go" ]
                specs = program_graph.specification.flatten()
                transform_map = specs.each { |spec|
                    transformed_spec_expr = transformer.transform_expression(spec.expression, program_graph.all_variables)
                    cmd = {  ltl: "check_ltlspec", ctl: "check_ctlspec" }[spec.expression.predict_type]
                    commands << "#{cmd} -p \"#{transformed_spec_expr}\""
                }
                output = eval_nusmv(nusmv_s, commands: commands)

                return parse_spec_results(program_graph, specs, output)
            end

            def parse_spec_results(program_graph, specs, nusmv_output)
                block = Struct.new(:success, :lines)

                # Split the output into blocks which describe the 
                # specifications
                blocks, current_block = [], nil
                nusmv_output.split("\n").each { |line|
                    result = line[/-- specification .* is (true|false)/, 1]
                    if !result.nil?
                        blocks << current_block unless current_block.nil? # Complete the last block
                        current_block = block.new(result == "true", [])   # Start a new block
                        next
                    end
                    current_block.lines << line unless current_block.nil?
                }
                blocks << current_block unless current_block.nil?

                return blocks.each_with_index.map { |block, index|
                    trace = block.success ? nil : block.lines.select { |l| l.start_with?(/\s+/) }
                    trace = parse_trace(program_graph, trace.join("\n")) unless trace.nil?

                    Model::SpecResult.new(specs[index], block.success, trace)
                }
            end

            # Calls NuSMV to calculate a possible state trace.
            # Returns an array of states, where each state is a hash from
            # variable to the value of that variable in that state
            def run_simulation(program_graph, steps, random: false)
                commands = []
                commands << "go"
                commands << "pick_state #{random ? '-r' : ''}"
                commands << "simulate -k #{steps.to_s.to_i} -v #{random ? '-r' : ''}"
                commands << "quit"
                nusmv_s = Transform::NuSmvTransformation.new.transform_graph(program_graph)
                output = eval_nusmv(nusmv_s, commands: commands)
                return parse_trace(program_graph, output)
            end

            def run_check!(program_graph)
                deadlock_state = run_check(program_graph)
                return if deadlock_state.nil?
                raise Model::Validation::DeadlockInFSMError.new(program_graph, deadlock_state)
            end

            def run_check(program_graph)
                commands = [ "go", "check_fsm", "quit" ]
                nusmv_s = Transform::NuSmvTransformation.new.transform_graph(program_graph, include_specs: false)
                output = eval_nusmv(nusmv_s, commands: commands)

                # Return "ok" if the FSM has no deadlocks
                return nil if output.include?("The transition relation is total: No deadlock state exists")

                # Otherwise compute and return the deadlock state
                lines = output.split("\n").drop_while { |str| 
                    !(str.start_with?("A deadlock state is:") || str.start_with?("successors is:"))
                }
                lines = lines[1, lines.length - 1]

                var_state = {}
                lines.each { |line|
                    key, val = parse_var_assignment_in_trace(line)
                    next if key.nil? || val.nil?
                    var_state[key] = val
                }

                return var_state
            end

            def parse_trace(program_graph, nusmv_output)
                var_states, current_var_state = [], nil

                loop_index = -1

                nusmv_output.split("\n").each { |line|
                    # Mark a loop for the following state
                    if line.include?("Loop starts here")
                        loop_index = var_states.length
                        next
                    end

                    # Wait for heading of new state
                    if line.match(/\s*-> State: .+ <-/)
                        # Complete and store the old state if any
                        unless current_var_state.nil?
                            missing_keys = var_states.empty? ? [] :  var_states[-1].keys - current_var_state.keys
                            missing_keys.each { |key|
                                current_var_state[key] = var_states[-1][key]
                            }  
                            var_states << current_var_state
                        end
                        # Create a new state
                        current_var_state = {}
                        next
                    end
                    # Skip lines before the first state
                    next if current_var_state.nil?

                    # Parse the variable state
                    key, val = parse_var_assignment_in_trace(line)
                    next if key.nil? || val.nil?
                    current_var_state[key] = val
                }
                # Finish up
                unless current_var_state.nil?
                    missing_keys = var_states.empty? ? [] :  var_states[-1].keys - current_var_state.keys
                    missing_keys.each { |key|
                        current_var_state[key] = var_states[-1][key]
                    }  
                    var_states << current_var_state
                end
                return Model::Trace.new(program_graph, var_states, loop_index: loop_index)
            end

            def parse_var_assignment_in_trace(line)
                return nil, nil unless line.include?("=")
                key_val = line.split("=").map(&:strip)
                key = key_val[0].gsub("v.V_", "").to_sym
                val = key_val[1].gsub("L_", "")
                return key, val
            end

            def eval_nusmv(nusmv_string, commands: [])
                tmp_file = PgVerify.tmp_file("pg.smv")
                File.write(tmp_file, nusmv_string)
                return eval_file(tmp_file, commands: commands)
            end

            def eval_file(file, commands: [])
                nusmv_path = find_nusmv_path()
                raise "NuSMV not integrated! (TODO: Make a better error message)" if nusmv_path.blank?
                if commands.blank?
                    nusmv_cmd = "#{nusmv_path} #{file}"
                else
                    tmp_cmd_file = PgVerify.tmp_file("nusmv_commands")
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
                candidates = Dir[File.join(PgVerify.addon_dir, "*", "bin", "NuSMV*")]
                return candidates.sort.first unless candidates.empty?
            end

        end
    end
end
