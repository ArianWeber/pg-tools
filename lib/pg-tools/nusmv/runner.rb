
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

            def eval_nusmv(nusmv_string)
                tmp_file = PgTools.tmp_file("pg.smv")
                File.write(tmp_file, nusmv_string)
                return eval_file(tmp_file)
            end

            def eval_file(file)
                nusmv_path = find_nusmv_path()
                raise "NuSMV not integrated! (TODO: Make a better error message)" if nusmv_path.blank?
                output, err, status = Open3.capture3({}, "#{nusmv_path} #{file}")
                raise RawNuSMVError.new(output, err, status, file) unless status.success?
                return output
            end

            def find_nusmv_path
                # Return by settings path if that exists
                return Settings.numsv.path if File.file?(Settings.numsv.path)

                # Fall back to looking in the addon directory
                candidates = Dir[File.join(PgTools.addon_dir, "*", "bin", "NuSMV")]
                return candidates.sort.first unless candidates.empty?
            end

        end
    end
end
