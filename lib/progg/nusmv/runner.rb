
module Progg
    module NuSMV

        class Runner

            def run_specs(program_graph)
                nusmv_s = Transform::NuSmvTransformation.new.transform_graph(program_graph)
                output = eval(nusmv_s)
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

            def eval(string)
                File.write("pg.smv", string)
                return eval_file("pg.smv")
            end

            def eval_file(file)
                output, err, status = Open3.capture3({}, "#{Settings.numsv.path} #{file}")
                raise "NuSMV error: #{}" unless status.success?
                return output
            end

            def load_file(file)
                output, err, status = Open3.capture3({}, "NuSMV-2.6.0-Darwin/bin/NuSMV pg.smv")
                puts ``
            end

        end

    end
end
