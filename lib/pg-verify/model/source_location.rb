module PgVerify
    module Model
        
        class SourceLocation

            attr_accessor :file
            attr_accessor :line_number

            def initialize(file, line_number)
                @file, @line_number = file, line_number
            end

            def render_code_block(num_lines = 2)
                index = @line_number - 1
                # Read lines and add line numbers
                lines = File.read(@file).split("\n").each_with_index.map { |l, i| "#{i.to_s.c_blue}: #{l}" }

                # Get the line surrounding the error
                start_index = [0, index - num_lines].max
                end_index = [lines.length - 1, index + num_lines].min
                surrounding_lines = lines[start_index..end_index]
                error_line = lines[index]

                surrounding_lines = [ "...".c_sidenote ] + surrounding_lines + [ "...".c_sidenote ]

                surrounding_lines = surrounding_lines.map { |l|
                    if l == error_line
                        l.ljust(surrounding_lines.map(&:length).max) + " < Error".c_error
                    else
                        l
                    end
                }

                return surrounding_lines.join("\n")
            end

            def to_s()
                "#{@file}:#{@line_number}"
            end
            

        end

    end
end
