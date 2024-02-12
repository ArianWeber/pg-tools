module PgTools

    module Core

        class Error < StandardError

            def formatted()
                raise "Not implemented in subclass #{self.class}"
            end

            def to_formatted()
                title, body, hint = self.formatted()

                message = []
                
                error_label = " ✖ ERROR ".bg_error
                message << "#{error_label} #{title.c_error}" unless title.nil?

                indent = "  ".bg_error + "  "
                message << "#{indent}\n#{body.indented(str: indent)}\n#{indent}" unless body.nil?

                indent = "  ".bg_warn + "  "
                message << backtrace.map.map {|l| "#{indent}#{l.c_warn}"}.join("\n") if Settings.full_stack_trace

                indent = "  ".bg_sidenote + "  "
                message << hint.split("\n").map {|l| "#{indent}#{l.c_sidenote}"}.join("\n") unless hint.nil?

                return message.join("\n")
            end

        end

    end

end

# Require all module files
Dir[File.join(__dir__, "**", '*.rb')].sort.each { |file| require file }