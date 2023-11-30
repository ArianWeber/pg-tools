
module PgTools
    module Interpret

        class PgScript

            # List of components for this graph
            attr_accessor :components
            attr_accessor :script_file

            def initialize()
                @components = []
            end
        
            def interpret(file)
                file = File.expand_path(file)
                raise NoSuchScriptError.new(file) unless File.file?(file)
                @script_file ||= file
                
                graph_ctx = Interpret::GraphContext.new(self)

                begin
                    Dir.chdir(File.dirname(file)) { eval(File.read(file), graph_ctx.get_binding(), file) }
                rescue Exception => e
                    re_raise_exception(file, self, e)
                end
                
                return graph_ctx.to_model()
            end

            # Re-raise an exception which occurred during evaluation of a script to more user
            # friendly error messages.
            def re_raise_exception(file, script, ex)
                # Determine the line in the script file where the error was raised
                _, line_number = find_source_location(ex.backtrace)

                # Include the full backtrace in the message if enabled
                if Settings.full_stack_trace
                    title = "Here is the full backtrace of the exception!".c_sidenote + "\n"
                    title += "(You don't have to bother with this unless you are developing pg-tools)".c_sidenote
                    bt = ex.backtrace.map(&:c_sidenote).join("\n").indented(str: ">>  ".c_sidenote)
                end

                raise InterpretError.new(file, line_number, ex)
            end
            private :re_raise_exception

            def find_source_location(trace = caller())
                line_number = trace
                    .select { |l| l.include?(@script_file) }
                    .map { |l| l.split(":in")[0] }.reject(&:blank?)
                    .map { |l| l.split(":")[-1] }.reject(&:blank?)
                    .uniq.first.to_i
                return [@script_file, line_number]
            end
        
        end
    end
end
