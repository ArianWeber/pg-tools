# Require all module files
Dir[File.join(__dir__, "**", '*.rb')].sort.each { |file| require file }

module PgVerify
    module NuSMV

        class RawNuSMVError < PgVerify::Core::Error
            def initialize(cmd, out, err, status, file)
                @cmd, @out, @err, @status, @file = cmd, out, err, status, file
            end

            def formatted()
                title = "Execution of NuSMV exited with #{@status}"

                body  = "The exact command was\n#{@cmd.c_string}\n\n"
                body += "#{@out}\n#{@err}"
                
                return title, body
            end
        end

    end
end