# Require all module files
Dir[File.join(__dir__, "**", '*.rb')].sort.each { |file| require file }

module PgTools
    module NuSMV

        class RawNuSMVError < PgTools::Core::Error
            def initialize(out, err, status, file)
                @out, @err, @status, @file = out, err, status, file
            end

            def formatted()
                title = "Execution of NuSMV exited with #{@status}"

                body = "#{@out}\n#{@err}"
                
                return title, body
            end
        end

    end
end