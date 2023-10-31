
module Progg
    module NuSMV

        class NuSMVRunner

            def initialize()
            end

            def load_file(file)
                puts `#{Settings.numsv.path} #{file}`
            end

        end

    end
end
