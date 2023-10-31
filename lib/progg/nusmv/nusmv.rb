# Require all module files
Dir[File.join(__dir__, "**", '*.rb')].sort.each { |file| require file }

module Progg
    module NuSMV
    end
end