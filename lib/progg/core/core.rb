
# Require all module files
Dir[File.join(__dir__, "**", '*.rb')].sort.each { |file| require file }

module Progg

    module Core

        class Error < StandardError; end

    end

end