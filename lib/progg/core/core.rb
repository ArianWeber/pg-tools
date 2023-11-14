
# Require all module files
Dir[File.join(__dir__, "**", '*.rb')].sort.each { |file| require file }

module PgTools

    module Core

        class Error < StandardError; end

    end

end