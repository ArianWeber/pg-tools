# Require all module files
Dir[File.join(__dir__, "**", '*.rb')].sort.each { |file| require file }

module PgTools
    module Model
        
        # class ValidationError < PgTools::Core::Error; end

    end
end