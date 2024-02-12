# Require all module files
Dir[File.join(__dir__, "**", '*.rb')].sort.each { |file| require file }

module PgVerify
    module Model
        
        # class ValidationError < PgVerify::Core::Error; end

    end
end