
module Progg
    module Model

        class ValidationError

            def to_s()
                return "#{self.title().c_error}\n#{self.body().indented(str: '>> '.c_error)}"
            end

        end

    end
end