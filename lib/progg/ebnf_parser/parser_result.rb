module Progg
    module EbnfParser

        class ParserResult

            attr_accessor :ast
            attr_accessor :error

            def initialize(ast, error)
                @ast, @error = ast, error
            end

            def check!()
                return unless error?()
                # TODO: Custom exception
                raise @error
            end

            def error?()
                return !@error.nil?
            end

        end

    end
end
