module PgTools
    module EbnfParser

        class Ast

            attr_accessor :map

            def initialize(map)
                @map = map
            end

            def find_variables(element=@map)
                # TODO: Actually parse the variables
                # puts "REC #{element.class}: #{element}"
                # puts "\n"
                # ret = []
                # ret += element.map { |val| find_variables(val) }.flatten if element.is_a?(Array)
                # element.each { |key, value| 
                #     # We found a variable if we found a value which is not a number nor a boolean constant
                #     if key == :Value && value.is_a?(String) && !(/\d+/.match?(value)) && value != 'true' && value != 'false'
                #         ret << value
                #     end
                #     ret += find_variables(value)
                # }  if element.is_a?(Hash)
                # return ret
            end

        end

    end
end