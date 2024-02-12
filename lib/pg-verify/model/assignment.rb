
module PgTools
    module Model

        class Assignment

            ASSIGN_OPERATOR = ':='

            # The name of the variable which is assigned a value
            attr_accessor :assigned_variable

            # The Model::Expression which of this assignment
            attr_accessor :expression

            def self.from_string(string)
                raise "Not an assignment: #{string}" unless string.include?(ASSIGN_OPERATOR)
                split = string.split(ASSIGN_OPERATOR).map(&:strip)
                assigned_variable = split[0].to_sym
                expression = Expression.from_string(split[1])
                return Assignment.new(assigned_variable, expression)
            end

            def initialize(assigned_variable, expression)
                @assigned_variable, @expression = assigned_variable, expression
            end

            def to_s()
                return "#{assigned_variable} #{ASSIGN_OPERATOR} #{expression.to_s}"
            end

        end

    end
end
