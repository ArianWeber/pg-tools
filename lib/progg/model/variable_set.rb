
module Progg
    module Model

        class VariableSet

            attr_accessor :map

            def initialize(*variables)
                @map = {}
                self << variables
            end

            def <<(var)
                return @map[var.name] = var if var.is_a?(Variable)
                return var.map { |v| self << v } if var.is_a?(Array)
                raise "Not a variable '#{var}'"
            end

            def get(*names)
                return names.map { |n| self[n] }
            end

            def +(varset)
                raise "Not a variable set '#{varset}'" unless varset.is_a?(VariableSet)
                return VariableSet.new(*(self.to_a() + varset.to_a()))
            end

            def to_a()
                return @map.values()
            end

            def names()
                return @map.keys()
            end

            def [](name)
                raise "No such variable #{name}!" unless @map.key?(name)
                return @map[key]
            end

            def include?(name_or_var)
                return varname?(name_or_var) || constname?(name_or_var)
            end

            def empty?()
                return @map.empty?
            end

            def map(&blk)
                return @map.values.map(&blk)
            end

            def varname?(name)
                return @map.key?(name)
            end

            def constname?(name)
                @map.values.include?(name)
            end

            def to_s()
                return "{#{@map.map { |name, var| "#{name}:#{var.range}(#{var.owner_name})" }.join(", ")}}"
            end

            def select_by_owner(owner_name)
                found = self.to_a().select { |var| var.owner_name == owner_name }
                return VariableSet.new(*found)
            end

            # Returns all possible values of variables in this set as an array
            def values()
                return self.to_a().map(&:values).flatten
            end

        end

    end
end
        