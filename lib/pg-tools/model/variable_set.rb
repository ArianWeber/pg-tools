
module PgTools
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
                varset = VariableSet.new(*varset) if varset.is_a?(Array)
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
                name = name.to_sym
                raise "No such variable #{name}!" unless @map.key?(name)
                return @map[name]
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
                return @map.key?(name.to_sym)
            end

            def constname?(name)
                values().map(&:to_s).include?(name.to_s)
            end

            def to_s()
                return "{#{@map.map { |name, var| "#{name}:#{var.range}(#{var.owner_name})" }.join(", ")}}"
            end

            def select_by_owner(owner)
                owner = owner.name if owner.is_a?(Model::Component)
                owner = owner.to_sym if owner.is_a?(String)
                found = self.to_a().select { |var| var.owner_name == owner }
                return VariableSet.new(*found)
            end

            # Returns all possible values of variables in this set as an array
            def values()
                return self.to_a().map(&:values).flatten
            end

        end

    end
end
        