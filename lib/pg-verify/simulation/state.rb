module PgVerify
    module Simulation

        class State

            attr_accessor :value_map

            def self.for_variable_set(var_set)
                value_map = var_set.map { |var|
                    # TODO: Choose random one when nil
                    raise "TODO: Implement this"
                    [var, init_val]
                }.to_h
                return self.new(value_map)
            end

            def initialize(value_map)
                @value_map = value_map
            end

            def [](var)
                var = var.to_sym if var.is_a?(String)
                var = @value_map.keys.detect { |v| v.name == var } if var.is_a?(Symbol)
                return @value_map[var]
            end

            def []=(var, value)
                var = var.to_sym if var.is_a?(String)
                var = @value_map.keys.detect { |v| v.name == var } if var.is_a?(Symbol)
                @value_map[var] = value
            end

            def variables()
                return @value_map.keys
            end

            def names()
                return @value_map.keys.map(&:name)
            end

            def clone()
                self.class.new(value_map.map { |k, v| [k, v] }.to_h)
            end

            def to_s()
                var_s = @value_map.keys.map(&:name).join("\n")
                val_s = @value_map.values.join("\n")
                return var_s.line_combine(val_s, separator: " = ")
            end

        end
    end
end
