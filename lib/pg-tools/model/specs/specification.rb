module PgTools
    module Model

        class Specification

            attr_accessor :spec_sets

            def self.empty()
                return self.new([])
            end

            def initialize(spec_sets)
                @spec_sets = spec_sets
            end

            def get_specs()
                spec_sets.map(&:get_specs).flatten
            end

            def flatten()
                return get_specs().map { |spec|
                    parents = spec.parents
                    prefix = parents.map { |spec_set|
                        spec_set.assumption.nil? \
                            ? spec_set.text \
                            : "(assuming #{spec_set.assumption[:text]})" #.c_sidenote
                    }.join(" ")
                    text = "#{prefix} #{spec.text}"

                    assumption_expression = parents.map { |spec_set|
                        next if spec_set.assumption.nil?
                        spec_set.assumption[:expression]
                    }.compact.join(" && ")

                    expression = assumption_expression.empty? \
                        ? spec.expression \
                        : "( #{assumption_expression} ) => #{spec.expression}"
                    
                    Spec.new(text, expression, nil)
                }
            end

        end
    end
end
