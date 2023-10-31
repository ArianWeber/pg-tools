module Progg
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

            def flat_specs()
                specs = get_specs()
                specs.each { |spec|
                    parents = spec.parents
                    prefix = parents.map { |spec_set|
                        if (spec_set.assumption.nil?)
                            spec_set.text
                        else
                            "assuming #{spec_set.assumption[:text]}".c_sidenote
                        end
                    }.join(" ")
                    text = "#{prefix} #{spec.text}"

                    assumption_expression = parents.map { |spec_set|
                        next if spec_set.assumption.nil?
                        spec_set.assumption[:expression]
                    }.compact.join(" && ")

                    expression = assumption_expression.empty? \
                        ? spec.expression \
                        : "( #{assumption_expression} ) => #{spec.expression}"


                    puts "[ #{'PASSED'.c_success} ] " + text
                    puts "           " + expression.c_blue
                    puts
                }
                puts "=> [ #{specs.length}/#{specs.length} ] specifications are valid!".c_success
            end

        end

    end
end
