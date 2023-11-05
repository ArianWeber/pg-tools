module Progg
    module Model

        class DCCA

            def initialize(graph, runner)
                @graph, @runner = graph, runner
            end

            def perform()
                @graph.hazards.map { |hazard|
                    fault_set = @graph.fault_components.map(&:name)
                    powerset = fault_set.powerset
                    sets = calc_minimal_critical_sets(powerset, fault_set, hazard)
                    [hazard, sets]
                }.to_h
            end

            def calc_minimal_critical_sets(powerset, fault_set, hazard, level: 0, minimal_critical_sets: [])
                to_check = powerset.select { |set| set.length == level }
                return minimal_critical_sets if to_check.empty?

                # Determine which of the sets to check can result in the hazard.
                critical = select_critical(to_check, fault_set, hazard)

                # Remove all super sets of the critical ones, as those must be critical as well,
                # but cannot be minimal.
                powerset = powerset.reject { |set|
                    critical.any? { |crit_set| set.subset?(crit_set) }
                }
                # Keep track of the new critical sets
                minimal_critical_sets += critical

                # Continue with the next level.
                calc_minimal_critical_sets(powerset, fault_set, hazard, 
                        level: level+1, minimal_critical_sets: minimal_critical_sets)
            end

            def select_critical(sets, fault_set, hazard)
                specs = sets.map { |set|
                    other_errors = fault_set.reject { |err| set.include?(err) }
                    only_errors = other_errors.empty? \
                        ? "TRUE" \
                        : other_errors.map { |err| "#{err} == no" }.join(" && ")
                    formula = "!( (#{only_errors}) U (#{hazard.expression}) )"

                    Spec.new("Fault set {#{set.join(', ')}} is safe", formula, nil)
                }

                spec_set = SpecSet.wrap(specs)
                @graph.specification = Specification.new([spec_set])
                
                results = @runner.run_specs(@graph)

                critical_sets = []
                results.each_with_index { |result, index|
                    critical_sets << sets[index] if result.failure?
                }

                return critical_sets
            end


        end

    end
end
