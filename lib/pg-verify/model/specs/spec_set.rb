module PgVerify
    module Model

        class SpecSet

            # The text of this spec set as a string.
            attr_accessor :text

            attr_accessor :assumption

            # The sub-spec sets contained in this spec set
            attr_accessor :children

            attr_accessor :parent

            def self.wrap(specs)
                spec_set = self.new("", nil, nil, nil)
                specs.each { |spec| spec.parent = spec_set }
                spec_set.children = specs
                return spec_set
            end

            def initialize(text, assumption, parent, children)
                @text, @assumption, @parent, @children = text, assumption, parent, children
            end

            def parent?
                return !@parent.nil?
            end

            def get_specs()
                ret = []
                children.each { |child|
                    ret << child             if child.is_a?(Spec)
                    ret += child.get_specs() if child.is_a?(SpecSet)
                }
                return ret
            end

        end

    end
end
