module Progg
    module Interpret

        class LTLBuilder

            def globally()
                return LTLBuilderGlobally.new()
            end
            def after(expression)
                return LTLBuilderAfter.new(expression)
            end
            def before(expression)
                LTLBuilderBefore.new(expression)
            end
            def between(); end
            def after_until(); end

            class LTLBuilderBase

                def build(pattern, map)
                    map.each { |key, val|
                        pattern = pattern.gsub(key.to_s, "#{val}")
                    }
                    return pattern
                end

            end

            class LTLBuilderGlobally < LTLBuilderBase
                def global(p)
                    build "G p", { p: p }
                end
                def never(p)
                    build "G !( p )", { p: p }
                end
                def exists(p)
                    build "F p", { p: p }
                end
                def reacts(p, s)
                    build "G ( p => F s )", { p: p, s: s }
                end
                def precedes(p, s)
                    build "!(p) W s", { p: p, s: s }
                end
            end

            class LTLBuilderAfter < LTLBuilderBase
                attr_accessor :q
                def initialize(q); @q = q end
                def global(p)
                     build "G ( q -> G p )", { p: p, q: q }
                end
                def never(p) 
                    build "G ( q -> G !(p))", { p: p, q: q }
                end
                def exists(p)
                     build "( G !(q) ) || ( F ( q && F p ) )", { p: p, q: q }
                end
                def reacts(p, s)
                    build "G ( q => G (p => F s) )", { p: p, q: q, s: s} 
                end
                def precedes(p, s)
                    build "G !(q) || F ( q && !(p) W s )", { p: p, q: q, s: s} 
                end
            end

            class LTLBuilderBefore < LTLBuilderBase
                attr_accessor :q
                def initialize(q); @q = q end
                def global(p)
                    
                end
                def never(p) 
                    build "( F q ) => ( !(p) U q )", { p: p, q: q }
                end
                def exists(p)
                     
                end
                def reacts(p, s)
                    
                end
                def precedes(p, s)
                    
                end
            end

        end

    end
end
