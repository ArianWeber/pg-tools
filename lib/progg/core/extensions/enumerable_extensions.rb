module Enumerable

    def powerset
        array = self.to_a()
        ret = Enumerator.new {|ps|
            array.size.times {|n|
                array.combination(n).each(&ps.method(:yield))
            }
        }
        return ret.to_a + [array]
    end

    def subset?(other)
        other = other.to_a()
        array = self.to_a()
        return other.all? { |item| array.include?(item) }
    end
    
  end