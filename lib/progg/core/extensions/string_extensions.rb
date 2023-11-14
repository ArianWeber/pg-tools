class String

    def integer?
        self.to_i.to_s == self
    end

    def snake_case
        self.strip()
            .gsub(/ +/, "_")
            .gsub(/::/, '/')
            .gsub(/([A-Z]+)([A-Z][a-z])/,'\1_\2')
            .gsub(/([a-z\d])([A-Z])/,'\1_\2')
            .tr("-", "_")
            .downcase()
    end

    def camel_case
        return self if self !~ /_/ && self =~ /[A-Z]+.*/
        split('_').map{|e| e.capitalize}.join
    end

    def grep(str, e)
        self.split("\n").select { |l| l.include?(str) }
    end

    def display_length()
        str = PgTools::Colorizer.uncolorize(self)
        str.length() + ( str.count("\t") * 4 )
    end

    def line_combine(other, separator: " ")
        PgTools::StringUtil.line_combine(self, other, separator: separator)
    end

    def indented(num: 1, str: " " * 4)
        PgTools::StringUtil.indented(self, num_indents: num, indent_string: str)
    end

    def remove_before(substring)
        split = self.split(substring)
        return "" if split.length == 1
        return split[1, split.length].join(substring)
    end

    def blank?
        self.empty?
    end

    def limit_lines(num_lines, separator: "...")
        return "" if num_lines == 0
        split = self.split("\n")
        return self unless split.length > num_lines
        first_part = split[0, num_lines / 2]
        second_part = split[split.length - (num_lines - first_part.length), split.length]
        return first_part.join("\n") + "\n#{separator}\n" + second_part.join("\n")
    end

    def shorten(length)
        return self if self.length <= length
        return self[0, [length - 3, 1].max] + "..."
    end

    def labelize(bg: :darkgreen, fg: :white)
        "◖".send(:"c_#{bg}") + " #{self} ".send(:"bg_#{bg}").send(:"c_#{fg}").c_bold + "◗".send(:"c_#{bg}")
    end

    def file?()
        File.file?(self)
    end

    def directory?()
        File.directory?(self)
    end

    # Support this method for Ruby <= 2.3
    unless self.method_defined?(:delete_prefix)
        def delete_prefix(prefix)
            self.respond_to?(:delete_prefix)
            return unless self.start_with?(prefix)
            return self[prefix.length, self.length - 1]
        end
    end

end
