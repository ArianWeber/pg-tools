require "rainbow"

module PgTools

    # A Module for easy coloration of strings with theme support.
    # After #attach is called strings can be colored using one of any c_ or bg_ methods
    #   e.g: "Hello".c_cyan, "Hello".bg_blue
    # Calls can also by chained:
    #   e.g: "Hello".c_red.bg_blue.c_bold
    #
    # A theme is a hash of keys to values. Values can be:
    #   1. Hex values. e.g '#00AABB'
    #   2. Color names that are defined elswhere. e.g: 'red'
    #   3. Arrays of values. e.g [ 'white', '_#000000', 'bold' ]
    # If a value starts with an underscore (e.g _red) the color will
    # be used as the background color.
    # Strings can then be colored using the c_<key> method.
    #   e.g: mycolor => [ "#FFFFFF", "_black" ]
    #        mystyle => [ "mycolor", "bold" ]
    #        "Hello".c_mystyle   <- Colored white on black in bold
    module Colorizer

        def self.send_call(rainbow, color_expr)
            prefix = color_expr.start_with?("_") ? "bg" : "c"
            color_expr = color_expr.sub("_", "")

            # If the color expression is a HEX (e.g #FF3400) ..
            if color_expr.start_with?("#")
                # Send the call directly to Rainbow
                return rainbow.send(prefix == "bg" ? :background : :color, color_expr)
            else
                # Otherwise forward the call
                return rainbow.send("#{prefix}_#{color_expr}".to_sym)
            end
        end

        def self.define_methods(hash)
            Rainbow::X11ColorNames::NAMES.each do |color_name, _|
                define_method "c_#{color_name}".to_sym do
                    Rainbow(self).color(color_name.to_sym)
                end
                define_method "bg_#{color_name}".to_sym do
                    Rainbow(self).send(:background, color_name)
                end
            end

            hash.each do |key, color_names|
                array = color_names.is_a?(Array) ? color_names : [ color_names ]
                define_method "c_#{key}".to_sym do
                    rainbow = Rainbow(self)
                    array.each { |color_name|
                        rainbow = Colorizer.send_call(rainbow, color_name)
                    }
                    rainbow
                end
                define_method "bg_#{key}".to_sym do
                    rainbow = Rainbow(self)
                    array.each { |color_name|
                        rainbow = Colorizer.send_call(rainbow, "_#{color_name}")
                    }
                    rainbow
                end
            end
        end

        def self.uncolorize(string)
            string.gsub(/\e\[([;\d]+)?m/, '')
        end

        def self.color?(string)
            !!/\e\[([;\d]+)?m/.match(string)
        end

        def color(color)
            method = "c_#{color}".to_sym
            return self unless self.respond_to?(method)
            self.send(method)
        end

        def c_bold
            Rainbow(self).bold
        end

        def c_italic
            Rainbow(self).italic
        end

        def c_underline
            Rainbow(self).underline
        end

        def c_none
            self
        end

        def color_bg(color)
            Rainbow(self).background(color)
        end

        def color_regex(hash)
            hash.each do |key, val|
                self.gsub!(Regexp.new(key)) { |match|
                    "#{match.color(val)}"
                 }
            end
            self
        end

        def color_unique()
            r, g, b = Colorizer.unique_color(self)
            return Rainbow(self).color(r, g, b)
        end

        def bg_color_unique()
            r, g, b = Colorizer.unique_color(self)
            return Rainbow(self).background(r, g, b)
        end

        # def self.unique_color(string)
        #     r = Random.new(Integer("0x#{Digest::SHA256.hexdigest(string)}"))
        #     number = r.rand(0..360)
        #     r, g, b = VsuColorUtil.hsl_to_rgb(number, 60, 50)
        #     return r, g, b
        # end

        def self.attach(theme, use_colors: true)
            define_methods(theme)
            use_colors = Settings.use_colors
            use_colors &&= Settings.use_colors_in_pipe if !$stdout.isatty
            Rainbow.enabled = use_colors
            String.class_eval { include PgTools::Colorizer }
        end

    end
    
end