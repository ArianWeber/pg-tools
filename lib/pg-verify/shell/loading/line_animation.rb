require_relative 'loading_animation.rb'

module PgTools
    module Shell

        class LineAnimation < LoadingAnimation

            def initialize(loading_message)
                super(loading_message)
                @last_line = nil
            end

            def on_anim(passed_time)
                color  = (passed_time * 2).to_i % 2 == 0 ? :loading : :white
                circle = (passed_time * 2).to_i % 2 == 0 ? "▶" : "-" 
                circle = circle.color(color)
                " #{circle}  #{string = format_line(@last_line)}  "
            end

            def printl(string)
                return unless started?()
                string = string.to_s
                string = (string ||= "").gsub("\n", '')
                string = string.c_sidenote unless Colorizer.color?(string)
                @last_line = string
                # print_anim(string)
            end

            def format_line(string)
                return loading_message() if string.blank?
                "#{loading_message()} [ #{string} ]"
            end

        end
    end
end
