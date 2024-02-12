# Require all module files
Dir[File.join(__dir__, "**", '*.rb')].sort.each { |file| require file }
require 'io/console'

module PgVerify
    module Shell

        def self.in_pipe?()
            !in_tty?()
        end

        def self.in_tty?()
            $stdout.isatty
        end

        def self.gen_prompt(level)
            prompt = Settings.prompt.prompt_format % Settings.prompt[level]
            prompt.color(level)
        end

        def self.expand_to_console(left, right)
            return "#{left}    (#{right})" unless Settings.allow_right_print
            return "#{left}    (#{right})" unless Shell.in_tty?
            width = IO.console.nil? ? 4 : IO.console.winsize[1]
            space = " " * [(width -(Colorizer.uncolorize(left).length + Colorizer.uncolorize(right).length)), 1].max
            return left + space.c_sidenote + right
        end

    end
end
