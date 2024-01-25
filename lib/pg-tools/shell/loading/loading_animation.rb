module PgTools
    module Shell

        class LoadingAnimation

            attr_reader :loading_message, :last_output_length, :start_time

            def initialize(loading_message)
                @loading_message = loading_message
                @last_output_length = 0
                @animation_thread = nil
                @start_time = nil
            end

            def start
                return unless @animation_thread.nil?
                @start_time = Time.now
                on_start()
                @animation_thread = Thread.new {
                    begin
                        while true do
                            passed_time = Time.now - @start_time
                            string = on_anim(passed_time)
                            print_anim(string) unless string.nil?
                            sleep(1.0 / 10)
                        end
                    rescue => e
                        puts e
                        puts e.backtrace 
                        raise e
                    end
                }
                self
            end

            def stop(status, finish_message)
                return if @animation_thread.nil?
                @animation_thread.exit
                prompt = Shell.gen_prompt(status)
                string = on_stop(prompt, finish_message)
                @animation_thread = nil
                print_anim(string)
                puts
            end

            def print_anim(string)
                return if string.blank?
                string = string.to_s
                # Fill the string with spaces to overwrite the last printed string
                print_string = string + ( " " * [0, @last_output_length - string.length].max )
                # Set the length of the last print to the string length without spaces, 
                # since they do not need to be overwritten.
                @last_output_length = string.length
                # Print the string with spaces.
                print("\r#{print_string}")
            end

            def on_anim(passed_time); end
            def on_start(); end
            def printl(string); end

            def on_stop(prompt, finish_message)
                message = "#{prompt} #{@loading_message}"
                message += " | #{finish_message}".c_sidenote unless finish_message.nil? || finish_message.empty?
                if Settings.print_loading_times
                    duration = TimeUtil.duration_string((Time.now - @start_time).to_i)
                    duration_string = duration.nil? ? "" : "Took #{duration}".c_sidenote
                    return Shell.expand_to_console(message, duration_string)
                else
                    return message
                end
            end

            def started?()
                !@animation_thread.nil?
            end

        end
    end
end
