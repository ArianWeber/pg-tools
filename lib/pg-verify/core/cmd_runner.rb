module PgVerify

    module Core

        module CMDRunner

            # Runs the command and raises on error.
            def self.run_cmd(cmd, env_variables = {}, include_stderr: false)
                start_time = Time.new
                output, err, status = Open3.capture3(env_variables, cmd)
                delta_seconds = (Time.new - start_time).to_i
                raise CMDRunnerError.new(cmd, output, err, delta_seconds) unless status.success?
                return output + err if include_stderr
                return output
            end

            # Runs the command and returns stdout on success. Returns the specified value otherwise.
            def self.run_or_return(cmd, fail_output = nil)
                output, err, status = Open3.capture3(cmd)
                return status.success? ? output : fail_output
            end

            # Runs the command and returns stdout, stderr and the status.
            # Status will be true only if the command succeeded.
            def self.run_for_result(cmd)
                output, err, status = run_for_result_with_plain_status(cmd)
                return output, err, status.success?
            end

            def self.run_for_result_with_plain_status(cmd)
                output, err, status = Open3.capture3(cmd)
                return output, err, status
            end

            def self.run_for_exit_code(cmd)
                output, err, status = Open3.capture3(cmd)
                return status.exitstatus
            end

            def self.drop_into_shell()
                shell = run_cmd("echo ${SHELL}")
                system("#{shell}")
            end

            def self.run_in_screen(cmd, session_name)
                run_cmd("screen -S #{session_name} -dm bash -c '#{cmd}'")
            end

            def self.run_with_timeout(cmd)
                require 'timeout'
                Open3.popen3(cmd) do |stdin, stdout, stderr, wait_thread|
                    Timeout.timeout(2) do
                        wait_thread.join
                    end
                end
            end

            def self.with_drop_on_fail(shell, intent: nil, &blk)
                begin
                    blk.call()
                rescue CMDRunnerError => e
                    shell.info("Output:\n#{e.output}")
                    shell.error("The action that was attempted did fail after #{e.delta_seconds} seconds!")
                    shell.info("The exact command was: #{e.command.c_command}")
                    shell.info("Intent was: #{intent}") unless intent.nil?
                    shell.info("You can try to resolve this problem manually.")
                    raise e unless shell.ask_confirm(nil, question: "Do you want to open a shell and try our luck?")
                    drop_into_shell()
                    shell.info("Welcome back :D")
                
                    op1 = "Run the command again to see if it works."
                    op2 = "Just continue and pretend the command did succeed.\n(This will return '' to the caller)"
                    op3 = "Assume the command failed."
                    sel = shell.select([op1, op2, op3], prompt: "How shall we continue?").first
                    case sel
                    when op1
                        return with_drop_on_fail(shell, blk, intent: intent,)
                    when op2
                        return ""
                    when op3
                        raise e
                    end
                end
            end

            # Runs the specified command and waits for it to complete. Calls the specified
            # block for each line the process writes to stdout and err.
            # Will return two things:
            #   1. The complete output as a string
            #   2. The result as a bool, where true means success.
            # If raise_on_fail is set to true it will raise directly on fail and not return the results.
            # If gulp_interrupt is set to true will also gulp interrupts.
            def self.run_and_follow(cmd, raise_on_fail: true, gulp_interrupt: false, timeout: nil, &blk)
                start_time = Time.new
                output = ""
                success = nil
                
                Open3.popen3(cmd) do |stdin, stdout, stderr, wait_thread|
                    begin
                        # Listen to stdout & stderr in seperate threads.
                        [stdout, stderr].each do |stream|
                            Thread.new do
                                begin
                                    until (line = stream.gets).nil? do
                                        blk.call(line) unless blk.nil?
                                        output += line
                                    end
                                rescue IOError => e
                                    # Expected when the stream is closed
                                end
                            end
                        end
                        unless timeout.nil?
                            require 'timeout'
                            Timeout.timeout(timeout) { wait_thread.join }
                        else
                            wait_thread.join
                        end
                        delta_seconds = Time.new - start_time
                        success = wait_thread.value.success?
                        raise CMDRunnerError.new(cmd, output, "", delta_seconds) if raise_on_fail && !success
                    rescue SignalException => e
                        raise e unless gulp_interrupt
                        success = false
                    end
                    return output, success
                end
            end

            def self.command_exists?(command)
                !run_or_return("command -v '#{command}'", nil).nil?
            end

            class CMDRunnerError < PgVerify::Core::Error
                def initialize(cmd, output, err, delta_seconds)
                    @cmd, @output, @err, @delta_seconds = cmd, output, err, delta_seconds
                end

                def formatted()
                    title = "Running '#{@cmd}' failed after #{@delta_seconds} seconds:"
                    
                    body = "$ #{@cmd.c_string} \n\n #{@err}"
                    return title, body
                end

            end

        end

    end
end
