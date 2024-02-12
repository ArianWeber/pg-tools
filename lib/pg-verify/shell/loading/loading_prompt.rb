module PgVerify
    module Shell

        module LoadingPrompt

            def self.while_loading(loading_message)
                if Shell.in_pipe? || !Settings.use_animations
                    # Do not play animations when piped
                    animation = NoAnimation.new(loading_message).start
                else
                    animation = LineAnimation.new(loading_message).start
                end

                start_time = Time.now

                begin
                    result = yield(animation) if block_given?
                    delta_time = Time.now - start_time
                rescue StandardError, SignalException, Interrupt, IOError => e
                    exception = e
                ensure
                    status = exception.nil? ? (result.nil? ? :empty : :success) : :error
                    status = result.state if !result.nil? && result.is_a?(LoadingResult)
                    animation.stop(status, result.is_a?(LoadingResult) ? result.message : "")
                    result = result.data if result.is_a?(LoadingResult)
                    raise exception unless exception.nil?
                end

                return result
            end

            class LoadingResult
                attr_accessor :data, :message, :state
                def initialize(data, message, state: :success)
                    @data = data
                    @message = message
                    @state = state
                end
            end

        end
    end
end
