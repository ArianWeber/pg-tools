require_relative 'loading_animation.rb'

module PgTools

    module Shell

        class NoAnimation < LoadingAnimation

            def initialize(loading_message)
                super(loading_message)
            end

            def on_anim(passed_time)
                nil
            end

        end

    end
end
