require "bundler/setup"
require 'pg-verify'

module PgVerify
    
    class PgVerifyCLI

        def run()
            begin
                Cli::BaseCommand.start()
            rescue Core::Error => e
                puts e.to_formatted()
            end
        end

    end

end
