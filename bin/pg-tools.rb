require "bundler/setup"
require 'pg-tools'

module PgTools
    
    class PgToolsCLI

        def run()
            begin
                Cli::BaseCommand.start()
            rescue Core::Error => e
                puts e.to_formatted()
            end
        end

    end

end
