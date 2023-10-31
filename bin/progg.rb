require "bundler/setup"
require 'progg'

module Progg
    
    class ProggCLI

        def run()

            Cli::BaseCommand.start()
            # executor = Cli::CommandExecutor.new

            # executor.exec(ARGV)

            # self.parse(ARGV)

            # script = ProggScript.new()
            # script.interpret('program-graph.rb')

            # if params[:help]
            #     print help
            # elsif params.errors.any?
            #     puts params.errors.summary
            # else
            #     pp params.to_h
            # end
        end

    end

end
