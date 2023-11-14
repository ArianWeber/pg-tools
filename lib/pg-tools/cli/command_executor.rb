

module PgTools

    module Cli

        class CommandExecutor

            RegisteredCommand = Struct.new(:file_path, :path)

            def initialize()
            end

            def exec(argv)
                cmd = find_command(argv)
            end

            def find_command(argv)
                cmds = registered_commands()
                current_path = []
                argv.each { |word|
                    current_path << word
                    candidates = cmds.select { |c| }
                }
            end

            def registered_commands()
                cmds = nil
                Dir.chdir(File.join(__dir__, 'commands')) {
                    cmds = Dir[File.join('.', '**', '*.rb')].map { |file|
                        next unless File.file?(file)
                        path = file.gsub('.rb', '').split(File::Separator).reject { |p| p == '.' }
                        RegisteredCommand.new(file, path)
                    }.compact
                }
                return cmds
            end

        end
    end

end