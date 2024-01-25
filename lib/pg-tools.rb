# frozen_string_literal: true

require_relative "pg-tools/version"
require_relative "pg-tools/core/core.rb"
require_relative "pg-tools/ebnf_parser/ebnf_parser.rb"
require_relative "pg-tools/model/model.rb"
require_relative "pg-tools/interpret/interpret.rb"
require_relative "pg-tools/cli/cli.rb"
require_relative "pg-tools/transform/transform.rb"
require_relative "pg-tools/nusmv/nusmv.rb"
require_relative "pg-tools/simulation/simulation.rb"
require 'config'
require 'fileutils'
require 'open3'

module PgTools

    def self.init()
        config_paths = []
        config_paths << File.expand_path('data/config/pg-tools.yml', self.root)
        config_paths << File.expand_path('.pg-tools.yml', Dir.home)
        config_paths << File.expand_path('.pg-tools.yml', Dir.pwd)
        config_paths.select! { |path| File.file?(path) }
        Config.load_and_set_settings(*config_paths)

        Colorizer.attach(Settings.theme.to_h)
    end

    def self.root()
        File.expand_path(File.join(__dir__, ".."))
    end

    def self.addon_dir()
        File.expand_path("addon", Dir.pwd)
    end

    def self.tmp_file(relative_path)
        path = File.expand_path(relative_path, Settings.workdir)
        FileUtils.mkdir_p(File.dirname(path))
        return path
    end

end

PgTools.init()
