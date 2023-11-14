# frozen_string_literal: true

require_relative "progg/version"
require_relative "progg/core/core.rb"
require_relative "progg/ebnf_parser/ebnf_parser.rb"
require_relative "progg/model/model.rb"
require_relative "progg/interpret/interpret.rb"
require_relative "progg/cli/cli.rb"
require_relative "progg/transform/transform.rb"
require_relative "progg/nusmv/nusmv.rb"
require_relative "progg/simulation/simulation.rb"
require 'config'
require 'fileutils'
require 'open3'

module PgTools

    def self.init()
        config_paths = []
        config_paths << File.expand_path('data/config/progg.yml', self.root)
        config_paths << File.expand_path('.progg.yml', Dir.home)
        config_paths << File.expand_path('.progg.yml', Dir.pwd)
        config_paths.select! { |path| File.file?(path) }
        Config.load_and_set_settings(*config_paths)

        Colorizer.attach(Settings.theme.to_h)
    end

    def self.root()
        File.expand_path(File.join(__dir__, ".."))
    end

    def self.tmp_file(relative_path)
        path = File.expand_path(relative_path, Settings.workdir)
        FileUtils.mkdir_p(File.dirname(path))
        return path
    end

end

PgTools.init()
