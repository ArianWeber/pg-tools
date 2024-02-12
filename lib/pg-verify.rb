# frozen_string_literal: true

require_relative "pg-verify/version"
require_relative "pg-verify/core/core.rb"
require_relative "pg-verify/shell/shell.rb"
require_relative "pg-verify/doctor/doctor.rb"
require_relative "pg-verify/model/model.rb"
require_relative "pg-verify/interpret/interpret.rb"
require_relative "pg-verify/cli/cli.rb"
require_relative "pg-verify/transform/transform.rb"
require_relative "pg-verify/nusmv/nusmv.rb"
require_relative "pg-verify/puml/puml.rb"
require_relative "pg-verify/simulation/simulation.rb"
require 'config'
require 'fileutils'
require 'open3'

module PgVerify

    def self.init()
        config_paths = []
        config_paths << File.expand_path('data/config/pg-verify.yml', self.root)
        config_paths << File.expand_path('.pg-verify.yml', Dir.home)
        config_paths << File.expand_path('.pg-verify.yml', Dir.pwd)
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

PgVerify.init()
