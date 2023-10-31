# frozen_string_literal: true

require_relative "progg/version"
require_relative "progg/core/core.rb"
require_relative "progg/ebnf_parser/ebnf_parser.rb"
require_relative "progg/model/model.rb"
require_relative "progg/interpret/interpret.rb"
require_relative "progg/cli/cli.rb"
require_relative "progg/transform/transform.rb"
require_relative "progg/nusmv/nusmv.rb"
require 'config'
require 'fileutils'

module Progg

    def self.init()
        config_paths = []
        config_paths << File.expand_path('data/config/progg.yml', self.root)
        config_paths << File.expand_path('.progg.yml', Dir.home)
        config_paths << File.expand_path('.progg.yml', Dir.pwd)
        config_paths.select! { |path| File.file?(path) }
        Config.load_and_set_settings(*config_paths)

        Colorizer.attach(Settings.colors.to_h)
    end

    def self.root()
        File.expand_path(File.join(__dir__, ".."))
    end

end

Progg.init()
