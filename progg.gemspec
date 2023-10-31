# frozen_string_literal: true

require_relative "lib/progg/version"

Gem::Specification.new do |spec|
  spec.name = "progg"
  spec.version = Progg::VERSION
  spec.authors = ["Arian Weber"]
  spec.email = ["weber.arian@web.de"]

  spec.summary = "Tool for the decleration and transformation of program graphs"
  # spec.description = "TODO: Write a longer description or delete this line."
  spec.homepage = "https://github.com/ArianWeber/progg"
  spec.required_ruby_version = ">= 2.6.0"

  # spec.metadata["allowed_push_host"] = "TODO: Set to your gem server 'https://example.com'"

  spec.metadata["homepage_uri"] = spec.homepage
  # spec.metadata["source_code_uri"] = "TODO: Put your gem's public repo URL here."
  # spec.metadata["changelog_uri"] = "TODO: Put your gem's CHANGELOG.md URL here."

  # Specify which files should be added to the gem when it is released.
  # The `git ls-files -z` loads the files in the RubyGem that have been added into git.
  spec.files = Dir.chdir(__dir__) do
    `git ls-files -z`.split("\x0").reject do |f|
      (f == __FILE__) || f.match(%r{\A(?:(?:bin|test|spec|features)/|\.(?:git|travis|circleci)|appveyor)})
    end
  end
  spec.bindir = "exe"
  spec.executables = spec.files.grep(%r{\Aexe/}) { |f| File.basename(f) }
  spec.require_paths = ["lib"]

  # Gem dependencies
  spec.add_dependency "thor", "~> 1.2.1"
  spec.add_dependency "ebnf", "~> 2.3.4"
  spec.add_dependency "rainbow", "~> 3.0.0"
  spec.add_dependency "config", "~> 4.2.1"
  spec.add_dependency "plantuml_builder", "~> 0.3.0"
  
end
