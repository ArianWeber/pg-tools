# frozen_string_literal: true

require "bundler/gem_tasks"
require "rspec/core/rake_task"

RSpec::Core::RakeTask.new(:spec)

task default: :spec

task :itest do
    versions = [ "3.0", "2.6" ]
    versions.each { |v| test_ruby_version(v) }
end


def test_ruby_version(version)

    message = "Running tests on ruby v#{version}"
    puts "#" * message.length
    puts message
    puts "#" * message.length

    FileUtils.rm_rf(".pg-work")
    FileUtils.mkdir(".pg-work")

    # Generate docker file
    dockerfile_path = File.join(".pg-work", "Dockerfile")
    image_name = "pg-tools-test"
    dockerfile = [ "FROM ruby:#{version}" ]
    File.write(dockerfile_path, dockerfile.join("\n"))
    sh "docker build --file #{dockerfile_path} --tag #{image_name} ."

    test_cmd = []
    test_cmd << "rm -f Gemfile.lock"
    test_cmd << "bundle config set without packaging documentation"
    test_cmd << "gem update --system --silent --no-document"
    test_cmd << "bundle lock"
    test_cmd << "bundle install --path /tmp/gems"
    test_cmd << "ruby --version"
    test_cmd << "bundle exec rspec"
    test_cmd << "bundle exec rake build"
    test_cmd << "gem install pkg/*"
    test_cmd << "pg-tools --help"
    test_cmd << "pg-tools doctor"
    test_cmd = test_cmd.join(" && ")

    docker_run = [ "docker run" ]
    docker_run << "--rm"
    docker_run << "--name pg-tools-container"
    docker_run << "--mount type=bind,source='#{Dir.pwd}',target=/app"
    docker_run << "--workdir /app"
    docker_run << image_name
    docker_run << "bash -c \"#{test_cmd}\""
    docker_run = docker_run.join(" ")

    sh docker_run

    gem_file = Dir[File.join("pkg", "*.gem")].first
    FileUtils.mkdir_p("out")
    out_path = File.join("out", File.basename(gem_file).gsub(".gem", "-r#{version}.gem"))
    mv gem_file, out_path
end
