
default task :gen_examples do
    files = Dir["integration_tests/ruby_dsl/*.rb"].select(&:file?)

    target_dir = "examples"
    FileUtils.rm_rf(target_dir); FileUtils.mkdir(target_dir)

    files.each { |file|
        json = x "./devpg show json --script #{file}", verbose: false
        yaml = x "./devpg show yaml --script #{file}", verbose: false
        json_file = File.join(target_dir, File.basename(file, '.rb') + ".json")
        yaml_file = File.join(target_dir, File.basename(file, '.rb') + ".yaml")
        FileUtils.cp(file, File.join(target_dir, File.basename(file)))
        File.write(json_file, json)
        File.write(yaml_file, yaml)

        x "tar -czf examples.tar.gz examples/"
    }
    FileUtils.rm_rf(target_dir)

    "Generated examples.tar.gz"
end