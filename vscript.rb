
task :gen_examples do
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

    done "Generated examples.tar.gz"
end

task :integration_test do

    work_dir = ".pg-work"
    FileUtils.rm_rf(work_dir); FileUtils.mkdir(work_dir)

    test_files = Dir["integration_tests/ruby_dsl/*.rb"].sort.select(&:file?)


    # Create json files for all tests
    test_files.each { |source|
        log "JSON #{File.basename(source)}"
        target = File.join(work_dir, File.basename(source, '.rb') + ".json")
        content = x "./devpg show json --script #{source}", verbose: false
        File.write(target, content)
    }

    # Create json files for all tests
    test_files.each { |source|
        log "YAML #{File.basename(source)}"
        target = File.join(work_dir, File.basename(source, '.rb') + ".yaml")
        content = x "./devpg show yaml --script #{source}", verbose: false
        File.write(target, content)
    }

    # Run tests for all files
    test_files.each { |source|
        args = {
            "script" => source,
            "json-file" => File.join(work_dir, File.basename(source, '.rb') + ".json"),
        }
        commands = [ "show puml", "test", "dcca", "show png", "simulate" ]
        args.each { |argument, file|
            commands.each { |command|
                log "pgv #{command} #{argument} | #{File.basename(file)}"
                x "./devpg #{command} --#{argument} #{file}", verbose: false
            }
        }
    }

    done "Great success!"
end