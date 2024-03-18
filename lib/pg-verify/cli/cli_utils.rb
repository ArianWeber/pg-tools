module PgVerify
    class CliUtils
        def self.load_models(options)
            dsl_script_file = options[:script]
            json_file       = options[:"json-file"]
            yaml_file       = options[:"yaml-file"]
            default_script_file = Settings.ruby_dsl.default_script_name

            unless dsl_script_file.nil?
                raise NoSuchFileError.new(dsl_script_file) unless File.file?(dsl_script_file)
                return Interpret::PgScript.new.interpret(dsl_script_file)
            end

            unless json_file.nil?
                raise NoSuchFileError.new(json_file) unless File.file?(json_file)
                json_string = File.read(json_file)
                array = JSON.load(json_string)
                array = [ array ] unless array.is_a?(Array)
                return array.map { |hash| Transform::HashTransformation.new.parse_graph(hash) }
            end

            unless yaml_file.nil?
                raise NoSuchFileError.new(yaml_file) unless File.file?(yaml_file)
                array = YAML.load_file(yaml_file)
                array = [ array ] unless array.is_a?(Array)
                return array.map { |hash| Transform::HashTransformation.new.parse_graph(hash) }
            end

            raise NoDefaultFileError.new(default_script_file) unless File.file?(default_script_file)
            return Interpret::PgScript.new.interpret(dsl_script_file)
        end

    end

    class NoSuchFileError < PgVerify::Core::Error
        def initialize(path)
            @path = path
        end

        def formatted()
            title = "No such file!"
            body = "There is no file at #{@path.c_file}!"
            hint = "Make sure to specify another file or specify another path"
            return title, body, hint
        end
    end

    class NoDefaultFileError < PgVerify::Core::Error
        def initialize(default_path)
            @default_path = default_path
        end
        def formatted()
            title = "Nothing to interpret!"
            body  = "You didn't specify a file to interpret and there is no DSL script\n"
            body += "at the default location: #{@default_path.c_file} (#{File.expand_path(@default_path).c_sidenote})"
            hint = "Make sure to specify another file or specify another path"
            return title, body, hint
        end
    end

end