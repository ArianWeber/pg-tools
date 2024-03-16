# Require all module files
Dir[File.join(__dir__, "**", '*.rb')].sort.each { |file| require file }


module PgVerify
    module Puml

        def self.find_path
            # Return by settings path if that exists
            return Settings.puml.path if !Settings.puml.path.blank? && File.file?(Settings.puml.path)

            # Fall back to looking in the addon directory
            candidates = Dir[File.join(PgVerify.addon_dir, "plantuml-*.jar")]
            return candidates.sort.first unless candidates.empty?
        end

        def self.convert_file(in_path, out_path)
            # CALL WITH java -Djava.awt.headless=true -jar addon/plantuml-1.2023.13.jar --help
            # TODO: The PlantUML jar switches focus to the desktop is if it would
            # attempt to open a window each time it is invoked.
        end

    end
end
