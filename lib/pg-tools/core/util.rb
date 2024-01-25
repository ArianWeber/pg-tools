module PgTools


    module TimeUtil

        SECONDS_IN_SECOND = 1
        SECONDS_IN_MINUTE = SECONDS_IN_SECOND * 60
        SECONDS_IN_HOUR   = SECONDS_IN_MINUTE * 60
        SECONDS_IN_DAY    = SECONDS_IN_HOUR   * 24
        SECONDS_IN_WEEK   = SECONDS_IN_DAY    * 7
        SECONDS_IN_MONTH  = SECONDS_IN_WEEK   * 4
        SECONDS_IN_YEAR   = SECONDS_IN_MONTH  * 12

        def self.duration_string(seconds, short: false)
            i_seconds = seconds.to_i
            return "~#{'%.2f' % (seconds*1000)}#{short ? "ms" : " milliseconds"}" if i_seconds == 0
            return duration_to_h(i_seconds, short: short)
        end

        def self.duration_to_h(seconds, short: false)
            seconds = seconds.to_i
            {  
                SECONDS_IN_YEAR   => ( short ? "y" : " year"   ),
                SECONDS_IN_MONTH  => ( short ? "M" : " month"  ),
                SECONDS_IN_WEEK   => ( short ? "w" : " week"   ),
                SECONDS_IN_DAY    => ( short ? "d" : " day"    ),
                SECONDS_IN_HOUR   => ( short ? "h" : " hour"   ),
                SECONDS_IN_MINUTE => ( short ? "m" : " minute" ),
                SECONDS_IN_SECOND => ( short ? "s" : " second" )
            }.each do |unit_seconds, unit_string|
                units = seconds / unit_seconds
                unit_string = unit_string + "s" if !short && units != 1
                return "#{units}#{unit_string} #{duration_to_h(seconds % unit_seconds, short: short)}".strip unless units == 0
            end
            nil
        end

        def self.ago_h(start_time, short: false)
            duration_to_h((Time.now - start_time).to_i, short: short) + " ago"
        end

        def self.timestamp(time = Time.new)
            return time.utc.to_i
        end

        def self.from_timestamp(ts)
            time = Time.at(ts.to_i).utc
            return time
        end

    end

    class StringUtil

        def self.make_unique(string, strings, &blk)
            base_string = string
            index = 0
            while strings.include?(string)
                index += 1
                string = blk.call(base_string, index)
            end
            string
        end

        def self.limit_width(string, width)
            return string if string.nil? || string.length <= width
            return string.chars.each_slice(width).map(&:join).join("\n")
        end

        def self.auto_complete(string, options)
            perfect_match = options.select { |o| o == string }.uniq
            return perfect_match unless perfect_match.empty?
            options.select { |o| o.start_with?(string) }.uniq
        end

        def self.levenshtein_suggest(string, options, suggestions: 5)
            options.map { |o| [o, levenshtein_distance(string, o)] }
                .sort_by{ |a| a[1] }
                .vsu_limit(suggestions)
                .map { |a| a[0] }
        end

        def self.line_combine(string1, string2, separator: " ")
            return string2 if string1.empty?
            lines1, lines2 = string1.split("\n"), string2.split("\n")
            both = [lines1, lines2]
            height  = both.map(&:length).max
            l_width = lines1.map(&:display_length).max

            # Fill up empty lines to match height
            both.each { |lines| loop { break if lines.length >= height; lines << "" } }
            
            # Fill up left lines to align right side
            lines1 = lines1.map { |l| l + " " * (l_width - l.display_length) }

            # Combine left and right.
            string = (0...height).map { |index|
                lines1[index] + separator + lines2[index]
            }.join("\n")

            return string
        end

        def self.shorten_unique(strings)
            # TODO: Implement
            return strings.each_with_index.map { |s, i| [s, i.to_s] }.to_h
           
            # chars = ".- _".chars
            # regex = /#{chars.map { |c| "\\#{c}" }.join("|")}/
            # map = {}
            # strings.each do |str|
            #     index = 0
            #     loop do 
            #         split = str.gsub(regex, " ").split
            #         short = split.map { |word| word[0, index] }.join("")
            #         puts short
            #         sleep(1)
            #         next if map.values.include?(short)
            #         map[str] = short
            #         break
            #     end
                
            # end

            # map
        end

        def self.indented(string, num_indents: 1, indent_string: "\t")
            string.split("\n").map { |l| "#{indent_string * num_indents}#{l}" }.join("\n")
        end

        def self.levenshtein_distance(a, b)
            a, b = a.downcase, b.downcase
            costs = Array(0..b.length) # i == 0
            (1..a.length).each do |i|
                costs[0], nw = i, i - 1  # j == 0; nw is lev(i-1, j)
                (1..b.length).each do |j|
                    costs[j], nw = [costs[j] + 1, costs[j-1] + 1, a[i-1] == b[j-1] ? nw : nw + 1].min, costs[j]
                end
            end
            costs[b.length]
        end

    end

end