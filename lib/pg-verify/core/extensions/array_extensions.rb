class Array

    def gsub(elem, substitute)
        return self.map { |orig| orig == elem ? substitute : orig }
    end

    def blank?()
        return empty?()
    end

end
