class Array

    def gsub(elem, substitute)
        return self.map { |orig| orig == elem ? substitute : orig }
    end

end
