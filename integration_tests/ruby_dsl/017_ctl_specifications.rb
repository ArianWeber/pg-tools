model :CtlSpecifications do

    graph :Example do
        states :one, :two, :three, init: :one

        transition :one => :two
        transition :two => :one
        transition :one => :three

    end

    specify "The model" do
        it "starts in state one" => :"Example == one"
        it "cannot stay in one" => :"! G Example == one"
        it "can stay in three" => :"EX AG Example == three"
        it "cannot go back to one from three" => :"AG ( (Example == three) => ! EF Example == one)"
    end

end
