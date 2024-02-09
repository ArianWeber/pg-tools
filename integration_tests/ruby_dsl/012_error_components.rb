model :Test do

    transient  error :TransientError
    persistent error :PersistentError
    
    specify "The persistent error" do
        it "Stays persistent" => :"G ( PersistentError == Yes => ( ! F PersistentError == No ) )"
    end
end
