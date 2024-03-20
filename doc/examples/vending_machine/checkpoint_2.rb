# Add products and coins

model :VendingMachine do

    products = {
        cola:  1.2,
        chips: 2.5
    }
    products = products.map { |c, v| [c, (v * 10).to_i] }.to_h
  
    coins = {
        ten_cents: 10,
        twenty_cents: 20,
        fifty_cents: 50,
        one_euro: 100,
        two_euro: 200
    }
    coins = coins.map { |c, v| [c, (v / 10).to_i] }.to_h
  
    MAX_MONEY = 100
    START_MONEY = 20
  
    graph :User do
        states :todo
    end
  
    graph :CoinReader do
        states :todo
    end
  
    graph :Controller do
        states :todo
    end
  
    graph :LED do
        states :todo
    end
  
    graph :Dispenser do
        states :todo
    end
  
    graph :CoinDispenser do
        states :todo
    end
  
end
