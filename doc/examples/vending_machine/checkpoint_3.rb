# Add states and variables

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
        press_states = products.keys.map { |product| :"press_#{product}" }
        grab_states  = products.keys.map { |product| :"grab_#{product}" }
        insert_states = coins.keys.map { |coin| :"insert_#{coin}" }
  
        var pocket_money: (0..MAX_MONEY), init: START_MONEY
        var value_in_products: (0..MAX_MONEY), init: 0
  
        states :inserting, *insert_states, \
               :pressing, *press_states, \
               :waiting, :grab_product, \
               :grab_change, \
               :done, \
               init: :inserting
    end
  
    graph :CoinReader do
      states :reading
  
      var read_value: (0..coins.values.max), init: 0
    end
  
    graph :Controller do
      var budget: (0..MAX_MONEY), init: 0
  
      dispense_states = products.keys.map { |product| :"dispense_#{product}" }
      states :accepting, *dispense_states, :rejected, :done, init: :accepting
  
    end
  
    graph :LED do
      states :idle, :red, :green, init: :idle
    end
  
    graph :Dispenser do
      dispensed_states = products.keys.map { |product| :"dispensed_#{product}" }
      states :empty, *dispensed_states, init: :empty
    end
  
    graph :CoinDispenser do
      states :idle, :dispensed_change, init: :idle
      var change: (0..MAX_MONEY), init: 0
    end
  
end
