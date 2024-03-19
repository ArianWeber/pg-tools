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

  transient error :CoinReadFails

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

      # The user can insert coins until the money runs out
      coins.each { |coin, value|
        transition :inserting => :"insert_#{coin}" do
          guard  "pocket_money - #{value} >= 0"
          action "pocket_money := pocket_money - #{value}"
        end
        transition :"insert_#{coin}" => :inserting
      }

      # The user can decide at any point to start pressing buttons
      transition :inserting => :pressing

      # Press one button and wait for the result
      products.each { |product, value|
        transition :pressing => :"press_#{product}"
        transition :"press_#{product}" => :waiting
      }

      # Grab a product and take note of the value
      products.each { |product, value|
        transition :waiting => :grab_product do
          precon "value_in_products + #{value} < #{MAX_MONEY}"
          guard  "LED == green && Dispenser == dispensed_#{product}"
          action "value_in_products := value_in_products + #{value}"
        end
      }
      transition :grab_product => :grab_change

      # Grab change directly 
      transition :waiting => :grab_change do
        guard "LED == red"
      end


      # Grab the change and be done
      transition :grab_change => :done do
        precon "pocket_money + change <= #{MAX_MONEY}"
        action "pocket_money := pocket_money + change"
      end
  end

  graph :CoinReader do
    states :reading

    var read_value: (0..coins.values.max), init: 0

    coins.each { |coin, value|
      transition :reading => :reading do
        guard  "User == insert_#{coin} && CoinReadFails == No"
        action "read_value := #{value}"
      end
    }

    transition :reading => :reading do
      guard  coins.keys.map { |coin| "User != insert_#{coin}" }.join(" && ")
      action "read_value := 0"
    end

  end

  graph :Controller do
    var budget: (0..MAX_MONEY), init: 0

    dispense_states = products.keys.map { |product| :"dispense_#{product}" }

    states :accepting, *dispense_states, :rejected, :done, init: :accepting

    transition :accepting => :accepting do
      precon "budget + read_value <= #{MAX_MONEY}"
      guard "read_value > 0"
      action "budget := budget + read_value"
    end

    products.each { |product, value|
      transition :accepting => :"dispense_#{product}" do
        guard  "User == press_#{product} && budget >= #{value}"
        action "budget := budget - #{value}"
      end
      transition :"dispense_#{product}" => :done

      transition :accepting => :rejected do
        guard "User == press_#{product} && budget < #{value}"
      end
    }

    transition :accepting => :accepting do
      guard  "CoinDispenser == dispensed_change"
      action "budget := 0"
    end
    transition :done => :accepting do
      guard  "CoinDispenser == dispensed_change"
      action "budget := 0"
    end


  end

  graph :LED do
    states :idle, :red, :green, init: :idle

    transition :idle => :green do
      guard "Controller == done"
    end
    transition :idle => :red do
      guard "Controller == rejected"
    end

  end

  graph :Dispenser do
    dispensed_states = products.keys.map { |product| :"dispensed_#{product}" }
    states :empty, *dispensed_states, init: :empty

    # Dispense the product the Controller tells us to
    products.each { |product, value|
      transition :empty => :"dispensed_#{product}" do
        guard "Controller == dispense_#{product}"
      end
    }
  end

  graph :CoinDispenser do
    states :idle, :dispensed_change, init: :idle
    var change: (0..MAX_MONEY), init: 0

    # Eject the change money
    transition :idle => :dispensed_change do
      guard  "Controller == rejected || Controller == done"
      action "change := budget"
    end

  end

  specify "The vending machine" do

    it "allows the user to buy something" => :"EF value_in_products > 0"
    it "always completes the transaction" => :"F User == done"

    products.each { |product, value|
      assuming "the product can be bough" => :"pocket_money >= #{value}" do
        it "allows the user to buy #{product}" => :"EF Dispenser == dispensed_#{product}"
      end
    }

  end

  hazard "The user looses money" => :"User == done && pocket_money + value_in_products < #{START_MONEY}"
  hazard "The machine looses money" => :"User == done && pocket_money + value_in_products > #{START_MONEY}"

end
