class AddApparelsToItems < ActiveRecord::Migration
  def self.up
    add_column :items, :coverage, :string
    add_column :items, :puncture_resistance, :integer
    add_column :items, :sunder_resistance, :integer
    add_column :items, :rupture_resistance, :integer
    add_column :items, :psionic_resistance, :integer
    add_column :items, :temperature_resistance, :integer
    add_column :items, :flexibility, :integer
    add_column :items, :comfort, :integer
  end

  def self.down
    remove_column :items, :coverage
    remove_column :items, :puncture_resistance
    remove_column :items, :sunder_resistance
    remove_column :items, :rupture_resistance
    remove_column :items, :psionic_resistance
    remove_column :items, :temperature_resistance
    remove_column :items, :flexibility
    remove_column :items, :comfort
  end
end
