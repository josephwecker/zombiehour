class AddWeaponsToItems < ActiveRecord::Migration
  def self.up
    add_column :items, :damage_base, :integer
    add_column :items, :damage_range, :integer
    add_column :items, :damage_type, :string
    add_column :items, :strength, :integer
    add_column :items, :control, :integer
  end

  def self.down
    remove_column :items, :damage_base
    remove_column :items, :damage_range
    remove_column :items, :damage_type
    remove_column :items, :strength
    remove_column :items, :control
  end
end
