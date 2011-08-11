class AddFirearmsToItems < ActiveRecord::Migration
  def self.up
    add_column :items, :fire_damage_base, :integer
    add_column :items, :fire_damage_range, :integer
    add_column :items, :rate_of_fire, :string
    add_column :items, :clip_type, :string
    add_column :items, :ammo_type, :string
  end

  def self.down
    remove_column :items, :fire_damage_base
    remove_column :items, :fire_damage_range
    remove_column :items, :rate_of_fire
    remove_column :items, :clip_type
    remove_column :items, :ammo_type
  end
end
