class CreateCharacters < ActiveRecord::Migration
  def self.up
    create_table :characters do |t|
      t.references :user
      t.string :name
      t.integer :gold
      t.string :inventory

      t.timestamps
    end
  end

  def self.down
    drop_table :characters
  end
end
