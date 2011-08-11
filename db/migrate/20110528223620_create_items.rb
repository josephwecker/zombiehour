class CreateItems < ActiveRecord::Migration
  def self.up
    create_table :items do |t|
      t.string :type
      t.string :name
      t.string :icon
      t.integer :cost
      t.integer :weight
      t.integer :space
      t.text :description

      t.timestamps
    end
  end

  def self.down
    drop_table :items
  end
end
