class CreatePointOfInterests < ActiveRecord::Migration
  def self.up
    create_table :point_of_interests do |t|
      t.references :district
      t.string :name
      t.string :stocks

      t.timestamps
    end
  end

  def self.down
    drop_table :point_of_interests
  end
end
