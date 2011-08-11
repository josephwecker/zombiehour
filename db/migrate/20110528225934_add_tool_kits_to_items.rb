class AddToolKitsToItems < ActiveRecord::Migration
  def self.up
    add_column :items, :tool_type, :string
    add_column :items, :enhancement, :integer
  end

  def self.down
    remove_column :items, :tool_type
    remove_column :items, :enhancement
  end
end
