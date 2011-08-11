class Character < ActiveRecord::Base
  belongs_to :user
  validates_presence_of   :name
  validates_uniqueness_of :name, :scope => :user_id
  
  def to_param
    name
  end

end
