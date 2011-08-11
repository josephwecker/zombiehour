class District < ActiveRecord::Base
  belongs_to :city
  has_many :point_of_interests

end
