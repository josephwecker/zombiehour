class City < ActiveRecord::Base

  has_many :districts

  def to_param
    name
  end

end
