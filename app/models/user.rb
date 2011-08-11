require 'digest/sha2'

class User < ActiveRecord::Base
  has_many :characters

  attr_reader :password

  def password=(pwd)
    @password = pwd
    return if pwd.blank?
    create_salt
    self.passcode = User.encrypt_password(self.password, self.salt)
  end

  validates_presence_of    :login
  validates_uniqueness_of  :login

  validates_confirmation_of :password, :on => :create
  validates_length_of :password, :in => 6..40, :on => :create

  def self.authenticate(user_info)
    if user = find_by_login(user_info[:login])
      test_pass = encrypt_password(user_info[:password], user.salt)
      if user.passcode == test_pass
        return user
      end
    end
    return nil
  end

  private

  def self.encrypt_password(password, salt)
    salted_password = salt + password + " Sam ist awesomste"
    Digest::SHA2.hexdigest(salted_password)
  end
  
  def create_salt
    self.salt = ActiveSupport::SecureRandom.base64(8)
  end

end
