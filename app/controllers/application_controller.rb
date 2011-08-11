class ApplicationController < ActionController::Base
  protect_from_forgery

  def user
    User.find_by_id(session[:id])
  end

  private
  def require_login
    if user.nil?
      flash[:error] = "You need to sign in to view that page." 
      redirect_to signup_url
    end
  end

end
