class AdminController < ApplicationController
  before_filter :admin_required

  protected
  def admin_required
    authenticate_or_request_with_http_basic do |user_name, password|
      user_name == 'admin' && password == 'thisisit'
    #end if RAILS_ENV == 'production' || params[:admin_http]
    end
  end
end
