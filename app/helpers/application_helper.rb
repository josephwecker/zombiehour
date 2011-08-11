module ApplicationHelper
  def user
    User.find_by_id(session[:id])
  end
end
