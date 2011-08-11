require 'test_helper'

class IntroControllerTest < ActionController::TestCase
  test "should get index" do
    get :index
    assert_response :success
  end

  test "should get controls" do
    get :controls
    assert_response :success
  end

  test "should get contact" do
    get :contact
    assert_response :success
  end

end
