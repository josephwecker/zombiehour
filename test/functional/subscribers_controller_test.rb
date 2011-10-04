require 'test_helper'

class SubscribersControllerTest < ActionController::TestCase
  setup do
    @subscriber = subscribers(:one)
  end

  test "should get index" do
    get :index
    assert_response :success
    assert_not_nil assigns(:subscribers)
  end

  test "should get new" do
    get :new
    assert_response :success
  end

  test "should create subscriber" do
    assert_difference('Subscriber.count') do
      post :create, :subscriber => @subscriber.attributes
    end

    assert_redirected_to subscriber_path(assigns(:subscriber))
  end

  test "should show subscriber" do
    get :show, :id => @subscriber.to_param
    assert_response :success
  end

  test "should get edit" do
    get :edit, :id => @subscriber.to_param
    assert_response :success
  end

  test "should update subscriber" do
    put :update, :id => @subscriber.to_param, :subscriber => @subscriber.attributes
    assert_redirected_to subscriber_path(assigns(:subscriber))
  end

  test "should destroy subscriber" do
    assert_difference('Subscriber.count', -1) do
      delete :destroy, :id => @subscriber.to_param
    end

    assert_redirected_to subscribers_path
  end
end
