require 'test_helper'

class FirearmsControllerTest < ActionController::TestCase
  setup do
    @firearm = firearms(:one)
  end

  test "should get index" do
    get :index
    assert_response :success
    assert_not_nil assigns(:firearms)
  end

  test "should get new" do
    get :new
    assert_response :success
  end

  test "should create firearm" do
    assert_difference('Firearm.count') do
      post :create, :firearm => @firearm.attributes
    end

    assert_redirected_to firearm_path(assigns(:firearm))
  end

  test "should show firearm" do
    get :show, :id => @firearm.to_param
    assert_response :success
  end

  test "should get edit" do
    get :edit, :id => @firearm.to_param
    assert_response :success
  end

  test "should update firearm" do
    put :update, :id => @firearm.to_param, :firearm => @firearm.attributes
    assert_redirected_to firearm_path(assigns(:firearm))
  end

  test "should destroy firearm" do
    assert_difference('Firearm.count', -1) do
      delete :destroy, :id => @firearm.to_param
    end

    assert_redirected_to firearms_path
  end
end
