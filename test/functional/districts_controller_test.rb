require 'test_helper'

class DistrictsControllerTest < ActionController::TestCase
  setup do
    @district = districts(:one)
  end

  test "should get index" do
    get :index
    assert_response :success
    assert_not_nil assigns(:districts)
  end

  test "should get new" do
    get :new
    assert_response :success
  end

  test "should create district" do
    assert_difference('District.count') do
      post :create, :district => @district.attributes
    end

    assert_redirected_to district_path(assigns(:district))
  end

  test "should show district" do
    get :show, :id => @district.to_param
    assert_response :success
  end

  test "should get edit" do
    get :edit, :id => @district.to_param
    assert_response :success
  end

  test "should update district" do
    put :update, :id => @district.to_param, :district => @district.attributes
    assert_redirected_to district_path(assigns(:district))
  end

  test "should destroy district" do
    assert_difference('District.count', -1) do
      delete :destroy, :id => @district.to_param
    end

    assert_redirected_to districts_path
  end
end
