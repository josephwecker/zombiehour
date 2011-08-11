require 'test_helper'

class ApparelsControllerTest < ActionController::TestCase
  setup do
    @apparel = apparels(:one)
  end

  test "should get index" do
    get :index
    assert_response :success
    assert_not_nil assigns(:apparels)
  end

  test "should get new" do
    get :new
    assert_response :success
  end

  test "should create apparel" do
    assert_difference('Apparel.count') do
      post :create, :apparel => @apparel.attributes
    end

    assert_redirected_to apparel_path(assigns(:apparel))
  end

  test "should show apparel" do
    get :show, :id => @apparel.to_param
    assert_response :success
  end

  test "should get edit" do
    get :edit, :id => @apparel.to_param
    assert_response :success
  end

  test "should update apparel" do
    put :update, :id => @apparel.to_param, :apparel => @apparel.attributes
    assert_redirected_to apparel_path(assigns(:apparel))
  end

  test "should destroy apparel" do
    assert_difference('Apparel.count', -1) do
      delete :destroy, :id => @apparel.to_param
    end

    assert_redirected_to apparels_path
  end
end
