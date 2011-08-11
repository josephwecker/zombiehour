require 'test_helper'

class ToolKitsControllerTest < ActionController::TestCase
  setup do
    @tool_kit = tool_kits(:one)
  end

  test "should get index" do
    get :index
    assert_response :success
    assert_not_nil assigns(:tool_kits)
  end

  test "should get new" do
    get :new
    assert_response :success
  end

  test "should create tool_kit" do
    assert_difference('ToolKit.count') do
      post :create, :tool_kit => @tool_kit.attributes
    end

    assert_redirected_to tool_kit_path(assigns(:tool_kit))
  end

  test "should show tool_kit" do
    get :show, :id => @tool_kit.to_param
    assert_response :success
  end

  test "should get edit" do
    get :edit, :id => @tool_kit.to_param
    assert_response :success
  end

  test "should update tool_kit" do
    put :update, :id => @tool_kit.to_param, :tool_kit => @tool_kit.attributes
    assert_redirected_to tool_kit_path(assigns(:tool_kit))
  end

  test "should destroy tool_kit" do
    assert_difference('ToolKit.count', -1) do
      delete :destroy, :id => @tool_kit.to_param
    end

    assert_redirected_to tool_kits_path
  end
end
