class UsersController < ApplicationController

  before_filter :require_login, :except => [:index, :new, :create, :login]

  def index
    @users = User.all

    respond_to do |format|
      format.html # index.html.erb
      format.xml  { render :xml => @users }
    end
  end

  def show
    @user = user

    respond_to do |format|
      format.html # show.html.erb
      format.xml  { render :xml => @user }
    end
  end

  def new
    @user = User.new

    respond_to do |format|
      format.html # new.html.erb
      format.xml  { render :xml => @user }
    end
  end

  def edit
    @user = user
  end
  
  def destroy
  end

  def create
    @user = User.new(params[:user])

    respond_to do |format|
      if @user.save
        session[:id] = @user.id
        format.html { redirect_to(profile_url, :notice => 'User was successfully created.') }
        format.xml  { render :xml => @user, :status => :created, :location => @user }
      else
        format.html { render :action => "new", alert => @user.errors }
        format.xml  { render :xml => @user.errors, :status => :unprocessable_entity }
      end
    end
  end

  def update
    @user = user

    respond_to do |format|
      if @user.update_attributes(params[:user])
        format.html { redirect_to(profile_url, :notice => 'User was successfully updated.') }
        format.xml  { head :ok }
      else
        format.html { render :action => "edit" }
        format.xml  { render :xml => @user.errors, :status => :unprocessable_entity }
      end
    end
  end

  def login
    @user = User.authenticate(params)
    respond_to do |format|
      if @user.nil?
        format.html { redirect_to(signup_url, :alert => 'That login doesn\'t appear to work.  Would you like to sign up?') }
        format.xml  { render :xml => @user.errors, :status => :unprocessable_entity } 
      else
        session[:id] = @user.id
        format.html { redirect_to("/", :notice => 'Login Successful') }
      end
    end
  end

  def logout
    session.clear
    respond_to do |format|
      format.html { redirect_to("/", :notice => 'Sign Out Successful') }
    end
  end

end
