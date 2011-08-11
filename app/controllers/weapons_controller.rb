class WeaponsController < ApplicationController
  # GET /weapons
  # GET /weapons.xml
  def index
    @weapons = Weapon.all

    respond_to do |format|
      format.html # index.html.erb
      format.xml  { render :xml => @weapons }
    end
  end

  # GET /weapons/1
  # GET /weapons/1.xml
  def show
    @weapon = Weapon.find(params[:id])

    respond_to do |format|
      format.html # show.html.erb
      format.xml  { render :xml => @weapon }
    end
  end

  # GET /weapons/new
  # GET /weapons/new.xml
  def new
    @weapon = Weapon.new

    respond_to do |format|
      format.html # new.html.erb
      format.xml  { render :xml => @weapon }
    end
  end

  # GET /weapons/1/edit
  def edit
    @weapon = Weapon.find(params[:id])
  end

  # POST /weapons
  # POST /weapons.xml
  def create
    @weapon = Weapon.new(params[:weapon])

    respond_to do |format|
      if @weapon.save
        format.html { redirect_to(@weapon, :notice => 'Weapon was successfully created.') }
        format.xml  { render :xml => @weapon, :status => :created, :location => @weapon }
      else
        format.html { render :action => "new" }
        format.xml  { render :xml => @weapon.errors, :status => :unprocessable_entity }
      end
    end
  end

  # PUT /weapons/1
  # PUT /weapons/1.xml
  def update
    @weapon = Weapon.find(params[:id])

    respond_to do |format|
      if @weapon.update_attributes(params[:weapon])
        format.html { redirect_to(@weapon, :notice => 'Weapon was successfully updated.') }
        format.xml  { head :ok }
      else
        format.html { render :action => "edit" }
        format.xml  { render :xml => @weapon.errors, :status => :unprocessable_entity }
      end
    end
  end

  # DELETE /weapons/1
  # DELETE /weapons/1.xml
  def destroy
    @weapon = Weapon.find(params[:id])
    @weapon.destroy

    respond_to do |format|
      format.html { redirect_to(weapons_url) }
      format.xml  { head :ok }
    end
  end
end
