class Admin::DistrictsController < AdminController

  def city
    City.find_by_name(params[:city_id])
  end

  # GET /districts
  # GET /districts.xml
  def index
    @city = city
    @districts = city.districts

    respond_to do |format|
      format.html # index.html.erb
      format.xml  { render :xml => @districts }
    end
  end

  # GET /districts/1
  # GET /districts/1.xml
  def show
    @city = city
    @district = District.find(params[:id])

    respond_to do |format|
      format.html # show.html.erb
      #format.xml  { render :xml => @district }
    end
  end

  # GET /districts/new
  # GET /districts/new.xml
  def new
    @city = city
    @district = @city.districts.build

    respond_to do |format|
      format.html # new.html.erb
      #format.xml  { render :xml => @district }
    end
  end

  # GET /districts/1/edit
  def edit
    @city = city
    @district = District.find(params[:id])
  end

  # POST /districts
  # POST /districts.xml
  def create
    @district = city.districts.build(params[:district])

    respond_to do |format|
      if @district.save
        format.html { redirect_to(admin_city_district_url(city, @district), :notice => 'District was successfully created.') }
        #format.xml  { render :xml => @district, :status => :created, :location => @district }
      else
        format.html { render :action => "new" }
        #format.xml  { render :xml => @district.errors, :status => :unprocessable_entity }
      end
    end
  end

  # PUT /districts/1
  # PUT /districts/1.xml
  def update
    @district = District.find(params[:id])

    respond_to do |format|
      if @district.update_attributes(params[:district])
        format.html { redirect_to(admin_city_district_url(city, @district), :notice => 'District was successfully updated.') }
        format.xml  { head :ok }
      else
        format.html { render :action => "edit" }
        #format.xml  { render :xml => @district.errors, :status => :unprocessable_entity }
      end
    end
  end

  # DELETE /districts/1
  # DELETE /districts/1.xml
  def destroy
    @district = District.find(params[:id])
    @district.destroy

    respond_to do |format|
      format.html { redirect_to(admin_city_districts_url) }
      format.xml  { head :ok }
    end
  end
end
