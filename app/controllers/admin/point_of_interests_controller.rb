class Admin::PointOfInterestsController < AdminController

  def city
    City.find_by_name(params[:city_id])
  end

  def district
    District.find(params[:district_id])
  end

  # GET /point_of_interests
  # GET /point_of_interests.xml
  def index
    @city = city
    @district = district
    @point_of_interests = district.point_of_interests

    respond_to do |format|
      format.html # index.html.erb
      format.xml  { render :xml => @point_of_interests }
    end
  end

  # GET /point_of_interests/1
  # GET /point_of_interests/1.xml
  def show
    @city = city
    @district = district
    @point_of_interest = PointOfInterest.find(params[:id])

    respond_to do |format|
      format.html # show.html.erb
      format.xml  { render :xml => @point_of_interest }
    end
  end

  # GET /point_of_interests/new
  # GET /point_of_interests/new.xml
  def new
    @district = district
    @point_of_interest = @district.point_of_interests.build

    respond_to do |format|
      format.html # new.html.erb
      format.xml  { render :xml => @point_of_interest }
    end
  end

  # GET /point_of_interests/1/edit
  def edit
    @city = city
    @district = district
    @point_of_interest = PointOfInterest.find(params[:id])
  end

  # POST /point_of_interests
  # POST /point_of_interests.xml
  def create
    @district = district
    @point_of_interest = @district.point_of_interests.build(params[:point_of_interest])

    respond_to do |format|
      if @point_of_interest.save
        format.html { redirect_to(admin_city_district_point_of_interest_url(city, @district, @point_of_interest), :notice => 'Point of interest was successfully created.') }
        #format.xml  { render :xml => @point_of_interest, :status => :created, :location => @point_of_interest }
      else
        format.html { render :action => "new" }
        #format.xml  { render :xml => @point_of_interest.errors, :status => :unprocessable_entity }
      end
    end
  end

  # PUT /point_of_interests/1
  # PUT /point_of_interests/1.xml
  def update
    @point_of_interest = PointOfInterest.find(params[:id])

    respond_to do |format|
      if @point_of_interest.update_attributes(params[:point_of_interest])
        format.html { redirect_to(admin_city_district_point_of_interest_url(city, district, @point_of_interest), :notice => 'Point of interest was successfully updated.') }
        format.xml  { head :ok }
      else
        format.html { render :action => "edit" }
        format.xml  { render :xml => @point_of_interest.errors, :status => :unprocessable_entity }
      end
    end
  end

  # DELETE /point_of_interests/1
  # DELETE /point_of_interests/1.xml
  def destroy
    @point_of_interest = PointOfInterest.find(params[:id])
    @point_of_interest.destroy

    respond_to do |format|
      format.html { redirect_to(admin_city_district_point_of_interests_url) }
      format.xml  { head :ok }
    end
  end
end
