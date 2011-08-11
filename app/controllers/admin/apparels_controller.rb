class Admin::ApparelsController < AdminController
  # GET /apparels
  # GET /apparels.xml
  def index
    @apparels = Apparel.all

    respond_to do |format|
      format.html # index.html.erb
      format.xml  { render :xml => @apparels }
    end
  end

  # GET /apparels/1
  # GET /apparels/1.xml
  def show
    @apparel = Apparel.find(params[:id])

    respond_to do |format|
      format.html # show.html.erb
      format.xml  { render :xml => @apparel }
    end
  end

  # GET /apparels/new
  # GET /apparels/new.xml
  def new
    @apparel = Apparel.new

    respond_to do |format|
      format.html # new.html.erb
      format.xml  { render :xml => @apparel }
    end
  end

  # GET /apparels/1/edit
  def edit
    @apparel = Apparel.find(params[:id])
  end

  # POST /apparels
  # POST /apparels.xml
  def create
    @apparel = Apparel.new(params[:apparel])

    respond_to do |format|
      if @apparel.save
        format.html { redirect_to(@apparel, :notice => 'Apparel was successfully created.') }
        format.xml  { render :xml => @apparel, :status => :created, :location => @apparel }
      else
        format.html { render :action => "new" }
        format.xml  { render :xml => @apparel.errors, :status => :unprocessable_entity }
      end
    end
  end

  # PUT /apparels/1
  # PUT /apparels/1.xml
  def update
    @apparel = Apparel.find(params[:id])

    respond_to do |format|
      if @apparel.update_attributes(params[:apparel])
        format.html { redirect_to(@apparel, :notice => 'Apparel was successfully updated.') }
        format.xml  { head :ok }
      else
        format.html { render :action => "edit" }
        format.xml  { render :xml => @apparel.errors, :status => :unprocessable_entity }
      end
    end
  end

  # DELETE /apparels/1
  # DELETE /apparels/1.xml
  def destroy
    @apparel = Apparel.find(params[:id])
    @apparel.destroy

    respond_to do |format|
      format.html { redirect_to(apparels_url) }
      format.xml  { head :ok }
    end
  end
end
