class Admin::FirearmsController < AdminController
  # GET /firearms
  # GET /firearms.xml
  def index
    @firearms = Firearm.all

    respond_to do |format|
      format.html # index.html.erb
      format.xml  { render :xml => @firearms }
    end
  end

  # GET /firearms/1
  # GET /firearms/1.xml
  def show
    @firearm = Firearm.find(params[:id])

    respond_to do |format|
      format.html # show.html.erb
      format.xml  { render :xml => @firearm }
    end
  end

  # GET /firearms/new
  # GET /firearms/new.xml
  def new
    @firearm = Firearm.new

    respond_to do |format|
      format.html # new.html.erb
      format.xml  { render :xml => @firearm }
    end
  end

  # GET /firearms/1/edit
  def edit
    @firearm = Firearm.find(params[:id])
  end

  # POST /firearms
  # POST /firearms.xml
  def create
    @firearm = Firearm.new(params[:firearm])

    respond_to do |format|
      if @firearm.save
        format.html { redirect_to(@firearm, :notice => 'Firearm was successfully created.') }
        format.xml  { render :xml => @firearm, :status => :created, :location => @firearm }
      else
        format.html { render :action => "new" }
        format.xml  { render :xml => @firearm.errors, :status => :unprocessable_entity }
      end
    end
  end

  # PUT /firearms/1
  # PUT /firearms/1.xml
  def update
    @firearm = Firearm.find(params[:id])

    respond_to do |format|
      if @firearm.update_attributes(params[:firearm])
        format.html { redirect_to(@firearm, :notice => 'Firearm was successfully updated.') }
        format.xml  { head :ok }
      else
        format.html { render :action => "edit" }
        format.xml  { render :xml => @firearm.errors, :status => :unprocessable_entity }
      end
    end
  end

  # DELETE /firearms/1
  # DELETE /firearms/1.xml
  def destroy
    @firearm = Firearm.find(params[:id])
    @firearm.destroy

    respond_to do |format|
      format.html { redirect_to(firearms_url) }
      format.xml  { head :ok }
    end
  end
end
