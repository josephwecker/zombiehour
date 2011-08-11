class Admin::ToolKitsController < AdminController
  # GET /tool_kits
  # GET /tool_kits.xml
  def index
    @tool_kits = ToolKit.all

    respond_to do |format|
      format.html # index.html.erb
      format.xml  { render :xml => @tool_kits }
    end
  end

  # GET /tool_kits/1
  # GET /tool_kits/1.xml
  def show
    @tool_kit = ToolKit.find(params[:id])

    respond_to do |format|
      format.html # show.html.erb
      format.xml  { render :xml => @tool_kit }
    end
  end

  # GET /tool_kits/new
  # GET /tool_kits/new.xml
  def new
    @tool_kit = ToolKit.new

    respond_to do |format|
      format.html # new.html.erb
      format.xml  { render :xml => @tool_kit }
    end
  end

  # GET /tool_kits/1/edit
  def edit
    @tool_kit = ToolKit.find(params[:id])
  end

  # POST /tool_kits
  # POST /tool_kits.xml
  def create
    @tool_kit = ToolKit.new(params[:tool_kit])

    respond_to do |format|
      if @tool_kit.save
        format.html { redirect_to(@tool_kit, :notice => 'Tool kit was successfully created.') }
        format.xml  { render :xml => @tool_kit, :status => :created, :location => @tool_kit }
      else
        format.html { render :action => "new" }
        format.xml  { render :xml => @tool_kit.errors, :status => :unprocessable_entity }
      end
    end
  end

  # PUT /tool_kits/1
  # PUT /tool_kits/1.xml
  def update
    @tool_kit = ToolKit.find(params[:id])

    respond_to do |format|
      if @tool_kit.update_attributes(params[:tool_kit])
        format.html { redirect_to(@tool_kit, :notice => 'Tool kit was successfully updated.') }
        format.xml  { head :ok }
      else
        format.html { render :action => "edit" }
        format.xml  { render :xml => @tool_kit.errors, :status => :unprocessable_entity }
      end
    end
  end

  # DELETE /tool_kits/1
  # DELETE /tool_kits/1.xml
  def destroy
    @tool_kit = ToolKit.find(params[:id])
    @tool_kit.destroy

    respond_to do |format|
      format.html { redirect_to(tool_kits_url) }
      format.xml  { head :ok }
    end
  end
end
