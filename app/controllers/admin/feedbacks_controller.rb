class Admin::FeedbacksController < AdminController
  # GET /feedbacks
  # GET /feedbacks.xml
  def index
    @feedbacks = Feedback.all

    respond_to do |format|
      format.html # index.html.erb
      format.xml  { render :xml => @feedbacks }
    end
  end
  
  def destroy
    @feedback = Feedback.find(params[:id])
    @feedback.destroy

    respond_to do |format|
      format.html { redirect_to('/admin/feedback') }
      format.xml  { head :ok }
    end
  end

end
