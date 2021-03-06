check_plottype <- function(x){
	!is.null(x$plot_type) #& !is.null(x$error_type)
}
check_orientation <- function(x){
	TRUE
}
check_calibrate <- function(x){
  # check if they all calibrate parts are filled in 
  if( is.null(x$calpoints) || is.null(x$variable) || is.null(x$point_vals)|| is.null(x$log_axes) ){
	  FALSE
	}else if(x$plot_type %in% c("mean_error","boxplot")){
		nrow(x$calpoints)==2 & length(x$point_vals)==2 & length(x$variable)==1
	}else if(x$plot_type %in% c("scatterplot","xy_mean_error")){
	nrow(x$calpoints)==4 & length(x$point_vals)==4 & length(x$variable)==2
	}else{ 
		TRUE
	}
}
check_extract <- function(x){
	if(is.null(x$raw_data)){
		FALSE
	}else{
		group_lengths<-unique(table(x$raw_data$id))
		if(is.null(x$error_type) & x$plot_type %in% c("mean_error","xy_mean_error")){
			FALSE
		}else if(length(group_lengths)>1){
			FALSE
		}else if(x$plot_type %in% c("mean_error","xy_mean_error","boxplot")){
			(x$plot_type=="mean_error" & group_lengths==2)|
			(x$plot_type=="xy_mean_error" & group_lengths==3)|
			(x$plot_type=="boxplot" & group_lengths==5)
		}else{
			TRUE
		}
	}
}
#work out what to do with error type
	#& !is.null(x$error_type)
	

