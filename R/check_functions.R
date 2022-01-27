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
	!is.null(x$raw_data) #& !is.null(x$error_type)
## check how many points per group
	# table(x$raw_data)
}
