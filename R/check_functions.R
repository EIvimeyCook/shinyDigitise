
#check plottype if its not null
check_plottype <- function(x){
	!is.null(x$plot_type)
}

#check orinetation if its TRUE
check_orientation <- function(x){
	TRUE
}

#check calibration and whether parts are filled in. And whether enough calpoints are specified. Depending on plot type
check_calibrate <- function(x){
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

#check the extraction is complete. If it's null = FALSE. Otherwsie if the group lengths are not suitable length i.e. enough points for means/boxplots etc. Scatterplot doesnt need any.
check_extract <- function(x) {
  if(is.null(x$raw_data)) {
    FALSE
  } else{

  group_lengths <- table(x$raw_data$id)
  print(group_lengths)

    if(length(group_lengths) < 1) {
      FALSE
    } else {
      if((x$plot_type == "mean_error" & (TRUE %in% (group_lengths != 2)) & is.null(x$error_type)) |
         (x$plot_type == "xy_mean_error" & (TRUE %in% (group_lengths != 3)) & is.null(x$error_type)) |
         (x$plot_type == "boxplot" & (TRUE %in% (group_lengths != 5)))) {
        FALSE
      } else {
        TRUE
      }
    }
}
}

