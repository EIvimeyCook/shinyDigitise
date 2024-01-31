

#' @title check_plottype
#' @description Checks whether the plottype has been selected
#' @param x list data created in sDigitise about the graph
#' @noRd
check_plottype <- function(x){
	!is.null(x$plot_type)
}

#' @title check_orientation
#' @description Checks whether a graph has been orientated
#' @param x list data created in sDigitise about the graph
#' @noRd
check_orientation <- function(x){
	TRUE
}

#' @title check_calibration
#' @description Checks whether a graph has been calibrated
#' @param x list data created in sDigitise about the graph
#' @noRd
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

#' @title check_extract
#' @description Checks whether a graph has been extracted
#' @param x list data created in sDigitise about the graph
#' @noRd
check_extract <- function(x) {
  if(is.null(x$raw_data)) {
    FALSE
  } else{

  group_lengths <- table(x$raw_data$id) 

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

