

#' @title check_plottype
#' @description Checks whether the plottype has been selected
#' @param x list data created in sDigitise about the graph
check_plottype <- function(x){
	!is.null(x$plot_type)
}

#' @title check_orientation
#' @description Checks whether a graph has been orientated
#' @param x list data created in sDigitise about the graph
check_orientation <- function(x){
	TRUE
}

#' @title check_calibration
#' @description Checks whether a graph has been calibrated
#' @param x list data created in sDigitise about the graph
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


#' @title fill_missing_n
#' @description Makes previously digitised data readable by shinyDigitise. Files
#'   created with metaDigitise() store scatterplot/histogram raw_data without a
#'   per-point sample size column (n) - n instead lives in object$knownN, or is
#'   estimated from the number of clicked points. It also stores id/col as factors.
#' @param raw_data raw_data from a saved metaDigitise/shinyDigitise object
#' @param plot_type plot type of the saved object
#' @param knownN known sample sizes saved by metaDigitise (NULL if not entered)
#' @param processed_data processed data saved in the same file (used to estimate
#'   histogram n the same way metaDigitise does: the sum of the bar frequencies)
fill_missing_n <- function(raw_data, plot_type, knownN = NULL, processed_data = NULL) {
  raw_data <- as.data.frame(raw_data, stringsAsFactors = FALSE)
  if (nrow(raw_data) == 0) return(raw_data)

  # factors (metaDigitise uses stringsAsFactors = TRUE) break later c() calls
  is_fac <- vapply(raw_data, is.factor, logical(1))
  raw_data[is_fac] <- lapply(raw_data[is_fac], as.character)

  if (!"n" %in% names(raw_data)) {
    ids <- as.character(raw_data$id)
    if (!is.null(knownN) && !is.null(names(knownN)) && all(unique(ids) %in% names(knownN))) {
      # known sample sizes entered in metaDigitise, named by group
      raw_data$n <- as.numeric(knownN[ids])
    } else if (!is.null(knownN) && length(knownN) == 1) {
      raw_data$n <- rep(as.numeric(knownN), nrow(raw_data))
    } else if (identical(plot_type, "scatterplot")) {
      # metaDigitise's default for scatterplots: n = number of points per group
      raw_data$n <- as.numeric(stats::ave(seq_along(ids), ids, FUN = length))
    } else if (identical(plot_type, "histogram") && !is.null(processed_data$frequency)) {
      # metaDigitise's default for histograms: n = total of the bar frequencies
      raw_data$n <- rep(sum(as.numeric(processed_data$frequency), na.rm = TRUE), nrow(raw_data))
    } else {
      raw_data$n <- rep(NA_real_, nrow(raw_data))
    }
  }
  raw_data
}
