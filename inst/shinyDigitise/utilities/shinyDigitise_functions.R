

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
#'   Only KNOWN sample sizes are copied into n (the group table's "sample size" is
#'   what the user typed). When none were entered, n is left blank (NA), so the
#'   export keeps using metaDigitise's estimate from the clicks - also after the
#'   points are re-clicked. (Filling n with the estimate made it look typed, so the
#'   estimate was frozen when the figure was saved again.)
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
    } else {
      raw_data$n <- rep(NA_real_, nrow(raw_data))
    }
  }
  raw_data
}

#' @title typed_known_n
#' @description For scatterplots and histograms, metaDigitise's exported sample size
#'   comes from object$knownN if it is set, otherwise from the clicks (points per
#'   group, or the total of the bar heights). This turns the sample sizes typed into
#'   shinyDigitise's group table into knownN so the export uses them. Groups left
#'   blank fall back to the clicked estimate.
#' @param raw_data raw_data being saved
#' @param plot_type plot type
#' @param processed_data processed data (bar frequencies, for histograms)
#' @return named vector of sample sizes by group, or NULL if none were typed
typed_known_n <- function(raw_data, plot_type, processed_data = NULL) {
  if (!plot_type %in% c("scatterplot", "histogram") || is.null(raw_data) ||
      nrow(as.data.frame(raw_data)) == 0 || !"n" %in% names(raw_data)) {
    return(NULL)
  }
  raw_data <- as.data.frame(raw_data)
  ids <- unique(as.character(raw_data$id))
  typed <- vapply(ids, function(g) {
    v <- suppressWarnings(as.numeric(raw_data$n[as.character(raw_data$id) == g]))
    v <- v[!is.na(v)]
    if (length(v) == 0) NA_real_ else v[1]
  }, numeric(1))
  if (all(is.na(typed))) return(NULL)

  # groups with no typed value: use the same estimate metaDigitise would
  if (any(is.na(typed))) {
    if (plot_type == "scatterplot") {
      counts <- table(as.character(raw_data$id))
      typed[is.na(typed)] <- as.numeric(counts[names(typed)[is.na(typed)]])
    } else {
      typed[is.na(typed)] <- sum(as.numeric(processed_data$frequency), na.rm = TRUE)
    }
  }
  typed
}

#' @title fill_missing_comments
#' @description Gives saved figures that have no comment field a comment of NA (what
#'   metaDigitise stores when no comment is given), so the development version of
#'   metaDigitise can export them. Only files with no comment field are re-saved;
#'   nothing else in them is changed.
#' @param cal_dir the caldat folder
fill_missing_comments <- function(cal_dir) {
  if (!dir.exists(cal_dir)) return(invisible(NULL))
  for (f in list.files(cal_dir, full.names = TRUE)) {
    obj <- tryCatch(readRDS(f), error = function(e) NULL)
    if (is.list(obj) && !is.null(obj$plot_type) && !"comment" %in% names(obj)) {
      obj["comment"] <- list(NA)
      saveRDS(obj, f)
    }
  }
  invisible(NULL)
}
