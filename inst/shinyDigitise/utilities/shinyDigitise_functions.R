

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

#' @title calibrate_problem
#' @description Describes what is missing or wrong in the calibration of a graph
#' @param x list data created in sDigitise about the graph
#' @return a message describing the problem, or NULL if the calibration is complete
calibrate_problem <- function(x){
  if( is.null(x$plot_type) ) return("No plot type has been selected.")
  if( is.null(x$calpoints) || is.null(x$variable) || is.null(x$point_vals) || is.null(x$log_axes) ){
    return("The axes have not been calibrated.")
  }
  n_cal <- if(x$plot_type %in% c("mean_error","boxplot")) 2 else 4
  n_var <- if(x$plot_type %in% c("scatterplot","xy_mean_error")) 2 else 1
  if( nrow(as.data.frame(x$calpoints)) != n_cal ){
    return(paste("Please double click", n_cal, "known values on the axes."))
  }
  if( length(x$variable) != n_var ){
    return("Please enter the variable name(s).")
  }
  # empty or non-numeric boxes come through from numericInput as NULL or NA
  vals <- suppressWarnings(as.numeric(x$point_vals))
  if( length(vals) != n_cal || any(!is.finite(vals)) ){
    return("Please enter a number for each of the axis values.")
  }
  if( vals[1] == vals[2] || (n_cal == 4 && vals[3] == vals[4]) ){
    return("The two values entered for an axis must be different.")
  }
  if( !all(is.finite(unlist(x$calpoints))) ) return("The axes have not been calibrated.")
  # stretched log axes are entered on the original scale, so need positive values
  axes <- if("axes" %in% names(x$log_axes)) x$log_axes[["axes"]] else "n"
  if( axes != "n" ){
    if( !all(c("transformed","base") %in% names(x$log_axes)) ){
      return("Please say whether the log axis is transformed or stretched, and its base.")
    }
    base <- x$log_axes[["base"]]
    if( base != "e" && (is.na(suppressWarnings(as.numeric(base))) || as.numeric(base) <= 0 || as.numeric(base) == 1) ){
      return("The log base must be 'e' or a positive number other than 1.")
    }
    if( x$log_axes[["transformed"]] == "s" ){
      logged <- c(if(grepl("y", axes)) 1:2, if(grepl("x", axes)) 3:4)
      if( any(vals[logged] <= 0) ) return("Values on a stretched log axis must be above zero.")
    }
  }
  NULL
}

#' @title check_calibration
#' @description Checks whether a graph has been calibrated
#' @param x list data created in sDigitise about the graph
check_calibrate <- function(x){
  is.null(calibrate_problem(x))
}

#' @title extract_problem
#' @description Describes what is missing or wrong in the extracted data of a graph
#' @param x list data created in sDigitise about the graph
#' @return a message describing the problem, or NULL if the extraction is complete
extract_problem <- function(x) {
  if(is.null(x$plot_type)) return("No plot type has been selected.")
  raw_data <- if(is.null(x$raw_data)) data.frame() else as.data.frame(x$raw_data)
  if(nrow(raw_data) == 0) return("No points have been extracted.")

  group_lengths <- table(as.character(raw_data$id))
  if(length(group_lengths) < 1) return("No points have been extracted.")

  if(x$plot_type %in% c("mean_error","xy_mean_error")) {
    n_clicks <- if(x$plot_type == "mean_error") 2 else 3
    if(any(group_lengths != n_clicks)) {
      return(paste0("Each group needs exactly ", n_clicks, " points (",
        if(n_clicks == 2) "error bar then mean" else "y error bar, mean, then x error bar",
        "). Please re-click: ", paste(names(group_lengths)[group_lengths != n_clicks], collapse = ", ")))
    }
    if(is.null(x$error_type) || length(x$error_type) != 1 || !x$error_type %in% c("se","CI95","sd")) {
      return("Please select the type of error (SE, 95%CI or SD).")
    }
  }
  if(x$plot_type == "boxplot" && any(group_lengths != 5)) {
    return(paste0("Each group needs exactly 5 points (max, upper quartile, median, lower quartile, min). Please re-click: ",
      paste(names(group_lengths)[group_lengths != 5], collapse = ", ")))
  }
  if(x$plot_type == "histogram") {
    bar_lengths <- table(raw_data$bar)
    if(nrow(raw_data) %% 2 != 0 || any(bar_lengths != 2)) {
      return("Each bar needs two points (its left and right upper corners). Please re-click the histogram.")
    }
  }
  NULL
}

#' @title check_extract
#' @description Checks whether a graph has been extracted
#' @param x list data created in sDigitise about the graph
check_extract <- function(x) {
  is.null(extract_problem(x))
}

#' @title check_icon
#' @description Tick or warning icon for the step headers. HTML entities are used so
#'   the icons show whatever the locale/encoding of the R session.
#' @param ok logical, has the step been completed
check_icon <- function(ok){
  shiny::HTML(if(isTRUE(ok)) "&#9989;" else "&#9888;&#65039;")
}

#' @title make_log_axes
#' @description Builds the log axis information in the same format as metaDigitise
#'   (c(axes=, transformed=, base=)). Only boxplots and scatterplots can have log
#'   axes in metaDigitise.
#' @param plot_type plot type
#' @param axes which axes are logged ("n","x","y","xy")
#' @param transformed "t" (transformed) or "s" (stretched)
#' @param base log base, "e" or a number
make_log_axes <- function(plot_type, axes, transformed, base){
  if(is.null(plot_type) || !plot_type %in% c("boxplot","scatterplot") ||
     is.null(axes) || axes == "n") {
    return(c(axes = "n"))
  }
  # boxplots only have a y axis to calibrate
  if(plot_type == "boxplot") axes <- "y"
  base <- trimws(as.character(base))
  if(tolower(base) %in% c("e","ln")) base <- "e"
  c(axes = axes, transformed = transformed, base = base)
}

#' @title add_metaDigitise_fields
#' @description Adds the fields metaDigitise needs to plot and edit a saved figure:
#'   entered_N (whether all sample sizes are known for mean/error and boxplots) and,
#'   for scatterplots, a numeric group column in raw_data.
#' @param x list data created in sDigitise about the graph
add_metaDigitise_fields <- function(x){
  raw_data <- as.data.frame(x$raw_data, stringsAsFactors = FALSE)
  if(x$plot_type %in% c("mean_error","xy_mean_error","boxplot")){
    x$entered_N <- "n" %in% names(raw_data) && !any(is.na(suppressWarnings(as.numeric(raw_data$n))))
  }else{
    x$entered_N <- TRUE
  }
  if(x$plot_type == "scatterplot" && nrow(raw_data) > 0){
    ids <- as.character(raw_data$id)
    raw_data$group <- match(ids, unique(ids))
    x$raw_data <- raw_data
  }
  x
}

#' @title md_supports_plot_type
#' @description Checks whether the installed metaDigitise can summarise a plot type
#'   (metaDigitise 1.0.2 on CRAN cannot summarise xy_mean_error plots)
#' @param plot_type plot type
md_supports_plot_type <- function(plot_type){
  sm <- tryCatch(utils::getS3method("summary", "metaDigitise"), error = function(e) NULL)
  !is.null(sm) && any(grepl(paste0('"', plot_type, '"'), deparse(body(sm)), fixed = TRUE))
}

#' @title safe_extract
#' @description Exports the digitised data with metaDigitise::getExtracted(). If that
#'   fails (e.g. one file cannot be summarised), each file is summarised on its own
#'   and the files that fail are skipped with a warning, so the rest is not lost.
#' @param dir the folder of images
#' @return data frame of summary data, or NULL if nothing has been digitised
safe_extract <- function(dir){
  details <- tryCatch(metaDigitise::dir_details(dir), error = function(e) NULL)
  if(is.null(details) || length(details$doneCalFiles) == 0){
    message("shinyDigitise: no figures have been digitised, so no data were exported.")
    return(NULL)
  }
  out <- tryCatch(metaDigitise::getExtracted(dir), error = function(e) NULL)
  if(!is.null(out)) return(out)

  skipped <- character(0)
  out <- lapply(seq_along(details$doneCalFiles), function(i){
    tryCatch(summary(readRDS(details$doneCalFiles[i])), error = function(e){
      skipped <<- c(skipped, paste0(details$calibrations[i], " (", conditionMessage(e), ")"))
      NULL
    })
  })
  out <- out[!vapply(out, is.null, logical(1))]
  if(length(skipped) > 0){
    warning("shinyDigitise: these figures could not be exported and were skipped: ",
      paste(skipped, collapse = "; "), call. = FALSE)
  }
  if(length(out) == 0) return(NULL)
  out <- do.call(rbind, out)
  rownames(out) <- 1:nrow(out)
  out
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
      # match() rather than knownN[ids]: metaDigitise names a single unnamed group "" and knownN[""] is NA
      raw_data$n <- as.numeric(knownN[match(ids, names(knownN))])
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
