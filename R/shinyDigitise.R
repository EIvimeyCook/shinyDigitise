#' @title shinyDigitise
#' @description Single or batch processing of figures with .png, .jpg, .tiff, .pdf extensions within a set directory. metaDigitise() consolidates the data and exports the data for each image and image type. It can also summarise the data, provide the raw data (if scatterplots) and automatically imports previously finished data and merges it with newly digitised data. metaDigitise() also allows users to check their calibration along with editing previous digitisations.
#' @param dir the path name to the directory / folder where the files are located
#' @param import_all Logical - whether to import already completed files, defaults to FALSE (i.e. only import new files)

shinyDigitise <- function(dir, import_all=FALSE){


	if( (substring(dir, nchar(dir)) == "/") == FALSE){
	    dir <- paste0(dir, "/")
	  }
	  setup_calibration_dir(dir)

  if(import_all){
  	details <- dir_details(dir)
  }else{
 	# done_details <- dir_details(dir)
		details <- get_notDone_file_details(dir) 	
  }

	counter_total <- length(details$paths)
	# filename(details$paths[1])

#https://bryer.org/post/2021-02-12-shiny_apps_in_r_packages/
  shiny_env <- new.env()
  assign('details', details, shiny_env)
  assign('counter_total', counter_total, shiny_env)
  
  environment(shinyDigitise_UI) <- shiny_env
  environment(shinyDigitise_server) <- shiny_env
  app <- shiny::shinyApp(
      ui = shinyDigitise_UI,
      server = shinyDigitise_server
  )
  runApp(app)

	}