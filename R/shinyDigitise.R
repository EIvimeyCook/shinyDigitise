#' @title shinyDigitise
#' @description Single or batch processing of figures with .png, .jpg, .tiff, .pdf extensions within a set directory. metaDigitise() consolidates the data and exports the data for each image and image type. It can also summarise the data, provide the raw data (if scatterplots) and automatically imports previously finished data and merges it with newly digitised data. metaDigitise() also allows users to check their calibration along with editing previous digitisations.
#' @param dir the path name to the directory / folder where the files are located
#' @param import_all Logical - whether to import already completed files, defaults to FALSE (i.e. only import new files)
#' @param image_name Vector of specific images for shinyDigitise to plot. defaults to NULL, i.e. all or only not completed images are input. If specified, overrides import_all
#' @export

shinyDigitise <- function(dir, import_all=FALSE, image_name=NULL){

	emojifont::load.emojifont('OpenSansEmoji.ttf')


	if( (substring(dir, nchar(dir)) == "/") == FALSE){
	    dir <- paste0(dir, "/")
	  }
	  metaDigitise::setup_calibration_dir(dir)
# dir="~/Downloads/Image/"

	if(is.null(image_name)){
	  if(import_all){
	  	details <- metaDigitise::dir_details(dir)
	  }else{
			details <- metaDigitise::get_notDone_file_details(dir) 	
	  }	
	}else{
		details <- metaDigitise::dir_details(dir)

		if(!image_name %in% details$name){
			stop("image_name must refer to images within the directory")
		}
		details$paths <- details$paths[match(image_name,details$name)]
		details$name <- image_name
	}

	counter_total <- length(details$paths)
	# filename(details$paths[1])

#https://bryer.org/post/2021-02-12-shiny_apps_in_r_packages/
  shiny_env <- new.env()
  assign('dir', dir, shiny_env)
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