#' @title shinyDigitise
#' @description Single or batch processing of figures with .png, .jpg, .tiff, .pdf extensions within a set directory. metaDigitise() consolidates the data and exports the data for each image and image type. It can also summarise the data, provide the raw data (if scatterplots) and automatically imports previously finished data and merges it with newly digitised data. metaDigitise() also allows users to check their calibration along with editing previous digitisations.
#' @param dir the path name to the directory / folder where the files are located

shinyDigitise <- function(dir){
# 	if(Sys.info()["user"]=="joelpick"){
#   dir <- "/Users/joelpick/Desktop/images"
# }else{
#   dir <- "~/Downloads/Image"
# }

# if(Sys.info()["user"]=="edwardivimey-cook"){
#   dir <- "/Volumes/GoogleDrive/My Drive/Postdoc/Other Projects/R Packages/ShinyDigitise/Image"
# }else{
#   dir <- "~/Downloads/Image"
# }


	if( (substring(dir, nchar(dir)) == "/") == FALSE){
	    dir <- paste0(dir, "/")
	  }
	  setup_calibration_dir(dir)
	#  done_details <- dir_details(dir)
	# details <- get_notDone_file_details(dir)

	details <- dir_details(dir)

	counter_total <- length(details$paths)
	# filename(details$paths[1])


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


	# n <- 20
	# qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
	# col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))

	# col=sample(col_vector, n)
	
	# shiny::shinyApp(ui = shinyDigitise_UI, server = shinyDigitise_server,details=details,counter_total=counter_total)
  # shiny::shinyApp(ui = shinyAppUI, server = shinyAppServer)


	}