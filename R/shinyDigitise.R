#' @title shinyDigitise
#' @description Single or batch processing of figures with .png, .jpg, .tiff, .pdf extensions within a set directory. metaDigitise() consolidates the data and exports the data for each image and image type. It can also summarise the data, provide the raw data (if scatterplots) and automatically imports previously finished data and merges it with newly digitised data. metaDigitise() also allows users to check their calibration along with editing previous digitisations.
#@param dir the path name to the directory / folder where the files are located
#@param import_all Logical - whether to import already completed files, defaults to FALSE (i.e. only import new files)
#@param image_name Vector of specific images for shinyDigitise to plot. defaults to NULL, i.e. all or only not completed images are input. If specified, overrides import_all
#' @export

shinyDigitise <- function(){
  shiny_env <- new.env()
  environment(shinyDigitise_UI) <- shiny_env
  environment(shinyDigitise_server) <- shiny_env
  app <- shiny::shinyApp(
      ui = shinyDigitise_UI,
      server = shinyDigitise_server
  )
  shiny::runApp(app)

	}