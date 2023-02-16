
#various functions

is.even <- function(x) x %% 2 == 0

filename <- function(x) {
	y <- strsplit(x,"/")
	sapply(y, function(z) z[length(z)], USE.NAMES = FALSE)
}


redraw_rotation <- function(image, flip, rotate){
	if(flip) image <- magick::image_flop(magick::image_rotate(image,270))
	image <- magick::image_rotate(image, rotate)
	return(image)
}


redraw_calibration <- function(plot_type, variable, calpoints,point_vals,image_details,cex){
	x_shift <- image_details["width"]/30
	y_shift <- image_details["height"]/30
	text_cex <- 2*cex
	line_width <- 2*cex
	cal_col <- "blue"

	graphics::points(calpoints, pch=3, col=cal_col, lwd=line_width)
	graphics::lines(calpoints[1:2,], col=cal_col, lwd=line_width)
	graphics::text(calpoints$x[1:2] - rep(x_shift, 2), calpoints$y[1:2], point_vals[1:2], col=cal_col, cex=text_cex)

	if(plot_type=="histogram"){
		graphics::text(mean(calpoints$x[3:4]), mean(calpoints$y[3:4]) - y_shift*1.5, variable[1], col=cal_col, cex=text_cex)
	}else{	
		graphics::text(mean(calpoints$x[1:2]) - x_shift*1.5, mean(calpoints$y[1:2]), variable[1], col=cal_col, cex=text_cex, srt=90)
		}

	if(!plot_type %in% c("mean_error","boxplot")){
		graphics::lines(calpoints[3:4,], col=cal_col, lwd=line_width)
		graphics::text(calpoints$x[3:4], calpoints$y[3:4] - rep(y_shift, 2), point_vals[3:4], col=cal_col, cex=text_cex)
		graphics::text(mean(calpoints$x[3:4]), mean(calpoints$y[3:4]) - y_shift*1.5, variable[2], col=cal_col, cex=text_cex)
	}
}


redraw_points <- function(plot_type, raw_data, image_details, cex, pos){
	image_width <- image_details["width"]
	image_height <- image_details["height"]
	legend_pos <- image_height/40

	text_cex <- 1*cex
	line_width <- 2*cex
	point_cex <- 1*cex
	point_col="red"
	pch = 20


	## legend
	if(plot_type == "mean_error"){
		graphics::points((image_width/4)*c(1,3,3), rep(-legend_pos,3), pch=c(19,19,20), col=c(point_col,point_col,"yellow"), cex=point_cex, xpd=TRUE)
		graphics::text((image_width/4)*c(1,3), rep(-legend_pos*2,2), c("mean","error"), cex=text_cex, xpd=TRUE)
	}

	if(plot_type == "xy_mean_error"){
		graphics::points((image_width/4)*c(1,2,2,3,3), rep(-legend_pos,5), 
			pch=c(19,19,20,19,20), 
			col=c(point_col,point_col,"yellow",point_col,"purple"), 
			cex=point_cex, 
			xpd=TRUE)
		graphics::text((image_width/4)*c(1,2,3), rep(-legend_pos*2,3), c("mean","y error","x error"), cex=text_cex, xpd=TRUE)
	}

	if(plot_type %in% c("mean_error","xy_mean_error","boxplot") & nrow(raw_data)>0){
		for(i in unique(raw_data$id)){
			group_data <- subset(raw_data,raw_data$id==i)
			graphics::points(y~x,group_data, pch=19, col=point_col, cex=point_cex)
			graphics::lines(y~x, group_data, lwd=line_width, col=point_col)
				
			if(pos=="right"){
				text_x <- max(group_data$x)+image_width/30
				text_y <- mean(group_data$y)
			}else if(pos=="top"){
				text_x <- max(group_data$x)
				text_y <- max(group_data$y) + image_height/30
			}
					
			graphics::text(text_x,text_y,paste0(group_data$id[1]," (",group_data$n[1],")"),srt=90, cex=text_cex, col=point_col, adj=if(pos=="right"){0.5}else{0})
			if(plot_type %in% c("xy_mean_error","mean_error", "scatterplot")){
				graphics::points(group_data$x[1],group_data$y[1], pch=20, col="yellow", cex=point_cex)
			}
			if(plot_type %in% c("xy_mean_error")){
				graphics::points(group_data$x[3],group_data$y[3], pch=20, col="purple", cex=point_cex)
			}
		}
	}

	if(plot_type=="scatterplot"& nrow(raw_data)>0){
		graphics::points(y~x,raw_data, pch=raw_data$pch, col=as.character(raw_data$col), cex=point_cex)

	#legend
		## changed in shinyD 
		#legend_dat <- stats::aggregate(x~id+col+pch + group,raw_data, length)

		legend_dat <- stats::aggregate(x~id+col+pch,raw_data, length)
		nGroups <- nrow(legend_dat)
		legend_x <- (image_width/nGroups)/2 + (image_width/nGroups)*((1:nGroups)-1)

		 graphics::points(legend_x, rep(-legend_pos*2.5,nGroups), 
		 	col=as.character(legend_dat$col), pch=legend_dat$pch, xpd=TRUE, cex=point_cex)
		 graphics::text(legend_x, rep(-legend_pos,nGroups), legend_dat$id, col=as.character(legend_dat$col), cex=text_cex, xpd=TRUE)
		 graphics::text(legend_x, rep(-legend_pos*5,nGroups), paste("n =",legend_dat$x), col=as.character(legend_dat$col), cex=text_cex, xpd=TRUE)
	}

	if(plot_type=="histogram"& nrow(raw_data)>0){
		bar_cols <- c("red","orange")
		for(i in unique(raw_data$bar)){
			bar_data <- subset(raw_data, raw_data$bar==i)
			bar_col <- bar_cols[is.even(i)+1]
			graphics::points(y~x,bar_data, pch=19, col=bar_col, cex=point_cex)
			graphics::lines(y~x, bar_data, lwd=line_width, col=bar_col)
			graphics::text(mean(bar_data$x),mean(bar_data$y)+legend_pos,bar_data$bar[1], col=bar_col, cex=text_cex)
		}
	}
}


#' @title internal_redraw
#' @description Redraws figure and extraction data
#' @param image_file Image filename
#' @param flip whether to flip figure
#' @param rotate how much to rotate figure
#' @param plot_type plot_type
#' @param variable variable
#' @param cex relative size of points and text
#' @param calpoints The calibration points
#' @param point_vals The point values
#' @param raw_data The raw data
#' @param rotation logical, should figure be rotated
#' @param rotate_mode logical, is plotting in rotation mode
#' @param calibration logical, should calibration be redrawn
#' @param pos where group names should be plotted (either "right" or "top")
#' @param points logical, should points be redrawn
#' @param shiny logical, is plotting occuring in shiny app?
#' @param zoom_coords x and y coords for zoom function
#' @param ... further arguments passed to or from other methods.
#' @export

internal_redraw <- function(image_file, flip=FALSE, rotate=0, plot_type=NULL, variable=NULL, cex=NULL, calpoints=NULL, point_vals=NULL, raw_data=NULL, rotation=TRUE, calibration=TRUE, points=TRUE, rotate_mode=FALSE, pos=NULL, shiny=FALSE, zoom_coords=NULL, ...){

	if(!shiny){
		op <- graphics::par(mar=c(3,0,2,0), mfrow=c(1,1))
		on.exit(graphics::par(op))
	}

	image <- magick::image_read(image_file)
	if(rotation) image <- redraw_rotation(image=image, flip=flip, rotate=rotate)

	image_details <- c(width = magick::image_info(image)["width"][[1]], height = magick::image_info(image)["height"][[1]])

	text_cex <- 1*cex

	if(is.null(zoom_coords)){
		xlim <- c(0,image_details[1])	
		ylim <- c(0,image_details[2])
	}else{
		xlim <- zoom_coords[c(1,2)]
		ylim <- zoom_coords[c(3,4)]
	}
	plot(NA, xlim=xlim, ylim=ylim)
	if(!is.null(image)) plot(image,add=TRUE)
	graphics::plot(image, xlim=xlim, ylim=ylim)
	if(!shiny){
		graphics::mtext(filename(image_file),3, 1, cex=text_cex)
	}
	if(!is.null(plot_type)) graphics::mtext(plot_type,3, 0, cex=text_cex)

	if(is.null(calpoints)) calibration=FALSE
	if(calibration) redraw_calibration(plot_type=plot_type, variable=variable, calpoints=calpoints,point_vals=point_vals,image_details=image_details, cex=cex)
 
	if(is.null(raw_data)) points=FALSE
	if(points) redraw_points(plot_type=plot_type,raw_data=raw_data,image_details=image_details,  cex=cex, pos=pos)

	if(rotate_mode) {
		graphics::abline(
			v=seq(0,image_details["width"], length.out=20),
			h=seq(0,image_details["height"], length.out=20),
			col=scales::alpha(1,0.5)
			)
		}

}
