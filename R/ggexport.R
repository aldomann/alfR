#' Simpler ggsave with useful defaults
#'
#' Alternative implementation of ggsave (from ggplot2 package) with nice defaults. Only GNU/Linux with libnotify macOS are supported right now.
#'
#' The function does the following things:
#'
#' \itemize{
#'   \item execute any R call
#'   \item print the elapsed time to execute it
#'   \item send a permanent desktop notification
#'   \item emit a completion alert sound
#' }
#' @param filename Destination path of the exported plot
#' @param size Selected plot size, to choose between "medium" and "small" or as a vector
#' @param dpi DPI used when exporting the plot
#' @param font Optional selected font, to choose between "lmodern" and "palatino"
#' @param extension Extension of the exported plot, either "pdf" or "png"
#' @keywords system.time
#' @export
#' @examples
#'
#' @import ggplot2
#' @import extrafont

ggexport <- function(filename, size = "medium", dpi = 96, font = NULL, extension = "pdf") {
	# Load fonts (mac OS)
	if (Sys.info()['sysname'] == "Darwin") {
		extrafont::loadfonts(quiet = T)
	}

	# Select extension/device
	if (extension == "pdf") {
		if (Sys.info()['sysname'] == "Linux") {
			gg.device <- cairo_pdf
		} else if (Sys.info()['sysname'] == "Darwin") {
			gg.device <- "pdf"
		}
	} else if (extension == "png") {
		gg.device <- "png"
	}

	# Select export size
	if (is.numeric(size)) {
		width <- size[1]
		height <- size[2]
	}
	else if (size == "medium") {
		width <- 6
		height <- 3.5
	}
	else if (size == "small") {
		width <- 4
		height <- 2.25
	}

	# Select font
	if (is.null(font)) {
		font.family <- NULL
	} else if (font == "lmodern") {
		font.family <- "LM Roman 10"
	} else if (font == "palatino") {
		font.family <- "Palatino"
	}

	# Export
	gg.list <- list(
		ggplot2::theme_bw(),
		ggplot2::theme(text = element_text(family = font.family)),
		ggplot2::ggsave(filename = filename, width = width, height = height, dpi = dpi, device = gg.device)
	)

	return(gg.list)
}
