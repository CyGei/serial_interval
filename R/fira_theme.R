
# COLOURs -----------------------------------------------------------------
firaPalette <- function(n = 5) {
  if (n == 4) return(firaCols[c(1, 3, 4, 5)])
  grDevices::colorRampPalette(firaCols, space = "Lab")(n)
}

firaCols <- c("#3030a3", "#dd7373", "#499293", "#30602d", "#e2bd36")

scale_fill_fira <- function(..., continuous = FALSE) {
  if (continuous) {
    pal <- grDevices::colorRampPalette(c(firaCols[1], firaCols[5]),
                                       space = "Lab")
    return(ggplot2::scale_fill_gradientn(..., colours = pal(256)))
  }
  ggplot2::discrete_scale("fill", paste0("fira"), firaPalette, ...)
}



scale_colour_fira <- function(..., continuous = FALSE) {
  if (continuous) {
    pal <- grDevices::colorRampPalette(c(firaCols[1], firaCols[5]),
                                       space = "Lab")
    return(ggplot2::scale_colour_gradientn(..., colours = pal(256)))
  }
  ggplot2::discrete_scale("colour", paste0("fira"), firaPalette, ...)
}


scale_color_fira <- scale_colour_fira



# THEME -------------------------------------------------------------------

theme_fira <- function(family = "Fira Sans") {
  ggplot2::`%+replace%`(
    ggplot2::theme_grey(base_size = 11.5, base_family = family),
    ggplot2::theme(
      # add padding to the plot
      plot.margin = grid::unit(rep(0.5, 4), "cm"),
      
      # remove the plot background and border
      plot.background = ggplot2::element_blank(),
      panel.background = ggplot2::element_blank(),
      panel.border = ggplot2::element_blank(),
      
      # make the legend and strip background transparent
      legend.background = ggplot2::element_rect(fill = "transparent",
                                                colour = NA),
      legend.key = ggplot2::element_rect(fill = "transparent",colour = NA),
      strip.background = ggplot2::element_rect(fill = "transparent",
                                               colour = NA),
      
      # add light, dotted major grid lines only
      panel.grid.major = ggplot2::element_line(linetype = "dotted",
                                               colour = "#454545",
                                               size = 0.3),
      panel.grid.minor = ggplot2::element_blank(),
      
      # remove the axis tick marks and hide axis lines
      axis.ticks = ggplot2::element_blank(),
      axis.line = ggplot2::element_line(color = "#454545", size = 0.3),
      
      # modify the bottom margins of the title and subtitle
      plot.title = ggplot2::element_text(size = 18, colour = "#454545",
                                         hjust = 0.5,
                                         margin = ggplot2::margin(b = 10)),
      plot.subtitle = ggplot2::element_text(size = 12, colour = "#454545",
                                            hjust = 0.5,
                                            margin = ggplot2::margin(b = 10)),
      
      # add padding to the caption
      plot.caption = ggplot2::element_text(size = 10, colour = "#454545",
                                           hjust = 1,
                                           margin = ggplot2::margin(t = 15)),
      
      # Adjust text size and axis title position
      axis.title = ggplot2::element_text(size = 13, colour = "#454545",
                                         hjust = 0.95),
      axis.text = ggplot2::element_text(size = 10, colour = "#212121"),
      legend.title = ggplot2::element_text(size = 12, colour = "#454545"),
      legend.text = ggplot2::element_text(size = 10, colour = "#454545"),
      strip.text = ggplot2::element_text(size = 12, colour = "#454545", 
                                         margin = ggplot2::margin(10, 10, 
                                                                  10, 10, 
                                                                  "pt"))
    )
  )
}


firaSave <- function(filename = "plot.pdf", device = "pdf", ...) {
  needsFont <- device == "pdf" || device == "eps" || device == "ps"
  
  # set up ghostscript if needed
  if (Sys.getenv("R_GSCMD") == "" && needsFont) setupGhostScript()
  
  # save the image
  ggplot2::ggsave(filename = filename, device = device, ...)
  
  if (needsFont) extrafont::embed_fonts(filename)
}


