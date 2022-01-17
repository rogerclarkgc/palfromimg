#' @export
scale_color_ipccline <- function(palette = c("line_shading", "line3c", "line4c",
                                             "line5c", "line6c"),
                                 ...){
  palette <- match.arg(palette)
  ggplot2::discrete_scale("colour", palette, pal_ipccline(palette), ...)
}

#' @export
scale_colour_ipccline <- function(palette = c("line_shading", "line3c", "line4c",
                                             "line5c", "line6c"),
                                  ...){
  palette <- match.arg(palette)
  ggplot2::discrete_scale("colour", palette, pal_ipccline(palette), ...)
}

#' @export
scale_fill_ipccline <- function(palette = c("line_shading", "line3c", "line4c",
                                             "line5c", "line6c"),
                                ...){
  palette <- match.arg(palette)
  ggplot2::discrete_scale("fill", palette, pal_ipccline(palette), ...)
}

#' @export
scale_color_rcpsline <- function(palette = c("rcpsline", "rcpslineshade"), ...){
  palette <- match.arg(palette)
  ggplot2::discrete_scale("colour", palette, pal_ipcc_rcpsline(palette), ...)
}

#' @export
scale_colour_rcpsline <- function(palette = c("rcpsline", "rcpslineshade"), ...){
  palette <- match.arg(palette)
  ggplot2::discrete_scale("colour", palette, pal_ipcc_rcpsline(palette), ...)
}

#' @export
scale_fill_rcpsline <- function(palette = c("rcpsline", "rcpslineshade"), ...){
  palette <- match.arg(palette)
  ggplot2::discrete_scale("fill", palette, pal_ipcc_rcpsline(palette), ...)
}

#' @export
scale_color_ipcctp <- function(palette = c("5", "6", "7", "8", "9", "10", "11"),
                               discrete = TRUE,
                               ...){
  palette <- match.arg(palette)
  if (discrete)
    ggplot2::discrete_scale("colour",
                            "ipcc_temperature",
                            pal_ipcc_temperature(palette),
                            ...)
  else
    ggplot2::scale_color_gradientn(colours = pal_ipcc_temperature(palette)(as.numeric(palette)),
                                   ...)
}

#' @export
scale_colour_ipcctp <- function(palette = c("5", "6", "7", "8", "9", "10", "11"),
                                discrete = TRUE,
                                ...){
  palette <- match.arg(palette)
  if (discrete)
    ggplot2::discrete_scale("colour",
                            "ipcc_temperature",
                            pal_ipcc_temperature(palette),
                            ...)
  else
    ggplot2::scale_color_gradientn(colours = pal_ipcc_temperature(palette)(as.numeric(palette)),
                                   ...)
}

#' @export
scale_fill_ipcctp <- function(palette = c("5", "6", "7", "8", "9", "10", "11"),
                              discrete = TRUE,
                              ...){
  palette <- match.arg(palette)
  if(discrete)
    ggplot2::discrete_scale("fill",
                            "ipcc_temperature",
                            pal_ipcc_temperature(palette),
                            ...)
  else
    ggplot2::scale_fill_gradientn(colours = pal_ipcc_temperature(palette)(as.numeric(palette)),
                                  ...)
}


#' @export
scale_color_ipccprp <- function(palette = c("5", "6", "7", "8", "9", "10", "11"),
                                discrete = TRUE,
                                ...){
  palette <- match.arg(palette)
  if (discrete)
    ggplot2::discrete_scale("colour",
                            "ipcc_precipitation",
                            pal_ipcc_precipitation(palette),
                            ...)
  else
    ggplot2::scale_color_gradientn(colours = pal_ipcc_precipitation(palette)(as.numeric(palette)),
                                   ...)
}

#' @export
scale_colour_ipccprp <- function(palette = c("5", "6", "7", "8", "9", "10", "11"),
                                 discrete = TRUE,
                                 ...){
  palette <- match.arg(palette)
  if (discrete)
    ggplot2::discrete_scale("colour",
                            "ipcc_precipitation",
                            pal_ipcc_precipitation(palette),
                            ...)
  else
    ggplot2::scale_color_gradientn(colours = pal_ipcc_precipitation(palette)(as.numeric(palette)),
                                   ...)
}

#' @export
scale_fill_ipccprp <- function(palette = c("5", "6", "7", "8", "9", "10", "11"),
                               discrete = TRUE,
                               ...){
  palette <- match.arg(palette)
  if(discrete)
    ggplot2::discrete_scale("fill",
                            "ipcc_precipitation",
                            pal_ipcc_precipitation(palette),
                            ...)
  else
    ggplot2::scale_fill_gradientn(colours = pal_ipcc_precipitation(palette)(as.numeric(palette)),
                                  ...)
}


#' @export
scale_color_singlehue <- function(palette = c("blue3", "blue4", "blue5",
                                              "purple3", "purple4", "purple5",
                                              "red3", "red4", "red5", "green3",
                                              "green4", "green5"),
                                  discrete = TRUE,
                                  ...){
  palette <- match.arg(palette)
  if(discrete)
    ggplot2::discrete_scale("colour",
                            "ipcc_singlehue",
                            pal_ipcc_singlehue(palette),
                            ...)
  else{
    coln <- as.numeric(sub('.*?(\\d+)', '\\1', palette))
    ggplot2::scale_color_gradientn(colours = pal_ipcc_singlehue(palette)(coln),
                                   ...)
  }
}

#' @export
scale_colour_singlehue <- function(palette = c("blue3", "blue4", "blue5",
                                              "purple3", "purple4", "purple5",
                                              "red3", "red4", "red5", "green3",
                                              "green4", "green5"),
                                   discrete = TRUE,
                                   ...){
  palette <- match.arg(palette)
  if(discrete)
    ggplot2::discrete_scale("colour",
                            "ipcc_singlehue",
                            pal_ipcc_singlehue(palette),
                            ...)
  else{
    coln <- as.numeric(sub('.*?(\\d+)', '\\1', palette))
    ggplot2::scale_color_gradientn(colours = pal_ipcc_singlehue(palette)(coln),
                                   ...)
  }
}

#' @export
scale_fill_singlehue <- function(palette = c("blue3", "blue4", "blue5",
                                             "purple3", "purple4", "purple5",
                                             "red3", "red4", "red5", "green3",
                                             "green4", "green5"),
                                 discrete = TRUE,
                                 ...){
  palette <- match.arg(palette)
  if(discrete)
    ggplot2::discrete_scale("fill",
                            "ipcc_singlehue",
                            pal_ipcc_singlehue(palette),
                            ...)
  else{
    coln <- as.numeric(sub('.*?(\\d+)', '\\1', palette))
    ggplot2::scale_fill_gradientn(colours = pal_ipcc_singlehue(palette)(coln),
                                   ...)
  }
}

#' @export
scale_color_multihue <- function(palette = c("blue3", "blue4", "blue5",
                                             "purple3", "purple4", "purple5",
                                             "red3", "red4", "red5",
                                             "green3", "green4", "green5"),
                                 discrete = TRUE,
                                 ...){
  palette <- match.arg(palette)
  if (discrete)
    ggplot2::discrete_scale("colour",
                            "ipcc_multihue",
                            pal_ipcc_multihue(palette),
                            ...)
  else{
    coln <- as.numeric(sub('.*?(\\d+)', '\\1', palette))
    ggplot2::scale_color_gradientn(colours = pal_ipcc_multihue(palette)(coln),
                                   ...)
  }
}

#' @export
scale_colour_multihue <- function(palette = c("blue3", "blue4", "blue5",
                                             "purple3", "purple4", "purple5",
                                             "red3", "red4", "red5",
                                             "green3", "green4", "green5"),
                                  discrete = TRUE,
                                  ...){
  palette <- match.arg(palette)
  if (discrete)
    ggplot2::discrete_scale("colour",
                            "ipcc_multihue",
                            pal_ipcc_multihue(palette),
                            ...)
  else{
    coln <- as.numeric(sub('.*?(\\d+)', '\\1', palette))
    ggplot2::scale_color_gradientn(colours = pal_ipcc_multihue(palette)(coln),
                                   ...)
  }
}

#' @export
scale_fill_multihue <- function(palette = c("blue3", "blue4", "blue5",
                                            "purple3", "purple4", "purple5",
                                            "red3", "red4", "red5",
                                            "green3", "green4", "green5"),
                                discrete = TRUE,
                                ...){
  palette <- match.arg(palette)
  if (discrete)
    ggplot2::discrete_scale("fill",
                            "ipcc_multihue",
                            pal_ipcc_multihue(palette),
                            ...)
  else{
    coln <- as.numeric(sub('.*?(\\d+)', '\\1', palette))
    ggplot2::scale_fill_gradientn(colours = pal_ipcc_multihue(palette)(coln),
                                  ...)
  }
}

#' @export
scale_color_naturesus <- function(palette = c("nature_sustain"), ...){
  palette <- match.arg(palette)
  ggplot2::discrete_scale("colour",
                          "nature_sustainability",
                          pal_naturesus(palette),
                          ...)
}

#' @export
scale_colour_naturesus <- function(palette = c("nature_sustain"), ...){
  palette <- match.arg(palette)
  ggplot2::discrete_scale("colour",
                          "nature_sustainability",
                          pal_naturesus(palette),
                          ...)
}

#' @export
scale_fill_naturesus <- function(palette = c("nature_sustain"), ...){
  palette <- match.arg(palette)
  ggplot2::discrete_scale("fill",
                          "nature_sustainability",
                          pal_naturesus(palette),
                          ...)
}

#' @export
scale_color_nieceimg <- function(palette = c("church", "lotus", "roof", "toyhouse"), ...){
  palette <- match.arg(palette)
  ggplot2::discrete_scale("colour",
                          "niceimg_palette",
                          pal_niceimg(palette),
                          ...)
}
#' @export
scale_colour_nieceimg <- function(palette = c("church", "lotus", "roof", "toyhouse"), ...){
  palette <- match.arg(palette)
  ggplot2::discrete_scale("colour",
                          "niceimg_palette",
                          pal_niceimg(palette),
                          ...)
}
#' @export
scale_fill_nieceimg <- function(palette = c("church", "lotus", "roof", "toyhouse"), ...){
  palette <- match.arg(palette)
  ggplot2::discrete_scale("fill",
                          "niceimg_palette",
                          pal_niceimg(palette),
                          ...)
}
