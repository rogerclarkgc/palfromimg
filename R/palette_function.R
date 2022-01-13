#' @export
pal_ipccline <- function(palette = c("line_shading", "line3c", "line4c",
                                     "line5c", "line5c", "line6c")){
  palette <- match.arg(palette)
  palette <- paste("ipcc_", palette, sep = "")
  col_code <- palette_data[[palette]]
  manual_pal(col_code)
}

#' export
pal_ipcc_rcpsline <- function(palette = c("rcpsline", "rcpslineshade")){
  palette <- match.arg(palette)
  palette <- paste("ipcc_", palette, sep = "")
  col_code <- palette_data[[palette]]
  manual_pal(col_code)
}

#' @export
pal_ipcc_temperature <- function(palette = c("5", "6", "7", "8", "9", "10", "11")){
  palette <- match.arg(palette)
  palette <- paste("ipcc_temperature", palette, sep = "")
  col_code <- palette_data[[palette]]
  manual_pal(col_code)
}

#' @export
pal_ipcc_precipitation <- function(palette = c("5", "6", "7", "8", "9", "10", "11")){
  palette <- match.arg(palette)
  palette <- paste("ipcc_precipitation", palette, sep = "")
  col_code <- palette_data[[palette]]
  manual_pal(col_code)
}

#' @export
pal_ipcc_singlehue <- function(palette = c("blue3", "blue4", "blue5",
                                           "purple3", "purple4", "purple5",
                                           "red3", "red4", "red5",
                                           "green3", "green4", "green5")){
  palette <- match.arg(palette)
  palette <- paste("ipcc_singlehue", palette, sep = "")
  col_code <- palette_data[[palette]]
  manual_pal(col_code)
}

#' export
pal_ipcc_multihue <- function(palette = c("blue3", "blue4", "blue5",
                                          "purple3", "purple4", "purple5",
                                          "red3", "red4", "red5",
                                          "green3", "green4", "green5")){
  palette <- match.arg(palette)
  palette <- paste("ipcc_multihue", palette, sep = "")
  col_code <- palette_data[[palette]]
  manual_pal(col_code)
}

#' export
pal_niceimg <- function(palette = c("church", "lotus", "roof", "toyhouse")){
  palette <- match.arg(palette)
  palette <- paste("img_", palette, sep = "")
  col_code <- palette_data[[palette]]
  manual_pal(col_code)
}

#' export
pal_naturesus <- function(palette = c("nature_sustain")){
  palette <- match.arg(palette)
  col_code <- palette_data[[palette]]
  manual_pal(col_code)
}
