#' @title Extract color palette from a image file
#' @export
#' @description This function use the Kmeans method to extract the color palette from a image file
#'
#' @param imgpath the path of the image file, only support image with RGB color space now
#' @param coln the number of colors extracted from the image
#' @param alpha the alpha value the palette
#' @param seed random seed of kmeans
#' @param resize resize(shrink) the image before kmeans to avoid memory overflow in Kmeans
#' @param sort sort the palette by color channel
#' @param onlycolour if TRUE, return the colour code instead of colour list object
#' @return A list object/character vector object, contain a ggplot2 palette function and meta info of the image/only colour code
#' @examples
#' pal_deus <- palfromimg(imgpath = "./ipcc10.jpg", coln =10, sort = 2)
#' show_col(pal_deus$pal(10))

palfromimg <- function(imgpath, coln = 6, alpha = 1, seed = 1234, resize = TRUE, sort = 1, onlycolour = FALSE){

  set.seed(seed)
  if (resize)
    img <- resize_img(imgpath, resize = "100")
  else
    img <- image_read(imgpath)
  space <- image_info(img)$colorspace
  img.df <- img2df(img)
  img.kmean <- kmeans(img.df, coln, nstart = 20, iter.max = 20)
  img.center <- img.kmean$centers
  if (!is.null(sort)){
    if (sort > ncol(img.center))
      stop("Wrong channel number, Channel number should < number of colour space!")
    o <- order(img.center[, sort])
    img.center <- img.center[o, ]
    cat(paste("Arrange the palette by channel:", sort, sep = " "))
  }
  colour.pal <- rgb(img.center[, 1],
                    img.center[, 2],
                    img.center[, 3],
                    maxColorValue = 255L,
                    alpha = alpha * 255L)
  if (onlycolour){
    return(colour.pal)
  }else{
    list(pal = scales::manual_pal(colour.pal),
       coln = coln,
       alpha = alpha,
       seed = seed,
       center = img.center,
       colorspace = space)
  }
}

#' @title Generate a ggplot2 color palette scale function (aes = colour)
#' @export
#' @description Generate a color scale function by using the palette extracted from a image
#'
#' @param palette the palette object returned by palfromimg
#' @param discrete the discrete scale or gradient scale
#' @examples
#' pal_deus <- palfromimg(imgpath = "./ipcc10.jpg", coln =10, sort = 2)
#' pal_deus$pal(10) %>% show_col()
#' scale_fill_deus <- scale_fill_mypal(pal_deus)
#' # two ways
#' fig_iris <- ggplot(data = iris, aes(x = Species, y = Sepal.Length, fill = Species)) +
#' geom_boxplot() +
#' scale_fill_deus
#' # or
#' fig_iris <- ggplot(data = iris, aes(x = Species, y = Sepal.Length, fill = Species)) +
#' geom_boxplot() +
#' scale_colour_mypal(pal_deus, discrete = FALSE)
#' @seealso [palfromimg()], [scale_fill_mypal()]

scale_colour_mypal <- function(palette, discrete = TRUE, ...){
  if(!require(ggplot2))
    stop("Need package: ggplot2")
  pal <- palette$pal
  coln <- palette$coln
  if (discrete){
    discrete_scale("colour", "mypal", palette = pal, ...)
  }else{
    scale_color_gradientn(colours = pal(coln), ...)
  }
}

#' @title Generate a ggplot2 color palette scale function (aes = fill)
#' @export
#' @description Generate a color scale function by using the palette extracted from a image
#'
#' @param palette the palette object returned by palfromimg
#' @param discrete the discrete scale or gradient scale
#' @examples
#' pal_deus <- palfromimg(imgpath = "./ipcc10.jpg", coln =10, sort = 2)
#' pal_deus$pal(10) %>% show_col()
#' scale_fill_deus <- scale_fill_mypal(pal_deus)
#' # two ways
#' fig_iris <- ggplot(data = iris, aes(x = Species, y = Sepal.Length, fill = Species)) +
#' geom_boxplot() +
#' scale_fill_deus
#' # or
#' fig_iris <- ggplot(data = iris, aes(x = Species, y = Sepal.Length, fill = Species)) +
#' geom_boxplot() +
#' scale_colour_mypal(pal_deus, discrete = FALSE)
#' @seealso [palfromimg()], [scale_colour_mypal()]

scale_fill_mypal <- function(palette, discrete = TRUE, ...){
  if(!require(ggplot2))
    stop("Need package: ggplot2")
  pal <- palette$pal
  coln <- palette$coln
  if (discrete){
    discrete_scale("fill", "mypal", palette = pal, ...)
  }else{
    scale_fill_gradientn(colours = pal(coln), ...)
  }
}

