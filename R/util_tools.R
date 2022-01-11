#' @export
#' @title Resize(Shrink) the image
#' @description Resize the image to smaller size
#' @param imgpath the path of image file
#' @param resize resize proportionally to width: default 50 px
#' @param write logical, if TRUE, write the image object to file
#' @param writepath the path of writed image
#' @return A magick object or image_write function

resize_img <- function(imgpath, resize = "50", write = F, writepath = "img_resize.jpg"){

  if(!require(magick))
    stop("Need package: magick")
  img <- image_read(imgpath)
  img <- image_scale(img, resize)
  if (write)
    image_write(img, path = writepath)
  else
    img
}

#' @export
#' @title Extract the raw data from a image
#' @description Extract the raw data from a image object, return an array object
#' @param img the magick image object
#' @return A array, the dimensions of the array are height, width,channel

img2array <- function(img){
  # img: the magick image object
  # return: a array height * width * channel
  as.integer(img[[1]])
}

#' @export
#' @title Transfrom the image to dataframe
#' @description Extract the raw data from a image object, return an data.frame object
#' @param img the magick image object
#' @return A dataframe, the columns are the channels of the image, row are the pixels

img2df <- function(img){

  img_arr <- img2array(img)
  ch <- dim(img_arr)[3]
  img_df <- as.data.frame(lapply(c(1:ch), function(x){as.integer(img_arr[,,x])}),
                          col.names = paste("channel", c(1:ch), sep = ""))
  return(img_df)
}

#' @export
#' @title Automate select the number of colours from an image
#' @description Using the variance-bootstrap method to automate select the number of colours
#' @param imgpath the path of image
#' @return a ggplot2 obejct, containing the boot-variance result.
#' @seealso [factoextra::fviz_nbclust()]
nb_col <- function(imgpath){
  # imgpath: path of image
  # return: ggplot object, display optimal number of clusters
  img_s <- resize_img(imgpath, resize = "50")
  img_df <- img2df(img_s)
  if(!require(factoextra))
    stop("Need package: factoextra")
  fviz_nbclust(img_df, kmeans, method = "wss", k.max = 10)
}
