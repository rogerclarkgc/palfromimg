#' @title Sort the palette by hand
#' @export
#' @description This shiny APP can handle the order of the palette by hand
#'
#' @param pal the palette list object returned by palfromimg

#' @return A list object, contain a ggplot2 palette function and meta info of the image
#' @examples
#' pal_deus <- palfromimg(imgpath = "./ipcc10.jpg", coln =10, sort = 2)
#' # the order of the palette is not optimized for gradient scale, re-order it
#' pal_deus_new <- palette_sort(pal_deus)
#' # order the palette by hand in the App window
#' @seealso [palfromimg()]

palette_sort <- function(pal){
  coln <- pal$coln
  old_pal <- pal$pal
  if(!(require(shiny) & require(shinyjqui) & require(scales)))
    stop("Need package: shiny, shinyjqui, scales")
  ui <- fluidPage(
    fluidRow(
      column(12,
             orderInput("op", "The Plalette are:",
                        items = old_pal(coln),
                        item_class = "info"))
    ),
    fluidRow(
      column(12,
             plotOutput("order"))
    ),
    fluidRow(
      column(6,
             actionButton("done", "Done"))
    )
  )
  server <- function(input, output, session){
    output$order <- renderPlot({
      scales::show_col(input$op)
    }
    )
    observeEvent(input$done,{
        new_cols <- input$op
        pal$pal <- scales::manual_pal(new_cols)
        stopApp(pal)}
    )
  }
  viewer <- dialogViewer("Sort the palette by hand", width = 800, height = 750)
  runGadget(shinyApp(ui, server),
            viewer = viewer,
            stopOnCancel = FALSE)
}
