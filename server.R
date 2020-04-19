
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyServer(function(input, output) {

  photos <- readPhotoCoordinates(path=".//Data//Photos//")
  observeEvent(input$plot_brush, {
    ##################################
    # Create a new bounding box      #
    ##################################
    # columns are min & max
    # rows are x & y
    print(input$plot_brush$xmin)
    t.matrix <- matrix(nrow=2, ncol=2)
    t.matrix[1,] <- c(input$plot_brush$xmin, input$plot_brush$xmax)
    t.matrix[2,] <- c(input$plot_brush$ymin, input$plot_brush$ymax)
    row.names(t.matrix) <- c("x", "y")
    colnames(t.matrix) <- c("min", "max")
    globalValues$b.box <- t.matrix
  })
  
  output$theMap <- renderPlot({
    t.sampled.photos <- sample.photos(photos, input$photoNumber)
    t.box <- create.bounding.box(t.sampled.photos)
    globalValues$b.box <- t.box
    print(t.box)
    
    create.map(t.sampled.photos, bounding.box=globalValues$b.box)
    #create.map(t.sampled.photos, bounding.box=t.box)
  })
  output$info <- renderText({
    xy_str <- function(e) {
      if(is.null(e)) return("NULL\n")
      paste0("x=", round(e$x, 1), " y=", round(e$y, 1), "\n")
    }
    xy_range_str <- function(e) {
      if(is.null(e)) return("NULL\n")
      paste0("xmin=", round(e$xmin, 1), " xmax=", round(e$xmax, 1), 
             " ymin=", round(e$ymin, 1), " ymax=", round(e$ymax, 1))
    }
    
    paste0(
      "click: ", xy_str(input$plot_click),
      "dblclick: ", xy_str(input$plot_dblclick),
      "hover: ", xy_str(input$plot_hover),
      "brush: ", xy_range_str(input$plot_brush)
    )
  })

})
