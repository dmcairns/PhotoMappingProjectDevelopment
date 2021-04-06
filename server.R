# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com

library(shiny)

shinyServer(function(input, output, session) {

  photos <- readPhotoCoordinates(directoryPath)
  dfDraw <- data.frame(matrix(ncol=0,nrow=0))
  
  print(photos)
  
  output$theMap <- renderPlot({
    ##################################
    # Draws map and creates a        #
    # bounding box with 2.5% margins #
    # if system is not in user       #
    # defined map control mode.      #
    ##################################
    
    t.sampled.photos <- sample.photos.select(photos, input$checkedPhotos)
    if(!input$userControlMap){
      globalValues$b.box <- create.bounding.box(t.sampled.photos, 2.5)
    }
    create.map(t.sampled.photos, globalValues$b.box, input$viewDistance, input$fovOn, dfDraw)
  })
  
  output$info <- renderText({
    ##################################
    # Makes text on stats at the     #
    # bottom of the window work.     #
    ##################################
    
    xy_str <- function(e) {
      if(is.null(e)) return("NULL\n")
      paste0("x=", round(e$x, 2), " y=", round(e$y, 2), "\n")
    }
    
    dms_str <- function(e) {
      if(is.null(e)) return("NULL\n")
      
      if(e$y < 0){
        yChar <- "S"
      } else {
        yChar <- "N"
      }
      
      if(e$x < 0){
        xChar <- "W"
      } else {
        xChar <- "E"
      }
      
      x <- abs(e$x)
      dx <- floor(x)
      mx <- floor((x - dx) * 60)
      sx <- floor((x - dx - mx / 60) * 3600)
      y <- abs(e$y)
      dy <- floor(y)
      my <- floor((y - dy) * 60)
      sy <- floor((y - dy - my / 60) * 3600)
      
      paste0(dy, "°", my, "'", sy, "\"", yChar, " ", dx, "°", mx, "'", sx, "\"", xChar, "\n")
    }
    
    xy_range_str <- function(e) {
      if(is.null(e)) return("NULL\n")
      paste0("xmin=", round(e$xmin, 2), " xmax=", round(e$xmax, 2), 
             " ymin=", round(e$ymin, 2), " ymax=", round(e$ymax, 2), "\n")
    }
    
    str_out <- function(e) {
      if(is.null(e)) return("NULL\n")
      paste0(e, "\n")              
    }
    
    selectedPhotos <- filter(photos, Name %in% input$checkedPhotos)
    
    paste0(
      c(" hover coordinates:", dms_str(input$plot_hover),
      "selected photo 1:", str_out(globalValues$selectedPhoto),
      "selected photo 2:", str_out(globalValues$selectedPhoto2))
    )
  })
  
  observeEvent(input$plot_dblclick, {
    ##################################
    # Create a new bounding box      #
    # when mouse is double clicked   #
    # using brush coordinates if the #
    # brush is not null. If the      #
    # brush is null then a routine   #
    # begins to find the nearest     #
    # photo and select it.           #
    ##################################
    
    if(!is.null(input$plot_brush)){
      t.matrix <- matrix(nrow=2, ncol=2)
      t.matrix[1,] <- c(input$plot_brush$xmin, input$plot_brush$xmax)
      t.matrix[2,] <- c(input$plot_brush$ymin, input$plot_brush$ymax)
      row.names(t.matrix) <- c("x", "y")
      colnames(t.matrix) <- c("min", "max")
      globalValues$b.box <- t.matrix
      session$resetBrush("plot_brush")
      
      ##################################
      # Deselects photos outside of    #
      # a user defined bounding box.   #
      ##################################
      
      selectedPhotos <- filter(photos, Name %in% input$checkedPhotos)
      updateCheckboxGroupInput(session,
                               "checkedPhotos",
                               choices = photosInsideBoundingBox(photos),
                               selected = photosInsideBoundingBox(selectedPhotos))
    } else {
      photosInside <- photosInsideBoundingBox(photos)
      photosIn <- photos %>%
        filter(Name %in% photosInside)
      photosIn$distance <- calc.distance(input$plot_dblclick, photosIn)
      photosIn <- arrange(photosIn, distance)
      photosIn <- head(photosIn, 1)
      globalValues$selectedPhoto <- photosIn$Name
      globalValues$selectedPhoto2 <- NULL
    }
  })
  
  observeEvent(input$selectAll, {
    ##################################
    # Makes select all checkbox      #
    # function correctly.            #
    ##################################
    
    if(input$userControlMap){
      updateCheckboxGroupInput(session,
                               "checkedPhotos",
                               choices = photosInsideBoundingBox(photos),
                               selected = if(input$selectAll) photosInsideBoundingBox(photos))
    } else {
      updateCheckboxGroupInput(session,
                               "checkedPhotos",
                               choices = readPhotoNames(directoryPath),
                               selected = if(input$selectAll) readPhotoNames(directoryPath))
    }
  })
  
  observeEvent(input$modalWindow, {
    ##################################
    # Creates a modal dialog with    #
    # the selected image.           .#
    ##################################
    
    if(!is.null(globalValues$selectedPhoto)){
      filepath <- normalizePath(file.path("Data", "Photos", globalValues$selectedPhoto))
    } else {
      filepath <- normalizePath(file.path("Data", "Other", "placeholder.jpg"))
    }
    jpgFilepath <- convertJPG(filepath)
    widthHeight <- rotateImage(jpgFilepath)
    w <- widthHeight[[2]]
    h <- widthHeight[[1]]
    mw <- 467
    mh <- 400
    if((h * mw) > (w * mh)){
      w <- w * mh / h
      h <- mh
    } else {
      h <- h * mw / w
      w <- mw
    }
    
    output$imageSelected <- renderImage({
      list(width = w,
           height = h,
           src = jpgFilepath)
    }, deleteFile = TRUE)
    
    showModal(div(id = "imageOut", modalDialog(
      title = globalValues$selectedPhoto,
      imageOutput("imageSelected"),
      easyClose = TRUE
    )))
    
  })
  
  observeEvent(input$tableClickUpdate, {
    ##################################
    # Logic controling the selection #
    # of the second photo.           #
    ##################################
    
    if(!is.null(globalValues$selectedPhoto2) && globalValues$selectedPhoto2 == input$tableClickText){
      globalValues$selectedPhoto2 <- NULL
    } else {
      globalValues$selectedPhoto2 <- input$tableClickText
    }
  })
  
  observe({
    ##################################
    # Deselects the selected photo   #
    # if selected photo is outside   #
    # the bounding box or is         #
    # deselected.                    #
    ##################################
    
    selectedPhotos <- filter(photos, Name %in% input$checkedPhotos)
    
    if(!is.null(globalValues$selectedPhoto) & !is.null(photosInsideBoundingBox(photos))){
      if(!(globalValues$selectedPhoto %in% photosInsideBoundingBox(photos))){
        globalValues$selectedPhoto <- NULL
        globalValues$selectedPhoto2 <- NULL
      } else {
        if(!is.null(selectedPhotos)){
          if(!(globalValues$selectedPhoto %in% selectedPhotos$Name)){
            globalValues$selectedPhoto <- NULL
            globalValues$selectedPhoto2 <- NULL
          }
        }
      }
    }
  })
  
  observe({
    ##################################
    # Updates table for photo        #
    # distances/overlap.             #
    ##################################
    
    input$viewDistance
    if(!is.null(globalValues$selectedPhoto)){
      selectedPhoto <- filter(photos, Name %in% globalValues$selectedPhoto)
      selectedPhotos <- filter(photos, Name %in% input$checkedPhotos)
      fakeClick <- selectedPhoto %>%
        mutate(x = GPSLongitude, y = GPSLatitude)
      photosInsideNames <- photosInsideBoundingBox(selectedPhotos)
      photosInside <- filter(photos, Name %in% photosInsideNames)
      photosInside$Distance <- calc.distance(fakeClick, photosInside)
      photosInside <- arrange(photosInside, Distance)
      photosInside <- subset(photosInside, select = c(GPSLatitude, GPSLongitude, Name, Distance))
      dfDrawFiltered <- filter(dfDraw, Name %in% photosInsideNames)
      fovPolysSFTest <- createPolysSF(dfDrawFiltered)
      photoDistTable <- photosInside
      photoOverlapTable <- determineOverlaps(globalValues$selectedPhoto, fovPolysSFTest)
      photoDistTableDT <- datatable(photoDistTable[c(3, 1, 2, 4)], rownames = FALSE)
      photoOverlapTableDT <- datatable(photoOverlapTable, rownames = FALSE)
    } else {
      photoDistTable <- NULL
      photoOverlapTable <- NULL
      photoDistTableDT <- NULL
      photoOverlapTableDT <- NULL
    }
    output$photoDistTableDT <- renderDataTable(photoDistTableDT)
    output$photoOverlapTableDT <- renderDataTable(photoOverlapTableDT)
  })
})