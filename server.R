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
    
    str_out <- function(e) {
      if(is.null(e)) return("NULL\n")
      paste0(e, "\n")              
    }
    
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
    imgPath <- normalizePath(file.path("Data", "Photos", globalValues$selectedPhoto))
    showModal(modalDialog(
      title = "Test",
      HTML('<img src="http://www.google.nl/images/branding/googlelogo/2x/googlelogo_color_272x92dp.png">'),
      easyClose = TRUE,
      footer = NULL
    ))
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
      distNames <- photoDistTable$Name
      overlapNames <- photoOverlapTable$image
      overlapCol <- c()
      for(name in distNames){
        if(name %in% overlapNames){
          overlapCol <- c(overlapCol, photoOverlapTable[which(photoOverlapTable$image == name), ]$pct.overlap[[1]])
        } else {
          overlapCol <- c(overlapCol, 0)
        }
      }
      photoDistTable$Overlap <- overlapCol
      photoDistTableDT <- datatable(photoDistTable[c(3, 1, 2, 4, 5)], rownames = FALSE)
    } else {
      photoDistTable <- NULL
      photoOverlapTable <- NULL
      photoDistTableDT <- NULL
    }
    output$photoDistTableDT <- renderDataTable(photoDistTableDT)
  })
})