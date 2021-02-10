# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com

library(shiny)

shinyUI(fluidPage(
  h4(" "),
  
  sidebarLayout(
    sidebarPanel(style = "overflow-y:scroll; max-height: 100vh; position:relative;",
      sliderInput("viewDistance",
                  "FOV Distance",
                  0, 100, 50),
      
      h4("Control Modes"),
      
      checkboxInput("fovOn",
                    "Draw FOV",
                    value = FALSE),
      
      checkboxInput("userControlMap",
                    "User Defined Map",
                    value = FALSE),
      
      h4("Photos"),
      
      checkboxInput("selectAll",
                    "Select All",
                    value = TRUE),
      
      checkboxGroupInput("checkedPhotos",
                         label = " ",
                         choices = readPhotoNames(directoryPath),
                         selected = readPhotoNames(directoryPath))
    ),
    
    mainPanel(style = "overflow-y:scroll; max-height: 100vh; position:relative;",
      h1("Repeat Photography Database"),
      
      plotOutput("theMap",
                 click = "plot_click",
                 dblclick = "plot_dblclick",
                 hover = "plot_hover",
                 brush = "plot_brush"),
      
      actionButton("modalWindow", "View Selected Photo"),
      
      h4("Statistics"),
      
      verbatimTextOutput("info"),
      
      h4("Photo Distances"),
      
      dataTableOutput("photoDistTableDT"),
      
      h4("Overlapping Images"),
      
      dataTableOutput("photoOverlapTableDT")
    )
  )
))