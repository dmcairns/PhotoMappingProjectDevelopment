# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com

library(shiny)
library(shinyjs)

shinyUI(fluidPage(
  useShinyjs(),
  
  h4(" "),
  
  sidebarLayout(
    sidebarPanel(style = "overflow-y:scroll; max-height: 100vh; position:relative;",
      h4("Control:"),
      
      sliderInput("viewDistance",
                  "FOV Distance",
                  0, 100, 10),
      
      checkboxInput("fovOn",
                    "Draw FOV",
                    value = FALSE),
      
      checkboxInput("userControlMap",
                    "User Defined Map",
                    value = FALSE),
      
      hr(),
      
      h4("Photos:"),
      
      fileInput("inputFile", "Upload image to working directory:", accept = c('image/jpg', 'image/heic', 'image/png')),
      
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
      
      actionButton("modalWindow", "Show modal dialog"),
      
      h4("Statistics"),
      
      verbatimTextOutput("info"),
      
      hr(),
      
      actionButton("modalWindow", "View Selected Photos"),
      
      h3(""),
        
      dataTableOutput("photoDistTableDT"),
      
      tagList(  
        tags$script(paste0("$(document).on('click', '#photoDistTableDT td', function(){
                                           Shiny.onInputChange('tableClickText', this.textContent);
                                           Shiny.onInputChange('tableClickUpdate', Math.random());
                                       });")),
        
        tags$script(paste0("$(document).on('dblclick', '#photoDistTableDT td', function(){
                                           Shiny.onInputChange('tableClickText', this.textContent);
                                           Shiny.onInputChange('tableDblclickUpdate', Math.random());
                                       })")),
      )
    )
  )
))