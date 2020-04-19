
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(fluidPage(

  # Application title
  titlePanel("Repeat Photography Database"),

  # Sidebar with a slider input for number of photos
  sidebarLayout(
    sidebarPanel(
      sliderInput("photoNumber",
                  "Number of photos:",
                  min = 1,
                  max = 11,
                  value = 1)
    ),

    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("theMap",
                 click = "plot_click",
                 dblclick = "plot_dblclick",
                 hover = "plot_hover",
                 brush = "plot_brush",
                 width=530, height=500),
      verbatimTextOutput("info")
    )
  )
))
