# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com

library(shiny)
library(shinyBS)
library(shinyjs)
library(shinydashboard)
library(shinydashboardPlus)

title <- tags$a(href="https://www.tamu.edu/",
                tags$img(src = "tamu.png",
                         width = 50,
                         height = 50))

header <- dashboardHeader(title = title,
                          titleWidth = 100)

sidebar <- dashboardSidebar(collapsed = TRUE,
                            sidebarMenu(menuItem("Controls",
                                                 tabName = "controls",
                                                 icon = icon("fas fa-sliders-h"),
                                                 sliderInput("viewDistance",
                                                             "FOV Distance",
                                                             0, 100, 10),
                                                 checkboxInput("fovOn",
                                                               "Draw FOV",
                                                               value = FALSE),
                                                 checkboxInput("userControlMap",
                                                               "User Defined Map",
                                                               value = FALSE),
                                                 checkboxInput("showPreview",
                                                               "Show Image Preview on Hover",
                                                               value = TRUE),
                                                 br()
                                                 ),
                                        menuItem("Photos",
                                                 tabName = "photos",
                                                 icon = icon("fas fa-camera-retro"),
                                                 fileInput("inputFile", "Upload:", accept = c('image/jpg', 'image/heic', 'image/png')),
                                                 checkboxInput("selectAll",
                                                               "Select All",
                                                               value = TRUE),
                                                 checkboxGroupInput("checkedPhotos",
                                                                    label = " ",
                                                                    choices = readPhotoNames(directoryPath),
                                                                    selected = readPhotoNames(directoryPath)),
                                                 br()
                                                 )),
                            width = 250,
                            minified = FALSE
                            )

body <- dashboardBody(h1("Repeat Photography Database"),
                      
                      br(),
                      
                      plotOutput("theMap",
                                 click = "plot_click",
                                 dblclick = "plot_dblclick",
                                 hover = "plot_hover",
                                 brush = "plot_brush") %>% withSpinner(type = 5, color="#808080"),
                      br(),
                      
                      verbatimTextOutput("info"),
                      
                      br(),
                      
                      actionButton("modalWindow", "View Selected Photos"),
                      
                      h3(""),
                      
                      dataTableOutput("photoDistTableDT"),
                      
                      bsPopover(id = "photoDistTableDT",
                                title = "Image Preview",
                                content = "<img id = \"previewImg\" src = \"placeholder.jpg\" width = \"200\" height = \"200\">",
                                placement = "top",
                                trigger = "hover"),
                      
                      tagList(
                        tags$script("$(document).on('click', '#photoDistTableDT td', function(){
                                        Shiny.onInputChange('tableClickText', this.textContent);
                                        Shiny.onInputChange('tableClickUpdate', Math.random());
                                    });"),
                        
                        tags$script("$(document).on('dblclick', '#photoDistTableDT td', function(){
                                        Shiny.onInputChange('tableClickText', this.textContent);
                                        Shiny.onInputChange('tableDblclickUpdate', Math.random());
                                    });"),
                        
                        tags$script("$(document).on('mouseenter', '#photoDistTableDT td', function(){
                                        Shiny.onInputChange('tableHoverText', this.textContent);
                                        Shiny.onInputChange('tableHoverUpdate', Math.random());
                                        document.getElementById(\"previewImg\").src = \"placeholder.jpg?t=\" + (new Date().getTime());
                                    })"),
                        
                        tags$script("Shiny.addCustomMessageHandler('refresh', function(time) {
                                        document.getElementById(\"previewImg\").src = \"previews/preview\" + time + \".jpg\";
                                    })"),
                        
                        tags$style(HTML("hr {border-top: 1px solid #000000;}")),
                        
                        tags$style(HTML("@import url('https://fonts.googleapis.com/css?family=Open+Sans');")),
                        
                        tags$style(HTML("@import url('https://fonts.googleapis.com/css?family=Oswald');")),
                        
                        tags$style(HTML("@import url('https://fonts.googleapis.com/css?family=Crimson+Pro');")),
                        
                        tags$style(HTML('.skin-blue .main-header .logo, .skin-blue .main-header .navbar{
                                            background-color: #500000;
                                        }
                                        .skin-blue .main-header .logo:hover, .skin-blue .main-header .navbar .sidebar-toggle:hover{
                                            background-color: #3c0000;
                                        }
                                        .skin-blue .main-sidebar {
                                            background-color: #333333;
                                            font-family: "Open Sans", sans-serif;
                                            font-weight: 300;
                                        }
                                        .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                                            background-color: #3e3e3e;
                                            border-left-color: #732f2f;
                                        }
                                        .skin-blue .main-sidebar .sidebar .sidebar-menu a{
                                            background-color: #333333;
                                            color: #eaeaea;
                                        }
                                        .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover, .skin-blue .sidebar-menu>li>.treeview-menu{
                                            background-color: #3e3e3e;
                                        }
                                        .irs-bar{
                                            background: #732f2f;
                                            border-top: #732f2f;
                                            border-bottom: #732f2f;
                                        }
                                        .irs-bar-edge{
                                            background: #732f2f;
                                            border: 1px solid #732f2f;
                                        }
                                        .irs-from, .irs-to, .irs-single{
                                            color: #eaeaea;
                                            background: #732f2f;
                                            font-family: "Crimson Pro", sans-serif;
                                            font-weight: 300;
                                        }
                                        .irs-grid-pol, .irs-grid-pol.small{
                                            background: #a7a7a7;
                                        }
                                        .irs-grid-text, .checkbox label, label{
                                            color: #eaeaea;
                                            font-family: "Crimson Pro", sans-serif;
                                            font-weight: 300;
                                        }
                                        .input-group-btn:first-child>.btn, .input-group-btn:first-child>.btn-group, .input-group .form-control:last-child, .input-group-addon:last-child, .input-group-btn:first-child>.btn-group:not(:first-child)>.btn, .input-group-btn:first-child>.btn:not(:first-child), .input-group-btn:last-child>.btn, .input-group-btn:last-child>.btn-group>.btn, .input-group-btn:last-child>.dropdown-toggle{
                                            background-color: #eaeaea;
                                            border-bottom-color: #a7a7a7;
                                            border-top-color: #a7a7a7;
                                            border-left-color: #a7a7a7;
                                            border-right-color: #a7a7a7;
                                            color: #333333;
                                        }
                                        .input-group .form-control:last-child, .input-group-addon:last-child, .input-group-btn:first-child>.btn-group:not(:first-child)>.btn, .input-group-btn:first-child>.btn:not(:first-child), .input-group-btn:last-child>.btn, .input-group-btn:last-child>.btn-group>.btn, .input-group-btn:last-child>.dropdown-toggle{
                                            height: 33px;
                                            border-top-right-radius: 3px;
                                            border-bottom-right-radius: 3px;
                                            font-family: "Crimson Pro", sans-serif;
                                            font-weight: 300;
                                        }
                                        .h1, h1{
                                            color: #3c0000;
                                            font-family: "Oswald", sans-serif;
                                            font-weight: 500;
                                            text-align: center;
                                        }'))))

shinyUI(dashboardPage(options = list(),
                      header = header,
                      sidebar = sidebar,
                      body = body))