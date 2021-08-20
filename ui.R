######################################################################
# Species Validation Tool                                    #########
# SPECIES NAMES VALIDATION TOOL                              #########
# (c) Lauri Vesa, Javier Garcia-Perez, Elisee Tchana, FAO    #########
# ui.R file                                                  #########
######################################################################


###########
# LOAD UI #
###########

shinyUI(fluidPage(
  
  # load custom stylesheet
  includeCSS("www/style.css"),
  
  # Tooltip texts end   ****************
  shinyjs::useShinyjs(),
  
  
  # remove shiny "red" warning messages on GUI
  tags$style(type="text/css",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }"
  ),
  
  # load page layout
  dashboardPage(
    
    skin = "blue",
      
    dashboardHeader(title="Species Validation Tool", titleWidth = 300),
    
    dashboardSidebar(width = 300,
      sidebarMenu(
        HTML(paste0(
          "<br>",
          "<img style = 'display: block; margin-left: auto; margin-right: auto;' src='Open-foris-Logo160.jpg' width = '186'>",
          "<br>"
        )),
        menuItem("Home", tabName = "home", icon = icon("home")),
        menuItem("Species Validation", tabName = "table", icon = icon("table")),
        menuItem("Releases", tabName = "releases", icon = icon("tasks")),
        HTML(paste0(
          "<br><br><br><br><br><br><br><br><br>",
          "<br><br><br><br><br><br><br><br><br>",
          "<br><br><br><br><br><br>",
          "<table style='margin-left:auto; margin-right:auto;'>",
            "<tr>",
              
              "<td style='padding: 5px;'><a href='https://www.youtube.com/channel/UCKwDbo_cf5PhwHDGOIcNA2A' target='_blank'><i class='fab fa-youtube fa-lg'></i></a></td>",
              "<td style='padding: 5px;'><a href='https://twitter.com/OpenForis?s=20' target='_blank'><i class='fab fa-twitter fa-lg'></i></a></td>",
          
    
              
          "</table>",
          "<br>"),
        HTML(paste0(
          "<script>",
            "var today = new Date();",
            "var yyyy = today.getFullYear();",
          "</script>")
        ))
      )
      
    ), # end dashboardSidebar
    
    dashboardBody(
     
      tabItems(
        
        tabItem(tabName = "home",
          
          # home section
          includeMarkdown("www/home.md")
          
        ),
        

        
        tabItem(
          tabName = "table",
          
          sidebarPanel(

            fileInput("file1", HTML("Upload CSV File <br><i> Input file with one or two columns: (code,) scientific_name</i>"),
                      accept = c(
                        "text/csv",
                        "text/comma-separated-values,text/plain",
                        ".csv")
            ),
#            checkboxInput("header", "Header", TRUE),
        

            fluidRow(
              box(
                "APPLIED REPOSITORIES", br(), br(),
                "* Leipzig Catalogue of Vascular Plants (LCVP)", br(),
                "* Tropicos - Missouri Botanical Garden", br(),
                "* Plants of the World Online (Kew)", br(),
                "* National Center for Biotechnology Information (NBCI)", br(),
                "* World Flora Online (WFO)", br(),
                "* Global Biodiversity Information Facility (GBIF)", br(),
                "* Global Tree Search (GTS)", br(), 
                
                width = 12,
              )),
            tags$br(),
            actionButton("run_analyse", "Run validation", class = "btn-primary")
          ),

          mainPanel(
            tabsetPanel(
              tabPanel("About input data",
                       tags$br(),
                       

                       textOutput("text"),
                       hr(),
                       tags$br(),
                       div(style='height:300px; width:100%; overflow: scroll ;background: #000000; color: #FFFFFF; margin-bottom: 30px; padding: 25px',
                           tableOutput("console_text")
                       ),

                    
              ),
              tabPanel("Input data",
                       tags$br(),
                       div(style='height:650px; width:100%; background-color: #FFFFFF; overflow-y: scroll',
                           tableOutput("contents")
                       )
                       
              ),
              tabPanel("Result table",
                       tags$br(),
                       downloadButton('downloadData', 'Download'),
                       
                       div(style='height:650px; width:100%; background-color: #FFFFFF;overflow: scroll',
                           tableOutput("result_contents")
                       )
                       

              ),
              tabPanel("Statistics",
                       tags$br(),
                       downloadButton('downloadData2', 'Download stat1'),
                       downloadButton('downloadData3', 'Download stat2'),
                       div(style='height:650px; width:100%; background-color: #FFFFFF; overflow: scroll',
                           tableOutput("result_statistics"),
                           tableOutput("result_statistics_2")
                       ),
                     
              ),
              id="tabs"), width = 7,  # tabsetPanel
          )# mainPanel
        
          
        ),
        
        
        tabItem(tabName = "releases", includeMarkdown("www/releases.md"))
              
      )
    
    ) # end dashboardBody
  
  )# end dashboardPage

))
