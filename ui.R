#################################################################################
# Species Validation Tool                                               #########
# SPECIES NAMES VALIDATION TOOL                                         #########
# (c) Lauri Vesa, Geal Sola, Javier Garcia-Perez, Elisee Tchana, FAO    #########
# ui.R file                                                             #########
#################################################################################


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
        id = "tabs",
        HTML(paste0(
          "<br>",
          "<img style = 'display: block; margin-left: auto; margin-right: auto;' src='Open-foris-Logo160.jpg' width = '186'>",
          "<br>"
        )),
        menuItem("Home"          , tabName = "home"    , icon = icon("home")),
        menuItem("Run Validation", tabName = "valid"   , icon = icon("leaf")),
        menuItem("Get results"   , tabName = "results" , icon = icon("table")),
        menuItem("Releases"      , tabName = "releases", icon = icon("tasks")),
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
        
        ## --- Home ---------------------------------------------------------
        tabItem(tabName = "home", includeMarkdown("www/home.md")),
        
        
        
        ## --- Run validation ----------------------------------------------- 
        tabItem(
          tabName = "valid",
          
          #fluidRow(
            
            ## Title
            h2("Upload species list and run the species validation algorithm"),
            br(), 
            h4("Before you can import a species list, 
               you need to prepare a table and save it as a CSV format file. 
               Then upload the file into this app."),
            br(), br(),
            
            ## Left column, load data 
            column(
              6,
              h4(strong("Upload CSV file")),
              br(),
              h4(em("Input file with one or two columns: ")),
              h4(em("code (optional), scientific_name")),
              fileInput("file1", NULL, accept = c(".csv", ".tsv")),
              #br(),
              fluidRow(
                tableOutput("contents"), 
                style = 'height:500px; width:100%; background-color: #FFFFFF; 
                 margin-bottom: 30px; padding: 25px; overflow-y: scroll'
                ),
              br()
            ),
            
            ## Right column, run validation
            column(
              6,
              h4(strong("Run the validation algorithm")),
              br(),
              conditionalPanel(
                condition = "output.file_valid",

                checkboxInput("opt_multicore", "Multicore processing", TRUE ),
                actionButton("run_analyse", "Run validation", class = "btn-primary"),
                br(), br(), br(), br(),
                fluidRow(
                  tableOutput("console_text"), 
                  style = 'height:400px; witdh=100%; background-color: #000000; 
                  color: #FFFFFF; margin-bottom: 30px; padding: 25px; overflow-y: scroll'
                ),
                hidden(
                  div(
                    id = "after_valid",
                    h4(icon("arrow-right"), "Continue to results"),
                    HTML("&nbsp;"),
                    actionButton("to_results", "Results")
                  )
                )
                
              ) ## End conditionalPanel
            )
          #) ## End fluidRow
          
        ),
        
        
        
        ## --- Get results --------------------------------------------------
        tabItem(
          tabName = "results",
          
          ## Title
          h2("Species validation results"),
          br(), 
          
          conditionalPanel(
            condition = "output.validation_done",
            
            tabsetPanel(
              tabPanel("Main results",
                       tabName = "res_table1",
                       tags$br(),
                       downloadButton('downloadData', 'Download'),
                       fluidRow(tableOutput("result_contents"), style='height:650px; width:100%; background-color: #FFFFFF;overflow: scroll')
              ),
              tabPanel("Stat1",
                       tabName = "res_stat1",
                       tags$br(),
                       downloadButton('downloadStat1', 'Download'),
                       fluidRow(tableOutput("stat1"), style='height:650px; width:100%; background-color: #FFFFFF;overflow: scroll')
              ),
              tabPanel("Stat2",
                       tabName = "res_stat2",
                       tags$br(),
                       downloadButton('downloadStat2', 'Download'),
                       fluidRow(tableOutput("stat2"), style='height:650px; width:100%; background-color: #FFFFFF;overflow: scroll')
              ),
              tabPanel("Stat3",
                       tabName = "res_stat3",
                       tags$br(),
                       downloadButton('downloadStat3', 'Download'),
                       fluidRow(tableOutput("stat3"), style='height:650px; width:100%; background-color: #FFFFFF;overflow: scroll')
              )
            ) ## End tabsetPanel
          ) ## End conditionalPanel
        ),
          
        #   mainPanel(
        #     tabsetPanel(
        #       tabPanel("Upload and run",
        #                tags$br(),
        #                fileInput("file1", 
        #                          HTML("Upload CSV File <br><i> Input file with one or two columns: (code,) scientific_name</i>"),
        #                          accept = ".csv"
        #                          # accept = c(
        #                          #   "text/csv",
        #                          #   "text/comma-separated-values,text/plain",
        #                          #   ".csv")
        #                ),
        #                fluidRow(
        #                  column(width = 12,
        #                         textOutput("text")
        #                  )),
        #                checkboxInput("opt_multicore", "Multicore processing", TRUE ),
        #                actionButton("run_analyse", "Run validation", class = "btn-primary"),
        #                
        #                tags$br(),tags$br(),
        #                fluidRow(tableOutput("console_text"), style = "height:550px; background-color: #000000; color: #FFFFFF; margin-bottom: 30px; padding: 25px; overflow-y: scroll")
        #       ),
        #       tabPanel("Input data",
        #                tags$br(),
        #                fluidRow(tableOutput("contents"), style='height:650px; width:100%; background-color: #FFFFFF; overflow-y: scroll')
        #       ),
        #       tabPanel("Results",
        #                tabsetPanel(
        #                   tabPanel("Main results",
        #                     tabName = "res_table1",
        #                     tags$br(),
        #                     downloadButton('downloadData', 'Download'),
        #                     fluidRow(tableOutput("result_contents"), style='height:650px; width:100%; background-color: #FFFFFF;overflow: scroll')
        #                   ),
        #                   tabPanel("Stat1",
        #                            tabName = "res_stat1",
        #                            tags$br(),
        #                            downloadButton('downloadStat1', 'Download'),
        #                            fluidRow(tableOutput("stat1"), style='height:650px; width:100%; background-color: #FFFFFF;overflow: scroll')
        #                   ),
        #                   tabPanel("Stat2",
        #                            tabName = "res_stat2",
        #                            tags$br(),
        #                            downloadButton('downloadStat2', 'Download'),
        #                            fluidRow(tableOutput("stat2"), style='height:650px; width:100%; background-color: #FFFFFF;overflow: scroll')
        #                   ),
        #                   tabPanel("Stat3",
        #                            tabName = "res_stat3",
        #                            tags$br(),
        #                            downloadButton('downloadStat3', 'Download'),
        #                            fluidRow(tableOutput("stat3"), style='height:650px; width:100%; background-color: #FFFFFF;overflow: scroll')
        #                   )
        #                   
        #                )
        #       ), id="tabs"), width = 12,  # tabsetPanel
        #   )# mainPanel
        # ),
        
        
        ## --- Releases ----------------------------------------------------- 
        tabItem(tabName = "releases", includeMarkdown("www/releases.md"))
              
      ) ## End tabItems
    
    ) # end dashboardBody
  
  ) # end dashboardPage

))
