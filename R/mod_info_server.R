mod_info_server <- function(id, rv) {

  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    ## OUTPUTS #################################################################
    output$speval_version <- renderText({ as.character(packageVersion("speval")) })

  }) ## END module server function

}
