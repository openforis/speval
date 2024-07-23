


mod_info_UI <- function(id){

  ## From https://shiny.rstudio.com/articles/modules.html
  # `NS(id)` returns a namespace function, which was save as `ns` and will
  # invoke later.
  ns <- NS(id)

  ##
  ## UI Elements ###############################################################
  ##

  card_left <- card(
    h1("Welcome"),

    h4("Objective"),

    p(
      "This app aims to help field inventory teams and other plants observers and
      enthusiasts to validate and keep your species list updated. The script validates
      a list of species names given in a CSV file."
    ),

    p(
      "you are using the package ", tags$code("speval"), "version: ",
      textOutput(outputId = ns("speval_version"), inline = TRUE), "."
    ),

    h4("What is ", tags$code("arena-helpers")),
    p(
      "This app is part of of a collection of tools designed to support Forest inventory related
      activities and grouped under ", tags$code("arena-helpers")
      ),
    br(),
    h4("Open Foris Arena ", tags$img(src="www/Arena-Logo.png", height = '30px')),
    p(
      "They aim to provide additional functionality to ",
      tags$a(
        href = "https://openforis.org/solutions/arena/",
        #alt = "arena-helpers",
        "Open Foris Arena ",
        bsicons::bs_icon("box-arrow-up-right", class = "text-primary"),
        .noWS = "before-end"
      ),
      " in particular support data analysis parts that cannot be embedded directly
      to OF Arena "
    )
  )


  ## Emission factors ----------------------------------------------------------
  card_right_sub1 <- card(
    p("Placeholder for content sub1"),
  )

  card_right_sub2 <- card(
    p("placeholder for content sub2")
  )

  card_right_top <- card(
    card_header(bsicons::bs_icon("1-circle-fill", size = "1.5rem", class = "text-primary"), "Card right top"),
    layout_column_wrap(
      width = 1/2,
      card_right_sub1,
      card_right_sub2
    )
  )

  card_right_bot <- card(
    p("placeholder for more content")
  )

  card_right <- card(
    card_right_top,
    card_right_bot
  )


  ## UI elements wrapped in a tagList() function
  tagList(

    layout_columns(
      col_widths = c(4, 8),
      card_left,
      card_right
    )

  ) ## END tagList

} ## END module UI function
