


mod_info_UI <- function(id){

  ## From https://shiny.rstudio.com/articles/modules.html
  # `NS(id)` returns a namespace function, which was save as `ns` and will
  # invoke later.
  ns <- NS(id)

  ##
  ## UI Content ################################################################
  ##

  ## 4 cards
  ## left width 4 and right width 8
  ## right split top and bottom
  ## right top split left 1/2 right 1/2


  ## Left - Welcome -------------------------------------------------------------------
  card_left <- card(
    h1("Welcome"),

    h4("Objective"),

    p(
      "This app aims to help field inventory teams and other plants observers and
      enthusiasts to validate and keep your species list updated. The script validates
      a list of species names given in a CSV file."
    ),

    p(
      "You are using the package ", tags$code("speval"), "version:",
      textOutput(outputId = ns("speval_version"), inline = TRUE), "."
    ),

    h4("What is ", tags$code("arena-helpers")),
    p(
      "This app is part of of a collection of tools designed to support Forest inventory related
      activities and grouped under ", tags$code("arena-helpers"), "."
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
      to OF Arena."
    )
  )

  ## right top 1 - Instructions ------------------------------------------------
  card_right_top1 <- card(
    h5("Instructions"),
    tags$ol(
      tags$li('In the ', tags$b('Run Validation'), ' tab, upload a list of species name as a CSV file.'),
      tags$li('The species names will be displayed and validation settings appear.'),
      tags$li('Select the taxonomic backbones and the fuzzy matching functions.'),
      tags$li('Select multicore to run the processes in parrallel (only useful if more than a few hundred names to validate).'),
      tags$li('Select IUCN and follow instructions to add IUCN endangered species list to the analysis.')
    ),
  )
  # It can be a one column file with header: "scientific_name"

  ## right top 2 - Results -----------------------------------------------------
  card_right_top2 <- card(
    h5("Results"),
    p("The app outputs a detailed table where the results of each backbone are given for each
      species name with the fuzzy distance of the closest name found in the backbone, the
      proposed accepted name and status of the input species if not an accepted name already."),
    p("The apps also provides a table where a unique validated name is proposed for each input
      species name. The selection process arbitrarily favors the Leipzig Catalogue of Vascular
      Plants but should be crossed-checked carefully, especially if the proposed solution as a
      low fuzzy score."),
    p("Finally, the app provides a series of statistics on the valdiation process: number of
      accepted names found, number of conflicts between taxonomic backbones, etc."
    )
  )

  ## Right bottom - Under the hood ---------------------------------------------
  card_right_bot <- card(
    h5("Under the hood"),
    p("placeholder for more content")
  )

  ## right top combine ---------------------------------------------------------
  card_right_top <- card(
    card_header(
      h4(bsicons::bs_icon("1-circle-fill", size = "1.5rem", class = "text-primary"), "How to use the app")
    ),
    layout_column_wrap(
      width = 1/2,
      card_right_top1,
      card_right_top2
    )
  )



  ##
  ## UI elements order wrapped in a tagList() function #########################
  ##

  tagList(

    layout_columns(
      col_widths = c(4, 8),
      card_left,
      card(
        card_right_top,
        card_right_bot
      )
    )

  ) ## END tagList

} ## END module UI function
