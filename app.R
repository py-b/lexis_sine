# LOAD --------------------------------------------------------------------

library(shiny)
library(shinyjs)
jscode <- "shinyjs.closeWindow = function() { window.close(); }"
options(encoding = "UTF-8")
source("function_schema_sine.R")

annee_courante <- as.integer(format(Sys.Date(), "%Y"))
next_proj <- (annee_courante + 2) %/% 4 * 4 + 2


# UI ----------------------------------------------------------------------

ui <- fluidPage(

  titlePanel("Générateur calendrier Sine"),

  sidebarLayout(

    sidebarPanel(

      useShinyjs(),

      extendShinyjs(
        text = jscode,
        functions = c("closeWindow")
      ),

      sliderInput(
        inputId = "bornes",
        label = "Période",
        min = 1994,
        max = 2030,
        value = c(1998, 2020),
        sep = ""
      ),

      selectInput(
        inputId = "proj",
        label = "Première cohorte pas encore enquêtée",
        choices = next_proj
      ),

      selectInput(
        inputId = "drop",
        label = "Masquer une cohorte",
        choices = "<aucune>"
      )

    ),

    mainPanel(

      h3("Prévisualisation"),

      plotOutput(
        outputId = "graphique",
        width = "80%"
      ),

      selectInput(
        inputId = "theme",
        label = "Apparence",
        choices = ggplot_themes,
        selected = "minimal"
      ),

      downloadButton(
        outputId = "sauv_img",
        label = "Export image"
      ),

      actionButton(
        inputId = "bQuit",
        label = "Quitter"
      )

    )

  )

)


# SERVER ------------------------------------------------------------------

server <- function(input, output, session) {

  cohortes_graph <- reactive(
    data_full %>%
    filter(annee %>% between(input$bornes[1], input$bornes[2])) %>%
    `[[`("cohorte") %>%
    unique %>%
    sort %>%
    c("<aucune>", .)
  )

  graph <- reactive({
    drop <- if(input$drop == "<aucune>") NULL else as.integer(input$drop)
    proj <- if(input$proj == "<aucune>") 2100 else as.integer(input$proj)
    schema_sine(
      as.integer(input$bornes[1]),
      as.integer(input$bornes[2]),
      cohorte_proj1 = proj,
      cohorte_drop = drop,
      theme = input$theme
    )
  })

  dim_png <- reactive(
    if (input$theme %in% c("economist", "fivethirtyeight", "hc")) {
      # plus haut si légende en haut ou en bas
      list(width = 7.5, height = 5.1)
    } else {
      list(width = 7.5, height = 3.5)
    }
  )

  observe({
    updateSelectInput(
      session,
      inputId = "proj",
      choices = cohortes_graph(),
      selected = if (next_proj %in% cohortes_graph()) next_proj else "<aucune>"
    )
  })

  observe({
    updateSelectInput(
      session,
      inputId = "drop",
      choices = c("<aucune>", cohortes_graph()),
      selected = "<aucune>"
    )
  })

  output$graphique <- renderPlot(
    graph()
  )

  output$sauv_img <- downloadHandler(
    filename = function() {
      sprintf("schema_sine_%s-%s.png", input$bornes[1], input$bornes[2])
    },
    content = function(file) {
      ggsave(
        file,
        graph(),
        width = dim_png()$width,
        height = dim_png()$height
      )
    }
  )

  observeEvent(input$bQuit, {
    js$closeWindow()
    stopApp()
  })

}


# RUN APP -----------------------------------------------------------------

shinyApp(ui, server)
