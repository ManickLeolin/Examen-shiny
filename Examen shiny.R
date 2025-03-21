library(shiny)
library(dplyr)
library(ggplot2)
library(DT)
library(bslib)
library(thematic)
library(plotly)

data("diamonds")

thematic_shiny()

ui <- fluidPage(
  theme = bs_theme(
    version = 5,
    bootswatch = "quartz"
  ),
  titlePanel("Exploration des Diamants"),
  sidebarLayout(
    sidebarPanel(
      h2("Filtrer par Prix"),
      sliderInput(
        inputId = "Price",
        label = "Prix des Diamants",
        min = min(diamonds$price),
        max = max(diamonds$price),
        value = c(min(diamonds$price), 5000)
      ),
      actionButton(
        inputId = "bouton",
        label = "Appliquer le filtre"
      )
    ),
    mainPanel(
      plotlyOutput(outputId = "DiamondPlot"), 
      DTOutput(outputId = "DiamondTable")
    )
  )
)

server <- function(input, output) {
  
  # Filtrage réactif basé sur le prix uniquement
  filtered_data <- reactive({
    diamonds %>%
      filter(price >= input$Price[1], price <= input$Price[2])
  })
  
  # Table interactive
  output$DiamondTable <- renderDT({
    req(filtered_data()) 
    datatable(filtered_data())
  })
  
  # Graphique interactif avec plotly
  output$DiamondPlot <- renderPlotly({
    req(filtered_data())
    
    p <- ggplot(filtered_data(), aes(x = carat, y = price, color = cut)) +
      geom_point(alpha = 0.5) +
      labs(
        title = "Relation entre Carat et Prix",
        x = "Carats",
        y = "Prix ($)"
      ) +
      theme_minimal()
    
    ggplotly(p)
  })
}

shinyApp(ui = ui, server = server)




