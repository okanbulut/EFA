# Ensure required packages are installed
required_packages <- c("shiny", "psych", "ggplot2", "corrplot", "gridExtra")
installed_packages <- installed.packages()

for (pkg in required_packages) {
  if (!(pkg %in% rownames(installed_packages))) {
    install.packages(pkg)
  }
}

# Load packages ----
library("shiny")
library("psych")
library("ggplot2")
library("corrplot")
library("gridExtra")

# Define UI for the Shiny app
ui <- fluidPage(
  titlePanel("Exploratory Factor Analysis with the 'psych' Package"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload CSV File", accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
      uiOutput("variableSelector"),
      numericInput("nfactors", "Number of Factors:", value = 2, min = 1),
      selectInput("rotate", "Rotation Method:",
                  choices = c("none", "varimax", "promax", "oblimin", "quartimin", "geominT", "geominQ", "bentlerT", "bentlerQ", "tandemI", "tandemII", "none"),
                  selected = "oblimin"),
      selectInput("fm", "Factor Method:",
                  choices = c("minres", "ml", "pa", "wls", "gls", "minchi", "minrank"),
                  selected = "ml"),
      selectInput("cor", "Correlation Type:",
                  choices = c("pearson", "kendall", "spearman", "tet", "poly"),
                  selected = "pearson"),
      checkboxInput("plot", "Plot Model Outcome?", value = FALSE),
      checkboxInput("heatmap", "Show Correlation Heatmap?", value = FALSE),
      checkboxInput("barplots", "Show Factor Loading Bar Plots?", value = FALSE),
      actionButton("run", "Run Factor Analysis")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Preview", tableOutput("datasetPreview")),
        tabPanel("Correlation Heatmap", plotOutput("heatmapPlot", width = "100%", height = "800px")),
        tabPanel("Analysis Output", verbatimTextOutput("faOutput")),
        tabPanel("Model Plot", plotOutput("faPlot")),
        tabPanel("Factor Loadings", plotOutput("barPlots"))
      )
    )
  )
)

# Define server logic for the Shiny app
server <- function(input, output) {
  
  dataset <- reactive({
    req(input$file)
    read.csv(input$file$datapath)
  })
  
  output$datasetPreview <- renderTable({
    req(dataset())
    head(dataset(), 10) # Show only the first 10 rows
  })
  
  output$variableSelector <- renderUI({
    req(dataset())
    checkboxGroupInput("selectedVars", "Select Variables for Factor Analysis:",
                       choices = names(dataset()),
                       selected = names(dataset()))
  })
  
  observeEvent(input$run, {
    req(dataset(), input$selectedVars)
    isolate({
      data <- dataset()[, input$selectedVars, drop = FALSE]
      
      # Run the fa function with user-specified parameters
      fa_result <- tryCatch({
        fa(
          r = data,
          nfactors = input$nfactors,
          rotate = input$rotate,
          fm = input$fm,
          cor = input$cor,
          scores = TRUE
        )
      }, error = function(e) e)
      
      # Display output
      output$faOutput <- renderPrint({
        if (inherits(fa_result, "error")) {
          paste("Error:", fa_result$message)
        } else {
          print(fa_result)
        }
      })
      
      # Plot output if selected
      output$faPlot <- renderPlot({
        req(input$plot)
        if (!inherits(fa_result, "error")) {
          fa.diagram(fa_result)
        }
      })
      
      # Correlation heatmap if selected
      output$heatmapPlot <- renderPlot({
        req(input$heatmap)
        corr <- cor(data, use = "pairwise.complete.obs")
        corrplot(corr, method = "color", tl.col = "black", addCoef.col = "black", tl.cex = 1.2, type = "lower")
      }, width = 1200, height = 800)
      
      # Factor loading bar plots if selected
      output$barPlots <- renderPlot({
        req(input$barplots)
        if (!inherits(fa_result, "error")) {
          loadings <- fa_result$loadings
          plots <- lapply(1:ncol(loadings), function(i) {
            ggplot(data.frame(Variables = rownames(loadings), Loading = loadings[, i], Highlight = abs(loadings[, i]) >= 0.3), 
                   aes(x = Variables, y = Loading, fill = Highlight)) +
              geom_bar(stat = "identity", show.legend = FALSE) +
              scale_fill_manual(values = c("TRUE" = "blue", "FALSE" = "lightgray")) +
              ggtitle(paste("Factor", i)) +
              theme_bw() +
              theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
              labs(caption = "Note: Blue bars represent loadings >= 0.3; Gray bars represent loadings < 0.3.") +
              scale_y_continuous(limits = c(-1, 1), breaks = seq(-1, 1, by = 0.25))
          })
          do.call(grid.arrange, c(plots, ncol = 2))
        }
      })
    })
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
