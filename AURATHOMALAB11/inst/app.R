library(shiny)
library(dplyr)
library(ggplot2)

# Custom bootstrap function provided in instructions
myboot <- function(x, fun){
  xs <- sample(x = x, size = length(x), replace = TRUE)
  do.call(what = fun, list(x = xs))
}

# Load DDT dataset
ddt <- read.csv("DDT.csv")
ui <- fluidPage(
  titlePanel("Task 2: Bootstrapping Shiny App"),

  sidebarLayout(
    sidebarPanel(
      # Selectors for quantitative variable and category filters
      selectInput("var", "Quantitative Variable:", choices = c("LENGTH", "WEIGHT", "DDT")),
      selectInput("river", "Select River:", choices = unique(ddt$RIVER)),
      selectInput("species", "Select Species:", choices = unique(ddt$SPECIES)),

      selectInput(inputId = "fun",
                  label = "Function to calculate stats",
                  choices = c("mean", "var", "sd", "IQR")),

      sliderInput("iter",
                  "Number of iterations (bins/samples):",
                  min = 100,
                  max = 10000,
                  value = 500,
                  step = 100),

      sliderInput("alpha",
                  "Alpha value (alpha / alpha2):",
                  min = 0.01,
                  max = 0.2,
                  value = 0.05,
                  step = 0.01),

      # Numeric display for clicked x-coordinate (mu_0 update)
      textOutput("mu_display")
    ),

    mainPanel(
      plotOutput("bootPlot", click = "plot_click"),
      tableOutput("ci_table") # List output printed to a table
    )
  )
)

server <- function(input, output, session) {

  # Reactive value to store clicked x-coordinate (mu_0)
  mu_0 <- reactiveVal(NULL)

  observeEvent(input$plot_click, {
    mu_0(input$plot_click$x)
  })

  # Filter data dynamically based on user selections
  filtered_data <- reactive({
    ddt %>%
      filter(RIVER == input$river, SPECIES == input$species) %>%
      pull(!!sym(input$var))
  })

  v <- reactive({
    x_vec <- filtered_data()
    req(length(x_vec) > 0)

    xs <- replicate(input$iter, expr = myboot(x_vec, input$fun))
    ci <- quantile(xs, c(input$alpha/2, 1 - input$alpha/2))

    list(xs = xs, ci = data.frame(Parameter = c("Lower Bound", "Upper Bound"), Value = ci))
  })

  output$bootPlot <- renderPlot({
    data_vals <- v()$xs

    # ggplot visualization with custom heading and colors
    df_plot <- data.frame(xs = data_vals)
    p <- ggplot(df_plot, aes(x = xs)) +
      geom_histogram(aes(y = ..density..), bins = 30, fill = "dodgerblue", color = "black") +
      labs(title = paste0("Bootstrap Distribution for ", input$var, " (", input$river, ", ", input$species, ")"),
           subtitle = paste0("Iterations = ", input$iter),
           x = input$var,
           y = "Density") +
      theme_minimal()

    # Add vertical line for clicked mu_0 if available
    if (!is.null(mu_0())) {
      p <- p + geom_vline(xintercept = mu_0(), color = "red", linetype = "dashed", size = 1)
    }

    p
  }, res = 96)

  output$ci_table <- renderTable({
    v()$ci
  })

  output$mu_display <- renderText({
    if (is.null(mu_0())) {
      "Click on the plot to update μ₀"
    } else {
      paste0("Current μ₀ (Clicked X): ", round(mu_0(), 2))
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)
