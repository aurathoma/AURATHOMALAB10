library(shiny)
library(ggplot2)
library(dplyr)

run_ci_shiny <- function(data_path = "DDT.csv") {
  ddt <- read.csv(data_path)

  ui <- fluidPage(
    titlePanel("Non-Symmetric Confidence Interval & Hypothesis Testing Dashboard"),
    sidebarLayout(
      sidebarPanel(
        selectInput("var", "Select Quantitative Variable:", choices = c("LENGTH", "WEIGHT", "DDT")),
        selectInput("cat_col", "Category Column:", choices = c("SPECIES", "RIVER")),
        uiOutput("cat_val_ui"),
        sliderInput("alpha", "Significance Level (alpha):", min = 0.01, max = 0.20, value = 0.05, step = 0.01),
        sliderInput("alpha2", "Lower Tail Alpha2:", min = 0.001, max = 0.10, value = 0.02, step = 0.001),
        sliderInput("mu0", "Null Mean (mu0):", min = 1, max = 100, value = 45, step = 0.5),
        sliderInput("epsilon", "Epsilon (epsilon):", min = 0.1, max = 10, value = 4, step = 0.1),
        sliderInput("bins", "Histogram Bins:", min = 5, max = 50, value = 25, step = 1)
      ),
      mainPanel(
        plotOutput("distPlot", click = "plot_click"),
        tableOutput("resultsTable")
      )
    )
  )

  server <- function(input, output, session) {
    output$cat_val_ui <- renderUI({
      req(input$cat_col)
      vals <- unique(ddt[[input$cat_col]])
      selectInput("cat_val", "Select Category Value:", choices = vals, selected = vals[1])
    })

    filtered_data <- reactive({
      req(input$cat_col, input$cat_val, input$var)
      ddt %>% filter(!!sym(input$cat_col) == input$cat_val) %>% pull(!!sym(input$var))
    })

    # Reactive mu0 updated via plot click
    mu_val <- reactiveVal(45)
    observeEvent(input$mu0, { mu_val(input$mu0) }, priority = 1)
    observeEvent(input$plot_click, {
      if (!is.null(input$plot_click$x)) {
        mu_val(round(input$plot_click$x, 2))
        updateSliderInput(session, "mu0", value = round(input$plot_click$x, 2))
      }
    })

    calc_res <- reactive({
      x <- filtered_data()
      req(length(x) > 1)
      ciNonSym(x, alpha = input$alpha, alpha2 = input$alpha2,
               mu0 = mu_val(), epsilon = input$epsilon, bins = input$bins)
    })

    output$distPlot <- renderPlot({
      x <- filtered_data()
      res <- calc_res()
      ggplot(data.frame(x = x), aes(x = x)) +
        geom_histogram(bins = input$bins, fill = "orange", color = "black", alpha = 0.7) +
        geom_vline(xintercept = res$ci, linetype = "dashed", color = "red", linewidth = 1.2) +
        geom_vline(xintercept = mu_val(), linetype = "solid", color = "blue", linewidth = 1.2) +
        labs(title = paste(input$var, "Distribution for", input$cat_col, "=", input$cat_val),
             subtitle = paste("Clicked/Current mu0 =", mu_val(), "| Test Result:", res$test),
             x = input$var, y = "Frequency") +
        theme_minimal()
    })

    output$resultsTable <- renderTable({
      res <- calc_res()
      data.frame(
        Metric = c("CI Lower", "CI Upper", "t Lower", "t Upper", "Test Result", "Ball Lower", "Ball Upper"),
        Value = c(res$ci[1], res$ci[2], res$t[1], res$t[2], res$test, res$ball[1], res$ball[2])
      )
    })
  }

  shinyApp(ui, server)
}
