# ============================================================
# 10 R Shiny — final combined app (for deployment, one app.R)
# ------------------------------------------------------------
# Folder layout
#   murders_Rshiny_app/          <- this folder name becomes the deployed URL
#   ├── app.R             <- this file
#   └── murders.csv       <- the data the app reads (same folder as app.R)
# ============================================================


# ============================================================
# [Setup — once only] Make the CSV data file
# ------------------------------------------------------------
# Run the block below "once" to create murders.csv in the same
# folder as app.R. Once the file exists you do not need to run it again,
# and it must not run inside app.R when you deploy, so it is commented out (#).
#
# # install.packages("dslabs")   # package with the original murders data
# library(dslabs)
# data(murders)
# write.csv(murders, "murders.csv", row.names = FALSE)   # save in the same folder
#
# ============================================================


# ------------------------------------------------------------
# Load packages
# (Do not put install.packages() in app.R -> a common cause of deployment errors)
# ------------------------------------------------------------

# install.packages("shiny")
library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)


# ------------------------------------------------------------
# Load the data: always with a 'relative path'
#   O  read.csv("murders.csv")                      -> relative to the app.R folder, works after deployment
#   X  read.csv("C:/Users/me/Desktop/murders.csv")  -> a path only on your computer, fails after deployment
# Run App / runApp() / the deployment server all run from the folder that holds app.R,
# so for a file in the same folder you only need its name.
# ------------------------------------------------------------
murders <- read.csv("data/murders.csv")

# Compute the murder rate (per 100,000 people) once, outside the app (at the top)
# -> runs only once when the app starts, which is efficient
murders <- murders %>%
  mutate(
    region = as.character(region),
    rate   = total / population * 100000
  )

# y-axis choices: name shown on screen = actual variable name
y_choices <- c("Number of murders" = "total", "Murders per 100,000" = "rate")


# ------------------------------------------------------------
# UI : what to show, and where
# ------------------------------------------------------------
ui <- page_sidebar(
  title = "Exploring US Murder Rates",

  # ---- Left sidebar: input widgets --------------------------------
  sidebar = sidebar(
    width = 300,

    selectInput(
      inputId  = "selected_region",
      label    = "Region",
      choices  = c("All", sort(unique(murders$region))),
      selected = "Northeast"
    ),

    sliderInput(
      inputId = "pop_range",
      label   = "Population range",
      min     = 0,
      max     = max(murders$population),
      value   = range(murders$population),
      step    = 100000,
      sep     = ","
    ),

    radioButtons(
      inputId  = "y_var",
      label    = "y-axis variable",
      choices  = y_choices,
      selected = "total"
    ),

    checkboxInput("show_labels", "Show state abbreviations", value = TRUE),
    checkboxInput("show_lm",     "Show regression line",     value = FALSE),

    hr(),
    downloadButton("download_csv", "Download filtered CSV")
  ),

  # ---- Main area, row 1: three summary numbers ------------------------------
  layout_columns(
    col_widths = c(4, 4, 4),
    value_box(title = "States selected",          value = textOutput("n_states")),
    value_box(title = "Total murders",            value = textOutput("sum_total")),
    value_box(title = "Mean rate (per 100,000)",  value = textOutput("mean_rate"))
  ),

  # ---- Main area, row 2: plot (6 columns) + table (6 columns) ----------------------
  layout_columns(
    col_widths = c(6, 6),
    card(
      card_header("Murders vs population"),
      plotOutput("murder_plot", height = "380px")
    ),
    card(
      card_header("Selected states"),
      tableOutput("result")
    )
  ),

  # ---- Main area, row 3: regression summary -------------------------------
  card(
    card_header("Regression: y ~ population (millions)"),
    verbatimTextOutput("lm_summary")
  )
)


# ------------------------------------------------------------
# Server : takes the inputs, computes, and makes the outputs
# ------------------------------------------------------------
server <- function(input, output) {

  # [Key] reactive(): two inputs -> one computation -> reused by many outputs
  filtered_data <- reactive({
    out <- murders %>%
      filter(
        population >= input$pop_range[1],
        population <= input$pop_range[2]
      )

    if (input$selected_region != "All") {
      out <- out %>% filter(region == input$selected_region)
    }

    out
  })

  # Display name of the chosen y variable (used as the axis title)
  y_label <- reactive({
    names(y_choices)[y_choices == input$y_var]
  })

  # ---- Summary numbers ------------------------------------------------
  output$n_states <- renderText({
    nrow(filtered_data())
  })

  output$sum_total <- renderText({
    format(sum(filtered_data()$total), big.mark = ",")
  })

  output$mean_rate <- renderText({
    d <- filtered_data()
    if (nrow(d) == 0) return("-")
    round(mean(d$rate), 2)
  })

  # ---- Scatter plot ---------------------------------------------------
  output$murder_plot <- renderPlot({
    d <- filtered_data()

    # If no state matches, show a message instead of an error
    validate(need(nrow(d) > 0, "No states match. Try a wider population range."))

    p <- ggplot(d, aes(x = population / 1e6, y = .data[[input$y_var]])) +
      geom_point(aes(color = region), size = 3) +
      labs(
        x     = "Population (millions)",
        y     = y_label(),
        color = "Region"
      ) +
      theme_minimal(base_size = 14)

    if (input$show_labels) {
      p <- p + geom_text(aes(label = abb), vjust = -0.9, size = 3.5)
    }

    if (input$show_lm && nrow(d) >= 3) {
      p <- p + geom_smooth(method = "lm", se = TRUE, color = "#B57A23", formula = y ~ x)
    }

    p
  })

  # ---- Table --------------------------------------------------------
  output$result <- renderTable(striped = TRUE, spacing = "xs", {
    filtered_data() %>%
      arrange(desc(.data[[input$y_var]])) %>%
      transmute(
        `State`                 = state,
        `Region`                = region,
        `Population (millions)` = round(population / 1e6, 2),
        `Murders`               = as.integer(total),
        `Rate`                  = round(rate, 2)
      )
  })

  # ---- Regression results (statistics updated live) ---------------------
  output$lm_summary <- renderPrint({
    d <- filtered_data()
    validate(need(nrow(d) >= 3, "At least 3 states are needed to fit the regression."))

    d$pop_million <- d$population / 1e6
    fit <- lm(reformulate("pop_million", response = input$y_var), data = d)

    cat("y =", y_label(), " / n =", nrow(d), "\n\n")
    print(round(summary(fit)$coefficients, 4))
    cat("\nR-squared:", round(summary(fit)$r.squared, 3), "\n")
  })

  # ---- CSV download ----------------------------------------------
  # observeEvent + write.csv() saves the file on the 'server computer'.
  # For users of a deployed app to get the file, use downloadHandler().
  output$download_csv <- downloadHandler(
    filename = function() {
      paste0("murders_", input$selected_region, "_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(filtered_data(), file, row.names = FALSE)
    }
  )
}


# ------------------------------------------------------------
# Run the app
# ------------------------------------------------------------
shinyApp(ui, server)


# ============================================================
# [Deployment — run in the Console, not inside app.R]
# ------------------------------------------------------------
# 1) Check that app.R and murders.csv are together in the folder
#    list.files("murders_Rshiny_app")
#
# 2) Local test: run the whole folder
#    shiny::runApp("C:/Users/jenny/Documents/Shiny_app/murders_Rshiny_app")
#
# 3) First time only: connect your shinyapps.io account token
#    # install.packages("rsconnect")
#    # shinyapps.io -> Account -> Tokens -> Show, then copy the command below
#    # rsconnect::setAccountInfo(name = "...", token = "...", secret = "...")
#    #   ^ never make the Token/Secret public
# 4) Deploy: upload the whole folder (app.R and murders.csv go up together)
#    # rsconnect::deployApp("murders_Rshiny_app")   # or the Publish button at the top right of RStudio
#    #   When it finishes, it opens at https://your-account.shinyapps.io/murders_Rshiny_app
# rsconnect::deployApp("")
# Checklist before deploying
#   [ ] Are app.R and murders.csv together in the folder?
#   [ ] Is the data read with a relative path ("murders.csv")? (no absolute C:/Users/... path)
#   [ ] Is there no install.packages() in app.R?
#   [ ] Are all packages you use loaded with library()?
#   [ ] Are there no files in the folder that must not be uploaded (personal data, etc.)? -> the whole folder goes to the server
# ============================================================


# Posit Cloud Connect
# install.packages("rsconnect")
# rsconnect::writeManifest()
