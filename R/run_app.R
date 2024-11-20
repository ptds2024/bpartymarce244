#' @title Run the Shiny Application
#' @description Launches the Shiny app that displays current weather data, forecasts, and simulations.
#' @param api_key Your OpenWeatherMap API key as a character string. If not provided, it will attempt to read from the environment variable \code{OPENWEATHERMAP_API_KEY}.
#' @return No return value. This function starts the Shiny application.
#' @examples
#' \dontrun{
#' run_app(api_key = "your_api_key")
#' }
#' @author MarcelaChoque
#' @export
run_app <- function(api_key = Sys.getenv("OPENWEATHERMAP_API_KEY")) {
  if (nchar(api_key) == 0) {
    stop("API key not found. Please set your OpenWeatherMap API key.")
  }

  # Define the user interface for the Shiny app
  ui <- shiny::fluidPage(
    shiny::titlePanel("Climatic factors today and in the next 5 days."),

    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::textInput("city", "City", value = "London"), # Input for city name
        shiny::selectInput("parameter", "Parameter",        # Dropdown for weather parameter selection
                           choices = c("Temperature", "Humidity", "Pressure")),
        shiny::actionButton("submit", "Submit"),            # Submit button to fetch weather data

        # Numeric input for number of simulations and action button
        shiny::numericInput("n_simulations", "Number of Simulations", value = 10000, min = 1),
        shiny::actionButton("run_simulations", "Run Simulations")     # Button to run simulations
      ),

      shiny::mainPanel(
        shiny::textOutput("current_weather"),               # Display current weather text output
        shiny::plotOutput("forecast_plot"),                 # Display plot for 5-day forecast

        # Output for volume summary and histogram
        shiny::textOutput("volume_summary_text"),           # Text for ice cream volume
        shiny::plotOutput("volume_histogram"),              # Histogram for total volume

        # Output for surface area summary and histogram
        shiny::textOutput("surface_summary_text"),          # Text for coating surface area
        shiny::plotOutput("surface_histogram")              # Histogram for total surface area
      )
    )
  )

  # Define server logic to process inputs and generate outputs
  server <- function(input, output, session) {

    # Event to validate city and retrieve weather data when "Submit" is clicked
    weather_data <- shiny::eventReactive(input$submit, {
      city <- input$city

      # Check if the entered city name is valid
      if (!check_city_validity(city, api_key)) {
        shiny::showModal(shiny::modalDialog(
          title = "Invalid City",
          paste("City", city, "not found. Please enter a valid city name."),
          easyClose = TRUE
        ))
        return(NULL)
      }

      # Fetch current weather data for the specified city
      current_weather <- jsonlite::fromJSON(httr::content(httr::GET(
        "http://api.openweathermap.org/data/2.5/weather",
        query = list(q = city, APPID = api_key, units = "metric")
      ), "text", encoding = "UTF-8"))

      # Fetch 5-day forecast data for the specified city
      forecast_data <- jsonlite::fromJSON(httr::content(httr::GET(
        "http://api.openweathermap.org/data/2.5/forecast",
        query = list(q = city, APPID = api_key, units = "metric")
      ), "text", encoding = "UTF-8"))

      list(current = current_weather, forecast = forecast_data)
    })

    # Display current weather information
    output$current_weather <- shiny::renderText({
      data <- weather_data()
      shiny::req(data)

      city <- input$city
      current <- data$current
      parameter <- input$parameter

      # Display message based on the selected parameter
      if (parameter == "Temperature") {
        paste("Welcome to", city, "! The Temperature in", city, "is", current$main$temp, "\u00b0C.")
      } else if (parameter == "Humidity") {
        paste("Welcome to", city, "! The Humidity in", city, "is", current$main$humidity, "%.")
      } else if (parameter == "Pressure") {
        paste("Welcome to", city, "! The Pressure in", city, "is", current$main$pressure, "hPa.")
      }
    })

    # Render plot for 5-day forecast based on the selected parameter
    output$forecast_plot <- shiny::renderPlot({
      data <- weather_data()
      shiny::req(data)

      forecast <- data$forecast$list

      # Convert forecast data into a data frame
      forecast_df <- data.frame(
        date_time = as.POSIXct(sapply(forecast, function(x) x$dt_txt), format="%Y-%m-%d %H:%M:%S", tz="UTC"),
        Temperature = sapply(forecast, function(x) x$main$temp),
        Humidity = sapply(forecast, function(x) x$main$humidity),
        Pressure = sapply(forecast, function(x) x$main$pressure)
      )

      # Call plot_forecast function with forecast data and user-selected parameter
      plot_forecast(forecast_df, input$parameter)
    })

    # Observe event for running simulations
    shiny::observeEvent(input$run_simulations, {

      # Basic validation of the number of simulations
      n_simulations <- as.integer(input$n_simulations)
      if (is.na(n_simulations) || n_simulations <= 0) {
        shiny::showModal(shiny::modalDialog(
          title = "Invalid Input",
          "Please enter a positive integer for the number of simulations.",
          easyClose = TRUE
        ))
        return(NULL)
      }

      # Get the current weather data
      data <- weather_data()
      shiny::req(data)

      current <- data$current
      temperature <- current$main$temp            # Temperature in Celsius
      humidity <- current$main$humidity / 100     # Humidity as a decimal (percentage / 100)
      pressure <- current$main$pressure           # Pressure in hPa

      # Calculate lambda for the Poisson distribution
      lambda <- exp(0.5 + 0.5 * temperature - 3 * humidity + 0.001 * pressure)

      # Run the simulations
      sim_results <- replicate(n_simulations, simulate_party(lambda))

      # Convert results into a data frame
      sim_results_df <- data.frame(volume = sim_results[1, ], surface = sim_results[2, ])

      # Calculate the 99th percentile for volume and surface area
      volume_99 <- stats::quantile(sim_results_df$volume, 0.99)
      surface_99 <- stats::quantile(sim_results_df$surface, 0.99)

      # Render the volume summary text
      output$volume_summary_text <- shiny::renderText({
        paste0(
          "Ice Cream Volume: To satisfy the guests with a 99% chance, ",
          "we will need approximately ", format(round(volume_99, 2), big.mark = ","), " cm\u00b3 of ice cream."
        )
      })

      # Generate and display the histogram of total volume
      output$volume_histogram <- shiny::renderPlot({
        hist(sim_results_df$volume,
             main = "Histogram of Total Volume",
             xlab = "Total Volume (cm\u00b3)",
             col = "#E28B55",
             border = "white")
      })

      # Render the surface area summary text
      output$surface_summary_text <- shiny::renderText({
        paste0(
          "Coating Surface Area: To coat the cones with a 99% chance of sufficiency, ",
          "we will need approximately ", format(round(surface_99, 2), big.mark = ","), " cm\u00b2 of coating material."
        )
      })

      # Generate and display the histogram of total surface area
      output$surface_histogram <- shiny::renderPlot({
        hist(sim_results_df$surface,
             main = "Histogram of Total Surface Area",
             xlab = "Total Surface Area (cm\u00b2)",
             col = "#DCB64D",
             border = "white")
      })
    })
  }

  # Run the Shiny app
  shiny::shinyApp(ui, server)
}
