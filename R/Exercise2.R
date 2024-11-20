# Set the OpenWeatherMap API key
api_key <- "2e24e9882da038c0d423b67af00ddaa4"
if (nchar(api_key) == 0) {
  stop("API key not found. Please set the OWM_API_KEY in .Renviron file.")
}

# Function to verify if a city is valid in OpenWeatherMap's dataset
check_city_validity <- function(city_name, api_key) {
  response <- GET(
    "http://api.openweathermap.org/data/2.5/weather",
    query = list(q = city_name, appid = api_key)
  )
  status <- status_code(response)
  return(status == 200)
}

# Function to create dynamic forecast plots
plot.forecast <- function(forecast_df, parameter) {
  ggplot(forecast_df, aes(x = date_time, y = !!sym(parameter))) +
    geom_line(color = "#439EB7", size = 1) +
    labs(
      title = paste("5-Day", parameter, "Forecast"),
      x = "Date and Time",
      y = parameter
    ) +
    theme_minimal()
}

# Define the user interface for the Shiny app
ui <- fluidPage(
  titlePanel("Climatic factors today and in the next 5 days for Multiple Locations"),

  # Create a tabset panel to separate each location's inputs and outputs
  tabsetPanel(
    # Location 1
    tabPanel("Location 1",
             sidebarLayout(
               sidebarPanel(
                 textInput("city_1", "City", value = "London"), # Input for city name
                 selectInput("parameter_1", "Parameter",        # Dropdown for weather parameter selection
                             choices = c("Temperature", "Humidity", "Pressure")),
                 actionButton("submit_1", "Submit"),            # Submit button to fetch weather data

                 numericInput("n_simulations_1", "Number of Simulations", value = 10000, min = 1),
                 actionButton("run_simulations_1", "Run Simulations")     # Button to run simulations
               ),

               mainPanel(
                 textOutput("current_weather_1"),               # Display current weather text output
                 plotOutput("forecast_plot_1"),                 # Display plot for 5-day forecast

                 textOutput("volume_summary_text_1"),           # Text for ice cream volume
                 plotOutput("volume_histogram_1"),              # Histogram for total volume

                 textOutput("surface_summary_text_1"),          # Text for coating surface area
                 plotOutput("surface_histogram_1")              # Histogram for total surface area
               )
             )
    ),

    # Location 2
    tabPanel("Location 2",
             sidebarLayout(
               sidebarPanel(
                 textInput("city_2", "City", value = "New York"), # Input for city name
                 selectInput("parameter_2", "Parameter",          # Dropdown for weather parameter selection
                             choices = c("Temperature", "Humidity", "Pressure")),
                 actionButton("submit_2", "Submit"),              # Submit button to fetch weather data

                 numericInput("n_simulations_2", "Number of Simulations", value = 10000, min = 1),
                 actionButton("run_simulations_2", "Run Simulations")     # Button to run simulations
               ),

               mainPanel(
                 textOutput("current_weather_2"),               # Display current weather text output
                 plotOutput("forecast_plot_2"),                 # Display plot for 5-day forecast

                 textOutput("volume_summary_text_2"),           # Text for ice cream volume
                 plotOutput("volume_histogram_2"),              # Histogram for total volume

                 textOutput("surface_summary_text_2"),          # Text for coating surface area
                 plotOutput("surface_histogram_2")              # Histogram for total surface area
               )
             )
    ),

    # Location 3
    tabPanel("Location 3",
             sidebarLayout(
               sidebarPanel(
                 textInput("city_3", "City", value = "Tokyo"),    # Input for city name
                 selectInput("parameter_3", "Parameter",          # Dropdown for weather parameter selection
                             choices = c("Temperature", "Humidity", "Pressure")),
                 actionButton("submit_3", "Submit"),              # Submit button to fetch weather data

                 numericInput("n_simulations_3", "Number of Simulations", value = 10000, min = 1),
                 actionButton("run_simulations_3", "Run Simulations")     # Button to run simulations
               ),

               mainPanel(
                 textOutput("current_weather_3"),               # Display current weather text output
                 plotOutput("forecast_plot_3"),                 # Display plot for 5-day forecast

                 textOutput("volume_summary_text_3"),           # Text for ice cream volume
                 plotOutput("volume_histogram_3"),              # Histogram for total volume

                 textOutput("surface_summary_text_3"),          # Text for coating surface area
                 plotOutput("surface_histogram_3")              # Histogram for total surface area
               )
             )
    )
  )
)

# Define server logic to process inputs and generate outputs
server <- function(input, output, session) {

  # ========== Location 1 ==========
  # Event to validate city and retrieve weather data when "Submit" is clicked
  weather_data_1 <- eventReactive(input$submit_1, {
    city <- input$city_1

    # Check if the entered city name is valid
    if (!check_city_validity(city, api_key)) {
      showModal(modalDialog(
        title = "Invalid City",
        paste("City", city, "not found. Please enter a valid city name."),
        easyClose = TRUE
      ))
      return(NULL)
    }

    # Fetch current weather data for the specified city
    current_weather <- fromJSON(content(GET(
      "http://api.openweathermap.org/data/2.5/weather",
      query = list(q = city, APPID = api_key, units = "metric")
    ), "text", encoding = "UTF-8"))

    # Fetch 5-day forecast data for the specified city
    forecast_data <- fromJSON(content(GET(
      "http://api.openweathermap.org/data/2.5/forecast",
      query = list(q = city, APPID = api_key, units = "metric")
    ), "text", encoding = "UTF-8"))

    list(current = current_weather, forecast = forecast_data)
  })

  # Display current weather information
  output$current_weather_1 <- renderText({
    data <- weather_data_1()
    req(data)

    city <- input$city_1
    current <- data$current
    parameter <- input$parameter_1

    # Display message based on the selected parameter
    if (parameter == "Temperature") {
      paste("Welcome to", city, "! The Temperature in", city, "is", current$main$temp, "°C.")
    } else if (parameter == "Humidity") {
      paste("Welcome to", city, "! The Humidity in", city, "is", current$main$humidity, "%.")
    } else if (parameter == "Pressure") {
      paste("Welcome to", city, "! The Pressure in", city, "is", current$main$pressure, "hPa.")
    }
  })

  # Render plot for 5-day forecast based on the selected parameter
  output$forecast_plot_1 <- renderPlot({
    data <- weather_data_1()
    req(data)

    forecast <- data$forecast$list

    # Convert forecast data into a data frame with DateTime and parameter columns (exactly as in Work 5)
    forecast_df <- data.frame(
      date_time = as.POSIXct(forecast$dt_txt, format="%Y-%m-%d %H:%M:%S", tz="UTC"),
      Temperature = forecast$main$temp,
      Humidity = forecast$main$humidity,
      Pressure = forecast$main$pressure
    )

    # Call plot function with forecast data and user-selected parameter
    plot.forecast(forecast_df, input$parameter_1)
  })

  # Observe event for running simulations
  observeEvent(input$run_simulations_1, {

    # Basic validation of the number of simulations
    n_simulations <- as.integer(input$n_simulations_1)
    if (is.na(n_simulations) || n_simulations <= 0) {
      showModal(modalDialog(
        title = "Invalid Input",
        "Please enter a positive integer for the number of simulations.",
        easyClose = TRUE
      ))
      return(NULL)
    }

    # Get the current weather data
    data <- weather_data_1()
    req(data)

    current <- data$current
    temperature <- current$main$temp            # Temperature in Celsius
    humidity <- current$main$humidity / 100     # Humidity as a decimal (percentage / 100)
    pressure <- current$main$pressure           # Pressure in hPa

    # Calculate lambda for the Poisson distribution
    lambda <- exp(0.5 + 0.5 * temperature - 3 * humidity + 0.001 * pressure)

    # Define the simulation function
    simulate_party <- function() {
      # Simulate number of guests
      n_guests <- rpois(1, lambda)

      # Simulate number of cones per guest (1 or 2)
      cones_per_guest <- sample(c(1, 2), n_guests, replace = TRUE, prob = c(0.67, 0.33))
      total_cones <- sum(cones_per_guest)

      # Random variations in cone dimensions
      h_variation <- function(x) {
        x + rnorm(1, mean = 0, sd = 0.1)
      }

      # Calculate total volume and surface area for all cones
      volume_integrand <- function(x) h_variation(x)^2
      surface_integrand <- function(x) {
        h <- h_variation(x)
        dh_dx <- (h_variation(x + 1e-5) - h_variation(x)) / 1e-5
        h * sqrt(1 + dh_dx^2)
      }

      total_volume <- total_cones * pi * integrate(volume_integrand, lower = 0, upper = 10)$value
      total_surface <- total_cones * 2 * pi * integrate(surface_integrand, lower = 0, upper = 10)$value

      c(volume = total_volume, surface = total_surface)
    }

    # Run the simulations
    sim_results <- replicate(n_simulations, simulate_party())

    # Convert results into a data frame
    sim_results_df <- data.frame(volume = sim_results[1, ], surface = sim_results[2, ])

    # Calculate the 99th percentile for volume and surface area
    volume_99 <- quantile(sim_results_df$volume, 0.99)
    surface_99 <- quantile(sim_results_df$surface, 0.99)

    # Render the volume summary text
    output$volume_summary_text_1 <- renderText({
      paste0(
        "Ice Cream Volume: To satisfy the guests with a 99% chance, ",
        "we will need approximately ", format(round(volume_99, 2), big.mark = ","), " cm³ of ice cream."
      )
    })

    # Generate and display the histogram of total volume
    output$volume_histogram_1 <- renderPlot({
      hist(sim_results_df$volume,
           main = "Histogram of Total Volume",
           xlab = "Total Volume (cm³)",
           col = "#E28B55",
           border = "white")
    })

    # Render the surface area summary text
    output$surface_summary_text_1 <- renderText({
      paste0(
        "Coating Surface Area: To coat the cones with a 99% chance of sufficiency, ",
        "we will need approximately ", format(round(surface_99, 2), big.mark = ","), " cm² of coating material."
      )
    })

    # Generate and display the histogram of total surface area
    output$surface_histogram_1 <- renderPlot({
      hist(sim_results_df$surface,
           main = "Histogram of Total Surface Area",
           xlab = "Total Surface Area (cm²)",
           col = "#DCB64D",
           border = "white")
    })
  })

  # ========== Location 2 ==========
  # Event to validate city and retrieve weather data when "Submit" is clicked
  weather_data_2 <- eventReactive(input$submit_2, {
    city <- input$city_2

    # Check if the entered city name is valid
    if (!check_city_validity(city, api_key)) {
      showModal(modalDialog(
        title = "Invalid City",
        paste("City", city, "not found. Please enter a valid city name."),
        easyClose = TRUE
      ))
      return(NULL)
    }

    # Fetch current weather data for the specified city
    current_weather <- fromJSON(content(GET(
      "http://api.openweathermap.org/data/2.5/weather",
      query = list(q = city, APPID = api_key, units = "metric")
    ), "text", encoding = "UTF-8"))

    # Fetch 5-day forecast data for the specified city
    forecast_data <- fromJSON(content(GET(
      "http://api.openweathermap.org/data/2.5/forecast",
      query = list(q = city, APPID = api_key, units = "metric")
    ), "text", encoding = "UTF-8"))

    list(current = current_weather, forecast = forecast_data)
  })

  # Display current weather information
  output$current_weather_2 <- renderText({
    data <- weather_data_2()
    req(data)

    city <- input$city_2
    current <- data$current
    parameter <- input$parameter_2

    # Display message based on the selected parameter
    if (parameter == "Temperature") {
      paste("Welcome to", city, "! The Temperature in", city, "is", current$main$temp, "°C.")
    } else if (parameter == "Humidity") {
      paste("Welcome to", city, "! The Humidity in", city, "is", current$main$humidity, "%.")
    } else if (parameter == "Pressure") {
      paste("Welcome to", city, "! The Pressure in", city, "is", current$main$pressure, "hPa.")
    }
  })

  # Render plot for 5-day forecast based on the selected parameter
  output$forecast_plot_2 <- renderPlot({
    data <- weather_data_2()
    req(data)

    forecast <- data$forecast$list

    # Convert forecast data into a data frame with DateTime and parameter columns (exactly as in Work 5)
    forecast_df <- data.frame(
      date_time = as.POSIXct(forecast$dt_txt, format="%Y-%m-%d %H:%M:%S", tz="UTC"),
      Temperature = forecast$main$temp,
      Humidity = forecast$main$humidity,
      Pressure = forecast$main$pressure
    )

    # Call plot function with forecast data and user-selected parameter
    plot.forecast(forecast_df, input$parameter_2)
  })

  # Observe event for running simulations
  observeEvent(input$run_simulations_2, {

    # Basic validation of the number of simulations
    n_simulations <- as.integer(input$n_simulations_2)
    if (is.na(n_simulations) || n_simulations <= 0) {
      showModal(modalDialog(
        title = "Invalid Input",
        "Please enter a positive integer for the number of simulations.",
        easyClose = TRUE
      ))
      return(NULL)
    }

    # Get the current weather data
    data <- weather_data_2()
    req(data)

    current <- data$current
    temperature <- current$main$temp            # Temperature in Celsius
    humidity <- current$main$humidity / 100     # Humidity as a decimal (percentage / 100)
    pressure <- current$main$pressure           # Pressure in hPa

    # Calculate lambda for the Poisson distribution
    lambda <- exp(0.5 + 0.5 * temperature - 3 * humidity + 0.001 * pressure)

    # Define the simulation function
    simulate_party <- function() {
      # Simulate number of guests
      n_guests <- rpois(1, lambda)

      # Simulate number of cones per guest (1 or 2)
      cones_per_guest <- sample(c(1, 2), n_guests, replace = TRUE, prob = c(0.67, 0.33))
      total_cones <- sum(cones_per_guest)

      # Random variations in cone dimensions
      h_variation <- function(x) {
        x + rnorm(1, mean = 0, sd = 0.1)
      }

      # Calculate total volume and surface area for all cones
      volume_integrand <- function(x) h_variation(x)^2
      surface_integrand <- function(x) {
        h <- h_variation(x)
        dh_dx <- (h_variation(x + 1e-5) - h_variation(x)) / 1e-5
        h * sqrt(1 + dh_dx^2)
      }

      total_volume <- total_cones * pi * integrate(volume_integrand, lower = 0, upper = 10)$value
      total_surface <- total_cones * 2 * pi * integrate(surface_integrand, lower = 0, upper = 10)$value

      c(volume = total_volume, surface = total_surface)
    }

    # Run the simulations
    sim_results <- replicate(n_simulations, simulate_party())

    # Convert results into a data frame
    sim_results_df <- data.frame(volume = sim_results[1, ], surface = sim_results[2, ])

    # Calculate the 99th percentile for volume and surface area
    volume_99 <- quantile(sim_results_df$volume, 0.99)
    surface_99 <- quantile(sim_results_df$surface, 0.99)

    # Render the volume summary text
    output$volume_summary_text_2 <- renderText({
      paste0(
        "Ice Cream Volume: To satisfy the guests with a 99% chance, ",
        "we will need approximately ", format(round(volume_99, 2), big.mark = ","), " cm³ of ice cream."
      )
    })

    # Generate and display the histogram of total volume
    output$volume_histogram_2 <- renderPlot({
      hist(sim_results_df$volume,
           main = "Histogram of Total Volume",
           xlab = "Total Volume (cm³)",
           col = "#E28B55",
           border = "white")
    })

    # Render the surface area summary text
    output$surface_summary_text_2 <- renderText({
      paste0(
        "Coating Surface Area: To coat the cones with a 99% chance of sufficiency, ",
        "we will need approximately ", format(round(surface_99, 2), big.mark = ","), " cm² of coating material."
      )
    })

    # Generate and display the histogram of total surface area
    output$surface_histogram_2 <- renderPlot({
      hist(sim_results_df$surface,
           main = "Histogram of Total Surface Area",
           xlab = "Total Surface Area (cm²)",
           col = "#DCB64D",
           border = "white")
    })
  })

  # ========== Location 3 ==========
  # Event to validate city and retrieve weather data when "Submit" is clicked
  weather_data_3 <- eventReactive(input$submit_3, {
    city <- input$city_3

    # Check if the entered city name is valid
    if (!check_city_validity(city, api_key)) {
      showModal(modalDialog(
        title = "Invalid City",
        paste("City", city, "not found. Please enter a valid city name."),
        easyClose = TRUE
      ))
      return(NULL)
    }

    # Fetch current weather data for the specified city
    current_weather <- fromJSON(content(GET(
      "http://api.openweathermap.org/data/2.5/weather",
      query = list(q = city, APPID = api_key, units = "metric")
    ), "text", encoding = "UTF-8"))

    # Fetch 5-day forecast data for the specified city
    forecast_data <- fromJSON(content(GET(
      "http://api.openweathermap.org/data/2.5/forecast",
      query = list(q = city, APPID = api_key, units = "metric")
    ), "text", encoding = "UTF-8"))

    list(current = current_weather, forecast = forecast_data)
  })

  # Display current weather information
  output$current_weather_3 <- renderText({
    data <- weather_data_3()
    req(data)

    city <- input$city_3
    current <- data$current
    parameter <- input$parameter_3

    # Display message based on the selected parameter
    if (parameter == "Temperature") {
      paste("Welcome to", city, "! The Temperature in", city, "is", current$main$temp, "°C.")
    } else if (parameter == "Humidity") {
      paste("Welcome to", city, "! The Humidity in", city, "is", current$main$humidity, "%.")
    } else if (parameter == "Pressure") {
      paste("Welcome to", city, "! The Pressure in", city, "is", current$main$pressure, "hPa.")
    }
  })

  # Render plot for 5-day forecast based on the selected parameter
  output$forecast_plot_3 <- renderPlot({
    data <- weather_data_3()
    req(data)

    forecast <- data$forecast$list

    # Convert forecast data into a data frame with DateTime and parameter columns (exactly as in Work 5)
    forecast_df <- data.frame(
      date_time = as.POSIXct(forecast$dt_txt, format="%Y-%m-%d %H:%M:%S", tz="UTC"),
      Temperature = forecast$main$temp,
      Humidity = forecast$main$humidity,
      Pressure = forecast$main$pressure
    )

    # Call plot function with forecast data and user-selected parameter
    plot.forecast(forecast_df, input$parameter_3)
  })

  # Observe event for running simulations
  observeEvent(input$run_simulations_3, {

    # Basic validation of the number of simulations
    n_simulations <- as.integer(input$n_simulations_3)
    if (is.na(n_simulations) || n_simulations <= 0) {
      showModal(modalDialog(
        title = "Invalid Input",
        "Please enter a positive integer for the number of simulations.",
        easyClose = TRUE
      ))
      return(NULL)
    }

    # Get the current weather data
    data <- weather_data_3()
    req(data)

    current <- data$current
    temperature <- current$main$temp            # Temperature in Celsius
    humidity <- current$main$humidity / 100     # Humidity as a decimal (percentage / 100)
    pressure <- current$main$pressure           # Pressure in hPa

    # Calculate lambda for the Poisson distribution
    lambda <- exp(0.5 + 0.5 * temperature - 3 * humidity + 0.001 * pressure)

    # Define the simulation function
    simulate_party <- function() {
      # Simulate number of guests
      n_guests <- rpois(1, lambda)

      # Simulate number of cones per guest (1 or 2)
      cones_per_guest <- sample(c(1, 2), n_guests, replace = TRUE, prob = c(0.67, 0.33))
      total_cones <- sum(cones_per_guest)

      # Random variations in cone dimensions
      h_variation <- function(x) {
        x + rnorm(1, mean = 0, sd = 0.1)
      }

      # Calculate total volume and surface area for all cones
      volume_integrand <- function(x) h_variation(x)^2
      surface_integrand <- function(x) {
        h <- h_variation(x)
        dh_dx <- (h_variation(x + 1e-5) - h_variation(x)) / 1e-5
        h * sqrt(1 + dh_dx^2)
      }

      total_volume <- total_cones * pi * integrate(volume_integrand, lower = 0, upper = 10)$value
      total_surface <- total_cones * 2 * pi * integrate(surface_integrand, lower = 0, upper = 10)$value

      c(volume = total_volume, surface = total_surface)
    }

    # Run the simulations
    sim_results <- replicate(n_simulations, simulate_party())

    # Convert results into a data frame
    sim_results_df <- data.frame(volume = sim_results[1, ], surface = sim_results[2, ])

    # Calculate the 99th percentile for volume and surface area
    volume_99 <- quantile(sim_results_df$volume, 0.99)
    surface_99 <- quantile(sim_results_df$surface, 0.99)

    # Render the volume summary text
    output$volume_summary_text_3 <- renderText({
      paste0(
        "Ice Cream Volume: To satisfy the guests with a 99% chance, ",
        "we will need approximately ", format(round(volume_99, 2), big.mark = ","), " cm³ of ice cream."
      )
    })

    # Generate and display the histogram of total volume
    output$volume_histogram_3 <- renderPlot({
      hist(sim_results_df$volume,
           main = "Histogram of Total Volume",
           xlab = "Total Volume (cm³)",
           col = "#E28B55",
           border = "white")
    })

    # Render the surface area summary text
    output$surface_summary_text_3 <- renderText({
      paste0(
        "Coating Surface Area: To coat the cones with a 99% chance of sufficiency, ",
        "we will need approximately ", format(round(surface_99, 2), big.mark = ","), " cm² of coating material."
      )
    })

    # Generate and display the histogram of total surface area
    output$surface_histogram_3 <- renderPlot({
      hist(sim_results_df$surface,
           main = "Histogram of Total Surface Area",
           xlab = "Total Surface Area (cm²)",
           col = "#DCB64D",
           border = "white")
    })
  })
}

# Run the Shiny app
shinyApp(ui, server)
