
# Loading Libraries ############################################################
library(tidyverse)
library(plotly)
library(here)
library(janitor) 
library(scales)
library(colorspace)
library(shinyWidgets)
library(DT)
library(fixest)
library(modelsummary)
library(huxtable)
library(jsonlite)
library(sf) 
library(leaflet) 
library(ggplot2)  
library(scales)  
library(colorspace)  
library(GGally) 
library(car)  
library(DescTools)  
library(corrplot) 
library(caret) 
library(moments)  
library(shiny)
library(shinythemes)
library(bslib)
# Data #########################################################################
here()

df_cleaned <- df_1 <- read_csv(here("Data/listings.csv"))

# Cleaning #####################################################################

unwanted_columns <- c("listing_url", "scrape_id", "source", "name", "picture_url", "host_url", "host_name", "host_thumbnail_url", "host_picture_url", "host_neighbourhood", "host_verifications", "neighbourhood", "neighbourhood_group_cleansed", "bathrooms", "beds", "minimum_minimum_nights", "maximum_minimum_nights", "minimum_maximum_nights", "maximum_maximum_nights", "minimum_nights_avg_ntm", "maximum_nights_avg_ntm", "calendar_updated", "number_of_reviews_l30d", "calculated_host_listings_count_entire_homes", "calculated_host_listings_count_private_rooms", "calculated_host_listings_count_shared_rooms", "first_review", "maximum_nights", "minimum_nights")
df_cleaned <- df_1 %>%
  select(-unwanted_columns)

na <- c("host_response_rate", "host_response_time", "host_acceptance_rate")

df_cleaned <- df_cleaned %>%
  mutate(
    across(
      na,
      ~ case_when(
        .x == 'N/A' ~ NA_character_,
        .default = .x
      )
    )
  )

df_cleaned$host_response_rate <- str_remove(df_cleaned$host_response_rate, pattern = "%")
df_cleaned$host_acceptance_rate <- str_remove(df_cleaned$host_acceptance_rate, pattern = "%")
df_cleaned$price<-str_remove(df_cleaned$price, pattern = "\\$")

df_cleaned$price <- gsub(",", "", df_cleaned$price)

df_cleaned$bathrooms_text <- gsub("[^0-9.]","",df_cleaned$bathrooms_text)

df_cleaned$host_response_rate <- as.numeric(df_cleaned$host_response_rate)
df_cleaned$host_acceptance_rate <- as.numeric(df_cleaned$host_acceptance_rate)
df_cleaned$price <- as.numeric(df_cleaned$price)

df_cleaned$host_since <- format(df_cleaned$host_since, "%Y")

# According to dataset (in Paris)
df_3 <- df_cleaned %>%
  count(host_id, name = "total_listings_Paris") %>%
  arrange(desc(total_listings_Paris))

#According to Airbnb calculations
df_4 <- df_cleaned %>%
  select(host_id,host_total_listings_count) %>%
  group_by(host_id)%>%
  arrange(desc(host_total_listings_count)) %>%
  slice(host_id, 1)

df_5 <- df_cleaned %>%
  select(host_id, host_listings_count) %>%
  group_by(host_id) %>%
  arrange(desc(host_listings_count)) %>%
  slice(host_id,1)

df_6 <- df_3 %>%
  left_join(df_4,
            by="host_id")

df_6 <- df_6 %>%
  left_join(df_5,
            by = "host_id")

df_cleaned %>% 
  group_by(host_id)%>%
  filter(host_id%in%c(33889201,314994947)) %>%
  select("host_about")%>%
  slice(host_id, 1)

df_cleaned <- df_cleaned %>%
  select(-host_listings_count)

df_cleaned <- df_cleaned %>%
  left_join(df_3,
            by = "host_id")

keywords <- c("tv", "wifi", "kitchen", "air conditioning", "parking", "pool", "washer", "dryer", "heating", "pets", "smoke alarm", "elevator")

df_cleaned <- df_cleaned %>%
  mutate(amenities_list = map(amenities, ~ fromJSON(.) %>% unlist() %>% tolower()))

for (keyword in keywords) {
  df_cleaned <- df_cleaned %>%
    mutate(!!paste0("has_", keyword) := map_lgl(amenities_list, ~ any(str_detect(.x, keyword))))
}

df_amenity <- df_cleaned %>%
  select(id, starts_with("has_"))  # Keep only the keyword indicator columns

missing_ids <- setdiff(df_1$id, df_amenity$id)

df_amenity <- df_amenity %>%
  select(-has_availability)

if (length(missing_ids) == 0) {
  print("All IDs from df_cleaned are present in df_amenity.")
} else {
  print("The following IDs are missing in df_amenity:")
  print(missing_ids)
}

unique_host_locations <- unique(df_cleaned$host_location)

df_cleaned <- df_cleaned %>%
  mutate(
    host_fr_other = ifelse(
      str_detect(host_location, "France"), "France", "Other"
    )
  )

df_cleaned <- df_cleaned%>%
  select(-c("amenities", "amenities_list"))


################################################################################
### SHINY Dashboard ############################################################
################################################################################


# User interface ###############################################################
ui <- page_navbar(
  title = "Paris Airbnb Data Exploration",
  
  ## PAGE 1
  nav_panel(title = "Introduction",
            
     h1("Paris Airbnb Market Analysis Dashboard"),
     
     h2("Overview"),
     p("This interactive dashboard provides an in-depth analysis of Airbnb listings in Paris, exploring key factors that influence pricing, availability, and host performance. Using various data visualizations and statistical models, the dashboard highlights trends in the Paris Airbnb market, examining host characteristics, listing attributes, and regulatory impacts such as licensing status."),
     
     h2("Key Questions Explored in this Dashboard"),
     tags$ul(
       tags$li("What factors influence the price of an Airbnb listing in Paris?"),
       tags$li("How does availability and price vary across different neighborhoods?"),
       tags$li("What is the relationship between host characteristics and listing performance?"),
       tags$li("Do licensed listings tend to have higher prices and better reviews?"),
       tags$li("How do property type and amenities affect listing prices?")
     ),
     
     h2("How to Use This Dashboard"),
     tags$ul(
       tags$li("Navigate between different panels using the tabs at the top."),
       tags$li("Explore maps to visualize the geographical distribution of prices and availability."),
       tags$li("Use interactive charts to analyze trends across different listing attributes."),
       tags$li("View regression results to understand the key drivers of price and demand."),
       tags$li("Examine host characteristics and their impact on listing performance.")
     ),
     
     h2("Dashboard Features"),
     tags$ul(
       tags$li(strong("Price & Availability Analysis:"), " Understand how price and availability vary across neighborhoods."),
       tags$li(strong("Host Performance Metrics:"), " Explore factors such as Superhost status, response rate, and experience."),
       tags$li(strong("Property Type & Amenities:"), " Examine how different property types and amenities influence pricing."),
       tags$li(strong("Regulatory Impact:"), " Analyze the effect of licensing on Airbnb prices and review scores."),
       tags$li(strong("Regression Analysis:"), " Use econometric models to determine key price drivers."),
       tags$li(strong("Data Explorer:"), " Browse and filter Airbnb listings based on various attributes.")
     ),
     
     h2("Why This Matters"),
     p("The Paris Airbnb market plays a crucial role in the city's tourism and housing sectors. Understanding the dynamics of pricing, availability, and host behavior helps policymakers, investors, and travelers make informed decisions. This dashboard leverages data analytics to uncover key insights into the Airbnb ecosystem in Paris, offering a data-driven perspective on urban short-term rentals."),
     
     hr()
  ),
  
  ## Page 2
  nav_panel(
    title = "Distribution Analysis",
    h1("Distribution of Numeric Variables"),
    
    # Dropdown to select variable for histogram
    selectInput(
      inputId = "selected_var",
      label = "Select Variable:",
      choices = names(df_cleaned %>% select_if(is.numeric) %>% select(-c(id, host_id, latitude, longitude))),
      selected = "price"
    ),
    
    br(),  # Spacing
    
    # Row for Graph & Description Side by Side
    fluidRow(
      column(
        width = 8,  # 8 out of 12 columns for the plot
        plotOutput(outputId = "dist_plot", height = "500px", width = "100%")  # Ensure the plot has space
      ),
      column(
        width = 4,  # 4 out of 12 columns for the description
        wellPanel(htmlOutput("variable_description"))  # Styled description panel
      )
    ),
    
    br(),  # Spacing
    
    # Summary Table of 'accommodates' variable
    h3("Summary of Accommodates"),
    tableOutput(outputId = "accommodates_summary")
  )
  ,
  
  ## PAGE 3
  nav_panel(
    title = "Paris Availability Map",
    h1("Availability of Listings in Paris by Neighborhood"),
    
    # Leaflet interactive map
    leafletOutput(outputId = "paris_map", height = "600px"),
    
    layout_column_wrap(
      width = 1/2,
      
      # Dropdown for selecting time period (30, 60, 90, 365 days)
      selectInput(
        inputId = "availability_period",
        label = "Select Availability Period",
        choices = c("Availability 30 Days" = "avg_availability_30",
                    "Availability 60 Days" = "avg_availability_60",
                    "Availability 90 Days" = "avg_availability_90",
                    "Availability 365 Days" = "avg_availability_365"),
        selected = "avg_availability_30"
      )
    )
  ),
  
  ##PAGE 4
  nav_panel(
    title = "Paris Price Map",
    h1("Average Price of Listings in Paris by Neighborhood"),
    
    # Leaflet interactive map for price
    leafletOutput(outputId = "paris_price_map", height = "600px")  # Correct output ID
  ),
  # Space in header
  nav_spacer(),
  
  # Theme settings
  theme = bs_theme(
    bootswatch = "zephyr"
  ),
  
  ##PAGE 5
  nav_panel(
    title = "Factors Influencing Price",
    h1("What Factors Influence the Price of an Airbnb Listing in Paris?"),
    
    # Tabs for different analyses
    tabsetPanel(
      
      # Room Type & Property Type Analysis
      tabPanel("Room Type & Property Type",
               fluidRow(
                 column(6, plotOutput("plot_room_type", height = "400px")),  
                 column(6, plotOutput("plot_property_type_low", height = "400px"))
               ),
               br(),
               fluidRow(
                 column(12, plotOutput("plot_property_type_high", height = "400px"))
               ),
               br(),
               h4("Room Type:"),
               p("This chart highlights the variation in average prices across different room types. Hotel rooms have the highest average price, followed by entire home/apartments, private rooms, and shared rooms. The significant price gap between hotel rooms and shared rooms suggests that room type heavily influences pricing, with shared rooms being the most budget-friendly option."),
               h4("Property Type (Below 500):"),
               p("This chart shows the distribution of average prices for property types where the price is below 500. It reveals that specific property types, such as Casa particular and Entire serviced apartments, tend to have higher average prices compared to others, such as <b>Shared room in a hostel. The ordering emphasizes that even within the below-500 price range, there is considerable variability based on property type."),
               h4("Property Type (Above 500):"),
               p("This chart illustrates average prices for property types exceeding 500. Entire villas and towers dominate the higher price range, reflecting the premium associated with luxury and exclusivity. Other unique properties, such as castles and houseboats, also fetch high average prices, indicating their niche market appeal. This chart underscores that specialized accommodations command significantly higher rates. ")
      ),
      
      # Neighborhood Effect
      tabPanel("Neighborhood Effect",
               fluidRow(
                 column(12, plotOutput("plot_neighborhood", height = "500px"))
               ),
               br(),
               h4("Neighborhood:"),
               p("This bar chart displays the average price of accommodations across different neighborhoods. The neighborhoods are ordered from the lowest to the highest average price. Notable patterns emerge, with neighborhoods like Ménilmontant and Buttes-Montmartre showing the lowest average prices, while upscale areas such as Palais-Bourbon and Élysée exhibit significantly higher average prices. This highlights substantial pricing variation influenced by the neighborhood, suggesting that location plays a critical role in determining accommodation costs.")
      ),
      
      # Size & Capacity Analysis
      tabPanel("Size & Capacity",
               fluidRow(
                 column(6, plotOutput("plot_accommodates", height = "400px")),
                 column(6, plotOutput("plot_bedrooms", height = "400px"))
               ),
               br(),
               h4("Number of People Accommodated:"),
               p("The graph shows a clear positive relationship between the number of people a property can accommodate and its average price. Properties that accommodate 1–4 people tend to have a relatively moderate average price (ranging up to €600). As the accommodation capacity increases, the price rises significantly, with properties accommodating 15 people reaching average prices above €1,600. This trend indicates that larger accommodations are associated with higher prices, likely reflecting the increased size, amenities, and demand for group bookings."),
               h4("Number of Bedrooms:"),
               p("This graph highlights a positive correlation between the number of bedrooms in a property and its average price. Properties with 1–4 bedrooms have moderate average prices (€500–€1,500). However, for 7 bedrooms, the average price spikes dramatically, with some exceeding €2,000. Properties with unusually high bedroom counts (e.g., 38) show lower average prices, possibly reflecting outliers or unconventional property types. This trend demonstrates that properties with more bedrooms command higher prices, likely due to their ability to accommodate larger groups or offer more luxury. ")
      ),
      
      # Amenities Analysis
      tabPanel("Amenities",
               fluidRow(
                 column(12, plotOutput("plot_amenities", height = "500px"))
               ),
               br(),
               h4("Amenities:"),
               p("The relationship between the number of amenities and average price reflects the interplay between property type and the amenities offered. Hotel rooms, often equipped with fewer but essential amenities like a TV and elevator access, can command high prices due to convenience and location, similar to fully equipped houses with features like a kitchen, washer, and parking. Mid-range properties follow a linear pattern where prices steadily increase with the number of amenities, reflecting the added value for guests. This progression highlights how both basic, high-convenience properties and fully equipped ones can justify premium pricing, while mid-tier properties show a more direct correlation between price and the number of amenities provided.")
      )
    )
  ),
  ##PAGE 6
  
  nav_panel(
    title = "Host Characteristics & Listing Performance",
    h1("What is the Relationship Between Host Characteristics and Listing Performance?"),
    
    # Tabs for different analyses
    tabsetPanel(
      
      # Superhost & Ratings Analysis
      tabPanel("Superhost & Ratings",
               fluidRow(
                 column(12, plotOutput("plot_superhost_ratings", height = "500px"))
               ),
               br(),
               fluidRow(
                 column(12, verbatimTextOutput("t_test_superhost"))
               ),
               br(),
               h4("Superhost:"),
               p("The vast majority of reviews for both groups are clustered in the top range (4.5 - 5), indicating an overall positive trend in Airbnb ratings. 
Superhosts outperform Regular Hosts in achieving top-tier ratings, highlighting their higher level of service quality and guest satisfaction. "),
               p("The t-test shows a highly significant difference in average review scores between superhosts and non-superhosts (p-value < 2.2e-16). Superhosts have higher average review scores (4.859) compared to non-superhosts (4.681), with a mean difference of approximately 0.18 within a 95% confidence interval of -0.183 to -0.174.We expand on this later with a regression. ")
      ),
      
      # Host Response Rate Analysis
      tabPanel("Host Response Rate",
               fluidRow(
                 column(12, plotOutput("plot_response_rate", height = "500px"))
               ),
               br(),
               h4("Host Response Rate:"),
               p("The boxplot illustrates the relationship between host response time and review scores (limited to the range of 4 to 5). Across all response times, the median review score remains consistently high, close to 5. However, there is a slight trend of higher review scores for hosts responding within an hour, as indicated by the more compact interquartile range near the upper limit of 5. The variability increases slightly for hosts who take within a day or a few days or more to respond, with a broader spread and lower outliers closer to 4. This suggests that quicker response times may correlate with slightly better review consistency, though the overall impact appears marginal given the consistently high median scores across all groups.")
      ),
      
      # Host Response Time Analysis
      tabPanel("Host Response Time",
               fluidRow(
                 column(12, plotOutput("plot_response_time", height = "500px"))
               ),
               br(),
               h4("Host's total count of listings:"),
               p("The bar chart shows the relationship between the total number of listings a host manages (categorized into small, medium, large, and very large portfolios) and the average review scores they receive. Hosts with small portfolios (1–2 listings) achieve the highest average review score of 4.76, while those managing very large portfolios (51+ listings) have the lowest average review score of 4.5. There is a clear downward trend as the portfolio size increases, suggesting that hosts with fewer listings may provide more personalized and attentive service, leading to better guest satisfaction and higher ratings. This may indicate that larger-scale operations face challenges in maintaining the same level of quality or attention to detail. ")
      ),
      
      # Host Listing Count Analysis
      tabPanel("Total Listings & Ratings",
               fluidRow(
                 column(12, plotOutput("plot_listings_ratings", height = "500px"))
               ),
               br(),
               h4("Licensed Listings:"),
               p("The box plot reveals that licensed listings tend to have higher prices, with a slightly elevated median and a broader interquartile range compared to unlicensed listings. This suggests that obtaining a license may allow hosts to charge a premium, likely reflecting higher perceived quality or regulatory compliance."),
               h4("Unlicensed Listings:"),
               p("Unlicensed listings exhibit lower median prices, with a narrower interquartile range. This could indicate that these listings target more budget-conscious customers or operate in less competitive or less regulated markets."),
               h4("Exempt Listings:"),
               p("While the focus is on licensable categories, exempt listings introduce an interesting middle ground:"),
               tags$ul(
                 tags$li("Hotel-Type Exemptions: Prices behave similarly to licensed listings, suggesting they target a similar customer base."),
                 tags$li("Mobility Lease Exemptions: Prices fall slightly below hotel-type exemptions but remain competitive, indicating these listings occupy a niche between budget and premium options.")
               )
      )
    )
  ),
  ##PAGE 7
  
  nav_panel(
    title = "Effect of Licence on Price or Rating",
    h1("What is the relationship between the listing having a licence and price and rating?"),
    
    tabsetPanel(
      tabPanel("License Status & Price",
               fluidRow(
                 column(12, plotOutput("plot_license_price", height = "500px"))
               ),
               br(),
               h4("Licensed Listings:"),
               p("The box plot reveals that licensed listings tend to have higher prices, with a slightly elevated median and a broader interquartile range compared to unlicensed listings. This suggests that obtaining a license may allow hosts to charge a premium, likely reflecting higher perceived quality or regulatory compliance."),
               h4("Unlicensed Listings:"),
               p("Unlicensed listings exhibit lower median prices, with a narrower interquartile range. This could indicate that these listings target more budget-conscious customers or operate in less competitive or less regulated markets."),
               h4("Exempt Listings:"),
               p("While the focus is on licensable categories, exempt listings introduce an interesting middle ground:"),
               tags$ul(
                 tags$li("Hotel-Type Exemptions: Prices behave similarly to licensed listings, suggesting they target a similar customer base."),
                 tags$li("Mobility Lease Exemptions: Prices fall slightly below hotel-type exemptions but remain competitive, indicating these listings occupy a niche between budget and premium options.")
               )
      ),
      
      # License Status & Ratings Analysis
      tabPanel("License Status & Ratings",
               fluidRow(
                 column(12, plotOutput("plot_license_ratings", height = "500px"))
               ),
               br(),
               h4("Licensed Status and Ratings"),
               p("This boxplot shows that exempt (hotel-type) hosts generally have the highest median ratings, around 4.5, and a relatively narrow spread, indicating more consistently positive reviews with a few high outliers. Exempt (mobility) listings appear slightly lower, with a slightly wider range but still centered near 4.4. In contrast, licensed hosts display a lower median around 4.2 and a cluster of outliers in the 3.5–3.8 zone, suggesting more variation and some negative experiences. Finally, not licensed hosts show the lowest median rating at about 4.0 and the widest distribution, indicating less consistent guest satisfaction and many low outliers.")
    ))
    ),
  
  
  ## PAGE 8
  nav_panel(
    title = "Data Browser",
    h1("Explore Airbnb Listings in Paris"),
    
    # Filtering Inputs
    fluidRow(
      column(4, selectInput("neighbourhood_select", "Select Neighborhood:", 
                            choices = c("All", unique(df_cleaned$neighbourhood_cleansed)), 
                            selected = "All")),
      column(4, selectInput("property_type_select", "Select Property Type:", 
                            choices = c("All", unique(df_cleaned$property_type)), 
                            selected = "All")),
      column(4, selectInput("room_type_select", "Select Room Type:", 
                            choices = c("All", unique(df_cleaned$room_type)), 
                            selected = "All")),
      column(4, numericInput("price_max", "Max Price (€):", value = 30400, min = 10, step = 10)),
      column(4, numericInput("availability365_max", "Max Availability:", value = 10, min = 0, step = 1))
    ),
    
    br(),  # Spacing
    
    # DataTable Output
    fluidRow(
      column(12, DTOutput("table_overview"))
    )
  )
  
)


# Server #######################################################################
server <- function(input, output, session) {
  
  #Data Browser
  
  filtered_data <- reactive({
    df <- df_cleaned %>%
      select(id, neighbourhood_cleansed, room_type, property_type, price, accommodates, review_scores_rating, availability_365)  # Select key columns
    
    if (input$neighbourhood_select != "All") {
      df <- df %>% filter(neighbourhood_cleansed == input$neighbourhood_select)
    }
    if (input$room_type_select != "All") {
      df <- df %>% filter(room_type == input$room_type_select)
    }
    if (input$property_type_select != "All") {
      df <- df %>% filter(property_type == input$property_type_select)
    }
    df <- df %>% filter(price <= input$price_max)
    
    df <- df %>% filter(availability_365 <= input$availability365_max)
    
    return(df)
  })
  
  # Render DataTable
  output$table_overview <- renderDT({
    datatable(
      filtered_data(),
      options = list(
        pageLength = 10,   
        autoWidth = TRUE, 
        scrollX = TRUE, 
        searchHighlight = TRUE 
      ),
      rownames = FALSE
    )
  })
  
  
  ## Reactive objects ##########################################################
  
  ## Read Paris neighborhood spatial data 
  paris_neighbourhoods <- st_read(here("Data/neighbourhoods.geojson"))
  
  ############################################################################
  ### Availability Data Processing ###########################################
  ############################################################################
  
  availability_data <- df_cleaned %>%
    filter(!is.na(neighbourhood_cleansed), 
           !is.na(availability_30),
           !is.na(availability_60),
           !is.na(availability_90),
           !is.na(availability_365)) %>%
    group_by(neighbourhood_cleansed) %>%
    summarise(
      avg_availability_30 = mean(availability_30, na.rm = TRUE),
      avg_availability_60 = mean(availability_60, na.rm = TRUE),
      avg_availability_90 = mean(availability_90, na.rm = TRUE),
      avg_availability_365 = mean(availability_365, na.rm = TRUE),
      .groups = "drop"
    )
  
  ## Join Availability Data with Spatial Data 
  paris_map_data <- paris_neighbourhoods %>%
    left_join(availability_data, by = c("neighbourhood" = "neighbourhood_cleansed"))
  
  ## Reactive Palette for Availability
  pal_availability <- reactive({
    colorNumeric(
      palette = switch(input$availability_period,
                       "avg_availability_30" = rev(hcl.colors(6, "YlOrRd")),  # Reverse for lighter high values
                       "avg_availability_60" = rev(hcl.colors(6, "BuGn")),
                       "avg_availability_90" = rev(hcl.colors(6, "PuBu")),
                       "avg_availability_365" = rev(hcl.colors(6, "Blues"))),
      domain = paris_map_data[[input$availability_period]],
      na.color = "transparent"
    )
  })
  
  ## Leaflet Map for Availability
  output$paris_map <- renderLeaflet({
    leaflet(data = paris_map_data) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(
        fillColor = ~pal_availability()(paris_map_data[[input$availability_period]]),
        weight = 1,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,
        highlight = highlightOptions(
          weight = 2,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE
        ),
        label = ~paste0(neighbourhood, ": ", round(paris_map_data[[input$availability_period]], 2)),
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto"
        )
      ) %>%
      addLegend(
        position = "bottomright",
        pal = pal_availability(),
        values = ~paris_map_data[[input$availability_period]],
        title = paste("Avg Availability<br>Next", 
                      gsub("avg_availability_", "", input$availability_period), 
                      "Days"),
        opacity = 0.7
      )
  })
  
  ############################################################################
  ## Price Data Processing ###################################################
  ############################################################################
  price_data <- df_cleaned %>%
    filter(!is.na(neighbourhood_cleansed), !is.na(price)) %>%
    group_by(neighbourhood_cleansed) %>%
    summarise(
      avg_price = mean(price, na.rm = TRUE),
      .groups = "drop"
    )
  
  ## Join Price Data with Spatial Data
  paris_price_map_data <- paris_neighbourhoods %>%
    left_join(price_data, by = c("neighbourhood" = "neighbourhood_cleansed"))
  
  ## Color Palette for Price Map
  pal_price <- colorNumeric(
    palette = "YlGnBu",  # Yellow-Green-Blue scale for price visualization
    domain = paris_price_map_data$avg_price,
    na.color = "transparent"
  )
  
  ## Render Leaflet Map for Prices 
  output$paris_price_map <- renderLeaflet({
    leaflet(data = paris_price_map_data) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(
        fillColor = ~pal_price(avg_price),
        weight = 1,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,
        highlight = highlightOptions(
          weight = 2,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE
        ),
        label = ~paste0(neighbourhood, ": €", round(avg_price, 2)),
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto"
        )
      ) %>%
      addLegend(
        position = "bottomright",
        pal = pal_price,
        values = ~paris_price_map_data$avg_price,
        title = "Avg Price (€)",
        opacity = 0.7
      )
  })
  
  ############################################################################
  ## Distribution Analysis ###################################################
  ############################################################################
  # Function to create distribution
  distribution_plot <- function(df, var) {
    ggplot(df, aes_string(x = var)) +
      geom_histogram(bins = 30, fill = 'blue', color = 'black', alpha = 0.7) +
      labs(title = paste("Distribution of", var), x = var, y = "Count") +
      theme_minimal(base_size = 18) +  # Increase base font size
      theme(
        plot.title = element_text(size = 24, face = "bold"),  # Larger title
        axis.title.x = element_text(size = 20, face = "bold"),
        axis.title.y = element_text(size = 20, face = "bold"),
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16)
      )
  }
  
  # Render Histogram with improved font size
  
  output$variable_description <- renderUI({
    var_desc <- list(
      price = "<b>Price:</b> The price of the Airbnb listing in euros (€).
      The histogram is highly right-skewed, with the majority of listings concentrated at lower price points. A small number of listings have significantly higher prices, indicating the presence of outliers.",
      
      accommodates = "<b>Accommodates:</b> The number of guests the listing can accommodate.
      The histogram shows that most listings cater to 2 to 4 guests, with a sharp decline in frequency for higher guest capacities. Listings accommodating more than 8 guests are rare.",
      
      bedrooms = "<b>Bedrooms:</b> Number of bedrooms in the listing.
      The histogram indicates that most listings have between 0 and 3 bedrooms, with higher counts being exceedingly rare. This suggests the dataset is predominantly composed of smaller accommodations.",
      
      bathrooms = "<b>Bathrooms:</b> Number of bathrooms in the listing.",
      
      availability_30 = "<b>Availability next 30 days: </b> 
      The distribution indicates that many listings have zero availability in the coming 30 days, with a smaller but steady frequency across other availability values. Listings with full availability (30 days) also show a slight increase.",
      
      availability_60 = "<b>Availability next 60 days: </b> 
      The distribution shows that many listings have no availability in the coming 60 days. There is a steady distribution across intermediate values, with a noticeable rise for listings with full availability (60 days).",
      
      availability_90 = "<b> Availability next 90 days: </b> 
      The histogram shows a heavily right-skewed distribution, with a significant concentration around zero availability. This suggests a strong bias toward inactive or completely booked hosts. There is a steady distribution across intermediate values, with a noticeable rise for listings with full availability (90 days).",
      
      availability_365 = "<b>Availability next 365 days:</b> 
      The histogram shows a right-skewed distribution, with a significant concentration around zero availability. Additionally, the data exhibits a recurring pattern roughly every quarter, suggesting seasonal or cyclical variations in availability throughout the year.",
      
      review_scores_rating = "<b>Review Score:</b> Average review score of the listing (1-5 scale). The histogram shows a heavily left-skewed distribution, with the majority of listings receiving ratings close to 5. This indicates a strong bias toward high review scores, suggesting generally positive feedback.",
      
      host_response_rate = "<b>Host Response Rate:</b> Percentage of messages the host responds to. 
      The histogram shows a heavily left-skewed distribution of host response rate, with a significant concentration around 100%. This suggests a strong bias toward high response rates.",
      
      host_total_listings_count = "<b>Host Listings:</b> Total number of listings the host has.
      The histogram is highly right-skewed, with most hosts having few listings while a small number of hosts have significantly more. This indicates a potential disparity between individual and professional hosts.",
      
      host_acceptance_rate = "<b> Host Acceptance Rate:</b> Percentage of bookings the host accepts.
      The histogram appears to be heavily left-skewed, with a large proportion of hosts having acceptance rates near 100%. This skewness may introduce bias.",
      
      number_of_reviews = "<b>Number of Reviews:</b>  
      The histogram shows a heavily right-skewed distribution, with the majority of listings having fewer than 50 reviews. A small number of listings exceed 1,000 reviews, indicating potential outliers.",
      
      number_of_reviews_ltm = "<b>Number of Reviews in the Last 12 Months:</b>  
      The histogram shows a heavily right-skewed distribution, with the majority of listings having fewer than 50 reviews in the last 12 months. A small number of listings exceed 400 reviews, indicating the presence of outliers.",
      
      review_scores_accuracy = "<b>Review Scores Accuracy:</b>  
      The histogram shows a heavily left-skewed distribution, with the majority of listings receiving accuracy scores close to 5. This indicates a strong bias toward high accuracy scores, suggesting generally positive feedback.",
      
      review_scores_cleanliness = "<b>Review Scores Cleanliness:</b>  
      The histogram shows a heavily left-skewed distribution, with the majority of listings receiving cleanliness scores close to 5. This indicates a strong bias toward high cleanliness scores, suggesting generally positive feedback.",
      
      review_scores_checkin = "<b>Review Scores Check-in:</b>  
      The histogram shows a heavily left-skewed distribution, with the majority of listings receiving check-in scores close to 5. This indicates a strong bias toward high check-in scores, suggesting generally positive feedback.",
      
      review_scores_communication = "<b>Review Scores Communication:</b>  
      The histogram shows a heavily left-skewed distribution, with the majority of listings receiving communication scores close to 5. This indicates a strong bias toward high communication scores, suggesting generally positive feedback.",
      
      review_scores_location = "<b>Review Scores Location:</b>  
      The histogram shows a heavily left-skewed distribution, with the majority of listings receiving location scores close to 5. This indicates a strong bias toward high location scores, suggesting generally positive feedback.",
      
      review_scores_value = "<b>Review Scores Value:</b>  
      The histogram shows a heavily left-skewed distribution, with the majority of listings receiving scores close to 5. This indicates a strong bias toward high value scores, suggesting generally positive feedback.",
      
      calculated_host_listings_count = "<b>Calculated Host Listings Count:</b>  
      The histogram shows a heavily right-skewed distribution, with the majority of hosts managing fewer than 10 listings. A small number of hosts manage significantly more listings, indicating potential outliers or professional operators.",
      
      reviews_per_month = "<b>Reviews Per Month:</b>  
      The histogram shows a heavily right-skewed distribution, with the majority of listings receiving fewer than 2 reviews per month. A small number of listings exceed 10 reviews per month, indicating potential outliers or highly active properties.",
      
      total_listings_Paris = "<b>Total Listings in Paris:</b>  
      The histogram shows a heavily right-skewed distribution, with the majority of hosts managing fewer than 10 listings. A small number of hosts manage significantly more listings, indicating potential outliers or professional operators."
    )
    
    HTML(var_desc[[input$selected_var]] %||% "<b> Select a variable to view its distribution.</b>")
  })
  
  # Ensure the plot has a valid variable and proper layout
  output$dist_plot <- renderPlot({
    req(input$selected_var)  # Ensure a variable is selected
    
    ggplot(df_cleaned, aes_string(x = input$selected_var)) +
      geom_histogram(bins = 30, fill = 'blue', color = 'black', alpha = 0.7) +
      labs(
        title = paste("Distribution of", input$selected_var),
        x = input$selected_var,
        y = "Count"
      ) +
      theme_minimal(base_size = 18) +  # Increase font size
      theme(
        plot.title = element_text(size = 24, face = "bold"),
        axis.title.x = element_text(size = 20, face = "bold"),
        axis.title.y = element_text(size = 20, face = "bold"),
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16)
      )
  })
  # Render Summary Table with larger font
  output$accommodates_summary <- renderTable({
    df_cleaned %>% tabyl(accommodates)
  })
  
  # Apply larger font size to the table in UI
  output$styled_accommodates_summary <- renderUI({
    div(
      tableOutput("accommodates_summary"),
      style = "font-size: 18px; font-weight: bold; text-align: left;"
    )
  })
  
  ################################################
  # PRICE
  ################################################
  # Room Type Plot
  output$plot_room_type <- renderPlot({
    df_cleaned %>%
      filter(!is.na(room_type), !is.na(price)) %>%
      group_by(room_type) %>%
      summarise(avg_price_rt = mean(price)) %>%
      mutate(price_bracket = cut(avg_price_rt, 
                                 breaks = seq(0, 350, by = 50), 
                                 labels = c("[0-50]", "[50-100]", "[100-150]", "[150-200]", 
                                            "[200-250]", "[250-300]", "[300-350]"),
                                 include.lowest = TRUE)) %>%
      ggplot(aes(x = room_type, y = avg_price_rt, fill = price_bracket)) +
      geom_bar(stat = "identity") + 
      labs(title = "Average Price by Room Type", x = "Room Type", y = "Average Price (€)") +
      scale_fill_viridis_d(option = "C") +  
      theme_bw() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Property Type (Below 500)
  output$plot_property_type_low <- renderPlot({
    df_cleaned %>%
      filter(!is.na(property_type), !is.na(price)) %>%
      group_by(property_type) %>%
      summarise(avg_price_rt = mean(price, na.rm = TRUE)) %>%
      filter(avg_price_rt < 500) %>%
      ggplot(aes(x = fct_reorder(property_type, avg_price_rt), y = avg_price_rt, fill = avg_price_rt)) + 
      geom_bar(stat = "identity", show.legend = FALSE) + 
      labs(title = "Average Price by Property Type (Below 500 €)", x = "Property Type", y = "Average Price (€)") +
      scale_fill_viridis_c(option = "C") +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Property Type (Above 500)
  output$plot_property_type_high <- renderPlot({
    df_cleaned %>%
      filter(!is.na(property_type), !is.na(price)) %>%
      group_by(property_type) %>%
      summarise(avg_price_rt = mean(price, na.rm = TRUE)) %>%
      filter(avg_price_rt > 500) %>%
      ggplot(aes(x = fct_reorder(property_type, avg_price_rt), y = avg_price_rt, fill = avg_price_rt)) + 
      geom_bar(stat = "identity", show.legend = FALSE) + 
      labs(title = "Average Price by Property Type (Above 500 €)", x = "Property Type", y = "Average Price (€)") +
      scale_fill_viridis_c(option = "C") +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Neighborhood Effect
  output$plot_neighborhood <- renderPlot({
    df_cleaned %>%
      filter(!is.na(neighbourhood_cleansed), !is.na(price)) %>%
      group_by(neighbourhood_cleansed) %>%
      summarise(avg_price_rt = mean(price)) %>%
      ggplot(aes(x = fct_reorder(neighbourhood_cleansed, avg_price_rt), y = avg_price_rt, fill = avg_price_rt)) +
      geom_bar(stat = "identity", show.legend = FALSE) + 
      labs(title = "Average Price by Neighborhood", x = "Neighborhood", y = "Average Price (€)") +
      scale_fill_viridis_c(option = "C") +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Accommodates Effect
  output$plot_accommodates <- renderPlot({
    df_cleaned %>%
      filter(!is.na(accommodates), !is.na(price)) %>%
      group_by(accommodates) %>%
      summarise(avg_price_rt = mean(price, na.rm = TRUE)) %>%
      ggplot(aes(x = accommodates, y = avg_price_rt, fill = avg_price_rt)) + 
      geom_bar(stat = "identity") + 
      labs(title = "Average Price by Number of People Accommodated", x = "Number of People", y = "Average Price (€)") +
      scale_fill_viridis_c(option = "C") +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Bedrooms Effect
  output$plot_bedrooms <- renderPlot({
    df_cleaned %>%
      filter(!is.na(bedrooms), !is.na(price)) %>%
      group_by(bedrooms) %>%
      summarise(avg_price_rt = mean(price, na.rm = TRUE)) %>%
      mutate(price_bracket = cut(avg_price_rt, 
                                 breaks = seq(0, 2500, by = 500),  # Increased upper limit
                                 labels = c("[0-500]", "[500-1000]", "[1000-1500]", "[1500-2000]", "[2000-2500]"),
                                 include.lowest = TRUE,
                                 right = TRUE)) %>%  # Ensure all values are captured
      ggplot(aes(x = bedrooms, 
                 y = avg_price_rt, 
                 fill = price_bracket)) + 
      geom_bar(stat = "identity") + 
      labs(title = "Average Price by Number of Bedrooms",
           x = "# of Bedrooms",
           y = "Average Price (€)",
           fill = "Price Bracket (€)") +
      scale_fill_viridis_d(option = "C", na.value = "gray") +  # NA values shown in gray
      theme_bw() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Amenities Effect
  output$plot_amenities <- renderPlot({
    df_cleaned %>%
      mutate(num_amenities = rowSums(select(., 
                                            has_tv, has_wifi, has_kitchen, 
                                            `has_air conditioning`, has_parking, has_pool, 
                                            has_washer, has_dryer, has_heating, has_pets, 
                                            `has_smoke alarm`, has_elevator), na.rm = TRUE)) %>%
      group_by(num_amenities) %>%
      summarise(avg_price_rt = mean(price, na.rm = TRUE), .groups = "drop") %>%
      ggplot(aes(x = num_amenities, y = avg_price_rt, fill = avg_price_rt)) + 
      geom_bar(stat = "identity") + 
      labs(title = "Average Price by Number of Amenities", x = "Number of Amenities", y = "Average Price (€)") +
      scale_fill_viridis_c(option = "C") +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  ########################################################################### 
  ## Host characteristics on Price and Ratings
  ########################################################################### 
  
  # Host is superhost
  
  output$plot_superhost_ratings <- renderPlot({
    df_filtered <- df_cleaned %>%
      filter(!is.na(review_scores_rating) & !is.na(host_is_superhost)) %>%
      mutate(
        review_bracket = cut(
          review_scores_rating,
          breaks = seq(0, 5, by = 0.25),  
          labels = paste0(seq(0, 4.75, by = 0.25), " - ", seq(0.25, 5, by = 0.25)),  
          include.lowest = TRUE
        )
      ) %>%
      group_by(host_is_superhost, review_bracket) %>%
      summarise(count = n(), .groups = "drop") %>%
      group_by(host_is_superhost) %>%
      mutate(percent = count / sum(count) * 100)  
    
    ggplot(df_filtered, aes(x = review_bracket, y = percent, fill = host_is_superhost)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(
        title = "Percentage Distribution of Review Scores by Superhost Status",
        x = "Review Score Bracket",
        y = "Percentage (%)"
      ) +
      scale_fill_manual(values = c("TRUE" = "#ee7854", "FALSE" = "#0d0888"), 
                        name = "Superhost Status", 
                        labels = c("Regular hosts", "Superhosts")) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Superhost T-Test Output
  output$t_test_superhost <- renderPrint({
    t_test_result <- t.test(
      review_scores_rating ~ host_is_superhost,
      data = df_cleaned,
      var.equal = FALSE
    )
    t_test_result
  })
  
  
  # Host Response Rate vs. Ratings Plot
  
  output$plot_response_rate <- renderPlot({
    df_aggregated <- df_cleaned %>%
      filter(!is.na(review_scores_rating) & !is.na(host_response_rate)) %>%
      mutate(
        response_rate_bracket = cut(
          host_response_rate,
          breaks = seq(0, 100, by = 10),
          labels = seq(10, 100, by = 10),
          include.lowest = TRUE
        )
      ) %>%
      group_by(response_rate_bracket) %>%
      summarize(
        mean_rating = mean(review_scores_rating), 
        .groups = "drop"
      )
    
    ggplot(df_aggregated, aes(x = as.numeric(as.character(response_rate_bracket)), y = mean_rating)) +
      geom_line(color = "#0d0888", size = 1) +
      scale_y_continuous(limits = c(4, 5), breaks = seq(0, 5, by = 0.5)) +
      labs(
        title = "Relationship Between Host Response Rate and Review Scores",
        x = "Host Response Rate (%)",
        y = "Average Review Score"
      ) +
      scale_x_continuous(breaks = seq(10, 100, by = 10)) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Host Response Time vs. Ratings Plot
  
  output$plot_response_time <- renderPlot({
    df_filtered_2 <- df_cleaned %>%
      filter(!is.na(host_response_time), !is.na(review_scores_rating), review_scores_rating >= 4, review_scores_rating <= 5) %>%
      mutate(
        host_response_time = factor(
          host_response_time,
          levels = c("within an hour", "within a few hours", "within a day", "a few days or more")
        )
      )
    
    ggplot(df_filtered_2, aes(x = host_response_time, y = review_scores_rating)) +
      geom_boxplot(fill = "#9d179e", outlier.color = "#0d0888", outlier.size = 1.5) +
      labs(
        title = "Relationship Between Host Response Time and Review Scores (4 to 5)",
        x = "Host Response Time",
        y = "Review Scores Rating"
      ) +
      scale_fill_viridis_c(option = "C") +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Host Listing Count vs. Ratings Plot
  
  output$plot_listings_ratings <- renderPlot({
    df_cleaned <- df_cleaned %>%
      mutate(listings_category = case_when(
        host_total_listings_count <= 2 ~ "Small (1-2)",
        host_total_listings_count <= 10 ~ "Medium (3-10)",
        host_total_listings_count <= 50 ~ "Large (11-50)",
        TRUE ~ "Very Large (51+)"
      ))
    
    average_ratings <- df_cleaned %>%
      group_by(listings_category) %>%
      summarize(mean_rating = mean(review_scores_rating, na.rm = TRUE), .groups = "drop")
    
    average_ratings$listings_category <- factor(
      average_ratings$listings_category,
      levels = c("Small (1-2)", "Medium (3-10)", "Large (11-50)", "Very Large (51+)")
    )
    
    ggplot(average_ratings, aes(x = listings_category, y = mean_rating, fill = listings_category)) +
      geom_bar(stat = "identity", width = 0.7) +
      geom_text(
        aes(label = round(mean_rating, 2)),
        position = position_stack(vjust = 0.5),
        fill = "white", 
        color = "black",  
        fontface = "bold"  
      ) +
      labs(
        title = "Average Review Scores by Total Listings Count",
        x = "Host Listings Category",
        y = "Average Review Score"
      ) +
      scale_fill_viridis_d(option = "C") +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            panel.grid.major.y = element_line(size = 0.5, color = "gray90"),
            panel.grid.minor.y = element_line(size = 0.3, color = "gray95")
      )})
    
    ########################################################################### 
    # License Status & Price Plot
    ########################################################################### 
    
  df_cleaned <- df_cleaned %>%
    mutate(
      license_detailed = case_when(
        str_detect(license, "\\d") ~ "licensed",  
        is.na(license) ~ "not licensed",            
        license == "Available with a mobility lease only (\"bail mobilité\")" ~ "exempt (mobility)",  
        license == "Exempt - hotel-type listing" ~ "exempt (hotel-type)",  
        TRUE ~ "unknown"                     
      )
    )
  
  # License Status & Price Plot
  output$plot_license_price <- renderPlot({
    ggplot(df_cleaned, aes(x = license_detailed, y = price, fill = license_detailed)) +
      geom_boxplot(outlier.colour = "red", outlier.size = 1, notch = TRUE) +
      scale_y_log10() +  # Log scale for better visualization
      labs(
        title = "Log-Scaled Price Distribution by License Status",
        x = "License Status",
        y = "Price (log scale)",
        fill = "License Status"
      ) +
      scale_fill_viridis_d(option = "C") +
      theme_bw() +
      theme(
        plot.title = element_text(hjust = 0.5, size = 16),
        axis.text.x = element_text(size = 10)
      )
  })
  
  # License Status & Ratings Plot
  output$plot_license_ratings <- renderPlot({
    ggplot(df_cleaned, aes(x = license_detailed, y = review_scores_rating, fill = license_detailed)) +
      geom_boxplot(outlier.colour = "red", outlier.size = 1, notch = TRUE) +
      scale_y_continuous(limits = c(3, 5)) + 
      labs(
        title = "Review Scores Rating Distribution by License Status",
        x = "License Status",
        y = "Review Scores Rating",
        fill = "License Status"
      ) +
      scale_fill_viridis_d(option = "C") +
      theme_bw() +
      theme(
        plot.title = element_text(hjust = 0.5, size = 16),
        axis.text.x = element_text(size = 10)
      )
    })
}


# Run the application 
shinyApp(ui = ui, server = server)
