#install.packages('remotes')
#library(remotes)
#remotes::install_version("tmap", version = "3.3-2", repos = "http://cran.us.r-project.org")
#remove.packages('tmap')
#install.packages('tmap')
library(shiny)
library(shinythemes)
library(dplyr)
library(bslib)
library(shinydashboard)
library(leaflet)
library(sf)
library(htmlwidgets)
library(leaflet.extras)
library(DT)
library(plotly)

#rsconnect::deployApp()


#UI
ui <- navbarPage(
  theme = shinytheme("cerulean"),
  id = "tabs",
  title=div(
    div('Jamaica Explorer', style = "font-size: 25px; float: left; line-height: 50px;"),
    style = "clear: both;"
  ),

  # 1. Welcome
  nav_panel(
    title = tags$div(class = "icon-text-container",
                     tags$i(class = "fa-solid fa-house"),
                     "Welcome"
    ),
    verticalLayout(
      div(class = "centered-text", 
          fluidRow(
            p("Jamaica Explorer", style = "font-family: 'Times New Roman', Times, serif; font-weight: bold; text-align: center; font-size: 60px;"),
            p("by Yibing Wang", style = "font-family: 'Times New Roman', Times, serif; font-weight: bold; text-align: center; font-size: 20px;"),
            p("This Shiny web application showcases extensive maps, tables, images, and data visualizations about Jamaica.", style="font-size: 21px;"),
            p("Explore dynamic maps, demographic dashboards, and interactive charts to learn about the island nation’s geography, population, and more.", style="font-size: 21px;"),
          )
      )
    )
  ),
  
  # 2. Geography
  navbarMenu(
    title = tags$div(
      class = "icon-text-container",
      tags$i(class = "fa-regular fa-map"),
      "Geography"
    ),
    # 2.1 Location in the World
    tabPanel(
      title = div(h3('Location in the World', style="font-size: 21px;")),
      fluidRow(
        column(width = 12,
               h3("World Map"),
               p("Use the quick travel buttons in the top-left (flag) corner of the map to zoom to Jamaica.", style = "font-size: 15px;"),
               leafletOutput("worldMap", height = "80vh")
        )
      )
    ),
    # 2.2 Country Map
    tabPanel(
      title= div(h3('Country Map', style="font-size: 21px;")),
      wellPanel(
        tags$h3("Parish Map"),
        tags$p("The earth and building buttons allow quick travel between the entire island and detailed parish views. The search function on the top-right can also help.",style ="font-size: 15px;"),
        leafletOutput("countryMap",height = "60vh")
      )
    )
  ),
  
  # 3. General Information
  navbarMenu(
    title = tags$div(
      class = "icon-text-container",
      tags$i(class = "fa-solid fa-file-lines"),
      "General Information"
    ),
    tabPanel(
      title= div(h3('About Jamaica', style="font-size: 21px;")),
      
      # Row 1
      fluidRow(
        column(6,
               wellPanel(
                 h3("Geographical Overview", style = "font-family: 'Times New Roman', Times, serif; font-weight: bold; font-size: 27px;"),
                 p("Jamaica is an island country in the Caribbean Sea, situated about 145 kilometers south of Cuba. The island covers about 10,990 square kilometers, making it the third-largest island in the Greater Antilles. It is renowned for its lush topography, rainforests, and reef-lined beaches, as well as its unique cultural contributions to music, food, and sports.",
                   style = "font-family: Arial, sans-serif; font-size: 18px;"),
                 img(src="jamaica_general1.jpg", height = 200, width = 350),
                 p("Photoed from Island Life Caribbean", style = "font-family: Arial, sans-serif; font-size: 9px;")
               )
        ),
        column(6,
               wellPanel(
                 h4("History and Colonization", style = "font-family: 'Times New Roman', Times, serif; font-weight: bold; font-size: 24px;"),
                 p("Originally inhabited by the Taíno and Arawak peoples, Jamaica was colonized by the Spanish in the early 16th century and later conquered by the British in 1655. It gained full independence in 1962. Notably, Jamaica's plantation economy thrived on sugar and slavery, leading to a rich cultural blend and significant diaspora communities worldwide.",
                   style = "font-family: Arial, sans-serif; font-size: 18px;"),
                 img(src="jamaica_general2.jpg", height = 200, width = 350),
                 p("Photoed from Black History Month 2024", style = "font-family: Arial, sans-serif; font-size: 9px;")
               )
        )
      ),
      
      # Row 2
      fluidRow(
        column(6,
               wellPanel(
                 h4("Economic Outlook", style = "font-family: 'Times New Roman', Times, serif; font-weight: bold; font-size: 24px;"),
                 p("Jamaica's economy heavily relies on services, with tourism and remittances being major contributors. Bauxite/alumina mining is another key sector, although it faces global market fluctuations. Economic reform programs aim to reduce government debt and stimulate growth in agriculture, logistics, and technology.",
                   style = "font-family: Arial, sans-serif; font-size: 18px;"),
                 img(src="jamaica_general3.jpg", height = 200, width = 350),
                 p("Photoed from Overlooked News", style = "font-family: Arial, sans-serif; font-size: 9px;")
               )
        ),
        column(6,
               wellPanel(
                 h4("Culture and Population", style = "font-family: 'Times New Roman', Times, serif; font-weight: bold; font-size: 24px;"),
                 p("With a population of nearly 3 million, Jamaica is known for its vibrant music genres (Reggae, Dancehall), world-class athletes (particularly in sprinting), and the Rastafari movement. English is the official language, though Jamaican Patois is widely spoken, reflecting the island's African heritage and British colonial influence.",
                   style = "font-family: Arial, sans-serif; font-size: 18px;"),
                 img(src="jamaica_general4.jpg", height = 200, width = 350),
                 p("Photoed from Britannica", style = "font-family: Arial, sans-serif; font-size: 9px;")
               )
        )
      ),
      
      #Row 3
      fluidRow(
        column(6,
               wellPanel(
                 h4("Climate and Geography", style = "font-family: 'Times New Roman', Times, serif; font-weight: bold; font-size: 24px;"),
                 p("Jamaica features a tropical climate with hot and humid weather, though higher inland regions, like the Blue Mountains, are more temperate. The island is prone to hurricanes and tropical storms during the Atlantic hurricane season. Preservation of coral reefs and ecological reserves is a growing priority.",
                   style = "font-family: Arial, sans-serif; font-size: 18px;"),
                 img(src="jamaica_general5.jpg", height = 200, width = 350),
                 p("Photoed from Jamaica Excursions", style = "font-family: Arial, sans-serif; font-size: 9px;")
               )
        ),
        column(6,
               wellPanel(
                 h4("Infrastructure and Development", style = "font-family: 'Times New Roman', Times, serif; font-weight: bold; font-size: 24px;"),
                 p("International airports in Kingston and Montego Bay serve as main gateways. Highway networks, expansion of seaports, and improvements in telecommunications underscore Jamaica’s ongoing efforts to modernize its infrastructure to foster tourism and foreign direct investment.",
                   style = "font-family: Arial, sans-serif; font-size: 18px;"),
                 img(src="jamaica_general6.jpg", height = 200, width = 350),
                 p("Photoed from Sellars Tech", style = "font-family: Arial, sans-serif; font-size: 9px;")
               )
        )
      )
    )
  ),
  
  # 4. Demographics
  navbarMenu(
    title = tags$div(
      class = "icon-text-container",
      tags$i(class = "fa-solid fa-chart-simple"),
      "Demographics"
    ),
    # 4.1 Current Data
    tabPanel(
      title = div(h3('Current Data', style="font-size: 21px;")),
      verticalLayout(
        fluidRow(
          column(
            width = 5,
            wellPanel(
              tags$h3("Demographics Overview"),
              tags$p("Below is current demographics data for major Jamaican cities, including interactive mapping and bar charts.",
                     style="font-size: 18px;")
            )
          ),
          column(
            width = 2,
            wellPanel(
              infoBoxOutput("AveragePopulation", width = "100%"),
              infoBoxOutput("TotalPopulation", width = "100%")
            )
          ),
          column(
            width = 5,
            wellPanel(
              tags$h3("Control Panel"),
              sliderInput("numCities", "Number of Cities:", width = "100%",
                          min = 1, max = 100, value = 10)
            )
          )
        ),
        tabsetPanel(
          tabPanel(
            "Map & Graph",
            splitLayout(cellWidths = c("50%", "50%"),
                        leafletOutput("heatMap", width = "100%", height = "550px"),
                        plotlyOutput("barPlot", width = "100%", height = "550px")
            )
          ),
          tabPanel("Table", DTOutput("cityTable"))
        )
      )
    ),
    # 4.2 Population change
    tabPanel(
      title = div(h3('Population Change', style="font-size: 21px;")),
      fluidRow(
        column(
          width = 7,
          wellPanel(
            tags$h3("Historical Population Trends"),
            tags$p("Using historical data from 1990 to present, we visualize Jamaican population changes and project forward. A secondary chart compares male/female population data between selected years for deeper gender-based analysis.", 
                   style = "font-size: 18px;")
          ),
          wellPanel(
            tags$h3("Historical and Projected Population"),
            plotlyOutput("populationPlot")
          )
        ),
        column(
          width = 5,
          wellPanel(
            titlePanel("Gender Comparison Over Selected Years"),
            sliderInput("yearSlider", "Year:", min = 1990, max = 2023, value = 2000),
            plotlyOutput("genderPlot")
          )
        )
      )
    )
  ),
  
  # 5. Status Analysis
  navbarMenu(
    title = tags$div(
      class = "icon-text-container",
      tags$i(class = "fa-solid fa-earth-americas"),
      "Status Analysis"
    ),
    # 5.1 Nearby Islands (or region)
    tabPanel(
      title= div(h3('Nearby Caribbean Islands', style="font-size: 21px;")),
      fluidRow(
        wellPanel(
          tabsetPanel(
            tabPanel("Greater Antilles",
                     icon = icon("home", lib = "font-awesome"),
                     fluidRow(
                       column(4,
                              h3("The Greater Antilles", style = "font-family: 'Times New Roman', Times, serif; font-weight: bold; font-size: 24px;"),
                              p("The Greater Antilles is a grouping of larger islands in the Caribbean Sea, including Cuba, Hispaniola (Haiti and the Dominican Republic), Puerto Rico, and Jamaica. These islands are culturally and historically interconnected, forming a major subregion of the Caribbean.",
                                style = "font-family: Arial, sans-serif; font-size: 18px;")
                       ),
                       column(8, img(src="jamaica_caribbean_map.png", height = 300, width = 450),
                              p("Photoed from Ant Wiki", style = "font-family: Arial, sans-serif; font-size: 9px;"))
                     )
            ),
            tabPanel("Cuba",
                     fluidRow(
                       column(6,
                              h3("Cuba", style = "font-family: 'Times New Roman', Times, serif; font-weight: bold; font-size: 24px;"),
                              p("Cuba is the largest island in the Caribbean, known for its rich cultural heritage, distinctive music, and revolutionary history. Economically, Cuba relies on exports of sugar, tobacco, and medical services, along with growing tourism. Although a different political system from Jamaica’s, both share aspects of Afro-Caribbean culture and Spanish colonial heritage.",
                                style = "font-family: Arial, sans-serif; font-size: 18px;")
                       ),
                       column(6, img(src="cuba.jpg", height = 300, width = 450),
                              p("Photoed from Latin America & Caribbean Geographic", style = "font-family: Arial, sans-serif; font-size: 9px;"))
                     )
            ),
            tabPanel("Hispaniola",
                     fluidRow(
                       column(6,
                              h3("Hispaniola", style = "font-family: 'Times New Roman', Times, serif; font-weight: bold; font-size: 24px;"),
                              p("Hispaniola is the second-largest island of the Caribbean, politically divided into Haiti and the Dominican Republic. It has a combined population of over 20 million. Although separated by language and governance, both nations share significant cultural and environmental challenges.",
                                style = "font-family: Arial, sans-serif; font-size: 18px;")
                       ),
                       column(6, img(src="hispaniola.jpg", height = 300, width = 450),
                              p("Photoed from Britannica", style = "font-family: Arial, sans-serif; font-size: 9px;"))
                     )
            ),
            tabPanel("Puerto Rico",
                     fluidRow(
                       column(6,
                              h3("Puerto Rico", style = "font-family: 'Times New Roman', Times, serif; font-weight: bold; font-size: 24px;"),
                              p("Puerto Rico is an unincorporated territory of the United States, known for its vibrant culture, mountainous landscapes, and beaches. It has a population of around 3 million. Economically reliant on manufacturing and services, Puerto Rico has a distinct identity blending Spanish, African, and North American influences.",
                                style = "font-family: Arial, sans-serif; font-size: 18px;")
                       ),
                       column(6, img(src="puertorico.jpg", height = 300, width = 450),
                              p("Photoed from Britannica", style = "font-family: Arial, sans-serif; font-size: 9px;"))
                     )
            )
          )
        )
      ),
      fluidRow(
        plotlyOutput("popCompare")
      )
    ),
    # 5.2 SWOT
    tabPanel(
      title = div(h3('SWOT Analysis', style="font-size: 21px;")),
      
      # First wellPanel for S & W
      wellPanel(
        # Row for S
        fluidRow(
          column(6,
                 h3(HTML("<span style='font-size: 300%;'>S</span>trengths"),
                    style = "font-family: 'Times New Roman', Times, serif; font-size: 28px; font-weight: bold;"),
                 p("• Strong global cultural influence through music (Reggae) and sports (sprinting)",style = "font-family: Arial, sans-serif; font-size: 18px;"),
                 p("• Strategic location in the Caribbean for tourism and trade",style = "font-family: Arial, sans-serif; font-size: 18px;"),
                 p("• Vibrant diaspora contributing remittances",style = "font-family: Arial, sans-serif; font-size: 18px;"),
                 p("• Diverse natural landscapes supporting agriculture and tourism",style = "font-family: Arial, sans-serif; font-size: 18px;"),
          ),
          column(6,
                 img(src = "jamaica_Strenth.jpg",height = 275, width = 400),
                 p("Photoed from Croydon in the Mountains", style = "font-family: Arial, sans-serif; font-size: 9px;")
          )
        ),
        # Row for W
        fluidRow(
          column(6,
                 h3(HTML("<span style='font-size: 300%;'>W</span>eaknesses"),
                    style = "font-family: 'Times New Roman', Times, serif; font-weight: bold; font-size: 28px;"),
                 p("• Persistent public debt and economic vulnerability",style = "font-family: Arial, sans-serif; font-size: 18px;"),
                 p("• High crime rates in certain urban areas",style = "font-family: Arial, sans-serif; font-size: 18px;"),
                 p("• Dependence on tourism, susceptible to global economic shifts",style = "font-family: Arial, sans-serif; font-size: 18px;"),
                 p("• Infrastructure gaps in rural regions",style = "font-family: Arial, sans-serif; font-size: 18px;"),
          ),
          column(6,
                 img(src = "jamaica_weakness.jpg", height = 275, width = 400),
                 p("Photoed from First Citizens", style = "font-family: Arial, sans-serif; font-size: 9px;")
          )
        )
      ),
      
      # Second wellPanel for O & T
      wellPanel(
        # Row for O
        fluidRow(
          column(6,
                 h3(HTML("<span style='font-size: 300%;'>O</span>pportunities"),
                    style = "font-family: 'Times New Roman', Times, serif; font-weight: bold; font-size: 28px;"),
                 p("• Growth in eco-tourism and cultural tourism",style = "font-family: Arial, sans-serif; font-size: 18px;"),
                 p("• Renewable energy investments (solar, wind)",style = "font-family: Arial, sans-serif; font-size: 18px;"),
                 p("• Expansion of agri-business and specialty exports (coffee, rum)",style = "font-family: Arial, sans-serif; font-size: 18px;"),
                 p("• Leveraging the diaspora to boost foreign investment and trade",style = "font-family: Arial, sans-serif; font-size: 18px;"),
          ),
          column(6,
                 img(src = "jamaica_opportunities.jpg", height = 275, width = 400),
                 p("Photoed from Deetken Impact", style = "font-family: Arial, sans-serif; font-size: 9px;")
          )
        ),
        # Row for T
        fluidRow(
          column(6,
                 h3(HTML("<span style='font-size: 300%;'>T</span>hreats"),
                    style = "font-family: 'Times New Roman', Times, serif; font-weight: bold; font-size: 28px;"),
                 p("• Climate change impacts: hurricanes, coastal erosion, coral reef damage",style = "font-family: Arial, sans-serif; font-size: 18px;"),
                 p("• Global economic downturns reducing tourism revenue",style = "font-family: Arial, sans-serif; font-size: 18px;"),
                 p("• Competition from other Caribbean islands for foreign investment",style = "font-family: Arial, sans-serif; font-size: 18px;"),
                 p("• Socioeconomic disparity and high unemployment fueling emigration",style = "font-family: Arial, sans-serif; font-size: 18px;"),
          ),
          column(6,
                 img(src = "jamaica_threats.jpg", height = 275, width = 400),
                 p("Photoed from Brookings Institution", style = "font-family: Arial, sans-serif; font-size: 9px;")
                 
          )
        )
      )
),
    

    # 5.3 Local Climate
    tabPanel(
      title= div(h3('Local Climate', style="font-size: 21px;")),
      titlePanel(tags$h1("As a island country, climate variability is a critical concern for Jamaica. Below are some pictures of climate impact on jamaica.", 
                         style = "font-family: 'Times New Roman', Times, serif; font-weight: bold; font-size: 24px;")),
      wellPanel(
        fluidRow(
          column(6, img(src="jamaica_climate1.jpg", height = 275, width = 400),
                 p("Photoed from Jamaica Climate Change Youth Council", style = "font-family: Arial, sans-serif; font-size: 9px;")),
          column(6, img(src="jamaica_climate2.jpg", height = 275, width = 400),
                 p("Photoed from Aksik", style = "font-family: Arial, sans-serif; font-size: 9px;"))
        ),
        fluidRow(
          column(6, img(src="jamaica_climate3.jpg", height = 275, width = 400),
                 p("Photoed from Phys.org", style = "font-family: Arial, sans-serif; font-size: 9px;")),
          column(6, img(src="jamaica_climate4.jpg", height = 275, width = 400),
                 p("Photoed from Thomson Reuters Foundation News", style = "font-family: Arial, sans-serif; font-size: 9px;"))
        )
      )
    )
  ),
  
  # 6. Bibliography
  navbarMenu(
    title = tags$div(
      class = "icon-text-container",
      tags$i(class = "fa-solid fa-asterisk"),
      "Bibliography"
    ),
    align = "right",
    tabPanel(tags$a(href="https://en.wikipedia.org/wiki/Jamaica", "Wikipedia: Jamaica")),
    tabPanel(tags$a(href="https://www.shinyapps.io/", "shinyapps.io for publishing")),
    tabPanel(tags$a(href="https://statinja.gov.jm/", "Statistical Institute of Jamaica (STATIN)")),
    tabPanel(tags$a(href="https://data.un.org/", "UN Data")),
    tabPanel(tags$a(href="https://www.worldbank.org/en/country/jamaica", "World Bank Data")),
    tabPanel(tags$a(href="https://climatedata.imf.org/pages/country-data", "IMF Climate Data"))
  )
)

# SERVER
server <- function(input, output, session) {
  jam_geojson <- sf::st_read("./data/gadm41_JAM_1.json")
  jam_country <- st_read("./data/gadm41_JAM_shp/gadm41_JAM_0.shp")
  jam_parish <- st_read("./data/gadm41_JAM_shp/gadm41_JAM_1.shp")
  jam_centroids <- st_centroid(jam_parish)
  
  # 2.1 MAPS
  output$worldMap <- renderLeaflet({
    # bounding box for Jamaica
    leaflet() %>%
      addTiles(urlTemplate = "https://server.arcgisonline.com/ArcGIS/rest/services/World_Street_Map/MapServer/tile/{z}/{y}/{x}") %>%
      addRectangles(
        lng1 = -78.4, lat1 = 17.7,
        lng2 = -76.2, lat2 = 18.7,
        color = "red", fill = FALSE, weight = 8
      ) %>%
      fitBounds(-120, -10, 60, 50) %>%  # Fit to a wide area first
      addEasyButton(easyButton(
        icon = "fa-globe", title = "World",
        onClick = JS("function(btn, map){ map.setView([0, 0], 2); }")
      )) %>%
      addEasyButton(easyButton(
        icon = "fa-flag", title = "Jamaica",
        onClick = JS("function(btn, map){ map.fitBounds([[17.7, -78.4], [18.7, -76.2]]); }")
      ))
  })
  
  output$satWorldMap <- renderLeaflet({
    leaflet() %>%
      addTiles(urlTemplate = "https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}") %>%
      addRectangles(
        lng1 = -78.4, lat1 = 17.7,
        lng2 = -76.2, lat2 = 18.7,
        color = "red", fill = FALSE, weight = 8
      ) %>%
      fitBounds(-120, -10, 60, 50) %>%
      addEasyButton(easyButton(
        icon = "fa-globe", title = "World",
        onClick = JS("function(btn, map){ map.setView([0, 0], 2); }")
      )) %>%
      addEasyButton(easyButton(
        icon = "fa-flag", title = "Jamaica",
        onClick = JS("function(btn, map){ map.fitBounds([[17.7, -78.4], [18.7, -76.2]]); }")
      ))
  })
  
  output$countryMap <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      fitBounds(lng1 = -77.5, lat1 = 17.9, lng2 = -77.3, lat2 = 18.1) %>%
      addSearchOSM(options = searchOptions(collapsed = FALSE, position = "topright")) %>%
      addEasyButton(easyButton(
        icon = "fa-globe", title = "Island View",
        onClick = JS("function(btn, map){ map.fitBounds([[17.7, -78.4], [18.7, -76.2]]); }")
      )) %>%
      addEasyButton(easyButton(
        icon = "fa-building", title = "City Detail",
        onClick = JS("function(btn, map){ map.fitBounds([[17.9, -77.5], [18.1, -77.3]]); }")
      ))
  })
  
  output$satCountryMap <- renderLeaflet({
    leaflet() %>%
      addTiles(urlTemplate = "https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}") %>%
      fitBounds(lng1 = -77.5, lat1 = 17.9, lng2 = -77.3, lat2 = 18.1) %>%
      addSearchOSM(options = searchOptions(collapsed = FALSE, position = "topright")) %>%
      addEasyButton(easyButton(
        icon = "fa-globe", title = "Island View",
        onClick = JS("function(btn, map){ map.fitBounds([[17.7, -78.4], [18.7, -76.2]]); }")
      )) %>%
      addEasyButton(easyButton(
        icon = "fa-building", title = "City Detail",
        onClick = JS("function(btn, map){ map.fitBounds([[17.9, -77.5], [18.1, -77.3]]); }")
      ))
  })
  
  # 4. DEMOGRAPHICS
  
  city_data <- read.csv("./data/jamaica_cities.csv")
  city_data_sf <- st_as_sf(city_data, coords = c("lng", "lat"), crs = 4326)
  
  # InfoBoxes
  output$AveragePopulation <- renderInfoBox({
    avg_pop <- mean(city_data$population, na.rm = TRUE)
    infoBox(
      "Average Population",
      format(round(avg_pop, 0), big.mark = ","),
      icon = icon("users", lib = "font-awesome"),
      color = "green", fill = TRUE
    )
  })
  
  output$TotalPopulation <- renderInfoBox({
    total_pop <- sum(city_data$population, na.rm = TRUE)
    infoBox(
      "Total Population",
      format(round(total_pop, 0), big.mark = ","),
      color = "purple", fill = TRUE
    )
  })
  
  # Table
  output$cityTable <- renderDT({
    top_cities <- city_data %>%
      arrange(desc(population)) %>%
      head(input$numCities)
    datatable(top_cities)
  })
  
  # Heat Map (Bubble map)
  output$heatMap <- renderLeaflet({
    leaflet(city_data_sf) %>%
      addTiles() %>%
      addCircleMarkers(
        radius = ~sqrt(population) / 500,  # scale radius from population
        fillColor = "blue",
        fillOpacity = 0.6,
        color = "black",
        stroke = TRUE,
        weight = 1,
        popup = ~paste0("<b>", city, "</b><br>Population: ", population)
      ) %>%
      setView(lng = -77.3, lat = 18.0, zoom = 8)
  })
  
  # BarPlot
  output$barPlot <- renderPlotly({
    top_cities <- city_data %>%
      arrange(desc(population)) %>%
      head(input$numCities)
    plot_ly(top_cities, x = ~city, y = ~population, type = 'bar', 
            marker = list(color = 'rgb(102,194,165)'),
            hoverinfo = 'text', text = ~paste("City:", city, "<br>Population:", population)) %>%
      layout(title = 'Distribution of City Population',
             xaxis = list(title = 'City'),
             yaxis = list(title = 'Population'),
             showlegend = FALSE,
             plot_bgcolor = 'rgba(245, 246, 249, 1)',
             margin = list(b = 80, l = 80, t = 50, r = 50))
  })
  
  # 4.2 Population over time
  gender_data <- read.csv("./data/jamaica_gender.csv")
  population_data <- read.csv("./data/jamaica_population.csv")
  
  output$genderPlot <- renderPlotly({
    filtered_data <- subset(gender_data, Year == input$yearSlider)
    
    plot_ly() %>%
      add_bars(
        x = ~filtered_data$Female, y = "Female", orientation = 'h',
        name = "Female", marker = list(color = 'skyblue')
      ) %>%
      add_bars(
        x = ~filtered_data$Male, y = "Male", orientation = 'h',
        name = "Male", marker = list(color = 'darkblue')
      ) %>%
      layout(
        barmode = 'stack',
        title = paste('Population by Gender in', input$yearSlider),
        xaxis = list(title = 'Population', dtick = 1e5, hjust = 45),
        yaxis = list(title = ''),
        showlegend = TRUE
      )
  })
  
  output$populationPlot <- renderPlotly({
    existing_data <- population_data[population_data$Year <= 2020, ]
    forecast_data <- population_data[population_data$Year > 2020, ]
    
    plot_ly() %>%
      add_lines(
        data = existing_data, 
        x = ~Year, 
        y = ~Population, 
        name = 'Historical Data', 
        line = list(color = 'skyblue')
      ) %>%
      add_lines(
        data = forecast_data, 
        x = ~Year, 
        y = ~Population,
        name = 'Projected Trends', 
        line = list(color = 'darkblue')
      ) %>%
      layout(
        title = 'Jamaica Population Over the Years',
        xaxis = list(title = 'Year', dtick = 2),
        yaxis = list(title = 'Population'),
        legend = list(x = 0.1, y = 0.9)
      )
  })
  
  # 5.1 Nearby Islands Comparison
  output$popCompare <- renderPlotly({
    islands <- c("Cuba", "Dominican Republic", "Puerto Rico", "Jamaica")
    population <- c(11194449, 11332972, 3205691, 2825544)
    percentages <- round(100 * population / sum(population), 1)
    colors <- c('pink', 'lightblue',
                'lightgreen', 'lightyellow')
    plot_ly(x = islands, y = population, type = 'bar', 
            text = ~paste(population, ' (', percentages, '%)', sep=""),
            hoverinfo = 'text', marker = list(color = colors)) %>%
      layout(title = "Population Comparison in the Greater Antilles",
             xaxis = list(title = "Island/Territory"),
             yaxis = list(title = "Population"),
             margin = list(b = 40, l = 70, r = 30, t = 70),
             font = list(family = "Arial, sans-serif", size = 12, color = "grey"))
  })
  
  
}

shinyApp(ui, server)
