library(ggplot2)
library(readxl)
library(dplyr)
library(shiny)
library(plotly)
library(tmap)
library(dplyr)
library(leaflet.extras)

# for graph 1
pop <- read_excel("Population.xlsx")

# for graph 2
Total_Race <- read_excel("Total.Race.xlsx")

# for graph 3
income <- read.csv("income_buckets.csv")
income$Year <- factor(income$Year)


#for graph 4
race_df <- read_excel("Race.xlsx")
race_df$year <- factor(race_df$year)
race_df <- race_df %>%
  mutate(race = ifelse(IDethnicity == 1, "Hispanic or Latino", race))
population_summary <- race_df %>%
  group_by(race, geo, year) %>%
  summarize(total_pop = sum(pop)) %>%
  ungroup()


ui <- fluidPage(
  headerPanel("Demographics of Largest Towns in Adair County"),
  tabsetPanel(
    tabPanel(
      titlePanel("App"),
  sidebarLayout(
    sidebarPanel(
      #for graph 1
      sliderInput("selectYear2", "Select Year for G1:", #change to animated?
                  min=min(pop$Year),
                  max=max(pop$Year),
                  value= 2016,
                  sep = "",
                  animate=
                    animationOptions(
                      interval = 100,
                      loop= FALSE,
                      playButton = NULL,
                      pauseButton = NULL)
                    ),
      
      #for graph 2
      sliderInput("selectYear", "Select Year for G2:", 
                  min=min(Total_Race$year),
                  max=max(Total_Race$year),
                  value= 2016,
                  sep = "",
                  animate=
                    animationOptions(
                      interval = 1000,
                      loop= FALSE,
                      playButton = NULL,
                      pauseButton = NULL)
      ),
      
      selectInput("selectRace", "Choose a Race or Ethnicity for G2:",
                  choices = unique(Total_Race$race),
                  selected = c(Total_Race$race),
                  multiple = TRUE),
      
      #for graph 4
      selectInput("selectedTown", "Choose a Town for G3:", 
                  choices = unique(race_df$geo)),
      selectInput("selectedRace", "Choose a Race or Ethnicity for G4:", 
                  choices = unique(race_df$race), 
                  selected = c(race_df$race),
                  multiple = TRUE)
      
    ),
    
    mainPanel(
      h4("G1: Population Size Map"),
      leafletOutput("popMap"),#g1
      p("Note: Circle size represents relative population sizes of towns."),
      plotlyOutput("histPlot"), #g2
      plotlyOutput("racepopPlot"), #g3 
      plotlyOutput("incomePlot") #g4
    )
  )),
  
  tabPanel(
    titlePanel("Description"),
      mainPanel(
       HTML("<h1>Introduction<h1>
       <h2>Overview of App<h2>
       <p>This app contains two tabs. The first tab contains the graphs and interactive elements, and this tab contains an in-depth description of the process and findings.</p>
       <h2>Data Source<h2>
       <p>The data is sourced from Data USA.  This is a visualization engine and data website that aggregates data from U.S. government sources to provide demographic and socioeconomic information to the public.  Nine datasets from the website were selected, each corresponding to one of the key variables of interest – income, race, and population – for each of the three towns.<p>
       <h2>Data Structure<h2>
       <p>The income data contained five variables: Income Bucket ID, Income Bucket, Year, Count, and Town.  The income buckets contained dollar amount ranges that were selected by census participants to indicate their annual household income.  The income buckets are initially organized in $5,000 increments, beginning with less than $10,000 and continuing up to $50,000. Beyond the $50,000 mark, the range of each income bucket broadens, first to $10,000 increments and then to $15,000 increments, until the final category for incomes of $200,000 or more.<p>
       <p>In order to attain the median annual incomes by year for each town in a way that could be translated onto a line graph, the data had to be reorganized.  First, the midpoint of each income bucket was found so that later, Excel’s median function could be used.  Then, each observation had to be replicated based on its count using the rep function in RStudio. Next, a table was set up to fill in the median annual income midpoints.  The column headers were the towns Kirksville, Brashear, and Novinger.  The row headers were years 2013 through 2021. A combination of Excel’s median and filter functions were used to fill in the median annual income for each year and town. The midpoints were then replaced by their original income buckets in the table. Last, the data was reshaped into a long format using the melt() function in Rstudio.  This left the data with three columns containing the towns, years, and corresponding median annual income buckets.<p>
       <p>The race and ethnicity data contained seven variables: Race ID, Race, Ethnicity ID, Ethnicity, Year, Town, and Population, covering from 2013 to 2021. In this dataset, the Ethnicity ID column uses a binary system where a value of 1 indicates the respondent identifies as ethnically Hispanic or Latino, and a 0 indicates they do not. Due to the structure of this data, those who identified as Hispanic or Latino were separated into a distinct category within the Race column, which is from then on referred to as Race or Ethnicity rather than just Race. This separation was necessary because each race was recorded twice for each year and town to account for individuals who may belong to any race and also identify as Hispanic or Latino. To ensure that each race is only listed once per town and year, ethnicity was treated as an independent category. While this approach helped create a clearer graph, it results in respondents who are ethnically Hispanic or Latino not being counted in the race category they otherwise selected.<p>
       <p>The population data contained five variables: Year, Population, Year on Year Change, Change in Percent, and Town. This dataset covers a broader time span compared to others, ranging from the year 2000 to 2022, adding 13 more years of data. The 'Year on Year Change' variable indicates the annual change in the population, represented as positive or negative whole numbers. The 'Change in Percent' variable details the percentage change in population annually.  Two columns for longitude and latitude and one for color were manually added.<p>
       <h1>Research Goals<h1>
       <p>The aim was to analyze various aspects of population dynamics in Kirksville and its neighboring towns. We wanted to explore trends in Kirksville's population size over the years, seeking to identify any patterns or significant changes, and to see how they compare to the trends in Novinger and Brashear. A key focus will be on how the demographics of race and ethnicity in Kirksville and its surrounding areas have evolved from 2013 to 2021. We also wanted to investigate whether there is a relationship between population trends and demographic changes in each of these towns. Another aspect of this research was to examine trends in the median annual income in Kirksville and its neighboring towns during the same period. We wanted to determine if Kirksville has maintained the highest median annual income compared to its neighboring towns consistently from 2013 to 2021.  Finally, we wanted to see if COVID or the student population of Truman affected demographics.<p>
       <h1>App Explanation<h1>
       <h2>Graph Descriptions<h2>
       <h3>G1: Population Size Map<h3>
       <p>This graph uses a map of Adair county as the background and three dynamic bubbles above the towns of interest. A map was chosen in order to visually connect Novinger and Brashear to Kirksville for those who are unfair with the county. The bubbles that appear over the towns have colors to differentiate the towns and a legend to clarify which color is matched with which town. Purple is associated with Truman State University, so that color was assigned to Kirksville.  Brashear had the smallest bubble on the map and was difficult to see, so it was assigned a vibrant red to increase visibility. Last, blue was assigned to Novinger.  The bubbles are partially transparent so that the map is still visible below the area of the bubble.  The size of the bubble is relative to the population size of the town it represents.  There is a pop-up that shows when a bubble is hovered over that shows the town name. When a bubble is clicked on, the exact population number is displayed. There is an interactive year slider that allows the user to choose what year and its respective population sizes to display.  The slider has an animation function so that all the years play in sequence at the click of a button.  The speed at which the years progress is quick for this graph as the changes in population are minimal and difficult to spot at a slow pace.<p>
       <h3>G2: Race Population Trends<h3>
       <p>This is a column graph displaying the proportion of the population of each race in their respective towns.  The y-axis shows the proportion of the population, with the highest value being 100% of the population. The x-axis categorizes the columns into the three towns, with each column representing a race.  The columns are outlined in a color randomly assigned to each race and are matched in the legend. If a column is clicked on or hovered over, the town name, exact population proportion, and race are shown in a pop-up. There is an interactive year slider that allows the user to choose which year’s data is displayed.  This slider has an animated feature that plays the years in order at a slow pace so that the proportion changes are easily tracked. There is a second interactive element to this graph: a drop-down allows the user to add or remove races depending on the information they wish to investigate. The default selection includes all of the races. Removing White makes it easier to view the proportions in the other races.<p>
       <h3>G3: Population Trends<h3>
       <p>This time series displays the population of one or more races as the years progress. The y-axis represents the population, and the x-axis represents the year.  There are two interactive drop downs for this graph.  The first allows the user to choose which town’s population will be broken down by race.  Only one town can be displayed at once as the population size of Kirksville is too large compared to the others.  Unless the y-axis was changed to proportion, the lines would be too compressed to easily read the graph.  The second drop down allows the user to add or remove races depending on the information they wish to investigate. The default selection includes all of the races. Removing White makes it easier to view the changes in the other races.<p>
       <h3>G4: Annual Median Income<h3>
       <p>With the goal of comparing the trends in annual median income of Kirksville to surrounding towns from 2013 to 2020, a time series was created.  The graph has median income buckets on the y-axis.  These buckets reflect what each household responded as their annual income in the census.  The x-axis represents the years 2013-2021.  There are three colored lines on the graph – green, blue, and red – corresponding to Kirksville, Novinger, or Brashear, respectively. There is a legend to match the color to the town.<p>
       <h2>Graph Findings<h2>
       <h3>G1: Population Size Map<h3>
       <p>After reviewing each year individually, as well as playing the animation several times and at several speeds, we concluded that Kirksville has not had a significant population change in the past 10 years, while Brashear and Novinger both decreased. Kirksville’s bubble, or population size, has no apparent change over time. However, Brashear and Novinger both have noticeable decreases in population size, as visualized by a reduction of the area of the bubble, in 2019.<p>
       <h3>G2 & G3: Race Population Trends<h3>
       <p>After reviewing changes in proportions over the years and changing which races were shown, we made several conclusions.  The most apparent is that the White population is much larger than any other race in every town.  Outside of the White population, “Two or More” was the majority in both Brashear and Novinger in most of the years. In 2019 in Kirksville, there was a decrease in the Native Hawaiian, Pacific Islander, and “Other” populations, but were otherwise steady. In Brashear, the Asian population begins to decrease in 2016 and then disappears completely for the rest of the timeline.<p>
       <h3>G4: Annual Median Income<h3>
       <p>After reviewing the graph, we can see that Kirksville has a higher median income every year after 2014.  Additionally, Kirksville’s income continually increases with a $10,000 difference between 2013 and 2021. Brashear and Novinger’s incomes are more similar to each other than to Kirksville, but still have their differences. Novinger’s income decreased by $10,000 between 2013 and 2016, then increased by that same amount between 2017-2021. Brashear’s income stays the same between 2013 and 2016, in the $25,000-$29,999 bucket, and then continually increases until reaching the $35,000-$39,999 bucket in 2021.<p>
       <h2>Summarize findings in relation to research questions<h2>
       <h3>Implication of findings<h3>
       <p>Given the demographic nature of the data and report, these findings could be used in a variety of disciplines. Trends in median annual income and population provide information necessary for planning housing, public service, or other community development projects. Additionally, Median annual income trends can be used when investigating local economies or how best to stimulate them. These trends are also informative to those interested in moving into or investing in the area.  Given this, Kirksville’s higher population and income trends might encourage investors to consider this location over others in the area. Furthermore, racial demographics are important to promote diversity, inclusion, and engagement in local communities. Marketing research could use all of this information to identify market segments and the attractiveness of the segments given their needs.<p>
       <p>Our app and data tells us a story that more people are drawn to Kikrsville than surrounding towns, whether that be for educational or career purposes. We discovered this through our first research question by simply observing population trends in different locations of Adair county. Then we questioned the race and ethnic demographics of the populations by town. All three towns are predominantly white as anticipated. However, Kirksville is known for having a large presence of Congolese immigrants which is reflected by the greater number of African Americans in Kirksville than its surrounding towns. Lastly, we wanted to understand how median income changed overtime. We observed Kirksville is increasing in median income which could reflect new job openings or new stores being added to the working industry, however further research would be needed to confirm this. Brashear and Novinger fluctuate more in overall income but have more recently increased. We would need to factor in economic changes, population fluctuations, and workforce trends to understand this trend in depth.  Overall, this app serves as a proof of concept and we hope users can learn something about our college town and its surrounding towns, but we acknowledge the study should be expanded with more data to make accurate conclusions.<p>
       <h2>Limitations<h2>
       <p>The size of Kirksville and its neighboring towns limits the data available to the public for analysis. Finding reliable data for these towns proved to be a struggle, but was overcome after discovering DataUSA aggregated census data in one place. Once the data was in hand, another difficulty identified was displaying data on a set scale. Each town’s White population is much larger than that of any other race. Additionally, Kirksville’s total population is much larger than that of the other towns.<p>
       <p>When a time series was made with all races displayed at once, the space between white and the rest of the races pushed and condensed the white trend line up at the top of the graph, and the rest at the bottom. The variation in the lines were flattened and hard to derive any observation from. This also meant Kirksville's graph was at a different scale than the others.  If the scale was set to be the same between all three to avoid providing misleading information, there would be too much space and the lines would be flattened.<p>
       <p>The first solution to this was to transform the data using logarithmic scaling. This made it possible to use one scale for all three graphs and still be able to see the trends, but with three facets each containing eight categories, it was busy and distracting. Luckily, the plan was to incorporate interactive elements all along. This meant that once the user was given the choice to select the town and any number of races to display, they could control how many lines they saw at once.<p>
       <p>We are also aware of the population turnover rate of Kirksville due to the fact Truman State University and A.T. Still University are located here. The census criteria considers where someone lives a majority of the time, therefore including the students that attend these universities. This knowledge explains the gap between a larger population for Kirksville and smaller populations for surrounding towns. It is noteworthy that international students are also included in this census therefore contributing to the diversity of Kirksville. However, students typically will only stay between four to six years and then leave, therefore explaining more drastic changes in the demographics of the population.<p>
       <h2>Future Work<h2>
       <p>This study has identified trends in income, racial demographics, and population distributions within the towns of Kirksville, Brashear, and Novinger. However, there is potential to broaden the scope of this research. Incorporating additional demographic variables, such as age groups and professions, could provide additional insights into the demographic makeup of these towns. Furthermore, gathering data from a longer time period would be a worthwhile expansion of this project. By analyzing data from earlier census records, long-term patterns might be found that are not apparent within the current data’s limited date range, better revealing how these towns have developed and changed over time.<p>
       <p>We could also narrow our focus to just looking at the demographics of Kirksville and comparing them to the universities. This would allow us to understand in more depth our findings in this study and how significantly the universities impact the population, demographics, and median income of Kirksville overall.<p>
       <h1>Citations<h1>
       <p>Brashear, MO[Data Set]. (2021). DataUSA. Retrieved from https://datausa.io/profile/geo/brashear-mo<p>
       <p>Kirksville, MO[Data Set]. (2021). DataUSA. Retrieved from https://datausa.io/profile/geo/kirksville-mo<p>
       <p>Novinger, MO[Data Set]. (2021). DataUSA. Retrieved from https://datausa.io/profile/geo/novinger-mo<p>
       
            ")
      )
  )
  )
)

server <- function(input, output) {
  
  filtered_data <- reactive({
    population_summary %>%
      filter(geo == input$selectedTown, race %in% input$selectedRace)
  })
  
  filter_data <- reactive({
    Total_Race %>%
      filter(year == input$selectYear, race %in% input$selectRace)
  })
  
#for graph 1
  newradius <- reactive({
    popYear <- pop %>% filter(Year == input$selectYear2)
    radius <- (popYear$Population / max(popYear$Population)) * 100
    return(list(popYear = popYear, radius = radius))
  })
  output$popMap <- renderLeaflet({
    popYear <- newradius()$popYear
    radius <- newradius()$radius
    
    leaflet(data = popYear) %>%
      addTiles() %>%
      addCircleMarkers(
        lng = ~Long, 
        lat = ~Lat, 
        popup = ~paste("Population:", Population),
        radius = radius, 
        fillOpacity = .5, 
        fillColor = ~Color,
        weight = 0,
        label = ~City
      ) %>%
      addLegend(
        title = "Town",
        colors = c("purple", "blue", "red"),
        labels = c("Kirksville", "Novinger", "Brashear")
      )
  })
    
  #for graph 2  
  output$histPlot <- renderPlotly({
    histplot <- ggplot(filter_data(), aes(x = city, y = proport, color = race, 
                                          group = race)) +
      geom_bar(stat="identity", position = "dodge") +
      theme_minimal() +
      labs(title = paste("G2: Race Population Trends in", input$selectYear),
           x = "Town",
           y = "Proportion of Population",
           color = "Race or Ethnicity")
    
    ggplotly(histplot)
  })
  
 #for graph 3
  output$racepopPlot <- renderPlotly({
    racepopPlot <- ggplot(filtered_data(), aes(x = year, y = total_pop, color = race, 
                                           group = race)) +
      geom_line() +
      scale_y_continuous(labels = scales::comma) +
      theme_minimal() +
      labs(title = paste("G3: Population Trends in", input$selectedTown),
           x = "Year",
           y = "Population",
           color = "Race or Ethnicity")
    ggplotly(racepopPlot)
  })  
  
#for graph 4
  output$incomePlot <- renderPlotly({
    incomePlot <- ggplot(income, aes(x = Year, y = IncomeBucket, color = Town, 
                                   group = Town)) +
      geom_line(size = 1) + geom_point() +
      labs(title = "G4: Annual Median Income from 2013 to 2021", x = "Year", 
           y = "Median Income", color = "Town") +
      theme_minimal()
    ggplotly(incomePlot)
  })  
  
 
  
  

}

shinyApp(ui = ui, server = server)