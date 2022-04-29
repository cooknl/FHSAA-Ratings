#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


require(stringr)
require(plotly)
require(shiny)
require(ggplot2)
require(ggmap)
require(maps)
require(mapproj)
require(paletteer)
require(viridis)
require(gginnards)
require(lubridate)
require(shinyjs)



load("./divisions.RData")
# load("./OLD-divisions.RData") # only for dev of update buttons

FL <- map_data("state", region = "florida")

lax_map <- ggplot(FL, aes(x = long, y = lat)) +
    geom_polygon(fill = '#7C89F7') +
    coord_map() +
    scale_color_paletteer_c("viridis::inferno",
                            limits = c(floor(min(divisions$Rating)),
                                       ceiling(max(divisions$Rating)))) +
    theme(axis.title = element_blank())


# Define UI for application that draws a histogram
ui <- fluidPage(
    useShinyjs(),
    # Application title
    titlePanel(title = "Florida Girls Lacrosse Ratings",
               windowTitle = "FL GLAX Ratings"),
    
    fluidRow(column(12,
        # Sidebar with a slider input for number of bins
        sliderInput(
            inputId = "ratings",
            label = "Range of Ratings:",
            min = floor(min(divisions$Rating)),
            max = ceiling(max(divisions$Rating)),
            value = c(floor(min(divisions$Rating)),
                      ceiling(max(divisions$Rating)))
        ),
        offset = 8
    )),
    fluidRow(column(12,
                    # Show a plot of the generated distribution
                    
                    plotlyOutput("FLmap", width = "100%"),)),
    
    fluidRow(
        column(
            12,
            actionButton(inputId = "check_rankings",
                         label = "Check for Updated Rankings"),
            
            actionButton(inputId = "update_rankings",
                         label = "Update Rankings"),
            
            textOutput("dateInfo"),
            offset = 8
        )
    ),
    hr(),
    HTML(
        '<a rel="license" href="http://creativecommons.org/licenses/by-nc-sa/4.0/">
            <img alt="Creative Commons License" style="border-width:0" src="https://i.creativecommons.org/l/by-nc-sa/4.0/88x31.png" />
        </a>
        <br />
        <span xmlns:dct="http://purl.org/dc/terms/" href="http://purl.org/dc/dcmitype/InteractiveResource" property="dct:title" rel="dct:type">
            Florida Girls Lacrosse Ratings
        </span>
        by
        <a xmlns:cc="http://creativecommons.org/ns#" href="https://capn.shinyapps.io/FL-GIrls-LAX-Ratings/" property="cc:attributionName" rel="cc:attributionURL">
            Nathan Cook
        </a>
        is licensed under a
        <a rel="license" href="http://creativecommons.org/licenses/by-nc-sa/4.0/">
            Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License
        </a>.'
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    # reactiveValues object for storing current data set.
    vals <- reactiveValues(date_check = NULL)
    
    observe({
        shinyjs::hide("update_rankings")
        if (!is.null(vals$date_check)) {
            if ((
                stringr::str_detect(
                    vals$date_check,
                    "^A rankings update is available as of"
                )
            )) {
                shinyjs::show("update_rankings")
            }
            
        }
    })
    
    observeEvent(input$check_rankings, {
        old_rankingsDate <- rankingsDate
        old_divisions <- divisions
        message('reloading')
        source("./get-fhsaa-rankings.R")
        load("./divisions.RData")
        print((old_rankingsDate  %--%  rankingsDate) %/% days(1))
        if ((old_rankingsDate  %--%  rankingsDate) %/% days(1)) {
            vals$date_check = paste(
                "A rankings update is available as of",
                day(rankingsDate),
                month(rankingsDate, label = TRUE),
                year(rankingsDate),
                sep = " "
            )
        }
        else
            vals$date_check = "No update available"
        
        rankingsDate <- old_rankingsDate
        divisions <- old_divisions
        
    })
    
    observeEvent(input$update_rankings, {
        old_rankingsDate <- rankingsDate
        old_divisions <- divisions
        message('reloading')
        source("./get-fhsaa-rankings.R")
        load("./divisions.RData")
        vals$date_check = paste(
            "Rankings have been updated and are",
            (rankingsDate %--% as_datetime(today())) %/% days(1),
            "days old.",
            sep = " "
        )
    })
    
    
    selected <- reactive({
        (divisions$Rating > input$ratings[[1]]) &
            (divisions$Rating < input$ratings[[2]])
    })
    
    output$FLmap <- renderPlotly({
        FL <- map_data("state", region = "florida")
        
        
        lax_map <- lax_map +
            geom_point(data = divisions[selected(), ],
                       aes(
                           x = lon,
                           y = lat,
                           color = Rating,
                           shape = Division,
                           label = SchoolName
                       ))
        
        ggplotly(lax_map) %>%
            plotly::config(displaylogo = FALSE) %>%
            plotly::config(
                modeBarButtonsToRemove = c(
                    "select2d",
                    "lasso2d",
                    "autoScale2d",
                    "hoverClosestCartesian",
                    "hoverCompareCartesian"
                )
            )
    })
    
    output$dateInfo <- renderText({
        if (is.null(vals$date_check))
            paste("Rankings are",
                  (rankingsDate %--% as_datetime(today())) %/% days(1),
                  "days old.",
                  sep = " ")
        else
            vals$date_check
    })
    
}

# Run the application
shinyApp(ui = ui, server = server)
