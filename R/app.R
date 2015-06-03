library(shiny)
#library(jake)
library(lubridate)
library(dplyr)
library(magrittr)
source("wiki_scrape.R")

ui <- fluidPage(
  # Application Title
  titlePanel("Wikipedia Page Views"),

  sidebarLayout(

    sidebarPanel(
      #sliderInput(inputId = "num",
      #            label = "Choose a number",
      #            value = 25, min = 1, max = 100),
      textInput("wiki_pages", "Wikipedia Page(s)", value="Thor Marvel"),

      sliderInput("start_year", "Start Year", 2015, min = 2010, max = 2015, step = 1),
      sliderInput("end_year", "End Year", 2015, min = 2010, max = 2015),
      checkboxInput("chk_smooth", "Smooth Data", FALSE),
      actionButton("dl_pageviews", "Fetch Pageview Data", icon("cloud-download"))
    ),

    mainPanel(
      plotOutput("pvplot", brush = "plot_brush", height = 480),
      verbatimTextOutput("info")
    ),

    position = "left"
  )
)

server <- function(input, output) {


  dat <- reactiveValues(wiki_data = getWikiStats(2015, 2015, "Thor Marvel") )

  observeEvent(input$dl_pageviews,
               {

                 dat$wiki_list <- unlist(strsplit(input$wiki_pages, ","))
                 #unlist(strsplit(c("Thor Marvel", "Captain America", "Ultron"), ","))
                 #Check for existing data

                 dat$wiki_term <- data.frame(url = sapply(dat$wiki_list, wikiURL, page_only = TRUE, USE.NAMES = FALSE))
                 #filter(test, !(url %in% wiki_data$Wikipedia))
                 #dat$wiki_term <- data.frame(url = wikiURL(dat$wiki_list, page_only = TRUE))

                 dat$wiki_list_add <- filter(dat$wiki_term, !(url %in% dat$wiki_data$Wikipedia))

                 dat$wiki_data <- rbind(
                                        dat$wiki_data[dat$wiki_data$Wikipedia %in% levels(dat$wiki_term$url),],
                                        getWikiStats(input$start_year, input$end_year, dat$wiki_list_add$url)
                                        )

               })

  output$pvplot <- renderPlot({
    #hist(rnorm(input$num))

    WikiPVPlot(na.omit(dat$wiki_data, TRUE), smooth = input$chk_smooth)
  })

  output$info <- renderPrint({
    brushedPoints(dat$wiki_data %>% select(Wikipedia, Date, Views), input$plot_brush)
  })
}

shinyApp(ui = ui, server = server)
