shinyOptions(cache = memoryCache(max_size = 50e6))

## app.R ##
library(shinydashboard)
library(flexdashboard)
library(ggplot2)
library(ggrepel)
library(data.table)
library(fhi)
library(magrittr)
library(dplyr)

fd::initialize("sykdomspuls")
source("global.R")
source("barometer.R")
source("signals.R")
source("number_weekly.R")
source("number_daily.R")
source("meminfluensa.R")



header = dashboardHeader(title = "Sykdomspulsen")
sidebar = dashboardSidebar(
  sidebarMenu(id = "tabs",
    menuItem("Nyheter", tabName = "nyheter", icon = icon("newspaper")),
    menuItem("Oversikt (ukentlig)", tabName = "oversikt_ukentlig", icon = icon("th")),
    menuItem("Signaler (ukentlig)", tabName = "signaler_ukentlig", icon = icon("exclamation-triangle")),
    menuItem("Antall (ukentlig)", tabName = "antall_ukentlig", icon = icon("chart-line")),
    menuItem("Antall (daglig)", tabName = "antall_daglig", icon = icon("chart-line")),
    menuItem("MeM-Influensa", tabName = "meminfluensa", icon = icon("snowflake"))
    #menuItemOutput("menuitem")
  )
)
body = dashboardBody(
  tabItems(
    # First tab content
    tabItem(
      tabName = "nyheter",
      fluidRow(
        column(width = 2),
        column(
          width = 8,
          box(
            title = "2019-09-20 / Ny nettside", width = NULL, solidHeader = TRUE, status = "primary",
            "Vi har byttet ut den gamle nettsiden med en ny for å gi dere en raskere opplevelse og for å være mer i tråd med kommunikasjonsavdelings anbefalinger om grafer. Håper dere liker det!"
          ),
          box(
            title = "2019-09-11 / MeM terskler til influensa tilsatt", width = NULL, solidHeader = TRUE, status = "primary",
            "Nå kan du se MeM terskler til influensa under fanen 'MeM-Influensa'"
          ),
          box(
            title = "2016-08-01 / Økning i aldersgruppen 15-19 år", width = NULL, solidHeader = TRUE, status = "primary",
            "Fra august 2016 er det en økning i antall konsultasjoner i aldersgruppen 15-19 år grunnet behov for sykemelding ved fravær i den videregående skole"
          ),
          box(
            title = "2015-01-01 / Lansering av Sykdomspulsen", width = NULL, solidHeader = TRUE, status = "primary",
            "Velkommen til Sykdomspulsen!"
          )
        ),
        column(width = 2)
      )
    ),

    tabItem(tabName = "oversikt_ukentlig",
            barometerUI("barometer", "Counter #1", GLOBAL=GLOBAL)
    ),

    tabItem(tabName = "signaler_ukentlig",
            signalsUI("signals", "Counter #1", GLOBAL=GLOBAL)
    ),

    tabItem(tabName = "antall_ukentlig",
            number_weeklyUI("number_weekly", "Counter #1", GLOBAL=GLOBAL)
    ),

    tabItem(tabName = "antall_daglig",
            number_dailyUI("number_daily", "Counter #1", GLOBAL=GLOBAL)
    ),

    tabItem(tabName = "meminfluensa",
            meminfluensaUI("meminfluensa", "Counter #1", GLOBAL=GLOBAL)
    )
  )
)

ui <- dashboardPage(
  header = header,
  sidebar = sidebar,
  body = body
)

server <- function(input, output) {

GLOBAL <- new.env(parent = emptyenv())
val <- pool %>% dplyr::tbl("spuls_standard_results") %>%
  dplyr::summarize(date=max(date,na.rm=T)) %>%
  dplyr::collect() %>%
  fd::latin1_to_utf8()

GLOBAL$dateMax <- val$date
GLOBAL$dateMinRestrictedRecent <- GLOBAL$dateMax - 365
GLOBAL$dateMinRestrictedLine <- GLOBAL$dateMax - 365 * 15

GLOBAL$outbreaksyrwk <- GLOBAL$weeklyyrwk <- rev(fhidata::days[yrwk<=fhi::isoyearweek(GLOBAL$dateMax)]$yrwk)[1:20]

vals <- unique(fhidata::norway_locations_current[,c("county_code","county_name")])
GLOBAL$weeklyCounties <- c("Norge", vals$county_code)
names(GLOBAL$weeklyCounties) <- c("Norge", vals$county_name)

CONFIG_OLD <- ConvertConfigForAPI()
GLOBAL$weeklyTypes <- GLOBAL$dailyTypes <- CONFIG_OLD$SYNDROMES[CONFIG_OLD$SYNDROMES %in% CONFIG$STANDARD[websiteInternal == TRUE]$tag]
GLOBAL$weeklyAges <- GLOBAL$dailyAges <- CONFIG_OLD$AGES

vals <- fhidata::norway_locations_long_current[location_code!="norway"]
vals[fhidata::norway_locations_current,on="location_code==municip_code",county_code:=county_code]
vals[is.na(county_code),county_code:=location_code]
vals[location_code=="norge",location_code:="Norge"]
vals[location_code=="norge",county_code:="Norge"]

GLOBAL$municipToCounty <- vals

GLOBAL$weeklyValues <- c(
  "Konsultasjoner" = "consults",
  "1 uke eksess" = "excess1"
)


  callModule(barometerServer, "barometer", GLOBAL=GLOBAL)
  callModule(signalsServer, "signals", GLOBAL=GLOBAL)
  callModule(number_weeklyServer, "number_weekly", GLOBAL=GLOBAL)
  callModule(number_dailyServer, "number_daily", GLOBAL=GLOBAL)
  callModule(meminfluensaServer, "meminfluensa", GLOBAL=GLOBAL)
#
#   output$menuitem <- renderMenu({
#     retval <- menuItem(" ")
#     if(input$tabs=="oversikt_ukentlig"){
#       retval <- menuItem("Instillinger",startExpanded = TRUE
#       )
#     } else if(input$tabs=="antall_ukentlig"){
#       weeklyMunicipChoices <- reactive({
#         if (is.null(input$weeklyCounty))
#           return(NULL)
#         if (input$weeklyCounty == "Norge") {
#           return("Norge")
#         } else {
#           data <- fhidata::norway_locations_current[county_code==input$weeklyCounty]
#           x <- data$municip_code
#           names(x) <- data$municip_name
#
#           return(c("Fylke", x))
#         }
#       })
#
#       selectInput("weeklyValue", "Verdier", as.list(GLOBAL$weeklyValues), selected = GLOBAL$weeklyValues[1])
#       retval <- menuItem("Instillinger",startExpanded = TRUE,
#         selectInput("weeklyType", "Sykdom/Symptom", as.list(GLOBAL$weeklyTypes), selected = GLOBAL$weeklyTypes[1]),
#         selectInput("weeklyAge", "Alder", as.list(GLOBAL$weeklyAges), selected = "Totalt"),
#         selectInput("weeklyCounty", "Fylke", as.list(GLOBAL$weeklyCounties), selected = GLOBAL$weeklyCounties[1]),
#         renderUI({
#           selectInput("weeklyMunicip", "Kommune", as.list(weeklyMunicipChoices()), selected = weeklyMunicipChoices()[1])
#         }),
#         selectInput("weeklyValue", "Verdier", as.list(GLOBAL$weeklyValues), selected = GLOBAL$weeklyValues[1])
#       )
#     }
#
#     return(retval)
#   })


}

shinyApp(ui, server)

#  shiny::runApp('inst/shiny/dashboard', port = 4989, host = "0.0.0.0")
