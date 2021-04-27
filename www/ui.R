
options(encoding = "UTF-8")
library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(shinydashboardPlus)
library(tidyverse)
library(plotly)
library(DT)
options(spinner.color = "#006272")
library(rlang)
library(highcharter)
library(tidyselect)
library(r2d3)
library(shinyjs)
library(shinycssloaders)
#library(V8)




### LOAD DATA AND SOME FUNCTIONS
#----------------------------------------------------------------------
data(worldgeojson, package = "highcharter")
source("refresh_coronavirus_jhu2.R")
source("helper.R")
source("data_af_prep.R")
africa_map <- readRDS("africa_map_data.RDS")
covidAfrica <- read_csv("all.csv") %>% 
  data_Af_prep()

covidAfrica_melt <- reshape2::melt(covidAfrica[,1:5]
                                   , id.vars = c("date", "country")
                                   , variable.name = "type"
                                   , value.name = "cases")


month_list <- as.list(1:12) %>%
  set_names(month.name)

month_list$`All Year` <- 99




corona_latest <- covidAfrica %>% 
  filter(date %in% max(covidAfrica$date))

total_cases <- sum(corona_latest$confirmed)

new_confirmed <- sum(corona_latest$confirmed) #computeContactRate()

recovered <- sum(corona_latest$recovered)

deaths <- sum(corona_latest$death) # computeCancellations()


countrylist <- unique(covidAfrica$country)
countrylist <- c("All Countries", countrylist)

last_date <-  max(covidAfrica$date)


col_vector <- c("#0000FF", "#FE2E2E", "#31B404")
type_vector <- c("confirmed", "death", "recovered")
### HEADER
#----------------------------------------------------------------------
header <- dashboardHeader( title = "Dashboard COVID",
                           disable = FALSE, 
                           titleWidth  = 550,
                           dropdownMenuCustom( type = 'message',  customSentence = customSentence,
                                               messageItem(
                                                 from = "Send Me Email",#'Feedback and suggestions',
                                                 message =  "",#paste0("TR_SharedMailbox@mbie.govt.nz" ),
                                                 icon = icon("envelope"),
                                                 href = "mailto:ayao.nomenyo@aims-cameroon.org"
                                               ),
                                               messageItem(
                                                 from = "Telegram Me",#'Feedback and suggestions',
                                                 message =  "",#paste0("TR_SharedMailbox@mbie.govt.nz" ),
                                                 icon = icon("fab fa-telegram"),
                                                 href = "https://t.me/ayao_nomenyo"
                                               ),
                                               
                                               messageItem(
                                                 from = "whatsapp Me",#'Feedback and suggestions',
                                                 message =  "Bonjour",#paste0("TR_SharedMailbox@mbie.govt.nz" ),
                                                 icon = icon("fab fa-whatsapp"),
                                                 href = "https://api.whatsapp.com/send?phone=22890537671&text=ayao%20nomenyo"
                                               ),
                                               
                                               icon = icon('comment')
                           ),
                           
                           dropdownMenuCustom( type = 'message',
                                               customSentence = customSentence_share,
                                               icon = icon("share-alt"),
                                               messageItem(
                                                 from = 'Twitter',
                                                 message = "",
                                                 icon = icon("twitter"),
                                                 href = "https://twitter.com/intent/tweet?url=http%3A%2F%2Ftradeintelligence.mbie.govt.nz&text=New%20Zealand%20Trade%20Intelligence%20Dashboard"
                                               ),
                                               messageItem(
                                                 from = 'Facebook',
                                                 message = "",
                                                 icon = icon("facebook"),
                                                 href = "https://www.facebook.com/sharer/sharer.php?u=http%3A%2F%2Ftradeintelligence.mbie.govt.nz"
                                               ),
                                               messageItem(
                                                 from = 'LinkedIn',
                                                 message = "",
                                                 icon = icon("linkedin"),
                                                 href = "http://www.linkedin.com/shareArticle?mini=true&url=http%3A%2F%2Ftradeintelligence.mbie.govt.nz&title=New%20Zealand%20Trade%20Intelligence%20Dashboard"
                                               )
                           )
)

header$children[[2]]$children[[2]] <- header$children[[2]]$children[[1]]
header$children[[2]]$children[[1]] <- tags$a(href='http://www.mbie.govt.nz',
                                             tags$img(src = 'MBIELogo/logo_reserve_small_corp.png'),
                                             target = '_blank') #,height='67',width='228.6', align = 'left'


### SIDBAR
#----------------------------------------------------------------------
sidbar <- dashboardSidebar(
  sidebarMenu(
    id = 'sidebar',
    style = "position: relative; overflow: visible;",
    menuItem(tabName = "main", "Main Dashboard", icon = icon("dashboard")),
    menuItem('Tendance', tabName = "tendance", icon =  icon("fas fa-chart-line")),
    
    menuItem(tabName = "extra", "Data", icon = icon("table")),
    
    
    useShinyjs(),
    
    div( id = 'sidebar_tend_extra',
         conditionalPanel(
           #"input.sidebar === 'extra' ||
           "input.sidebar === 'tendance'",
           selectInput(
             inputId = "country",
             label = div("Countries:", style = "color:white"),
             choices = countrylist,
             selected = "All Countries",
             selectize = FALSE,
             multiple = FALSE
           ),
           
           dateRangeInput("dates",
                          "Date range",
                          start = "2020-01-22",
                          end = as.character(Sys.Date()-1),
                          width = '300px',
                          separator = "-",
                          weekstart = 1
                          
           ),
           checkboxGroupInput("type_plot", 
                              h4("Les tendances"), 
                              choices = list("Cas confirmé" = "confirmed", 
                                             "Décès" = "death", 
                                             "Guérison" = "recovered"),
                              selected = "confirmed")
           
         ))
    
  ),
  tags$head(tags$style(HTML(".datepicker {z-index:99999 !important;}"))
  )
)

### BODY
#----------------------------------------------------------------------
body <- dashboardBody(
  source("tags_styles.R"),
  tabItems(
    #  tabItem(tabName = "main",
    #                 fluidRow(
    #                   box(width = 6,title = "Graph 1",color = "green", title_side = "top left",
    #                       column(width = 12, align = 'center', plotOutput("boxplot1"))
    #                   ),
    #                   
    #                   box(width = 6, title = "Graph 2", color = "red", ribbon = TRUE, title_side = "top left",
    #                       column(width = 12, plotly::plotlyOutput("dotplot1"))
    #                   )
    #                 )
    # ),
    tabItem(tabName = "main",
            
            fluidRow(
              # A static valueBox
              valueBox(
                new_confirmed,
                strong(paste0("Total des Cas confirmés : ", max(covidAfrica$date))),
                icon = tags$i(class = "fas fa-users", style ="font-size: 100px; color: white"),
                color = "light-blue"
              ),              
              # Dynamic valueBoxes
              # valueBoxOutput("total_confirmed_Af"),
              
              box(flexdashboard::gaugeOutput("recovered_covd", height = "110px"),
                  width=4,title="Recovered",background ="olive"),
              
              # valueBoxOutput("approvalBox"),
              
              box(flexdashboard::gaugeOutput("deaths_covd", height = "110px"),
                  width=4,title="Deaths",background ="red")
              
            ) ,
            
            fluidRow(column( width = 12,withSpinner(highchartOutput("covid_africa", height = "600px"), type = 4))
            )
    ),
    
    tabItem(tabName = "extra",
            fluidRow( withSpinner(dataTableOutput("carstable"), type = 4))
    ),
    
    tabItem(tabName = "tendance",
            div(id = 'main_wait_message',
                h1('Note, initial load may take up to 3 seconds.',
                   style = "color:darkblue" , align = "center" ) ,
                tags$hr(),
                
            ),
            
            # infoBoxes with fill=TRUE
            strong(h2(textOutput("country_choice")), align = 'center') ,
            fluidRow(
              
              # infoBox(paste0("Total des Cas confirmés : ", max(covidAfrica$date))
              #         , strong(10 * 2, align = "center", style="font-size: 40px"),
              #           icon = tags$i(class = "fas fa-users",
              #                       style ="font-size: 50px; color: white"), fill = TRUE),
              
              infoBoxOutput("n_confirmed"),
              infoBoxOutput("n_recovered"),
              infoBoxOutput("n_deaths")
            ),
            
            fluidRow( column( width = 12
                              ,#h4("Tendance du nombre cas confirmé", align = 'center'),
                              withSpinner(highchartOutput('tendance_confirmed', height = "600px")))
            ),
            
            
            fluidRow( column( width = 6,h4("Les pays les plus affectés", align = 'center'), withSpinner(highchartOutput('count_most_affect'), type = 4)),
                      column( width = 6,h4("Les régions les plus affectés", align = 'center'), withSpinner(highchartOutput('reg_most_affect'), type = 4))
            ),
            
            # fluidRow( column( width = 6 ,h4("Evolution du nombre de guérisons", align = 'center'), withSpinner(highchartOutput('tendance_recoved'),type = 4)),
            #              column( width = 6,h4("Evolution du nombre de décès", align = 'center'), withSpinner(highchartOutput('tendance_deaths'), type =4))
            # ),
            # 
            shinyjs::hidden( div( id = "load_more_message",
                                  tags$hr(),
                                  tags$h1("Loading...", align = "center")  )
            )
    )
    
    
  )
)

### UI : complet
#----------------------------------------------------------------------
ui <- shinydashboard::dashboardPage(header, sidbar, body)





