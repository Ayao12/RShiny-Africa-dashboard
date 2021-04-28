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
library(googlesheets4)
#library(V8)



data(worldgeojson, package = "highcharter")
source("refresh_coronavirus_jhu2.R")
source("helper.R")
source("data_af_prep.R")

africa_map <- readRDS("africa_map_data.RDS")

covidAfrica <- read_csv("all.csv") %>% 
  data_Af_prep()

covidAfrica[,3:5] <- mutate_all(covidAfrica[,3:5], replcace_inf_0)

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

new_recovered <- sum(corona_latest$recovered)

new_deaths <- sum(corona_latest$death) #computeCancellations()


countrylist <- unique(covidAfrica$country)
countrylist <- c("Africa", countrylist)

last_date <-  max(covidAfrica$date)


col_vector <- c("#0000FF", "#FE2E2E", "#31B404")
type_vector <- c("confirmed", "death", "recovered")




## Setup Shiny app UI components
#------------------------------------------------------------------------------

header <- dashboardHeader( title = HTML("Dashboard COVID"),
                           disable = FALSE, 
                           titleWidth  = 240,
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
                                                 href = "https://twitter.com/intent/tweet?text=https%3A//ayaofreeman.shinyapps.io/rshinycovid/"
                                               ),
                                               messageItem(
                                                 from = 'Facebook',
                                                 message = "",
                                                 icon = icon("facebook"),
                                                 href = "https://www.facebook.com/sharer/sharer.php?u=https%3A//ayaofreeman.shinyapps.io/rshinycovid/"
                                               ),
                                               messageItem(
                                                 from = 'LinkedIn',
                                                 message = "",
                                                 icon = icon("linkedin"),
                                                 href = "https://www.linkedin.com/shareArticle?mini=true&url=https%3A//ayaofreeman.shinyapps.io/rshinycovid/&title=&summary=&source="
                                               )
                           )
)

#header$children[[2]]$children[[2]] <- header$children[[2]]$children[[1]]
header$children[[2]]$children[[1]] <- tags$a(
                                             tags$img(src = 'aff2.png', style="height: 50px;"),
                                             target = '_blank', height='67', align = 'left') #,height='67',width='228.6', align = 'left'



sidbar <- dashboardSidebar(
  sidebarMenu(
    id = 'sidebar',
    style = "position: relative; overflow: visible;",
    menuItem(tabName = "main", "Main Dashboard", icon = icon("dashboard")),
    menuItem('Tendance', tabName = "tendance", icon =  icon("fas fa-chart-line")),
    menuItem(tabName = "extra", "Data", icon = icon("table")),
    menuItem("A propos", tabName = "about", icon = icon("fas fa-info-circle v", style="font-size: 20px;")),

    useShinyjs(),
    
    div( id = 'sidebar_tend_extra',
    conditionalPanel(
    #"input.sidebar === 'extra' ||
    "input.sidebar === 'tendance'",
                     selectInput(
                       inputId = "country",
                       label = div("Countries:", style = "color:white"),
                       choices = countrylist,
                       selected = "Africa",
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


body <- dashboardBody(
### tag
#--------------------------------------------------------------------------
  tags$head(
    # ## JS codes
    # tags$script(src = "fixedElement.js" ),
    # tags$style(HTML(".scroller_anchor{height:0px; margin:0; padding:0;}; 
    #                  .scroller{background: white; 
    #                   border: 1px solid #CCC; 
    #                   margin:0 0 10px; 
    #                   z-index:100; 
    #                   height:50px; 
    #                   font-size:18px; 
    #                   font-weight:bold; 
    #                   text-align:center; 
    #                  width:500px;}")),
    
    #tags$script(src = "world.js" ),
    
    
    tags$script("document.title = 'AFRICA COVID 19'"),
    
    ### Styles 
    tags$style(HTML(".small-box {height: 172px}")),
    tags$style(HTML(".fa { font-size: 15px; }")),
    #tags$style(HTML(".glyphicon { font-size: 33px; }")),  ## use glyphicon package
    tags$style(HTML(".fa-dashboard { font-size: 15px; }")),
    tags$style(HTML(".fa-globe { font-size: 20px; }")),
    tags$style(HTML(".fa-barcode { font-size: 20px; }")),
    tags$style(HTML(".tab-content { padding-left: 20px; padding-right: 30px; }")) ,
    tags$style(HTML(".fa-wrench { font-size: 15px; }")),
    tags$style(HTML(".fa-refresh { font-size: 15px; }")),
    tags$style(HTML(".fa-search { font-size: 15px; }")),
    tags$style(HTML(".fa-comment { font-size: 20px; }")),
    tags$style(HTML(".fa-share-alt { font-size: 20px; }")),
    tags$style(HTML(".fa-envelope { font-size: 20px; }")),
    tags$style(HTML(".fa-question-circle { font-size: 20px; }")),
    tags$style(HTML(".fa-chevron-circle-down { font-size: 15px; }")),
    tags$style(HTML(".fa-bell { font-size: 17px; }")),
    tags$style(HTML(".fa-check { font-size: 14px; }")),
    tags$style(HTML(".fa-times { font-size: 14px; }")),
    #tags$head(tags$style(".html-widget.gauge svg {height: 400px; width: 800px;}")),
    
    tags$style(HTML('
                       /* logo */
                       .skin-blue .main-header .logo {
                       background-color: #008080;
                       height: 60px;
                       }

                       /* logo when hovered */
                       .skin-blue .main-header .logo:hover {
                       background-color: #008080;
                       }

                       /* navbar (rest of the header) */
                       .skin-blue .main-header .navbar {
                       background-color: #008080;
                       align : left;
                       }

                       /* active selected tab in the sidebarmenu */
                       .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                       background-color: #767676;
                       }
                                 
                      
                       ')
    ),
    
    tags$style(HTML('
                       /* change size of icons in sub-menu items */
                      .sidebar .sidebar-menu .treeview-menu>li>a>.fa {
                      font-size: 15px;
                      }

                      .sidebar .sidebar-menu .treeview-menu>li>a>.glyphicon {
                      font-size: 13px;
                      }

                      /* Hide icons in sub-menu items */
                      .sidebar .sidebar-menu .treeview>a>.fa-angle-left {
                      display: none;
                      } 
                      '
    )),
    
    tags$style( HTML("hr {border-top: 1px solid #000000;}") ),
    
    ## to not show error message in shiny
    tags$style( HTML(".shiny-output-error { visibility: hidden; }") ),
    tags$style( HTML(".shiny-output-error:before { visibility: hidden; }") ),
    
    ## heand dropdown menu size
    tags$style(HTML('.navbar-custom-menu>.navbar-nav>li>.dropdown-menu { width:100px;}')), 
    tags$style(HTML('.navbar-custom-menu>.navbar-nav>li:last-child>.dropdown-menu { width:10px; font-size:10px; padding:1px; margin:1px;}')),
    tags$style(HTML('.navbar-custom-menu> .navbar-nav> li:last-child > .dropdown-menu > h4 {width:0px; font-size:0px; padding:0px; margin:0px;}')),
    tags$style(HTML('.navbar-custom-menu> .navbar-nav> li:last-child > .dropdown-menu > p {width:0px; font-size:0px; padding:0px; margin:0px;}'))
  ),
  
### tab
#----------------------------------------------------------------------------
  tabItems(
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
                  width=4,title="Recovered",background = "olive"),
              
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
          
          shinyjs::hidden( div( id = "load_more_message",
                                tags$hr(),
                                tags$h1("Loading...", align = "center")  )
          )
  ),
  tabItem(tabName = "about",
          fluidRow(
            column(width = 6, align = 'left', 
                   h2("Les données", style= "text-decoration: underline"),
                   tags$a(href='https://ramikrispin.github.io/coronavirus/',
                          tags$img(src = 'coronavirus.png', width=100)),
                   p("Les données sont issues du package", a("coronavirus") , "entretenu par", a(href = "https://github.com/RamiKrispin/coronavirus", "Rami Krispin.")),
                    br(),
                  p("Les données brutes proviennent du dépôt GitHub de JHU CCSE (Johns Hopkins University Center for Systems Science and Engineering)"),
                  img(src="enterprise-medicine.logo.small.horizontal.white.581be190.png", 
                      width=120, style="background-color:#002d72;padding: 0.8em;"),
            ),
            column(width = 6, 
                   h2("Auteur", style= "text-decoration: underline"),
                    tags$img(src = 'avatar.png', width=80),
                   p(a(alt="Ayao Nomenyo", "Ayao Nomenyo"),
                     "Statisticien Programmeur, Développeur d'applications Shiny"),
                   p(a(href="https://www.linkedin.com/in/ayao-nomenyo/", "LinkedIn")),
                   p(a(href="http://ayaowebsite.rbind.io/", "Homepage")),
                   img(src="github.png", width=80),
                   p(a(href="https://github.com/Ayao12/xxxx", "Tout le code pour ce projet"), "peut être trouvé sur github")
                   ),
             )
          )
)
)
ui <- shinydashboardPlus::dashboardPage(header, sidbar, body)


## Setup Shiny app back-end components
#------------------------------------------------------------------------------



server <- shinyServer(function(input, output, session) {
  
  # colscale <- c(semantic.dashboard::semantic_palette[["red"]], 
  #               semantic.dashboard::semantic_palette[["green"]], 
  #               semantic.dashboard::semantic_palette[["blue"]])
  
  
  
  ###  VALUES BOX
  #---------------------------------------------------------------------------
  
  output$total_confirmed_Af <- renderValueBox({
    valueBox(
      new_confirmed,
      strong(paste0("Total des Cas confirmés : ", max(covidAfrica$date))),
      icon = tags$i(class = "fas fa-users", style="font-size: 100px; color: white")
    )
  })
  
  output$recovered_covd <- flexdashboard::renderGauge({
    flexdashboard::gauge(
      new_recovered, min = 0, max = sum(corona_latest$confirmed),  
      flexdashboard::gaugeSectors(
        success = c(80, 100), warning = c(40, 79), danger = c(0, 39)
      ), abbreviate = FALSE)  
    
  })
  
  
  output$deaths_covd <- flexdashboard::renderGauge({
    
    flexdashboard::gauge(new_deaths, min = 0, max = sum(corona_latest$confirmed)
                         ,flexdashboard::gaugeSectors(
                           danger = c(80, 100), warning = c(40, 79), success = c(0, 39)
                         ), abbreviate = FALSE)
    
  })
  
  
  ### CHART GRAPH
  #---------------------------------------------------------------------------
  
  output$covid_africa <- renderHighchart({
    corona_latest %>% 
      janitor::clean_names()  %>%
      group_by(country) %>% 
      summarise(total_confirmed = sum(confirmed, na.rm = TRUE)) %>% 
      filter(total_confirmed > 0) %>%
      mutate(log_total_confirmed = log(total_confirmed)) -> countries
    
    highchart() %>%
      hc_add_series_map(africa_map, countries, 
                        value = 'log_total_confirmed',
                        name = "Coronavirus, Nouveau cas",
                        joinBy = c('name','country'))  %>%
      hc_mapNavigation(enabled = TRUE) %>%
      hc_colors(c("darkorange", "darkgray")) %>% 
      hc_colorAxis(stops = color_stops()) %>% 
      hc_legend(align = "right", verticalAlign = "center", x= -100,
                y = 230, layout = "vertical") %>% 
      hc_title(text = "Les Pays exposés au COVID19 en AFRIQUE") %>% 
      hc_subtitle(text = 'Avec le log du nombre de cas confirmé') 
  })
  
  
  output$count_most_affect <- renderHighchart({
    corona_latest %>% 
      group_by(country) %>% 
      summarise(total_confirmed = sum(confirmed)) %>% 
      arrange(desc(total_confirmed)) %>% 
      head(10) %>% 
      hchart("bar", hcaes(x = country,  y =total_confirmed),
             name = "Nombre de cas détecté") %>%
      hc_yAxis(title = list(text = " "), labels = list(format = "{value}"))%>%
      hc_xAxis(title = list(text = " "))%>%
      hc_add_theme(hc_theme_smpl())
  })
  
  output$reg_most_affect <- renderHighchart({
    corona_latest %>% 
      group_by(subregion) %>% 
      summarise(total_confirmed = sum(confirmed)) %>% 
      arrange(desc(total_confirmed)) %>% 
      head(10) %>% 
      hchart("bar",hcaes(x = subregion,  y =total_confirmed),
             name = "Nombre de cas détecté") %>%
      hc_yAxis(title = list(text = " "))%>%
      hc_xAxis(title = list(text = " "), labels = list(format = "{value}"))%>%
      hc_add_theme(hc_theme_smpl())
  })
  
  
  output$tendance_confirmed <- renderHighchart({
    if (input$country %in% "Africa") {
      covidAfrica %>%
        group_by(date) %>%
        mutate(confirmed = sum(confirmed), 
               recovered = sum(recovered),
               death = sum(death),
        ) %>%
        ungroup() %>% 
        select(date, country,input$type_plot) %>% 
        reshape2::melt(
          , id.vars = c("date", "country"), variable.name = "type"
          , value.name = "cases") %>% 
        select(date,cases , type) %>% 
        unique.data.frame() %>%
        filter((input$dates[1] <= date) & (date <= input$dates[2])) %>% 
        hchart('line', hcaes(x = "date", y = "cases", group = "type", color = "type")) %>% 
        hc_yAxis(title = list(text = " "), labels = list(format = "{value}")) %>%
        hc_colors(col_vector[which( type_vector  %in% input$type_plot)])
    }
    else{
      covidAfrica %>%
        select(date, country, input$type_plot) %>%
        filter(country %in% input$country) %>%
        filter((input$dates[1] <= date) & (date <= input$dates[2])) %>% 
        reshape2::melt(
          , id.vars = c("date", "country")
          , variable.name = "type"
          , value.name = "cases") %>% 
        select(date,cases , type) %>% 
        unique.data.frame() %>%
        filter((input$dates[1] <= date) & (date <= input$dates[2])) %>% 
        hchart('line', hcaes(x = "date", y = "cases", group = "type", color = "type")) %>% 
        hc_yAxis(title = list(text = " "), labels = list(format = "{value}")) %>%
        hc_colors(col_vector[which( type_vector  %in% input$type_plot)])
    }
    
  })
  
  
  
  
  ### SECTION : BOX INFO 
  #---------------------------------------------------------------------------
  
  output$n_confirmed <- renderInfoBox({
    
    if (input$country == "Africa"){
      infoBox("Nouveaux cas", new_confirmed, icon = tags$i(class = "fas fa-users", style = "font-size: 40px; color: white"),
              color = "light-blue", fill = TRUE)
    }else{
      confirmed_last_day <- covidAfrica %>% 
        subset(date==max(covidAfrica$date) & country== input$country, select = c(confirmed)) %>% 
        as.numeric()
      infoBox(
        "Nouveaux cas", strong(confirmed_last_day, align = "center", style="font-size: 40px"),
        icon = tags$i(class = "fas fa-users", style = "font-size: 50px; color: white"),
        color = "light-blue", fill = TRUE)
    }
  })
  
  
  
  output$n_recovered <- renderInfoBox({
    
    if (input$country == "Africa"){
      infoBox("Guérison", new_recovered, icon = tags$i(class = "fas fa-users", style = "font-size: 40px; color: white"),
              color = "purple", fill = TRUE)
    }else{
      recovered_last_day <- covidAfrica %>% 
        subset(date==max(covidAfrica$date) & country== input$country, select = c(recovered)) %>% 
        as.numeric()
      infoBox(
        "Guérison", strong(recovered_last_day, align = "center", style="font-size: 40px"),
        icon = tags$i(class = "fas fa-users", style = "font-size: 40px; color: white"),
        color = "purple", fill = TRUE)
    }
  })
  
  
  output$n_deaths <- renderInfoBox({
    if (input$country == "Africa"){
      infoBox("Décès", new_deaths, icon = tags$i(class = "fas fa-user-nurse", style = "font-size: 40px; color: white"),
              color = "yellow", fill = TRUE)
    }else{
      confirmed_last_day <- covidAfrica %>% 
        subset(date==max(covidAfrica$date) & country== input$country, select = c(death)) %>% 
        as.numeric()
      infoBox(
        "Décès",  strong(confirmed_last_day, align = "center", style="font-size: 40px; width:500px; height:150px"),
        icon = icon("far fa-tombstone"),
        color = "yellow", fill = TRUE)
    }
  })
  

  output$country_choice <- renderText({
    if (input$country == "Africa"){
      paste0("COVID 19 en  ", "Afrique" , " : " , max(covidAfrica$date))
    }else{
      paste0("COVID 19, ", input$country , " : " , max(covidAfrica$date))}
    
  })
  
  ## remove the waiting message -- 
  removeUI( selector = '#main_wait_message' )
  
  ### DATA TABLE
  #---------------------------------------------------------------------------
  dataoutoup <- reactive({
    if (input$country == "Africa") {
      covidAfrica %>% 
        arrange(desc(date)) %>% 
        filter((input$dates[1] <= date) & (date <= input$dates[2]))
    }
    else{
      covidAfrica %>% 
        arrange(desc(date)) %>% 
        filter(country == input$country) %>% 
        filter((input$dates[1] <= date) & (date <= input$dates[2]))
    }
  })
  
  
  output$carstable <- renderDataTable(
    datatable( dataoutoup(),
               rownames = F,
               filter = c("top"),
               extensions = c('Buttons'
                              #, 'FixedColumns'
               ),
               options = list(dom = 'Bfltp', #'Bltp',# 'Bt',
                              buttons = c('copy', 'csv', 'excel', 'pdf', 'print') #, pageLength = -1, 
                              ,scrollX = TRUE
                              #,fixedColumns = list(leftColumns = 2) 
                              ,autoWidth = T
                              ,pageLength = 10
                              ,lengthMenu = list(c(10, 50, 100,  -1), list('10', '50', '100', 'All'))
                              ,searchHighlight = TRUE,
                              search = list(regex = TRUE, caseInsensitive = TRUE )
               ) 
    )                                   
  )
})

## Render Shiny app 
#------------------------------------------------------------------------------

shinyApp(ui, server)


