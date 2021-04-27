

server <- shinyServer(function(input, output, session) {
  
  colscale <- c(semantic.dashboard::semantic_palette[["red"]], 
                semantic.dashboard::semantic_palette[["green"]], 
                semantic.dashboard::semantic_palette[["blue"]])
  
  
  
###  VALUES BOX
#---------------------------------------------------------------------------

output$total_confirmed_Af <- renderValueBox({
    valueBox(
      confirmed,
      strong(paste0("Total des Cas confirmés : ", max(covidAfrica$date))),
      icon = tags$i(class = "fas fa-users", style="font-size: 100px; color: white")
    )
  })
  
output$recovered_covd <- flexdashboard::renderGauge({
    flexdashboard::gauge(
      new_confirmed, min = 0, max = sum(corona_latest$confirmed),  
      flexdashboard::gaugeSectors(
        success = c(80, 100), warning = c(40, 79), danger = c(0, 39)
      ))  
    
  })
  
  
output$deaths_covd <- flexdashboard::renderGauge({
    
    flexdashboard::gauge(deaths, min = 0, max = sum(corona_latest$confirmed)
                         ,flexdashboard::gaugeSectors(
                           danger = c(80, 100), warning = c(40, 79), success = c(0, 39)
                         ))
    
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
      hc_yAxis(title = list(text = " "))%>%
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
      hc_xAxis(title = list(text = " "))%>%
      hc_add_theme(hc_theme_smpl())
  })
  
  
  output$tendance_confirmed <- renderHighchart({
    if (input$country %in% "All Countries") {
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
        hc_yAxis(title = list(text = " ")) %>%
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
        hc_yAxis(title = list(text = " ")) %>%
        hc_colors(col_vector[which( type_vector  %in% input$type_plot)])
    }
    
  })
  
  
  
  
### SECTION : BOX INFO 
#---------------------------------------------------------------------------
  
  output$n_confirmed <- renderInfoBox({
    
    if (input$country == "All Countries"){
      infoBox("Nouveaux cas", confirmed, icon = tags$i(class = "fas fa-users", style = "font-size: 50px; color: white"),
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
    
    if (input$country == "All Countries"){
      infoBox("Guérison", recovered, icon = tags$i(class = "fas fa-users", style = "font-size: 50px; color: white"),
              color = "purple", fill = TRUE)
    }else{
      recovered_last_day <- covidAfrica %>% 
        subset(date==max(covidAfrica$date) & country== input$country, select = c(recovered)) %>% 
        as.numeric()
      infoBox(
        "Guérison", strong(recovered_last_day, align = "center", style="font-size: 50px"),
        icon = tags$i(class = "fas fa-users", style = "font-size: 50px; color: white"),
        color = "purple", fill = TRUE)
    }
  })
  
  
  output$n_deaths <- renderInfoBox({
    if (input$country == "All Countries"){
      infoBox("Décès", deaths, icon = tags$i(class = "fas fa-users", style = "font-size: 50px; color: white"),
              color = "yellow", fill = TRUE)
    }else{
      confirmed_last_day <- covidAfrica %>% 
        subset(date==max(covidAfrica$date) & country== input$country, select = c(death)) %>% 
        as.numeric()
      infoBox(
        "Décès",  strong(confirmed_last_day, align = "right", style="font-size: 50px"),
        icon = tags$i(class = "fas fa-users", style = "font-size: 50px; color: white"),
        color = "yellow", fill = TRUE)
    }
  })
  
  
  output$country_choice <- renderText({
    if (input$country == "All Countries"){
      paste0("COVID 19 en  ", "Afrique" , " : " , max(covidAfrica$date))
    }else{
      paste0("COVID 19 au ", input$country , " : " , max(covidAfrica$date))}
    
  })
  
  ## remove the waiting message -- 
  removeUI( selector = '#main_wait_message' )
  
### DATA TABLE
#---------------------------------------------------------------------------
  dataoutoup <- reactive({
    if (input$country == "All Countries") {
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

