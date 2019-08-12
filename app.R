library('dplyr')
library('shiny')
library('leaflet') # maps
library('leaflet.extras') # maps
library('shinydashboard')

r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()

ui <- fluidPage(
  titlePanel("FIAP-04IA - Machu Picchu - Fila do Onibus"),
  leafletOutput("mymap"),
  p(),
  actionButton("syncBusPosition", "Atualizar o sentido do ônibus"),
  br(),
  br(),
  uiOutput("proximoOnibus"),
  uiOutput("nextBus"),
  br(),
  br(),
  box(width = 4,
      uiOutput("tempoBusTable")
  )
)

server <- function(input, output, session) {
  load('bus_position.RData')
  dirColors <-c("1"="#595490", "2"="#527525", "3"="#A93F35", "4"="#BA48AA")
  
  aguas_calientes_stop <- data.frame(lat = -13.154995, long =  -72.525267)
  
  IconSet <- awesomeIconList(
    bus_subindo_icon   = makeAwesomeIcon(icon= 'rocket', markerColor = 'green', iconColor = 'green', library = "fa"),
    bus_descendo_icon   = makeAwesomeIcon(icon= 'rocket', markerColor = 'red', iconColor = 'red', library = "fa")
  )
  
  
  macchu_picchu_fila <- data.frame(lat = -13.16596185499693, long =  -72.54285335540771)
  
  # busPosition <- eventReactive(input$goButton, {
  #   input$n
  # })
  
  output$mymap <- renderLeaflet({
    
    df_bus_position <- load('bus_position.RData')
    df_bus_position <- bus_position
    df_bus_position$tempo <- as.numeric(df_bus_position$tempo)
    df_bus_position$lat <- as.numeric(df_bus_position$lat)
    df_bus_position$long <- as.numeric(df_bus_position$long)
    df_bus_position <- df_bus_position %>% arrange(tempo)
    
    leaflet() %>%
      addTiles() %>%  # Add default OpenStreetMap map tiles
      addAwesomeMarkers(lng=df_bus_position$long[1], lat=df_bus_position$lat[1],
                        icon = ifelse(df_bus_position$sentido[1] == 'subindo', IconSet['bus_subindo_icon'], IconSet['bus_descendo_icon']),
                        label = ifelse(df_bus_position$sentido[1] == 'subindo', 'Subindo', 'Descendo')) %>%
      addAwesomeMarkers(lng=df_bus_position$long[2], lat=df_bus_position$lat[2],
                        icon = ifelse(df_bus_position$sentido[2] == 'subindo', IconSet['bus_subindo_icon'], IconSet['bus_descendo_icon']),
                        label = ifelse(df_bus_position$sentido[2] == 'subindo', 'Subindo', 'Descendo')) %>%
      addAwesomeMarkers(lng=df_bus_position$long[3], lat=df_bus_position$lat[3],
                        icon = ifelse(df_bus_position$sentido[3] == 'subindo', IconSet['bus_subindo_icon'], IconSet['bus_descendo_icon']),
                        label = ifelse(df_bus_position$sentido[3] == 'subindo', 'Subindo', 'Descendo')) %>%
      addAwesomeMarkers(lng=df_bus_position$long[4], lat=df_bus_position$lat[4],
                        icon = ifelse(df_bus_position$sentido[4] == 'subindo', IconSet['bus_subindo_icon'], IconSet['bus_descendo_icon']),
                        label = ifelse(df_bus_position$sentido[4] == 'subindo', 'Subindo', 'Descendo'))
  })
  
  #### Tempos do ônibus ####
  
  bus_time_data <- reactive({
    
    load('bus_position.RData')
    
    if (any(bus_position$sentido == "subindo")) {
      bus_possiveis_subindo <- filter(bus_position, sentido == "subindo")
      for (row in 1:nrow(bus_possiveis_subindo)) {
        howfar <- drive_time(
          address= paste(bus_possiveis_subindo$lat[row], bus_possiveis_subindo$long[row], sep = ','),
          dest="-13.16596185499693,-72.54285335540771",
          auth="standard_api", privkey="AIzaSyAN1z_0_ygouVwIwvYnxM6004DSUOak6fk", clean=FALSE,
          add_date='today', verbose=FALSE, travel_mode="driving",
          units="metric"
        )
        tempos_onibus_vec <- c("onibus_id" = bus_possiveis_subindo$onibus_id[row], "tempo" = howfar$time_mins)
        tempos_onibus <- rbind(tempos_onibus,tempos_onibus_vec)
        bus_position$tempo[which(bus_position$onibus_id == bus_possiveis_subindo$onibus_id[row])] <- howfar$time_mins
        colnames(tempos_onibus) <- c("onibus_id", "tempo")
      }
    } 
    
    row <- 1
    
    if (any(bus_position$sentido == "descendo")) {
      bus_possiveis_descendo <- filter(bus_position, sentido == "descendo")
      for (row in 1:nrow(bus_possiveis_descendo)) {
        howfar <- drive_time(
          address= paste(bus_possiveis_descendo$lat[row], bus_possiveis_descendo$long[row], sep = ','),
          dest="-13.154995,-72.525267",
          auth="standard_api", privkey="AIzaSyAN1z_0_ygouVwIwvYnxM6004DSUOak6fk", clean=FALSE,
          add_date='today', verbose=FALSE, travel_mode="driving",
          units="metric"
        )
        tempo_total_desembarque <- howfar$time_mins + 15
        howfar <- drive_time(
          address= "-13.154995,-72.525267",
          dest="-13.16596185499693,-72.54285335540771",
          auth="standard_api", privkey="AIzaSyAN1z_0_ygouVwIwvYnxM6004DSUOak6fk", clean=FALSE,
          add_date='today', verbose=FALSE, travel_mode="driving",
          units="metric"
        )
        tempo_total <- howfar$time_mins + tempo_total_desembarque
        tempos_onibus_vec <- c("onibus_id" = bus_possiveis_descendo$onibus_id[row], "tempo" = tempo_total)
        tempos_onibus <- rbind(tempos_onibus,tempos_onibus_vec)
        bus_position$tempo[which(bus_position$onibus_id == bus_possiveis_descendo$onibus_id[row])] <- howfar$time_mins <- tempo_total
        colnames(tempos_onibus) <- c("onibus_id", "tempo")
      }
    }
    
    tempos_onibus <- arrange(tempos_onibus, tempo)
    
    if (nrow(fila_nomes) <= 40) {
      fila_nomes <- cbind(fila_nomes, rep(tempos_onibus$tempo[1], nrow(fila_nomes)))
      colnames(fila_nomes) <- c("Posicao", "Nome", "Tempo.Espera")
    }
    
    if (nrow(fila_nomes) > 40 && nrow(fila_nomes) <= 80) {
      number_less80 <- nrow(fila_nomes) - 40
      time_vector <- c(rep(tempos_onibus$tempo[1], 40), rep(tempos_onibus$tempo[2], number_less80))
      fila_nomes <- cbind(fila_nomes, time_vector)
      colnames(fila_nomes) <- c("Posicao", "Nome", "Tempo.Espera")
    }
    
    return(tempos_onibus)
  })
  
  output$proximoOnibus <- renderText({
    as.character(paste0("Ônibus mais próximo: ", bus_time_data()$onibus_id[1]))
  })
  
  output$nextBus <-  renderText("Teste STring T3t4")
  
  #### Tabela Onibus Tempo ####
  
  output$tempoBusTable <- renderUI({
    df_bus_position <- load('bus_position.RData')
    df_bus_position <- bus_position
    df_bus_position$tempo <- as.numeric(df_bus_position$tempo)
    df_bus_position$lat <- as.numeric(df_bus_position$lat)
    df_bus_position$long <- as.numeric(df_bus_position$long)
    df_bus_position <- df_bus_position %>% arrange(tempo)
    
    if (length(df_bus_position) == 0 || nrow(df_bus_position) == 0)
      return(NULL)

    # Create a Bootstrap-styled table
    tags$table(class = "table",
               tags$thead(tags$tr(
                 tags$th("Color"),
                 tags$th("onibus_id"),
                 tags$th("sentido"),
                 tags$th("tempo")
               )),
               tags$tbody(
                 
                 # Linha 1
                 tags$tr(
                   tags$td(span(style = sprintf(
                     "width:1.1em; height:1.1em; background-color:%s; display:inline-block;",
                     ifelse(df_bus_position$sentido[1] == 'subindo', dirColors[2], dirColors[3])
                   ))),
                   tags$td(as.character(df_bus_position$onibus_id[1])),
                   tags$td(ifelse(df_bus_position$sentido[1] == 'subindo', "subindo", "descendo")),
                   tags$td(as.character(df_bus_position$tempo[1]))
                   
                 ),
                 
                 # Linha 2
                 tags$tr(
                   tags$td(span(style = sprintf(
                     "width:1.1em; height:1.1em; background-color:%s; display:inline-block;",
                     ifelse(df_bus_position$sentido[2] == 'subindo', dirColors[2], dirColors[3])
                   ))),
                   tags$td(as.character(df_bus_position$onibus_id[2])),
                   tags$td(ifelse(df_bus_position$sentido[2] == 'subindo', "subindo", "descendo")),
                   tags$td(as.character(df_bus_position$tempo[2]))
                 ),
                 
                 # Linha 3
                 tags$tr(
                   tags$td(span(style = sprintf(
                     "width:1.1em; height:1.1em; background-color:%s; display:inline-block;",
                     ifelse(df_bus_position$sentido[3] == 'subindo', dirColors[2], dirColors[3])
                   ))),
                   tags$td(as.character(df_bus_position$onibus_id[3])),
                   tags$td(ifelse(df_bus_position$sentido[3] == 'subindo', "subindo", "descendo")),
                   tags$td(as.character(df_bus_position$tempo[3]))
                   
                 ),
                 
                 # Linha 4
                 tags$tr(
                   tags$td(span(style = sprintf(
                     "width:1.1em; height:1.1em; background-color:%s; display:inline-block;",
                     ifelse(df_bus_position$sentido[4] == 'subindo', dirColors[2], dirColors[3])
                   ))),
                   tags$td(as.character(df_bus_position$onibus_id[4])),
                   tags$td(ifelse(df_bus_position$sentido[4] == 'subindo', "subindo", "descendo")),
                   tags$td(as.character(df_bus_position$tempo[4]))
                   
                 )
               )
    )
  })

  
}

shinyApp(ui, server)