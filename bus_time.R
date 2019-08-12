library("tibble")
library("dplyr")
library('dplyr') # data manipulation
library("placement")

#### Set path files ####
setwd('C:/Users/duffzord/Documents/R-DEV/FIAP/Geolocalização')
load('bus_position.RData')
fila_nomes <- read.csv2('fila_posicao_nomes.csv')

api_key_char <- "API KEY - GET ONE WITH GOOGLE"

#### Data columns ####

tempos_onibus <- data.frame("onibus_id" = as.character(),
                            "tempo" = as.numeric())

tempos_onibus_vec <- c("onibus_id" = as.character(),
                       "tempo" = as.numeric())

#### Calculate time to Machu Picchu ####

if (any(bus_position$sentido == "subindo")) {
  bus_possiveis_subindo <- filter(bus_position, sentido == "subindo")
  for (row in 1:nrow(bus_possiveis_subindo)) {
    howfar <- drive_time(
      address= paste(bus_possiveis_subindo$lat[row], bus_possiveis_subindo$long[row], sep = ','),
      dest="-13.16596185499693,-72.54285335540771",
      auth="standard_api", privkey=api_key_char, clean=FALSE,
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
      auth="standard_api", privkey=api_key_char, clean=FALSE,
      add_date='today', verbose=FALSE, travel_mode="driving",
      units="metric"
    )
    tempo_total_desembarque <- howfar$time_mins + 15
    howfar <- drive_time(
      address= "-13.154995,-72.525267",
      dest="-13.16596185499693,-72.54285335540771",
      auth="standard_api", privkey=api_key_char, clean=FALSE,
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

#### Save data table ####

save(bus_position, file = 'C:/Users/duffzord/Documents/R-DEV/FIAP/Geolocalização/bus_position.RData')


