setwd('C:/Users/duffzord/Documents/R-DEV/FIAP/Geolocalização')


bus_position <- data.frame(
  onibus_id  <- as.character(),
  lat <- as.numeric(),
  long <- as.numeric(),
  sentido <- as.character(),
  tempo <- as.numeric()
)

bus_position <- rbind(bus_position,
                      c("onibus_id" = 1,
                        "lat" = -13.15486151,
                        "long" = -72.52951977,
                        "sentido" = "descendo",
                        "tempo" = 0)
                      )

bus_position <- rbind(bus_position,
                      c("onibus_id" = 2,
                        "lat" = -13.1569843510655,
                        "long" = -72.5321116669356,
                        "sentido" = "subindo",
                        "tempo" = 0)
                      )

bus_position <- rbind(bus_position,
                      c("onibus_id" = 3,
                        "lat" = -13.1631990927883,
                        "long" = -72.537918455202,
                        "sentido" = "descendo",
                        "tempo" = 0)
                      )

bus_position <- rbind(bus_position,
                      c("onibus_id" = 4,
                        "lat" = -13.1670757781198,
                        "long" = -72.5412343344586,
                        "sentido" = "subindo",
                        "tempo" = 0)
                      )

colnames(bus_position) <- c("onibus_id", "lat", "long", "sentido", "tempo")

save(bus_position, file = 'C:/Users/duffzord/Documents/R-DEV/FIAP/Geolocalização/bus_position.RData')
load('bus_position.RData')
