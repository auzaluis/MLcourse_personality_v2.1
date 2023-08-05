
# Tema 01: Carga de datos ----

## Carga local
DF <- read.csv(file = "Personalidad y uso de apps (respuestas) - Respuestas de formulario 1.csv",
               sep = ",",
               check.names = F)


## Carga en línea
install.packages("gsheet")
library(gsheet)

url_google <- "https://docs.google.com/spreadsheets/d/1IQ_RxxTSmBKHTExlxboIRNlMov_F6RyqdcOPrflCv_w/edit?usp=sharing"

DF <- read.csv(text = gsheet2text(url = url_google),
               check.names = F)



# Estructura de un data frame ----
class(DF)
class(DF$`Escribe tu edad exacta`)
nrow(DF) # Cantidad de filas del DF
ncol(DF) # Cantidad de columnas de DF



# Transformación del data frame ----

## Valores perdidos (NA)

DF$`Escribe tu edad exacta`
is.na(DF$`Escribe tu edad exacta`)
summary(is.na(DF$`Escribe tu edad exacta`))


### Reemplazo por la media
install.packages("tidyverse")
library(tidyverse)

DF2 <- DF %>%
  mutate(edad2 = ifelse(test = is.na(`Escribe tu edad exacta`),
                        yes = mean(`Escribe tu edad exacta`,
                                   na.rm = T),
                        no = `Escribe tu edad exacta`))

# hacer un relocate







