
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
                        yes  = mean(`Escribe tu edad exacta`,
                                    na.rm = T),
                        no   = `Escribe tu edad exacta`)) %>% 
  relocate(edad2, .before = Sexo)



### Eliminar la fila completa

# DF2 <- DF %>% na.omit()
# DF2 <- na.omit(DF)

nrow(DF)
nrow(DF2)


## Estandarización de variables

### Normalización

DF$`Escribe tu edad exacta`
scale(DF$`Escribe tu edad exacta`)
DF$`Escribe tu edad exacta` %>% scale()


# Crear una nueva columnas que se llame edad3
# que contenga los valores normalizados de edad2
# y además ubicarla después de edad 2
DF3 <- DF2 %>% 
  mutate(edad3 = scale(edad2)) %>% 
  relocate(edad3, .after = edad2)



### Convertir a rango
library(scales)

rescale(DF3$edad2)
# Creando un DF (tbl) temporal
tibble(
  edad2 = DF3$edad2,
  edad4 = rescale(DF3$edad2)
)



DF3 <- DF3 %>% 
  mutate(edad4 = rescale(edad2)) %>% 
  relocate(edad4, .after = edad3)


## Agrupaciones

### Variables numéricas

DF3$edad2
cut(x = DF3$edad2,
    breaks = c(-Inf, 18, 23, Inf),
    labels = c("18 o menos", "19 a 23", "Más de 23"))

# Creando un DF (tbl) temporal
tibble(
  edad2 = DF3$edad2,
  edad5 = cut(x = DF3$edad2,
              breaks = c(-Inf, 18, 23, Inf),
              labels = c("18 o menos", "19 a 23", "Más de 23"))
)



DF4 <- DF3 %>% 
  mutate(edad5 = cut(x = edad2,
                     breaks = c(-Inf, 18, 23, Inf),
                     labels = c("18 o menos", "19 a 23", "Más de 23"))) %>% 
  relocate(edad5, .after = edad4) %>% 
  # convirtiendo a tibble
  as_tibble()



### Variables categóricas

DF4[,11] %>% unique()
tibble(
  original = DF4[,11],
  t2b      = ifelse(test = DF4[,11] == "Totalmente verdadero" |
                      DF4[,11] == "Un poco verdadero",
                    yes = "SI",
                    no = "NO")
)



# (paréntesis) Bucle

## Paso 1: Crear un vector que contenga los nombres
## de las variables donde se va a aplicar el bucle

frases <- DF4 %>% 
  # selecciona columnas
  select(starts_with("Según tu")) %>% 
  colnames()

frases

## Backup del DF
DF5 <- DF4


### Paso2: Ejecutar el bucle

for (variable in frases) {
  
  DF5[,variable] <-ifelse(
    test = DF5[,variable] == "Totalmente verdadero" |
      DF5[,variable] == "Un poco verdadero",
    yes = 1,
    no = 0
  ) 
  
}



# Manipulación de data frames ----

class(DF5)
DF5 %>% as_tibble()

## Función select ----

DF5 %>% select(Sexo)
DF5 %>% select(Sexo, `Escribe tu edad exacta`)
# eliminando 1 columna
DF5 %>% select(-`Marca temporal`)
# eliminando más de 1 columna 
DF5 %>% select(!c(`Marca temporal`,
                  `¿Estás estudiando en algún colegio, universidad o instituto?`))
DF5 %>% select(starts_with("edad"))
DF5 %>% select(ends_with("00"))
DF5 %>% select(contains("forma de"))



## Función filter ----

DF5 %>%
  filter(Sexo == "Mujer") %>% 
  select(Sexo)



DF5 %>% 
  select(Sexo) %>% 
  filter(Sexo == "Hombre")



DF5 %>% 
  select(`Escribe tu edad exacta`) %>% 
  filter(`Escribe tu edad exacta` >= 20)

DF5 %>% 
  filter(`Escribe tu edad exacta` >= 18 &
           `Escribe tu edad exacta` <= 21) %>% 
  select(`Escribe tu edad exacta`) 
  


DF5 %>% 
  filter(between(`Escribe tu edad exacta`,
                 left = 18,
                 right = 21)) %>% 
  select(`Escribe tu edad exacta`) 



DF5 %>% 
  filter(Sexo == "Mujer",
         between(`Escribe tu edad exacta`,
                 left = 15,
                 right = 18)) %>% 
  select(`Escribe tu edad exacta`,
         Sexo)



## Cambio de nombre de columnas ----
DF6 <- DF5
colnames(DF6)



### APPS
#### Paso1: Crear un vector con los nuevos nombres
apps <- c("TikTok", "Instagram", "Facebook", "YouTube")

#### Paso2: Reemplazar los nombres
colnames(DF6)[35:38] <- apps



### Frases
#### Paso1: Crear un vector con los nuevos nombres
frases2 <- frases %>% 
  as_tibble() %>% 
  
  separate(col = "value",
           into = c("No sirve", "Me sirve"),
           sep ="\\[") %>% 
  select("Me sirve") %>% 
  
  separate(col = "Me sirve",
           into = c("Me sirve", "No sirve"),
           sep = "\\]") %>% 
  select("Me sirve") %>% 
  
  as_vector()



## Pivotado ----

### Pivot longer

DF7 <- DF6 %>%
  select(`Marca temporal`, Sexo, all_of(apps)) %>% 
  pivot_longer(cols = apps,
               names_to = "apps",
               values_to = "time")



### Pivot wider

DF8 <- DF7 %>% 
  pivot_wider(names_from = "apps",
              values_from = "time")



### (Transformación de hora a numero)

# strsplit separa textos

strsplit(x = DF7$time,
         split = ":") %>% 
  head()



# transformación

DF7$time <- sapply(X = strsplit(x = DF7$time,
                                split = ":"),
                   
                   function(x){
                     
                     x <- as.numeric(x)
                     x[1] + x[2]/60 + x[3]/60^2
                     
                   })



## Graficas con GGPLOT ----

## Boxplots

# Boxplot feo
boxplot(DF7$time, range = 3)



# boxplot bonito
library(plotly)
library(tidyquant)

boxplot <- ggplotly(
  DF7 %>% 
    ggplot(mapping = aes(x = apps,
                         y = time,
                         fill = apps)) +
    geom_boxplot() +
    labs(x = "", y = "Promedio en horas",
         title = "Uso de redes sociales") +
    theme_classic() +
    theme(legend.position = "none")
)


ggplotly(
  DF7 %>% 
    filter(Sexo %in% c("Hombre", "Mujer")) %>% 
    ggplot(mapping = aes(x = Sexo,
                         y = time,
                         fill = apps)) +
    geom_boxplot() +
    facet_wrap(~ apps, nrow = 1) +
    labs(x = "", y = "Promedio en horas",
         title = "Uso de redes sociales") +
    theme_minimal() +
    scale_fill_tq() +
    theme(legend.position = "none")
)



# violin chart

ggplotly(
  DF7 %>% 
    ggplot(mapping = aes(x = apps,
                         y = time,
                         fill = apps)) +
    geom_violin() +
    labs(x = "", y = "Promedio en horas",
         title = "Uso de redes sociales") +
    theme_classic() +
    theme(legend.position = "none")
)



# gráfico de densidad, para ver distribución

DF7 %>% 
  filter(Sexo %in% c("Hombre", "Mujer")) %>% 
  
  ggplot(mapping = aes(x = time,
                       fill = Sexo)) +
  
  geom_density(alpha = 0.5,
               color = "#787878") +
  
  labs(title = "Distribución del consumo de RRSS",
       subtitle = "Consumo en los últimos 7 días",
       caption = "Conclusión: YouTube es la app menos usada") +
  
  facet_wrap(~ apps, nrow =2) +
  
  theme_minimal() +
  
  scale_fill_manual(values = c("#8ecae6",
                               "#ffb703")) +
  
  theme(plot.title = element_text(face = "bold",
                                  size = 12,
                                  color = "#023047"),
        legend.position = "top",
        #quitar lineas intermedias
        panel.grid.minor = element_blank())



## Detección de outliers ----

### Boxplots
boxplot

DF7.1 <- DF7 %>% 
  mutate(outlier = case_when(
    
    apps == "Facebook"  & time > 10    ~ "SI",
    apps == "Instagram" & time > 13.28 ~ "SI",
    apps == "TikTok"    & time > 15.58 ~ "SI",
    apps == "YouTube"   & Sexo == "Hombre" & time > 16  ~ "SI",
    apps == "YouTube"   & Sexo == "Mujer"  & time > 5 ~ "SI",
    .default = "NO"
    
  )) %>% na.omit()



## Uso de group_by & summarise

DF7.1 %>%
  filter(outlier == "NO") %>%
  group_by(apps, Sexo) %>% 
  summarise(media =      mean(time),
            mediana =    median(time),
            desviación = sd(time),
            min =        min(time),
            max =        max(time)) %>% 
  ungroup()



DF7.1 %>%
  filter(outlier == "NO") %>%
  group_by(apps, Sexo) %>% 
  summarise(media = mean(time)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = Sexo,
              values_from = media)



## Valores normalizados

DF7.1 %>% 
  group_by(apps) %>% 
  mutate(time_z = scale(time),
         outlier = ifelse(test = time_z > 2,
                          yes = "SI",
                          no = "NO")) %>%
  filter(outlier == "NO") %>% 
  summarise(media = mean(time))



















