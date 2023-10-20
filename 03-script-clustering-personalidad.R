
# install.packages("NbClust")
library(NbClust)
library(FactoMineR)

# Crear una vector con los nombres de las dimensiones

dimensiones <- DF11 %>%
  select(tradicion:exito) %>%
  colnames()


# Clusterización

clustering <- NbClust(
  data = DF11 %>% select(all_of(dimensiones)),
  distance = "euclidean",
  method = "ward.D2",
  index = "dunn"
    )

## Definición de la cantidad cluster

clustering$All.index



## Creación de 3 clusters

clustering2 <- NbClust(
  data = DF11 %>% select(all_of(dimensiones)),
  distance = "euclidean",
  method = "ward.D2",
  index = "dunn",
  
  # Limitar la iteración
  min.nc = 4,
  max.nc = 4
)



## tabla que muestra el peso de cada segmento
table(clustering2$Best.partition)



# Analizando los segmentos

## Creando el DF12

DF12 <- DF11 %>% 
  mutate(segmento = as.factor(clustering2$Best.partition))


summary(DF12$segmento)


## Convertiendo en rango 0-1
library(scales)

DF13 <- DF12 %>% 
  mutate_at(.vars = dimensiones,
            .funs = rescale)


## Summarise

DF14 <- DF13 %>% 
  group_by(segmento) %>% 
  summarise(tradicion = mean(tradicion),
            social = mean(social),
            aventura = mean(aventura),
            introversion = mean(introversion),
            exito = mean(exito)) %>% 
  column_to_rownames(var = "segmento")



## Mapa perceptual

FactoMineR::CA(X = DF14)



## Cruzar por el consumo de apps

### Convertir las horas en números

#### Creando una función para operar varias columnas

conv_funct <- function(var) {
  
  sapply(X = strsplit(x = var,
                      split = ":"),
         
         function(x){
           
           x <- as.numeric(x)
           x[1] + x[2]/60 + x[3]/60^2
           
         })
  
}



#### Aplicar la conv_funct a las apps

DF15 <- DF13 %>% 
  mutate_at(.vars = all_of(apps),
            .funs = conv_funct) %>% 
  
  # bautizar a los segmentos
  mutate(segmento = case_when(
    segmento == 1 ~ "tradicionales",
    segmento == 2 ~ "activos",
    segmento == 3 ~ "sociables",
    segmento == 4 ~ "introvertidos",
    .default = NA
  ))


## Ver el resumen de consumo de apps por segmento

DF16 <- DF15 %>% 
  group_by(segmento) %>% 
  summarise_at(.vars = apps,
               .funs = ~ mean(., na.rm = T)) %>% 
  column_to_rownames("segmento")

## Mapa perceptual por uso de apps

FactoMineR::CA(DF16)

round(prop.table(table(DF15$segmento, DF15$Sexo), margin = 1),
      digits = 2)

round(prop.table(table(DF15$segmento, DF15$Sexo), margin = 2),
      digits = 2)








