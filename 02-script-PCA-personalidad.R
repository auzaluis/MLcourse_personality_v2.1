
library(ggcorrplot)
library(FactoMineR)

# Matriz de correlaciones ----

frases3 <- as.vector(frases2)

colnames(DF6)[10:33] <- frases3

r <- cor(x = DF6 %>% select(all_of(frases3)),
         method = "spearman")


# Gráfico de matriz de correlaciones

ggplotly(
  
  ggcorrplot(corr = r,
             tl.cex = 13,
             # type = "upper",
             show.legend = F,
             colors = c("red", "white", "blue")) +
    
    theme(axis.text.x = element_blank(),
          panel.grid.major.x = element_blank())
  
)


# PCA: Principal component analysis

## Dimensión 1: Tradición
tradicion <- frases3[c(9,14)]


## Crear la dimension
PCA.tradicion <- FactoMineR::PCA(
  X = DF6 %>% select(all_of(tradicion)),
  ncp = 1
)


## Eigenvalues y var. explicada
PCA.tradicion$eig

## Correlación entre la dimensión y las var originales
PCA.tradicion$var$cor


## Valores de la dimension
PCA.tradicion$ind$coord %>% head()


## Comparar los valores orig con la dimensión
tibble(
  dim = PCA.tradicion$ind$coord * -1,
  DF6 %>% select(all_of(tradicion))
) %>% View()



## Dimensión 2: Social
social <- frases3[c(20,23)]


## Crear la dimension
PCA.social <- FactoMineR::PCA(
  X = DF6 %>% select(all_of(social)),
  ncp = 1
)


## Eigenvalues y var. explicada
PCA.social$eig

## Correlación entre la dimensión y las var originales
PCA.social$var$cor


## Valores de la dimension
PCA.social$ind$coord %>% head()


## Comparar los valores orig con la dimensión
tibble(
  dim = PCA.social$ind$coord,
  DF6 %>% select(all_of(social))
) %>% View()



## Dimensión 3: Aventura
aventura <- frases3[c(7,8)]


## Crear la dimension
PCA.aventura <- FactoMineR::PCA(
  X = DF6 %>% select(all_of(aventura)),
  ncp = 1
)


## Eigenvalues y var. explicada
PCA.aventura$eig

## Correlación entre la dimensión y las var originales
PCA.aventura$var$cor


## Valores de la dimension
PCA.aventura$ind$coord %>% head()


## Comparar los valores orig con la dimensión
tibble(
  dim = PCA.aventura$ind$coord,
  DF6 %>% select(all_of(aventura))
) %>% View()



## Dimensión 4: introversión
introversion <- frases3[c(11,21,22)]


## Crear la dimension
PCA.introversion <- FactoMineR::PCA(
  X = DF6 %>% select(all_of(introversion)),
  ncp = 1
)


## Eigenvalues y var. explicada
PCA.introversion$eig

## Correlación entre la dimensión y las var originales
PCA.introversion$var$cor


## Valores de la dimension
PCA.introversion$ind$coord %>% head()


## Comparar los valores orig con la dimensión
tibble(
  dim = PCA.introversion$ind$coord,
  DF6 %>% select(all_of(introversion))
) %>% View()



## Dimensión 5: éxito
exito <- frases3[c(4,5,19)]


## Crear la dimension
PCA.exito <- FactoMineR::PCA(
  X = DF6 %>% select(all_of(exito)),
  ncp = 1
)


## Eigenvalues y var. explicada
PCA.exito$eig

## Correlación entre la dimensión y las var originales
PCA.exito$var$cor


## Valores de la dimension
PCA.exito$ind$coord %>% head()


## Comparar los valores orig con la dimensión
tibble(
  dim = PCA.exito$ind$coord * -1,
  DF6 %>% select(all_of(exito))
) %>% View()



# Añadir las dimensiones al data.frame

DF11 <- DF6 %>% 
  
  mutate(tradicion =    PCA.tradicion$ind$coord *-1,
         social =       PCA.social$ind$coord *-1,
         aventura =     PCA.aventura$ind$coord *-1,
         introversion = PCA.introversion$ind$coord,
         exito =        PCA.exito$ind$coord *-1)




























