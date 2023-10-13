
library(ggcorrplot)
library(FactoMineR)

# Matriz de correlaciones ----

frases3 <- as.vector(frases2)

colnames(DF6)[10:33] <- frases3

r <- cor(x = DF6 %>% select(all_of(frases3)),
         method = "spearman")


# Gráfico de matriz de correlaciones

ggcorrplot(corr = r,
           tl.cex = 13)
