#install.packages("tidyverse")
library(tidyverse)
library(ggplot2)
#install.packages("plotly")
library(plotly)

dados <- read.csv("C:/Users/winseven/Downloads/datasets_553928_1008865_simpsons_episodes (1).csv", sep = ",", stringsAsFactors = FALSE)
dados

nomes <- colnames(dados)

nomes     <- c( "identificador",
                "link_imagem",
                "avaliacao_IMDB",
                "votos_IMDB",
                "num_temporada",
                "num_serie",
                "exibicao_original_data",
                "exibicao_original_ano",
                "codigo_producao",
                "temporada",
                "titulo_episodio",
                "espectadores_usa_milions",
                "link_video",
                "visualizacoes")


colnames(dados) <- nomes

head(dados)

qtde <- table(dados$exibicao_original_ano)
qtde
barplot(qtde,
        main = "Episodios",
        xlab = "Anos",
        ylab = "Valores",
        border = "#ffffff",
        col = "#FEDC03",
        xlim = c(0,30))

qtde <- table(dados$num_temporada)
qtde

barplot(qtde,
        main = "Episodios",
        xlab = "Temporada",
        ylab = "Valores",
        border = "#ffffff",
        col = "#FEDC03",
        xlim = c(0, 30))

#Avaliacao 

summary(dados$avaliacao_IMDB)

boxplot(dados$avaliacao_IMDB,
        border = "#5D8AA8",
        col = "#FEDC03")

hist(dados$avaliacao_IMDB,
        border = "white",
        col = "#FEDC03",
        ylab = "Frequencia",
        xlab = "Distribuição",
        main = "Frequencia de notas.")

#Espectadores
summary(dados$espectadores_usa_milions)

boxplot(dados$espectadores_usa_milions, col = "#FEDC03")

    hist(dados$espectadores_usa_milions,
         border = "white",
         col = "#FEDC03",
         ylab = "Frequencia",
         xlab = "Distribuição",
         main = "Frequencia de espectadores \n americanos")

# Visualizações

summary(dados$visualizacoes)

boxplot(dados$visualizacoes, col = "#FEDC03")

hist(dados$visualizacoes,
     col = "#FEDC03",
     ylab = "Frequencia",
     xlab = "Distribuição",
     main = "Frequencia de Visualizações")

##  Votos do Imdb
summary(dados$votos_IMDB)

boxplot(dados$votos_IMDB, col = "#FEDC03")

hist(dados$votos_IMDB,
     col = "#FEDC03",
     ylab = "Frequencia",
     xlab = "Distribuição",
     main = "Frequencia de Votos IMDB")

### Verificar valores Faltantes
dados[!complete.cases(dados),]
vazios = dados[!complete.cases(dados),]

## Tratar Avaliações do IMDB
summary(dados$avaliacao_IMDB)

median(dados$avaliacao_IMDB, na.rm = TRUE)

dados[is.na(dados$avaliacao_IMDB),]$avaliacao_IMDB = median(dados$avaliacao_IMDB, na.rm = TRUE)

dados[is.na(dados$avaliacao_IMDB),]

### Verificar votos no IMDB
summary(dados$votos_IMDB)

median(dados$votos_IMDB, na.rm = TRUE)

dados[is.na(dados$votos_IMDB),]$votos_IMDB = median(dados$votos_IMDB, na.rm = TRUE)

dados[is.na(dados$avaliacao_IMDB),]

### Verificar Espectadores Americanos
summary(dados$espectadores_usa_milions)

median(dados$espectadores_usa_milions,na.rm = TRUE)

dados[is.na(dados$espectadores_usa_milions),]$espectadores_usa_milions = median(dados$espectadores_usa_milions, na.rm = TRUE)

dados[is.na(dados$espectadores_usa_milions),]

### Verificar dados de Visualizações
summary(dados$visualizacoes)

median(dados$visualizacoes,na.rm = TRUE)

dados[is.na(dados$visualizacoes),]$visualizacoes = median(dados$visualizacoes, na.rm = TRUE)

dados[is.na(dados$visualizacoes),]

## organizar os dados

ordenado <- dados[order(dados$num_serie),]

ggplot(ordenado, aes(x = num_serie, y = espectadores_usa_milions, colour = "#FEDC03")) +
    geom_line() +
    labs(y = "Espectadores", x = "Episódios") +
    theme(panel.background = element_rect(fill = "white", colour = "white"),
          legend.position = "none")

coef(lm(espectadores_usa_milions ~ num_serie, data = ordenado))

ggplot(ordenado, aes(x = num_serie, y = espectadores_usa_milions, colour = "#FEDC03")) +
    geom_line() +
    geom_abline(intercept = 21.22215198, slope = -0.03126384, colour = "#5D8AA8") +
    labs(y = "Espectadores", x = "Episódios") +
    theme(panel.background = element_rect(fill = "white", colour = "white"),
          legend.position = "none")

temporadas <- dados %>% count(temporada, sort = TRUE)

temporadas <- temporadas[order(temporadas$temporada),]
temporadas$temporada <- as.character(temporadas$temporada)

ggplot(temporadas, aes(x = temporada, y = n)) + geom_bar(stat = "identity")