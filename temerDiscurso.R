library(rvest)

discursoTemer <- read_html("http://www2.planalto.gov.br/acompanhe-planalto/discursos/discursos-do-presidente-da-republica/discurso-do-presidente-da-republica-michel-temer-na-abertura-do-debate-geral-da-72o-sessao-da-assembleia-geral-da-onu")

textoDiscurso <- discursoTemer %>% html_nodes("p") %>% html_text()

cat(textoDiscurso)

library(tm)

#transformar os dados do texto em vetores
vs <- VectorSource(textoDiscurso)

#Coloca no formato de Corpus do pacote tm
temp<- VCorpus(vs,readerControl = list(language = "pt"))

#colocar tudo em minusculo
temp <- tm_map(temp,FUN = content_transformer(tolower))

# remover pontuação, stopwords e numeros
temp <- tm_map(temp,FUN = removePunctuation)
temp <- tm_map(temp,FUN = removeNumbers)
temp <- tm_map(temp,FUN = removeWords, words = stopwords("portuguese"))
temp <- tm_map(temp,FUN = stripWhitespace)

termosDocumentados <- DocumentTermMatrix(temp)

frequencia <- slam::colapply_simple_triplet_matrix(termosDocumentados,FUN=sum)
frequencia <- sort(frequencia,decreasing = TRUE)

library(ggplot2);library(magrittr);library(dplyr)

data.frame(palavras = names(frequencia), frequencia =unname(frequencia)) %>%
  filter(frequencia > 5) %>%
  mutate(palavras = reorder(palavras,frequencia))%>%
  ggplot(aes(palavras,frequencia)) +
  geom_col()+
  xlab("Palavras") +
  ylab("Frequencias")+
  coord_flip()
  
library(wordcloud)

wordcloud(temp,family = "times",min.freq = 5,max.words = 20,colors = brewer.pal(7,"Paired"))









