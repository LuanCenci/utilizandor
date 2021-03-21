install.packages("rebus")

library(tidyverse)
library(rvest)
library(stringr)
library(rebus)
library(lubridate)

webpage <- read_html("https://motorsport.uol.com.br/f1/drivers/")

results <- webpage %>% html_nodes(".ms-item_info")

records <- vector("list",length = length(results))

for (i in seq_along(results)){
  
  title <- str_sub(results[i] %>% html_nodes(".ms-item_title") %>% html_text(trim = TRUE),1,-1)
  number <- str_sub(results[i] %>% html_nodes(".ms-item-driver_number") %>% html_text(trim = TRUE),1,-1)
  age <- str_extract(str_sub(results[i] %>% html_nodes(".ms-item-driver_info-row--age") %>% html_text(trim = TRUE),1,-1),"\\d+")
  team <- str_sub(results[i] %>% html_nodes(".ms-item-driver_info-row .ms-item_link") %>% html_text(trim = TRUE),1,-1)
  birth <- str_sub(results[i] %>% html_nodes(".ms-item--driver .ms-item_title, .ms-item-driver_info") %>% html_text(trim = TRUE),1,-1)
  
  country <- str_remove(word(birth,-1,sep =fixed(":"))," ")
  
  birth <- str_extract(birth,"\\d+-\\d+-\\d+")
  
  records[[i]] = data_frame(numero = as.integer(number),
                            nome = title,
                            nascimento = as_date(birth),
                            idade = as.integer(age),
                            pais = country,
                            escuderia = team)
  
}

df <- bind_rows(records)
df

ggplot(df,aes(y=pais,fill = c("#CD5C5C"))) + geom_bar() + 
  theme(panel.background = element_rect(fill = "white", colour = "white"),
        legend.position = "none") +
  labs(x = "Quantidades de pilotos",
       y = "Países com pilotos")

round(mean(df$idade))

idades <- count(df,idade < round(mean(df$idade)))
colnames(idades) <- c("condicao","quantidade")

idades$condicao <- c("Abaixo \nda \nmédia", "Acima \nda \nmédia")

idades <- idades %>%
          arrange(desc(condicao)) %>%
          mutate(prop = quantidade / sum(idades$quantidade) * 100) %>%
          mutate(ypros = cumsum(prop) - 0.5*prop)
  

ggplot(idades,aes(x="",y = prop, fill = condicao)) +
  geom_bar(stat = "identity",width=1,color = "white") +
  coord_polar("y",start = 0)+
  theme_void() +
  theme(legend.position="none") +
  geom_text(aes(y = ypros,label = condicao),color= "white",size = 5.5)

df %>% group_by(escuderia)

dplyr::filter(df,idade == max(idade))

dplyr::filter(df,idade == min(idade))


equipesIdades <- df %>%
                 group_by(escuderia) %>%
                 summarise(somaIdade = sum(idade),
                           mediaIdade = mean(idade),
                           maxIdade = max(idade),
                           minIdade = min(idade),
                           diferencaIdade = max(idade) -  min(idade))

ggplot(data = equipesIdades, aes(x= escuderia,y = somaIdade)) +
  geom_bar(stat = "identity",aes(fill = c("#CD5C5C"))) + 
  coord_flip() +
  theme(panel.background = element_rect(fill = "white", colour = "white"),
        legend.position = "none")


ggplot(data = equipesIdades, aes(x= escuderia,y = diferencaIdade)) +
  geom_bar(stat = "identity",aes(fill = c("#CD5C5C"))) + 
  coord_flip() +
  theme(panel.background = element_rect(fill = "white", colour = "white"),
        legend.position = "none")

ggplot(data = equipesIdades, aes(x= escuderia,y = mediaIdade)) +
  geom_bar(stat = "identity",aes(fill = c("#CD5C5C"))) + 
  coord_flip() +
  theme(panel.background = element_rect(fill = "white", colour = "white"),
        legend.position = "none")
