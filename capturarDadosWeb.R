webCapture <- function(lur){
  
  library(xml2)
  library(rvest)
  library(stringr)
  library(tidyr)
  
  pagina <- read_html(lur)
  
  sb_tabela <- html_nodes(pagina, 'table')
  webTable <- html_table(sb_tabela)[[2]]
  return(webTable)
  
}