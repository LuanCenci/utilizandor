tanimoto <- function(x, similarity=F) {
  res<-sapply(x, function(x1){
    sapply(x, function(x2) {i=length(which(x1 & x2)) / length(which(x1 | x2)); ifelse(is.na(i), 0, i)})
  })
  if(similarity==T) return(res)
  else return(1-res)
}

x12 <- data.frame(Samp1=c(0,0,0,1,1,1,0,0,1), Samp2=c(1,1,1,1,1,1,0,0,1), Samp3=c(1,0,0,1,0,0,0,0,0), Samp4=c(1,1,1,1,1,1,1,1,1))
tanimoto(x12)
