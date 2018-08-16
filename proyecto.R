library(stringr)

MR = rtweet::get_timeline("tv_monica", n=3200)$text
CF = rtweet::get_timeline("Cami_FloresO", n=3200)$text
JAN = rtweet::get_timeline("jananeme", n=3200)$text
JAK = rtweet::get_timeline("joseantoniokast", n=3200)$text

scores <- read.csv("scores.csv", header=FALSE)
scores$V1 = str_to_lower(as.character(scores$V1))

rate <- function(strings) {
  strings = str_to_lower(strings)
  total_result = 0
  for (i in 1:length(scores$V1)) {
    present = str_count(strings, scores$V1[i])
    word_result = sum(present)*scores$V2[i]
    total_result = total_result + word_result
  }
  return(total_result/length(strings)) # normalizar
}

puntajes = c(0, rate(MR), rate(CF), rate(JAN), rate(JAK))
names(puntajes) = c("  Centro","  Mónica Rincón","  Camila Flores","  José Antonio Neme","  José Antonio Kast")
plot(range(puntajes),c(1,1),type="l",col="lightgrey",ylab="", yaxt="n",xlab="") # línea gráfico
points(puntajes, rep(1,length(puntajes))) # Puntos
text(puntajes,1,names(puntajes),pos=4,srt=90) # Nombres para los puntos