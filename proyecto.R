library(stringr)

MR = rtweet::get_timeline("tv_monica", n=3200)$text
CF = rtweet::get_timeline("Cami_FloresO", n=3200)$text
JAN = rtweet::get_timeline("jananeme", n=3200)$text
JAK = rtweet::get_timeline("joseantoniokast", n=3200)$text

# scores.csv es un archivo con expresiones regulares para identificar
# las opiniones políticas de una persona
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

library(ggplot2)

puntajes = c(rate(MR), rate(CF), rate(JAN), rate(JAK))
names(puntajes) = c("Mónica Rincón","Camila Flores","José Antonio Neme","José Antonio Kast")
df = data.frame(personajes = names(puntajes), puntaje = puntajes)

# crear un ggplot con los nombres y distintos colores
p = ggplot(df, aes(x=puntaje, y=1, label=personajes, colour = personajes))
p = p + geom_point()
# se usa nudge_x y nudge_y para que los nombres no se tapen entre ellos.
p = p + geom_text(hjust=0, vjust=0, angle = 45, nudge_x = c(0.04,0,0,-0.075), nudge_y = 0.005, size=5)
p = p + coord_cartesian(ylim = c(0.9,1.2), xlim = c(0,0.9))
# mostrarlo
p