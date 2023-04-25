#1
sum=0
for(i in 10:40){
  a<- ((2^i)/i) + ((3^i)/(i^2))
  sum<-sum+a
}
sum
#Crear una matriz de 4∗5 que contengan valores aleatorios de una uniforme con a=0,b=10
m1<- c(runif(4, min=0, max=10) )
m2<- c(runif(4, min=0, max=10) )
m3<- c(runif(4, min=0, max=10) )
m4<- c(runif(4, min=0, max=10) )
m5<- c(runif(4, min=0, max=10) )
m<- cbind(m1,m2,m3,m4,m5)
m
#Calcular
sum2 <- vector()
sum2
for(i in 1:20){
  for(j in 2:5)
    sum2 <- c(sum2,(i^4/(3+j)))
}
sum(sum2)
#la suma no es correcta

#Crear una función que dado un vector numérico devuelva el coeficiente de variación y el coeficiente de asimetría de Pearson y Fisher.

### Comentario: Incompleto, 2/5

#Pregunta 3 (5 pts)
library(rvest)
library(dplyr)
library(xml2)
www <- "https://www.worldometers.info/coronavirus/"
bd <- read_html(www)
bdt <- bd %>% html_table()
t1 <- bdt[[1]]
t1d <- as.data.frame(t1)
t1d$`Tot Cases/1M pop`
t_actual <- t1d[-c(1:8, 240:247),] %>% select("Country,Other", "Continent","Deaths/1M pop","Tot Cases/1M pop") %>% filter(Continent == "Asia")
asia <- t_actual
asia$`Deaths/1M pop` <- as.numeric(gsub(",", "", asia$`Deaths/1M pop`))
asia$`Tot Cases/1M pop` <- as.numeric(gsub(",", "", asia$`Tot Cases/1M pop`))
asia
res <- cor.test(asia$`Deaths/1M pop`, asia$`Tot Cases/1M pop`, method = "pearson")
res

t_actual2 <- t1d[-c(1:8, 240:247),] %>% select("Country,Other", "Continent","Deaths/1M pop","Tot Cases/1M pop") %>% filter(Continent == "Europe")
europe <- t_actual2
europe$`Deaths/1M pop` <- as.numeric(gsub(",", "", europe$`Deaths/1M pop`))
europe$`Tot Cases/1M pop` <- as.numeric(gsub(",", "", europe$`Tot Cases/1M pop`))
europe
res2<- cor.test(europe$`Deaths/1M pop`, europe$`Tot Cases/1M pop`, method = "pearson")
res2
#La correlacion estan asociadas en un sentido inverso

### Comentario: Era para sur america 4/5

#Pregunta 4 (5 pts)


www2<- "https://www.dismac.com.bo/lo-que-te-regalarias-si-fueras-ni-no.html"
bd2<- read_html(www2)
au1<- html_text(html_nodes(bd2,".value"))
au1
au2<- html_text(html_nodes(bd2,".price-now"))
au2
au3<- html_text(html_nodes(bd2,".product-item-name"))
au3
aubd<- data.frame(Identificador=au1,Precio=au2)

bda<-NULL
bda
for(i in 1:2){
  print(i)
  d<-read_html(paste0(www2,i))
  aux<-d %>% html_nodes(".product-item-details ")
  bd0<-data.frame(
    id = aux %>% html_nodes(".value") %>% html_text2(),
    precio = aux %>% html_nodes(".price-now") %>% html_text2()
    
  )
  bda<-bda %>% bind_rows(bd0)
}  

### Comentario: Incompleto 0/5

### Total: 2+0+4+0=6
  