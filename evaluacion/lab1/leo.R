#############################
###p1
x=10:40
sum(((2^x)/x)+((3^x)/x^2))
###p2
matrix(runif(4*5,0,10),nrow = 4,ncol = 5)
###p3
y=0
for (i in 1:20) {
  for (j in 2:5) {
    y=y+(i^3)/(2+i*j)
  }
}
y
###p4
coef=function(x){
  coefvar=sd(x)/mean(x)
  coefAPearson=3*(mean(x)-median(x))/sd(x)
  coefAFisher=sum((x-mean(x))^3)/(length(x)*sd(x)^3)
  list(coefvar=coefvar,coefAPearson=coefAPearson,
       coefAFisher=coefAFisher)
}
x=rnorm(100)
cv(x)
coef(x)

### Comentario: cv no funciona 5/5

##2
y2=scan()
762   327   468
484   239   477
y2=matrix(y2,nrow = 2,ncol = 3,byrow = T)

pea=function(y2){
  y3=matrix(0,nrow = nrow(y2),ncol = ncol(y2))
  for (i in 1:nrow(y2)) {
    for (j in 1:ncol(y2)) {
      y3[i,j]=sum(y2[i,])*sum(y2[,j])/sum(y2)
    }
  }
  pchi=sum((y2-y3)^2/y3)
  list(test.chi.pearson=pchi,
       df=(nrow(y2)-1)*(ncol(y2)-1),
       pvalue=pchisq(pchi,(nrow(y2)-1)*(ncol(y2)-1),lower.tail = F))
}

pea(y2)
chisq.test(y2)
y3=matrix(rnorm(10,10,3),nrow =2, ncol=5)
pea(y3)
chisq.test(y3)

### Comentario 5/5

####3
library(rvest)
library(dplyr)
www="https://www.worldometers.info/coronavirus/"
bd=read_html(www)

bdt = bd %>% html_table()

t=bdt[[1]]
names(table(t$Continent))
t1=filter(t,!(`#`== "NA"),Continent %in% c("South America","Europe"))
View(t1)
t1$`Deaths/1M pop`=as.numeric(gsub(",","",t1$`Deaths/1M pop`))
t1$`Tot Cases/1M pop`=as.numeric(gsub(",","",t1$`Tot Cases/1M pop`))
t2=t1 %>% select(`Country,Other`,Continent,`Deaths/1M pop`,
                 `Tot Cases/1M pop`) %>% arrange(Continent)
View(t2)
t2$`Deaths/1M pop`[is.na(t2$`Deaths/1M pop`)]=0
t2$`Tot Cases/1M pop`[is.na(t2$`Tot Cases/1M pop`)]=0
View(t2)
## Sur America

t3=t2 %>% filter(Continent=="South America") 
cor(t3$`Deaths/1M pop`,t3$`Tot Cases/1M pop`)
## Europa
t4=t2 %>% filter(Continent=="Europe") 
cor(t4$`Deaths/1M pop`,t4$`Tot Cases/1M pop`)

## La corralacion de ambos valores es negativa la correlacion de sur 
## america esta mas correlacionada negativamente que Europa

### Comentario: Incorrecto, no se puede asignar 0 a los valores perdidos 2/5

####4 
www2="https://www.dismac.com.bo/lo-que-te-regalarias-si-fueras-ni-no.html"
bd2=read_html(www2)
PrecioActual=bd2 %>% html_nodes(".price-now") %>% html_text2()
Identificador=bd2 %>% html_nodes(".value") %>% html_text2()
Descripción=bd2 %>% html_nodes(".product-item-name") %>% html_text2()
Descripción=Descripción[6:25]
www2="https://www.dismac.com.bo/lo-que-te-regalarias-si-fueras-ni-no.html?p="
bd3=NULL
for (i in 1:2) {
  e=read_html(paste0(www2,i))
  PrecioActual=e %>% html_nodes(".cuotos-price") %>% html_text2()
  Identificador=e %>% html_nodes(".value") %>% html_text2()
  Descripción=e %>% html_nodes(".product-item-name") %>% html_text2()
  Descripción=Descripción[6:25]
  bd0=data.frame(
    PrecioActual,
    Identificador,
    Descripción
  )
  bd3=bd3 %>% bind_rows(bd0)
}
View(bd3)
bd3$PrecioActual

### Comentario: Incompleto, no esta la caja 2/5

### Total: 5+5+2+2=14/20
