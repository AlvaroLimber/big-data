#Usando R realizar una "demostración" (simulaciòn)
#del teorema del límite central
N<-11000000;n<-100;k<-10000
x<-runif(N,0,100)
k/choose(N,n)
format(choose(N,n),scientific = F)
dmean<-NULL
for(i in 1:k){
  dmean<-c(dmean,mean(sample(x,n,replace = T)))
}
mean(dmean)
mean(x)
var(dmean) #V(xbar)
var(x)/n   #sigma^2/n
mx<-mean(x)
vx<-var(x)
plot(density(dmean),
     main=paste("Tamaño de muestra:", n))
curve(dnorm(x,mx,sqrt(vx/n)),add=T,col="red",lwd=2)
###librerías en R
library()
library(foreign)
library(help=foreign)#ayuda de la librería
install.packages("haven")#instalar/actualizar librerías
install.packages("flexdashboard")
install.packages("shiny")
library(haven)
library(flexdashboard)
library(shiny)





