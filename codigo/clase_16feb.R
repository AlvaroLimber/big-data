rm(list = ls())
#los datos
n<-10000
set.seed(245)
bd<-data.frame(edad=round(rnorm(n,27,5),0),
               ingreso=round(runif(n,0,20000),0),
               sexo=sample(c("H","M"),n,replace = T,prob = c(0.5,0.5)),
               area=factor(runif(n)>=0.3,c(T,F),c("U","R")))
#Operaciones estadísticas
t1<-table(bd$sexo)#tablas de frecuencia (simples, dobles contingencia, ...)
t2<-table(bd$sexo,bd$area)
t1
t2
prop.table(t1)*100

t3<-prop.table(t2)*100
prop.table(t2,1)*100
prop.table(t2,2)*100
addmargins(t3)
prop.table(table(bd$edad>30))*100
####medidas de tendencia central
summary(bd$edad)
summary(bd$ingreso)
mean(bd$edad)
median(bd$edad)
#media cuadrática
x<-bd$ingreso 
sqrt(sum(x**2)/length(x))

mcua<-function(x){
  y<-sqrt(sum(x**2)/length(x))
  return(y)
}
mcua(bd$edad)
mcua(bd$ingreso)
####loops y condiciones
names(bd)
bd$edad
bd["edad"]
bd[["edad"]]
i<-"edad"
bd[i]
bd[[i]]
bd[,4]
for(i in names(bd)){
 if(class(bd[[i]])=="numeric"){
   print(mean(bd[[i]]) )
 } else {
   print(table(bd[[i]]))
 }
}
for(i in names(bd)){
  if(is.numeric(bd[[i]])){
    print(mean(bd[[i]]) )
  } else {
    print(table(bd[[i]]))
  }
}
for(i in 1:ncol(bd)){
  print(i)
}











