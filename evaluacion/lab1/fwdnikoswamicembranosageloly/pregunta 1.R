# primera suma
suma <- 0
for (i in 10:40) {
  termino <- (2^i / i) + (3^i / i^2)
  suma <- suma + termino
}
print(suma)
# matris uniforme 
matriz <- matrix(runif(20, min = 0, max = 10), nrow = 4, ncol = 5)
print(matriz)
# segunda suma 
suma2 <- 0
for (i in 1:20) {
  for (j in 2:5) {
    termino <- i^3 / (2 + i*j)
    suma2 <- suma2 + termino
  }
}
print(suma2)
# 
coeficientes <- function(x) {
  cv <- sd(x) / mean(x) * 100 
  skewness_p <- sum((x - mean(x))^3) / (length(x) * sd(x)^3) 
  skewness_f <- sum((x - mean(x))^3) / ((length(x) - 1) * (length(x) - 2)) / (sd(x)^3) 
  return(list(cv = cv, skewness_p = skewness_p, skewness_f = skewness_f))
}
# Ejemplo de uso de la función
x <- c(1, 2, 3, 4, 5)
resultado <- coeficientes(x)
print(resultado$cv)
print(resultado$skewness_p)
print(resultado$skewness_f)

### Comentario 5/5