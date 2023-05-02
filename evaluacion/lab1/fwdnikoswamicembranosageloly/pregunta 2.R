## funcion 
prueba_chi_cuadrado <- function(obs, exp) {
  df <- length(obs) - 1 
  chi_cuadrado <- sum((obs - exp)^2 / exp) 
  p_valor <- pchisq(chi_cuadrado, df, lower.tail = FALSE)
  return(list(estadistico = chi_cuadrado, grados_libertad = df, p_valor = p_valor))
}
# Ejemplo de uso de la función
obs <- c(18, 15, 7, 8, 12)
exp <- c(12, 12, 12, 12, 12)
resultado <- prueba_chi_cuadrado(obs, exp)
print(resultado$estadistico)
print(resultado$grados_libertad)
print(resultado$p_valor)
######
M <- as.table(rbind(c(762, 327, 468), c(484, 239, 477)))
dimnames(M) <- list(gender = c("F", "M"),
                    party = c("Democrat","Independent", "Republican"))
M
M2<- prueba_chi_cuadrado(M)
M2

### Comentario: Incorrecto 0/5