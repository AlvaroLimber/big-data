# PREGUNTA 1

# a)
inicio <- 10
fin <- 40
resultado <- lapply(inicio:fin, function(i) { 2^i/i + 3^i/i^2 } )
sum(unlist(resultado))

#b)
library(matricks)
runifm(4, 5, min = 0, max = 10)

#c) 
sum(sapply(1:20, function(i) { sapply(2:5, function(j) { i^3 / (2 + i*j) }) }) )

### Comentario: Falta cv y asimetria 3/5

# PREGUNTA 2
observada <- rbind(c(762, 327, 468), c(484, 239, 477)) 
teorica <- rbind(c(761, 320, 469), c(485, 238, 476)) 
r <- 2
k <- 3
gl = (r-1) * (k-1)
  
chi2Pearson = function(observada, teorica, r, k) {
  sum(sapply(1:r, function(i) { sapply(1:k, function(j) { (observada[i,j] - teorica[i,j])^2 / teorica[i,j] } ) } ) )
}
chi2Pearson(observada, teorica, r, k)

# prueba con R
chisq.test(observada, teorica)
M <- as.table(observada)
dimnames(M) <- list(gender = c("F", "M"),
                    party = c("Democrat","Independent", "Republican"))
(Xsq <- chisq.test(M))  # Prints test summary
Xsq$observed   # observed counts (same as M)
Xsq$expected   # expected counts under the null
Xsq$residuals  # Pearson residuals
Xsq$stdres     # standardized residuals

### Comentario: Incorrecto 0/5

# PREGUNTA 3
library(rvest)
library(dplyr)
library(xml2)
library(knitr)
www <- "https://www.worldometers.info/coronavirus/#main_table"
bd <- read_html(www)
bdt <- bd %>% html_table()
t1 <- bdt[[1]]
t2 <- bdt[[2]]
t3 <- bdt[[3]]
t1d <- as.data.frame(t1)

t_actual <- t1d[-c(1:8, 240:247),] %>% select("Country,Other", "Deaths/1M pop", "Continent") %>% filter(Continent == "South America")
t_south <- t_actual
t_south$`Deaths/1M pop` <- as.numeric(gsub(",", "", t_south$`Deaths/1M pop`))
t_south %>% arrange(desc(t_south$`Deaths/1M pop`)) %>% slice(1:5) %>% select("Country,Other", "Deaths/1M pop") %>% kable()

t_actual <- t1d[-c(1:8, 240:247),] %>% select("Country,Other", "Deaths/1M pop", "Continent") %>% filter(Continent == "Europe")
t_europa <- t_actual
t_europa$`Deaths/1M pop` <- as.numeric(gsub(",", "", t_europa$`Deaths/1M pop`))
t_europa %>% arrange(desc(t_europa$`Deaths/1M pop`)) %>% slice(1:5) %>% select("Country,Other", "Deaths/1M pop") %>% kable()

### Comentario: Incompleto 0/5

# PREGUNTA 4
www <- "https://www.dismac.com.bo/lo-que-te-regalarias-si-fueras-ni-no.html"
bd <- read_html(www)
# Identificador + Descripción + Precio actual
Identificador <- bd %>% html_nodes(".sku value selectorgadget_rejected") %>% html_text() 
Identificador
description <- bd %>% html_nodes(".product-item-link simple-product-item-link") %>% html_text() 
description
precio <- bd %>% html_nodes(".no-special") %>% html_text() 
precio

### Comentario: Incompleto 0/5

### Total: 3+0+0+0=3/20


