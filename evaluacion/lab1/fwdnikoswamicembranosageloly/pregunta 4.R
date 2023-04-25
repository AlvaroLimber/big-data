

library(rvest)
library(dplyr)
library(ggplot2)


url_base <- "https://www.dismac.com.bo/lo-que-te-regalarias-si-fueras-ni-no.html?p="
num_paginas <- 2

datos <- list()


for (i in 1:num_paginas) {
  
  # Construir la URL completa
  url_pagina <- paste0(url_base, i)
  
  # Extraer el contenido de la página
  pagina <- read_html(url_pagina)
  
  # Extraer los identificadores y descripciones
  ids <- pagina %>% html_nodes(".item .product-name a") %>% html_text() %>% trimws()
  descripciones <- pagina %>% html_nodes(".item .product-name .product-desc") %>% html_text() %>% trimws()
  
  # Extraer los precios y agregarlos solo si están disponibles
  precios <- pagina %>% html_nodes(".item .product-price .price") %>% html_text() %>% trimws()
  if (length(precios) > 0) {
    precios <- as.numeric(gsub("[^0-9.]", "", precios))
  } else {
    precios <- NA
  }
  
  # Combinar los datos en un marco de datos y agregarlos a la lista
  datos_pagina <- data.frame(id = ids, descripcion = descripciones, stringsAsFactors = FALSE) %>%
    mutate(precio_actual = precios)
  datos[[i]] <- datos_pagina
  
}


datos_completos <- do.call(rbind, datos)

#
datos_completos <- na.omit(datos_completos)

#
library(ggplot2)


datos_completos <- na.omit(datos_completos)

ggplot(datos_completos, aes(x = "", y = precio_actual)) +
  geom_boxplot(fill = "") +
  scale_y_continuous(labels = scales::dollar_format()) +
  labs(title = "Precios de productos en Dismac",
       y = "Precio actual")

### Comentario: No corre 0/5
