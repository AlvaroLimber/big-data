library(dplyr)
library(readr)

# Cargar datos
url <- "https://covid.ourworldindata.org/data/owid-covid-data.csv"
data <- read_csv(url)

# Definir lista de países de Sudamérica
sa_countries <- c("Argentina", "Bolivia", "Brazil", "Chile", "Colombia", "Ecuador", 
                  "Guyana", "Paraguay", "Peru", "Suriname", "Uruguay", "Venezuela")

eu_countries <- c("Albania", "Andorra", "Austria", "Belarus", "Belgium", "Bosnia and Herzegovina",
                  "Bulgaria", "Croatia", "Cyprus", "Czech Republic", "Denmark", "Estonia",
                  "Finland", "France", "Germany", "Greece", "Hungary", "Iceland", "Ireland",
                  "Italy", "Kosovo", "Latvia", "Liechtenstein", "Lithuania", "Luxembourg", "Malta",
                  "Moldova", "Monaco", "Montenegro", "Netherlands", "North Macedonia", "Norway",
                  "Poland", "Portugal", "Romania", "Russia", "San Marino", "Serbia", "Slovakia",
                  "Slovenia", "Spain", "Sweden", "Switzerland", "Ukraine", "United Kingdom", "Vatican City")

# Filtrar y seleccionar datos para Sur América y Europa
sa_eu_data <- data %>%
  filter(continent %in% c("South America", "Europe")) %>%
  select(location, new_deaths_per_million, total_cases_per_million) %>%
  na.omit()

# Calcular correlación para Sur América
sa_corr <- cor(sa_eu_data %>% filter(location %in% sa_countries) %>%
                 select(new_deaths_per_million, total_cases_per_million))

# Calcular correlación para Europa
eu_corr <- cor(sa_eu_data %>% filter(location %in% eu_countries) %>%
                select(new_deaths_per_million, total_cases_per_million))
# Imprimir resultados
cat("Correlación para Sur América:", sa_corr, "\n")
cat("Correlación para Europa:", eu_corr, "\n")

### Comentario: Incorrecto, no se uso la página del ejercicio. 1/5
