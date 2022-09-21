library(readxl)
library(tidyverse)

Data <- read_excel("All_basins_results.xlsx", sheet = "Hoja2")
colnames(Data)
Data_2 <- pivot_longer(Data, 
  cols = c("2015_Natural", "2020_Natural", "2025_Natural", "2030_Natural", "2035_Natural","2040_Natural"),
  names_to = "year" ,
  values_to = 'value'
)

Data_2 <- Data_2[,c(1:7,23:24)]
Data_2$Year <- as.numeric(Data_2$Year)

library(stringr)
str_split_fixed(Data_2$year, "_", 2)
Data_2 <- Data_2 %>%
  separate(year, c("Year", "Type"), "_")
Data_2 <- na.omit(Data_2)

x <- "Year" # Variable dependiente
y <- "value" # Variables independiente

# Generamos la tabla con las tendencias y otras medidas de las tendencias para el set global de datos

# All species -----

# Creamos tabla vacia para almacenar los resultados
tabla_general <- data.frame(
  "Variable"= character(),
  "Trend" = numeric(),
  "t" = numeric(),
  "p" = numeric(),
  "P95_max" = numeric(),
  "P95_min" = numeric(),
  "F" = numeric()
)


for (i in 1:length(y)) {        # Bucle para calcular las estadisticas de todas las variables independientes
  tabla <- data.frame(          # Tabla vacía donde se guardan resultados de cada variable independiente 
    "Variable" = NA,            # para despues unir a la tabla general
    "Trend" = NA,
    "t" = NA,
    "p" = NA,
    "P95_max" = NA,
    "P95_min" = NA,
    "F" = NA
  )
  tabla$Variable <- y[i]  # Rellena primera columna con el nombre de la variable
  model_g <- lm(formula(paste(y[i],  # Crea formula utilizando la variable del bucle
                              paste(x, collapse = "+"),
                              sep = " ~ ")), 
                data = Data_2)
  tabla$Trend <- model_g$coefficients[[2]] # Tendencia
  tabla$t <- summary(model_g)$coefficients[2, 3] # t del modelo
  tabla$p <- summary(model_g)$coefficients[2, 4] # p del modelo
  tabla$P95_max <-  confint(model_g, "Año_Mes", level = .95)[, 2] # Intervalo de confianza max del 95%
  tabla$P95_min <-  confint(model_g, "Año_Mes", level = .95)[, 1] # Intervalo de confianza min del 95%
  tabla$F <- summary(model_g)$fstatistic[1] # F del modelo
  tabla_general <- rbind(tabla_general, tabla) # Unimos las filas de la tabla general con cada una d elas tablas individuales
}

watershed <- unique(Data_2$`1pol_id`)

# Tabla vacía para guardar los resultados 
tabla_ind <- data.frame(
  "watershed" = character(),
  "prior_pre_5" = numeric(),
  "prior_pre_10" = numeric(),
  "prior_pre_20" = numeric(),
  "prior_fut_5" = numeric(),
  "prior_fut_10" = numeric(),
  "prior_fut_20" = numeric(),
  "Trend" = numeric(),
  "t" = numeric(),
  "p" = numeric(),
  "95_max" = numeric(),
  "95_min" = numeric(),
  "F" = numeric(),
  "Dif" = numeric()
)

# Bucle para calcular las tendencias de cada una de las cuencas

for (n in 1:length(watershed)) {           
  ind <- Data_2 %>% 
    filter(Data_2$`1pol_id` == watershed[270]) %>%
    mutate(group = "i")             
        tabla <- data.frame(                          # Crea tabla vacia para despues unificar a tabla de resultados
          "watershed" = NA,
          "prior_pre_5" = NA,
          "prior_pre_10" = NA,
          "prior_pre_20" = NA,
          "prior_fut_5" = NA,
          "prior_fut_10" = NA,
          "prior_fut_20" = NA,
          "Trend" = NA,
          "t" = NA,
          "p" = NA,
          "F" = NA,
          "Dif_2_coef" = NA,
          "Dif_2_pvalue" = NA,
          "Dif_2_F" = NA
        )
        # General
        model_g <- lm(ind$value ~ ind$Year, data = Data_2)
        
        tabla$watershed <- unique(ind[[1]])
        tabla$prior_pre_5 <- unique(ind[[3]])
        tabla$prior_pre_10 <- unique(ind[[4]])
        tabla$prior_pre_20 <- unique(ind[[5]])
        tabla$prior_fut_5 <- unique(ind[[6]])
        tabla$prior_fut_10 <- unique(ind[[7]])
        tabla$prior_fut_20 <- unique(ind[[8]])
        
        model_i <-  lm(ind$value ~ ind$Year, data = ind)                
      
        
        tabla$Trend <- model_i$coefficients[[2]]
        tabla$t <- summary(model_i)$coefficients[2, 3]
        tabla$p <- summary(model_i)$coefficients[2, 4]
        tabla$F <- summary(model_i)$fstatistic[1]
        
        
        gen <- Data_2 %>%
          mutate(group = "g")
        
        dat <- rbind(gen,ind)
        
        
        model_int = lm(ind$value ~ ind$Year, data = dat)
        
        
        tabla$Dif_2_coef <- summary(model_int)$coefficients[2,1]
        tabla$Dif_2_pvalue <- summary(model_int)$coefficients[2,4]
        tabla$Dif_2_F <- summary(model_int)$fstatistic[1]
        
        tabla_ind <- rbind(tabla_ind, tabla)  # Unimos tablas
        
      }
