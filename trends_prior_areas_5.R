library(readxl)
library(tidyverse)

Data <- read_excel("All_basins_results.xlsx", sheet = "Hoja2")

x <- "Year" # Variable dependiente
y <- "value" # Variables independiente

# PRESENTE ----
## Natural ----

### Top 5
top_5 <- filter(Data, Data$`12prior_rf_max_pres_5` == 3)

top_5 <- top_5[,c(8:13)]
top_5 <- as.data.frame(t(colMeans(na.omit(top_5))))

top_5 <- pivot_longer(top_5, 
                      cols = c("2015_Natural", "2020_Natural", "2025_Natural", "2030_Natural", "2035_Natural","2040_Natural"),
                      names_to = "year" ,
                      values_to = 'value')


top_5 <- top_5 %>%
  separate(year, c("Year", "Type"), "_") %>% 
  mutate(priority = "Top 5")

top_5$Year <- as.numeric(top_5$Year)

### Rest
Rest <- filter(Data, Data$`14prior_rf_max_pres_20` == 0|Data$`13prior_rf_max_pres_10` == 3|Data$`14prior_rf_max_pres_20` == 3)

Rest <- Rest[,c(8:13)]
Rest <- as.data.frame(t(colMeans(na.omit(Rest))))

Rest <- pivot_longer(Rest, 
                     cols = c("2015_Natural", "2020_Natural", "2025_Natural", "2030_Natural", "2035_Natural","2040_Natural"),
                     names_to = "year" ,
                     values_to = 'value')


Rest <- Rest %>%
  separate(year, c("Year", "Type"), "_") %>% 
  mutate(priority = "Rest")

Rest$Year <- as.numeric(Rest$Year)

All <- rbind(top_5, Rest)

# Tabla vacía para guardar los resultados 
tabla_pre <- data.frame(
  "prior" = character(),
  "Trend" = numeric(),
  "t" = numeric(),
  "p" = numeric(),
  "Dif_pvalue" = numeric(),
  "prior_dif" = character(),
  "land_use" = character(), 
  "period" = character()
)


# Bucle para calcular las tendencias de cada una de las cuencas

prior <- unique(All$priority)

for (n in 1:length(prior)) {           
  ind <- All %>% 
    filter(All$priority == prior[n])
  
  
  tabla <- data.frame(
    "prior" = NA,
    "Trend" = NA,
    "t" = NA,
    "p" = NA,
    "Dif_pvalue" = NA,
    "prior_dif" = NA,
    "land_use" = NA,
    "period" = NA
  )
  kk <- prior[-n]
  for (k in 1:length(kk)) {           
    gen <- All %>% 
      filter(All$priority == kk[k])
    
    model_g <- lm(ind$value ~ ind$Year, data = gen )
    
    tabla$prior <- unique(ind[[4]])
    
    model_i <-  lm(ind$value ~ ind$Year, data = ind)                
    
    
    tabla$Trend <- model_i$coefficients[[2]]
    tabla$t <- summary(model_i)$coefficients[2, 3]
    tabla$p <- summary(model_i)$coefficients[2, 4]
    
    dat <- rbind(gen,ind)
    
    model_int = lm(dat$value ~ dat$Year+dat$priority, data = dat)
    
    tabla$Dif_pvalue <- summary(model_int)$coefficients[3,4]
    tabla$prior_dif <- kk[k]
    tabla$land_use <- c("Natural")
    tabla$period <- c("Present")
    
    tabla_pre <- rbind(tabla_pre, tabla)  # Unimos tablas
  }
}

tabla_pre <- tabla_pre[1:9,]

# FUTURO ----

## Natural ----

### Top 5
top_5 <- filter(Data, Data$`24prior_rf_max_fut_5` == 3)

top_5 <- top_5[,c(8:13)]
top_5 <- as.data.frame(t(colMeans(na.omit(top_5))))

top_5 <- pivot_longer(top_5, 
                      cols = c("2015_Natural", "2020_Natural", "2025_Natural", "2030_Natural", "2035_Natural","2040_Natural"),
                      names_to = "year" ,
                      values_to = 'value')


top_5 <- top_5 %>%
  separate(year, c("Year", "Type"), "_") %>% 
  mutate(priority = "Top 5")

top_5$Year <- as.numeric(top_5$Year)

### Rest
Rest <- filter(Data, Data$`26prior_rf_max_fut_20` == 0|Data$`25prior_rf_max_fut_10` == 3|Data$`26prior_rf_max_fut_20` == 3)

Rest <- Rest[,c(8:13)]
Rest <- as.data.frame(t(colMeans(na.omit(Rest))))

Rest <- pivot_longer(Rest, 
                     cols = c("2015_Natural", "2020_Natural", "2025_Natural", "2030_Natural", "2035_Natural","2040_Natural"),
                     names_to = "year" ,
                     values_to = 'value')


Rest <- Rest %>%
  separate(year, c("Year", "Type"), "_") %>% 
  mutate(priority = "Rest")

Rest$Year <- as.numeric(Rest$Year)

All <- rbind(top_5, Rest)
