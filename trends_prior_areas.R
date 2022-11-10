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

### Top 10
Top_10 <- filter(Data, Data$`13prior_rf_max_pres_10` == 3)

Top_10 <- Top_10[,c(8:13)]
Top_10 <- as.data.frame(t(colMeans(na.omit(Top_10))))

Top_10 <- pivot_longer(Top_10, 
                       cols = c("2015_Natural", "2020_Natural", "2025_Natural", "2030_Natural", "2035_Natural","2040_Natural"),
                       names_to = "year" ,
                       values_to = 'value')


Top_10 <- Top_10 %>%
  separate(year, c("Year", "Type"), "_") %>% 
  mutate(priority = "Top 10")

Top_10$Year <- as.numeric(Top_10$Year)

### Top 20
Top_20 <- filter(Data, Data$`14prior_rf_max_pres_20` == 3)

Top_20 <- Top_20[,c(8:13)]
Top_20 <- as.data.frame(t(colMeans(na.omit(Top_20))))

Top_20 <- pivot_longer(Top_20, 
                       cols = c("2015_Natural", "2020_Natural", "2025_Natural", "2030_Natural", "2035_Natural","2040_Natural"),
                       names_to = "year" ,
                       values_to = 'value')


Top_20 <- Top_20 %>%
  separate(year, c("Year", "Type"), "_") %>% 
  mutate(priority = "Top 20")

Top_20$Year <- as.numeric(Top_20$Year)


### Rest
Rest <- filter(Data, Data$`14prior_rf_max_pres_20` == 0)

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

All <- rbind(top_5, Top_10, Top_20, Rest)



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

## Irrigated -----

### Top 5
top_5 <- filter(Data, Data$`12prior_rf_max_pres_5` == 3)

top_5 <- top_5[,c(15:20)]
top_5 <- as.data.frame(t(colMeans(na.omit(top_5))))

top_5 <- pivot_longer(top_5, 
                      cols = c("2015_Irrigated", "2020_Irrigated", "2025_Irrigated", "2030_Irrigated", "2035_Irrigated","2040_Irrigated"),
                      names_to = "year" ,
                      values_to = 'value')


top_5 <- top_5 %>%
  separate(year, c("Year", "Type"), "_") %>% 
  mutate(priority = "Top 5")

top_5$Year <- as.numeric(top_5$Year)

### Top 10
Top_10 <- filter(Data, Data$`13prior_rf_max_pres_10` == 3)

Top_10 <- Top_10[,c(15:20)]
Top_10 <- as.data.frame(t(colMeans(na.omit(Top_10))))

Top_10 <- pivot_longer(Top_10, 
                       cols = c("2015_Irrigated", "2020_Irrigated", "2025_Irrigated", "2030_Irrigated", "2035_Irrigated","2040_Irrigated"),
                       names_to = "year" ,
                       values_to = 'value')


Top_10 <- Top_10 %>%
  separate(year, c("Year", "Type"), "_") %>% 
  mutate(priority = "Top 10")

Top_10$Year <- as.numeric(Top_10$Year)

### Top 20
Top_20 <- filter(Data, Data$`14prior_rf_max_pres_20` == 3)

Top_20 <- Top_20[,c(15:20)]
Top_20 <- as.data.frame(t(colMeans(na.omit(Top_20))))

Top_20 <- pivot_longer(Top_20, 
                       cols = c("2015_Irrigated", "2020_Irrigated", "2025_Irrigated", "2030_Irrigated", "2035_Irrigated","2040_Irrigated"),
                       names_to = "year" ,
                       values_to = 'value')


Top_20 <- Top_20 %>%
  separate(year, c("Year", "Type"), "_") %>% 
  mutate(priority = "Top 20")

Top_20$Year <- as.numeric(Top_20$Year)


### Rest
Rest <- filter(Data, Data$`14prior_rf_max_pres_20` == 0)

Rest <- Rest[,c(15:20)]
Rest <- as.data.frame(t(colMeans(na.omit(Rest))))

Rest <- pivot_longer(Rest, 
                     cols = c("2015_Irrigated", "2020_Irrigated", "2025_Irrigated", "2030_Irrigated", "2035_Irrigated","2040_Irrigated"),
                     names_to = "year" ,
                     values_to = 'value')


Rest <- Rest %>%
  separate(year, c("Year", "Type"), "_") %>% 
  mutate(priority = "Rest")

Rest$Year <- as.numeric(Rest$Year)

All <- rbind(top_5, Top_10, Top_20, Rest)


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
    tabla$land_use <- c("Irrigated")
    tabla$period <- c("Present")
    
    tabla_pre <- rbind(tabla_pre, tabla)  # Unimos tablas
  }
}

tabla_pre <- tabla_pre[1:18,]

## Non_Irrigated -----

### Top 5
top_5 <- filter(Data, Data$`12prior_rf_max_pres_5` == 3)

top_5 <- top_5[,c(22:27)]
top_5 <- as.data.frame(t(colMeans(na.omit(top_5))))

top_5 <- pivot_longer(top_5, 
                      cols = c("2015_Non_Irrigated", "2020_Non_Irrigated", "2025_Non_Irrigated", "2030_Non_Irrigated", "2035_Non_Irrigated","2040_Non_Irrigated"),
                      names_to = "year" ,
                      values_to = 'value')


top_5 <- top_5 %>%
  separate(year, c("Year", "Type"), "_") %>% 
  mutate(priority = "Top 5")

top_5$Year <- as.numeric(top_5$Year)

### Top 10
Top_10 <- filter(Data, Data$`13prior_rf_max_pres_10` == 3)

Top_10 <- Top_10[,c(22:27)]
Top_10 <- as.data.frame(t(colMeans(na.omit(Top_10))))

Top_10 <- pivot_longer(Top_10, 
                       cols = c("2015_Non_Irrigated", "2020_Non_Irrigated", "2025_Non_Irrigated", "2030_Non_Irrigated", "2035_Non_Irrigated","2040_Non_Irrigated"),
                       names_to = "year" ,
                       values_to = 'value')


Top_10 <- Top_10 %>%
  separate(year, c("Year", "Type"), "_") %>% 
  mutate(priority = "Top 10")

Top_10$Year <- as.numeric(Top_10$Year)

### Top 20
Top_20 <- filter(Data, Data$`14prior_rf_max_pres_20` == 3)

Top_20 <- Top_20[,c(22:27)]
Top_20 <- as.data.frame(t(colMeans(na.omit(Top_20))))

Top_20 <- pivot_longer(Top_20, 
                       cols = c("2015_Non_Irrigated", "2020_Non_Irrigated", "2025_Non_Irrigated", "2030_Non_Irrigated", "2035_Non_Irrigated","2040_Non_Irrigated"),
                       names_to = "year" ,
                       values_to = 'value')


Top_20 <- Top_20 %>%
  separate(year, c("Year", "Type"), "_") %>% 
  mutate(priority = "Top 20")

Top_20$Year <- as.numeric(Top_20$Year)


### Rest
Rest <- filter(Data, Data$`14prior_rf_max_pres_20` == 0)

Rest <- Rest[,c(22:27)]
Rest <- as.data.frame(t(colMeans(na.omit(Rest))))

Rest <- pivot_longer(Rest, 
                     cols = c("2015_Non_Irrigated", "2020_Non_Irrigated", "2025_Non_Irrigated", "2030_Non_Irrigated", "2035_Non_Irrigated","2040_Non_Irrigated"),
                     names_to = "year" ,
                     values_to = 'value')


Rest <- Rest %>%
  separate(year, c("Year", "Type"), "_") %>% 
  mutate(priority = "Rest")

Rest$Year <- as.numeric(Rest$Year)

All <- rbind(top_5, Top_10, Top_20, Rest)


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
    tabla$land_use <- c("Non_Irrigated")
    tabla$period <- c("Present")
    
    tabla_pre <- rbind(tabla_pre, tabla)  
  }
}

tabla_pre <- tabla_pre[1:27,]

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

### Top 10
Top_10 <- filter(Data, Data$`25prior_rf_max_fut_10` == 3)

Top_10 <- Top_10[,c(8:13)]
Top_10 <- as.data.frame(t(colMeans(na.omit(Top_10))))

Top_10 <- pivot_longer(Top_10, 
                       cols = c("2015_Natural", "2020_Natural", "2025_Natural", "2030_Natural", "2035_Natural","2040_Natural"),
                       names_to = "year" ,
                       values_to = 'value')


Top_10 <- Top_10 %>%
  separate(year, c("Year", "Type"), "_") %>% 
  mutate(priority = "Top 10")

Top_10$Year <- as.numeric(Top_10$Year)

### Top 20
Top_20 <- filter(Data, Data$`26prior_rf_max_fut_20` == 3)

Top_20 <- Top_20[,c(8:13)]
Top_20 <- as.data.frame(t(colMeans(na.omit(Top_20))))

Top_20 <- pivot_longer(Top_20, 
                       cols = c("2015_Natural", "2020_Natural", "2025_Natural", "2030_Natural", "2035_Natural","2040_Natural"),
                       names_to = "year" ,
                       values_to = 'value')


Top_20 <- Top_20 %>%
  separate(year, c("Year", "Type"), "_") %>% 
  mutate(priority = "Top 20")

Top_20$Year <- as.numeric(Top_20$Year)


### Rest
Rest <- filter(Data, Data$`26prior_rf_max_fut_20` == 0)

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

All <- rbind(top_5, Top_10, Top_20, Rest)



# Tabla vacía para guardar los resultados 
tabla_fut <- data.frame(
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
    tabla$period <- c("Future")
    
    tabla_fut <- rbind(tabla_fut, tabla)  # Unimos tablas
  }
}

tabla_fut <- tabla_fut[1:9,]

## Irrigated -----

### Top 5
top_5 <- filter(Data, Data$`24prior_rf_max_fut_5` == 3)

top_5 <- top_5[,c(15:20)]
top_5 <- as.data.frame(t(colMeans(na.omit(top_5))))

top_5 <- pivot_longer(top_5, 
                      cols = c("2015_Irrigated", "2020_Irrigated", "2025_Irrigated", "2030_Irrigated", "2035_Irrigated","2040_Irrigated"),
                      names_to = "year" ,
                      values_to = 'value')


top_5 <- top_5 %>%
  separate(year, c("Year", "Type"), "_") %>% 
  mutate(priority = "Top 5")

top_5$Year <- as.numeric(top_5$Year)

### Top 10
Top_10 <- filter(Data, Data$`25prior_rf_max_fut_10` == 3)

Top_10 <- Top_10[,c(15:20)]
Top_10 <- as.data.frame(t(colMeans(na.omit(Top_10))))

Top_10 <- pivot_longer(Top_10, 
                       cols = c("2015_Irrigated", "2020_Irrigated", "2025_Irrigated", "2030_Irrigated", "2035_Irrigated","2040_Irrigated"),
                       names_to = "year" ,
                       values_to = 'value')


Top_10 <- Top_10 %>%
  separate(year, c("Year", "Type"), "_") %>% 
  mutate(priority = "Top 10")

Top_10$Year <- as.numeric(Top_10$Year)

### Top 20
Top_20 <- filter(Data, Data$`26prior_rf_max_fut_20` == 3)

Top_20 <- Top_20[,c(15:20)]
Top_20 <- as.data.frame(t(colMeans(na.omit(Top_20))))

Top_20 <- pivot_longer(Top_20, 
                       cols = c("2015_Irrigated", "2020_Irrigated", "2025_Irrigated", "2030_Irrigated", "2035_Irrigated","2040_Irrigated"),
                       names_to = "year" ,
                       values_to = 'value')


Top_20 <- Top_20 %>%
  separate(year, c("Year", "Type"), "_") %>% 
  mutate(priority = "Top 20")

Top_20$Year <- as.numeric(Top_20$Year)


### Rest
Rest <- filter(Data, Data$`26prior_rf_max_fut_20` == 0)

Rest <- Rest[,c(15:20)]
Rest <- as.data.frame(t(colMeans(na.omit(Rest))))

Rest <- pivot_longer(Rest, 
                     cols = c("2015_Irrigated", "2020_Irrigated", "2025_Irrigated", "2030_Irrigated", "2035_Irrigated","2040_Irrigated"),
                     names_to = "year" ,
                     values_to = 'value')


Rest <- Rest %>%
  separate(year, c("Year", "Type"), "_") %>% 
  mutate(priority = "Rest")

Rest$Year <- as.numeric(Rest$Year)

All <- rbind(top_5, Top_10, Top_20, Rest)


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
    tabla$land_use <- c("Irrigated")
    tabla$period <- c("Future")
    
    tabla_fut <- rbind(tabla_fut, tabla)  # Unimos tablas
  }
}

tabla_fut <- tabla_fut[1:18,]

## Non_Irrigated -----

### Top 5
top_5 <- filter(Data, Data$`24prior_rf_max_fut_5` == 3)

top_5 <- top_5[,c(22:27)]
top_5 <- as.data.frame(t(colMeans(na.omit(top_5))))

top_5 <- pivot_longer(top_5, 
                      cols = c("2015_Non_Irrigated", "2020_Non_Irrigated", "2025_Non_Irrigated", "2030_Non_Irrigated", "2035_Non_Irrigated","2040_Non_Irrigated"),
                      names_to = "year" ,
                      values_to = 'value')


top_5 <- top_5 %>%
  separate(year, c("Year", "Type"), "_") %>% 
  mutate(priority = "Top 5")

top_5$Year <- as.numeric(top_5$Year)

### Top 10
Top_10 <- filter(Data, Data$`25prior_rf_max_fut_10` == 3)

Top_10 <- Top_10[,c(22:27)]
Top_10 <- as.data.frame(t(colMeans(na.omit(Top_10))))

Top_10 <- pivot_longer(Top_10, 
                       cols = c("2015_Non_Irrigated", "2020_Non_Irrigated", "2025_Non_Irrigated", "2030_Non_Irrigated", "2035_Non_Irrigated","2040_Non_Irrigated"),
                       names_to = "year" ,
                       values_to = 'value')


Top_10 <- Top_10 %>%
  separate(year, c("Year", "Type"), "_") %>% 
  mutate(priority = "Top 10")

Top_10$Year <- as.numeric(Top_10$Year)

### Top 20
Top_20 <- filter(Data, Data$`26prior_rf_max_fut_20` == 3)

Top_20 <- Top_20[,c(22:27)]
Top_20 <- as.data.frame(t(colMeans(na.omit(Top_20))))

Top_20 <- pivot_longer(Top_20, 
                       cols = c("2015_Non_Irrigated", "2020_Non_Irrigated", "2025_Non_Irrigated", "2030_Non_Irrigated", "2035_Non_Irrigated","2040_Non_Irrigated"),
                       names_to = "year" ,
                       values_to = 'value')


Top_20 <- Top_20 %>%
  separate(year, c("Year", "Type"), "_") %>% 
  mutate(priority = "Top 20")

Top_20$Year <- as.numeric(Top_20$Year)


### Rest
Rest <- filter(Data, Data$`26prior_rf_max_fut_20` == 0)

Rest <- Rest[,c(22:27)]
Rest <- as.data.frame(t(colMeans(na.omit(Rest))))

Rest <- pivot_longer(Rest, 
                     cols = c("2015_Non_Irrigated", "2020_Non_Irrigated", "2025_Non_Irrigated", "2030_Non_Irrigated", "2035_Non_Irrigated","2040_Non_Irrigated"),
                     names_to = "year" ,
                     values_to = 'value')


Rest <- Rest %>%
  separate(year, c("Year", "Type"), "_") %>% 
  mutate(priority = "Rest")

Rest$Year <- as.numeric(Rest$Year)

All <- rbind(top_5, Top_10, Top_20, Rest)


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
    tabla$land_use <- c("Non_Irrigated")
    tabla$period <- c("Future")
    
    tabla_fut <- rbind(tabla_fut, tabla)  
  }
}

tabla_fut <- tabla_fut[1:27,]

# PRESENTE/FUTURO ----

## Natural ----

### Top 5
top_5 <- filter(Data, Data$`12prior_rf_max_pres_5` == 3 & Data$`24prior_rf_max_fut_5` == 3)

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

### Top 10
Top_10 <- filter(Data, Data$`13prior_rf_max_pres_10` == 3 & Data$`25prior_rf_max_fut_10` == 3)

Top_10 <- Top_10[,c(8:13)]
Top_10 <- as.data.frame(t(colMeans(na.omit(Top_10))))

Top_10 <- pivot_longer(Top_10, 
                       cols = c("2015_Natural", "2020_Natural", "2025_Natural", "2030_Natural", "2035_Natural","2040_Natural"),
                       names_to = "year" ,
                       values_to = 'value')


Top_10 <- Top_10 %>%
  separate(year, c("Year", "Type"), "_") %>% 
  mutate(priority = "Top 10")

Top_10$Year <- as.numeric(Top_10$Year)

### Top 20
Top_20 <- filter(Data, Data$`14prior_rf_max_pres_20` == 3 & Data$`26prior_rf_max_fut_20` == 3)

Top_20 <- Top_20[,c(8:13)]
Top_20 <- as.data.frame(t(colMeans(na.omit(Top_20))))

Top_20 <- pivot_longer(Top_20, 
                       cols = c("2015_Natural", "2020_Natural", "2025_Natural", "2030_Natural", "2035_Natural","2040_Natural"),
                       names_to = "year" ,
                       values_to = 'value')


Top_20 <- Top_20 %>%
  separate(year, c("Year", "Type"), "_") %>% 
  mutate(priority = "Top 20")

Top_20$Year <- as.numeric(Top_20$Year)


### Rest
Rest <- filter(Data, Data$`14prior_rf_max_pres_20` == 0 & Data$`26prior_rf_max_fut_20` == 0)

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

All <- rbind(top_5, Top_10, Top_20, Rest)



# Tabla vacía para guardar los resultados 
tabla_pre_fut <- data.frame(
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
    tabla$period <- c("Present/Future")
    
    tabla_pre_fut <- rbind(tabla_pre_fut, tabla)  # Unimos tablas
  }
}

tabla_pre_fut <- tabla_pre_fut[1:9,]

## Irrigated -----

### Top 5
top_5 <- filter(Data, Data$`12prior_rf_max_pres_5` == 3 & Data$`24prior_rf_max_fut_5` == 3)

top_5 <- top_5[,c(15:20)]
top_5 <- as.data.frame(t(colMeans(na.omit(top_5))))

top_5 <- pivot_longer(top_5, 
                      cols = c("2015_Irrigated", "2020_Irrigated", "2025_Irrigated", "2030_Irrigated", "2035_Irrigated","2040_Irrigated"),
                      names_to = "year" ,
                      values_to = 'value')


top_5 <- top_5 %>%
  separate(year, c("Year", "Type"), "_") %>% 
  mutate(priority = "Top 5")

top_5$Year <- as.numeric(top_5$Year)

### Top 10
Top_10 <- filter(Data, Data$`13prior_rf_max_pres_10` == 3 & Data$`25prior_rf_max_fut_10` == 3)

Top_10 <- Top_10[,c(15:20)]
Top_10 <- as.data.frame(t(colMeans(na.omit(Top_10))))

Top_10 <- pivot_longer(Top_10, 
                       cols = c("2015_Irrigated", "2020_Irrigated", "2025_Irrigated", "2030_Irrigated", "2035_Irrigated","2040_Irrigated"),
                       names_to = "year" ,
                       values_to = 'value')


Top_10 <- Top_10 %>%
  separate(year, c("Year", "Type"), "_") %>% 
  mutate(priority = "Top 10")

Top_10$Year <- as.numeric(Top_10$Year)

### Top 20
Top_20 <- filter(Data, Data$`14prior_rf_max_pres_20` == 3 & Data$`26prior_rf_max_fut_20` == 3)

Top_20 <- Top_20[,c(15:20)]
Top_20 <- as.data.frame(t(colMeans(na.omit(Top_20))))

Top_20 <- pivot_longer(Top_20, 
                       cols = c("2015_Irrigated", "2020_Irrigated", "2025_Irrigated", "2030_Irrigated", "2035_Irrigated","2040_Irrigated"),
                       names_to = "year" ,
                       values_to = 'value')


Top_20 <- Top_20 %>%
  separate(year, c("Year", "Type"), "_") %>% 
  mutate(priority = "Top 20")

Top_20$Year <- as.numeric(Top_20$Year)


### Rest
Rest <- filter(Data, Data$`14prior_rf_max_pres_20` == 0 & Data$`26prior_rf_max_fut_20` == 0)

Rest <- Rest[,c(15:20)]
Rest <- as.data.frame(t(colMeans(na.omit(Rest))))

Rest <- pivot_longer(Rest, 
                     cols = c("2015_Irrigated", "2020_Irrigated", "2025_Irrigated", "2030_Irrigated", "2035_Irrigated","2040_Irrigated"),
                     names_to = "year" ,
                     values_to = 'value')


Rest <- Rest %>%
  separate(year, c("Year", "Type"), "_") %>% 
  mutate(priority = "Rest")

Rest$Year <- as.numeric(Rest$Year)

All <- rbind(top_5, Top_10, Top_20, Rest)


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
    tabla$land_use <- c("Irrigated")
    tabla$period <- c("Present/Future")
    
    tabla_pre_fut <- rbind(tabla_pre_fut, tabla)  # Unimos tablas
  }
}

tabla_pre_fut <- tabla_pre_fut[1:18,]

## Non_Irrigated -----

### Top 5
top_5 <- filter(Data, Data$`12prior_rf_max_pres_5` == 3 & Data$`24prior_rf_max_fut_5` == 3)

top_5 <- top_5[,c(22:27)]
top_5 <- as.data.frame(t(colMeans(na.omit(top_5))))

top_5 <- pivot_longer(top_5, 
                      cols = c("2015_Non_Irrigated", "2020_Non_Irrigated", "2025_Non_Irrigated", "2030_Non_Irrigated", "2035_Non_Irrigated","2040_Non_Irrigated"),
                      names_to = "year" ,
                      values_to = 'value')


top_5 <- top_5 %>%
  separate(year, c("Year", "Type"), "_") %>% 
  mutate(priority = "Top 5")

top_5$Year <- as.numeric(top_5$Year)

### Top 10
Top_10 <- filter(Data, Data$`13prior_rf_max_pres_10` == 3 & Data$`25prior_rf_max_fut_10` == 3)

Top_10 <- Top_10[,c(22:27)]
Top_10 <- as.data.frame(t(colMeans(na.omit(Top_10))))

Top_10 <- pivot_longer(Top_10, 
                       cols = c("2015_Non_Irrigated", "2020_Non_Irrigated", "2025_Non_Irrigated", "2030_Non_Irrigated", "2035_Non_Irrigated","2040_Non_Irrigated"),
                       names_to = "year" ,
                       values_to = 'value')


Top_10 <- Top_10 %>%
  separate(year, c("Year", "Type"), "_") %>% 
  mutate(priority = "Top 10")

Top_10$Year <- as.numeric(Top_10$Year)

### Top 20
Top_20 <- filter(Data, Data$`14prior_rf_max_pres_20` == 3 & Data$`26prior_rf_max_fut_20` == 3)

Top_20 <- Top_20[,c(22:27)]
Top_20 <- as.data.frame(t(colMeans(na.omit(Top_20))))

Top_20 <- pivot_longer(Top_20, 
                       cols = c("2015_Non_Irrigated", "2020_Non_Irrigated", "2025_Non_Irrigated", "2030_Non_Irrigated", "2035_Non_Irrigated","2040_Non_Irrigated"),
                       names_to = "year" ,
                       values_to = 'value')


Top_20 <- Top_20 %>%
  separate(year, c("Year", "Type"), "_") %>% 
  mutate(priority = "Top 20")

Top_20$Year <- as.numeric(Top_20$Year)


### Rest
Rest <- filter(Data, Data$`14prior_rf_max_pres_20` == 0 & Data$`26prior_rf_max_fut_20` == 0)

Rest <- Rest[,c(22:27)]
Rest <- as.data.frame(t(colMeans(na.omit(Rest))))

Rest <- pivot_longer(Rest, 
                     cols = c("2015_Non_Irrigated", "2020_Non_Irrigated", "2025_Non_Irrigated", "2030_Non_Irrigated", "2035_Non_Irrigated","2040_Non_Irrigated"),
                     names_to = "year" ,
                     values_to = 'value')


Rest <- Rest %>%
  separate(year, c("Year", "Type"), "_") %>% 
  mutate(priority = "Rest")

Rest$Year <- as.numeric(Rest$Year)

All <- rbind(top_5, Top_10, Top_20, Rest)


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
    tabla$land_use <- c("Non_Irrigated")
    tabla$period <- c("Present/Future")
    
    tabla_pre_fut <- rbind(tabla_pre_fut, tabla)  
  }
}

tabla_pre_fut <- tabla_pre_fut[1:27,]

Results <- rbind(tabla_pre, tabla_fut, tabla_pre_fut)
writexl::write_xlsx(Results, "C:/GITHUB_REP/morocco_be/Results.xlsx")

# PLOTS ----

ggplot() + 
  geom_smooth(data= top_5,  aes(x = Year, y = value), col = "black", fill = "black", method = "lm", alpha = .2)+
  geom_smooth(data= Top_10, aes(x = Year, y = value), col = "red", fill = "red", method = "lm", alpha = .2)+
  geom_smooth(data= Top_20, aes(x = Year, y = value), col = "blue", fill = "blue", method = "lm", alpha = .2)+
  geom_smooth(data= Rest,   aes(x = Year, y = value), col = "green", fill = "green", method = "lm", alpha = .2)+
  annotate("text", x = 2035, y=69, label = "Top 5", col = "black")+
  annotate("text", x = 2035, y=68, label = "Top 10", col = "red")+
  annotate("text", x = 2035, y=67, label = "Top 20", col = "blue")+
  annotate("text", x = 2035, y=66, label = "Rest", col = "green")+
  ggtitle("Natural (Present priority areas)")+
  labs(x= "Year", y = "Land use probability")+
  theme_minimal()

ggplot() + 
  geom_smooth(data= top_5,  aes(x = Year, y = value), col = "black", fill = "black", method = "lm", alpha = .2)+
  geom_smooth(data= Top_10, aes(x = Year, y = value), col = "red", fill = "red", method = "lm", alpha = .2)+
  geom_smooth(data= Top_20, aes(x = Year, y = value), col = "blue", fill = "blue", method = "lm", alpha = .2)+
  geom_smooth(data= Rest,   aes(x = Year, y = value), col = "green", fill = "green", method = "lm", alpha = .2)+
  annotate("text", x = 2017, y=3.2, label = "Top 5", col = "black")+
  annotate("text", x = 2017, y=3.1, label = "Top 10", col = "red")+
  annotate("text", x = 2017, y=3, label = "Top 20", col = "blue")+
  annotate("text", x = 2017, y=2.9, label = "Rest", col = "green")+
  ggtitle("Irrigated (Present priority areas)")+
  labs(x= "Year", y = "Land use probability")+
  theme_minimal()

ggplot() + 
  geom_smooth(data= top_5,  aes(x = Year, y = value), col = "black", fill = "black", method = "lm", alpha = .2)+
  geom_smooth(data= Top_10, aes(x = Year, y = value), col = "red", fill = "red", method = "lm", alpha = .2)+
  geom_smooth(data= Top_20, aes(x = Year, y = value), col = "blue", fill = "blue", method = "lm", alpha = .2)+
  geom_smooth(data= Rest,   aes(x = Year, y = value), col = "green", fill = "green", method = "lm", alpha = .2)+
  annotate("text", x = 2037, y=9, label = "Top 5", col = "black")+
  annotate("text", x = 2037, y=8, label = "Top 10", col = "red")+
  annotate("text", x = 2037, y=7, label = "Top 20", col = "blue")+
  annotate("text", x = 2037, y=6, label = "Rest", col = "green")+
  ggtitle("Non-Irrigated (Present priority areas)")+
  labs(x= "Year", y = "Land use probability")+
  theme_minimal()
