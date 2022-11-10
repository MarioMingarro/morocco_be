library(readxl)
library(tidyverse)
library(ggpubr)

Data <- read_excel("All_basins_results.xlsx", sheet = "Hoja2")

x <- "Year" # Variable dependiente
y <- "value" # Variables independiente

# PRESENTE ----
## Natural ----

### Top 5
top_5_p_n <- filter(Data, Data$`12prior_rf_max_pres_5` == 3)

top_5_p_n <- top_5_p_n[,c(8:13)]
top_5_p_n <- as.data.frame(t(colMeans(na.omit(top_5_p_n))))

top_5_p_n <- pivot_longer(top_5_p_n, 
                      cols = c("2015_Natural", "2020_Natural", "2025_Natural", "2030_Natural", "2035_Natural","2040_Natural"),
                      names_to = "year" ,
                      values_to = 'value')


top_5_p_n <- top_5_p_n %>%
  separate(year, c("Year", "Type"), "_") %>% 
  mutate(priority = "Top 5")

top_5_p_n$Year <- as.numeric(top_5_p_n$Year)

### Rest_p_n
Rest_p_n <- filter(Data, Data$`14prior_rf_max_pres_20` == 0|Data$`13prior_rf_max_pres_10` == 3|Data$`14prior_rf_max_pres_20` == 3)

Rest_p_n <- Rest_p_n[,c(8:13)]
Rest_p_n <- as.data.frame(t(colMeans(na.omit(Rest_p_n))))

Rest_p_n <- pivot_longer(Rest_p_n, 
                     cols = c("2015_Natural", "2020_Natural", "2025_Natural", "2030_Natural", "2035_Natural","2040_Natural"),
                     names_to = "year" ,
                     values_to = 'value')


Rest_p_n <- Rest_p_n %>%
  separate(year, c("Year", "Type"), "_") %>% 
  mutate(priority = "Rest")

Rest_p_n$Year <- as.numeric(Rest_p_n$Year)

All <- rbind(top_5_p_n, Rest_p_n)

# Tabla vacia para guardar los resultados 
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

## Irrigated -----

### Top 5
top_5_p_i <- filter(Data, Data$`12prior_rf_max_pres_5` == 3)

top_5_p_i <- top_5_p_i[,c(15:20)]
top_5_p_i <- as.data.frame(t(colMeans(na.omit(top_5_p_i))))

top_5_p_i <- pivot_longer(top_5_p_i, 
                          cols = c("2015_Irrigated", "2020_Irrigated", "2025_Irrigated", "2030_Irrigated", "2035_Irrigated","2040_Irrigated"),
                          names_to = "year" ,
                          values_to = 'value')


top_5_p_i <- top_5_p_i %>%
  separate(year, c("Year", "Type"), "_") %>% 
  mutate(priority = "Top 5")

top_5_p_i$Year <- as.numeric(top_5_p_i$Year)

### Rest
Rest_p_i <- filter(Data, Data$`14prior_rf_max_pres_20` == 0|Data$`13prior_rf_max_pres_10` == 3|Data$`14prior_rf_max_pres_20` == 3)

Rest_p_i <- Rest_p_i[,c(15:20)]
Rest_p_i <- as.data.frame(t(colMeans(na.omit(Rest_p_i))))

Rest_p_i <- pivot_longer(Rest_p_i, 
                         cols = c("2015_Irrigated", "2020_Irrigated", "2025_Irrigated", "2030_Irrigated", "2035_Irrigated","2040_Irrigated"),
                         names_to = "year" ,
                         values_to = 'value')


Rest_p_i <- Rest_p_i %>%
  separate(year, c("Year", "Type"), "_") %>% 
  mutate(priority = "Rest")

Rest_p_i$Year <- as.numeric(Rest_p_i$Year)

All <- rbind(top_5_p_i, Rest_p_i)


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

## Non_Irrigated -----

### Top 5
top_5_p_ni <- filter(Data, Data$`12prior_rf_max_pres_5` == 3)

top_5_p_ni <- top_5_p_ni[,c(22:27)]
top_5_p_ni <- as.data.frame(t(colMeans(na.omit(top_5_p_ni))))

top_5_p_ni <- pivot_longer(top_5_p_ni, 
                           cols = c("2015_Non_Irrigated", "2020_Non_Irrigated", "2025_Non_Irrigated", "2030_Non_Irrigated", "2035_Non_Irrigated","2040_Non_Irrigated"),
                           names_to = "year" ,
                           values_to = 'value')


top_5_p_ni <- top_5_p_ni %>%
  separate(year, c("Year", "Type"), "_") %>% 
  mutate(priority = "Top 5")

top_5_p_ni$Year <- as.numeric(top_5_p_ni$Year)

### Rest
Rest_p_ni <- filter(Data, Data$`14prior_rf_max_pres_20` == 0|Data$`13prior_rf_max_pres_10` == 3|Data$`14prior_rf_max_pres_20` == 3)

Rest_p_ni <- Rest_p_ni[,c(22:27)]
Rest_p_ni <- as.data.frame(t(colMeans(na.omit(Rest_p_ni))))

Rest_p_ni <- pivot_longer(Rest_p_ni, 
                          cols = c("2015_Non_Irrigated", "2020_Non_Irrigated", "2025_Non_Irrigated", "2030_Non_Irrigated", "2035_Non_Irrigated","2040_Non_Irrigated"),
                          names_to = "year" ,
                          values_to = 'value')


Rest_p_ni <- Rest_p_ni %>%
  separate(year, c("Year", "Type"), "_") %>% 
  mutate(priority = "Rest")

Rest_p_ni$Year <- as.numeric(Rest_p_ni$Year)

All <- rbind(top_5_p_ni, Rest_p_ni)


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
    
    tabla_pre <- rbind(tabla_pre, tabla)  # Unimos tablas
  }
}

# FUTURO ----
## Natural ----

### Top 5
top_5_f_n <- filter(Data, Data$`24prior_rf_max_fut_5` == 3)

top_5_f_n <- top_5_f_n[,c(8:13)]
top_5_f_n <- as.data.frame(t(colMeans(na.omit(top_5_f_n))))

top_5_f_n <- pivot_longer(top_5_f_n, 
                          cols = c("2015_Natural", "2020_Natural", "2025_Natural", "2030_Natural", "2035_Natural","2040_Natural"),
                          names_to = "year" ,
                          values_to = 'value')


top_5_f_n <- top_5_f_n %>%
  separate(year, c("Year", "Type"), "_") %>% 
  mutate(priority = "Top 5")

top_5_f_n$Year <- as.numeric(top_5_f_n$Year)

### Rest_f_n
Rest_f_n <- filter(Data, Data$`26prior_rf_max_fut_20` == 0|Data$`25prior_rf_max_fut_10` == 3|Data$`26prior_rf_max_fut_20` == 3)

Rest_f_n <- Rest_f_n[,c(8:13)]
Rest_f_n <- as.data.frame(t(colMeans(na.omit(Rest_f_n))))

Rest_f_n <- pivot_longer(Rest_f_n, 
                         cols = c("2015_Natural", "2020_Natural", "2025_Natural", "2030_Natural", "2035_Natural", "2040_Natural"),
                         names_to = "year" ,
                         values_to = 'value')


Rest_f_n <- Rest_f_n %>%
  separate(year, c("Year", "Type"), "_") %>% 
  mutate(priority = "Rest")

Rest_f_n$Year <- as.numeric(Rest_f_n$Year)

All <- rbind(top_5_f_n, Rest_f_n)

# Tabla vacia para guardar los resultados 
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

## Irrigated -----

### Top 5
top_5_f_i <- filter(Data, Data$`24prior_rf_max_fut_5` == 3)

top_5_f_i <- top_5_f_i[,c(15:20)]
top_5_f_i <- as.data.frame(t(colMeans(na.omit(top_5_f_i))))

top_5_f_i <- pivot_longer(top_5_f_i, 
                          cols = c("2015_Irrigated", "2020_Irrigated", "2025_Irrigated", "2030_Irrigated", "2035_Irrigated","2040_Irrigated"),
                          names_to = "year" ,
                          values_to = 'value')


top_5_f_i <- top_5_f_i %>%
  separate(year, c("Year", "Type"), "_") %>% 
  mutate(priority = "Top 5")

top_5_f_i$Year <- as.numeric(top_5_f_i$Year)

### Rest
Rest_f_i <- filter(Data, Data$`26prior_rf_max_fut_20` == 0|Data$`25prior_rf_max_fut_10` == 3|Data$`26prior_rf_max_fut_20` == 3)

Rest_f_i <- Rest_f_i[,c(15:20)]
Rest_f_i <- as.data.frame(t(colMeans(na.omit(Rest_f_i))))

Rest_f_i <- pivot_longer(Rest_f_i, 
                         cols = c("2015_Irrigated", "2020_Irrigated", "2025_Irrigated", "2030_Irrigated", "2035_Irrigated","2040_Irrigated"),
                         names_to = "year" ,
                         values_to = 'value')


Rest_f_i <- Rest_f_i %>%
  separate(year, c("Year", "Type"), "_") %>% 
  mutate(priority = "Rest")

Rest_f_i$Year <- as.numeric(Rest_f_i$Year)

All <- rbind(top_5_f_i, Rest_f_i)


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

## Non_Irrigated -----

### Top 5
top_5_f_ni <- filter(Data, Data$`24prior_rf_max_fut_5` == 3)

top_5_f_ni <- top_5_f_ni[,c(22:27)]
top_5_f_ni <- as.data.frame(t(colMeans(na.omit(top_5_f_ni))))

top_5_f_ni <- pivot_longer(top_5_f_ni, 
                           cols = c("2015_Non_Irrigated", "2020_Non_Irrigated", "2025_Non_Irrigated", "2030_Non_Irrigated", "2035_Non_Irrigated","2040_Non_Irrigated"),
                           names_to = "year" ,
                           values_to = 'value')


top_5_f_ni <- top_5_f_ni %>%
  separate(year, c("Year", "Type"), "_") %>% 
  mutate(priority = "Top 5")

top_5_f_ni$Year <- as.numeric(top_5_f_ni$Year)

### Rest
Rest_f_ni <- filter(Data, Data$`26prior_rf_max_fut_20` == 0|Data$`25prior_rf_max_fut_10` == 3|Data$`26prior_rf_max_fut_20` == 3)

Rest_f_ni <- Rest_f_ni[,c(22:27)]
Rest_f_ni <- as.data.frame(t(colMeans(na.omit(Rest_f_ni))))

Rest_f_ni <- pivot_longer(Rest_f_ni, 
                          cols = c("2015_Non_Irrigated", "2020_Non_Irrigated", "2025_Non_Irrigated", "2030_Non_Irrigated", "2035_Non_Irrigated","2040_Non_Irrigated"),
                          names_to = "year" ,
                          values_to = 'value')


Rest_f_ni <- Rest_f_ni %>%
  separate(year, c("Year", "Type"), "_") %>% 
  mutate(priority = "Rest")

Rest_f_ni$Year <- as.numeric(Rest_f_ni$Year)

All <- rbind(top_5_f_ni, Rest_f_ni)


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
    
    tabla_fut <- rbind(tabla_fut, tabla)  # Unimos tablas
  }
}


# PLOTS ----
## Present -----
pre <- ggplot() + 
  geom_smooth(data= top_5_p_n,  aes(x = Year, y = value), col = "darkgreen", fill = "darkgreen", method = "lm", alpha = .2)+
  geom_smooth(data= top_5_p_i,  aes(x = Year, y = value), col = "darkgoldenrod1", fill = "darkgoldenrod1", method = "lm", alpha = .2)+
  geom_smooth(data= top_5_p_ni, aes(x = Year, y = value), col = "sienna4", fill = "sienna4", method = "lm", alpha = .2)+
  geom_smooth(data= Rest_p_n,  aes(x = Year, y = value), col = "darkgreen", fill = "darkgreen", method = "lm", alpha = .2,linetype = "dashed")+
  geom_smooth(data= Rest_p_i,  aes(x = Year, y = value), col = "darkgoldenrod1", fill = "darkgoldenrod1", method = "lm", alpha = .2,linetype = "dashed")+
  geom_smooth(data= Rest_p_ni, aes(x = Year, y = value), col = "sienna4",   fill = "sienna4",   method = "lm", alpha = .2,linetype = "dashed")+
  labs(x= "Year", y = "Land use probability")+
  scale_y_continuous(limits=c(0,80))+
  theme(
    panel.background = element_rect(fill = "white"),
    title = element_blank()
  )

## Future -----
fut <- ggplot() + 
  geom_smooth(data= top_5_f_n,  aes(x = Year, y = value), col = "darkgreen", fill = "darkgreen", method = "lm", alpha = .2)+
  geom_smooth(data= top_5_f_i,  aes(x = Year, y = value), col = "darkgoldenrod1", fill = "darkgoldenrod1", method = "lm", alpha = .2)+
  geom_smooth(data= top_5_f_ni, aes(x = Year, y = value), col = "sienna4", fill = "sienna4", method = "lm", alpha = .2)+
  geom_smooth(data= Rest_f_n,  aes(x = Year, y = value), col = "darkgreen", fill = "darkgreen", method = "lm", alpha = .2,linetype = "dashed")+
  geom_smooth(data= Rest_f_i,  aes(x = Year, y = value), col = "darkgoldenrod1", fill = "darkgoldenrod1", method = "lm", alpha = .2,linetype = "dashed")+
  geom_smooth(data= Rest_f_ni, aes(x = Year, y = value), col = "sienna4",   fill = "sienna4",   method = "lm", alpha = .2,linetype = "dashed")+
  labs(x= "Year", y = "Land use probability")+
  scale_y_continuous(limits=c(0,80))+
  theme(
    panel.background = element_rect(fill = "white"),
    axis.text.y  = element_blank(),
    axis.title.y = element_blank(),
    title = element_blank()
  )
# Extract the legend. Returns a table
leg <- ggplot() + 
  geom_smooth(data= top_5_f_n,  aes(x = Year, y = value, col = "darkgreen"), fill = "white", method = "lm", alpha = .2)+
  geom_smooth(data= top_5_f_i,  aes(x = Year, y = value, col = "darkgoldenrod1"), fill = "white", method = "lm", alpha = .2)+
  geom_smooth(data= top_5_f_ni, aes(x = Year, y = value, col = "sienna4"), fill = "white", method = "lm", alpha = .2)+
  scale_color_manual(name = 'Land use category',
                   breaks = c("Natural", 'Irrigated', "Non-Irrigated"),
                   values = c("Natural" = "darkgreen", "Irrigated" = "darkgoldenrod1", "Non-Irrigated" = "sienna4"))+
  theme(
    panel.background = element_rect(fill = "white"))
  
leg <- get_legend(leg)
plot(leg)

# Convert to a ggplot and print
as_ggplot(leg)

tabla_pre[,c(-1, -6:-8)] <- round(tabla_pre[,c(-1, -6:-8)],2)

table.p <- ggtexttable(tabla_pre[, c(1:2, 7:8)], rows = NULL, theme = ttheme("mOrange"))

ggarrange(pre, fut, table.p, labels = c("Present", "Future"), ncol = 2, nrow = 2)
