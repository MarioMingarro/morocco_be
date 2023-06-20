
library(readxl)
library(tidyverse)
library(writexl)

Data <- read_excel("All_basins_results_GIS.xlsx")

Data %>% 
  group_by(`12prior_rf_max_pres_5`) %>% 
  summarise(median = median(`2015_Natural`))

Data %>% 
  group_by(`24prior_rf_max_fut_5`) %>% 
  summarise(median = median(`2040_Natural`))

Data %>% 
  group_by(`12prior_rf_max_pres_5`) %>% 
  summarise(median = median(`2015_Irrigated`))

Data %>% 
  group_by(`24prior_rf_max_fut_5`) %>% 
  summarise(median = median(`2040_Irrigated`))

Data %>% 
  group_by(`12prior_rf_max_pres_5`) %>% 
  summarise(median = median(`2015_Non_Irrigated`))

Data %>% 
  group_by(`24prior_rf_max_fut_5`) %>% 
  summarise(median = median(`2040_Non_Irrigated`))

kk <- Data %>% 
select(`12prior_rf_max_pres_5`, `2015_Natural`)
colnames(kk) <- c("prior", "prob")
  

ggplot(kk, aes(x=prior, y=prob, group = prior)) + 
  geom_boxplot() 

  geom_line(aes(group =  ID),
            alpha = 0.5, linetype=2, col = "gray")+
  ggtitle("Irrigated crop") +
  xlab("Year") +
  ylab("Land use probability")+
  theme_minimal()

 kk <-  Data %>% 
    group_by(`12prior_rf_max_pres_5`) %>% 
    select(`2015_Natural`) 
 
 
kk1 <- filter(Data, `12prior_rf_max_pres_5` == 3) 
kk1 <- kk1[["2015_Natural"]]

kk2 <- filter(Data, `24prior_rf_max_fut_5` == 3) 
kk2 <- kk2[["2040_Natural"]]
res <- wilcox.test(kk1, kk2, exact=FALSE, simulate.p.value=TRUE)

kk1 <- filter(Data, `12prior_rf_max_pres_5` == 3) 
kk1 <- kk1[["2015_Irrigated"]]

kk2 <- filter(Data, `24prior_rf_max_fut_5` == 3) 
kk2 <- kk2[["2040_Irrigated"]]

res <- wilcox.test(kk1, kk2, exact=FALSE, simulate.p.value=TRUE)

kk1 <- filter(Data, `12prior_rf_max_pres_5` == 3) 
kk1 <- kk1[["2015_Non_Irrigated"]]

kk2 <- filter(Data, `24prior_rf_max_fut_5` == 3) 
kk2 <- kk2[["2040_Non_Irrigated"]]

res <- wilcox.test(kk1, kk2, exact=FALSE, simulate.p.value=TRUE)