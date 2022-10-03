library(readxl)
library(tidyverse)
library(writexl)
library(ggpubr)
data <- read_excel("A:/MONICA/data_fig_2_2.xlsx", 
                         sheet = "TMAX")


data2 <- as.data.frame(sapply(data[,1:21], as.numeric))
data <- mutate(data2, data[,22])
data <- rbind(data, seq(1, 21))
data <- reshape2::melt(data)
write_xlsx(data, "A:/MONICA/data_fig_2_tmin.xlsx")
colnames(data) <- c("Model", "date", "Temp")

data3 <- read_excel("A:/MONICA/data_fig_1.xlsx", 
                   sheet = "Hoja3")

################################################################################
data <- read_excel("A:/MONICA/data_fig_2_2.xlsx", 
                   sheet = "TMIN")

datalogger_data <- filter(data, data$MODEL == "DATALOGGER")
chelsa_data <- filter(data, data$MODEL == "CHELSA")
microclima_data <- filter(data, data$MODEL == "MICROCLIMA")

datalogguer_tmax <- ggplot(datalogger_data)+
  geom_point(aes(x=date, y=Temp),col = "dodgerblue4")+
  geom_smooth(aes(x=date,y=Temp),col = "dodgerblue4", fill = "dodgerblue4", alpha = 0.2)+
  scale_x_discrete(name ="Date", 
                   limits=c("10_2011","11_2011","12_2011","01_2012","02_2012","03_2012","04_2012",
                            "05_2012","06_2012","07_2012","08_2012","09_2012","10_2012","11_2012",
                            "12_2012","01_2013","02_2013","03_2013","04_2013","05_2013","06_2013"))+
  xlab("Date") + ylab("Temperature (ºC)")+
  scale_y_continuous(limits = c(-5, 60))+
  theme(panel.background = element_rect(fill = "white"),
        legend.title = element_blank(),
        axis.title.x = element_blank(),
        legend.position = "none",
        axis.text.x = element_blank(),
        axis.ticks = element_blank())


chelsa_tmax <- ggplot(chelsa_data)+
  geom_point(aes(x=date, y=Temp), col = "darkorange")+
  geom_smooth(aes(x=date,y=Temp), col = "darkorange", fill = "darkorange", alpha = 0.2)+
  scale_x_discrete(name ="Date", 
                   limits=c("10_2011","11_2011","12_2011","01_2012","02_2012","03_2012","04_2012",
                            "05_2012","06_2012","07_2012","08_2012","09_2012","10_2012","11_2012",
                            "12_2012","01_2013","02_2013","03_2013","04_2013","05_2013","06_2013"))+
  xlab("Date") + ylab("Temperature (ºC)")+
  scale_y_continuous(limits = c(-5, 60))+
  theme(panel.background = element_rect(fill = "white"),
        legend.title = element_blank(),
        axis.title = element_blank(),
        legend.position = "none",
        axis.text.x = element_blank(),
        axis.ticks = element_blank())


microclima_tmax <- ggplot(microclima_data)+
  geom_point(aes(x=date, y=Temp), col = "darkolivegreen")+
  geom_smooth(aes(x=date,y=Temp), col = "darkolivegreen", fill = "darkolivegreen", alpha = 0.2)+
  scale_x_discrete(name ="Date", 
                   limits=c("Oct\n2011"," "," ","Jan\n2012","","","Apr\n2012"," ","Jul\n2012"," "," "," ",
                            "Oct\n2012"," "," ","Jan\n2013"," "," ","Apr\n2013"," ","Jun\n2013"))+
  xlab("Date") + ylab("Temperature (ºC)")+
  scale_y_continuous(limits = c(-5, 60))+
  theme(panel.background = element_rect(fill = "white"),
        legend.title = element_blank(),
        axis.title = element_blank(),
        legend.position = "none",
        axis.text.x = element_blank(),
        axis.ticks = element_blank())



datalogguer_tmin <- ggplot(datalogger_data)+
  geom_point(aes(x=date, y=Temp),col = "dodgerblue4")+
  geom_smooth(aes(x=date,y=Temp),col = "dodgerblue4", fill = "dodgerblue4", alpha = 0.2)+
  scale_x_discrete(name ="Date", 
                   limits=c("Oct\n2011"," "," ","Jan\n2012","","","Apr\n2012"," "," ","Aug\n2012"," "," ",
                            "Oct\n2012"," "," ","Jan\n2013"," "," ","Apr\n2013"," ","Jun\n2013"))+
  xlab("Date") + ylab("Temperature (ºC)")+
  scale_y_continuous(limits = c(-30, 20))+
  theme(panel.background = element_rect(fill = "white"),
        legend.title = element_blank(),
        legend.position = "none",
        axis.text.x = element_text(angle = 0),
        axis.ticks = element_blank())


chelsa_tmin <- ggplot(chelsa_data)+
  geom_point(aes(x=date, y=Temp), col = "darkorange")+
  geom_smooth(aes(x=date,y=Temp), col = "darkorange", fill = "darkorange", alpha = 0.2)+
  scale_x_discrete(name ="Date", 
                   limits=c("Oct\n2011"," "," ","Jan\n2012","","","Apr\n2012"," "," ","Aug\n2012"," "," ",
                            "Oct\n2012"," "," ","Jan\n2013"," "," ","Apr\n2013"," ","Jun\n2013"))+
  xlab("Date") + ylab("Temperature (ºC)")+
  scale_y_continuous(limits = c(-30, 20))+
  theme(panel.background = element_rect(fill = "white"),
        legend.title = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "none",
        axis.text.x = element_text(angle = 0),
        axis.ticks = element_blank())

lim <- as.Date(c("2011-10-01", "2013-06-01"))
microclima_tmin <- ggplot(microclima_data)+
  geom_point(aes(x=date, y=Temp), col = "darkolivegreen")+
  geom_smooth(aes(x=date,y=Temp), col = "darkolivegreen", fill = "darkolivegreen", alpha = 0.2)+
  scale_x_discrete(name ="Date", 
                   limits=c("Oct\n2011"," "," ","Jan\n2012","","","Apr\n2012"," "," ","Aug\n2012"," "," ",
                            "Oct\n2012"," "," ","Jan\n2013"," "," ","Apr\n2013"," ","Jun\n2013"))+
  xlab("Date") + ylab("Temperature (ºC)")+
  scale_y_continuous(limits = c(-30, 20))+
  theme(panel.background = element_rect(fill = "white"),
        legend.title = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "none",
        axis.text.x = element_text(angle = 0),
        axis.ticks = element_blank())

ggarrange(datalogguer_tmax, chelsa_tmax, microclima_tmax, 
          datalogguer_tmin, chelsa_tmin, microclima_tmin,
          labels = c("Datalogger\nTMax", "Mesoclimate\n", "Microclimate\n", "TMin"),
          ncol = 3, nrow = 2, align = "v")

"Oct\n2011"," "," ","Jan\n2012","","","Apr\n2012"," ","Jul\n2012"," "," "," ","Oct\n2012"," "," ","Jan\n2013"," "," ","Apr\n2013"," ","Jun\n2013"


################################################
 
datalogguer <- filter(data, data$MODEL == "DATALOGGUER")  
datalogguer <- reshape2::melt(datalogguer)
colnames(datalogguer) <- c("Model", "date", "Temp")

chelsa <- filter(data, data$MODEL == "CHELSA")  
chelsa <- reshape2::melt(chelsa)
colnames(chelsa) <- c("Model", "date", "Temp")

ggplot(datalogguer)+
  geom_point(aes(x=date, y=Temp, col = Model))+
  geom_smooth(aes(x=date, y=Temp),na.rm = TRUE,  alpha = 0.2)

ggplot(chelsa)+
  geom_point(aes(x=date, y=Temp, col = Model))+
  geom_smooth(aes(x=date, y=Temp),na.rm = TRUE,  alpha = 0.2)


ggplot(data)+
  geom_point(aes(x=date, y=Datalogguer), col = "dodgerblue4")+
  geom_smooth(aes(x=date, y=Datalogguer), col = "dodgerblue4", fill = "dodgerblue4", alpha = 0.2)+
  geom_smooth(aes(x=date, y=Datalogguer), col = "dodgerblue4", method = lm, se = FALSE, linetype = "dashed")+
  geom_point(aes(x=date, y=Chelsa), col =  "darkorange")+
  geom_smooth(aes(x=date, y=Chelsa), col = "darkorange", fill = "darkorange", alpha = 0.2)+
  geom_smooth(aes(x=date, y=Chelsa), col = "darkorange", method = lm, se = FALSE, linetype = "dashed")+
  geom_point(aes(x=date, y=Microclimate), col =  "darkolivegreen")+
  geom_smooth(aes(x=date, y=Microclimate), col = "darkolivegreen", fill = "darkolivegreen", alpha = 0.2)+
  geom_smooth(aes(x=date, y=Microclimate), col = "darkolivegreen", method = lm, se = FALSE, linetype = "dashed")+
  scale_x_discrete(name ="Date", 
                   limits=c("10_2011","11_2011","12_2011","01_2012","02_2012","03_2012","04_2012",
                            "05_2012","06_2012","07_2012","08_2012","09_2012","10_2012","11_2012",
                            "12_2012","01_2013","02_2013","03_2013","04_2013","05_2013","06_2013"))+
  xlab("Date") + ylab("Temperature (ºC)")+
  theme(panel.background = element_rect(fill = "white"),
        legend.title = element_blank(),
        legend.position = "none",
        axis.text.x = element_text(angle = 90),
        axis.ticks = element_blank())
  


limits=c("10_2011","11_2011","12_2011","01_2012","02_2012","03_2012","04_2012",
         "05_2012","06_2012","07_2012","08_2012","09_2012","10_2012","11_2012",
         "12_2012","01_2013","02_2013","03_2013","04_2013","05_2013","06_2013")