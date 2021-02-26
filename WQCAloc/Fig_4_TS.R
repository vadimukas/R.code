# making time series for bubble plots

library(readr)
# starting with EC for KZ site in stream water 
EC_KZ_TS <- read_csv("EC KZ TS.csv", 
                     col_types = cols(year = col_integer(), 
                                      month = col_integer(), day = col_integer()))

library(lubridate)
EC_KZ_TSd <-EC_KZ_TS %>% 
  mutate(Date = make_date(year, month, day))

is.Date(EC_KZ_TSd$Date)

library(ggplot2)

library(dplyr)
EC_KZ_TSd <- EC_KZ_TSd %>%
  select(Date, `KZ-8`, `KZ-9`) %>%
  gather(key = "variable", value = "value", -Date)

fig_4a<-ggplot(EC_KZ_TSd, aes(x = Date, y = value)) + 
  geom_line(aes(color = variable), size = 0.7, show.legend = FALSE) +
  geom_point(size = 1.5, color= "black") +
  scale_color_manual(values = c("red", "blue")) +
  labs(y = "EC [microS/cm]") +
  scale_x_date(date_labels = "%m-%y", date_breaks = "1 month")+
  ggthemes::theme_base()+
  theme(text=element_text(size = 12),axis.text.x = element_text(angle = 45, hjust = 1),plot.title = element_text(size=12, face="plain", vjust = -20, hjust = 0.35))



ggsave(fig_4a, filename = "fig_4a.png", width = 6, height = 4, units="in", dpi = 300)
dev.off()

# continue with nitrate in Groundwater TJ-6
Nitrate_TJ_TS <- read_csv("Nitrate TJ TS.csv", 
                          col_types = cols(year = col_integer(), 
                                           month = col_integer(), day = col_integer()))

library(lubridate)
Nitrate_TJ_TSd <-Nitrate_TJ_TS %>% 
  mutate(Date = make_date(year, month, day))

is.Date(Nitrate_TJ_TSd$Date)


fig_4b<-ggplot(Nitrate_TJ_TSd, aes(x = Date, y = `TJ-6 NO3-N [mg/L]`)) + 
  geom_line(size = 0.7, color="red") +
  geom_point(size = 1.5, color= "black") +
  labs(y = "N03-N [mg/L]", title = "TJ-6") +
  scale_x_date(date_labels = "%m-%y", date_breaks = "1 month")+
  geom_hline(yintercept = 11.3, color="red", size= 0.5, linetype="dashed")+
  ggthemes::theme_base()+
  theme(text=element_text(size = 12),axis.text.x = element_text(angle = 45, hjust = 1), plot.title = element_text(color="red", size=12, face="plain", vjust = -20, hjust = 0.35))

ggsave(fig_4b, filename = "fig_4b.png", width = 6, height = 4, units="in", dpi = 300)
dev.off()


# finishing with Phosphate in KG 

Phosphate_KG_TS <- read_csv("Phosphate_KG_TS.csv", 
                            col_types = cols(year = col_integer(), 
                                             month = col_integer(), day = col_integer()))

library(lubridate)
Phosphate_KG_TSd <-Phosphate_KG_TS %>% 
  mutate(Date = make_date(year, month, day))

is.Date(Phosphate_KG_TSd$Date)


library(ggplot2)

library(dplyr)
Phosphate_KG_TSd <- Phosphate_KG_TSd %>%
  select(Date, `KG-1`, `KG-5`) %>%
  gather(key = "variable", value = "value", -Date)

fig_4c<-ggplot(Phosphate_KG_TSd, aes(x = Date, y = value)) + 
  geom_line(aes(color = variable), size = 0.7, show.legend = FALSE) +
  geom_point(size = 1.5, color= "black") +
  scale_color_manual(values = c("blue", "red")) +
  labs(y = "P04-P [mg/L]") +
  scale_x_date(date_labels = "%m-%y", date_breaks = "1 month")+
  ggthemes::theme_base()+
  theme(text=element_text(size = 12),axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(fig_4c, filename = "fig_4c.png", width = 6, height = 4, units="in", dpi = 300)
dev.off()

library(lattice)

xyplot(`KG-1`+`KG-5` ~ Date, data = Phosphate_KG_TSd, type = "l", auto.key = TRUE)

ggplot(Phosphate_KG_TSd) +
  aes(x = Date, y = `KG-1`) +
  geom_line(size = 0.5, colour = "blue") +
  geom_point(size = 1.5, color = "blue")+
  labs(y = "P04-P [mg/L]") +
  scale_x_date(date_labels = "%m-%y", date_breaks = "2 month")+
  theme(axis.text.x=element_text(angle=60, hjust=1))+ 
  ggthemes::theme_base()

ggplot(Phosphate_KG_TSd) +
  aes(x = Date, y = `KG-5`) +
  geom_line(size = 1L, colour = "#0c4c8a") +
  labs(y = "P04-P [mg\\L]") +
  ggthemes::theme_base()

