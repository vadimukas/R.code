library(readr)

UA13_2019_2020 <- read_csv("UA13_2019-2020.csv", 
                           col_types = cols(Sample = col_integer(), 
                                            Month = col_factor(levels = c("1", 
                                                                          "2", "3", "4", "5", "6", "7", 
                                                                          "8", "9", "10", "11", "12")), 
                                            Year = col_factor(levels = c("2019", 
                                                                         "2020"))))

View(UA13_2019_2020)

library(ggplot2)

ggplot(UA13_2019_2020) +
 aes(x = "Year", y = `Discharge, m3/s`) +
 geom_boxplot() +
 facet_wrap(vars(Year))+
  theme_test()

ggplot(UA13_2019_2020) +
 aes(x = "", y = `Discharge, m3/s`, group = Month) +
 geom_boxplot(fill = "#ef562d") +
 labs(x = "month") +
 theme_minimal() +
 facet_wrap(vars(Year))

library(DataExplorer)

plot_boxplot(UA13_2019_2020 [1:396,] [c(4, 7:10)], by = "Month", title = "UA13 2019", ggtheme = theme_bw())
plot_boxplot(UA13_2019_2020 [397:714,] [c(4, 7:10)], by = "Month", title = "UA13 2020", ggtheme = theme_bw())
plot_boxplot(UA13_2019_2020 [c(5, 7:10)], by = "Year", title = "UA13", ggtheme = theme_bw())

pairs(UA13_2019_2020 [1:396,] [c(7:10)])
pairs(UA13_2019_2020 [397:714,][c(7:10)])


