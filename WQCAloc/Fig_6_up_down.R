# Before we start install the required packages

# exploring new WQ data, first load the required packages
library(readr) 
library(ggplot2)
library(tidyverse)
#https://boxuancui.github.io/DataExplorer/ - package DataExplorer to explore the dataset
library(DataExplorer)
library(psych)
library(DescTools)
library(pastecs)
library(gridExtra)
library(xts)
library(ggthemes)


#set working directory to subfolder WQCAloc
setwd("D:/R/R.code/WQCAloc")

local_WQm_up <- read_csv("local_WQm_up.csv", col_types = cols(Month = col_factor(levels = c("1", 
                                                                                            "2", "3", "4", "5", "6", "7", "8", "9", 
                                                                                            "10", "11", "12")), Country = col_factor(levels = c("KZ", 
                                                                                                                                                "KG", "UZ", "TJ"))))
local_WQm_down <- read_csv("local_WQm_down.csv", col_types = cols(Month = col_factor(levels = c("1", 
                                                                                                "2", "3", "4", "5", "6", "7", "8", "9", 
                                                                                                "10", "11", "12")), Country = col_factor(levels = c("KZ", 
                                                                                                                                        "KG", "UZ", "TJ"))))
fig_6a <- local_WQm_up %>% 
  ggplot()+
  aes(Month, `EC [microS/cm]`)+
  geom_line (color="black")+
  geom_point(size=1)+
  theme_test()+
  theme(strip.background = element_rect(fill = "white"))+
  ylab(expression(paste("EC ("~uS~cm^-1,")")))+
  facet_wrap(~Site) 
ggsave(fig_6a, filename = "fig_6a.png", width = 5, height = 4, units="in", dpi = 300)
dev.off()

fig_6c <- local_WQm_up %>% 
  ggplot()+
  aes(Month, `NO3-N [mg/L]`)+
  geom_line (color="red")+
  geom_point(size=1)+
  theme_test()+
  theme(strip.background = element_rect(fill = "white"))+
  ylab(expression(NO[3]-N~mg~L^-1))+
  facet_wrap(~Site)
ggsave(fig_6c, filename = "fig_6c.png", width = 5, height = 4, units="in", dpi = 300)
dev.off()


local_WQm_up %>% 
  ggplot()+
  aes(Month, `NO3-N [mg/L]`)+
  geom_line (color="red")+
  geom_point(size=1)+
  ylab(expression(PO[4]- P~mg~L^-1))+
  theme_test()+
  theme(strip.background = element_rect(fill = "white"))+
  facet_wrap(~Site)

f3e <- local_WQm_up %>% 
  ggplot()+
  aes(Month, `PO4-P [mg/L]`)+
  geom_line (color="blue")+
  geom_point(size=1)+
  ylab(expression(PO[4]- P~mg~L^-1))+
  theme(strip.background = element_rect(fill = "white"))+
  theme_test()+
  facet_wrap(~Location)

fig_6e <- local_WQm_up %>% 
  ggplot()+
  aes(Month, `PO4-P [mg/L]`)+
  geom_line (color="blue")+
  geom_point(size=1)+
  ylab(expression(PO[4]- P~mg~L^-1))+
   theme_test()+
  theme(strip.background = element_rect(fill = "white"))+
   facet_wrap(~Site)
ggsave(fig_6e, filename = "fig_6e.png", width = 5, height = 4, units="in", dpi = 300)
dev.off()

# figure 3 for a pane second column - DOWNSTREAM

f3b <- local_WQm_down %>%
  ggplot () +
  aes(Month, `EC [microS/cm]`)+
  geom_line (color="black")+
  geom_point(size=1)+
  theme_test()+
  facet_wrap(~Location)

fig_6b <- local_WQm_down %>%
  ggplot () +
  aes(Month, `EC [microS/cm]`)+
  geom_line (color="black")+
  geom_point(size=1)+
  theme_test()+
  theme(strip.background = element_rect(fill = "white"))+
  ylab(expression(paste("EC ("~uS~cm^-1,")")))+
    facet_wrap(~Site)
ggsave(fig_6b, filename = "fig_6b.png", width = 5, height = 4, units="in", dpi = 300)
dev.off()

f3d <- local_WQm_down %>%
  ggplot ()+
  aes(Month, `NO3-N [mg/L]`)+
  geom_line (color="red")+
  geom_point(size=1)+
  theme_test()+
  facet_wrap(~Location)

fig_6d <- local_WQm_down %>%
  ggplot ()+
  aes(Month, `NO3-N [mg/L]`)+
  geom_line (color="red")+
  geom_point(size=1)+
  theme_test()+
  theme(strip.background = element_rect(fill = "white"))+
  ylab(expression(NO[3]-N~mg~L^-1))+
  facet_wrap(~Site)
ggsave(fig_6d, filename = "fig_6d.png", width = 5, height = 4, units="in", dpi = 300)
dev.off()

f3f <- local_WQm_down %>%
  ggplot ()+
  aes(Month, `PO4-P [mg/L]`)+
  geom_line (color="blue")+
  geom_point(size=1)+
  theme_test()+
  facet_wrap(~Location)

ggsave(fig_6f, filename = "fig_6f.png", width = 5, height = 4, units="in", dpi = 300)
dev.off()


fig_3v1<-grid.arrange(fig_3a, fig_3b, fig_3c, fig_3d, fig_3e, fig_3f, nrow = 3, ncol = 2)
ggsave(fig_3v1, filename = "fig_3v1.png", width = 10, height = 9, units="in", dpi = 600)
dev.off()

# importing month as numeric to allow to plot smooth lines 

local_WQm_up <- read_csv("local_WQm_up.csv", col_types = cols(Month = col_integer()))


fig_6a <- local_WQm_up %>% 
  ggplot()+
  aes(Month, `EC [microS/cm]`)+
  geom_point(size=1)+
  geom_smooth(method = "loess", se = TRUE, size=0.5, color = "black")+
  scale_x_continuous(breaks = seq(1, 12, by = 1))+
  theme_test()+
  labs(title = "(a)")+
  theme(plot.title = element_text(color="black", size=12, face="bold", vjust = 0, hjust = -0.08))+
  theme(strip.background = element_rect(fill = "white"))+
  ylab(expression(paste("EC ("~uS~cm^-1,")")))+
  facet_wrap(~Site) 
ggsave(fig_6a, filename = "fig_6a.png", width = 5, height = 4, units="in", dpi = 300)


fig_6c <- local_WQm_up %>% 
  ggplot()+
  aes(Month, `NO3-N [mg/L]`)+
  geom_point(size=1)+
  geom_smooth(method = "loess", se = TRUE, size=0.5, color = "red")+
  scale_x_continuous(breaks = seq(1, 12, by = 1))+
   theme_test()+
  labs(title = "(c)")+
  theme(plot.title = element_text(color="black", size=12, face="bold", vjust = 0, hjust = -0.08))+
  theme(strip.background = element_rect(fill = "white"))+
  ylab(expression(paste("NO3-N ("~mg~L^-1,")")))+
  facet_wrap(~Site)
ggsave(fig_6c, filename = "fig_6c.png", width = 5, height = 4, units="in", dpi = 300)

fig_6e <- local_WQm_up %>% 
  ggplot()+
  aes(Month, `PO4-P [mg/L]`)+
  geom_smooth(method = "loess", se = TRUE, size=0.5, color = "blue")+
  scale_x_continuous(breaks = seq(1, 12, by = 1))+
  geom_point(size=1)+
  ylab(expression(paste("PO4-P ("~mg~L^-1,")")))+
  theme_test()+
  labs(title = "(e)")+
  theme(plot.title = element_text(color="black", size=12, face="bold", vjust = 0, hjust = -0.08))+
  theme(strip.background = element_rect(fill = "white"))+
  facet_wrap(~Site)
ggsave(fig_6e, filename = "fig_6e.png", width = 5, height = 4, units="in", dpi = 300)

# figure 3 for a pane second column - DOWNSTREAM
# first it is needed to import month as numeric
local_WQm_down <- read_csv("local_WQm_down.csv", col_types = cols(Month = col_integer()))

fig_6b <- local_WQm_down %>%
  ggplot () +
  aes(Month, `EC [microS/cm]`)+
  geom_smooth(method = "loess", se = TRUE, size=0.5, color = "black")+
  scale_x_continuous(breaks = seq(1, 12, by = 1))+
  geom_point(size=1)+
  theme_test()+
  labs(title = "(b)")+
  theme(plot.title = element_text(color="black", size=12, face="bold", vjust = 0, hjust = -0.08))+
  theme(strip.background = element_rect(fill = "white"))+
  ylab(expression(paste("EC ("~uS~cm^-1,")")))+
  facet_wrap(~Site)
ggsave(fig_6b, filename = "fig_6b.png", width = 5, height = 4, units="in", dpi = 300)

fig_6d <- local_WQm_down %>%
  ggplot ()+
  aes(Month, `NO3-N [mg/L]`)+
  geom_smooth(method = "loess", se = TRUE, size=0.5, color = "red")+
  scale_x_continuous(breaks = seq(1, 12, by = 1))+
  geom_point(size=1)+
  theme_test()+
  labs(title = "(d)")+
  theme(plot.title = element_text(color="black", size=12, face="bold", vjust = 0, hjust = -0.08))+
  theme(strip.background = element_rect(fill = "white"))+
  ylab(expression(paste("NO3-N ("~mg~L^-1,")")))+
  facet_wrap(~Site)
ggsave(fig_6d, filename = "fig_6d.png", width = 5, height = 4, units="in", dpi = 300)


fig_6f <- local_WQm_down %>%
  ggplot ()+
  aes(Month, `PO4-P [mg/L]`)+
  geom_smooth(method = "loess", se = TRUE, size=0.5, color = "blue")+
  scale_x_continuous(breaks = seq(1, 12, by = 1))+
  geom_point(size=1)+
  theme_test()+
  labs(title = "(f)")+
  theme(plot.title = element_text(color="black", size=12, face="bold", vjust = 0, hjust = -0.08))+
  theme(strip.background = element_rect(fill = "white"))+
  ylab(expression(paste("PO4-P ("~mg~L^-1,")")))+
  facet_wrap(~Site)

ggsave(fig_6f, filename = "fig_6f.png", width = 5, height = 4, units="in", dpi = 300)



fig_6v1<-grid.arrange(fig_6a, fig_6b, fig_6c, fig_6d, fig_6e, fig_6f, nrow = 3, ncol = 2)
ggsave(fig_6v1, filename = "fig_6v1.png", width = 10, height = 9, units="in", dpi = 600)


library(ggcorrplot)
# correlation between EC and nitrate
KG_up<-local_WQm_up %>% 
  group_by(Site) %>%  
  filter (Country == "KG")
  plot_correlation(KG_up [c(15,17)])
  pairs (KG_up [c(15,17)])
  
  print(cor.test(KG_up$`NO3-N [mg/L]`, KG_up$`EC [microS/cm]`,
                  use = "pairwise", method = "kendall", alpha = .05, ci = TRUE), short = FALSE)
  print(cor.test(KG_up$`NO3-N [mg/L]`, KG_up$`EC [microS/cm]`,
                 use = "pairwise", method = "pearson", alpha = .05, ci = TRUE), short = FALSE)
  
  KZ_up<-local_WQm_up %>% 
    group_by(Site) %>%  
    filter (Country == "KZ")
  plot_correlation(KZ_up [c(15,17)])
  pairs (KZ_up [c(15,17)])

  print(cor.test(KZ_up$`NO3-N [mg/L]`, KZ_up$`EC [microS/cm]`,
                  use = "pairwise", method = "kendall", alpha = .05, ci = TRUE), short = FALSE)
  print(cor.test(KZ_up$`NO3-N [mg/L]`, KZ_up$`EC [microS/cm]`,
                 use = "pairwise", method = "pearson", alpha = .05, ci = TRUE), short = FALSE)  
  
  UZ_up<-local_WQm_up %>% 
    group_by(Site) %>%  
    filter (Country == "UZ")
  plot_correlation(UZ_up [c(15,17)])
  pairs (UZ_up [c(15,17)])
  
  print(cor.test(UZ_up$`NO3-N [mg/L]`, UZ_up$`EC [microS/cm]`,
                  use = "pairwise", method = "kendall", alpha = .05, ci = TRUE), short = FALSE)
  print(cor.test(UZ_up$`NO3-N [mg/L]`, UZ_up$`EC [microS/cm]`,
                 use = "pairwise", method = "pearson", alpha = .05, ci = TRUE), short = FALSE)

  TJ_up<-local_WQm_up %>% 
    group_by(Site) %>%  
    filter (Country == "TJ")
  plot_correlation(TJ_up [c(15,17)])
  pairs (TJ_up [c(15,17)])
  
  print(cor.test(TJ_up$`NO3-N [mg/L]`, TJ_up$`EC [microS/cm]`,
                  use = "pairwise", method = "kendall", alpha = .05, ci = TRUE), short = FALSE)
  print(cor.test(TJ_up$`NO3-N [mg/L]`, TJ_up$`EC [microS/cm]`,
                 use = "pairwise", method = "pearson", alpha = .05, ci = TRUE), short = FALSE)  
  
 lm(TJ_up$`NO3-N [mg/L]`~TJ_up$`EC [microS/cm]`)
 
# and now correlations downstream
 KG_down<-local_WQm_down %>% 
   group_by(Site) %>%  
   filter (Country == "KG")
 plot_correlation(KG_down [c(15,17)])
 pairs (KG_down [c(15,17)])
 
 print(cor.test(KG_down$`NO3-N [mg/L]`, KG_down$`EC [microS/cm]`,
                 use = "pairwise", method = "kendall", alpha = .05, ci = TRUE), short = FALSE)
 print(cor.test(KG_down$`NO3-N [mg/L]`, KG_down$`EC [microS/cm]`,
                use = "pairwise", method = "pearson", alpha = .05, ci = TRUE), short = FALSE)
 
 KZ_down<-local_WQm_down %>% 
   group_by(Site) %>%  
   filter (Country == "KZ")
 plot_correlation(KZ_down [c(15,17)])
 pairs (KZ_down [c(15,17)])
 
 print(cor.test(KZ_down$`NO3-N [mg/L]`, KZ_down$`EC [microS/cm]`,
                use = "pairwise", method = "kendall", alpha = .05, ci = TRUE), short = FALSE)
 print(cor.test(KZ_down$`NO3-N [mg/L]`, KZ_down$`EC [microS/cm]`,
                 use = "pairwise", method = "pearson", alpha = .05, ci = TRUE), short = FALSE)  
 
 UZ_down<-local_WQm_down %>% 
   group_by(Site) %>%  
   filter (Country == "UZ")
 plot_correlation(UZ_down [c(15,17)])
 pairs (UZ_down [c(15,17)])
 
 print(cor.test(UZ_down$`NO3-N [mg/L]`, UZ_down$`EC [microS/cm]`,
                 use = "pairwise", method = "kendall", alpha = .05, ci = TRUE), short = FALSE)
 print(cor.test(UZ_down$`NO3-N [mg/L]`, UZ_down$`EC [microS/cm]`,
                use = "pairwise", method = "pearson", alpha = .05, ci = TRUE), short = FALSE)
 
 
 TJ_down<-local_WQm_down %>% 
   group_by(Site) %>%  
   filter (Country == "TJ")
 plot_correlation(TJ_down [c(15,17)])
 pairs (TJ_down [c(15,17)])
 
 print(cor.test(TJ_down$`NO3-N [mg/L]`, TJ_down$`EC [microS/cm]`,
                use = "pairwise", method = "kendall", alpha = .05, ci = TRUE), short = FALSE)  
 print(cor.test(TJ_down$`NO3-N [mg/L]`, TJ_down$`EC [microS/cm]`,
                 use = "pairwise", method = "pearson", alpha = .05, ci = TRUE), short = FALSE)  
 
 plot_correlation(local_WQm_up [c(15, 17)])
 plot_correlation(local_WQm_down [c(15, 17)])

    pairs(local_WQm_up [c(15, 17)])
    pairs(local_WQm_down [c(15, 17)])
    
print(corr.test(local_WQm_up$`NO3-N [mg/L]`, local_WQm_up$`EC [microS/cm]`,
                use = "pairwise", method = "pearson", alpha = .05, ci = TRUE), short = FALSE)


print(corr.test(local_WQm_down$`NO3-N [mg/L]`, local_WQm_down$`EC [microS/cm]`,
                use = "pairwise", method = "pearson", alpha = .05, ci = TRUE), short = FALSE)

# adding the lowess line 

local_WQm_up %>% 
  ggplot(aes(x = Month, y = `EC [microS/cm]`))+
  geom_smooth(method = "loess", se = FALSE, size=0.5, color = "black")+
  geom_point(size=1)+
  theme_test()+
  theme(strip.background = element_rect(fill = "white"))+
  ylab(expression(paste("EC ("~uS~cm^-1,")")))+
  facet_wrap(~Site) 


  