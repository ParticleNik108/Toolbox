# This is a simple workflow script to implement analysis and usage of our functions 


# First source all our tools we have coded ! 

suppressPackageStartupMessages({
source("cnn_data_formatter.R")
source("cnn_data_tools.R")
source("Distributiontools.R")
source("plot_sample_sigs.R")
source("Pulse_Height.R")
source("trd_signal_extractor.R")
})


# Next lets ensure we have all the packaes needed

suppressPackageStartupMessages({
library(tidyverse)
library(ggplot2)
#library(ggpubr)
library(patchwork)
library(gridExtra)
library(reticulate)
library(reshape2)
})


pions <- read.csv("digits_data_pion.csv")

sig_dataframe <- get_TRD_signals(pions)
#save(sig_dataframe, file = "sig_dataframe.RData")



# Now get the Condensed Signals 
sig_cd <- condense(sig_dataframe)
#save(sig_cd, file = "sig_cd.RData")


# Process into cnn format 
sig_pics <- cnn_preprocess(sig_cd)
#save(sig_pics, file = "sig_pics.RData")


# Now to call plots 

ggsave("AdcSumDist.png" ,plot=dist_adc_sum(sig_cd[, -c(121)]), width = 8, height = 6)
ggsave("AdcMeanDist.png" , plot=dist_adc_mean(sig_cd[, -c(121)]),  width = 8, height = 6)
ggsave("AdcSdDist.png", plot=dist_adc_sd(sig_cd[, -c(121)]),  width = 8, height = 6)


ggsave("AvgPh.png",plot=avgpulse2(sig_cd)[[1]], width = 8, height = 6)

dist_adc_sum(sig_cd[, -c(121)])
dist_adc_mean(sig_cd[, -c(121)])
dist_adc_sd(sig_cd[, -c(121)])

avgpulse2(sig_cd)[[1]]

imgph(sig_cd)

ggsave("Signals.png", plot=sample_signals(sig_pics), width = 8, height = 6) 



s0 <- imgphsm(sig_cd %>% filter(SM==0))[[2]]
s1 <- imgphsm(sig_cd %>% filter(SM==1))[[2]]
s2 <- imgphsm(sig_cd %>% filter(SM==2))[[2]]
s3 <- imgphsm(sig_cd %>% filter(SM==3))[[2]]
s4 <- imgphsm(sig_cd %>% filter(SM==4))[[2]]
s5 <- imgphsm(sig_cd %>% filter(SM==5))[[2]]
s6 <- imgphsm(sig_cd %>% filter(SM==6))[[2]]
s7 <- imgphsm(sig_cd %>% filter(SM==7))[[2]]
s8 <- imgphsm(sig_cd %>% filter(SM==8))[[2]]
s9 <- imgphsm(sig_cd %>% filter(SM==9))[[2]]
s10 <- imgphsm(sig_cd %>% filter(SM==10))[[2]]
s11 <- imgphsm(sig_cd %>% filter(SM==11))[[2]]
s12 <- imgphsm(sig_cd %>% filter(SM==12))[[2]]
s13 <- imgphsm(sig_cd %>% filter(SM==13))[[2]]
s14 <- imgphsm(sig_cd %>% filter(SM==14))[[2]]
s15 <- imgphsm(sig_cd %>% filter(SM==15))[[2]]
s16 <- imgphsm(sig_cd %>% filter(SM==16))[[2]]
s17 <- imgphsm(sig_cd %>% filter(SM==17))[[2]]


# The code below should produce the supermodule plots in an Rplots.pdf file when run 
# on the server but we can ggsave them for surety !
Supermodplot <- (s0 + s1 + s2)/(s3 + s4 + s5)/(s6 + s7 + s8)
Supermodplot



Supermodplot2 <- (s9 + s10 + s11)/(s12 + s13 + s14)/(s15 + s16 + s17)
Supermodplot2

ggsave("SMs0_to_8.png", plot= Supermodplot, width = 8, height = 6)
ggsave("SMs9_to_17.png", plot= Supermodplot2, width = 8, height = 6)


