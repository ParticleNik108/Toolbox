

# Functions fot sampling and plotting trd signals 

# They take in as input the reshaped data ready to be fed to a cnn ie. 4x30 arrays 

library(ggplot2)
library(gridExtra)
library(ggpubr)
library(cowplot)
library(patchwork)
library(reshape2)


sample_signals <- function(df, nb = 6) {
  
  samps <- sample(1:dim(df)[1], size = nb)
  
  p1 <- df[samps[1],,] %>% melt() %>% ggplot(aes(x=Var2, y=Var1, fill=value)) + geom_tile() + 
    scale_fill_viridis_c() +
    labs(x="Time Bin (100ns)", y="Pads", fill="ADC\nvalue") + theme_bw() + 
    theme(axis.text.y = element_blank() ) 
  
  p2 <- df[samps[2],,] %>% melt() %>% ggplot(aes(x=Var2, y=Var1, fill=value)) + geom_tile() + 
    scale_fill_viridis_c() +
    labs(x="Time Bin (100ns)", y="Pads", fill="ADC\nvalue") + theme_bw() + 
    theme(axis.text.y = element_blank() ) 
  
  p3 <- df[samps[3],,] %>% melt() %>% ggplot(aes(x=Var2, y=Var1, fill=value)) + geom_tile() + 
    scale_fill_viridis_c() +
    labs(x="Time Bin (100ns)", y="Pads", fill="ADC\nvalue") + theme_bw() + 
    theme(axis.text.y = element_blank() ) 
  
  p4 <- df[samps[4],,] %>% melt() %>% ggplot(aes(x=Var2, y=Var1, fill=value)) + geom_tile() + 
    scale_fill_viridis_c() +
    labs(x="Time Bin (100ns)", y="Pads", fill="ADC\nvalue") + theme_bw() + 
    theme(axis.text.y = element_blank() ) 
  
  p5 <- df[samps[5],,] %>% melt() %>% ggplot(aes(x=Var2, y=Var1, fill=value)) + geom_tile() + 
    scale_fill_viridis_c() +
    labs(x="Time Bin (100ns)", y="Pads", fill="ADC\nvalue") + theme_bw() + 
    theme(axis.text.y = element_blank() ) 
  
  p6 <- df[samps[6],,] %>% melt() %>% ggplot(aes(x=Var2, y=Var1, fill=value)) + geom_tile() + 
    scale_fill_viridis_c() +
    labs(x="Time Bin (100ns)", y="Pads", fill="ADC\nvalue") + theme_bw() + 
    theme(axis.text.y = element_blank() ) 
  
  
  
  plotgrids <- (p1 + p2)/(p3+p4)/(p5+p6)
  
  #plotgrids <- plot_grid(plotlist = c(p1,p2, p3, p4, p5, p6), ncol = 2)
  
  
  return(plotgrids)
  
}



# Save the plot ! 
# -----------

#ggsave("TRD_signals_6.png",
 #      plot = sample_signals(pion_pics),
  #     width = 9, height = 6)




  