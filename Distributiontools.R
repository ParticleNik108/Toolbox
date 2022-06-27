

# Functions to visualize the distributions of the ADC info from data 


#---------------------------------------------------------------------------
# Distribution of adc sums of the signals 

dist_adc_sum <- function(df, linecol="red") {
  
  sigsums <- df %>% apply(1, FUN = sum)
  sigsums <- as.data.frame(sigsums)
  
  # Create num of bins for the distribution using sturges rule 
  
  nbins = 1 + ceiling(log2(nrow(df)))
  
  # We log scale the x axis to make the distribution more visisble 
  sumdist <- ggplot(sigsums, aes(x= sigsums)) + 
    stat_bin(geom = "step", col=linecol, lwd=0.5 ,na.rm = T, bins = nbins) + 
    scale_x_log10() + 
    geom_text(aes(x= Inf, y= Inf, label=paste0("Mean = ", mean(sigsums, na.rm = T) %>% round(2))),
              hjust=1.3, vjust=3) + 
    labs(x="ADC Signal Sum", y = "counts") + 
    theme_bw()
  
  
  return(sumdist)
  
}



#-------------------------------------------------------------------------------

# Distribution of mean adc values of signals 


dist_adc_mean <- function(df, linecol="red") {
  
  sigmeans <- df %>% apply(1, FUN = mean)
  sigmeans <- as.data.frame(sigmeans)
  
  # Create breaks for the distribution using sturges rule 
  
  nbins = 1 + ceiling(log2(nrow(df)))
  
  # We log scale the x axis to make the distribution more visisble 
  ggplot(sigmeans, aes(x= sigmeans)) + 
    stat_bin(geom = "step", bins=nbins, col=linecol, lwd=0.5 ,na.rm = T) + 
    scale_x_log10() + 
    geom_text(aes(x= Inf, y= Inf, label=paste0("Mean = ", mean(sigmeans, na.rm = T) %>% round(2))),
              hjust=1.3, vjust=3) + 
    labs(x="Average ADC of Signals ", y = "counts") + 
    theme_bw()
  
}


#--------------------------------------------------------------------------------

# Distribution of the std dev of the adc values of signals 

dist_adc_sd <- function(df, linecol="red") {
  
  sigsd <- df %>% apply(1, FUN = sd)
  sigsd <- as.data.frame(sigsd)
  
  # Create breaks for the distribution using sturges rule 
  
  nbins = 1 + ceiling(log2(nrow(df)))
  
  # We log scale the x axis to make the distribution more visisble 
  ggplot(sigsd, aes(x= sigsd)) + 
    stat_bin(geom = "step", bins=nbins, col=linecol, lwd=0.5 ,na.rm = T) + 
    scale_x_log10() + 
    geom_text(aes(x= Inf, y= Inf, label=paste0("Mean = ", mean(sigsd, na.rm = T) %>% round(2))),
              hjust=1.3, vjust=3) + 
    labs(x="Standard deviations of Signal ADCs", y = "counts") + 
    theme_bw()
  
}







