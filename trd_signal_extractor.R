
# This is a function to obtain the TRD Signals 

#-------------------------------------------------------------------------------
# We first create a function to fill in the missing pad numbers to our dataframe

fill <- function(df){
  q1 <- c(1:142)
  #q2 <- c(2,3,5)
  #q3 <- c(2, 7, 4)
  
  #d <- cbind.data.frame(x = q2, y = q3)
  #d
  #df
  
  # this checks which elements from q1 are not in q2 and prints out those elements 
  miss <- q1[which(!q1 %in% df$pad)]
  #miss %>% print()
  
  for(m in miss){
    
    df <- rbind(df, c(rep(NA,4), df$detector[1], NA, df$padrow[1], m, rep(0,31)))
    #df <- rbind(df, c(rep(NA,6), m, rep(0,31)))
    #df <- rbind.data.frame(df, c(pad=m))
    df <- df %>% arrange(df$pad)
  }
  
  return(df)
  
}

#-------------------------------------------------------------------------------



# We create a function to get the local maxima 

#-------------------------------------------------------------------------------
local_max <- function(df){
  
  # initialize some local vectors and an empty dataframe
  event <- data.frame()
  maxima <- c()
  low <- c()
  high <- c()
  Hnn <- c()
  
  mpos <- c()
  lpos <- c()
  hpos <- c()
  Hnnpos <- c()  
  
  L <- length(df$pad) 
  
  for(p in 1:L){
    
    if(p==1){
      dlow <- 0
    }else{dlow <- df$ADC_sum[p] - df$ADC_sum[p-1]}
    
    
    if(p==L){
      dhi <- 0
    }else{dhi <- df$ADC_sum[p+1] - df$ADC_sum[p]}
    
    
    # condition for local max (satisfied)
    if(dhi<0 & dlow>0){
      #maxima <- append(maxima, df$ADC_sum[p])
      #mpos <- append(mpos,p)
      
      
      if(p==1){next
        #low <- append(low, 0)
        #lpos <- append(lpos, NA)
      }#else{
        #low <- append(low, df$ADC_sum[p-1])
        #lpos <- append(lpos, p-1)
      #}
      
      if(p==L){next
        #high <- append(high,0)
        #hpos <- append(hpos, NA)
        #Hnn <- append(Hnn, 0)
        #Hnnpos <- append(Hnnpos, NA)
      }#else{
        #high <- append(high,df$ADC_sum[p+1])
        #hpos <- append(hpos, p+1)
        #Hnn <- append(Hnn, df$ADC_sum[p+2])
        #Hnnpos <- append(Hnnpos, p+2)
      #}
      
      # add the events to our dataframe 
      event <- rbind.data.frame(event, df[c(p-1, p, p+1, p+2), ] )
    }
    
    
    # if theres consecutive zeros then just skip them
    if(dhi==0 & dlow==0){
      next} 
  }  
  
  #res <- cbind.data.frame(lpos, low, mpos, maxima, hpos, high, Hnnpos, Hnn)
  #return(res)
  
  return(event)  # we return the events we extracted and put into a dataframe 
  
}

#-----------------------------------------------------------------------------




# Now lets implement everything into one elegant function 

# df --> for each det --> for each padrow --> remove duplicates --> fill missing pads --> 
# get local max --> append to master dataframe that stores all the trd sigs


get_TRD_signals <- function(df){
  
  # First filter the data 
  df <- df %>% filter(ADC_sum>400, is_shared=="False")
  
  # create empty dataframe to store all the signals only 
  trd_sigs <- data.frame()
  
  
  det_range <- df$detector %>% unique() %>% sort()
  
  
  for(i in det_range){
    rows_per_det <- df$padrow[df$detector==i] %>% unique() %>% sort()
    
    for(j in rows_per_det){
      df_s <- df %>% filter(detector==i, padrow==j)
      
      # Remove duplicates (for now !)
      df_s <- df_s[!duplicated(df_s$pad), ] %>% arrange(pad)
      
      # Now call function to fill missing pads 
      df_s <- df_s %>% fill()
      
      # Now call local max finder and append sigs to trd_sigs dataframe
      trd_sigs <- rbind(trd_sigs, local_max(df_s))
      
      
    }
  }
  
  
  trd_sigs <- trd_sigs %>% mutate(SM = detector %/% 30)
  # return a dataframe with our extracted trd signals ! :) 
  return(trd_sigs)
  
  
}






