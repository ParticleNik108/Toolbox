
# some tools for plotting and visualization 


# Function to convert data into long form for plotting 

long_form <- function(df){
  
  # list all the column names in a vector
  cnames <- c("adc_0", "adc_1", "adc_2", "adc_3", "adc_4",
              "adc_5", 
              "adc_6",
              "adc_7",
              "adc_8",
              "adc_9",
              "adc_10",
              "adc_11",
              "adc_12",
              "adc_13",
              "adc_14",
              "adc_15",
              "adc_16",
              "adc_17",
              "adc_18",
              "adc_19",
              "adc_20",
              "adc_21",
              "adc_22",
              "adc_23",
              "adc_24",
              "adc_25",
              "adc_26",
              "adc_27",
              "adc_28",
              "adc_29")
  
  # use the pivot longer function to make data into long format where each adc value has a timebin 
  long_digits <- df  %>% pivot_longer(cols = cnames, names_to = "timebin",
                                      names_prefix = "adc_")
  
  
  
  # convert the timebins to integers 
  long_digits$timebin <- as.integer(long_digits$timebin)
  
  # return long form dataset 
  
  return(long_digits)
  
}



#-------------------------------------------------------------------------------




# Heat map plots 
# this take a dataframe in LONG FORM AS INPUT 
hmap <- function(df, Det, row, ps, pe){
  
  df  %>% filter(detector==Det ,padrow==row, between(pad, ps, pe)) %>% 
    ggplot() +
    geom_raster(aes(x=timebin, y=pad, fill=value)) + 
    ggtitle(paste("Det: ",Det ,"padrow: ", row )) +
    scale_fill_viridis_c() 
  
  
}
