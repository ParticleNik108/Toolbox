
# This is to convert our cleaned dataframe to a condensed form that would be easier to pass to a CNN


# First list all the necessary columns  

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



 


#-----------------------------------------------------------------------------

condense <- function(df) {
  
  # get rid of all the other columns and only retain cnames as above
  
  dfn <- df[, cnames]
  
  
  # Now lets get a sequence of starting ending points to loop over 
  # We want to grab batches of 4 rows at a time
  brks <- seq(1, nrow(dfn), 4)  # step in 4 
  
  
  # Now create empty dataframe to store all batches as a row 
  bat <- data.frame()
  
  # loop through the breaks to grab our batches an condense them
  
  for(i in brks) {
    batch <- dfn[seq(i, i+3), ] %>% as.matrix(dimnames=NULL) %>% t() %>%  array()
    bat <- rbind.data.frame(bat, batch)
  }
  
  # * include code to rename cols appropriately for bat 
  
  # Return the condensed dataframe
  return(bat)
    
  
}
  

#-------------------------------------------------------------------------------
# Now heres another function to reshape the condensed data for CNN feeding

reshape_condensed <- function(cond_df) {
  
  # First convert to matrix 
  reshp_df <- as.matrix(cond_df, dimnames = NULL)
  
  reshp_df <- reshp_df %>% array_reshape(dim = c(4, 30, nrow(cond_df)),
                                         order=c("F"))
  
  return(reshp_df)
}
  