#--- Load required packages 

library(readxl) 
library(tidyr)
library(reshape2)
library(fuzzyjoin)
library(dplyr)
library(ggplot2)

#--- Create function to parse player names 

parse_player_names = function(x){
  
  if(grepl(",", x) == TRUE){
    
    split = strsplit(x, ",")
    
    player_name = paste(trimws(split[[1]][2]), trimws(split[[1]][1]))
    
  }else{
    
    player_name = x
  }
  
  return(player_name)
  
}