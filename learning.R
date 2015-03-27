# Anti-spam filter based in perceptron training with continous learning.
#---------------------------------------------------------------------------------------
# Author: Diego Gonzalez Rodriguez
# Date: 22/03/2015
# Contact: umax46@gmail.com
#---------------------------------------------------------------------------------------
# Algorithm
#---------------------------------------------------------------------------------------
#if criteria happened then
# Merge Adaptive_ Input_Layers with Innate_Input_Layers and then order it descending based on Token.spam_matched
#end if
#Select top Innate_Input_Layers
#Adaptive_ Input_Layers <- Empty
#---------------------------------------------------------------------------------------
learning <- function() {
  # Combine two data frame
  combine_input_layers <- rbind(innate_input_layers, adaptive_input_layers);  
  # Order new data frame by weight
  combine_input_layers <- combine_input_layers[order(as.numeric(combine_input_layers$spam_matched), decreasing = TRUE),]; 
  # Select top num_node values of new data frame
  new_input_layers <- combine_input_layers[1:num_node,];
  new_input_layers <- na.omit(new_input_layers);
  adaptive_input_layers <- data.frame(token=character(), weight=numeric(), msg_matched=numeric(), spam_matched=numeric(), stringsAsFactors=FALSE) 
  list_result <- list("innate_input_layers" = new_input_layers, "adaptive_input_layers" = adaptive_input_layers);
  return(list_result);
}
