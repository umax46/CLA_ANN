# Anti-spam filter based in perceptron training with continous learning.
#---------------------------------------------------------------------------------------
# Author: Diego Gonzalez Rodriguez
# Date: 20/03/2015
# Contact: umax46@gmail.com
#---------------------------------------------------------------------------------------
# Algorithm
#---------------------------------------------------------------------------------------
#for each token in Innate_Input_Layer do
# if token matches message then 
#   total_weight <- total_weight + token.weight 
#   number_of_token_matched= number_of_token_matched+1
#   if token.weight > threshold then 
#     Desired_Output <- 0.9999 (Spam) 
#   else
#     Desired_Output <- 0.1111 (Ham) 
#   end if
# end if 
#end for
#score <- total_weight / number_of_token_matched
#if score > threshold then
# Message is spam
# error_rate <- Absolute (Desired_Output – Score) 
# correction <- Error_rate*Learning_rate
# Token.weight <- Token.weight + Correction
# If token does not exist in Innate _ Input_Layer then 
#   Add token to Adaptive_ Input_Layer (This is to represent continuous learning)
# end if
#else
# Message is not spam
# error_rate <- Absolute (Desired_Output – Score) 
# correction <- Error_rate*Learning_rate 
# Token.weight <- Token.weight - Correction
#end if
#---------------------------------------------------------------------------------------
cla_ann <- function (i, mail_tokens) {
  source("utils.R"); # to include necessary functions
  
  # Set working directory
  #setwd("/Users/umax46/TECI/Redes\ Neuronales\ y\ aprendizaje\ estadistico/Practicas/CLA_ANN");
  
  #res$evaluate_type == "SPAM";
  
  learning_rate <- 0.2;
  threshold <- 0.75;
  number_of_token_matched <- 0;
  total_weight <- 0;
  
  for (t in mail_tokens) {
    exist <- exist_token(toupper(t), innate_input_layers);
    if (exist) {
      index <- which(innate_input_layers$token == toupper(t))
      if (length(index) == 1) {
        total_weight <- total_weight + as.numeric(innate_input_layers$weight[index]);
        number_of_token_matched <- number_of_token_matched + 1;
        if (as.numeric(innate_input_layers$weight[index]) > threshold) {
          desired_output <- 0.9999; # Spam
        } else {
          desired_output <- 0.1111; # Ham
        }
      }
    }
  }
  score <- total_weight / number_of_token_matched;
  if (!is.nan(score)) {
    if (score > threshold) {
      # Message is spam
      error_rate <- abs(desired_output - score);
      correction <- error_rate * learning_rate;
      res[i, ] <- c(type,"SPAM");
      #--------------------------------------------------------------------
      listResult <- update_input_layers_being_spam(correction);
      innate_input_layers <- listResult$innate_input_layers;
      adaptive_input_layers <- listResult$adaptive_input_layers;
      #--------------------------------------------------------------------
    } else {
      # Message is NOT spam
      error_rate <- abs(desired_output - score);
      correction <- error_rate * learning_rate;
      res[i, ] <- c(type,"NOT SPAM");
      #--------------------------------------------------------------------
      #innate_input_layers <- update_input_layers_being_ham(correction);
      listResult <- update_input_layers_being_ham(correction);
      innate_input_layers <- listResult$innate_input_layers;
      adaptive_input_layers <- listResult$adaptive_input_layers;
      #--------------------------------------------------------------------
      
    }
  } else {
    res[i, ] <- c(type,"ERROR");
  }
  result <- list("innate_input_layers" = innate_input_layers, "adaptive_input_layers" = adaptive_input_layers, "res" = res);
  return(result);
  #return(res);
}
