# Code for training the neural network. Implemented using a data frame
#---------------------------------------------------------------------------------------
# Author: Diego Gonzalez Rodriguez
# Date: 18/03/2015
# Contact: umax46@gmail.com
#---------------------------------------------------------------------------------------
# Algorithm
#---------------------------------------------------------------------------------------
#For each token in the spam message corpus do
# If layer is already exist in Innate_Input_Layer then 
#    Innate_Input_Layer.msg_matched->Innate_Input_Layer.msg_matched + 1 
#    Innate_Input_Layer.spam_matched->Innate_Input_Layer.spam_matched + spam_increment 
# else
#  Add token to Innate_Input_Layer 
#  Innate_Input_Layer.msg_matched->Innate_Input_Layer.msg_matched + 1 
#  Innate_Input_Layer.spam_matched->Innate_Input_Layer.spam_matched + spam_increment 
# end if
#end for
#---------------------------------------------------------------------------------------
library(stringr)

# Set working directory
setwd("/Users/umax46/TECI/Redes\ Neuronales\ y\ aprendizaje\ estadistico/Practicas/CLA_ANN");

# Create an empty innate_input_layer
innate_input_layers <- data.frame(token=character(), weight=numeric(), msg_matched=numeric(), spam_matched=numeric(), stringsAsFactors=FALSE) 
# Define the size of the neural network
num_node = 100;

# Function to get tokens from a message
gimmie_tokens_from_message <- function(fileName) {
  file <- scan(fileName, what=character(), sep="", skip = 30, fileEncoding="UTF-8-BOM");
  htlm_start_elements <- str_match(file,"<\\w+>");
  htlm_end_elements <- str_match(file,"</\\w+>");
  file <- file [! file %in% htlm_start_elements]; # to delete html elements
  file <- file [! file %in% htlm_end_elements]; # to delete html elements
  mail_tokens <- str_match(file,"\\w+");
  mail_tokens <- na.omit(mail_tokens);
  numbers <- regexpr("[^\\.0-9]",mail_tokens) == -1; 
  mail_tokens <- mail_tokens [numbers == FALSE]; # to delete numeric values
  return(mail_tokens);
}

# Function to check if token exists in message
exist_token <- function(mail_token) {
  return(any(grepl(mail_token, innate_input_layers$token)));
}

# Function to retrieve a valid index for new token
# If table is full check for token with minimum weight
gimmie_index <- function(min_weight) {
  current_index <- nrow(innate_input_layers);
  if (current_index >= num_node) {
    current_weight <- innate_input_layers$weight[which.min(innate_input_layers$weight)];
    if ( current_weight >= min_weight) {
      return(0);
    } else {
      return(which.min(innate_input_layers$weight));
    }  
  } else {
    return(current_index + 1);
  }
}

# Set files from the training spam set
training_set_directory <-"./training_spam_set";
spam<-list.files(path = training_set_directory);

# Algorithm for spam training
spam_increment = 1;
for (i in 1:length(spam)) {
  fileName <- paste(training_set_directory, spam[i], sep="/");
  mail_tokens <- gimmie_tokens_from_message(fileName);
  mail_tokens <- mail_tokens [! mail_tokens %in% ""] #to evict "" in mail_tokens
  for (t in mail_tokens) {
    exist <- exist_token(toupper(t));
   if (exist) {
     index <- which(innate_input_layers$token == t)
     if (length(index) == 1) {
       innate_input_layers$msg_matched[index] <- as.numeric(innate_input_layers$msg_matched[index]) + 1;
       innate_input_layers$spam_matched[index] <- as.numeric(innate_input_layers$spam_matched[index]) + spam_increment;
       new_weight <- as.numeric(innate_input_layers$spam_matched[index]) / (as.numeric(innate_input_layers$msg_matched[index]) + 1);
       innate_input_layers$weight[index] <- new_weight;
     }
    } else {
      index <- gimmie_index(0.1);
      if (index != 0) {
        innate_input_layers[index, ] <- c(toupper(t), 0.08, 1, spam_increment);        
      }
    }
  }
}


# Set files from the training ham set
training_set_directory <-"./training_ham_set";
ham<-list.files(path = training_set_directory);

# Algorithm for ham training
for (i in 1:length(ham)) {
  fileName <- paste(training_set_directory, ham[i], sep="/");
  mail_tokens <- gimmie_tokens_from_message(fileName);
  mail_tokens <- mail_tokens [! mail_tokens %in% ""] #to evict "" in mail_tokens
  for (t in mail_tokens) {
    exist <- exist_token(toupper(t));
    if (exist) {
      index <- which(innate_input_layers$token == t)
      if (length(index) == 1) {
        innate_input_layers$msg_matched[index] <- as.numeric(innate_input_layers$msg_matched[index]) + 1;
        new_weight <- as.numeric(innate_input_layers$spam_matched[index]) / (as.numeric(innate_input_layers$msg_matched[index]) + 1);
        innate_input_layers$weight[index] <- new_weight;
      }
    }
  }
}
