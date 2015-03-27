# Training the neural network. Implemented using a data frame
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
training <- function(nodes, innate_input_layers) {
  library(stringr)

  # Define the size of the neural network nodes
  num_node <- nodes;
  
  # Set files from the training spam set
  training_spam_set_directory <-"./training_spam_set";
  spam<-list.files(path = training_spam_set_directory);
  
  # Algorithm for spam training
  spam_increment = 1;
  for (i in 1:length(spam)) {
    fileName <- paste(training_spam_set_directory, spam[i], sep="/");
    mail_tokens <- gimmie_tokens_from_message(fileName);
    mail_tokens <- mail_tokens [! mail_tokens %in% ""] #to evict "" in mail_tokens
    for (t in mail_tokens) {
      exist <- exist_token(toupper(t), innate_input_layers);
      if (exist) {
        index <- which(innate_input_layers$token == t)
        if (length(index) == 1) {
          innate_input_layers$msg_matched[index] <- as.numeric(innate_input_layers$msg_matched[index]) + 1;
          innate_input_layers$spam_matched[index] <- as.numeric(innate_input_layers$spam_matched[index]) + spam_increment;
          new_weight <- as.numeric(innate_input_layers$spam_matched[index]) / (as.numeric(innate_input_layers$msg_matched[index]) + 1);
          innate_input_layers$weight[index] <- new_weight;
        }
      } else {
        index <- gimmie_index(0.1, innate_input_layers);
        if (index != 0) {
          innate_input_layers[index, ] <- c(toupper(t), 0.08, 1, spam_increment);        
        }
      }
    }
  }
  
  # Set files from the training ham set
  training_ham_set_directory <-"./training_ham_set";
  ham<-list.files(path = training_ham_set_directory);
  
  # Algorithm for ham training
  for (i in 1:length(ham)) {
    fileName <- paste(training_ham_set_directory, ham[i], sep="/");
    mail_tokens <- gimmie_tokens_from_message(fileName);
    mail_tokens <- mail_tokens [! mail_tokens %in% ""] #to evict "" in mail_tokens
    for (t in mail_tokens) {
      exist <- exist_token(toupper(t), innate_input_layers);
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
  # Delete those entries which spam_matched and msg_matched is equal to 1
  innate_input_layers <- delete_insignificant_tokens(innate_input_layers, 1); 
  return(innate_input_layers);
}
