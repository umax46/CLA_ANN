# Main program
#---------------------------------------------------------------------------------------
# Author: Diego Gonzalez Rodriguez
# Date: 21/03/2015
# Contact: umax46@gmail.com
#---------------------------------------------------------------------------------------
# Algorithm
#---------------------------------------------------------------------------------------
#Start training
#For each message do
# Start cla_ann
# Start learning
#End
#Calculate statistics
#---------------------------------------------------------------------------------------
source("utils.R"); # to include necessary functions
source("training.R"); # to include training function
source("cla_ann.R"); # to include cla_ann function
source("learning.R"); # to include learning function

# Set working directory
setwd("/Users/umax46/TECI/Redes\ Neuronales\ y\ aprendizaje\ estadistico/Practicas/CLA_ANN");

# Create an empty innate_input_layer
innate_input_layers <- data.frame(token=character(), weight=numeric(), msg_matched=numeric(), spam_matched=numeric(), stringsAsFactors=FALSE) 
adaptive_input_layers <- data.frame(token=character(), weight=numeric(), msg_matched=numeric(), spam_matched=numeric(), stringsAsFactors=FALSE) 
num_node <- 1000;

# Create the data frame to store the final result
res <- data.frame(origin_type=character(), evaluate_type=character(), stringsAsFactors=FALSE); 

#Start the training
innate_input_layers <- training(num_node, innate_input_layers);

# Set files from the training ham set
#validation_set_directory <-"./validation_ham_set";
validation_set_directory <-"./validation_spam_set";
emails<-list.files(path = validation_set_directory);
#type <- "NOT SPAM"
type <- "SPAM"

# Algorithm for validation emails
for (i in 1:length(emails)) {
  fileName <- paste(validation_set_directory, emails[i], sep="/");
  mail_tokens <- gimmie_tokens_from_message(fileName);
  mail_tokens <- mail_tokens [! mail_tokens %in% ""] #to evict "" in mail_tokens
  #res <- cla_ann(i, mail_tokens, res); 
  listResult <- cla_ann(i, mail_tokens);
  innate_input_layers <- listResult$innate_input_layers;
  adaptive_input_layers <- listResult$adaptive_input_layers;
  res <- listResult$res;
  # To update innate_input_layers once every 100 mails analyzed
  if (i %% 100 == 0 ) {
    listResult <- learning();
    innate_input_layers <- listResult$innate_input_layers;
    adaptive_input_layers <- listResult$adaptive_input_layers;
  }
}
right_evaluated <- sum(res$evaluate_type == res$origin_type, na.rm=TRUE);
total <- sum(res$evaluate_type != "ERROR", na.rm=TRUE);

precission <- (right_evaluated/total) * 100;
