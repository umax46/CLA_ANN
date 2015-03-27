# Common functions used in the CLA_ANN problem
#---------------------------------------------------------------------------------------
# Author: Diego Gonzalez Rodriguez
# Date: 22/03/2015
# Contact: umax46@gmail.com
#---------------------------------------------------------------------------------------

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
  html_elements = c("a","abbr","acronym","address","applet","area","article","aside","audio","b","base","basefont","bdi","bdo","bgsound","big","blink","blockquote","body","br","button","canvas","caption","center","cite","code","col","colgroup","command","content","data","datalist","dd","del","details","dfn","dialog","dir","div","dl","dt","element","em","embed","fieldset","figcaption","figure","font","footer","form","frame","frameset","head","header","hgroup","hr","html","i","iframe","image","img","input","ins","isindex","kbd","keygen","label","legend","li","link","listing","main","map","mark","marquee","menu","menuitem","meta","meter","multicol","nav","nobr","noembed","noframes","noscript","object","ol","optgroup","option","output","p","param","picture","plaintext","pre","progress","q","rp","rt","rtc","ruby","s","samp","script","section","select","shadow","small","source","spacer","span","strike","strong","style","sub","summary","sup","table","tbody","td","template","textarea","tfoot","th","thead","time","title","tr","track","tt","u","ul","var","video","wbr","xmp");
  mail_tokens <- mail_tokens [! mail_tokens %in% html_elements]; 
  return(mail_tokens);
}

# Function to check if token exists in message
exist_token <- function(mail_token, input_layers) {
  return(any(grepl(mail_token, input_layers$token)));
}

# Function to retrieve a valid index for new token
# If table is full check for token with minimum weight
gimmie_index <- function(min_weight, input_layers) {
  current_index <- nrow(input_layers);
  if (length(current_index) == 0) {
    current_index <- 1;
  }
  if (current_index >= num_node) {
    current_weight <- input_layers$weight[which.min(input_layers$weight)];
    if ( current_weight >= min_weight) {
      return(0);
    } else {
      return(which.min(input_layers$weight));
    }  
  } else {
    return(current_index + 1);
  }
}
 
# To delete those entries with message_matched = min_value
delete_insignificant_tokens <- function(innate_input_layers, min_value) {
  innate_input_layers <- subset(innate_input_layers,innate_input_layers$msg_matched != as.numeric(min_value));
  innate_input_layers <- subset(innate_input_layers,innate_input_layers$spam_matched != as.numeric(min_value));
  return(innate_input_layers);
}

# To update innate_input_layers with a more precise weight and to add new tokens in adaptive_input_layers 
# for continous learning in the case of being a spam message
update_input_layers_being_spam <- function(correction) {
  for (t in mail_tokens) {
    exist <- exist_token(toupper(t), innate_input_layers);
    if (exist) { # token is in innate_input_layers
      index <- which(innate_input_layers$token == t)
      if (length(index) == 1) {
        innate_input_layers$weight[index] <- min(abs(as.numeric(innate_input_layers$weight[index]) + correction), 0.999999999999);
      }
    } else { # token has to be added in adaptive_input_layers for continuous learning
      exist <- exist_token(toupper(t), adaptive_input_layers);
      if (exist) {
        index <- which(adaptive_input_layers$token == t)
        if (length(index) == 1) {
          adaptive_input_layers$weight[index] <- min(abs(as.numeric(adaptive_input_layers$weight[index]) + correction), 0.999999999999);
        }
      } else {
        index <- gimmie_index((as.numeric(correction) + 0.01), adaptive_input_layers);
        if (length(index) == 1) {
          adaptive_input_layers[index, ] <- c(toupper(t), as.numeric(correction), 1, spam_increment);   
        } else {
          adaptive_input_layers[1, ] <- c(toupper(t), as.numeric(correction), 1, spam_increment);   
        }
      }
    }
  }
  list_result <- list("innate_input_layers" = innate_input_layers, "adaptive_input_layers" = adaptive_input_layers);
  return(list_result);
}

# To update innate_input_layers with a more precise weight in the case of being a ham message
update_input_layers_being_ham <- function(correction) {
  for (t in mail_tokens) {
    exist <- exist_token(toupper(t), innate_input_layers);
    if (exist) {
      index <- which(innate_input_layers$token == t);
      if (length(index) == 1) {
        innate_input_layers$weight[index] <- min(abs(as.numeric(innate_input_layers$weight[index]) - correction), 0.999999999999);
      }
    }
  }
  list_result <- list("innate_input_layers" = innate_input_layers, "adaptive_input_layers" = adaptive_input_layers);
  #return(innate_input_layers);
  return(list_result);
}
