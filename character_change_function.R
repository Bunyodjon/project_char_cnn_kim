change_random_character <- function(x) {
  for (j in 1:x) {
    number_character <- j
    # randomly select integers and replace them with one of the integers from the list of alphabete
    for (i in 1:length(sequences_small)) {
      k <- sample(1:lengths(sequences_small[i]), min(number_character, lengths(sequences_small[i])) , replace = FALSE)
      sequences_small[[i]][k] <- sample(list_of_alphabet, number_character, replace = TRUE)
    }  
    
    # pad the test data maxlen is 1014 
    test_small <- pad_sequences(sequences_small, maxlen = 1014, padding = 'post')
    
    pred_small <- model %>% predict(test_small)
    class <- apply(pred_small[ ,1:4], 1, which.max)
    change_or_not <- ifelse(pred$initial_prediction==class, 0, 1) 
    pred[j+1] <- change_or_not
  
  }
  return(pred)
}

#source('character_change_function.R')



