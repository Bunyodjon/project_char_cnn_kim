library(keras)
library(dplyr)
# ---------------------------------------------------------------------------------#
# Date updated 3/11/2019              
# Created: 03/02/2019 

# load the trained model 
model <- load_model_hdf5("my_model_2.h5")

#train_data_source <- read.csv("data/ag_news_csv/train.csv", header=FALSE)
# load the test data 
test_data_source <- read.csv("test.csv", header=FALSE)

test_data_source$V2 <- as.character(test_data_source$V2)
test_data_source$V3 <- as.character(test_data_source$V3)

test_data_source$V4 <- paste(test_data_source$V2, test_data_source$V3, sep = " ")
test_data_source$V2 <- NULL
test_data_source$V3 <- NULL

test_text <- test_data_source$V4
test_label <- test_data_source$V1

# it is not necessary to change labels
# test_label2 <- factor(test_label, labels = c(0, 1, 2, 3))

# create tokenizer for test data 
tokenizer <- text_tokenizer(char_level = TRUE, oov_token = 'UNK') %>%
  fit_text_tokenizer(test_text)

# create word index and dictionary
word_index <- tokenizer$word_index

# we can use this dictionary if we want to convert the integers back to words
word_index_df <- data.frame(
  word = names(word_index),
  idx = unlist(word_index, use.names = FALSE),
  stringsAsFactors = FALSE
)

# create sequences  
sequences <- texts_to_sequences(tokenizer, test_text)

# pad the test data maxlen is 1014 
test_data <- pad_sequences(sequences, maxlen = 1014, padding = 'post')

# let's predict only 100 observations going forward 
pred <- model %>% predict(test_data[1:100,])
pred <- as.data.frame(pred)
# determine the class
pred$initial_prediction <- apply(pred[ ,1:4], 1, which.max)

# we can quickly check the accuracy
table(test_label[1:100], pred$class)


# this is the list of integers that refers to alphabet letters in our dictionary word_index_df
list_of_alphabet <- c(3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 
                      19, 20, 21, 22, 24, 25, 31, 33, 34, 41)

# create smaller list with only 100 samples 
sequences_small <- sequences[1:100]

# number of characters we want to replace inside a text
number_character <- 10

# randomly select integers and replace them with one of the integers from the list of alphabete
for (i in 1:length(sequences_small)) {
    k <- sample(1:lengths(sequences_small[i]), min(number_character, lengths(sequences_small[i])) , replace = FALSE)
    sequences_small[[i]][k] <- sample(list_of_alphabet, number_character, replace = TRUE)
}  

# pad the test data maxlen is 1014 
test_small <- pad_sequences(sequences_small, maxlen = 1014, padding = 'post')

# initialize data frame 
pred <- pred %>% 
  select(initial_prediction)

# I am doing 10 iterations for now and it seems like that classes don't 
# change from one iteration to another iteration.  

iterations <- 1:10

for (j in iterations) {
  pred_small <- model %>% predict(test_small)
  class <- apply(pred_small[ ,1:4], 1, which.max)
  change_or_not <- ifelse(pred$initial_prediction==class, 0, 1) 
  pred[j+1] <- change_or_not
}

pred <- pred %>% 
  mutate(mean_change=rowMeans(pred[, 2:ncol(pred)])) %>% 
  select(initial_prediction, mean_change)

############ Function #####################################################

# I created a function that does above steps in one line:
# in order to run the function you should have following things in your environment
# 1) sequences_small 
# 2) list_of_alphabet 
# 3) pred ( data frame with initial prediction)

# let's predict only 100 observations going forward 
pred <- model %>% predict(test_data[1:100,])
pred <- as.data.frame(pred) 

# determine the class
pred$initial_prediction <- apply(pred[ ,1:4], 1, which.max)

# initialize data frame 
pred <- pred %>% 
  select(initial_prediction)


# this is the list of integers that refers to alphabet letters in our dictionary word_index_df
list_of_alphabet <- c(3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 
                      19, 20, 21, 22, 24, 25, 31, 33, 34, 41)

# subset sequences from above
sequences_small <- sequences[1:100]

# load the function
source('character_change_function.R')

# change characters from 1 to 10, and see how classification changes
pred_final <- change_random_character(x=10)

pred_final$mean_change <- rowMeans(pred_final[, 2:ncol(pred_final)])




# --------------------------------------------------------------------------------# 
# construct a new vocabulary of needed  
alphabet <- "abcdefghijklmnopqrstuvwxyz0123456789 ,;.!?:'\"/\\|_@#$%^&*~`+-=<>()[]{}"
character_split <- strsplit(alphabet, "")[[1]]
char_dict <- data.frame(
  character=character_split,
  index=1:length(character_split)
)
pad <- data.frame(
  character=c('UNK'),
  index=c(70))

char_dict <- rbind(char_dict, pad)
# ---------------------------------------------------------------------------------#



