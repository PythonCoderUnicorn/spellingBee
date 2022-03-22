

# NYT Spelling Bee / Regular Expressions
# source:  https://www.youtube.com/watch?v=rhmZ14r8vVw

library(tidyverse)


dictionary_raw = read_delim("https://raw.githubusercontent.com/dwyl/english-words/master/words.txt", delim = "|", col_names = "word")

dictionary_raw

# clean up the words and uppercase 
dictionary_clean = dictionary_raw %>% 
  mutate(word = str_to_upper(word)) %>% 
  filter(word %>% str_detect("^[A-Z]+$"))

dictionary_clean

# get only specific letters for word search: YAELHIV
dictionary_clean %>% 
  filter(word %>% str_detect("^[MNICAG]+$"),
         # word length is 4 or more
         str_length(word) >= 4) %>% 
  # find words with V in it
  filter(word %>%  str_detect("P")) %>% 
  mutate(word_length = str_length(word)) %>% 
  arrange( desc(str_length(word_length)))



#  make this a function 

spellingBee = function(dictionary= dictionary, possible_letters, req_letter){
  letters_ = str_c("^[",possible_letters,"]+$")
  dictionary_clean %>% 
    filter(word %>% str_detect(letters_ ),
           # word length is 4 or more
           str_length(word) >= 4) %>% 
    # find words with V in it
    filter(word %>%  str_detect( req_letter )) %>% 
    mutate(word_length = str_length(word)) %>% 
    arrange( desc(str_length(word_length)))
}

dictionary_clean %>% 
  spellingBee(possible_letters = "GANIMCP", req_letter = "P") %>% view()















