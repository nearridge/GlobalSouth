# This script imports the raw .txt files and exports an unaltered table of just full speeches and a tidytext-a-fied table as well. 
# This means that I don't lose any information going from the .txt files to the .csv files which is nice. 

library(tidyverse)
library(readr)
library(here)
library(readtext)
library(countrycode)
library(tidytext)
library(stringr)
library(SnowballC)


imported_files <- read_tsv(here("Data", "raw_speeches_mikhaylov_project.tsv"))

data(stop_words)

corpus <- imported_files %>%
  mutate(Country = countrycode(Country_ABB, origin = "iso3c", destination = "country.name")) %>%
  # Unknown countries that I have to handmatch are CSK, DDR, EU, YDYE, YUG. These countries aren't in the ISO3c vocab because they no longer exist 
  mutate(Country = ifelse(is.na(Country), ifelse(`Country_ABB` == "CSK", "Czechoslovakia", NA), Country)) %>%
  mutate(Country = ifelse(is.na(Country), ifelse(`Country_ABB` == "DDR", "East Germany", NA), Country)) %>%
  mutate(Country = ifelse(is.na(Country), ifelse(`Country_ABB` == "EU", "EU?", NA), Country)) %>%
  mutate(Country = ifelse(is.na(Country), ifelse(`Country_ABB` == "YDYE", "South Yemen/People's Democratic Republic of Yemen", NA), Country)) %>%
  mutate(Country = ifelse(is.na(Country), ifelse(`Country_ABB` == "YUG", "Yugoslavia", NA), Country))

unigrams <- corpus %>%
  # Expand text of speeches into individual words
  unnest_tokens(output = word, input = text) %>%
  # Removes numbers
  filter(!str_detect(word, "^[0-9]*$")) %>%
  # Removes stop words
  anti_join(stop_words) %>%
  # Stemms words
  mutate(word_stem = wordStem(word))

write_tsv(unigrams, "Data/unigrams_mikhaylov_project.tsv", na = "NA", col_names = TRUE)

# I want to look at bigrams as well because my research centers arounds words like "Third World" and "Global South." Those must be stemmed. 
bigrams <- corpus %>%
  # Expand text of speeches into individual words
  unnest_tokens(output = word, input = text, token = "ngrams", n = 2) %>%
  # Removes stop words
  separate(word, into = c("first_word","second_word"), sep = " ", remove = FALSE) %>%
  anti_join(stop_words, by = c("first_word" = "word")) %>%
  anti_join(stop_words, by = c("second_word" = "word")) %>%
  # Removes numbers
  filter(!str_detect(first_word, "^[0-9]*$") & !str_detect(second_word, "^[0-9]*$")) %>%
  # Stemms words
  mutate(first_word_stem = wordStem(first_word), second_word_stem = wordStem(second_word), word_stem = paste(first_word_stem, second_word_stem, sep = " "))

write_tsv(bigrams, "Data/bigrams_mikhaylov_project.tsv", na = "NA", col_names = TRUE)
