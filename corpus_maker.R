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
library(textclean)
library(cleanNLP)
reticulate::use_python("/anaconda3/bin/python.app")
cnlp_init_spacy()

# This portion of the script takes in .txt files of speeches and appends them all to one another to create a massive, comprehensive dataframe.
# imported_files <- readtext(paste0(here("data", "Raw Speeches")), 
#                            docvarsfrom = "filenames", dvsep = "_", 
#                            docvarnames = c("Country_ABB", "Session", "Year")) %>%
#   as_tibble()

imported_files <- read_tsv(here("Data", "raw_speeches_mikhaylov_project.tsv"))

data(stop_words)

annotated_imported_files <- cnlp_annotate(imported_files %>% slice(1:10), as_strings = TRUE, doc_var = "text", text_var = "doc_id")

corpus <- imported_files %>%
  mutate(Country = countrycode(Country_ABB, origin = "iso3c", destination = "country.name")) %>%
  # Unknown countries that I have to handmatch are CSK, DDR, EU, YDYE, YUG. These countries aren't in the ISO3c vocab because they no longer exist 
  mutate(Country = ifelse(is.na(Country), ifelse(`Country_ABB` == "CSK", "Czechoslovakia", NA), Country)) %>%
  mutate(Country = ifelse(is.na(Country), ifelse(`Country_ABB` == "DDR", "East Germany", NA), Country)) %>%
  mutate(Country = ifelse(is.na(Country), ifelse(`Country_ABB` == "EU", "EU", NA), Country)) %>%
  mutate(Country = ifelse(is.na(Country), ifelse(`Country_ABB` == "YDYE", "South Yemen/People's Democratic Republic of Yemen", NA), Country)) %>%
  mutate(Country = ifelse(is.na(Country), ifelse(`Country_ABB` == "YUG", "Yugoslavia", NA), Country))

sentences <- corpus %>%
  # Expand text of speeches into individual words
  unnest_tokens(output = word, input = text) %>%
  # Removes numbers
  filter(!str_detect(word, "^[0-9]*$")) %>%
  # Removes stop words
  anti_join(stop_words) %>%
  # removes apostrophies
  mutate(word = strip(word, apostrophe.remove = TRUE)) %>%
  # Stemms words
  mutate(word_stem = wordStem(word))

unigrams <- corpus %>%
  # Expand text of speeches into individual words
  unnest_tokens(output = word, input = text) %>%
  # Removes numbers
  filter(!str_detect(word, "^[0-9]*$")) %>%
  # Removes stop words
  anti_join(stop_words) %>%
  # removes apostrophies
  mutate(word = strip(word, apostrophe.remove = TRUE)) %>%
  # Stemms words
  mutate(word_stem = wordStem(word))

write_tsv(unigrams, "Data/unigrams_mikhaylov_project.tsv", na = "NA", col_names = TRUE)
