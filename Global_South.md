Mapping the Politics of the New Global South
================
Neeraj Sharma
7/31/2019

# Introduction

I am working for Professor Mark Bradley in the History Department at the
University of Chicago this summer on his research project, Mapping the
Politics of the New Global South.

# Setup

Prior to performing analysis on the data collected, it is necessary to
prep the work environment so it contains all the packages necessary for
exploration.

## Load Packages

``` r
# Relevant to data importation, structuring and visualization
library(tidyverse)
library(knitr)
library(readr)
library(here)

# Relevant to data formatting
library(lubridate)
library(countrycode)

# Relevant to text analysis
library(tidytext)
library(stringr)
library(SnowballC)
```

## Import datasets

These datasets were produced through the corpus\_maker.R script. The
original source files are from
Mikhaylov.

``` r
unigrams_corpus_1970on <- read_tsv(here::here("Data", "unigrams_mikhaylov_project.tsv")) %>%
  select(Session, Year, Country, word_stem)

bigrams_corpus_1970on <- read_tsv(here::here("Data", "bigrams_mikhaylov_project.tsv")) %>%
  select(Session, Year, Country, word_stem)
```

## Glimpse at the content of the datasets

Unigrams

``` r
kable(unigrams_corpus_1970on %>% slice(1:10))
```

| Session | Year | Country | word\_stem |
| ------: | ---: | :------ | :--------- |
|      25 | 1970 | Albania | convei     |
|      25 | 1970 | Albania | presid     |
|      25 | 1970 | Albania | congratul  |
|      25 | 1970 | Albania | albanian   |
|      25 | 1970 | Albania | deleg      |
|      25 | 1970 | Albania | elect      |
|      25 | 1970 | Albania | presid     |
|      25 | 1970 | Albania | twenti     |
|      25 | 1970 | Albania | session    |
|      25 | 1970 | Albania | assembli   |

Bigrams

``` r
kable(bigrams_corpus_1970on %>% filter(Session == 25) %>% slice(1:10))
```

| Session | Year | Country | word\_stem         |
| ------: | ---: | :------ | :----------------- |
|      25 | 1970 | Albania | albanian deleg     |
|      25 | 1970 | Albania | unit nation        |
|      25 | 1970 | Albania | peac love          |
|      25 | 1970 | Albania | satisfactori activ |
|      25 | 1970 | Albania | unit nation        |
|      25 | 1970 | Albania | albanian deleg     |
|      25 | 1970 | Albania | balanc sheet       |
|      25 | 1970 | Albania | activ cover        |
|      25 | 1970 | Albania | short period       |
|      25 | 1970 | Albania | intern organ       |

# Analysis
