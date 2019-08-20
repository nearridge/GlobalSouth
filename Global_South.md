Mapping the Politics of the New Global South, Progress Update
================
Neeraj Sharma
7/31/2019

``` r
UN_stop_words <- tibble(words = c("nation", 
                                  "unit", 
                                  "intern", 
                                  "countri", 
                                  "develop", 
                                  "peac",
                                  "world",
                                  "peopl",
                                  "deleg",
                                  # Stops countries from counting their own names as very commonly repeated words. Stripping removes casing. This takes codelist from countrycode. It might be bad because it eliminates when speeches mention other countries. 
                                  strip(codelist$country.name.en)
                                  ))
```

# Common Words by Decade

![](Global_South_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->![](Global_South_files/figure-gfm/unnamed-chunk-6-2.png)<!-- -->![](Global_South_files/figure-gfm/unnamed-chunk-6-3.png)<!-- -->

# Looking at specific countries common words over time

I was encouraged by Professor Bradley to investigate keyword trends over
time in the following countries:

  - Indonesia
  - Algeria
  - Kenya
  - Mexico
  - Egypt

What I’ve done is to group the speeches of each country into 5 year
increments and then took the top 10 words identified in each chunk. This
allows us to see what keywords rise and fall over time. Here is what
emerged based on this analysis.

## Indonesia

![](Global_South_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

Interesting observations:

  - The word “Partnership” arises in the range of 2006-2010. This is
    interesting because it’s right at the start of the rise of the usage
    of the term “Global South” per my understanding, which is importent
    as it indicates a shift towards a cooperative outlook towards
    foreign policy potentially.
  - ASEAN becoming a continually present term starting in 2000.
  - Words like “commit,” “goal,” etc. appear more in the later years.
    Generally, the tone is more outward looking and cooperative.

## Algeria

![](Global_South_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

Interesting observations:

  - Terrorism is a key phrase mentioned a lot during the later period of
    the civil war.
  - Crisis is said a lot in 1980s. Not exactly sure why that’s the case.
    I don’t know much about Algeria but my quick internet investigation
    couldn’t reveal anything obvious.
  - Any changes from 1970 to 2018 not as apparent as other countries.
    It’s not the most revealing country to see changes in the
    discourse surrounding development.

## Kenya

![](Global_South_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

Interesting observations:

  - Cooperate and global appear as keywords in 1990-1995.
  - Language of cooperation definately has changed in the early period
    compared to the late period.
  - Relations with Somalia seem to be very important in the late 2000s.
    They seem to have started campaigns against Al-Shabab around that
    time.
  - This trend appears to continue through the rest of the 2000s with
    mentions of Sudan, Somalia again, Support, and Sustain.

## Mexico

![](Global_South_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

Interesting observations:

  - Human rights are a big deal in 200s onward period recently.
    Especially in 2015, they are at the forefront of topics discussed.
  - Drug crime and climate change are themes in 2000s
  - Nuclear weapons and international security appear to be concerns for
    Mexico? The terms appear frequently in multiple different decades
    which indicates a continuing interest in this topic. It obviously
    makes sense given that nuclear technology is always a concern for
    nations, but it seems interesting for a country that has renounced
    the ability to make weapons (Treaty of Tlatelolco, 1968).

## Egypt

![](Global_South_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

Interesting observations:

  - Egypt talks about Palestine a lot, especially in the early periods
    of this analysis. That is not surprising AT ALL.

  - 
# Sentiment Analysis

This is for a later time and wasn’t the focus of this project update.
I’m keeping this here just so I have it in my backpocket for when it
does become important later on in the
project.

``` r
# corpus_affin df only contains words that have affinities mapped to them
unigrams_corpus_affin <- unigrams_corpus_1970on %>% 
  inner_join(get_sentiments("afinn"), by = c("word_stem" = "word"))

bigrams_corpus_affin <- bigrams_corpus_1970on %>% 
  inner_join(get_sentiments("afinn"), by = c("first_word_stem" = "word")) %>%
  inner_join(get_sentiments("afinn"), by = c("second_word_stem" = "word")) %>%
  mutate(mean_sentiment = (value.x+value.y)/2)
```
