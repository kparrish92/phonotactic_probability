# Bigram loop script -----------------------------------------------------------
#
# Last update: 2021-09-02
# For each corpus, use the bigram function to break individual words into bigrams
# and save the output
# -----------------------------------------------------------------------------

# Source libs -----------------------------------------------------------------

source(here::here("scripts", "00_libs.R"))
source(here::here("scripts", "01_helpers.R"))

# -----------------------------------------------------------------------------

# load in data, clean column names and filter words 2 chars or less
eng <- read.csv(here("data", "corpora", "subtlex_eng.csv")) %>% 
  janitor::clean_names() %>% 
  filter(nchar(word) > 2)
span <- read.csv(here("data", "corpora", "subtlex_span.csv")) %>% 
  janitor::clean_names() %>% 
  filter(nchar(word) > 2)
german <- read.csv(here("data", "corpora", "subtlex_germ.csv")) %>% 
  janitor::clean_names() %>% 
  filter(nchar(word) > 2)

# make all words lowercase and remove and punctuation for each corpus
eng$word <- gsub('[[:punct:] ]+',' ',eng$word)
eng$word <- tolower(eng$word)
eng$word <- trimws(eng$word)

span$word <- gsub('[[:punct:] ]+',' ',span$word)
span$word <- tolower(span$word)
span$word <- trimws(span$word)

german$word <- gsub('[[:punct:] ]+',' ',german$word)
german$word <- tolower(german$word)
german$word <- trimws(german$word)

corpus_eng <- eng %>% 
  dplyr::select(word, fre_qcount) %>% 
  rename(token_freq = fre_qcount) %>% 
  mutate(log_freq = log(token_freq)) %>% 
  mutate(length = nchar(word)) 
  
corpus_eng %>% 
  write.csv(here("data", "tidy", "corpus_eng_tidy.csv"))

corpus_span <- span %>% 
  dplyr::select(word, freq_count) %>% 
  rename(token_freq = freq_count) %>% 
  mutate(log_freq = log(token_freq)) %>% 
  mutate(length = nchar(word))

corpus_span %>%
  write.csv(here("data", "tidy", "corpus_span_tidy.csv"))

corpus_german <- german %>% 
  dplyr::select(word, w_ffreqcount) %>% 
  rename(token_freq = w_ffreqcount) %>% 
  mutate(log_freq = log(token_freq)) %>% 
  mutate(length = nchar(word)) 

corpus_german %>% 
  write.csv(here("data", "tidy", "corpus_german_tidy.csv"))


## loop bigram function in all corpora
## English
#### this loop might get stuck - I hit stop and it still created an output 
new_df_eng <- character()
for(thisRun in 1:nrow(corpus_eng))
{
  words <- bigram(corpus_eng$word[thisRun])
  new_df_eng <- rbind(new_df_eng, words) %>% 
    as.data.frame()
}

new_df_eng %>% 
  write.csv(here("data", "tidy", "bigram_df_eng.csv"))

## Spanish 

new_df_span <- character()
for(thisRun in 1:nrow(corpus_span))
{
  words <- bigram(corpus_span$word[thisRun])
  new_df_span <- rbind(new_df_span, words) %>% 
    as.data.frame()
}

new_df_span %>% 
  write.csv(here("data", "tidy", "bigram_df_span.csv"))

## German 

new_df_german <- character()
for(thisRun in 1:nrow(corpus_german))
{
  words <- bigram(corpus_german$word[thisRun])
  new_df_german <- rbind(new_df_german, words) %>% 
    as.data.frame()
}

new_df_german %>% 
  write.csv(here("data", "tidy", "bigram_df_german.csv"))



