# 03 find phonotactic probability for a list of word ---------------------------
#
# Last update: 2021-09-02
# 
# -----------------------------------------------------------------------------

# Source libs -----------------------------------------------------------------

source(here::here("scripts", "00_libs.R"))
source(here::here("scripts", "01_helpers.R"))
# -----------------------------------------------------------------------------
# read in data 

bigram_data_sp <- read.csv(here("data", "tidy", "bigram_df_span.csv"))
bigram_data_en <- read.csv(here("data", "tidy", "bigram_df_eng.csv"))
bigram_data_ge <- read.csv(here("data", "tidy", "bigram_df_german.csv"))

span <- read.csv(here("data", "tidy", "corpus_span_tidy.csv")) 
eng <- read.csv(here("data", "tidy", "corpus_eng_tidy.csv")) 
german <- read.csv(here("data", "tidy", "corpus_german_tidy.csv")) 


bigram_df_s <- left_join(bigram_data_sp, span, by = "word")
bigram_df_e <- left_join(bigram_data_en, eng, by = "word")
bigram_df_g <- left_join(bigram_data_ge, german, by = "word")


# create/read word list 
#stim_list <- c("pampe", "poche", "pomme", "patte", "pata", "that") 
stim_list <- unique(eng$word[1:100])


# loop word_list through spanish corpus
result_df_s <- matrix(nrow = length(stim_list), ncol = 2)
for(iteration in 1:length(stim_list)){
  result_df_s[iteration, 1] <- find_bigram_prob(word = stim_list[iteration], bigram_df = bigram_df_s, corpus = span)
  result_df_s[iteration, 2] <- stim_list[iteration]
}

# loop word_list through english corpus
result_df_e <- matrix(nrow = length(stim_list), ncol = 2)
for(iteration in 1:length(stim_list)){
  result_df_e[iteration, 1] <- find_bigram_prob(word = stim_list[iteration], bigram_df = bigram_df_e, corpus = eng)
  result_df_e[iteration, 2] <- stim_list[iteration]
}


# combine result and clean it up 

span_clean <- result_df_s %>%
  as.data.frame() %>% 
  mutate(source = "spanish") %>% 
  rename("prob" = V1) %>%
  rename("word" = V2)

eng_clean <- result_df_e %>%
  as.data.frame() %>% 
  mutate(source = "english") %>% 
  rename("prob" = V1) %>%
  rename("word" = V2)

stim_df <- rbind(span_clean, eng_clean) %>% 
  pivot_wider(names_from = source, values_from = prob)

stim_df$spanish = as.numeric(stim_df$spanish)
stim_df$english = as.numeric(stim_df$english)

stim_df <- stim_df %>% 
  mutate(log_spanish = log(spanish), log_english = log(english))


view(stim_df)


find_bigram_prob("hola", bigram_df_s, span)
find_bigram_prob("hablar", bigram_df_s, span)
