library(ggplot2)
library(dplyr)
library(magrittr)
INFILE <- '../data/ratings.tsv'

ratings <- read.csv(INFILE, sep = '\t', stringsAsFactors = FALSE) %>%
  dplyr::rename_all(tolower)

ratings %<>%
  dplyr::filter(abs(rating) >= 1) %>%
  dplyr::mutate(rank_type = ifelse(rating < 0, 'k', 'd'),
                rank = floor(abs(rating)),
                full_rank = paste0(rank, rank_type))
ord <- ratings %>% distinct(rank, rank_type, full_rank) %>%
  dplyr::arrange(ifelse(rank_type == 'k', -rank, rank)) %$%
  full_rank

ranks_counts <- ratings %>%
  dplyr::group_by(rank, rank_type, full_rank) %>%
  dplyr::summarize(count = n()) %>%
  dplyr::mutate(full_rank = factor(full_rank, levels = ord))

ranks_counts %>% ggplot(aes(x = full_rank, y = count)) +
  geom_bar(stat = "identity") +
  theme_bw()

histogram <- ratings %>%
  ggplot(aes(x = rating)) +
  geom_histogram() +
  theme_bw()