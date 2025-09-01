## ----message=FALSE, warning=FALSE, echo = FALSE-------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 6, fig.height = 4.5
)
library(fec16)
library(dplyr)
library(ggplot2)
library(scales)
library(stringr)

## ----eval=FALSE, message=FALSE, warning=FALSE, include=TRUE-------------------
# # The entire expenditures dataset can be accessed by:
# all_expenditures <- fec16::read_all_expenditures()
# 
# # The first 30 entries in this dataset can be accessed by:
# expenditures_30 <- fec16::read_all_expenditures(n_max = 30)

## -----------------------------------------------------------------------------
head(results_house)

## ----message=FALSE, warning=FALSE---------------------------------------------
wins <- left_join(results_president, candidates) %>%
  group_by(cand_id, cand_name) %>%
  summarise(
    total_votes = sum(general_votes, na.rm = TRUE),
    states_won = sum(won)
  ) %>%
  arrange(desc(total_votes))
head(wins)

## -----------------------------------------------------------------------------
ggplot(
  wins %>% head(6),
  aes(x = reorder(cand_name, total_votes), y = total_votes, fill = cand_id)
) +
  geom_col() +
  scale_fill_discrete(guide = FALSE) +
  scale_y_continuous(labels = comma) +
  labs(
    title = "2016 Presidential Election",
    subtitle = "Total votes",
    x = NULL, y = "Number of Votes"
  ) +
  coord_flip()

## ----eval=FALSE, message=FALSE, warning=FALSE---------------------------------
# all_contributions <- fec16::read_all_contributions()
# results_by_cand <- left_join(results_house, candidates, by = "cand_id") %>%
#   left_join(all_contributions, by = "cand_id") %>%
#   group_by(cand_id, cand_name, cand_pty_affiliation) %>%
#   summarise(
#     sum_votes = sum(general_votes),
#     contribution = sum(transaction_amt)
#   ) %>%
#   filter(sum_votes > 1000)
# head(results_by_cand)

## ----eval=TRUE, echo=FALSE, message=FALSE, warning=FALSE----------------------
# save local .Rdata file to save time on compilation
# save(results_by_cand, file = "results_by_cand.Rda")
load(file = "results_by_cand.Rda")
head(results_by_cand)

## ----eval=TRUE, message=FALSE, warning=FALSE----------------------------------
ggplot(results_by_cand, aes(x = contribution, y = sum_votes)) +
  geom_point() +
  scale_x_log10(labels = comma) +
  scale_y_sqrt(labels = comma) +
  geom_smooth(method = "auto") +
  labs(
    title = "Contributions vs. Votes in 2016",
    x = "Contributions in US Dollars", y = "Total Votes"
  )

## ----message=FALSE, warning=FALSE---------------------------------------------
house_winners <- left_join(results_house, candidates, by = "cand_id") %>%
  mutate(party_1 = str_sub(party, 1, 1)) %>%
  filter(won, state %in% state.abb) %>%
  group_by(cand_id, cand_name, party_1) %>%
  summarize(
    total_votes = sum(general_votes),
    total_pct = sum(general_percent),
  )

ggplot(
  house_winners,
  aes(
    x = total_votes, y = total_pct,
    color = party_1
  )
) +
  geom_point() +
  labs(
    title = "Not all Congressional Races are the same",
    fill = "Candidate", x = "Total Votes", y = "Total Percent"
  ) +
  scale_y_continuous(labels = comma)

