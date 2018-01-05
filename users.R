
require(caret)
require(tidyverse)
# =================================================================== #
# prepare the data for modeling:

bidders <- bids %>% 
    group_by(bidder_id, outcome) %>% 
    summarise(
        n_bids = n_distinct(bid_id),
        n_device = n_distinct(device),
        n_auction = n_distinct(auction),
        n_merchandise = n_distinct(merchandise),
        n_country = n_distinct(country),
        n_ip = n_distinct(ip)
    ) %>% ungroup() # 2,013 x 9

uuu <- read_csv('assets/uuu.csv')
rrr <- read_csv('assets/rrr.csv')

# the following two have majoirty NA; de-select them:
uuu2 <- uuu %>% 
    select(-c(avg_inter_bid_t_SD, avg_inter_bid_t_SD_diff))

bidders2 <- bidders %>%
    select(bidder_id, outcome, n_auction, n_bids) %>% 
    left_join(uuu2) %>% 
    left_join(rrr) # 2013 x 42

bidders2 %>% 
    keep(is.numeric) %>% 
    map_dbl(~ mean(is.na(.)))

bidders2 <- bidders2 %>% 
    map_df(~ ifelse(is.na(.), -1, .))
sum(is.na(bidders2)) # 0

bidders2$outcome <- ifelse(bidders2$outcome == 1, 'yes', 'no')
bidders2$outcome <- as.factor(bidders2$outcome)

# prepare the data for modeling: -----------
X <- bidders2 %>% select(- c(outcome, bidder_id))

nzvs <- nearZeroVar(X)
names(X)[nzvs]
X <- X[-nzvs] # remove the near-zero variance variables 

cor_vals <- cor(X)
high_cor <- findCorrelation(cor_vals, cutoff = .75)
length(high_cor) # 19
X <- X[-high_cor]

bidders3 <- X
bidders3[['outcome']] <- bidders2$outcome
dim(bidders3) # 2013 x 17

