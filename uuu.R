

require(tidyverse)
# =================================================================== #

df <- read_csv('data/train.csv.zip')
dim(df) # 2013 x 4

df %>% count(outcome) # 0: 1910; 1(bots): 103
# The outcome variable is HIGHLY skewed: more than 94.88% are humans.


bids <- read_csv('data/bids.csv.zip')
dim(bids) # 7,656,334 x 9

# Get the relevant bids' info for bidders in the current set of bidders:
bids2 <- left_join(df, bids)
dim(bids2) # 3,071,253 x 12
# Save it for later usage:
write_csv(bids2, 'assets/bids_trn.csv') # Set the names properly, though.

# Read the bids' info for the current bidders:
bids <- read_csv('assets/bids_trn.csv') # bids_trn or. bids_tst
dim(bids) # 3,071,253 x 12

# ======================================================================= #

# bids$time: --------------------------------------------------------------
#inter_bid_t <- function(df){
#    t <- df$time
#    if (length(t) < 2){
#        return(NA_real_)
#    }
#    diff(sort(t))
#}

inter_bid_t <- function(df){
    t <- df$time
    if (length(t) < 2){
        return(0)
    }
    diff(sort(t))
}


inter_bid_t_summary <- function(df){
    df %>% 
        mutate(
            t_length = map_dbl(data, 
                ~ max(.$time, na.rm = TRUE) - min(.$time, na.rm = TRUE)),
            inter_bid_t = map(data, inter_bid_t),
            inter_bid_t_mean = map_dbl(inter_bid_t, mean, na.rm = TRUE),
            inter_bid_t_median = map_dbl(inter_bid_t, median, na.rm = TRUE),
            inter_bid_t_SD = map_dbl(inter_bid_t, sd, na.rm = TRUE),
            inter_bid_t_IQR = map_dbl(inter_bid_t, IQR, na.rm = TRUE)
        )
}

# 1. by bidder: ------------------------
by_bidder <- bids %>% 
    group_by(bidder_id) %>% 
    nest() # 2,013 x 2

by_bidder <- inter_bid_t_summary(by_bidder) # 2,013 x 8
#<- The warnings are because: max(NA, na.rm = TRUE); same true for min()
#Hence, if a bidder's bidding time is all NA, we get infinite values:

# In the 't_length' column, replace the infinite values created with NA:
tmp <- by_bidder[['t_length']]
by_bidder[['t_length']] <- ifelse(is.infinite(tmp), NA_real_, tmp)

# 2. by bidder and auction: ------------
by_bidder_auction <- bids %>% 
    group_by(bidder_id, auction) %>% 
    nest()

by_bidder_auction <- inter_bid_t_summary(by_bidder_auction) # 124,199      9

# In the 't_length' column, replace the infinite values created with NA:
tmp <- by_bidder_auction[['t_length']]
by_bidder_auction[['t_length']] <- ifelse(is.infinite(tmp), NA_real_, tmp)

# notice: mean(c(NA, NA), na.rm = TRUE) -> NaN
#Many bidders bid once in a particular auction.

by_bidder_auction_aggregated <- by_bidder_auction %>% 
    select(-c(auction, data, inter_bid_t)) %>% 
    group_by(bidder_id) %>% 
    summarise_all(mean, na.rm = TRUE) # 2,013 x 6

names(by_bidder_auction_aggregated) <- c('bidder_id',
        paste0('avg_', names(by_bidder_auction_aggregated)[2:6]))

# The following two helper data sets will be used to aid aggregation:
# How long each bidder was present during the life of an auction:
#I take the ratio: if max - min for bidder was 9 (units of time) in an auction,
#and the auction itself lasted 10 (units of time), then I want to have 9/10.
by_bidder_auction__t_length <- by_bidder_auction %>% 
    select(bidder_id, auction, t_length)

# These are the mean, median, ...; 
#I seperated these from 't_length', because I want to calculate 
#the deviance from the mean of corresponding variable for a specific function.
by_bidder_auction__t_rest <- by_bidder_auction %>% 
    select(-c(data, t_length, inter_bid_t))

# 3. by auction: -----------------------

by_auction <- bids %>% 
    arrange(auction, time) %>% 
    group_by(auction) %>% 
    nest()

by_auction <- inter_bid_t_summary(by_auction)
# In the 't_length' column, replace the infinite values created with NA:
tmp <- by_auction[['t_length']]
by_auction[['t_length']] <- ifelse(is.infinite(tmp), NA_real_, tmp)


by_auction__t_length <- by_auction %>% 
    select(auction, t_length)

by_auction__t_rest <- by_auction %>% 
    select(-c(data, t_length, inter_bid_t))

# Take the ratio for t_length
ba_t_length <- by_bidder_auction__t_length %>% 
    left_join(by_auction__t_length,
              by = 'auction',
              suffix = c('_b', '_a')) %>% 
    mutate(t_b_to_a = t_length_b / t_length_a) %>% 
    select(bidder_id, t_b_to_a) %>% 
    group_by(bidder_id) %>% 
    summarise(avg__t_b_to_a = mean(t_b_to_a, na.rm = TRUE))

# Take the deviance from the mean in each auction:
ba_t_rest <- by_bidder_auction__t_rest %>% 
    left_join(by_auction__t_rest,
              by = 'auction',
              suffix = c('_b', '_a')) %>% 
    transmute(
        bidder_id,
        inter_bid_t_mean_diff = inter_bid_t_mean_a - inter_bid_t_mean_b,
        inter_bid_t_median_diff = inter_bid_t_median_a - inter_bid_t_median_b,
        inter_bid_t_SD_diff = inter_bid_t_SD_a - inter_bid_t_SD_b,
        inter_bid_t_IQR_diff = inter_bid_t_IQR_a - inter_bid_t_IQR_b
    ) %>% 
    group_by(bidder_id) %>% 
    summarise_all(mean)

names(ba_t_rest) <- c('bidder_id', paste0('avg_', names(ba_t_rest)[2:5]))

# 4. Join the variables: ---------------
uuu <- by_bidder %>% 
    select(-c(data, inter_bid_t)) %>% 
    left_join(by_bidder_auction_aggregated) %>% 
    left_join(ba_t_length) %>% 
    left_join(ba_t_rest) # 2,013 x 16


# Save for later usage:
write_csv(uuu, 'assets/uuu.csv')

