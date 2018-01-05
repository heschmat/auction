


df2 <- bids %>% 
    select(- c(payment_account, address, time)) %>% 
    select(bid_id, bidder_id, everything())


rest_summary <- function(dfg){
    dfg %>% 
        mutate(
            n_country = map_int(data, ~ n_distinct(.$country)),
            n_device = map_int(data, ~ n_distinct(.$device)),
            n_url = map_int(data, ~ n_distinct(.$url)),
            n_ip = map_int(data, ~ n_distinct(.$ip)),
            n_merchandise = map_int(data, ~ n_distinct(.$merchandise))
        )
}


# by auction: ---- 
by_auction2 <- df2 %>% 
    group_by(auction) %>% 
    nest() # 12741     2


by_auction2 <- rest_summary(by_auction2) # 12,741 x 7   

# by bidder: ---- 
by_bidder2 <- df2 %>% 
    group_by(bidder_id) %>% 
    nest()

by_bidder2 <- rest_summary(by_bidder2) # 2,013 x 7

# by bidder and auction: ---- 
by_ba2 <- df2 %>% 
    group_by(bidder_id, auction) %>% 
    nest()

by_ba2 <- rest_summary(by_ba2)

by_ba2_mean <- by_ba2 %>% 
    group_by(bidder_id) %>% 
    summarise_if(is.numeric, mean)
names(by_ba2_mean) <- c('bidder_id', paste0(names(by_ba2_mean)[2:6], '_mean'))

by_ba2_med <- by_ba2 %>% 
    group_by(bidder_id) %>% 
    summarise_if(is.numeric, median)
names(by_ba2_med) <- c('bidder_id', paste0(names(by_ba2_med)[2:6], '_median'))

# This is the average count for a user over all the auctions she placed a bid.
by_ba2_agg <- left_join(by_ba2_mean, by_ba2_med)
# Remove the temporary data:
rm(by_ba2_mean, by_ba2_med)


#
by_ba2 <- by_ba2 %>% 
    left_join(by_auction2 %>% select(-data),
              by = 'auction',
              suffix = c('_bidder', '_auction')) # 124,199     13


by_ba2_diff <- by_ba2 %>% 
    transmute(
        bidder_id,
        auction,
        n_country_diff = n_country_bidder - n_country_auction,
        n_device_diff = n_device_bidder - n_device_auction,
        n_url_diff = n_url_bidder - n_url_auction,
        n_ip_diff = n_ip_bidder - n_ip_auction,
        n_merchandise_diff = n_merchandise_bidder - n_merchandise_auction
    ) # 124,199     7


by_ba2_diff_mean <- by_ba2_diff %>% 
    group_by(bidder_id) %>% 
    summarise_if(is.numeric, mean)
names(by_ba2_diff_mean) <- c('bidder_id',
                             paste0(names(by_ba2_diff_mean)[2:6], '_mean'))

by_ba2_diff_med <- by_ba2_diff %>% 
    group_by(bidder_id) %>% 
    summarise_if(is.numeric, median)
names(by_ba2_diff_med) <- c('bidder_id',
                            paste0(names(by_ba2_diff_med)[2:6], '_median'))

# This is the average count for a user over all the auctions she placed a bid.
by_ba2_diff_agg <- left_join(by_ba2_diff_mean, by_ba2_diff_med) # 2013   11
# Remove the temporary data:
rm(by_ba2_diff_mean, by_ba2_diff_med)


# aggregate: ----
rrr <- by_bidder2 %>%
    select(-data) %>% 
    left_join(by_ba2_agg) %>% 
    left_join(by_ba2_diff_agg)

dim(rrr) # 2013 x 26

# Save for later usage:
write_csv(rrr, 'assets/rrr.csv')

