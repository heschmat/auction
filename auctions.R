

require(tidyverse)
# =================================================================== #

n_distinct(bids$auction) # 12,741

sum(is.na(bids$auction)) # 29


# auctions by the number of bids placed and bots penetration:
anp <- bids %>% 
    group_by(auction) %>% 
    summarise(n = n(),
              p = mean(outcome))

auctions <- bids %>% 
    group_by(auction) %>% 
    summarise(
        t_min = min(time),
        t_length = max(time) - min(time),
        t_max = max(time),
        n_bids = n_distinct(bid_id),
        n_bidders = n_distinct(bidder_id),
        n_bids_by_bots = sum(outcome, na.rm = TRUE),
        n_hmns_by_bots = sum(outcome == 0, na.rm = TRUE),
        p_bids_by_bots = mean(outcome, na.rm = TRUE),
        p_bids_by_hmns = mean(outcome == 0, na.rm = TRUE)
    )

write_csv(auctions, 'assets/auctions.csv')

auctions %>% 
    ggplot(aes(t_length)) +
    geom_histogram(bins = 100)
#=> Two clusters of auctions seems to exists in terms of their length.

# The lightest, the less dominated by bots.
auctions %>% 
    mutate(t_min = t_min/1e15, t_max = t_max/1e15) %>% 
    ggplot(aes(t_min, t_max)) +
    geom_point(aes(color = - p_bids_by_bots),
               size = 1,
               alpha = 1/20)

# Plot eauch auction according to p_bids_by_bots:
auctions %>% 
    ggplot(aes(t_min, t_max)) +
    geom_point(aes(size = p_bids_by_bots + .1),
               alpha = 1/20)


head(auctions %>% select(t_min, t_max))


cl <- auctions %>% 
    filter(!is.na(auction)) %>% 
    select(t_min, t_max) %>% 
    kmeans(4)

auctions %>% 
    filter(!is.na(auction)) %>% 
    ggplot(aes(t_min, t_max)) +
    geom_point(aes(size = p_bids_by_bots + .1,
                   color = as.factor(cl$cluster)),
               alpha = 1/20)
# :-))))))))))))))))))))))))))))))))
# what kind of clustering is that?!
# Perhaps because we didn't normalize?

auctions2 <- auctions %>% 
    filter(!is.na(auction)) %>% 
    select(t_min, t_max, p_bids_by_bots)

scale01 <- function(v){
    (v - min(v))/(max(v) - min(v))
}

auctions2[, c('t_min', 't_max')] <- auctions2 %>% 
    select(t_min, t_max) %>% 
    map_df(scale01)

cl2 <- auctions2 %>% select(t_min, t_max) %>% 
    kmeans(4)

auctions2 %>% 
    ggplot(aes(t_min, t_max)) +
    geom_point(aes(size = p_bids_by_bots + .1,
                   color = as.factor(cl2$cluster)),
               alpha = 1/20)
# hum ?????????????????????????????
# A little bit better, but not much still.
# WORK ON THIS MORE.



# --------------------------
auctions %>% 
    ggplot(aes(t_length, n_bids)) +
    geom_point()

sum(auctions$n_bids > 25000) # 5
#<- out of 12741 auctions, 5 of them had +25000 bids.

sum(auctions$n_bids > 5000) # 75

auctions %>% 
    filter(n_bids <= 5000) %>% 
    ggplot(aes(t_length, n_bids, color = p_bids_by_bots)) +
    geom_point(alpha = 1/10)

auctions %>% 
    filter(n_bids <= 5000) %>% 
    ggplot(aes(t_length, n_bidders, color = p_bids_by_bots)) +
    geom_point(alpha = 1/10)

