library(tidyverse)

# generate state environment for lupin
# two terminal state

lupin_grid <- function(nhouse, seed=1234){
  set.seed(seed) # note that if p=0, this changes nothing
  data.frame(x=1:nhouse) %>%
    mutate(is_terminal = ((x==nhouse) | (x==nhouse-1)) )%>%
    mutate(reward = val_house)
}
### EXAMPLE OF VECTOR OF HOUSES VALUES  ###
val_house<-c(20,5,5,200,5)

# Initialize dataframe with all values equals 0
df <- lupin_grid(length(val_house))
df$v <- 0
df$v[df$is_terminal] <- tail(val_house,2)
# set rewards to houses values
df$reward <- val_house
not_done <- TRUE
while(not_done){
  v_cache <- df$v
  for(i in 1:nrow(df)){
    if(!df$is_terminal[i]){
        df$v[i] <- df$reward[i] + max(v_cache[c((i+2):nhouse)])
    }
  }
  not_done<-sum(abs(df$v - v_cache))>1e-3
  }

df
# recommended action
first_house <-which(df$v == max(df$v))
house <- first_house
action <- c(first_house)
which(df$v[c((house+2):nhouse)]==max(df$v[c((house+2):nhouse)]))
while(!df$is_terminal[house]){
    house <- house + which(df$v[c((house+2):nhouse)]==max(df$v[c((house+2):nhouse)]))
    action <- c(action, house)
    print(house)
  }

paste(c("Les actions à prendre sont  : cambrioler les maisons", action), collapse = " ")

      