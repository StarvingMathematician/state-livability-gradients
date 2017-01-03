#############################
# Load and process the data #
#############################

score_df <- read.csv('state_scores_lat_long.csv', stringsAsFactors = FALSE)

attach(score_df)

adj_list <- readLines('state_adjacency_list_sans_DC.txt')
adj_list <- strsplit(adj_list,',')

N <- length(unlist(adj_list))
adj_df <- data.frame(state1=rep("", N), state2=rep("", N),
                     weight_ratio=rep(NA, N), weight_ratio_norm=rep(NA, N),
                     weight_diff=rep(NA, N), weight_diff_norm=rep(NA, N),
                     stringsAsFactors=FALSE)

i <- 1
for (line in adj_list){
  first_i <- i
  ratio_total <- 0
  diff_total <- 0
  state1 <- State[Abbreviation==line[1]]
  for (state_abrev in line){
    state2 <- State[Abbreviation==state_abrev]
    adj_df$state1[i] <- state1
    adj_df$state2[i] <- state2
    adj_df$weight_ratio[i] <- Value[State==state2] # temporary, still needs to be rescaled
    adj_df$weight_diff[i] <- max(0, Value[State==state2] - Value[State==state1]) # temporary, still needs to be rescaled
    ratio_total <- ratio_total + Value[State==state2]
    diff_total <- diff_total + max(0, Value[State==state2] - Value[State==state1])
    i <- i + 1
  }
  adj_df$weight_ratio_norm[first_i:(i-1)] <- adj_df$weight_ratio[first_i:(i-1)] / ratio_total # rescaling step
  if (diff_total > 0){ # pure sinks will have diff_total == 0 --> all zero row in transition matrix
    adj_df$weight_diff_norm[first_i:(i-1)] <- adj_df$weight_diff[first_i:(i-1)] / diff_total # rescaling step
  }
}

# Divided 0/0 in some spots, so replace those NA's with 0's
adj_df$weight_diff_norm[is.na(adj_df$weight_diff_norm)] = 0

##################################################################################################################

library(maps)
library(diagram)

map('state', resolution = 1)

##################################################################################################################

############
# Clean up #
############

detach(score_df)
