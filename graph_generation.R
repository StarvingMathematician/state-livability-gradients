score_df <- read.csv('state_scores_lat_long.csv', stringsAsFactors = FALSE)

attach(score_df)

adj_list <- readLines('state_adjacency_list_sans_DC.txt')
adj_list <- strsplit(adj_list,',')

N <- length(unlist(adj_list))
adj_df <- data.frame(state1=rep("", N), state2=rep("", N), weight=rep(NA, N), stringsAsFactors=FALSE)

i <- 1
for (line in adj_list){
  first_i <- i
  state_total <- 0
  state1 <- State[Abbreviation==line[1]]
  for (state_abrev in line){
    state2 <- State[Abbreviation==state_abrev]
    adj_df$state1[i] <- state1
    adj_df$state2[i] <- state2
    adj_df$weight[i] <- Value[State==state2] # temporary, still needs to be rescaled
    state_total <- state_total + Value[State==state2]
    i <- i + 1
  }
  adj_df$weight[first_i:(i-1)] <- adj_df$weight[first_i:(i-1)] / state_total # rescaling step
}

############################################################################################################

# For the purposes of visualization, we don't want this to be a Markov chain
# Instead, we want it to be a raw measure of how much better one state is than another, i.e. score2 - score1

adj_df$score_diff <- NA
for (i in 1:nrow(adj_df)){
  adj_df$score_diff[i] <- Value[State==adj_df$state2[i]] - Value[State==adj_df$state1[i]]
}

############################################################################################################

# Get normalized diff to compute Markov probabilities. Most reasonable way to do things is for people to
# always move towards states which are better. Note that the resulting MC is not ergotic; some states are
# pure sinks (e.g. Oregon, Florida, Montana)
# 
# Either solve in closed-form, or iterate until convergence
# Note that the initial condition *does* matter due to non-ergoticity, as all probability mass will be
# sucked up by this small set of states...

############################################################################################################

library(maps)
library(diagram)

map('state', resolution = 1)

for (i in 1:nrow(adj_df)){
  if (adj_df$score_diff[i] > 0){
    x1 <- Longitude[State==adj_df$state1[i]]
    y1 <- Latitude[State==adj_df$state1[i]]
    x2 <- Longitude[State==adj_df$state2[i]]
    y2 <- Latitude[State==adj_df$state2[i]]
    arrows(x1, y1, x2, y2, length=0.075, angle=15, lwd=10*adj_df$score_diff[i])
    # break
  }
}

# for (i in 1:nrow(adj_df)){
#   if (!(adj_df$state1[i] %in% c('Alaska','Hawaii')) && adj_df$state1[i] != adj_df$state2[i]){
#     x1 <- Longitude[State==adj_df$state1[i]]
#     y1 <- Latitude[State==adj_df$state1[i]]
#     x2 <- Longitude[State==adj_df$state2[i]]
#     y2 <- Latitude[State==adj_df$state2[i]]
#     arrows(x1, y1, x2, y2, length=0.075, angle=15)
#     # break
#   }
# }

# 1) Steady-state Markov Chain
# 2) Larger/Cleaner/Non-overlapping plotting
# 3) Width-weighting w/o clutter (color-code? keep only dominant forwards arrow = score2-score1)

detach(score_df)

########################################################################################################

# Convert to matrix, compute eigendecomposition
adj_mat <- matrix(0, 50, 50)
rownames(adj_mat) <- State
colnames(adj_mat) <- State

for (i in 1:nrow(adj_df)){
  adj_mat[adj_df$state1[i],adj_df$state2[i]] <- adj_df$weight[i]
}

adj_mat <- adj_mat[-which(rownames(adj_mat) %in% c('Alaska','Hawaii')),-which(colnames(adj_mat) %in% c('Alaska','Hawaii'))]

library(expm)

m <- adj_mat %^% 10000
pi <- m[1,]

# Plot steady-states upon the map
colors <- sapply(pi, function(x) rgb(1, 0, 0,x/max(pi)))
map(database = "state",regions = names(pi),col = colors,fill=T)

# Noooope
# This is terrible, states with shitty scores to begin with wind up attracting more people, because using ratios
# Change to using normalized differences instead


# eqn_system_A <- t(adj_mat - diag(48))
# eqn_system_A <- rbind(adj_mat,rep(1,48))
# eqn_system_b <- c(rep(0,48),1)
# x <- qr.solve(eqn_system_A, eqn_system_b)
# 
# eig <- eigen(adj_mat)
# eig2 <- eigen(t(adj_mat))



