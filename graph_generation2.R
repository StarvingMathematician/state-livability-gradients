#############################
# Load and process the data #
#############################

score_df <- read.csv('data/state_scores_lat_long.csv', stringsAsFactors = FALSE)

attach(score_df)

adj_list <- readLines('data/state_adjacency_list_sans_DC.txt')
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
# Note that in some rows this will result in all-zeros, therefore fix this in the matrix formulation below
adj_df$weight_diff_norm[is.na(adj_df$weight_diff_norm)] = 0

##################################################################################################################

##########################
# Solve for steady-state #
##########################

library(maps)
library(diagram)
library(expm)

adj_mat_ratio <- matrix(0, 50, 50)
rownames(adj_mat_ratio) <- State
colnames(adj_mat_ratio) <- State

adj_mat_diff <- adj_mat_ratio

for (i in 1:nrow(adj_df)){
  adj_mat_ratio[adj_df$state1[i],adj_df$state2[i]] <- adj_df$weight_ratio_norm[i]
  adj_mat_diff[adj_df$state1[i],adj_df$state2[i]] <- adj_df$weight_diff_norm[i]
}

adj_mat_ratio <- adj_mat_ratio[-which(rownames(adj_mat_ratio) %in% c('Alaska','Hawaii')),-which(colnames(adj_mat_ratio) %in% c('Alaska','Hawaii'))]
adj_mat_diff <- adj_mat_diff[-which(rownames(adj_mat_diff) %in% c('Alaska','Hawaii')),-which(colnames(adj_mat_diff) %in% c('Alaska','Hawaii'))]

# Need to make sure that the matrix is stochastic, so insert 1's along main diagonal (i.e. self-loops) as needed
for (i in 1:nrow(adj_mat_diff)){
  if (sum(adj_mat_diff[i,]) == 0){
    adj_mat_diff[i,i] = 1
  }
}

m_ratio <- adj_mat_ratio %^% 10000
pi_ratio <- m_ratio[1,]

m_diff <- adj_mat_diff %^% 10000
# pi_diff <- m_diff[1,]

# (i,j)th entry of m_diff tells you how much of final probability mass ends up in state j 
# supposing that the initial state had been a 1-hot vector placing 100% of mass in state i

# Only fair way to break the tie is to use actual state populations:
population_df <- read.csv('data/state_population.csv', stringsAsFactors = FALSE)
pi_diff <- population_df$PopulationNorm[!(population_df$State %in% c('Alaska','Hawaii'))] %*% m_diff

####################
# Plot Ratio Chain #
####################

# Plot steady-states on the map
colors_ratio <- sapply(pi_ratio, function(x) rgb(1, 0, 0,x/max(pi_ratio)))
map(database="state", regions=names(pi_ratio), col=colors_ratio, fill=T)

tmp_ratio <- map('state',plot=FALSE,namesonly=TRUE) # getting the names used by map
tmp_ratio <- match(gsub('(:.*)','',tmp_ratio),tolower(names(pi_ratio))) # matching (after adjusting using gsub and tolower)
map('state',fill=TRUE,col=colors_ratio[tmp_ratio]) # convert your numbers to grey-scale and selct using the match

for (i in 1:nrow(adj_df)){
  if (!(adj_df$state1[i] %in% c('Alaska','Hawaii')) && adj_df$state1[i] != adj_df$state2[i]){
    x1 <- Longitude[State==adj_df$state1[i]]
    y1 <- Latitude[State==adj_df$state1[i]]
    x2 <- Longitude[State==adj_df$state2[i]]
    y2 <- Latitude[State==adj_df$state2[i]]
    arrows(x1, y1, x2, y2, length=0.075, angle=30, lwd=10*adj_df$weight_ratio_norm[i])
  }
}

###################
# Plot Diff Chain #
###################

colors_diff <- sapply(pi_diff, function(x) rgb(1, 0, 0,x/max(pi_diff)))
map(database="state", regions=names(pi_diff), col=colors_diff, fill=T)

tmp_diff <- map('state',plot=FALSE,namesonly=TRUE) # getting the names used by map
tmp_diff <- match(gsub('(:.*)','',tmp_diff),tolower(colnames(pi_diff))) # matching (after adjusting using gsub and tolower)
map('state',fill=TRUE,col=colors_diff[tmp_diff]) # convert your numbers to grey-scale and selct using the match

for (i in 1:nrow(adj_df)){
  if (!(adj_df$state1[i] %in% c('Alaska','Hawaii')) && adj_df$state1[i] != adj_df$state2[i] && adj_df$weight_diff_norm[i] > 0){
    x1 <- Longitude[State==adj_df$state1[i]]
    y1 <- Latitude[State==adj_df$state1[i]]
    x2 <- Longitude[State==adj_df$state2[i]]
    y2 <- Latitude[State==adj_df$state2[i]]
    arrows(x1, y1, x2, y2, length=0.075, angle=30, lwd=10*adj_df$weight_diff_norm[i])
  }
}

##################################################################################################################

############
# Clean up #
############

detach(score_df)
