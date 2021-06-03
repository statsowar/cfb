# This program runs a little simulation for the college baseball regionals using runs for, runs against, and strength of schedule. 
# 
# The ratings are just Runs Per Game - Runs Allowed Per Game for Each team, and they are weighted by strength of schedule. Win probabilities for each game are calculated in the style of Bradley-Terry. 
# 

easypackages::libraries("knitr", "tidyverse", "magrittr")

#Enter teams, ratings, and sos 
team <- c("TCU", "Oregon State", "Dallas Baptist", "McNeese")
seed <- c(1, 2, 3, 4)
rating <- c(3.91, 1.87, 3.39, 1.22)
sos <- c(4, 9, 79, 179)

#Create DataFrame, Add simple opponent-adjustment if you want!

region <- as.data.frame(cbind(teams, seed, rating, sos)) %>%
    mutate_at(vars(c("seed", "rating", "sos")), as.numeric) %>%
    mutate(adj_rating = (1+(1/sos)) * rating)

#How the Bracket Works: 
#G1: 1 v 4
#G2: 2 v 3
#G3: Winner G1 v Winner G2
#G4: Loser G1 v Loser G2 (loser eliminated)
#G5: Loser G3 v Winner G4 (loser eliminated)
#G6: Winner G5 vs Winner G3
#G7: Winner G6 vs Loser G6 (if Winner G6 != Winner G3)

set.seed(1873)
# Labelling Seeds for Round 1
seed1 <- region %>% filter(seed == 1)
seed2 <- region %>% filter(seed == 2)
seed3 <- region %>% filter(seed == 3)
seed4 <- region %>% filter(seed == 4)

# Create Empty Data Frame for results
regional.results <- as.data.frame(matrix(ncol = 2, nrow = 1)) %>% select(sim=V1, winner=V2) 

# Set number of sims you want to run
num.sims <- 100000

for(i in 1:num.sims){
#Sim G1
prob1seed <- seed1$adj_rating/(seed1$adj_rating + seed4$adj_rating)
g1.winner <- sample(c(seed1$team, seed4$team), replace = T, size = 1, prob = c(prob1seed, 1-prob1seed))

#Sim G2
prob2seed <- seed2$adj_rating/(seed2$adj_rating + seed3$adj_rating)
g2.winner <- sample(c(seed2$team, seed3$team), replace = T, size = 1, prob = c(prob2seed, 1-prob2seed))

#Sim G3
g3.team1 <- region %>% filter(team == g1.winner)
g3.team2 <- region %>% filter(team == g2.winner)

prob.team1 <- g3.team1$adj_rating/(g3.team1$adj_rating+g3.team2$adj_rating)
g3.winner <- sample(c(g3.team1$team, g3.team2$team), replace = T, size = 1, prob = c(prob.team1, 1-prob.team1))

#Sim G4
g4 <- region %>% filter(!team %in% c(g1.winner, g2.winner))
g4.team1 <- g4[1, ]
g4.team2 <- g4[2, ]

prob.team1 <- g4.team1$adj_rating/(g4.team1$adj_rating + g4.team2$adj_rating)
g4.winner <- sample(c(g4.team1$team, g4.team2$team), replace = T, size = 1, prob = c(prob.team1, 1-prob.team1))

#Sim G5
g5.team1 <- region %>% filter(team == g4.winner)
g5.team2 <- region %>% filter(!team %in% c(g4.team1, g4.team2, g3.winner))

prob.team1 <- g5.team1$adj_rating/(g5.team1$adj_rating + g5.team2$adj_rating)
g5.winner <- sample(c(g5.team1$team, g5.team2$team), replace = T, size = 1, prob = c(prob.team1, 1-prob.team1))

#Sim G6
g6.team1 <- region %>% filter(team == g3.winner)
g6.team2 <- region %>% filter(team == g5.winner)

prob.team1 <- g6.team1$adj_rating/(g6.team1$adj_rating + g6.team2$adj_rating)

g6.winner <- sample(c(g6.team1$team, g6.team2$team), replace = T, size = 1, prob = c(prob.team1, 1-prob.team1))
# If loser already has one loss, we have a winner. Otherwise have to play again!
if(g6.winner == g3.winner){
   regional.results[i, ] <- c(i, g6.winner)
}else{
    g7.winner <- sample(c(g6.team1$team, g6.team2$team), replace = T, size = 1, prob = c(prob.team1, 1-prob.team1))
    regional.results[i, ] <- c(i, g7.winner)
}
}

#Create frequencies of how often each team won
sim <- regional.results %>% group_by(winner) %>% count() %>% arrange(desc(n)) %>%
    mutate(freq = paste0(100*n/num.sims, "%")) %>% select(-n)
