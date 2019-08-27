#### INTRO ####
# 
# A simulated exploration of the card game blackjack
# FYI: the simulation for loop takes about a minute to build 50K games, 1MB
# v2019.08.26.20.14.IW
# 
#### INTRO ^^^ ####

#### LOAD LIBRARIES ####
library(tidyverse)
library(tidylog)
library(reshape2)
library(lubridate)
clock_a <- now()
#### INTRO ^^^ ####

#### DIGITAL CARD DECK ---------------------------------------------------- ####

cardKey <- c(1:52) # cards numbered 1 to 52
c2 <- rep(1, 52) # cound of unique cards instances is always 1 in all rows
c3 <- c(rep(1, 13), rep(2, 13), rep(3, 13), rep(4, 13)) # suit in numbers
c4 <- c(1:13, 1:13, 1:13, 1:13) # card value ace 1
c5 <- rep(c(14, 2:13), 4) # card value ace 14
c6 <- rep(c(1:10, 10, 10, 10), 4) # blackjack card value ace 1
c7 <- rep(c(11, 2:10, 10, 10, 10), 4) # blackjack card value ace 11
c8 <- rep(c("A", 2:10, "J", "Q", "K"), 4) # card value name
c9 <- c(rep("C", 13), rep("D", 13), rep("H", 13), rep("S", 13)) # card suit name

deckofcards <- data.frame(cardKey, c2, c3, c4, c5, c6, c7, c8, c9)
deckofcards  
rm(cardKey, c2, c3, c4, c5, c6, c7, c8, c9)
# NOTES: 
# The above code block builds a DF of 52 playing cards
# CardKey is a unique numbering for each individual card, used in the simulation
# c6 and c7 will be the card values of interest in blackjack simulation


#### DIGITAL CARD DECK ^^^ ####

#### CARD DEALING SIMULATION :::::::::::::::::::::::::::::::::::::::::::::: ####

nz <- 50000
sim1 <- data.frame(card1 = c(1:nz), card2 = c(1:nz), 
                   card3 = c(1:nz), card4 = c(1:nz),
                   card5 = c(1:nz)
                   # , gameNum = c(1:nz) # may need this in another sim
)
clock_1 <- now()
for (i in 1:nz) {
  aaa <- sample(deckofcards$cardKey, 5, replace = FALSE)
  sim1[i,1] <- aaa[1]
  sim1[i,2] <- aaa[2]
  sim1[i,3] <- aaa[3]
  sim1[i,4] <- aaa[4]
  sim1[i,5] <- aaa[5]
}
clock_2 <- now()
time_sim1_build <- clock_2 - clock_1
time_sim1_build
head(sim1, 20)
rm(aaa, i)
gc(verbose = TRUE)
format(object.size(sim1), units = "MB")
# NOTES: 
# Uses a for loop to draw 5 random cards without replacement, nz times
# A DF is used to house each card in a column for 5 total columns
# Not all 5 cards may be needed but they are always drawn, just in case
# This could be built without a for loop, but this made sense for me
#   but this is admittedly pretty slow
# For loop takes about 1min to run with 50K simulations


#### CARD DEALING SIMULATION ^^^ ####

#### MELT/JOIN/SNAP/BIND -------------------------------------------------- ####

# Melt the 5 cards and nz games into key/value pairs
sim1.1 <- melt(sim1, variable.name = "cardNum", value.name = "cardKey")
head(sim1.1, 10)
tail(sim1.1, 10)
length(sim1.1$cardKey) == nz * 5
table(sim1.1$cardNum)

# Bring the game number back into the data frame (didn't have to two step this)
sim1.2 <- mutate(sim1.1, gameNum = rep(1:nz, 5))
head(sim1.2)
tail(sim1.2)
sim1.2[(nz-5):(nz+5),]

# Perform a join on the card deck to get the various card values
sim1.3 <- left_join(sim1.2, deckofcards, by = "cardKey") %>% 
  mutate(aceInd = ifelse(c8 == "A", 1, 0))
head(sim1.3)
sim1.3[(nz-5):(nz+5),]
rm(sim1, sim1.1, sim1.2)
format(object.size(sim1.3), units = "MB")
gc(verbose = TRUE)

# Build subsets, one for each card (1 through 5)
card1 <- filter(sim1.3, cardNum == "card1") %>% 
  rename(cardNum1 = cardNum,
         cardKey1 = cardKey,
         gameNum1 = gameNum, 
         cardCount1 = c2, 
         c31 = c3,
         c41 = c4,
         c51 = c5,
         c61 = c6,
         c71 = c7,
         c81 = c8,
         c91 = c9, 
         aceInd1 = aceInd
  )
card2 <- filter(sim1.3, cardNum == "card2") %>% 
  rename(cardNum2 = cardNum,
         cardKey2 = cardKey,
         gameNum2 = gameNum, 
         cardCount2 = c2, 
         c32 = c3,
         c42 = c4,
         c52 = c5,
         c62 = c6,
         c72 = c7,
         c82 = c8,
         c92 = c9, 
         aceInd2 = aceInd
  )
card3 <- filter(sim1.3, cardNum == "card3") %>% 
  rename(cardNum3 = cardNum,
         cardKey3 = cardKey,
         gameNum3 = gameNum, 
         cardCount3 = c2, 
         c33 = c3,
         c43 = c4,
         c53 = c5,
         c63 = c6,
         c73 = c7,
         c83 = c8,
         c93 = c9, 
         aceInd3 = aceInd
  )
card4 <- filter(sim1.3, cardNum == "card4") %>% 
  rename(cardNum4 = cardNum,
         cardKey4 = cardKey,
         gameNum4 = gameNum, 
         cardCount4 = c2, 
         c34 = c3,
         c44 = c4,
         c54 = c5,
         c64 = c6,
         c74 = c7,
         c84 = c8,
         c94 = c9, 
         aceInd4 = aceInd
  )
card5 <- filter(sim1.3, cardNum == "card5") %>% 
  rename(cardNum5 = cardNum,
         cardKey5 = cardKey,
         gameNum5 = gameNum, 
         cardCount5 = c2, 
         c35 = c3,
         c45 = c4,
         c55 = c5,
         c65 = c6,
         c75 = c7,
         c85 = c8,
         c95 = c9, 
         aceInd5 = aceInd
  )

# Bind all columns from the 5 card subsets into one combined DF
sim1.4 <- bind_cols(card1, card2, card3, card4, card5)
rm(card1, card2, card3, card4, card5)
format(object.size(sim1.4), units = "MB")
gc(verbose = TRUE)

# Quick plot to show how the cards are being randomly dealt
plt0 <- sim1.3 %>% 
  ggplot(aes(x = cardKey)) + 
  geom_histogram(bins = 52, fill = "#63d05a") + 
  facet_grid(vars(cardNum)) +
  theme_bw()
plt0
rm(plt0)


#### MELT/JOIN/SNAP/BIND ^^^ ####

#### FUNCTIONS FOR PLAYING BLACKJACK ::::::::::::::::::::::::::::::::::::::: ####

# Create functions to simulate potential moves a player can make

# A simple function to determine if 21 has been reached
WinInd <- function(x) { 
  return(ifelse(x == 21, 1, ifelse(x > 21, -1, 0)))
} # x arg takes a cumulative hand total

# A function to solve for drawing 2 aces in first hand
PlayAceHand1 <- function(x, y) { 
  return(ifelse(x == 11 & y == 11, 12, x + y))
} # x = card1, y = card2

# A function that changes card 2 if both are aces to prevent two 11s
PreventBust1 <- function(x, y) { 
  return(ifelse(x == 11 & y == 11, 1, y))
} # x is card 1 and y is card 2


#### FUNCTIONS FOR PLAYING BLACKJACK ^^^ ####

#### MODELING FIRST TWO CARDS --------------------------------------------- ####

# Use the sim games and the functions, determine the first two cards played
hand_1 <- sim1.4 %>% 
  select(gameNum1, c61, c62, c71, c72) %>% 
  mutate(h1 = PlayAceHand1(c71, c72), 
         c72.2 = PreventBust1(c71, c72), 
         winInd1 = WinInd(h1))
head(hand_1)
summary(hand_1$h1)

# Visuals _______________________________________
plt1 <- hand_1 %>% mutate(Blackjack = as.factor(winInd1)) %>% 
  ggplot(aes(x = h1, fill = Blackjack)) + 
  geom_histogram(binwidth = 1, color = "white", alpha = 0.8) + 
  geom_hline(aes(yintercept = nz / 10), linetype = 2, 
             size = 1) +
  annotate("text", x = 6, y = nz / 10 - 150, 
           label = "10% observation rate") + 
  geom_hline(aes(yintercept = nz / 20), linetype = 2, 
             size = 1) +
  annotate("text", x = 6, y = nz / 20 + 150, label = "5% observation rate") + 
  scale_x_continuous(breaks = c(4:21)) + 
  theme_bw() +
  labs(title = "Distribution of starting blackjack hands (sum of both cards)", 
       subtitle = "2 aces treated as 12", 
       caption = "Simulation was run 50K times so dash line represents 10% etc.", 
       tag = "#simulation") + 
  xlab("Starting Hand Value") + 
  ylab("Simulation's Observed Frequency")
plt1
rm(plt1)
# NOTES:
# About 5% chance of getting a natural 21 with the first two cards
# About a 10% chance of getting a 20 on the fist two cards

plt2 <- hand_1 %>% mutate(FirstCard = as.factor(c71)) %>% 
  ggplot(aes(x = h1, fill = FirstCard)) + 
  geom_histogram(binwidth = 1, color = "white") + 
  scale_x_continuous(breaks = c(4:21)) + 
  theme_minimal() + 
  labs(title = "Distribution of starting blackjack hands (sum of both cards)", 
       caption = 
         "Colors represent the first drawn card, you can deduce the other card") +  
  xlab("Starting Hand Value") + 
  ylab("Simulation's Observed Frequency")
plt2
rm(plt2)

plt3 <- hand_1 %>% 
  ggplot(aes(x = h1)) + 
  stat_ecdf(geom = "step", pad = FALSE, size = 2.5, color = "#00b74f") + 
  geom_hline(aes(yintercept = 0.25), linetype = 2, size = 1) +
  geom_hline(aes(yintercept = 0.5), linetype = 2, size = 1) + 
  geom_hline(aes(yintercept = 0.75), linetype = 2, size = 1) + 
  geom_hline(aes(yintercept = 0.95), linetype = 2, size = 1) + 
  scale_x_continuous(breaks = c(seq(4, 21, by = 1))) + 
  scale_y_continuous(breaks = c(seq(0, 1, by = 0.1))) + 
  theme_bw() + 
  labs(title = "ECDF of Starting Blackjack Hands") + 
  xlab("Hand 1 Total Card Value") + 
  ylab("Cumulative Hand Probability")
plt3
rm(plt3)
# NOTES:
# The graph indicates that there is ~50% chance of starting with a 14 or less

# First hand probability tables __________________________
hand_1_prob <- hand_1 %>%
  select(h1) %>% 
  group_by(h1) %>% 
  summarise(freq = n()) %>% 
  as.data.frame() %>% 
  mutate(prob = freq / nz, 
         cumProb = cumsum(prob))
hand_1_prob 
# NOTES:
# ~4.7% change of getting a natural 21 on the first two cards
# ~12% chance of having less than 10 with your first two cards
# Starting with a hand total of 4 is 'bad luck' with less than a
#   1% probability of occurring

ls()
gc(verbose = TRUE)


#### MODELING FIRST TWO CARDS ^^^ ####

#### END OF SCRIPT ####
clock_b <- now()
time_script_run <- clock_b - clock_a
rm(clock_1, clock_2, clock_a, clock_b)
time_sim1_build
time_script_run
ls()
gc(verbose = TRUE)

rm(list = ls())

# ps ========================================================================= #
# Still working on ways to model the third card ETC