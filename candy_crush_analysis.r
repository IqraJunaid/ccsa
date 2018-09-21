
# This sets the size of plots to a good default.
options(repr.plot.width = 5, repr.plot.height = 4)

# Loading in packages
library(tidyverse)

# Reading in the data
data <- read_csv("candy_crush.csv")

# Printing out the first couple of rows
head(data)

print("Number of players:")
length(unique(data$player_id))

print("Period for which we have data:")
range(data$dt)

# Calculating level difficulty
difficulty <- data %>%
    group_by(level) %>%
    summarise(wins = sum(num_success), attempts = sum(num_attempts)) %>%
    mutate(p_win = wins / attempts)
# Printing out the level difficulty
difficulty

# Plotting the level difficulty profile
ggplot(difficulty, aes(x = level, y = p_win)) +
    geom_line(color='red') +
    scale_x_continuous(breaks = c(1:15)) +
    scale_y_continuous(labels = scales::percent)

# Adding points and a dashed line
ggplot(difficulty, aes(x = level, y = p_win)) +
    geom_line(color='red') +
    geom_point(color='darkblue') +
    geom_hline(yintercept = 0.10, linetype = "dashed",color='darkgreen') +
    scale_x_continuous(breaks = c(1:15)) +
    scale_y_continuous(labels = scales::percent)

# Computing the standard error of p_win for each level
difficulty <- difficulty %>%
    mutate(error = sqrt(p_win * (1 - p_win) / attempts))

# Adding standard error bars
ggplot(difficulty, aes(x = level, y = p_win)) +
    geom_line(color='yellow') +
    geom_point(color='blue') +
    geom_hline(yintercept = 0.10, linetype = "dashed",color='darkgreen') +
    geom_errorbar(aes(ymin = p_win - error, ymax = p_win + error),color='red') +
    scale_x_continuous(breaks = c(1:15)) +
    scale_y_continuous(labels = scales::percent)

# The probability of completing the episode without losing a single time
p <- prod(difficulty$p_win)

# Printing it out
p

# Should our level designer worry about that a lot of 
# players will complete the episode in one attempt?
should_the_designer_worry = FALSE # TRUE / FALSE
