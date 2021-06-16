rm(list =ls())

library(chron)
library(ggplot2)

setwd("C:/Users/muson/OneDrive/Documents/Side Projects/Football Analytics/")

##### Manchester Utd #####

data           <- read.table(file = "Data/Manchester United/Stats.txt", 
                             sep = ",", header = TRUE)
head(data)
str(data)
data$Date      <- as.Date(data$Date)
data$Time      <- substr(data$Time, 0, 5)
data$Comp      <- as.factor(data$Comp)
data$Venue     <- as.factor(data$Venue)
data$Result    <- as.factor(data$Result)
data$xG        <- as.numeric(data$xG)
data$xGA       <- as.numeric(data$xGA)
data$Captain   <- as.factor(data$Captain)
data$Formation <- as.factor(data$Formation)
tail(data)
str(data)

graph <- ggplot(data, aes(x = xG, y = GF)) + 
  geom_point(aes(color = Result), size = 2.5) +
  geom_abline(intercept = 0) + 
  scale_color_manual(values = c("#006600", "red", "#56B4E9"))

graph +
  theme_bw() + 
  scale_x_continuous(breaks = seq(1, 4, by = 1)) +
  scale_y_continuous(breaks = seq(1, 10, by = 1)) +
  labs(title = "Plot of Man United's Goals against Expected Goals
       for the 2020/21 season",
       caption = "Data obtained from www.fbref.com, 
       plot constructed by Musonda Sinkala",
       x = "Expected Goals",
       y = "Goals") +
  geom_text(x = 3.7, y = 8.9, label = 'Southampton', size = 2.5) +
  geom_text(x = 4, y = 5.9, label = 'Leeds', size = 2.5) +
  geom_text(x = 2.5, y = 0.9, label = 'West Brom', size = 2.5) +
  geom_text(x = 1.5, y = -0.1, label = 'Arsenal', size = 2.5) + 
  geom_text(x = 0.3, y = -0.1, label = 'Arsenal', size = 2.5) + 
  geom_text(x = 2.0, y = 1.8, label = 'Granada (A)', size = 2.5) +
  geom_text(x = 1.6, y = 7.5, label = 'Overperforming', size = 5, color = 'orange', angle = 27) + 
  geom_text(x = 3.2, y = 2.0, label = 'Underperforming', size = 5, color = 'orange', angle = 27)

###---###---###---###---###---###---###---###---###---###---###---###---###---###---###---###---###---###---###---###---###---###---###---###

graph2 <- ggplot(data, aes(x = xGA, y = GA)) + 
  geom_point(aes(color = Result), size = 2.5) +
  geom_abline(intercept = 0) + 
  scale_color_manual(values = c("#006600", "red", "#56B4E9"))

graph2 +
  theme_bw() + 
  scale_x_continuous(breaks = seq(1, 4, by = 1)) +
  scale_y_continuous(breaks = seq(1, 10, by = 1)) +
  labs(title = "Plot of Man United's Goals against versus Expected Goals against
       for the 2020/21 season",
       caption = "Data obtained from www.fbref.com, 
       plot constructed by Musonda Sinkala",
       x = "Expected Goals Against",
       y = "Realized Goals Against") +
  geom_text(x = 1.7, y = 2.9, label = 'Crystal Palace', size = 2.5) +
  geom_text(x = 1.3, y = 3.1, label = 'RB Leipzig', size = 2.5) +
  geom_text(x = 2.9, y = 3.1, label = 'PSG', size = 2.5) +
  geom_text(x = 3.8, y = 5.9, label = 'Spurs', size = 2.5) +
  geom_text(x = 1.3, y = 5.0, label = 'Underperforming', size = 5, color = 'orange', angle = 31) + 
  geom_text(x = 2.6, y = 1.0, label = 'Overperforming', size = 5, color = 'orange', angle = 31) 

###---###---###---###---###---###---###---###---###---###---###---###---###---###---###---###---###---###---###---###---###---###---###---###

data.noNA         <- na.omit(data[, c(1:13)])
data.noNA$GF.cum  <- data.noNA$GF[1]
data.noNA$GA.cum  <- data.noNA$GA[1]
data.noNA$xG.cum  <- data.noNA$xG[1]
data.noNA$xGA.cum <- data.noNA$xGA[1]

for(i in 2:nrow(data.noNA))
{
  data.noNA$GF.cum[i] <- data.noNA$GF.cum[i - 1] + data.noNA$GF[i]
  data.noNA$GA.cum[i] <- data.noNA$GA.cum[i - 1] + data.noNA$GA[i]
  data.noNA$xG.cum[i] <- data.noNA$xG.cum[i - 1] + data.noNA$xG[i]
  data.noNA$xGA.cum[i] <- data.noNA$xGA.cum[i - 1] + data.noNA$xGA[i]
}

p <- ggplot(data.noNA, aes(x = Date)) +
  theme_bw() +
  ylim(0, 9) +
  geom_line(aes(y = xG,
                col = '#56B4E9'),
            size = 1) + 
  geom_line(aes(y = GF,
                col = "orange"),
            size = 1) +
  theme(legend.title = element_blank()) +
  scale_color_manual(labels = c("xG", "Goals"), 
                     values = c("#56B4E9", "orange")) +
  labs(title = "Time Series of Man United's Goals and Expected Goals
       for the 2020/21 season",
       caption = "Data obtained from www.fbref.com, 
       plot constructed by Musonda Sinkala",
       x = "Date",
       y = "Expected Goals/Goals For")
p

##### Manchester City #####

data <- read.table(file = "Data/Manchester City/Stats.txt", sep = ",", header = TRUE)

head(data)
str(data)
data$Date      <- as.Date(data$Date)
data$Time      <- substr(data$Time, 0, 5)
data$Comp      <- as.factor(data$Comp)
data$Venue     <- as.factor(data$Venue)
data$Result    <- as.factor(data$Result)
data$xG        <- as.numeric(data$xG)
data$xGA       <- as.numeric(data$xGA)
data$Captain   <- as.factor(data$Captain)
data$Formation <- as.factor(data$Formation)
tail(data)
str(data)

graph <- ggplot(data, aes(x = xG, y = GF)) + 
  geom_point(aes(color = Result), size = 2.5) +
  geom_abline(intercept = 0) + 
  scale_color_manual(values = c("#006600", "red", "#56B4E9"))

graph +
  theme_bw() + 
  xlim(0, 4) +
  ylim(0, 5) +
  labs(title = "Plot of Man City's Goals against Expected Goals
       for the 2020/21 season",
       caption = "Data obtained from www.fbref.com, 
       plot constructed by Musonda Sinkala",
       x = "Expected Goals",
       y = "Goals") +
  # geom_text(x = 3.7, y = 8.9, label = 'Southampton', size = 2.5) +
  # geom_text(x = 4, y = 5.9, label = 'Leeds', size = 2.5) +
  # geom_text(x = 2.5, y = 0.9, label = 'West Brom', size = 2.5) +
  # geom_text(x = 1.5, y = -0.1, label = 'Arsenal', size = 2.5) + 
  # geom_text(x = 0.3, y = -0.1, label = 'Arsenal', size = 2.5) + 
  # geom_text(x = 2.0, y = 1.8, label = 'Granada (A)', size = 2.5) +
  geom_text(x = 1.6, y = 4.5, label = 'Overperforming', size = 5, color = 'orange', angle = 27) + 
  geom_text(x = 3.2, y = 0.7, label = 'Underperforming', size = 5, color = 'orange', angle = 27)

###---###---###---###---###---###---###---###---###---###---###---###---###---###---###---###---###---###---###---###---###---###---###---###

graph2 <- ggplot(data, aes(x = xGA, y = GA)) + 
  geom_point(aes(color = Result), size = 2.5) +
  geom_abline(intercept = 0) + 
  scale_color_manual(values = c("#006600", "red", "#56B4E9"))

graph2 +
  theme_bw() + 
  scale_x_continuous(breaks = seq(1, 4, by = 1)) +
  scale_y_continuous(breaks = seq(1, 10, by = 1)) +
  labs(title = "Plot of Man City's Goals against versus Expected Goals against
       for the 2020/21 season",
       caption = "Data obtained from www.fbref.com, 
       plot constructed by Musonda Sinkala",
       x = "Expected Goals Against",
       y = "Realized Goals Against") +
  # geom_text(x = 1.7, y = 2.9, label = 'Crystal Palace', size = 2.5) +
  # geom_text(x = 1.3, y = 3.1, label = 'RB Leipzig', size = 2.5) +
  # geom_text(x = 2.9, y = 3.1, label = 'PSG', size = 2.5) +
  # geom_text(x = 3.8, y = 5.9, label = 'Spurs', size = 2.5) +
  geom_text(x = 1.0, y = 3.5, label = 'Underperforming', size = 5, color = 'orange', angle = 31) + 
  geom_text(x = 2.0, y = 0.5, label = 'Overperforming', size = 5, color = 'orange', angle = 31) 

###---###---###---###---###---###---###---###---###---###---###---###---###---###---###---###---###---###---###---###---###---###---###---###
