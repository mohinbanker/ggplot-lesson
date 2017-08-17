# PACKAGE AND DATA SETUP -----------
# Run this code before starting
# This loads your packages and your data

ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

# Installing and loading packages

packages <- c("data.table", "tidyverse", "ggthemes", "ISLR", 
              "gganimate", "gapminder", "forcats", "ggmap")
ipak(packages)

gapminder <- data.table(gapminder)
run <- fread("run.csv")
baseball <- fread("baseball_salaries.csv")

baseball_yearly <- aggregate(baseball$Salary ~ baseball$Years, FUN=mean)
names(baseball_yearly) <- c("years", "mean_salary")
baseball_yearly$median_salary <- tapply(baseball$Salary, baseball$Years, FUN=median)
baseball_yearly$fq_salary <- tapply(baseball$Salary, baseball$Years, FUN=quantile,probs=c(0.25))
baseball_yearly$tq_salary <- tapply(baseball$Salary, baseball$Years, FUN=quantile,probs=c(0.75))
baseball_yearly$sd_salary <- tapply(baseball$Salary, baseball$Years, FUN=sd)
baseball_yearly$num_players <- tapply(baseball$Salary, baseball$Years, FUN=length)


# PROBLEMS -------

# An example of a ggplot call to help you:


# Question 1
# Using the gapminder dataset, create a boxplot of population by continent. 
# Make sure that each continent's boxplot is colored distinctly.
# Change the y coordinate system to a log scale
# Add a title and labels to the axes and legend

# Helpful functions
?geom_boxplot
?scale_y_log10

ggplot() # Add some more

# Question 2
# Use your knowledge of ggplot to fix the errorbars on this plot.
ggplot(data= baseball_yearly) +
  geom_point(aes(x=years,y=median_salary,size=num_players,fill=num_players),shape=22,stroke=0.8) +
  geom_line(aes(x=years,y=median_salary),lty=2) + scale_fill_gradient(low="#FFFFFF",high="#0000FF",name="Number of \nPlayers") +
  geom_errorbar(aes(x=years, ymax = tq_salary, ymin=fq_salary), 
                position= position_dodge(width=0.8), width=0.8) +
  scale_size_continuous(name="Number of \nPlayers") +
  guides(colour = guide_legend()) + ylab("Salary ($K)") + xlab("Years player has played") + ggtitle("Median Salaries by Year") +
  theme_bw()

# Question 3 - CHALLENGE
# Using the run dataset, make a bar graph of average speed by run session.
# Then, overlay a boxplot of the speed distributions in each run

# Calculate mean speed of run 
run_summary <- run[, .(speed_mph = mean(speed_mph, na.rm = T)), .(run)]
run_summary

ggplot() # Add some more

