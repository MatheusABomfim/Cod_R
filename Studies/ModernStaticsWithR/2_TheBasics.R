library("ggplot2")

#Import dataset
msleep
?msleep

#Descriptive analysis
##Visualization
View(msleep)
head(msleep)
tail(msleep)
dim(msleep)
str(msleep)
names(msleep)

##Summary
summary(msleep)
summary(msleep$sleep_cycle)
sum(msleep$sleep_total > 8)   # Frequency (count)
mean(msleep$sleep_total > 8)  # Relative frequency (proportion)
sum(msleep$sleep_cycle, na.rm = TRUE) # Ignore False values

cor(msleep$sleep_total, msleep$sleep_rem, use = "complete.obs")
?cor

##Visualization of frequency of categorical data
table(msleep$conservation)

# Counts:
table(msleep$vore, msleep$conservation)

# Proportions, per row:
proportions(table(msleep$vore, msleep$conservation),
            margin = 1)

# Proportions, per column:
proportions(table(msleep$vore, msleep$conservation),
            margin = 2)

# Proportions, out of total:
proportions(table(msleep$vore, msleep$conservation))
?proportions

## Lesson 1
library(palmerpenguins)
View(penguins) # 3 categorical variables and 5 numerical variables, there are NAs rows
summary(penguins)
dim(penguins)
table(penguins$sex, penguins$island)
?penguins
str(penguins)

## Plots
library(ggplot2)
ggplot(msleep, aes(x = sleep_total, y = sleep_rem)) + geom_point()

