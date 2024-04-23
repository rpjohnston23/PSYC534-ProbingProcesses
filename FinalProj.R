setwd("Processes Files")

### Read in data ###
pathEstimates = read.csv("indivPathEstimatesNoLag.csv", header = TRUE, 
              na = c("0", "~"))

narcissismIndex = readRDS("data_Wright2017.RDS")

### Creating new titles for simplication of data ###
# Creating subgroup titles #
pathEstimates <- as.data.frame(pathEstimates)
pathEstimates$subgroup_titles <- matrix(NA,length(pathEstimates$file),1)

pathEstimates$subgroup_titles[which(pathEstimates$sub_membership<= 1)] <- "Group 1"
pathEstimates$subgroup_titles[which(pathEstimates$sub_membership>= 2)] <- "Group 2"

print(pathEstimates$subgroup_titles)

# Creating titles for processes #
pathEstimates$processes_titles <- matrix(NA,length(pathEstimates$file),1)

pathEstimates$processes_titles[which(pathEstimates$lhs == "na")] <- "Hostile to Negative Affect"
pathEstimates$processes_titles[which(pathEstimates$lhs == "nervous")] <- "Negative Affect to Nervous"
pathEstimates$processes_titles[which(pathEstimates$lhs == "energy")] <- "Positive Affect to Energy"

print(pathEstimates$processes_titles)

### Creating narcissism scale ###
# Creating new column for average narcissism score #
narcissismIndex$narcissismScale <- matrix(NA,length(narcissismIndex$subj_id),1)

# Calculating narcissism score #
narcissismIndex$narcissismScale <- (rowSums(traitsIndex[, 2:32], na.rm = T)/31)

### Some statistical stuff ###
# Hostile to Negative Affect #

##### Bar graph for Path Estimates in subgroups####
library(ggplot2)
library(dplyr)

#Group 1
group1_data <- filter(pathEstimates, subgroup_titles == "Group 1")

group1_plot <- ggplot(group1_data, aes(x = processes_titles)) +
  geom_bar(stat = "count", position = "dodge") +
  labs(title = "Group 1 Processes Comparison", x = "Titles", y = "Count")+
  scale_y_continuous(limits = c(0, 120)) 

#Group 2 
group2_data <- filter(pathEstimates, subgroup_titles =="Group 2")

 group2_plot <- ggplot(group2_data, aes(x = processes_titles)) +
  geom_bar(stat = "count", position = "dodge") +
  labs(title = "Group 2 Processes Comparison", x = "Titles", y = "Count",)+scale_y_continuous(limits = c(0, 120)) 

