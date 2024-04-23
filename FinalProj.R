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

### Function to recognize when subject ID numbers change ###
recognize_id_changes <- function(subj_id) {
  # Initialize an empty vector to store the indices of ID changes
  id_change_indices <- c(1)  # Start with the first index
  
  # Loop through the ID column
  for (i in 2:length(narcissismIndex$subj_id)) {
    # Check if the current ID is different from the previous one
    if (narcissismIndex$subj_id[i] != narcissismIndex$subj_id[i - 1]) {
      # If it's different, record the index
      id_change_indices <- c(id_change_indices, i)
    }
  }
  
  # Return the indices where ID changes occur
  return(id_change_indices)
}

id_changes <- recognize_id_changes(narcissismIndex)
print(id_changes)

### Some statistical stuff ###
# Hostile to Negative Affect #

##### Bar graph for Path Estimates in subgroups####
library(ggplot2)

ggplot(pathEstimates, aes(x = processes_titles, fill= subgroup_titles)) +
  geom_bar(stat = "count", position = "dodge") +
  labs(title = "Group Processes Comparisons", x = "Titles", y = "Count", fill= "Groups")+
  scale_y_continuous(limits = c(0, 120)) 


