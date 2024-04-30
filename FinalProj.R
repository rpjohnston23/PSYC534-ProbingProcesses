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
narcissismIndex$narcissismScale <- (rowSums(narcissismIndex[, 9:39], na.rm = T)/31)

####Combining Narcissism Scores by Subject ID ####

narcissism_averages <- aggregate(narcissismScale ~ subj_id, data = narcissismIndex, FUN = mean)
print(narcissism_averages)


##### Bar graph for Path Estimates in subgroups####
library(ggplot2)

ggplot(pathEstimates, aes(x = processes_titles, fill= subgroup_titles)) +
  geom_bar(stat = "count", position = "dodge") +
  labs(title = "Group Processes Comparisons", x = "Titles", y = "Count", fill= "Groups")+
  scale_y_continuous(limits = c(0, 120))

####Beta Averages For Group 1 and Group 2 #####

beta_averages <- aggregate(beta ~ subgroup_titles, data = pathEstimates, FUN = mean)
print(beta_averages)

#T-test to test for significant differences

t.test(pathEstimates$beta[which(pathEstimates$subgroup_titles == "Group 1")], 
       pathEstimates$beta[which(pathEstimates$subgroup_titles == "Group 2")], 
       alternative = c("two.sided", "less", "greater"),
       mu = 0, paired = FALSE, var.equal = FALSE,
       conf.level = 0.95)

## hostile to na beta averages
na_beta_subset <- subset(pathEstimates, lhs == "na")
na_beta_averages <- aggregate(beta ~ subgroup_titles, data = na_beta_subset, FUN = mean)

### na beta t-test
t.test(na_beta_subset$beta[which(na_beta_subset$subgroup_titles == "Group 1")],
       na_beta_subset$beta[which(na_beta_subset$subgroup_titles == "Group 2")],
       alternative = c("two.sided", "less", "greater"),
       mu = 0, paired = FALSE, var.equal = FALSE,
       conf.level = 0.95)

## na to nervous beta averages
nervous_beta_subset <- subset(pathEstimates, lhs == "nervous")
nervous_beta_averages <- aggregate(beta ~ subgroup_titles, data = nervous_beta_subset, FUN = mean)

### nervous beta t-test
t.test(nervous_beta_subset$beta[which(nervous_beta_subset$subgroup_titles == "Group 1")],
       nervous_beta_subset$beta[which(nervous_beta_subset$subgroup_titles == "Group 2")],
       alternative = c("two.sided", "less", "greater"),
       mu = 0, paired = FALSE, var.equal = FALSE,
       conf.level = 0.95)

## pa to energy beta averages
energy_beta_subset <- subset(pathEstimates, lhs == "energy")
energy_beta_averages <- aggregate(beta ~ subgroup_titles, data = energy_beta_subset, FUN = mean)

### energy beta t-test
t.test(energy_beta_subset$beta[which(energy_beta_subset$subgroup_titles == "Group 1")],
       energy_beta_subset$beta[which(energy_beta_subset$subgroup_titles == "Group 2")],
       alternative = c("two.sided", "less", "greater"),
       mu = 0, paired = FALSE, var.equal = FALSE,
       conf.level = 0.95)

#### Merging Data sets #####
library(dplyr)

pathEstimates$file <- as.numeric(sub("person", "", pathEstimates$file)) #removing "person" from number ids
pathEstimates$subj_id <- pathEstimates$file #creating column with same name as narrissism_averages

narc_path_merge <- merge(pathEstimates, narcissism_averages, by.y= "subj_id", all=TRUE)

###filtering subsets for each of the three paths for future analysis
narc_path_merge_na_path <- subset(narc_path_merge, lhs == "na")
narc_path_merge_nervous_path <- subset(narc_path_merge, lhs == "nervous")
narc_path_merge_energy_path <- subset(narc_path_merge, lhs == "energy")

#### Linear Regression Summary and Graph####
full_linear_regression <- function(x,y,z,a){
  model <- lm(x~y)
  graph <- plot (x,y, xlab = z, ylab = a, main = "Linear Model",
        xlim= c(-1,3), ylim = c(-1,2.5))
        abline(model, col = "red",lwd=2)
  details <- summary(model)
  
  return(list(graph,details))
}

full_linear_regression(narc_path_merge$narcissismScale, narc_path_merge$beta, "Narcissism", "Beta")

#linear progression for each path's beta vs. narcissism score
full_linear_regression(narc_path_merge_na_path$narcissismScale, narc_path_merge_na_path$beta, "Narcissism", "NA Path Beta")
full_linear_regression(narc_path_merge_nervous_path$narcissismScale, narc_path_merge_nervous_path$beta, "Narcissism", "Nervous Path Beta")
full_linear_regression(narc_path_merge_energy_path$narcissismScale, narc_path_merge_energy_path$beta, "Narcissism", "Energy Path Beta")
