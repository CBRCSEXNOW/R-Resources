############################################################
######################## Lecture 5 ######################### 
############################################################

# This lecture shows how you can identify factors associated with your outcome variable using the tableOne package.

#Read in Data
data <- read.csv(file = "C:\\Users\\Kiffer\\Google Drive\\Work\\2. Current Projects\\HSCI_410\\3. Lectures\\CSCS_data.csv", na.strings = c(" ", "", "NA", "9999: Missing")) 

#Install Table One Package
install.packages("tableone")
library(tableone)

#Outcome of Interest (Recommend dichotomizing for simplicity)
table(data$gender)
class(data$gender)

data$outcome_var <- data$gender

#Create Bivariable Table
?CreateTableOne
Table <- CreateTableOne(data = data, strata = "outcome_var")

library(dplyr)
Frequencies <- print(Table, showAllLevels = TRUE, missing = FALSE, printToggle = FALSE, cramVars = TRUE, varLabels = TRUE, explain = FALSE) %>%
  as_tibble(rownames = NA) 

Table <- as.data.frame(Frequencies)
View(Table)

############################################################
##### Tips and Questions for planning your analyses ######## 
############################################################
# Sample Size: You need about 10 observations per variable included in your analyses

# Avoid "cell counts" smaller than 5 (i.e., collapse variables in a way that is appropraite)

# Make sure to consider all relevant confounding effects (i.e., factors associated with both your primary explanatory variable and your primary outcome variable)

# You may have multiple models. Required components are a descriptives table for your sample, bivariable tests, a multivariable model, and an additional exploratory model (interaction, mediation, or factor analysis) 