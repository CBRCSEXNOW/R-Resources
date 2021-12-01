#Read in Data
data <- read.csv(file = "C:\\Users\\Kiffer\\Google Drive\\Work\\2. Current Projects\\HSCI_410\\3. Lectures\\CSCS_data.csv", na.strings = c(" ", "", "NA", "9999: Missing")) 

#Install Table One Package
install.packages("tableone")
library(tableone)
table(data$drugs_alcohol)

#Outcome of Interest (Recommend dichotomizing for simplicity)
data$outcome_var <- data$drugs_alcohol

#Create Bivariable Table
Table <- CreateTableOne(data = data, strata = "outcome_var")

library(dplyr)
Frequencies <- print(Table, showAllLevels = TRUE, missing = FALSE, printToggle = FALSE, cramVars = TRUE, varLabels = TRUE, explain = FALSE) %>%
  as_tibble(rownames = NA) 

Table <- as.data.frame(Frequencies)
View(Table)

