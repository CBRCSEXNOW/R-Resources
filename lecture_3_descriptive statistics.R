#### LECTURE 3 ####

#In last lecture we learned about the R Studio environment, packages, libraries, objects, and functions, and how to load, view, merge, and susbet dataframes. Today we will learn how to (1) calculate descriptive statistics, (2) create simple charts and graphs, and (3) export datasets.

#To begin, we will read in and merge our datasets.
library(readxl) #Tell R I am using the readxl library

setwd("C:\\Users\\Kiffer\\Google Drive\\Work\\2. Current Projects\\HSCI_410\\3. Lectures") #Set Working Directory

CSCS_data <- read.csv(file = "CSCS_data.csv", na.strings = c(" ", "", "NA", "9999: Missing")) #read in CSCS data

census_data <- read_xlsx(path = "census_data.xlsx", sheet = 1) #read in census data

merged_data <- merge(x = CSCS_data, y = census_data, by = "FSA", all.x = TRUE, all.y = FALSE) #Merge CSCS_data and census_data to create merged_data

#Lets now take a look at variables in the merged_data dataframe
colnames(merged_data) #Retrieve colnames for merged_data

#As you can seee there are many variables in this dataset. Lets use the skills we learned last week to create a smaller dataset. To select these variables I went through the questionnaire and data dictionary and identified those that I thought might be interesting to explore. I decided that I would look at four main groups of variables (1) demographic variables, (2) those related to social connection, (3) those related to personality (i.e., attachment, and personality), (4) and those related to work and burnout.

keepvars <- c("gender",
              "age",
              "ethnicity",
              "income",
              "relationship_status",
              "identity_vetrans",
              "identity_indigenous",
              "identity_lgbtq",
              "identity_disability",
              "identity_bipoc",
              "identity_pwud",
              "identity_newcomers",
              "identity_homeless",
              "identity_mental_health",
              "educational_attainment",
              "lonely_change_covid",
              "province",
              "POP_DENS_SQ_KM",
              "FAMILY_SIZE_AVG",
              "UNSUITABLE_HOUSING_PER",
              "UCLA3Score",
              "lonely_direct",
              "lonely_duration",
              "close_friends_num",
              "live_with_partner",
              "zimet_overall_score",
              "discrimination_score",
              "tipi_agreeable_score",
              "tipi_extraversion_score",
              "tipi_conscientiousness_score",
              "tipi_emotional_stability_score",
              "tipi_openness_score",
              "attachment_secure_score",
              "attachment_anxious_score",
              "attachment_avoidant_score",
              "employment",
              "money_concerned",
              "work_paid_enough",
              "work_quitting",
              "work_dignity",
              "work_support",
              "work_quitting",
              "burnout_tired",
              "burnout_disappointed",
              "burnout_hopeless",
              "burnout_trapped",
              "burnout_helpless",
              "burnout_depressed",
              "burnout_sick",
              "burnout_worthless",
              "burnout_difficulty_sleeping",
              "burnout_had_it",
              "BurnoutScore",
              "BurnoutScore_groups",
              "weightvec") #Creates a list of variable names and stores it as an object called "keepvars"

data_burnout <- merged_data[, names(merged_data) %in% keepvars] #create a new dataset called "data_men_burnout_vars" that only contains variables that are listed in the keepvars object

#Now that I have created a new smaller dataset, I want to save it for future use. That way I don't have to recreate my dataset everytime I want to call it in future lectures. To save a dataset I will use the write.csv() function to save it as a csv:

write.csv(data_burnout, file = "burnout_data.csv") #Save data_burnout to the working directory. Will be able to now load it in future lectures without having to merge the datasets or select my variables.

#Now that we have a more managable sized dataset, lets learn how to calculate descriptive statistics for (1) categorical (i.e., factor/character/logical) and (2) numeric (i.e., numeric/integer) variables. Lets start with a variable that we know is a categorical variable, such as gender. However, we should always start by verifying the class using the class() function.
class(data_burnout$gender) #Show the class of the gender variable

#As we suspected Gender is saved as a character variable. To get descriptive statistics for a categorical variable, we can use the table() and prop.table() functions. The table() function provides frequencies and the prop.table() function can be combined with the table function to calculate proportions. Functions can be combined by nesting them. R processess these functions by running the function from the inside out. Take a look:

table(data_burnout$gender) #calculate frequencies of gender 
prop.table(table(data_burnout$gender)) #calculate proportions of gender

#As you can see, 50.4% (n = 1232) identified as a woman, 47.3% (n = 1157) identified as a man, and 2.4% identified as non-binary. 

#Lets now try a numeric variable, such as Burnout Score. To do so we will use the summary() function.
class(data_burnout$BurnoutScore) #Confirm the class of the variable
summary(data_burnout$BurnoutScore) #get a summary

#As you can see, the summary() function gives us the minimum, 1st Quartile, Median, Mean, 3rd Quartile, Maximum, and the number of missing variables. Since we have the mean, we probably also want to standard deviation, which can be calculated using the sd() function.
?sd #read about the sd function
sd(data_burnout$BurnoutScore, na.rm = TRUE) #get the sd

#The numbers summarizing the variable are great, but sometimes it is helpful to look at the data graphically. Lets review a few of the functions that can be used to look at the data graphically:

dotchart(data_burnout$BurnoutScore) #Create a dotchart, the dotchat gives you a sense of the volume of data in given areas.
hist(data_burnout$BurnoutScore) #create a histogram. The histogram shows that the basic shape is a bellcurve.  
boxplot(data_burnout$BurnoutScore) #create a boxplot. The boxplot shows that the first quartile is slightly below 3 and the 3rd quartile is slightly above 4 and there are some outliers at the upper end of the distribution.

#As you can see, all the charts show that the data is centered around 3-4, which agrees with our summary statistics (Median = 3.5, Mean = 3.4), the data are also slightly right-skewed, but basically the shape is normal, and the minimum of 1 and maximum of 7 are clearly identified.

#You can also create an easy bar chart for the categorical variables, by nesting a table() function within the barplot() function. 
barplot(table(data_burnout$gender))

#To save the images, you can go to the plots window in the bottom right and select "Export" then "Save as Image"

#Frequently, you are interested in providing descriptive statistics stratified by a second (or even third) variable. Lets first try this for a categorical variable. For the table function, we can simply seperate the variables by a comma: 
table(data_burnout$gender, data_burnout$BurnoutScore_groups) #calculate frequencies of gender 

#One nice feature of the table function is you can add labels if it is helpful. 
table(gender = data_burnout$gender, Burnt_Out = data_burnout$BurnoutScore_groups) #calculate frequencies of gender 

#Getting proportions requires just one more step. You simply nest the table function from above within the prop.table() function and then specify a margin (1 or 2) that is used to calculate the percentage. The Margin = 1 argument calculates row total percentages, and Margin = 2 calculates column total percentages. 

prop.table(table(gender = data_burnout$gender, Burnt_Out = data_burnout$BurnoutScore_groups), margin = 1) #calculate percent of men that are burnt out.

prop.table(table(gender = data_burnout$gender, Burnt_Out = data_burnout$BurnoutScore_groups), margin = 2) # calculate the percent of burnt out people that are men.

#You can add additional variables, as needed, simply by adding them to the list:
table(gender = data_burnout$gender, Burnt_Out = data_burnout$BurnoutScore_groups, LGBTQ = data_burnout$identity_lgbtq) #calculate the frequency of burnout by gender AND sexual orientation

#Now you should know how to (1) calculate descriptive statistics, (2) create simple charts and graphs, and (3) export data. Next time we will learn how to edit, create, and transform variables -- a process often referred to as "data cleaning"






