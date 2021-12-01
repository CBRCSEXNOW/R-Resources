#### LECTURE 4 ####

#So far in this course, we've learned how to read in datasets and calculate descriptive statistics. This lecture focuses on data cleaning. We will learn how to (1) edit and combine variable levels, (2) convert a numeric variable into a categorical one, (3) combine multiple variables into a single variable, and (4) calculate scale scores.

#To get started, lets read in the burnout dataset we created last week. 
setwd("C:\\Users\\Kiffer\\Google Drive\\Work\\2. Current Projects\\HSCI_410\\3. Lectures") #Set Working Directory
burnout_data <- read.csv(file = "burnout_data.csv") #Read in burnout_data

#Now that the data is read in, lets take a look at the intersection of two variables: Gender and Relationship status. Remember we can use the table() to create a cross tab.

table(burnout_data$gender, burnout_data$relationship_status) #cross tab for gender and relationship status

#The function above presents these variables together. But imagine that we wanted to analyze these data as a single variable with four levels: Men in a relationship, women in a relationship, single men, and single women. To do this we would have to edit and combine the variable. Lets start by simplifying the relationship status variable. To do so, we will use the [] symbol to tell R that we want to conditionally recode the data. The first step, however, is to create a new variable, as we never want to over-write our data in case we must make edits later. I will create an empty variable called rel_di in the burnout_data dataframe:

burnout_data$rel_di <- NA #Create empty variable

#Now that I have the rel_di variable created, I will fill it conditionally based on the old variable, as follows:

burnout_data$rel_di[burnout_data$relationship_status == "Single and dating"] <- "Single" #Recode Single and dating as just single
burnout_data$rel_di[burnout_data$relationship_status == "Single and not dating"] <- "Single" #Recode Single and not-dating as just single
burnout_data$rel_di[burnout_data$relationship_status == "In a relationship"] <- "Not Single" #Recode In a relationship as not single
table(burnout_data$rel_di, useNA = "always") #Check to make sure it worked

#As you can see above, I used the == sign to tell R that where the old relationship_status was equal to "Single and dating", I wanted to assign "Single" to the new variable. The brackets allowed me to create that conditional recode. #I could have also coded the data using the != symbols, which indicates "Does not equal." Lets give that a try:

burnout_data$rel_di_version_two <- NA #Create empty variable
burnout_data$rel_di_version_two[burnout_data$relationship_status != "In a relationship"] <- "Single" #Recode "Single and not dating" and "Single and dating" as "single"
burnout_data$rel_di_version_two[burnout_data$relationship_status == "In a relationship"] <- "Not Single" #Recode "In a relationship" as "not single"

#Lets compare these two versions:
table(burnout_data$rel_di_version_two, burnout_data$rel_di, useNA = "always") #Compare to new variables

#Note that the values that were originally NA were ignored, this is because we use a special function to deal with missing data: The is.na() function. Here is how it works

burnout_data$rel_di_version_three[is.na(burnout_data$relationship_status)] <- "9999: Missing" #Assign NA the "9999: Missing" value

table(burnout_data$rel_di_version_three, useNA = "always") #Make sure it worked

#You can see that the NA's have now been reassigned using the is.na() function. I can also insert an ! point in from the is.na function, to specify when I want to recode every value that is not missing: 

burnout_data$rel_di_version_three[!is.na(burnout_data$rel_di_version_two)] <- "4444: Not Missing" #Assign all non missing variables as missing
table(burnout_data$rel_di_version_three) #Make sure it worked!

#Now that we have a simplified relationship status variable, lets create a new variable that combines relationship and gender:

burnout_data$rel_status_and_gender <- NA #Create an empty variable

#To combine variables we need the | symbol and the & symbol. | indicates "or" and & indicates "and". We can use these to stack conditions as we recode: 
burnout_data$rel_status_and_gender[burnout_data$gender == "Man" & 
                                     burnout_data$rel_di == "Single"] <- "Single Men" #Create Single Men level

burnout_data$rel_status_and_gender[burnout_data$gender == "Woman" & 
                                     burnout_data$rel_di == "Single"] <- "Single Women" #Create Single Women level

burnout_data$rel_status_and_gender[burnout_data$gender == "Non-binary" & 
                                     burnout_data$rel_di == "Single"] <- "Single Non-binary folk" #Create Single Women level

burnout_data$rel_status_and_gender[burnout_data$gender == "Man" & 
                                     burnout_data$rel_di == "Not Single"] <- "Non-single Men" #Create Single Men level

burnout_data$rel_status_and_gender[burnout_data$gender == "Woman" & 
                                     burnout_data$rel_di == "Not Single"] <- "Non-single Women" #Create Single Women level

burnout_data$rel_status_and_gender[burnout_data$gender == "Non-binary" & 
                                     burnout_data$rel_di == "Not Single"] <- "Non-single Non-binary folk" #Create Single Women level

table(burnout_data$rel_status_and_gender) #Make Sure it worked

#We now have a variable that represents both relationship status and gender, lets take a look at the % of people who are burnt out in each category

prop.table(table(burnout_data$BurnoutScore_groups, burnout_data$rel_status_and_gender), margin = 2) #Cross tab burnout and gender/rel status

#You can see in this data that 24.7% of men in relationships, 29.5% of women in relationships, 38.1% of non-binary folk in relationships, 30.5% of single men, 38.7% of single women, and 48.4% of single non-binary folks were burnt out. 

#Now imagine, that you wanted to consider the effect that age was playing in burnout within these groups. To do so we would want to take a look at the age variable.
class(burnout_data$age) #Check class
summary(burnout_data$age) #Get summary

#As you can see the median age was 33, and the data are stored as an "integer" Lets say that based on this data, you wanted to know whether the patterns shown above differed for those older than 33 compared to those younger than 33. The first step we'd need to do was to create a new age variable. To do so, we can use the greater than (>), greater than or equal to (>=). less than (<) and less than or equal to (<=) symbols and conditionally recode the data, as follows:

burnout_data$age_di_at_33 <- NA #Create an empty variable
burnout_data$age_di_at_33[burnout_data$age >= 33] <- "33 years or older"
burnout_data$age_di_at_33[burnout_data$age < 33] <- "Younger than 33"
table(burnout_data$age_di_at_33, burnout_data$age) #make sure it worked

#Note that we could have created more groups if we had wanted to!, we could have broken down the data according to quartiles (Younger than 27, 27-32, 33-47, 48+):
burnout_data$age_quartiles <- NA #Create an empty variable
burnout_data$age_quartiles[burnout_data$age < 27] <- "Younger than 27"
burnout_data$age_quartiles[burnout_data$age >= 27 & 
                             burnout_data$age <= 32] <- "27-32"
burnout_data$age_quartiles[burnout_data$age >= 33 & 
                             burnout_data$age <= 47] <- "33-47"
burnout_data$age_quartiles[burnout_data$age >= 48] <- "48+"
table(burnout_data$age_quartiles, burnout_data$age) #make sure it worked

#Lets look at the cross tabs between these variables and the other two variables we have been working with:

table(burnout_data$age_quartiles, burnout_data$rel_status_and_gender) #Cross tab burnout and gender/rel status
table(burnout_data$age_di_at_33, burnout_data$rel_status_and_gender) #Cross tab burnout and gender/rel status
table(burnout_data$age_quartiles, burnout_data$BurnoutScore_groups) #Cross tab burnout and gender/rel status
table(burnout_data$age_di_at_33, burnout_data$BurnoutScore_groups) #Cross tab burnout and gender/rel status

#You can see that for the four level age breakdown, some of the cell counts get quite small when looking at gender, so preceding with the two level age variable might be better (though sample sizes for the non-binary folk are still quite small).

#To now look at burnout groups among those under and over 33, we can use the [] to conditionally select the data where 

prop.table(table(burnout_data$BurnoutScore_groups[burnout_data$age_di_at_33 == "33 years or older"], 
                 burnout_data$rel_status_and_gender[burnout_data$age_di_at_33 == "33 years or older"]), margin = 2) #older xtab
prop.table(table(burnout_data$BurnoutScore_groups[burnout_data$age_di_at_33 == "Younger than 33"], 
                 burnout_data$rel_status_and_gender[burnout_data$age_di_at_33 == "Younger than 33"]), margin = 2) #younger xtab

#Looking at these data, one thing that sticks out to me is that the proportion of non-single non-binary folk who are burnt out nearly doubles when looking at those younger than 33 compared to those older than 33. To make sure this data, makes sense, it's always a good idea to double check the underlying frequencies:

table(burnout_data$BurnoutScore_groups[burnout_data$age_di_at_33 == "33 years or older"], 
                 burnout_data$rel_status_and_gender[burnout_data$age_di_at_33 == "33 years or older"]) #older xtab
table(burnout_data$BurnoutScore_groups[burnout_data$age_di_at_33 == "Younger than 33"], 
                 burnout_data$rel_status_and_gender[burnout_data$age_di_at_33 == "Younger than 33"]) #younger xtab

#As you can see from this quality check, it seems the jump in burnout is based on only a few observations. Its always important to be aware that when you're slicing up data in this way that it can create small sample sizes that make data analyses less reliable. 

#So far, we've covered how to edit variables. One of the common reasons that you have to edit variables, is when you are trying to calculate scale scores. Many of the variables included in this dataset consist of scale items, that need to be calculated to create scale scores. In most cases, I have already done that for the CSCS data, but nevertheless, many datasets that you might work with will not already have these scores calculated, so it is important to learn how to do. 

#Lets recreate the burnout score. The burnout scale consists of 10 items:               
# "burnout_tired"
# "burnout_disappointed"
# "burnout_hopeless"
# "burnout_trapped"
# "burnout_helpless"
# "burnout_depressed"
# "burnout_sick"
# "burnout_worthless"
# "burnout_difficulty_sleeping"
# "burnout_had_it"
#If you look at these variables, they are scores from "never" to "always":
table(burnout_data$burnout_tired) #check 

#In order to create a scale score, I will need to recode these variables as numeric variables and the sum their values together to create a final scale score. To do so, I will recode the values using the same variable editing skills taught above:
burnout_data$burnout_tired[burnout_data$burnout_tired == "Never"] <- 1 #Assign Never a value of 1
burnout_data$burnout_tired[burnout_data$burnout_tired == "Almost never"] <- 2  #Assign Almost never a value of 2
burnout_data$burnout_tired[burnout_data$burnout_tired == "Rarely"] <- 3  #Assign Rarely a value of 3
burnout_data$burnout_tired[burnout_data$burnout_tired == "Sometimes"] <- 4  #Assign Sometimes a value of 4
burnout_data$burnout_tired[burnout_data$burnout_tired == "Often"] <- 5  #Assign Often a value of 5
burnout_data$burnout_tired[burnout_data$burnout_tired == "Very Often"] <- 6  #Assign Very Often a value of 6
burnout_data$burnout_tired[burnout_data$burnout_tired == "Always"] <- 7  #Assign Always a value of 7
table(burnout_data$burnout_tired) #Make sure it worked

#As you can see, this would take a few lines of code to do for each of the 10 variables. To make this go a little bit faster, I am going to use the mutate_at, vars, and case_when functions. The intricacies of this code are beyond the scope of this course, but I will provide comments on each line to help you follow. 
library(dplyr)
burnout_data <- mutate_at(.tbl = burnout_data, #Use the mutate function to recreate the burnout data
                  .vars = vars(burnout_disappointed:burnout_had_it), #Edit the vars in the dataset from "burnout_disappointed" to "burnout_had_it"
                  .funs = ~case_when(. == "Never" ~ "1", #When the old variable says "Never", assign a value of 1
                                     . == "Almost never" ~ "2", #When the old variable says "Almost never", assign  a value of 2
                                     . == "Rarely" ~ "3", #When the old variable says "Rarely", assign  a value of 3
                                     . == "Sometimes" ~ "4", #When the old variable says "Sometimes", assign  a value of 4
                                     . == "Often" ~ "5", #When the old variable says "Often", assign  a value of 5
                                     . == "Very Often" ~ "6", #When the old variable says "Very Often", assign  a value of 6
                                     . == "Always" ~ "7", #When the old variable says "Always", assign  a value of 7
                                     TRUE ~ .)) #Everywhere else just leave it whatever it was in the old dataset

table(burnout_data$burnout_disappointed, useNA = "always") #Check the first variable to make sure it worked

##Don't worry, you wont be tested on the intricacies of the mutate_at function, but as you can see, there is usually an easier more efficient way to do something in R. In this course, our goal is to teach the most straightforward way. But Learning new things is a great way to make your R coding experience better.

#The next step in calculating the scale scores is to add the individual variable scores together. Before we do this, lets make sure they are numeric variables (remember they were originally stored as characters)

class(burnout_data$burnout_disappointed) #check class
burnout_data$burnout_tired <- as.numeric(burnout_data$burnout_tired) #recode as numeric
burnout_data$burnout_disappointed <- as.numeric(burnout_data$burnout_disappointed) #recode as numeric
burnout_data$burnout_hopeless <- as.numeric(burnout_data$burnout_hopeless) #recode as numeric
burnout_data$burnout_trapped <- as.numeric(burnout_data$burnout_trapped) #recode as numeric
burnout_data$burnout_helpless <- as.numeric(burnout_data$burnout_helpless) #recode as numeric
burnout_data$burnout_depressed <- as.numeric(burnout_data$burnout_depressed) #recode as numeric
burnout_data$burnout_sick <- as.numeric(burnout_data$burnout_sick) #recode as numeric
burnout_data$burnout_worthless <- as.numeric(burnout_data$burnout_worthless) #recode as numeric
burnout_data$burnout_difficulty_sleeping <- as.numeric(burnout_data$burnout_difficulty_sleeping) #recode as numeric
burnout_data$burnout_had_it <- as.numeric(burnout_data$burnout_had_it) #recode as numeric
class(burnout_data$burnout_disappointed) #make sure it worked

#Now to add these numeric variables together, we can apply mathmatical transformations (e.g., +, -, *, /).
burnout_data$NewScore <- burnout_data$burnout_tired +
  burnout_data$burnout_disappointed +
  burnout_data$burnout_hopeless +
  burnout_data$burnout_trapped +
  burnout_data$burnout_helpless +
  burnout_data$burnout_depressed +
  burnout_data$burnout_sick +
  burnout_data$burnout_worthless +
  burnout_data$burnout_difficulty_sleeping +
  burnout_data$burnout_had_it
summary(burnout_data$NewScore) #check to make sure it worked
summary(burnout_data$BurnoutScore) #compare with old score
#As you can see there is one big difference between the old score and the new score. The new score is 10 times bigger! This is because the burnout inventory is not actually calculated as a sum of the individual items, it is an average of score items. This is an easy fix, we simply divide by 10!
burnout_data$NewScore  <- burnout_data$NewScore / 10 #Calculate average of scale items, by dividing by number of items
summary(burnout_data$NewScore) #check to make sure it worked

#While not shown here, you can use other mathmatical functions, such as exp() to exponentiate, sum() to add values together, sqrt() to take the square root, mean() to get the average.
?exp
?sum
?sqrt
?mean

#In review,  we learned how to (1) edit and combine variable levels, (2) convert a numeric variable into a categorical one, (3) combine multiple variables into a single variable, and (4) calculate scale scores. Next time we will dive into understanding how to conduct correlation tests so we can begin understanding the relationships between variables. 

