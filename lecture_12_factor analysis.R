#### LECTURE 12 ####

#This lecture focuses on Factor Analysis, a method designed to uncover the relationships between a given set of variables. It looks for a smaller set of underlying or latent constructs that can explain the relationships among the observed or manifest variables. By the end of this lecture, you should be able to conduct a factor analysis and understand some scenarios in which it might be useful to conduct one. 

#Lets start today by reading in our CSCS_dataset
setwd("C:\\Users\\Kiffer\\Google Drive\\Work\\2. Current Projects\\HSCI_410\\3. Lectures") #Set Working Directory
burnout_data <- read.csv(file = "burnout_data.csv", na.strings = c("", "NA")) #Read in burnout_data

# First,  we need to convert the items into numeric variables. 
burnout_data <- mutate_at(.tbl = burnout_data, #Use the mutate function to recreate the dataset
                                      .vars = vars(burnout_tired:burnout_had_it), #Identify the vars you want to edit
                                      .funs = ~case_when(. == "Always" ~ "7", #recode level
                                                         . == "Very Often" ~ "6", #recode level
                                                         . == "Often" ~ "5", #recode level
                                                         . == "Sometimes" ~ "4", #recode level
                                                         . == "Rarely" ~ "3", #recode level
                                                         . == "Almost never" ~ "2", #recode level
                                                         . == "Never" ~ "1", #recode level
                                                         TRUE ~ .)) #Everywhere else just leave it whatever it was in the old dataset

burnout_data$burnout_tired <- as.numeric(burnout_data$burnout_tired)
burnout_data$burnout_disappointed <- as.numeric(burnout_data$burnout_disappointed)
burnout_data$burnout_hopeless <- as.numeric(burnout_data$burnout_hopeless)
burnout_data$burnout_trapped <- as.numeric(burnout_data$burnout_trapped)
burnout_data$burnout_helpless <- as.numeric(burnout_data$burnout_helpless)
burnout_data$burnout_depressed <- as.numeric(burnout_data$burnout_depressed)
burnout_data$burnout_sick <- as.numeric(burnout_data$burnout_sick)
burnout_data$burnout_worthless <- as.numeric(burnout_data$burnout_worthless)
burnout_data$burnout_difficulty_sleeping <- as.numeric(burnout_data$burnout_difficulty_sleeping)
burnout_data$burnout_had_it <- as.numeric(burnout_data$burnout_had_it)


#We will work with a single scale, called the ten item personality inventory, which includes the following variables:
burnout_vars <- c("burnout_tired",
               "burnout_disappointed",
               "burnout_hopeless",
               "burnout_trapped",
               "burnout_helpless",
               "burnout_depressed",
               "burnout_sick",
               "burnout_worthless",
               "burnout_difficulty_sleeping",
               "burnout_had_it")

#Lets create a dataframe with only these vars:

burnout_data_scale_items <- burnout_data[, names(burnout_data) %in% burnout_vars] #remove unnecessary columns
burnout_data_scale_items <- na.omit(burnout_data_scale_items) #remove observations with missing data

# Now that our dataset is ready for analysis, we should first assess the internal consistency of our scale items. We can use Chronbach's alpha to do this. Chronbach's alpha tells us how closely related a set of items in a group are. The alpha() function in the psych package can be used to assess internal consistency:

library("psych")
alpha(burnout_data_scale_items)
#As you can see the Cronbach's alpha is very high at 0.91, which indicates excellent internal consistency:

# Cronbach's alpha	Internal consistency
# 0.9 ??? ??	            Excellent
# 0.8 ??? ?? < 0.9	      Good
# 0.7 ??? ?? < 0.8	      Acceptable
# 0.6 ??? ?? < 0.7	      Questionable
# 0.5 ??? ?? < 0.6	      Poor
# ?? < 0.5	            Unacceptable

#You can also look under the item statistics at the following statistics:
#n = number of complete cases
#raw.r = correlation of each item with the total score, not corrected for item overlap
#std.r = standardized correlation with total score, not correct for item overlap
#r.cor = correlation of each item, corrected for item overlap and scale reliability. 
#r.drop = correlation of each item with total score, not including the item
#mean = mean 
#sd = standard deviation

#This tells us that all the items in the scale are highly correlated (and that being hopeless and depressed are the items most correlated with total scores). However, Cronbach's alpha does not mean that the items are unidimensional. Since the Burnout Measure is a single scale, we assume it represents a single factor. Therefore, lets construct a factor model, with 1 factor using the factanal() function.  

factanal(x = burnout_data_scale_items, factors = 1, rotation="varimax")

#The output from the factanal() function is robust: The first set of results deal with uniqueness. Uniqueness is the variance that is 'unique' to the variable and not shared with other variables. A variable with high uniqueness is not explained well by the factor model. A variable with a low uniqueness is explained well by the factor model. Based on this we can see that hopelessness, helplessness, being trapped, and being depressed are expalined well by the factor model, while being tired, dissapointeded, and not sleeping well are less so described by the variable. 

#The next group of output is the factor loadings. These numbers describe how well each variable maps onto a single factor. Since we only have one factor, we see that all variables map onto it. Again we see that hopelessness, trapped, helplessness, and depression map well onto the one factor model. 

#In the next section, we have SS loadings. Factors with SS loadings greater than 1 indicate they should be kept. The Proportion Var variable indicates the proportion of variance explained by each factor. If there were multiple factors, we would also see a row for Cumulative Variance, which would indicate the variance of each additional factor. You want these values to be high as they tell you the variance in the data that is explained by the factor model. 

#Finally, at the bottom is a hypothesis test. This tests whether 1 factor model sufficiently characterizes the variance in the data. A significant p-value suggests that the model is NOT sufficient. To address this we can turn to the fa.parallel() function in the psych package

psych::fa.parallel(x = burnout_data_scale_items) #Create a scree plot. The point in the scree plot where things clearly start to level off (i.e., the elbow) indicates the number of factors that you should use. The function also runs a parallel analysis tells you the optimal numbers of factors (in this case 3).

#Given this test, lets run a factor analysis with three factors:
factanal(x = burnout_data_scale_items, factors = 3, rotation="varimax")

#In the new output, we see considerbly lower uniqueness scores. In the factor loadings, we see the first factor relates to being hopeless, trapped, and helpless. The second factor represents being tired and dissapointed. The third factor relates to being sick and having difficulty sleeping. 

#Judging by the SS loadings, we should keep all three factors, and a higher ammount of variance is explained (62%). Technically speaking, the p-value remains insignificant, suggesting we could look at the larger factors, but we should have enough support from our parallel analysis and scree plot to decide that we've gone far enough. That said, lets take a look at the other options.

factanal(x = burnout_data_scale_items, factors = 4, rotation="varimax") #p-value = 0.0004
factanal(x = burnout_data_scale_items, factors = 5, rotation="varimax") #p-value = 0.0385
factanal(x = burnout_data_scale_items, factors = 6, rotation="varimax") #p-value test satisfied

#As we can see, it takes six factors to satisfy the p-value requirement. However, one reason for this is that we have quite a large dataset. As sample size increases, it is hard to have a "good fitting" model. I personally, would rely on the results of the fa.parallel() analysis, however the decision is subjective. 

#Given our factor analysis, we could choose to create three new subscales for burnout, assigning each variable to its highest scoring factor:

burnout_data$hope_trapped_helpless_subscale <- burnout_data$burnout_hopeless + burnout_data$burnout_trapped + burnout_data$burnout_helpless + burnout_data$burnout_depressed + burnout_data$burnout_worthless + burnout_data$burnout_had_it

burnout_data$tired_disappointed_subscale <- burnout_data$burnout_tired + burnout_data$burnout_disappointed

burnout_data$sick_subscale <- burnout_data$burnout_sick + burnout_data$burnout_difficulty_sleeping

burnout_data$burnout_scale <- burnout_data$burnout_sick + 
  burnout_data$burnout_difficulty_sleeping + 
  burnout_data$burnout_hopeless + 
  burnout_data$burnout_trapped + 
  burnout_data$burnout_helpless + 
  burnout_data$burnout_depressed + 
  burnout_data$burnout_worthless + 
  burnout_data$burnout_had_it + 
  burnout_data$burnout_tired + 
  burnout_data$burnout_disappointed

#Now we can test correlations with the full subscale: 
cor.test(x = burnout_data$burnout_scale, burnout_data$hope_trapped_helpless_subscale)
cor.test(x = burnout_data$burnout_scale, burnout_data$tired_disappointed_subscale)
cor.test(x = burnout_data$burnout_scale, burnout_data$sick_subscale)

#Clearly the mental manifestations subscales had the highest correlation: r = 0.961, followed by the the exhaustion subscale (r = 0.809), followed by the physical manifestations subscale (r = 0.759) 

#With these new scales, we could examine which part of the scale items are most stronly impacted by loneliness (a variable we've seen pop up again and again as being important to burnout):

model <- lm(formula = UCLA3Score ~ sick_subscale + tired_disappointed_subscale + hope_trapped_helpless_subscale, data = burnout_data) #include subscales as explanatory factors for UCLA loneliness scores
summary(model) #summarize model
varImp(model) #check variable importance 

##From the above, we see that loneliness is especially related to exhaustion and mental manifestations of burnout and not the physical manifestations of burnout. This sort of analysis illustrates why it may be sometimes helpful to identify individual factors from a larger scale. 

#We could also use the information from a factor analysis  to create a shortened version of the scale, making sure to select the one or two most improtant variables for each factor. I'll choose an arbitrary cut point of 0.60 to measure inclusion:

burnout_data$short_burnout <- burnout_data$burnout_trapped + burnout_data$burnout_tired + burnout_data$burnout_sick

cor.test(x = burnout_data$burnout_scale, y = burnout_data$short_burnout)

#As you can see, the new 3 item burnout measure is strongly correlated with the full burnout scale. Shortening a measure might be useful in instances where using the full scale would be too cumbersome (i.e., future research projects)

#We could also use this information to test whether a scale works the same way in different populations. For example, we might run a factor analysis on two seperate datasets representing peopel with and without disability:

#create dataset for people with disability
table(burnout_data$identity_disability)
burnout_data_disability <- burnout_data[burnout_data$identity_disability == "People with chronic health problems or disabilities (e.g., Living with any impairment, including a physical, mental, intellectual, cognitive, learning, communication or sensory impairments-or a functional limitation-whether permanent, temporary or episodic in nature, or evident or not, that, in interaction with a barrier, hinders a person's full and equal participation in society.)", ] #create dataset for people with disability
burnout_data_disability <- burnout_data_disability[, names(burnout_data_disability) %in% burnout_vars] #remove unnecessary columns
burnout_data_disability <- na.omit(burnout_data_disability) #remove observations with missing data


#create dataset for people without disability
burnout_data_no_disability <- burnout_data[burnout_data$identity_disability == "8888: Not Selected", ] 

burnout_data_no_disability <- burnout_data_no_disability[, names(burnout_data_no_disability) %in% burnout_vars] #remove unnecessary columns
burnout_data_no_disability <- na.omit(burnout_data_no_disability) #remove observations with missing data

#Check for number of factors
psych::fa.parallel(x = burnout_data_disability) # Parallel analysis Suggests 2 factors
psych::fa.parallel(x = burnout_data_no_disability)  #Parallel analysis  Suggests 3 factors

#Compare 3 factor models. 
disability_loading <- factanal(x = burnout_data_disability, factors = 3, rotation="varimax") 
#factor 1 = hopeless, depressed, and wortheless
#factor 2 = tired
#factor 3 = trapped
no_disability_loading <- factanal(x = burnout_data_no_disability, factors = 3, rotation="varimax")
#factor 1 = hopeless, trapped, helpless, worthless
#factor 2 = sick, difficulty sleeping
#factor 3 = tired, disappointed


#Factor loadings can be compared using the fa.congruence() function. 
fa.congruence(disability_loading$loadings, no_disability_loading$loadings)

#Compare 2 factor models. 
disability_loading <- factanal(x = burnout_data_disability, factors = 2, rotation="varimax")
disability_loading
#factor 1 = hopeless, depressed, and wortheless
#factor 2 = tired
#factor 3 = trapped
no_disability_loading <- factanal(x = burnout_data_no_disability, factors = 2, rotation="varimax")
#factor 1 = hopeless, trapped, helpless, worthless
#factor 2 = sick, difficulty sleeping
#factor 3 = tired, disappointed

#Factor loadings can be compared using the fa.congruence() function. 
fa.congruence(disability_loading$loadings, no_disability_loading$loadings)


#A congruence coefficient of 0.95 is interpreted as nearly identical, 0.90 as high similarity, and above 0.85 is a fair similarity. Below .85 is indication of bias. Results from our analysis show that factor 1 among people with disability is similar to both factors 1 and 2, factor 2 for PWD is similar to factor 3, and factor 3 is similar to factor 1. 

#Based on these differences, it is apparent that Burnout may represent a slightly different concept among people with disabilities. At the very least, we should  carefully look for differences in the performance of any Burnout scales we use when modeling. Furthermore, this may motivate us to model the effects of burnout using stratified models to see if the predictors of burnout are different in these two populations. In the present case, the two-item factors are more comparable for people with and without disabilities, therefore, we might be motivated to construct and use subscales representing two rather than three subscales -- perhaps one subscale reflecting the mental manifestations and second scale representing the physical manifestations. 
#In summary, factor analysis can be used when we don't want to take for granted the composition of the scales we are working with. It is a useful, albeit complex type of analysis. You can use it to shorten scales, identify subscale components, and test whether a scale works similarly in two different populations. 
#With that, you have now finished the core content for this course. You should be able to use R, load and clean data, conduct linear and logistic regression, select variables and identify the best fitting models, test for mediation and moderation, and validate the factor structure of scales you are working with. This is everything you need to know to pursue fundamental epidemiological research. 
