#### LECTURE 8 ####

#Last time, we showed you how to construct linear regression models. Logistic Regression models are another useful modeling approach. This time we will cover three types of logistic regresison models: binomial models, multinomial models, and ordinal models. 

#To get started with learning about logistic regression, lets read in our burnout dataset. 
setwd("C:\\Users\\Kiffer\\Google Drive\\Work\\2. Current Projects\\HSCI_410\\3. Lectures") #Set Working Directory
burnout_data <- read.csv(file = "burnout_data.csv") #Read in burnout_data

#Now lets explore the use of the glm() function, which is used to create a logsitic regression model. 

?glm #Call help function. As you can see the type of model you create is specified in the family argument. You may explore and persue information about these other types of models, but they are beyond the scope of this course. If you encounter a scenario in which you might need these models (e.g., you have count data and are therefore considering a Poisson model), you may want to consider restructuring the variable as binary or you can explore R tutorials about these other model types. Outside of linear and logistic regression models, the most commonly used models are Poisson models. For this exercise, we want to create a binomial logistic regression model, the kind used to analyze a binary outcome variable. To do so, I will use the categorical version of the Burnout Score as the outcome and the continuous version of the loneliness scale score as the explanatory factor:
burnout_data$BurnoutScore_groups <- as.factor(burnout_data$BurnoutScore_groups) #make sure outcome is a factor

glm_model <- glm(formula = BurnoutScore_groups ~ UCLA3Score, family = binomial(link = "logit"), data = burnout_data)
summary(glm_model)

#The coefficient here is interpreted as for each 1-point increase in the UCLA 3 loneliness score there is a 0.60 increase in the log odds of being burnt out. Now log odds are kind of a pain, so when epidemiologists work with logistic regression models, we typically convert the log odds to an odds ratio. To do this we simply exponentiate the estimate (coefficient) using the exp() function. To call the coefficients out of the model, we could use the coef() function or you could use model$coefficients:
exp((coef(glm_model))) #Use coef() function to call results, and exp() to convert to Odds Ratio
exp(glm_model$coefficients) #use $ to refer to coefficients element of object and exp() to convert to Odds Ratio

#We interpret the odds ratio as there being a 82.7% increase in odds of being burnt out for each 1 point increase in the UCLA 3 scores. It is also common to report an odds ratio along with a confidence interval to communicate uncertainty around your estimate. To calculate the confidence intervals, we can use the confint() function.

exp(confint(glm_model))

#You can use the cbind() function to save these results:

cbind(exp((coef(glm_model))), exp(confint(glm_model))) #Bind the columns from the coef() and confint() model

#You can also assign this as a dataframe and save the dataframe as a csv using the following code:

GLM_table <- cbind(exp((coef(glm_model))), exp(confint(glm_model))) #save cbind results as object
GLM_table <- as.data.frame(GLM_table) #convert object to data frame
write.csv(x = GLM_table, file = "GLM_table.csv") #Write dataset to csv on computer

# Now that you've got some results, you can assess the goodness of fit for your model, using a hosmer-Lemeshow goodness of test fit using the logitgof() function, which is part of the generalhoslem package. Note that Traditional residual plots are not very helpful with logistic regression.
install.packages("generalhoslem") #install package
library("generalhoslem") #load library for package

logitgof(obs = glm_model$y, fitted(glm_model)) #run hoslem test #notice The observed values are extracted from the outcome of the model and Then the expected values are calculated using the fitted() function. Because different objects have different features, each of the examples today gets to the observed and fitted values differently. 


#The results of this test have a p-value of 0.0709, indicating no evidence of poor fit (but very close). This tells you that your model has adequately acceptable goodness of fit to your data, but in our case it is borderline so you may want to consider a more complex model. 

#In addition to testing for goodness of fit, logistic regression are also frequently evaluated based on their predictive power, similar to how we used R-squared in linear regression. However, the R-squared does not work well in logistic regression, so we employ McFadden's Pseudo R-squared using the PseudoR2() function from the DescTools package:

install.packages("DescTools") #install package
library("DescTools") #load library for package

PseudoR2(glm_model, "all") #run McFadden's Pseudo R-squared; #All specifies we want all model statistics.

#The values for a pseudo R squared tend to be considerably lower than R squared; The creator for the statistic says that values of 0.2 to 0.4 indicates good fit. In our case our score is below that, so perhaps we need to revise our model and compare the Pseudo R2 of the old and new models to see if we can improve things?
glm_model_2 <- glm(formula = BurnoutScore_groups ~ gender + age + UCLA3Score + zimet_overall_score + discrimination_score + tipi_extraversion_score + tipi_agreeable_score + tipi_conscientiousness_score + tipi_emotional_stability_score + tipi_emotional_stability_score + tipi_openness_score + attachment_secure_score + attachment_avoidant_score + attachment_anxious_score, family = binomial(link = "logit"), data = burnout_data)
summary(glm_model_2)

PseudoR2(glm_model_2, "all") #rerun McFadden's Pseudo R-squared for model_2
PseudoR2(glm_model, "all") #rerun McFadden's Pseudo R-squared for model

#As you can the new model substantially improves over the other models. But how do we know which variables in our model are relatively more important? Simply adding variables to a model to increase its Pseudo R-squared seems like a dubious practice. I suggest using the varImp function, of the caret pacakge: 

install.packages("caret") #install package
library("caret") #load library for package

varImp(glm_model_2) #Run the varImp function on model 2, which provides the absolute value of the t-statistic for each model parameter. Variables with large absolute values are relatively more important. As we can see here, the loneliness scores and social support scores are the most important (similar to what we found when modelling our data as a linear regression model using the continuous burnout scores).

#Check multicolinearity
library("regclass") #load library for package
VIF(glm_model_2) #looks good no extremely high values

#Another thing you might want to do is to test model accuracy. The model accuracy is measured as the proportion of observations that have been correctly classified. It can be calculated using the predict() function. 
glm.probs <- predict(glm_model_2,type = "response") #Predict() assigns fitted model values for each participants based on the model
glm.pred <- ifelse(glm.probs > 0.5, "1", "0") #ifelse() is a function that Assigns 1 if prediction is above 50%, and 0 if prediction is below 50%
prop.table(table(glm.pred == glm_model_2$y)) #prop.table() function tells me that the correct assignment is made about 75.7% of the time (i.e., the predicted value == the actual value)

table(predicted = glm.pred, actual = glm_model_2$y) # This sounds good, but a coin flip has a 50-50 chance of being accurate. When can dive deeper, by constructing a table, you can see that 77 people who were burnt out were predicted by our model not to be (false negative); and 44 of those who were not burnt out were predicted to be (false positive). This suggests that your model predicts better than random, but its still not perfect. You don't need to search for a perfect model, but you should be cautious claiming that your model is good, when it misclassifies almost one in four participants. #later in this course we will discuss more about comparing models. Sometimes the best strategy is to just build a better model than you have, because no model is perfect. 

#Binary logistic regression is great when you have just two levels in your outcome. In our dataset, we have a variable that asked participants whether the pandemic had made them more lonely, less lonely, or whether they were about the same as they had always been. Binary logistic regression would not work on this variable because it has more than two levels. 

table(burnout_data$lonely_change_covid)

#As you can see most people felt somewhat (n = 964) or much (n = 523) more lonely. BUt sizeable numbers reported that their loneliness was about the same as it was before the pandemic. We might be curious what factors are associated with these experiences. Lets examine, whether women or men were more likely to report changes in their levels of loneliness. TO do this, we will run a multinomial logistic regression using the multinom() function from the nnet package. 
install.packages("nnet") #install package
library("nnet") #load library for package

multinom_model <- multinom(formula = lonely_change_covid ~ gender, model = TRUE, data = burnout_data)
summary(multinom_model)

#The output for this multinom model is somewhat hard to look at because we recieve a coefficient for every level of the outcome. The reference variable in this case is "About the same", which you could change using the relevel() function if you wanted to. We will leave it as is for now. Lets first use the tidy() function in the broom pacakge to convert the results to odds ratios (i.e., exponentiate = TRUE) and calculate confidence intervals (i.e., conf.int = TRUE, conf.level = 0.95, )
install.packages("broom") #install package
library("broom") #load library for package

tidy(multinom_model, conf.int = TRUE, conf.level = 0.95, exponentiate = TRUE) #Tidy of the results using the tidy function!!

#In these results, the "About the same" group is your referent, so all comparisons are against that group. So, compared to people who's loneliness did not change, people who were much more lonely had 85% higher odds of being a woman. 

#Lets do a conceptual check on this data by reviewing the descriptive: 

prop.table(table(burnout_data$lonely_change_covid, burnout_data$gender), margin = 1) #calculate % of each group who were each gender

#As you can see, 50.2% of those who were "about the same," were men compared to only about 36.1% of those who were much more lonely. We could also look at this data by gender:

prop.table(table(burnout_data$gender, burnout_data$lonely_change_covid), margin = 1) #calculate % of each group who were each gender

#Only 16.4% of men were much more lonely as aresult of the pandemic, compared to 26.7% of men. These descriptive statistics agree with the results found in the model! 

#We can also use the same functions above to check goodness of fit, the pseudo R squared, and identify the importance of variables

#Check goodness of fit
library("generalhoslem")
logitgof(obs = na.omit(burnout_data$lonely_change_covid), exp = fitted(multinom_model)) #notice I use the na.omit function here to restrict down the outcome variable to include only those that were present in the observed dataset na.omit() removes mising. Then the expected values are calculated using the fitted() function. 

#Check Pseudo R-squared
library("DescTools") #load package
PseudoR2(multinom_model, "all")

#Check variable importance (though, we only have one variable, so lets first build a model with age in it as well)
library("nnet") #load package
multinom_model_2 <- multinom(formula = lonely_change_covid ~ gender + age, model = TRUE, data = burnout_data) 
summary(multinom_model_2)

library("caret") #load package
varImp(multinom_model_2) #Check variable importance

#The third type of regression we want to cover today is an ordinal logistic regression model. This is useful when your outcome has a natural order to it. Usually your selected referent is either the highest or lowest level of the variable. The first step is to make sure your variable is saved as an ordered factor. Lets give this a try using the question that assesses how frequently participants feel lonely:
table(burnout_data$lonely_direct) #call descriptives

#As you can see, the variable levels are currently ordered alphabetically. We will need to reorder the variable using the ordered() function:
burnout_data$lonely_direct <- ordered(burnout_data$lonely_direct, levels = c("None of the time (e.g., 0 days)", "Rarely (e.g. less than 1 day)", "Some or a little of the time (e.g. 1-2 days)", "Occasionally or a moderate amount of time (e.g. 3-4 days)", "All of the time (e.g. 5-7 days)]")) #Convert to factor and specify order

table(burnout_data$lonely_direct) #call descriptives; As you can see now, the variable is now ordered. We can construct an ordinal logistic regresison using the polr() function from the MASS package

install.packages("MASS") #install package
library("MASS") #load library for package

ord_model <- polr(formula = lonely_direct ~ age + gender + lonely_change_covid + BurnoutScore, Hess = TRUE, data = burnout_data)
summary(ord_model) # Get a summary, which is hard to read, so convert to a tidy table using the broom package

library("broom") #load library for broom package
tidy(ord_model, conf.int = TRUE, conf.level = 0.95, pvalue = TRUE, exponentiate = TRUE) #Tidy of the results using the tidy function!!

#As you can see, in this model, we do not have to worry about multiple coefficients for each explanatory variable. The nice thing with ordinal regression is it gives us a single effect for each term. So we might interpret the effect of Burnout as "Each 1 point increase in burnout scores was associated with a 2.52 times (or 152%) increase in odds of being lonely more frequently. The other variable level with a significant effect was among those who said they were filling much more lonelier since the start of the pandemic: We would interpret this as "People who were much more lonelier since the start of the pandemic had 4.90 times (or 390%) higher odds of being lonely more frequently. 

#One assumption unique to an ordinal model is that no variable in your model has a disproportionate effect on a specific level of your outcome variable. To test for this assumption, we conduct the Brant-Wald test using the brant() function within the brant package. 
install.packages("brant") 
library("brant")
brant::brant(ord_model)

# A p-value of less than 0.05 on this test-particularly on the Omnibus plus at least one of the variables-should be interpreted as a failure of the proportional odds assumption. In this case, we would likely opt for a multinomial model or collapse our variable in a way that would make it suitable for a binary logistic regression model. 


#Check goodness of fit
library("generalhoslem")
logitgof(ord_model$model$lonely_direct, exp = fitted(ord_model), ord = TRUE) 

#Check Pseudo R-squared
library("DescTools") #load package
PseudoR2(ord_model, "all")

#Check variable importance
#Unfortunately the varImp() function does not have a method for dealing with a polr() objects, as such it cannot be used for ordinal regression. One option you have it to construct the model without each variable and then test the pseudo R squared on each, taking note of which variables have the biggest impact on values.
ord_model_no_age <- polr(formula = lonely_direct ~ gender + lonely_change_covid + BurnoutScore, Hess = TRUE, data = burnout_data) #construct without age

ord_model_no_gender <- polr(formula = lonely_direct ~ age + lonely_change_covid + BurnoutScore, Hess = TRUE, data = burnout_data) #construct without gender

ord_model_no_lonely_change_covid <- polr(formula = lonely_direct ~ age + gender + BurnoutScore, Hess = TRUE, data = burnout_data) #construct without lonely_change

ord_model_no_burnoutScore <- polr(formula = lonely_direct ~ age + gender + lonely_change_covid, Hess = TRUE, data = burnout_data) #construct without burnout

#Calculate PseudoR2 for each model:
PseudoR2(ord_model, "all") #Mcfadden = 0.1370127
PseudoR2(ord_model_no_age, "all")  #Mcfadden = 0.1366661
PseudoR2(ord_model_no_gender, "all")  #Mcfadden = 0.1369715
PseudoR2(ord_model_no_lonely_change_covid, "all")  #Mcfadden = 0.1100810
PseudoR2(ord_model_no_burnoutScore, "all")  #Mcfadden = 0.0635745

#We can now calculate the change in R2 from removing each variable

0.1370127 - 0.1366661 #ord_model_no_age = 0.0003466
0.1370127 - 0.1369715 #ord_model_no_gender = 0.0000412
0.1370127 - 0.1100810 #ord_model_no_lonely_change_covid = 0.0269317
0.1370127 - 0.0635745 #ord_model_no_burnoutScore = 0.0734382

#Here we can see that the biggest variable impact was on the removal of the burnout score. Again highlighting a strong connection between loneliness and burnout. Relatively small changes in the model were attributed to age and gender. This gives you a sense of what the most important variables in the model R. Technically, a similar approach could be used in other models. 

#THe other limitation of ordinal models is they do not work with your typical variable inflation factors. You can however check multicolienarity by converting your outcome to a numeric variable, constructing a linear regression model, and calculating the VIF scores to test for multicolinearity.

#Create numeric version of the outcome
burnout_data$lonely_direct <- as.character(burnout_data$lonely_direct)
burnout_data$lonely_direct_numeric[burnout_data$lonely_direct == "None of the time (e.g., 0 days)"] <- 1
burnout_data$lonely_direct_numeric[burnout_data$lonely_direct == "Rarely (e.g. less than 1 day)"] <- 2
burnout_data$lonely_direct_numeric[burnout_data$lonely_direct == "Some or a little of the time (e.g. 1-2 days)"] <- 3
burnout_data$lonely_direct_numeric[burnout_data$lonely_direct == "Occasionally or a moderate amount of time (e.g. 3-4 days)"] <- 4
burnout_data$lonely_direct_numeric[burnout_data$lonely_direct == "All of the time (e.g. 5-7 days)]"] <- 5

#Create a linear regression model
linear_model <- lm(formula = lonely_direct_numeric ~ age + gender + lonely_change_covid + BurnoutScore, data = burnout_data)

#Calculate VIF to measure multicolinearity
VIF(linear_model)

#This is a nice work-around that should satisfy. Since the VIF is really a function of inter-correlations in the design matrix (which doesn't depend on the dependent variable or the non-linear mapping from the linear predictor into the space of the response variable [i.e., the link function in a glm]).

#With that, You have now learned logistic regression. In the next lecture we will cover model building and variable selection for linear and logistic regression models. While you already have many of the tools needed to build a regression model, our next lecture will introduce a few more strategies to help you build a model.  



