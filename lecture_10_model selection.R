#### LECTURE 10 ####

#So far, we have covered linear and logistic regression models and how to evaluate them. Now, our focus is on how to compare models to decide which one is better. We will also review some different ways to construct a model. These skills will add to those we've already learned to help you build better models by deciding what variables and combinations of variables you should include. 

#As always, lets start by reading in our burnout dataset:
setwd("C:\\Users\\Kiffer\\Google Drive\\Work\\2. Current Projects\\HSCI_410\\3. Lectures") #Set Working Directory
burnout_data <- read.csv(file = "burnout_data.csv") #Read in burnout_data

#Now lets learn how to compare linear regression models first. We will start by building three similar linear regression models using the BurnoutScore, age, income, and ethnicity variables. To start I am going to create a new dataset that removes missing observations that are missing any of the variables of interest. This ensures that all the models I am looking at are built with the same underlying data.
burnout_data_subset <- burnout_data[!is.na(burnout_data$BurnoutScore),]
burnout_data_subset <- burnout_data_subset[!is.na(burnout_data_subset$age),]
burnout_data_subset <- burnout_data_subset[!is.na(burnout_data_subset$income),]
burnout_data_subset <- burnout_data_subset[!is.na(burnout_data_subset$ethnicity),]
burnout_data_subset <- burnout_data_subset[!is.na(burnout_data_subset$BurnoutScore_groups),]

#Model 1 predicts burnout scores using age as the explanatory factor
model_1 <- lm(formula = BurnoutScore ~ age, data = burnout_data_subset) #Create a linear regression model object
summary(model_1) # get results for the model object

#Model 2 predicts burnout scores using age and income as the explanatory factors
model_2 <- lm(formula = BurnoutScore ~ age + income, data = burnout_data_subset) #Create a linear regression model object
summary(model_2) # get results for the model object

#Model 3 predicts burnout scores using age and ethnicity as the explanatory factors
model_3 <- lm(formula = BurnoutScore ~ age + ethnicity, data = burnout_data_subset) #Create a linear regression model object
summary(model_3) # get results for the model object

#To compare these models, we use something called a fit index, in particular we frequently rely on the Adjusted Information Criterion (AIC). AIC is not displayed by default. However, can be extracted using the AIC() function, as follows:

AIC(model_1, model_2, model_3)

#As you can see, model 3 has the lowest AIC, and is therefore the best fitting model of those reviewed. We can test whether these differences are statistically significant using the lrtest() function from the lmtest package. 
install.packages("lmtest")
library("lmtest")
lrtest(model_1, model_2)

#As you can see, the result shows a   p-value (0.006329). This means that adding the income variable to the model did lead to a significantly improved fit over the model 1.

#Model 1 and model 2 can be compared using the lrtest() function, only when they are nested (meaning that model 2 contains all the variables that model 1 does, plus some additional ones). In our example model 1 is tested inside model 2. However, model 2 is not nested inside model 3, because model 3 does not include ethnicity. Therefore we could compare model 3 to model 1 using lrtest, because they are nested:

lrtest(model_1, model_3)

#However, to compare model 2 and model 3, we need to use the coxtest() function, also from the lmtest package
coxtest(model_2, model_3)

#As you can see, the p-value is statistically significant, suggesting that the difference in model fit is statistically significant. Combine this with what you know about the AIC, and you can tell that model_3 is a better fit than model_2. 

#As models become more and more complex, we can also use functions that automate the variable selection based on the AIC value. One such function is the step() function. Lets build ourselves a complex model and test the step function out:
linear_model_complex <- lm(formula = BurnoutScore ~ age + gender + ethnicity + income + educational_attainment + identity_lgbtq + identity_disability + lonely_direct + work_dignity + work_support + discrimination_score, model = TRUE, data = burnout_data)
summary(linear_model_complex)

step(linear_model_complex, direction = "backward") #backwards selection of variables

#The following output is the last step of the backwards elimination procedure. Backwards elimiation begins with all variables listed in the model, then removes the least significant variables until AIC in minimized. You can see here that age, gender, educational_attainment, identity_lgbtq, identity_disability, lonely_direct, work_dignity, work_support, discrimination_score are retained while ethnicity and income are removed. 
#                           Df Sum of Sq  RSS      AIC
# <none>                                 941.39 -275.447
# - identity_lgbtq          1     2.025  943.42 -274.800
# - work_dignity            4     7.686  949.08 -273.429
# - work_support            4     8.544  949.94 -272.316
# - gender                  2    10.035  951.43 -266.384
# - educational_attainment  8    19.863  961.26 -265.723
# - age                     1     9.495  950.89 -265.084
# - identity_disability     1    15.564  956.96 -257.245
# - discrimination_score    1    35.727  977.12 -231.557
# - lonely_direct           4   276.194 1217.59   33.503

#So for our final model, we can simply build the model with these variables

final_linear_model <- lm(formula = BurnoutScore ~ age + gender + educational_attainment + identity_lgbtq + identity_disability + lonely_direct + work_dignity + work_support + discrimination_score, model = TRUE, data = burnout_data) #create final multivariable model

library(broom) #load in broom package to create tidy table
table <- tidy(final_linear_model, conf.int = TRUE, conf.level = 0.95) #create and save table using tidy() function
View(table) #open table

#Lets now look at model fit for a binomial logistic regression models: To start we will create three binomial logistic regression models:

#Model 1 predicts burnout score groups using age as the explanatory factor
bi_model_1 <- glm(formula = BurnoutScore_groups ~ age, data = burnout_data_subset, family = "binomial") #Create a linear regression model object
summary(bi_model_1) # get results for the model object

#Model 2 predicts burnout score groups using age and income as the explanatory factors
bi_model_2 <- glm(formula = BurnoutScore_groups ~ age + income, data = burnout_data_subset, family = "binomial") #Create a linear regression model object
summary(bi_model_2) # get results for the model object

#Model 3 predicts burnout score groups using age,  ethnicity, and Loneliness scores as the explanatory factors
bi_model_3 <- glm(formula = BurnoutScore_groups ~ age + ethnicity, data = burnout_data_subset, family = "binomial") #Create a linear regression model object
summary(bi_model_3) # get results for the model object

#The AIC Scores can again be extracted (though you may have noticed they are provided in the default output of the glm() function)
AIC(bi_model_1, bi_model_2, bi_model_3)

#Model_1 in this case shows the lowest AIC score. However, lets use lrtest() to compare the nested models and coxtest to compare the non-nested models:
lrtest(bi_model_1, bi_model_2)
lrtest(bi_model_1, bi_model_3)
coxtest(bi_model_2, bi_model_3)

#Here we see that both models 2 and 3 are statistically better than model 1 (and model 3 is better than 2), despite model 1 having the lowest AIC. However, we can see that the p-values comparing model 1 to the other two models are not very  small. This demonstrates that model fit is complex. On one hand the lrtest() and coxtest() are saying "choose model 3" and the AIC() values are saying "choose model 1". Its important to know that the AIC penalizes model complexity -- therefore trying to strike a good balance between model fit and model simplicity. What we know from reading all these results together, plus looking at model coefficients, is that the contributions of gender and ethnicity alone do not necessarily justify a more complex model. Thats not saying that a more complex model can't be used, just that the evidence we have is conflicting and not very strong one way or another. For the sake of exploration, lets create a fourth model, combining age, gender, and ethnicity and compare the aic values and conduct a liklihood ratio test using the lrtest()

bi_model_4 <- glm(formula = BurnoutScore_groups ~ age + gender + ethnicity, data = burnout_data_subset, family = "binomial") #Create a linear regression model object
summary(bi_model_4) # get results for the model object

AIC(bi_model_1, bi_model_4) #compare AIC

lrtest(bi_model_1, bi_model_4) #compare lrtest()

#Here we see that the full model is improved compared to the age only model. 


#Lets now build some multinomial logistic regresison models using the multinom() function from the nnet package and use the AIC function to compare:
install.packages("nnet") #install package
library("nnet") #load library for package

#Age only model
multinom_model_1 <- multinom(formula = lonely_change_covid ~ age, model = TRUE, data = burnout_data)
summary(multinom_model_1)

#Age gender model
multinom_model_2 <- multinom(formula = lonely_change_covid ~ age + gender, model = TRUE, data = burnout_data)
summary(multinom_model_2)

#Age ethnicity model
multinom_model_3 <- multinom(formula = lonely_change_covid ~ age + ethnicity, model = TRUE, data = burnout_data)
summary(multinom_model_3)

#test AIC
AIC(multinom_model_1, multinom_model_2, multinom_model_3) #compare AIC

#Here we see that the second model has the lowest AIC

#Lets use lrtest() to compare the nested models:
lrtest(multinom_model_1, multinom_model_2)
lrtest(multinom_model_1, multinom_model_3)

#In the case of the non-nested models, there is no function readily available to asess relative fit of non-nested multinomial logistic regression models. As such, we rely on the AIC comparisons and conclude that model 2 is preferable. However, we can turn any non-nested model into a nested model and test whether adding a specific variable enhances the model overall. Lets give that a try:

multinom_model_4 <- multinom(formula = lonely_change_covid ~ age + gender + ethnicity, model = TRUE, data = burnout_data)
summary(multinom_model_4)
lrtest(multinom_model_3, multinom_model_4)

#Here we see that adding the variable is beneficial. Note we could also run the lrtest by calling specific variables:
lrtest(multinom_model_4, "gender")
lrtest(multinom_model_4, "ethnicity")
lrtest(multinom_model_4, "age") 

#Lets go through this one more time, with ordinal logistic regression models created using the polr() function. 

#Convert out outcome to an ordinal variable
burnout_data$lonely_direct <- ordered(burnout_data$lonely_direct, levels = c("None of the time (e.g., 0 days)", "Rarely (e.g. less than 1 day)", "Some or a little of the time (e.g. 1-2 days)", "Occasionally or a moderate amount of time (e.g. 3-4 days)", "All of the time (e.g. 5-7 days)]")) #Convert to factor and specify order

table(burnout_data$lonely_direct) #call descriptives; As you can see now, the variable is now ordered. We can construct an ordinal logistic regresison using the polr() function from the MASS package

install.packages("MASS") #install package
library("MASS") #load library for package

ord_model_1 <- polr(formula = lonely_direct ~ age, Hess = TRUE, data = burnout_data)
summary(ord_model_1) 

ord_model_2 <- polr(formula = lonely_direct ~ age + gender, Hess = TRUE, data = burnout_data)
summary(ord_model_2) 

ord_model_3 <- polr(formula = lonely_direct ~ age + ethnicity, Hess = TRUE, data = burnout_data)
summary(ord_model_3) 

AIC(ord_model_1, ord_model_2, ord_model_3) #Check AIC, see that model 2 has the lowest; model 2 is better
lrtest(multinom_model_1, multinom_model_2) #Check nested comparison of 1 and 2; 2 is better
lrtest(multinom_model_1, multinom_model_3) #Check nested comparison of 1 and 3; 3 is better
#Again, comparing the non-nested models is difficult, but the AIC is pretty good and you could create a nested model or check to see if the additional variable is worth it. 

#Note that step() also works with the non-linear models. 

#In summary, the liklihood ratio test and AIC are useful tests to compare models, particularly when they are nested. Considering these in context of theory, model accuracy/goodness of fit, coefficients, and r-squared measures can help you choose between models in order to ensure the results you present are accurate and percise. Balancing model fit and model complexity is critical to creating useful models of the real world. Next time we will look at other ways of improving your model by integrating theoretical and statistical approaches, such as mediation, interaction, and stratification. 



