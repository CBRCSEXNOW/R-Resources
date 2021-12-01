#### LECTURE 7 ####

#Last time we talked about correlation and this week's lecture will focus on linear regression. As you will see, linear regression is based on many of the same ideas as correlation. However, linear regression is advantageous because you can begin to work with multiple variables and you can mix variable types quite easily. Linear regression is used when you have a numeric outcome, such as the Burnout Score we have been working with.   

#To get started with linear regression, lets read in our burnout dataset. 
setwd("C:\\Users\\Kiffer\\Google Drive\\Work\\2. Current Projects\\HSCI_410\\3. Lectures") #Set Working Directory
burnout_data <- read.csv(file = "burnout_data.csv") #Read in burnout_data

#Now that we have our dataset, lets jump right in an build a linear regression model by creating an object called "model" using the lm() function.
?lm #Get information about the lm() function. 

#The main pieces of the lm model are the formula and dataset arguments. A formula is written using the ~ as an equals sign (because = and == are already used for other processes in R). When writing a formula, the variable on the left side of the ~ is your outcome. For a linear regression model it must be a numeric variable. The variables on the right side of the ~ are your explanatory factors. These are stratified by + signs. Lets give it a try to model whether burnout and gender are related:
model_1 <- lm(formula = BurnoutScore ~ gender, data = burnout_data) #Create a linear regression model object
summary(model_1) # get results for the model object

#As you can see, the model provides regression coefficients (etimate), standard errors, t-values, and p-values (pr(>|t|)). 

#The intercept is listed along with two coefficients. 

#P-values and coefficients in regression analysis work together to tell you which relationships in your model are statistically significant and the nature of those relationships. The coefficients describe the mathematical relationship between each independent variable and the dependent variable. The p-values for the coefficients indicate whether these relationships are statistically significant. A p-value less than 0.05 is typically considered to be statistically significant. The sign of a regression coefficient tells you whether there is a positive or negative correlation between each independent variable and the dependent variable. A positive coefficient indicates that as the value of the independent variable increases, the mean of the dependent variable also tends to increase. A negative coefficient suggests that as the independent variable increases, the dependent variable tends to decrease. The coefficient value signifies how much the mean of the dependent variable changes given a one-unit shift in the independent variable.

#You may have noticed that the third coefficient for men, is missing. This is because it is the referent level. You can change the referent level of a factor variable, as follows:
class(burnout_data$gender)
burnout_data$gender <- as.factor(burnout_data$gender)
burnout_data$gender <- relevel(x = burnout_data$gender, ref = "Woman")

#Now if we run the model again, you will see that "woman" is the referent level:
model_2 <- lm(formula = BurnoutScore ~ gender, data = burnout_data) #Create a linear regression model object
summary(model_2) # get results for the model object

#When selecting a referent, you should choose one that theoretically makes sense or sometimes if we don't particularly care, we just choose the largest group.

#In the model we just created our results show that for comparing men to women, burnout scores are -0.2209 points lower. We can compare this with the descriptive results, where there is a 0.2209 difference in the mean burnout scores. 

summary(burnout_data$BurnoutScore[burnout_data$gender == "Man"]) #Burnout Score summary is 3.297 for men
summary(burnout_data$BurnoutScore[burnout_data$gender == "Woman"]) #Burnout Score summary is 3.518 for woman
3.518-3.297 #Calculate difference in means for women and men
summary(burnout_data$BurnoutScore[burnout_data$gender == "Non-binary"]) #Burnout Score summary is 3.518 for woman
3.518-3.787  #Calculate difference in means for women and non-binary folk 

#As you can see, the coefficients bare a resemblance to the data. Its important to note however, that only the coefficient for men was statistically significant (p < 0.05), whereas the coefficient for non-binary folks was near, but not statistically significant (p > 0.05). This is probably because of the small sample size of non-binary folks. 

#The simple regression model can be used to control for confounding variables. Lets build a multivariable regression model controlling for age, loneliness, social support, discrimination, and other personality traits we worked with last week:

model_3 <- lm(formula = BurnoutScore ~ gender + age + UCLA3Score + zimet_overall_score + discrimination_score + tipi_extraversion_score + tipi_agreeable_score + tipi_conscientiousness_score + tipi_emotional_stability_score + tipi_emotional_stability_score + tipi_openness_score + attachment_secure_score + attachment_avoidant_score + attachment_anxious_score, data = burnout_data) #Create a linear regression model object
summary(model_3) # View Results

# Regression coefficients in this model are interpreted independently -- they assume all other variables in the model are held constant. This property of holding the other variables constant is crucial because it allows you to assess the effect of each variable in isolation from the others. We would interpret these findings as suggesting that burnout is a function of loneliness, and social support scores and personality traits -- particularly emotional stability and oppenness to new experiences. 

#One other factor to take notice of is the change in the R-squared between our simple model and our multivariable model. R-squared is a measure of model fit in linear regression -- it is the proportion of the variation in the outcome that is predictable from the explanatory variables. The Adjusted R-squared is a modified version of R-squared that has been adjusted for the number of predictors in the model. The adjusted R-squared increases when the new term improves the model more than would be expected by chance. It decreases when a predictor improves the model by less than expected. In our simple model the adjusted R-squared was 0.01169 and in the multivariable model the R squared was 0.3618. This suggests that our current model predicts about 36% of the variation in Burnout scores.

#The output for your model also includes a measure called the residual standard error. In the simple regression model, the residual standard error was 1.091 and in the multivariable model, the RSE was 0.8974. A smaller RSE indicates better model fit because the RSE is the average variation of points around the fitted regression line. Basically it tells you that your line is better

#You can use the RSE and R-squared to help you choose between models. The goal is to have higher R-squared and lower RSE. Sometimes this encourages us to put more and more variables into a model. However, generally speaking, we should prefer smaller less complex models to more complex ones -- so if you are not seeing improvement you can opt for the more parsimonious (or simple) model.

#In addition to checking model fit, you also have to check to make sure that your model meets the assumptions of a linear regression model, which are:
# 1) Linearity of the data. The relationship between the predictor (x) and the outcome (y) is assumed to be linear.
# 2) Normality of residuals. The residual errors are assumed to be normally distributed.
# 3) Homogeneity of residuals variance. The residuals are assumed to have a constant variance (homoscedasticity)
# 4) Independence of residuals error terms.
# 5) No multi-colinearity of variables included in model. Multicollinearity occurs when two or more independent variables are highly correlated with one another in a regression model.


#Lets start with checking the linearity assumption, which can be checked using the residuals vs. fitted plot, we can call up this plot using a simple plot function:

plot(model_3, 1) #The linearity assumption is met when the red line is approximately horizontal and centred on 0.

#Homogeneity of variance can be checked using the scale0location plot, which can be called using the 3rd type of plot called by the plot function:

plot(model_3, 3) #The homogeneity of variance assumption is met when the red line is approximately horizontal. Large swings indicate that the variance changes as the values of the outcome changes, which indicates heterogeneity. 

#The normality assumption can be checked using the normality of residuals plot, which can be called using the following function:

plot(model_3, 2). #In this plot, you want to see the data approximately on a diagonal line. 

#You can also visually inspect your outcome to get a sense of whether your outcome is normally distributed. However, you should note that the assumption for linear regression is the normality of the residuals, and not necessarily the normality of your variable outcome. Of course, these frequently go together and so we like to know whether our outcome is distributed normally.

hist(burnout_data$BurnoutScore) #Plot a histogram: As you can see there is some skew from the high burnout end, however, overall I think its a pretty healthy bell curve.

#Sometimes a visual inspection wont cut it and you can check out a few quantitative measures, such as shapiro wilk test or kolmogorov-smirnov test. 
shapiro.test(burnout_data$BurnoutScore) #Run the shapiro-wilk test; 
ks.test(burnout_data$BurnoutScore, "pnorm") #run the KS test; pnorm indicates we are testing against a normal distribution

#Note that in both of the above tests, the p-values are statistically significant, indicating that the data are non-normal. This is because we have a large sample size, so detecting deviations from normality is really easy. As such, we usually do not rely on these tests solely. The histogram and Q-Q plots in the present case were satisfactory. With enough experience you will "know it when you see it" when it comes to assessing normality. 

#Next we want to check for outliers. The plot() function can be used to identify these:

plot(model_3, 5) #Observations with standardized residuals greater than or less than 3 may be outliers. You could try modeling the data by omitting those variables. They are numbered in the dataset so they can be easily removed by nesting a list within the bracket symbol and using the - sign to say I do not want these variables. Remember that from the first class, the content in brackets to the left of the comma refers to rows and to the right of the comma refers to columns

burnout_data_no_outliers <- burnout_data[-c(370, 392, 1063)] #

#Lets try the model without the outliers:
model_4 <- lm(formula = BurnoutScore ~ gender + age + UCLA3Score + zimet_overall_score + discrimination_score + tipi_extraversion_score + tipi_agreeable_score + tipi_conscientiousness_score + tipi_emotional_stability_score + tipi_emotional_stability_score + tipi_openness_score + attachment_secure_score + attachment_avoidant_score + attachment_anxious_score, data = burnout_data_no_outliers) #Create a linear regression model object
summary(model_4) # View Results

plot(model_4, 5) #plot the residuals vs. leverage. If the red line continues to be quite skewed, you might identify additional outliers and remove these as well by adding them to your list above. In this case, the line looks quite good.

#Another type of outlier you might want to consider is the presence of influential values. This can be done by looking at cooks distance. A rule of thumb is that an observation has high influence if Cook's distance exceeds 4/(n - p - 1), where n is the number of observations and p the number of explanatory variables. So in our case, we can identify a thresshold using the formula below
4/(497 - 13 - 1) # Thresshold is 0.008281573 for model 3, which is based on 497 observations and has 13 predictor variables. 

plot(model_3, 4) #As you can see, observations 392, 370, and 1063 (the same outliers identified in the last model, could be removed). Note there may be others that are below our thresshold, but we know that these are the worrying variables we should deal with first. Generally speaking, we want to limit the data we remove as then our model may become less externally valid to the real world.

#The final thing we want to check for is multicolinearity, which can be done by calculating the variable inflation factor for each variable in our model. This can be done using the VIF() function from the regclass package. 

install.packages("regclass") #Install package
library(regclass) #read library
VIF(model_3) #Calculate VIFs; Values from 5-10 or up indicate potential multicolinearity and could be a good sign for you to consider the variables in your model from a theoretical perspective. Note that multicolinearity is less a concern whenthe variables with high VIFs are control variables, and the variables of interest do not have high VIFs. Here's the thing about multicollinearity: it's only a problem for the variables that are collinear. It increases the standard errors of their coefficients, and it may make those coefficients unstable in several ways. But so long as the collinear variables are only used as control variables, and they are not collinear with your variables of interest, there's no problem. The coefficients of the variables of interest are not affected, and the performance of the control variables as controls is not impaired. Also, note that If the proportion of cases in the reference category is small, the indicator variables will necessarily have high VIFs, even if the categorical variable is not associated with other variables in the regression model.

#If any of the results from the test indicate violation of the assumptions, it is good to test explanatory models 1 by 1 to see which ones might be the issue. If the issue is with your outcome, you may need to convert it to a categorical variable and analyze it using logistic regression (which we will discuss next week). Alternatively, if the issue is with an explanatory variable, you might need to drop it form your model, or transform it in some way, or remove specific observations that might be impacting the results. 

#Once you have a model that you are fairly satisfied with and you feel meets the assumptions of linear regression at least as best as you can, one additional thing you might do is calculate the relative importance of the variables in your model. There are lots of ways to do this, and little agreement as to which is best. I suggest using the varImp function, of the caret pacakge: 
install.packages("caret") #install package
library("caret") #load library for package

varImp(model_3) #Run the varImp function on model 3, Larger values indicate higher contribution to the model. As we can see here, the loneliness scores and social support scores are the most important.

#With that, you've now got what you need to know to construct a linear regression model. As you have seen, linear regression is all about key assumptions. As such, more flexible models are often used. Next time we will talk about one such flexible approach, logistic regression. 

