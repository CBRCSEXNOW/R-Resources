#### LECTURE 11 ####

#This lecture focuses on mediation and moderation. Three methods will be reviewed: (1) mediation, (2) interaction, and (3) stratification. 

#Lets start by reading in our dataset
setwd("C:\\Users\\Kiffer\\Google Drive\\Work\\2. Current Projects\\HSCI_410\\3. Lectures") #Set Working Directory
burnout_data <- read.csv(file = "burnout_data.csv") #Read in burnout_data

#The first method we are going to review in this lecture is called mediation. Mediation seeks to identify and explain the mechanism or process that underlies an observed relationship between an independent variable and a dependent variable via the inclusion of a third hypothetical variable, known as a mediator variable. There are three steps to create a mediation model:

#Step 1 - Model relationship between your explanatory factor and outcome. In this case, lets use Burnout scores as our outcome and support from coworkers as our explanatory factor (we will also control for age, gender, and ethnicity).

table(burnout_data$work_support) #Check out our variable;
burnout_data$work_support[burnout_data$work_support == "8888:Not Working During COVID"] <- NA#remove those not employed during covid
#I'm going to use a binary explanatory factor in this case, so will collapse the not at all, somewhat, and very little groups and the completely and very well groups:
burnout_data$work_support_di[burnout_data$work_support == "Completely" | 
                               burnout_data$work_support == "Very Well"] <- "Completely/Very Well"
burnout_data$work_support_di[burnout_data$work_support == "Somewhat" | 
                               burnout_data$work_support == "Very Little" |
                               burnout_data$work_support == "Not at all"] <- "Somewhat/Very Little/Not at all"

burnout_data$work_support_di <- relevel(as.factor(burnout_data$work_support_di), ref = "Completely/Very Well") #Set reference level for variable
table(burnout_data$work_support_di)
#Remove participants with missing observations across key variables <- Necessary step because each model needs to be based on the same underlying data. The work_support variable was only asked of one-third of respondents. 
burnout_data_subset <- burnout_data[!is.na(burnout_data$BurnoutScore),]
burnout_data_subset <- burnout_data_subset[!is.na(burnout_data_subset$work_support_di),]
burnout_data_subset <- burnout_data_subset[!is.na(burnout_data_subset$UCLA3Score),]
burnout_data_subset <- burnout_data_subset[!is.na(burnout_data_subset$age),]
burnout_data_subset <- burnout_data_subset[!is.na(burnout_data_subset$gender),]
burnout_data_subset <- burnout_data_subset[!is.na(burnout_data_subset$ethnicity),]

Y_X <- lm(formula = BurnoutScore ~ work_support_di + age + gender + ethnicity, data = burnout_data_subset)
summary(Y_X) #Results show that those who are less supported are more likely to burnout. 

#Step 2 - Model relationship between your explanatory factor/outcome and your mediator. In this case, our mediator will be lonelinesss. I hypothesize that people who don't feel supported, experience loneliness, and that loneliness contributes to burnout. 

M_X <- lm(formula = UCLA3Score ~ work_support_di + age + gender + ethnicity, data = burnout_data_subset)
summary(M_X) #less worplace support is associated with loneliness

Y_M <- lm(formula = BurnoutScore ~ UCLA3Score + age + gender + ethnicity, data = burnout_data_subset)
summary(M_X) #Loneliness is associated with burnout

#Step 3 - Model relationship between your explanatory factor and your outcome, controlling for mediator

Y_XM <- lm(formula = BurnoutScore ~ work_support_di + UCLA3Score + age + gender + ethnicity, data = burnout_data_subset)
summary(Y_XM) #Thou the effect  remain statistically significant, You can see that the effect sizes dropped:

# Model               Y_X       ->   Y_XM   
# Effect Size         0.548862  ->   0.325329   

#I can now use the mediate() function from the mediate package 
install.packages("mediation")
library("mediation")
mediation_results <- mediate(M_X, Y_XM, treat='work_support_di', mediator='UCLA3Score', control.value = "Completely/Very Well", treat.value = "Somewhat/Very Little/Not at all", boot=TRUE, sims=100) #Test for mediation using 100 bootstrap simulations

summary(mediation_results) #Get mediation test results.

#ACME (Effect of mediator/Indirect Effect) - approx 0.224 is the average increase in the outcome variable among the treatment group (i.e. those with low social support) that arrives as a result of loneliness rather than 'directly' from the treatment. 
#ADE (Direct Effect of Explanatory Factor) - approx 0.325 is the average increase in the outcome variable among the treatment group (i.e. those with low social support) that arrives 'directly' from the treatment. 
#Total effects, is the sum of the ACME and ADE. 
#Prop. Mediated is the proportion of the effect of your outcome that is mediated by your mediator. 


#Mediation is used when you want to understand the mechanism or pathway by which an explanatory factor impacts an outcome. However, when you want to understand whether an effect operates differently in two populations, you should instead stratify your analysis. Lets try this by seeing if the effect of discrimination on burnout is the same on people who identify as Black, indigenous, or as a person of colour compared to those who do not. This can be done either by creating two seperate datasets (which you already know how to do) or by using the subset function.
table(burnout_data$identity_bipoc) #Check out our stratifiying variable (i.e., ethnicity)
summary(burnout_data$discrimination_score) #Check out our explanatory variable (i.e., discrimination)
summary(burnout_data$BurnoutScore) #Check out our outcome variable (i.e., burnout)

BIPOC_model <- lm(formula = BurnoutScore ~ discrimination_score + age + gender, data = burnout_data, subset = (identity_bipoc == "People of colour (e.g., Black, Indigenous, Asian  or other racialized minority)"))
summary(BIPOC_model)
#R^2 0.03441 #Effect of discrimination: 0.016692   

Non_BIPOC_model <- lm(formula = BurnoutScore ~ discrimination_score + age + gender, data = burnout_data, subset = (identity_bipoc == "8888: Not Selected"))
summary(Non_BIPOC_model)
#R^2 0.06568  #Effect of discrimination: 0.023961 

#From the output of the two models above, we can see that by stratifying our analyses, we actually see that the effect of discrimination is smaller on BIPOC vs. non-BIPOC participants. Additionally, the model among BIPOC individuals explains less of the variance in Burnout Scores than the model among non-BIPOC participants (0.016692 vs. 0.023961). This might suggest that discrimination is not the only factor driving burnout among BIPOC people, or perhaps BIPOC people are just more resilient to experiences of discrimination (because they face it so often)

#We can do some checks, such as look at the distribution of burnout among non-BIPOC vs. BIPOC- identified participants. To do so, I am going to use an advanced graphics package called ggplot2 and use the functions, ggplot() and geom_density to display the densities of discrimination scores, stratified by identity_bipoc using the fill = function

install.packages("ggplot2") #read in package
library("ggplot2") #read in library
ggplot(burnout_data, aes(discrimination_score, fill = !is.na(identity_bipoc))) + #aes() is the mapping command in ggplot, I tell it the variable I want plotted; fill = tells R that I want the data stratified, !is.na() ensures that the non-missing data is not plotted.
  geom_density(alpha = 0.5) #alpha = 0.5 controls transparency #I use the plus sign above to allow me to connect the ggplot() mapping to the type of graph I want()

# A simpler way of doing this graph would be to just graph each histogram separately:
hist(burnout_data$discrimination_score[burnout_data$identity_bipoc == "People of colour (e.g., Black, Indigenous, Asian  or other racialized minority)"]) #Plot BIPOC
hist(burnout_data$discrimination_score[burnout_data$identity_bipoc == "8888: Not Selected"]) #Plot Non-BIPOC

#Either graphing method reveals a similar story.. Many more of the BIPOC folk experience daily  discrimination. You can verify this by looking at the descriptives, stratified by the identity_BIPOC variable:
summary(burnout_data$discrimination_score[burnout_data$identity_bipoc == "8888: Not Selected"]) #Summary for Non-BIPOC
summary(burnout_data$discrimination_score[burnout_data$identity_bipoc == "People of colour (e.g., Black, Indigenous, Asian  or other racialized minority)"]) #Plot BIPOC

#I could go through and compare these two models more thouroughly -- assess their fit. But you can see from this example that stratifying can be a complex approach to understanding the effect. Another way you can understand these sorts of moderators is through the use of an interaction term. Interactions are specified in a model using the * symbol, as shown below:

Interaction_model <- lm(formula = BurnoutScore ~ discrimination_score*identity_bipoc + age + gender, data = burnout_data)

broom::tidy(Interaction_model) #Use tidy to view results, notice I use :: to specify the package that tidy belongs to, which saves me from using the library() function to call in the broom library

#In this model, the effects of discrimination and being a person of colour are both statistically significant predictors of burnout. However, the effect of the interaction term (I.e, the last variable listed showing discrimination:identity_bipoc) is not significant. This helps us understand the results better from our stratified model: Namely, that the effect of discrimination is not statistically different for BIPOC vs. non-BIPOC people. You will notive that the effect of the interaction term is actually negative. This is an artifact for the fact that I am also controlling for the independent effect of discrimination. If you want to include an interaction term without including the individual main effects you can seperate your interaction variables by the : symbol, as shown below

Interaction_model_2 <- lm(formula = BurnoutScore ~ discrimination_score:identity_bipoc + age + gender, data = burnout_data) #Create interaction without independent main effects of interaction term
broom::tidy(Interaction_model_2) #call results

#This approach is similar to (though not identical to) stratification. It allows us to see the effect of discrimination seperately for BIPOC and non-BIPOC seperately. 

#Lets cover one more example of interaction with two continuous variables. We will look at the interaction of loneliness and extraversion in predicting burnout. I hypothesize that loneliness contributes more to burnout among those with high extraversion (because they need more social connection). 

Interaction_model_3 <- lm(formula = BurnoutScore ~ UCLA3Score*tipi_extraversion_score + age + gender, data = burnout_data) #Create interaction without independent main effects of interaction term
broom::tidy(Interaction_model_3) #call results

#Surprisingly the, interaction term is not significant, though we do see that extravertedness is associated with lower burnout and loneliness is associated with higher burnout. We can explore these effects further using the plot_model() function of the sjPlot package

install.packages("sjPlot") #install package
library("sjPlot") #Install library
plot_model(Interaction_model_3, type = "pred", terms = c("UCLA3Score", "tipi_extraversion_score")) #Plot the interaction model. type = "pred" is an argument to plot the marginal effects (or predicted values of Burnout across levels of loneliness)

#As you can see the predicted levels of Burnout across UCLA scores are basically the same (overlapping) for the three levels displayed of the etraversion score, which were defined based on the mean and sd. So the red line represents those values 1 standard deviation below the mean, the blue represents the values up to the mean, and the green represents greater than 1 standard deviation above the mean:
summary(burnout_data$tipi_extraversion_score)
sd(burnout_data$tipi_extraversion_score, na.rm = TRUE)

#We can also plot a chart for the interaction_model_2:
plot_model(Interaction_model_2, type = "pred", terms = c("discrimination_score", "identity_bipoc"))

#Based on these results, neither of my hypothesized interactions were confirmed. But hopefully, you see the value of using interaction terms. If you recall from last week, we talked about compraing model fit. These same methods are helpful when you are considering whether to include an interaction term. Let's check the AIC values for the interaction model compared to a new model without the interaction:

no_interaction <- lm(formula = BurnoutScore ~ UCLA3Score + tipi_extraversion_score + age + gender, data = burnout_data) #Create interaction without independent main effects of interaction term
broom::tidy(no_interaction) #call results


AIC(Interaction_model_3, no_interaction) #As you can see the model without the interaction is superior. Adding the interaction, while perhaps interesting to demonstrate a null result from my hypothesized effect, does not add information to the model. This can be confirmed using the liklihood ratio test since the non-interaction model is nested within the interaction model. 
lrtest(Interaction_model_3, no_interaction)

#With that, we have covered mediation and moderation by exploring mediation models, stratified models, and interaction terms.

