#### LECTURE 6 ####

#This lecture will focus on correlation. 

#To get started, lets read in our burnout dataset. 
setwd("C:\\Users\\Kiffer\\Google Drive\\Work\\2. Current Projects\\HSCI_410\\3. Lectures") #Set Working Directory
burnout_data <- read.csv(file = "burnout_data.csv") #Read in burnout_data

#Lets start with a type of correlation, that is not quite correlation at all. Typically correlation is between two numeric variables. The cramerV() function can be used to calculate correlation, based on the Cramers V and Chi-Squared test, between two categorical variables. cramerV() is a part of the rcompanion package.
install.packages("rcompanion")
library(rcompanion)

#Lets test if there is an association between Gender and being burnt out:

cramerV(burnout_data$BurnoutScore_groups, burnout_data$gender) #Calculate Cramers V to get an effect size
chisq.test(burnout_data$BurnoutScore_groups, burnout_data$gender) #Calculate the Chi.sq p-value to test for significance

#The coefficient ranges from 0 to 1 (perfect association). In practice, you may find that a Cramer's V of . 10 provides a good minimum threshold for suggesting there is a substantive relationship between two variables. 
# >.5       strong association
# .3 to .5  moderate association
# .1 to .3  weak association
# 0 to .1   very weak if any association
#Given this, we would suggest there is a quite weak, albeit statistically significant association

#As I mentioned, correlation is typically used When comparing the association between two numeric variables. In our dataset we have a few continuous variables to work with. Lets start with looking at the association between burnout and loneliness scores:

class(burnout_data$BurnoutScore) #Make sure they are numeric
class(burnout_data$UCLA3Score) #Make sure they are numeric
burnout_data$UCLA3Score <- as.numeric(burnout_data$UCLA3Score) #Convert to numeric

#Lets start by graphing the data using the plot() function
plot(burnout_data$BurnoutScore, burnout_data$UCLA3Score) #Plot the correlation between burnout scores and UCLA loneliness scores.

#As you can see the top left part of the plot and bottom right part of the plot are quite sparse. You can imagine that if you were to draw a line through this data, you would probably draw it going from the bottom left to the top right. This indicates a positive association. However, with many scales that have a limited range, it can be difficult to ascertain an association visually because observations are overlapping. One strategy you can do is nest your variables within a jitter() function. The jitter() function adds a little bit of noise to your data so that all your observations are not overlapping one another. This can help you see trends a little bit clearer:
?jitter # Get information on the factor

plot(jitter(burnout_data$BurnoutScore), jitter(burnout_data$UCLA3Score)) #Plot the correlation between burnout scores and UCLA loneliness scores using Jitter. 

#While in our data the jitter() function makes it easier to draw a line through the "bulky" party of the data. In other situations it might still be difficult plotting a line. As such, it is useful to calculate statistics that can help tell us the strength of an association objectively. 

# The cor.test() function can be used to calculate the correlation between two continuous variables
cor.test(burnout_data$BurnoutScore, burnout_data$UCLA3Score)

#Correlation ranges from -1 (Strong negative association) to +1 (Strong Positive Association) 
# 00-.19 "very weak"
# .20-.39 "weak"
# .40-.59 "moderate"
# .60-.79 "strong"
# .80-1.0 "very strong"

#The correlation here is .5422 and the p-value is very small (i.e, 2.2e-16) Therefore we find that there is a moderately strong, statistically-signficant correlation between loneliness and burn out scores. 

#When loooking at correlations for many variables as once, it is  useful to create a correlation matrix. A correlation matrix is a table showing correlation coefficients between variables. Each cell in the table shows the correlation between two variables. A correlation matrix is used to summarize data, as an input into a more advanced analysis, and as a diagnostic for advanced analyses. To create a correlation matrix we use the corrplot() function, within the corrplot package

install.packages("corrplot") #Install the corrplot package
library(corrplot) #read in the corrplot library

#The first step to creating a correlation is to subset into a new dataframe the variables you are wanting to build a correlation matrix for. Otherwise, your correlation plot would be way to big!

keepvars <- c("BurnoutScore", #Select numeric variables you want included in your correlation matrix
              "UCLA3Score",
              "zimet_overall_score",
              "discrimination_score",
              "tipi_extraversion_score",
              "tipi_agreeable_score",
              "tipi_conscientiousness_score",
              "tipi_emotional_stability_score",
              "tipi_openness_score",
              "attachment_secure_score",
              "attachment_avoidant_score",
              "attachment_anxious_score")
corr_data <- burnout_data[keepvars] #Create new dataframe with the variables you selected

#Now that you have the variables selected, you need to create and plot a correlation matrix using the cor() and corrplot() functions respectively. 
corr_matrix<-cor(corr_data, use = "pairwise.complete.obs") #Create Correlation Matrix, removing any data with missing for each  correlation.
corrplot(corr_matrix, is.corr = FALSE, method = "number") #Plot correlation matrix, and show results as "numbers"

#This correlation matrix shows you how strongly the variables in your dataset are correlated using the colour and number. You can choose the method to display the plot using other symbols as well. 

#Another function you can use to create a correlation plot is the chart.Correlation() function within the PerformanceAnalytics packages. This method builds and plots the correlation plot in one step and has lots more useful graphs (histograms, individual correlation plots, etc), shows statistical significance of each test (the stars), and the size of effect. Note that the variables are listed in the middle square over top the historgram
install.packages("PerformanceAnalytics") #Load in PerformanceAnalytics package
library(PerformanceAnalytics) #Use PerformanceAnalytics library
chart.Correlation(corr_data, histogram=TRUE) #Create plots

# Note that there are three kinds of correlation coefficients that are widely used. Most of the correlation functions allow you to specify which one: The Default is the Pearsons correlation coefficient, which requires the variables to be normally distributed, linearly related, and have equal errors across all levels (i.e., homoscedastic). Next week, when we talk about linear regression, We will look at ways of checking whether your data meat these assumptions. In cases where these assumptions are not likely met, you can use the Spearman rank correlation, which is a non-parametric test that is used to measure the degree of association between two variables.  The Spearman rank correlation test does not carry any assumptions about the distribution of the data and is the appropriate correlation analysis when the variables are measured on a scale that is at least ordinal. Examples of using the spearman are provided below
chart.Correlation(corr_data, histogram=TRUE, method = "spearman") #Create correlation matrix using the spearman correlation
cor.test(burnout_data$BurnoutScore, burnout_data$UCLA3Score, method = "spearman") #Create Correlation between two variables with the spearman method argument

# This time learned about correlation between two continuous variables and two categorical variables. Next week, we will introduce the concept of linear regression and the week after we will talk about logistic regression.

