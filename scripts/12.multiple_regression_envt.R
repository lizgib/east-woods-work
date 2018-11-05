# EXAMINING ENVIRONMENTAL VARIABLES EFFECT ON EACH OTHER 

# Read in all of the environmental variables 
source('~/Documents/GitHub/east_woods_work/scripts/11.analyses.R')
source()
# Instead of doing plot dissimilarity im going to try first with just the plot level

# doing multiple regression 
# Multiple Linear Regression Example 
#fit <- lm(y ~ x1 + x2 + x3, data=mydata)
#summary(fit) # show results

fit_com_class <- lm(com_class ~ factor(elevation) + factor(slope) + factor(aspect) , data = liz_data)
summary(fit_com_class)

