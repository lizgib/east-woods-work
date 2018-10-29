# EXAMINING ENVIRONMENTAL VARIABLES EFFECT ON EACH OTHER 

# Read in all of the environmental variables 
source('~/Documents/morton arb/east_woods_phylogeny/SCRIPTS/analyzez.R')
# Instead of doing plot dissimilarity im going to try first with just the plot level

# doing multiple regression 
# Multiple Linear Regression Example 
#fit <- lm(y ~ x1 + x2 + x3, data=mydata)
#summary(fit) # show results

fit_com_class <- lm(com_class ~ factor(elevation) + factor(slope) + factor(aspect) , data = liz_data)
summary(fit_com_class)

