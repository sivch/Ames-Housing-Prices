library("ggplot2")
library(scales)
library("BAS")
library(reshape2)
# Set plotting theme
theme_set(
  theme_minimal() +
    theme(legend.position = "right") + 
    theme(axis.text = element_text(size = 16)) + 
    theme(axis.title = element_text(size = 20))
  
)

# Read data 
ames <- read.csv("ameshouse.txt", sep="")
ames <- data.frame(ames)

######## DATA CLEANING #########

# Remove outliers (GrLivingArea > 4000) ? MAYBE
ames<- ames[ames$GrLivArea < 4000,]

# Check number of missing values 
data.frame(sort(colSums(is.na(ames)), decreasing = TRUE)[0:20])

# Remove features FireplaceQu, LotFrontage, secondary garage features
drop_cols <- c("FireplaceQu",
               "LotFrontage", 
               "GarageType", 
               "GarageYrBlt",
               "GarageFinish"
               ,"GarageQual",
               "GarageCond")
ames <- ames[,!(names(ames) %in% drop_cols)]

# Remove rows with missing values 
ames <- ames[which(complete.cases(ames)),]

# Check for missing values
data.frame(sort(colSums(is.na(ames)), decreasing = TRUE)[0:20])

# Convert categorical variables from numerical to factors 
ames$MSSubClass <- as.factor(ames$MSSubClass)
ames$OverallQual <- as.factor(ames$OverallQual)
ames$OverallCond <- as.factor(ames$OverallCond)

######################################################################## 
###### DATA TRANSFORMING AND VARIABLE SELECTION #######################
########################################################################

# Log transform 
ames$SalePrice <- log10(ames$SalePrice) # log transform 
ames$GrLivArea <- log10(ames$GrLivArea) # log transform

# Convert month sold from numeric to quarters (categorical variable)
ames$Sold_Q1 <- as.numeric(ames$MoSold < 4)
ames$Sold_Q2 <- as.numeric(ames$MoSold >= 4 & ames$MoSold < 7)
ames$Sold_Q3 <- as.numeric(ames$MoSold >= 7 & ames$MoSold < 10)
ames$MoSold <- as.factor(ames$MoSold)

# Convert the chosen categorical features to dummy-variables
ames_dummy <- fastDummies::dummy_cols(ames, 
                                      select_columns = c("Sold_Q1",
                                                         "Sold_Q2",
                                                         "Sold_Q3",
                                                         "CentralAir"),     
                                      remove_selected_columns	= TRUE, 
                                      remove_first_dummy = TRUE) # keep n-1 dummies


# Examine which numeric variables to keep and discard: 
  # Correlation plot 
cor_matrix <- round(cor(ames_dummy[,unlist(lapply(ames_dummy, is.numeric), use.names = FALSE)]),2)
melted_cor_matrix <- melt(cor_matrix)
ggplot(data = melted_cor_matrix, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()

# Drop "GarageCars" since it is very correlated with "GarageArea"
drop_cols <-c("GarageCars")

# Drop "BsmtFinSF1", "BsmtFinSF2" and "BsmtFullBath" since they are very correlated with "BsmtUnfSF" 
# keep "BsmtUnfSF" as it might be interesting to see how much it reduces the sale price 
# if basement is unfinished??
drop_cols <- append(drop_cols, c("BsmtFinSF1","BsmtFinSF2","BsmtFullBath"))

# Drop "X1stFlrSF" and "X2ndFlrSF"
drop_cols <- append(drop_cols, c("X1stFlrSF", "X2ndFlrSF"))

# Drop "TotRmsAbvGrd" since highly correlated with "GrLivArea"
drop_cols <- append(drop_cols, c("TotRmsAbvGrd"))

# Drop yearSold since it is not very interesting explanatory variable 
# the year sold does not say anything meaningful about the price, other than the prices 
# have risen with inflation and that some years were probably better than others. 
# however, it is interesting whether the month/quarter it is sold influences the sale price
drop_cols <- append(drop_cols, c("YrSold"))

# other
drop_cols <- append(drop_cols, c("MiscVal"))

# DROP numerical columns
ames_dummy <- ames_dummy[,!(names(ames_dummy) %in% drop_cols)]
# DROP categorical columns
ames_dummy = ames_dummy[,unlist(lapply(ames_dummy, is.numeric), use.names = FALSE)]
           
######## Split into training and test ##########
## 80% of the sample size
train_size <- floor(0.8 * nrow(ames_dummy))
## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(ames_dummy)), size = train_size)

ames_train <- ames_dummy[train_ind, ]
ames_test <- ames_dummy[-train_ind, ]

######## Scale #########
## ? should we standardize? 
ames_train <- scale(ames_train)
ames_test <- scale(ames_test)

######## Fit Full model #######

# Use `bas.lm` to run regression model
cog.bas = bas.lm(SalePrice ~ ., 
                 data = ames_dummy, 
                 prior = "g-prior",
                 modelprior = Bernoulli(1), 
                 bestmodel = rep(1, ncol(ames_dummy)), 
                 n.models = 1)

# Posterior Means and Posterior Standard Deviations:
cog.coef = coef(cog.bas)
cog.coef

# Plot marginal posterior distributions of each regressor
par(mfrow = c(2, 2))
plot(cog.coef, subset = 2:5, ask = F)

####### MODEL SELECTION #######

# Find best model with BIC
cog.BIC = bas.lm(SalePrice ~ ., data = ames_train,
                 prior = "BIC", 
                 modelprior = uniform(),
                 method = "MCMC")

round(summary(cog.BIC), 3)

# Find the index of the model with the largest logmarg
best = which.max(cog.BIC$logmarg)

# Retreat the index of variables in the best model, 0 is the intercept index
bestmodel = cog.BIC$which[[best]]+1

print(bestmodel)

# 0 vector with length equal to the number of variables in the full model
bestgamma = rep(0, cog.BIC$n.vars)
# Change the indicator to 1 where variables are used
bestgamma[bestmodel] = 1

print(bestgamma)

# Fit the best BIC model. Impose the variables to use via bestgamma
cog.bestBIC = bas.lm(SalePrice ~ ., data = ames_train, prior = "BIC",
                     modelprior=uniform(), n.models=1, bestmodel=bestgamma)

# Retreat coefficients information
cog.coef = coef(cog.bestBIC)

# Retreat bounds of credible intervals
out = confint(cog.coef)[, 1:2]

# Combine results and construct summary table
coef.BIC = cbind(cog.coef$postmean, cog.coef$postsd, out)
names = c("post mean", "post sd", colnames(out))
colnames(coef.BIC) = names

round(coef.BIC[bestmodel,], 3)

########### Prediction ##########
fitted<-predict(cog.bestBIC, estimator = "BMA")
prednew <- predict(cog.bestBIC,newdata=ames_test, estimator = "BMA")

plot(fitted$Ypred[1:length(fitted$Ypred)],ames_train$SalePrice[1:length(fitted$Ypred)],
     pch = 16,
     xlab = expression(hat(mu[i])), ylab = 'Y',type="p")

points(prednew$Ypred, ames_test$SalePrice,
       pch = 16,
       col="red",type="p"
)

########## Plot errors #########
plot(seq(1,nrow(ames_test)),prednew$Ypred-ames_test$SalePrice,
       pch = 1,
      cex = 0.5,
       col="darkblue",type="p", 
     xlab = "Index",
     ylab = "Error"
)


