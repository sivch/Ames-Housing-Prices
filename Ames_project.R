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

############################################
############## DATA CLEANING ##############
############################################

# Remove outliers (GrLivingArea > 4000) 
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

# Examine which numeric variables to keep and discard: 
# Correlation plot 
cor_matrix <- round(cor(ames[,unlist(lapply(ames, is.numeric), use.names = FALSE)]),2)
melted_cor_matrix <- melt(cor_matrix)
ggplot(data = melted_cor_matrix, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                   size = 13, hjust = 1),
        axis.text.y = element_text(size = 13), 
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 16))+
  labs(x = "", y = "") + 
  coord_fixed()
#ggsave("corplot3.png")

# Convert month sold from numeric to quarters (categorical variable)
ames$Sold_Q1 <- as.numeric(ames$MoSold < 4)
ames$Sold_Q2 <- as.numeric(ames$MoSold >= 4 & ames$MoSold < 7)
ames$Sold_Q3 <- as.numeric(ames$MoSold >= 7 & ames$MoSold < 10)
ames$MoSold <- as.factor(ames$MoSold)

# Convert porch-variables to a single numeric response "totalPorchAreaSF"
ames$totalPorchAreaSF =  ames$OpenPorchSF + ames$EnclosedPorch + ames$X3SsnPorch + ames$ScreenPorch

# Convert the chosen categorical features to dummy-variables
ames_dummy <- fastDummies::dummy_cols(ames, 
                                      select_columns = c("Sold_Q1",
                                                         "Sold_Q2",
                                                         "Sold_Q3",
                                                         "CentralAir"),     
                                      remove_selected_columns	= TRUE, 
                                      remove_first_dummy = TRUE) # keep n-1 dummies


# Drop "GarageCars" since it is very correlated with "GarageArea"
drop_cols <-c("GarageCars")

# Drop "BsmtFinSF1", "BsmtFinSF2" and "BsmtFullBath" since they are very correlated with "BsmtUnfSF" 
# keep "BsmtUnfSF" as it might be interesting to see how much it reduces the sale price 
# if basement is unfinished. 
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
drop_cols <- append(drop_cols, c("MiscVal", 
                                 "OpenPorchSF", 
                                 "EnclosedPorch",
                                 "X3SsnPorch",
                                 "ScreenPorch"))

# Drop chosen numerical columns
ames_dummy <- ames_dummy[,!(names(ames_dummy) %in% drop_cols)]
# Drop categorical columns
ames_dummy = ames_dummy[,unlist(lapply(ames_dummy, is.numeric), use.names = FALSE)]

# Total number of variables
print(ncol(ames_dummy))

######## Split into training and test ##########
## 80% of the sample size
train_size <- floor(0.8 * nrow(ames_dummy))
set.seed(123)
train_ind <- sample(seq_len(nrow(ames_dummy)), size = train_size)

ames_train <- ames_dummy[train_ind, ]
ames_test <- ames_dummy[-train_ind, ]

######## Scale #########
ames_train <- data.frame(scale(ames_train))
ames_test <- data.frame(scale(ames_test))

#######################################
######### MODEL SELECTION #############
#######################################

# hyperparam
alphaparam <- 30

# Find best model g-prior
ames.gprior = bas.lm(SalePrice ~ ., data = ames_train,
                     prior = "g-prior", 
                     a = alphaparam,
                     modelprior = uniform(),
                     method = "MCMC")

round(summary(ames.gprior), 3)

# Find the index of the model with the largest logmarg
best = which.max(ames.gprior$logmarg)

# Retrieve the index of variables in the best model
bestmodel = ames.gprior$which[[best]]+1

# 0 vector with length equal to the number of variables in the full model
bestgamma = rep(0, ames.gprior$n.vars)
# Change the indicator to 1 where variables are used
bestgamma[bestmodel] = 1

# Fit the best g-prior model. Impose the variables to use via bestgamma
ames.best_gprior = bas.lm(SalePrice ~ ., 
                          data = ames_train, 
                          prior = "g-prior",
                          modelprior=uniform(), 
                          a=alphaparam,
                          n.models=1, 
                          bestmodel=bestgamma)

# Retrieve coefficients information
ames.coef = coef(ames.best_gprior)

# Retreat bounds of credible intervals
out = confint(ames.coef)[, 1:2]

# Combine results and construct summary table
coef.gprior = cbind(ames.coef$postmean, ames.coef$postsd, out)
names = c("post mean", "post sd", colnames(out))
colnames(coef.gprior) = names

round(coef.gprior[bestmodel,], 4)

# Plot marginal posterior distributions of each regressor
par(mfrow = c(3, 2))
plot(ames.coef, subset = (bestmodel[1:6]), ask = F)
plot(ames.coef, subset = (bestmodel[7:12]), ask = F)
plot(ames.coef, subset = (bestmodel[13:13]), ask = F)

##########################################
###### MODEL UNCERTAINTY ##################
##########################################

# show the posterior probabilities of the top 5 models
round(summary(cog_bas), 3)

# Print marginal posterior probabilities of inclusion
print(ames.gprior)

# Visualize Model uncertainty
par()
image(ames.gprior, rotate = F)

##########################################
############### Prediction ############
##########################################
par(mfrow = c(1, 1))
fitted <- predict(ames.best_gprior, estimator = "BMA")
prednew <- predict(ames.best_gprior, newdata=ames_test, estimator = "BMA")

plot(fitted$Ypred,ames_train$SalePrice,
     pch = 16,
     cex = 0.8,
     xlab = expression(hat(mu[i])), ylab = 'Y',type="p")

points(prednew$Ypred, ames_test$SalePrice,
       pch = 16,
       cex = 0.8,
       col="red",type="p")
abline(0, 1)
legend(x = -3, y = 3.45, legend = c("Training data", "Test data"), col = c("black", "red"), cex = 1.4, pch = 16)

prednew_se <- predict(ames.best_gprior, estimator = "BPM", newdata=ames_test,se.fit = TRUE)
conf.fit <- confint(prednew_se, parm = "mean")
conf.pred <- confint(prednew_se, parm = "pred")
plot(conf.pred[1:40], col = "black")
points(seq(1:40),ames_test$SalePrice[1:40],col="red")
legend(x = 33.3, y = -1.8, legend = c("Predicted", "True"), col = c("Black", "Red"), pch = 1, cex = 1)

n = 40
BPM <- predict(ames.best_gprior, estimator = "BPM", newdata=ames_test,se.fit = TRUE)
conf.fit <- confint(BPM, parm = "mean")
conf.pred <- confint(BPM, parm = "pred")
plot(conf.pred)
points(ames_test$SalePrice,col="Red")

########## Plot errors #########

d = data.frame(Index = seq(1,nrow(ames_test)), Error = c(prednew$Ypred-ames_test$SalePrice))
ggplot(data = d)+ geom_point(aes(x = Index, y = Error), shape=1, col = "black", size = 3) +
  theme_minimal()+
  theme(axis.text.x = element_text(size = 16), 
        axis.text.y = element_text(size = 16), 
        axis.title = element_text(size = 20)) +
  geom_hline(yintercept=0,color = "red", size=1)

