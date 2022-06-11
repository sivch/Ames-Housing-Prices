library("ggplot2")
library(scales)
library("BAS")
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

##### Exploratory ##########

print(ncol(ames))
print(nrow(ames))
str(ames)

# Visualize target
options(scipen=10000)
p1 <- ggplot(df, aes(x=SalePrice)) + geom_density(fill = "lightgrey") + labs(x="Sale Price ($)") + scale_x_continuous(labels = label_number(suffix = " K"))
p2 <- ggplot(df, aes(x=SalePrice)) + geom_density(fill = "lightgrey") +  scale_x_log10() + labs(x="log(Sale Price)", y = "")
p <- grid.arrange(p1, p2, ncol=2)
ggsave("SalePrice_plot.png",p, dpi = 300)

# Choose subset of features for scatter plot
subset_cols <- c("SalePrice", "GrLivArea", "YearBuilt", "LotArea")
subset_data <- ames[subset_cols]
library(psych)
#png(filename = "pairs_plot.png", width = 6, height = 6, units = 'in', res = 300)
pairs.panels(subset_data, 
             method = "pearson", # correlation method
             hist.col = "grey",
             density = TRUE,  # show density plots       
             smooth = FALSE,
             ellipses = FALSE,
             cex.cor = 0.5,
             cex.axis = 1.5,
             pch = 21,
             bg= rev(heat.colors(10))[factor(ames$OverallQual)]
)
#dev.off() 
legend(0,1, as.vector(sort(unique(ames$OverallQual))),  
       fill=rev(heat.colors(10)))

ggcorplot(
  data = subset_data,
  var_text_size = 5,
  cor_text_limits = c(5,10))

#par(xpd = TRUE)
#legend("bottomright", fill = unique(ames$OverallQual), legend = c( levels(as.factor(ames$OverallQual))))


######## Transform target #############

# Log transform 
ames$SalePrice <- log10(ames$SalePrice) # log transform 

######## DATA CLEANING #########

# Remove outliers (GrLivingArea > 4000) ? MAYBE

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

# Convert quality and condition from numeric to categorical 
ames$OverallCond_Poor = as.numeric(ames$OverallCond <= 4)
ames$OverallCond_Good = as.numeric(ames$OverallCond >= 7)

ames$OverallQual_Poor = as.numeric(ames$OverallQual <= 4)
ames$OverallQual_Good = as.numeric(ames$OverallQual >= 8)

# Convert MSSubClass from numerical to categorical 
ames$MSSubClass <- as.factor(ames$MSSubClass)

# Convert month sold from numeric to quarters (categorical variable)
ames$Sold_Q1 <- as.numeric(ames$MoSold < 4)
ames$Sold_Q2 <- as.numeric(ames$MoSold >= 4 & ames$MoSold < 7)
ames$Sold_Q3 <- as.numeric(ames$MoSold >= 7 & ames$MoSold < 10)

# Convert Condition-features to binary [normal, not-normal]:
ames$Condition1_norm <- as.numeric(ames$Condition1 == "Norm")
ames$Condition2_norm <- as.numeric(ames$Condition2 == "Norm")

# Drop converted columns
ames <- ames[,!(names(ames) %in% c("OverallQual", 
                                   "OverallCond",
                                   "MoSold",
                                   "Condition1",
                                   "Condition2"))]

# Convert categorical features to dummy-variables
ames_dummy <- fastDummies::dummy_cols(ames, 
                                      remove_selected_columns	= TRUE, 
                                      remove_first_dummy = TRUE) # keep n-1 dummies

######## Split into training and test ##########
## 80% of the sample size
train_size <- floor(0.8 * nrow(ames))
## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(ames_dummy)), size = train_size)

ames_train <- ames_dummy[train_ind, ]
ames_test <- ames_dummy[-train_ind, ]

######## Normalize #########


####### Model with only GrLivearea

# Frequentist linear model
ames.lm = lm(SalePrice ~ GrLivArea, data = ames_train)
summary(ames.lm)
# Extract coefficients
beta = coef(ames.lm)

# Visualize regression line on the scatter plot
ggplot(data = ames_train, aes(x = GrLivArea, y = SalePrice)) +
  geom_point(color = "steelblue") +
  geom_abline(intercept = beta[1], slope = beta[2], size = 1, col='darkorange') +
  xlab("GrLivArea") + ggtitle("Regression Line")

output = summary(ames.lm)$coef[, 1:2]
output

out = cbind(output, confint(ames.lm))
colnames(out) = c("posterior mean", "posterior std", "2.5", "97.5")
round(out, 3)

# Construct current prediction
alpha = ames.lm$coefficients[1]
beta = ames.lm$coefficients[2]
new_x = seq(min(ames$GrLivArea), max(ames$GrLivArea), 
            length.out = 100)

y_hat = alpha + beta * new_x

# Get lower and upper bounds for mean
ymean = data.frame(predict(ames.lm,
                           newdata = data.frame(GrLivArea = new_x),
                           interval = "confidence",
                           level = 0.95))

# Get lower and upper bounds for prediction
ypred = data.frame(predict(ames.lm,
                           newdata = data.frame(GrLivArea = new_x),
                           interval = "prediction",
                           level = 0.95))

output = data.frame(x = new_x, y_hat = y_hat,
                    ymean_lwr = ymean$lwr, ymean_upr = ymean$upr,
                    ypred_lwr = ypred$lwr, ypred_upr = ypred$upr)

# Plot
plot1 = ggplot(data = ames_train, aes(x = GrLivArea, y = SalePrice)) + 
  geom_point(color = "steelblue") + xlab("GrLivArea") +
  geom_line(data = output, aes(x = new_x, y = y_hat, color = "first"),lty=1) +
  geom_line(data = output, aes(x = new_x, y = ymean_lwr, lty = "second")) +
  geom_line(data = output, aes(x = new_x, y = ymean_upr, lty = "second")) +
  geom_line(data = output, aes(x = new_x, y = ypred_upr, lty = "third")) +
  geom_line(data = output, aes(x = new_x, y = ypred_lwr, lty = "third")) + 
  scale_colour_manual(values="darkorange", labels="Posterior Mean", name="") +
  scale_linetype_manual(values = c(1, 2), labels = c("Mean", "Predictions"),
                        name = "95% C.I.")
plot1

######## Fit Full model #######
# Use `bas.lm` to run regression model
cog.bas = bas.lm(SalePrice ~ ., data = ames_dummy, prior = "g-prior",
                 modelprior = Bernoulli(1), bestmodel = rep(1, ncol(ames_dummy)), n.models = 1)

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
                 modelprior = uniform())

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
