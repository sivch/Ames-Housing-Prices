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
                                      remove_selected_columns	= TRUE, 
                                      remove_first_dummy = TRUE) # keep n-1 dummies

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

######## Split into training and test ##########
## 80% of the sample size
train_size <- floor(0.8 * nrow(ames_dummy))
## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(ames_dummy)), size = train_size)

ames_train <- ames_dummy[train_ind, ]
ames_test <- ames_dummy[-train_ind, ]

######## Normalize #########
## ? should we normalize? 

# Select a data subset
sub.idx = 1:12  
num.data = 681

# Define outcome and design matrix
X <- as.matrix(ames_dummy[train_ind,!(names(ames_dummy) %in% c("SalePrice"))])
Y <- as.vector(ames_dummy[train_ind,"SalePrice"])

# Get dimensions
N <- dim(X)[1]
p <- dim(X)[2]

# Define model
cat(
  "
  model {
    # Likelihood 
    for (i in 1:N) {
    	mu[i] <- beta0 + inprod(X[i,], beta)
      z[i]  <- phi(mu[i])
    	Y[i] ~ dbern(z[i]) # usual probit regression model 
    } 
  
    # Tracing the visited model
    for (j in 1:p) {
    	TempIndicator[j] <- g[j]*pow(2, j) 
    }
    mdl <- 1 + sum(TempIndicator[]) # model index in binary coding 
  
    # Gaussian distribution is parametrized in terms of precision parameter
    beta0 ~ dnorm(0, 0.001)
  
    for(j in 1:p) {
    	tprior[j] <- 1 / var_beta[j]
    	bprior[j] <- 0
    }
  
    for(j in 1:p) {
    	beta_temp[j] ~ dnorm(bprior[j], tprior[j])
    	g[j] ~ dbern(theta[j])
    	theta[j] ~ dunif(0,1)
    	beta[j] <- g[j] * beta_temp[j]	
    }
  }
  "
  , file = "models/SpSl_probut.bug")

# Data to pass to JAGS
data_JAGS_SpSl <- list(N = N, p = p, Y = Y, X = as.matrix(X), var_beta = rep(1, p))

# A list of initial value for the MCMC algorithm 
inits = function() {
  list(beta0 = 0.0, beta_temp = rep(0,p), g = rep(0,p), theta = rep(0.5, p),
       .RNG.seed = 321, .RNG.name = 'base::Wichmann-Hill') 
}

# Compile model (+ adaptation)
model <- jags.model("models/SpSl_probut.bug", data = data_JAGS_SpSl,
                    n.adapt = 1000, inits = inits, n.chains = 1) 