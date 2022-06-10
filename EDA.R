library("ggplot2")

# Read data 
ames <- read.csv("ameshouse.txt", sep="")
ames$SalePrice <- ames$SalePrice/1000
ames <- data.frame(ames)

# Stats
print(ncol(ames))
print(nrow(ames))
str(ames)

# Visualize target
require(gridExtra)
options(scipen=10000)
p1 <- ggplot(df, aes(x=SalePrice)) + geom_density() + labs(x="Sale Price (in thousand $)")
p2 <- ggplot(df, aes(x=SalePrice)) + geom_density() +  scale_x_log10()  + labs(x="log(Sale Price)", y = "")
grid.arrange(p1, p2, ncol=2)

# Check number of missing values 
data.frame(sort(colSums(is.na(ames)), decreasing = TRUE)[0:20])

# Remove features FireplaceQu, LotFrontage, secondary garage features
drop_cols <- c("FireplaceQu","LotFrontage", "GarageType", "GarageYrBlt","GarageFinish","GarageQual","GarageCond")
ames <- ames[,!(names(ames) %in% drop_cols)]

# Remove rows with missing values 
ames <- ames[which(complete.cases(ames)),]

# Check for missing values
data.frame(sort(colSums(is.na(ames)), decreasing = TRUE)[0:20])

# Scatter plot of continous variables
nums <- unlist(lapply(ames, is.numeric)) 
ames_num <- ames[ , nums]
plot(ames$SalePrice, ames$nums[2], ylab = ames_num[2])
pairs(ames_num)
