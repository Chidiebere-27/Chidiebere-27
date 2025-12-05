# PACKAGES AND LIBRARIES
install.packages("ggplot2")
install.packages("Hmisc")
install.packages("polycor")
install.packages("boot")
install.packages("Rcmdr")
install.packages("ggm")
install.packages("corrplot")
install.packages("QuantPsyc")
install.packages("tidyverse")
install.packages("lm.beta")
install.packages("caret")
install.packages("MASS")

library(ggplot2)
library(Hmisc)
library(polycor)
library(boot)
library(Rcmdr)
library(ggm)
library(corrplot)
library(QuantPsyc)
library(tidyverse)
library("lm.beta")
library(car)
library(caret)
library(MASS)

Sq# GRAPHS
# SCATTER PLOT
scatter <- ggplot(House_price_, aes(x = Square_Footage, y = House_Price))  + geom_point() + labs(x = "Square Footage", y = "House Price")
scatter + geom_point()+ geom_smooth(method = "lm" , se= F,  colour = "red", alpha = 0.1) + labs(x = "Square Footage", y = "House Price")
scatter + geom_point() + ggtitle("Square Footage vs House Price")
ggsave("graph1.png")

scatter <- ggplot(House_price_, aes(x = House_Price, y = Neighborhood_Quality))  + geom_point() + labs(x = "House price", y = "Neighbourhood quality")
scatter + geom_point()+ geom_smooth(method = "lm" , se= F,  colour = "blue", alpha = 0.1) + labs(x = "House price", y = "Neighbourhood quality")
scatter + geom_point() + ggtitle("Neighbourhood quality vs House Price")
ggsave("graph2.png") 

scatter <- ggplot(House_price_, aes(x = Num_Bedrooms, y = House_Price))  + geom_point() + labs(x = "Num Bedrooms", y = "House Price")
scatter + geom_point()+ geom_smooth(method = "lm" , se= F,  colour = "red", alpha = 0.1) + labs(x = "Num Bedrooms", y = "House Price")
scatter + geom_point() + ggtitle("House Price vs Num Bedrooms")
ggsave("graph3.png")

# Line graph
line <- ggplot(House_price_, aes(House_Price,Garage_Size))
line + stat_summary(fun=mean, geom= "line", aes(group=1), colour = "magenta", linetype = 1) +  ggtitle("Num of bedrooms vs Garage Size")
ggsave("graph4.png")

line <- ggplot(House_price_, aes(Num_Bedrooms,Square_Footage))
line + stat_summary(fun=mean, geom= "line", aes(group=1), colour = "Blue", linetype = 1) + ggtitle("Num of Bedrooms vs Square Footage ")
ggsave("graph5.png")

# Bar chart
bar <- ggplot(House_price_, aes(x = factor (Num_Bedrooms), y = (Num_Bathrooms)))
bar + stat_summary(fun="mean", geom="bar", fill="green", colour="black") + labs(x = "Num of bedrooms", y = "Num of Bedrooms") + theme_minimal()
ggsave("graph6.png")

# Box plot
House_price_Boxplot <- ggplot(House_price_, aes(x = Lot_Size, y = House_Price)) 
House_price_Boxplot + geom_boxplot() + labs(x = "Lot Size", y = "House Price") + ggtitle("Lot Size vs House Price")
ggsave("graph7.png") 

# Histogram
House_price_Histogram <- ggplot(House_price_, aes(House_Price))
House_price_Histogram + geom_histogram() + labs(x="House Price", y="Frequency") + theme(plot.title = element_text(hjust=0.5))
ggsave("graph8.png")


line <- ggplot(House_price_, aes(Num_Bedrooms,Num_Bathrooms))
line + stat_summary(fun=mean, geom= "line", aes(group=1), colour = "Blue", linetype = 1) + ggtitle("Num of Bedrooms vs Num of Bathrooms ")
ggsave("graph9.png")

piechart <- ggplot(House_price_, aes(x = "", y = House_Price, fill = Year_Built)) + geom_bar(width = 1 ,stat= "identity") 
  + coord_polar(theta = "y", start=0, direction = 1) + ggtitle(" Year Built") 

# CENTRAL TENDENCIES AND DISPERSION MEASURES
# HOUSE PRICE
mean_House_price <- mean(House_price_$House_Price, na.rm = TRUE)
print(mean_House_price)

mode_Num_Bedrooms <- mode(House_price_$Num_Bedrooms)
print(mode_Num_Bedrooms)

variance_House_price_ <- var(House_price_$House_Price)
print(variance_House_price_)

std_dev_House_price_ <- sd(House_price_$House_Price)
print(std_dev_House_price_)

# SQUARE FOOTAGE
mean_Square_Footage <- mean(House_price_$Square_Footage, na.rm = TRUE)
print(mean_Square_Footage)

mode_Num_Bathrooms <- mode(House_price_$Num_Bathrooms)
print(mode_Num_Bathrooms)

variance_Square_Footage <- var(House_price_$Square_Footage)
print(variance_Square_Footage)

std_dev_Square_Footage <- sd(House_price_$Square_Footage)
print(std_dev_Square_Footage)

# NUMBER OF BEDROOMS
mean_Num_Bedrooms <- mean(House_price_$Num_Bedrooms, na.rm = TRUE)
print(mean_Num_Bedrooms)

mode_Num_Bedrooms <- mode(House_price_$Num_Bedrooms)
print(mode_Num_Bedrooms)

variance_Num_Bedrooms <- var(House_price_$Num_Bedrooms)
print(variance_Num_Bedrooms)

std_dev_Num_Bedrooms <- sd(House_price_$Num_Bedrooms)
print(std_dev_Num_Bedrooms)

# NUMBER OF BATHROOMS
mean_Num_Bathrooms <- mean(House_price_$Num_Bathrooms, na.rm = TRUE)
print(mean_Num_Bathrooms)

mode_Num_Bathrooms <- mode(House_price_$Num_Bathrooms)
print(mode_Num_Bathrooms)

variance_Num_Bathrooms<- var(House_price_$Num_Bathrooms)
print(variance_Num_Bathrooms)

std_dev_Num_Bathrooms <- sd(House_price_$Num_Bathrooms)
print(std_dev_Num_Bathrooms)

# NEIGHBORHOOD QUALITY
mean_Neighborhood_Quality <- mean(House_price_$Neighborhood_Quality, na.rm = TRUE)
print(mean_Neighborhood_Quality)

mode_Neighborhood_Quality <- mode(House_price_$Neighborhood_Quality)
print(mode_Neighborhood_Quality)

variance_Neighborhood_Quality <- var(House_price_$Neighborhood_Quality)
print(variance_Neighborhood_Quality)

std_dev_Neighborhood_Quality <- sd(House_price_$Neighborhood_Quality)
print(std_dev_Neighborhood_Quality)

# LOT SIZE
mean_Lot_Size <- mean(House_price_$Lot_Size, na.rm = TRUE)
print(mean_Lot_Size)

mode_Lot_Size <- mode(House_price_$Lot_Size)
print(mode_Lot_Size)

variance_Lot_Size<- var(House_price_$Lot_Size)
print(variance_Lot_Size)

std_dev_Lot_Size <- sd(House_price_$Lot_Size)
print(std_dev_Lot_Size)

# GARAGE SIZE
mean_Garage_Size <- mean(House_price_$Garage_Size, na.rm = TRUE)
print(mean_Garage_Size)

mode_Garage_Size <- mode(House_price_$Garage_Size)
print(mode_Garage_Size)

variance_Garage_Size <- var(House_price_$Garage_Size)
print(variance_Garage_Size)

std_dev_Garage_Size <- sd(House_price_$Garage_Size)
print(std_dev_Garage_Size)

# CORRELATION
# cor()
correlation <- cor(House_price_$Num_Bedrooms,House_price_$House_Price, 
                   use = "complete.obs", method = "pearson")
print(correlation)
sum(is.na(House_price_$Num_Bedrooms))
sum(is.na(House_price_$House_Price))

correlation <- cor(House_price_$Num_Bathrooms,House_price_$House_Price, 
                   use = "complete.obs", method = "pearson")
print(correlation)

correlation <- cor(House_price_$Square_Footage,House_price_$House_Price, 
                   use = "complete.obs", method = "pearson")
print(correlation)

correlation <- cor(House_price_$Year_Built,House_price_$House_Price, 
                   use = "complete.obs", method = "pearson")
print(correlation)
correlation <- cor(House_price_$Lot_Size,House_price_$House_Price, 
                   use = "complete.obs", method = "pearson")
print(correlation)
correlation <- cor(House_price_$Garage_Size,House_price_$House_Price, 
                   use = "complete.obs", method = "pearson")
print(correlation)
correlation <- cor(House_price_$Neighborhood_Quality,House_price_$House_Price, 
                   use = "complete.obs", method = "pearson")
print(correlation)

# RCORR
data_matrix <- as.matrix(House_price_[, c("Num_Bedrooms", "Num_Bathrooms","Neighborhood_Quality", 
                                          "Year_Built", "Garage_Size","Square_Footage", "Lot_Size", "House_Price")])
correlation_matrix <- rcorr(data_matrix, type = "pearson")
print(correlation_matrix)

#COR TEST
correlation <- cor.test(House_price_$Square_Footage,House_price_$House_Price, 
                        alternative= "two.side", method = "pearson", conf.level=0.95)
print(correlation)

correlation <- cor.test(House_price_$Num_Bathrooms,House_price_$House_Price, 
                        alternative= "two.side", method = "pearson", conf.level=0.95)
print(correlation)
correlation <- cor.test(House_price_$Num_Bedrooms,House_price_$House_Price, 
                        alternative= "two.side", method = "pearson", conf.level=0.95)
print(correlation)
correlation <- cor.test(House_price_$Neighborhood_Quality,House_price_$House_Price, 
                        alternative= "two.side", method = "pearson", conf.level=0.95)
print(correlation)
correlation <- cor.test(House_price_$Garage_Size,House_price_$House_Price, 
                        alternative= "two.side", method = "pearson", conf.level=0.95)
print(correlation)
correlation <- cor.test(House_price_$Lot_Size,House_price_$House_Price, 
                        alternative= "two.side", method = "pearson", conf.level=0.95)
print(correlation)
correlation <- cor.test(House_price_$Year_Built,House_price_$House_Price, 
                        alternative= "two.side", method = "pearson", conf.level=0.95)
print(correlation)

# Perform significance tests for correlations with House_Price  (P value)
for (col in colnames(House_price_)[-c(1, ncol(House_price_))]) 
  test_result <- cor.test(House_price_[[col]], House_price_$House_Price, method = "pearson")
  cat(sprintf("Correlation between %s and House_Price: r = %.2f, p-value = %.4f\n",
              col, test_result$estimate, test_result$p.value))

# Creating a Heat map of the correlation matrix

# Select only numeric columns
numeric_House_price_ <- House_price_[, sapply(House_price_, is.numeric)]

# Calculate the correlation matrix
correlation_matrix <- cor(numeric_House_price_, use = "complete.obs")

# Plot the correlation matrix heat map
corrplot(correlation_matrix, 
         method = "color", 
         type = "lower", 
         tl.col = "black", 
         tl.srt = 35, 
         title = "Correlation Matrix Heatmap")

summary(House_price_)

# Model

test_House_price_Test <- c(995, 996,997,998, 999,1000)
test_House_price_Square_Footage <- c(1887,3261,3179,2606,4723,3268)
test_House_price_Num_Bedrooms <- c(2,4,1,4,5,4)
test_House_price_Num_Bathrooms <- c(1,1,2,2,2,2)
test_House_price_Year_Built <- c(2014,1978,1999,1962,1950,1983)
test_House_price_Lot_Size <- c(1.88,2.17,2.98,4.06,1.93,3.11)
test_House_price_Garage_Size <- c(0,2,1,0,0,2)
test_House_price_Neighborhood_Quality <- c(2,10,10,2,7,2)
test_House_price_House_Price <- rep(NA, length(test_House_price_Test))
test_House_price_ <- data.frame(Test = test_House_price_Test, Square_Footage = test_House_price_Square_Footage,
                                Num_Bedrooms = test_House_price_Num_Bedrooms, Num_Bathrooms = test_House_price_Num_Bathrooms,
                                Year_Built = test_House_price_Year_Built, Lot_Size = test_House_price_Lot_Size, Garage_Size = test_House_price_Garage_Size,
                                Neighborhood_Quality = test_House_price_Neighborhood_Quality, House_Price = test_House_price_House_Price)

print(test_House_price_)

## Build the Multivariate Linear Regression Model
correlation_matrix <- cor(House_price_[, c("Square_Footage", "Num_Bedrooms", 
                                 "Num_Bathrooms", "Year_Built", 
                                 "Lot_Size", "Garage_Size", 
                                 "Neighborhood_Quality")],
                          use = "complete.obs")
print(correlation_matrix)

correlation_matrix <- cor(test_House_price_[, c("Square_Footage", "Num_Bedrooms", 
                                           "Num_Bathrooms", "Year_Built", 
                                           "Lot_Size", "Garage_Size", 
                                           "Neighborhood_Quality")],
                          use = "complete.obs")
print(correlation_matrix)
# Backward Elimination
full_model <- lm(House_Price ~ ., data = House_price_)
summary(full_model)

# Performing  backward elimination using stepAIC
final_model <- stepAIC(full_model, direction = "backward")
summary(final_model)
predicted_prices <- predict(final_model, newdata = test_House_price_)
test_House_price_$House_Price <- predicted_prices
# View the test data set with predicted prices
print(test_House_price_)

plot(final_model)
lm.beta(final_model)
confint(final_model)
resid(final_model)
# Calculate AIC
aic_value <- AIC(final_model)
print(aic_value)


vif(final_model)
mean(final_model)
cooksd <- cooks.distance(final_model)
plot(cooksd, type = "h", main = "Cook's Distance", ylab = "Cook's Distance", xlab = "Observation Index")
abline(h = 4/(nrow(df)), col = "red", lty = 2)  # Common threshold for high influence

influential_points <- which(cooksd > (4 / nrow(df)))
print(influential_points)

# Compute leverage values (Hat values)
leverage_values <- hatvalues(final_model)
print(leverage_values)
# Set leverage threshold (2p/n rule)
n <- nrow(df)  # Number of observations
p <- length(coef(final_model))  # Number of predictors including intercept
leverage_threshold <- (2 * p) / n

# Identify high leverage points
high_leverage_points <- which(leverage_values > leverage_threshold)

# Print results
print(paste("Leverage threshold:", leverage_threshold))
print("High leverage points (row indices):")
print(high_leverage_points)
plot(leverage_values, type = "h", main = "Leverage Values", ylab = "Leverage", xlab = "Observation Index")
abline(h = leverage_threshold, col = "red", lty = 2)  # Add threshold line


# Simplified model (Assumption with better predictors)
model <- lm(House_Price ~ Square_Footage+ Year_Built + Lot_Size, data = House_price_)
summary(model)
# predict prices
predicted_prices <- predict(model, newdata = test_House_price_)
test_House_price_$House_Price <- predicted_prices
# View the test data set with predicted prices
print(test_House_price_)

plot(model)
lm.beta(model)
confint(model)
resid(model)
# Calculate AIC
aic_value <- AIC(final_model)
print(aic_value)


vif(model)
mean(model)
cooksd <- cooks.distance(model)
plot(cooksd, type = "h", main = "Cook's Distance", ylab = "Cook's Distance", xlab = "Observation Index")
abline(h = 4/(nrow(df)), col = "red", lty = 2)  # Common threshold for high influence

influential_points <- which(cooksd > (4 / nrow(df)))
print(influential_points)

# Compute leverage values (Hat values)
leverage_values <- hatvalues(model)
print(leverage_values)
# Set leverage threshold (2p/n rule)
n <- nrow(df)  # Number of observations
p <- length(coef(model))  # Number of predictors including intercept
leverage_threshold <- (2 * p) / n

# Identify high leverage points
high_leverage_points <- which(leverage_values > leverage_threshold)

# Print results
print(paste("Leverage threshold:", leverage_threshold))
print("High leverage points (row indices):")
print(high_leverage_points)
plot(leverage_values, type = "h", main = "Leverage Values", ylab = "Leverage", xlab = "Observation Index")
abline(h = leverage_threshold, col = "red", lty = 2)  # Add threshold line

