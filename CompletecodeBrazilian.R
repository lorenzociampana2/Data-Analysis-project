

### INTRODUCTION TO THE RSCRIPT ###

#### Rscript Final Project Brazilian Houses ####



#### In this script, you can find the code we used for our analysis. We will also include some code we tried at the end of the section
#### but we did not include in the final analysis (other models/trials). However, the RMarkdown provides the immediate code below,
#### that is the used code and hence, the relevant one.

#### Matteo Carucci, Alessandro Natoli, Tommaso Agudio and Lorenzo Ciampana ####
---------------------------------------------------------------------------------------------------------------------------------------------
  
#### 1. Understanding the dataset ####

#importing libraries
library(dplyr)
library(ggplot2)
library(cowplot)
library(corrplot)
library(GGally)
library(gridExtra)
library(glmnet)
library(MASS)
library(caret)
library(randomForest)
library(mgcv)
library(purrr)
library(cowplot)
library(tidyverse)
library(factoextra)
library(cluster)
library(patchwork)



#### 1.1 Data cleaning, getting rid of nulls. ####

#importing the data! (CHANGE THE DIRECTORY)
data <- read.csv("C:/BrazHousesRent.csv")

# Head and structure of the dataset
#head(data)
#str(data)

#the floor is a character but should be a number, let's convert it!
  data$floor <- as.numeric(data$floor)

#missing values?
sapply(data, function(x) sum(is.na(x)))  #no null in df except floors!
sapply(data, function(x) sum(x == 0))   #there are many values == 0!, still makes sense, some houses may not have parking spaces and taxes


#duplicated(data)  #some duplicates in the df
data <- data[!duplicated(data),]  #dataframe with no duplicates


#### 1.2 What about missing floor data?

#How do we treat nulls? We could either use Median or mean imputation but MICE usually works very well!
library(mice)
data_imputed <- data


# Create the imputation object. We will use a simple pmm.
set.seed(123)
imp <- mice(data_imputed, method = "pmm")
imputed <- complete(imp)[,"floor"]

# replaced imputed data with previous (nonetheless floor should matter nothing to the regression!)
data_imputed$floor[is.na(data_imputed$floor)] <- imputed[is.na(data_imputed$floor)]  #replacing the new imputed with previous


#let us factor the other categorical columns before starting
data_imputed$city <- as.factor(data_imputed$city)
data_imputed$animal <- as.factor(data_imputed$animal)
data_imputed$furniture <- as.factor(data_imputed$furniture)


#rename columns with better names
data_imputed <- data_imputed %>%
  rename(hoa = hoa..R.., rent = rent.amount..R.., proptax = property.tax..R..
         , fireins = fire.insurance..R..)


#a little summary of new df
summary(data_imputed)

--------------------------------------------------------------------------------------------------------------------------------------------
  
## 2. EDA, is there anything affecting the rental prices? ####


#boxplot and histograms with density lines for interesting numerical columns
num_cols <- sapply(data_imputed, is.numeric)
cat_cols <- sapply(data_imputed,is.factor)
numcols1 <- c("area", "hoa", "rent", "proptax", "fireins")

#storing the plots
p_boxplot <- list()
p_boxplot1 <- list()
p_histogram <- list()
p_histogram1 <- list()

#plots
for (col in names(data_imputed[numcols1])) {
  #boxplot
  p_boxplot[[col]] <- ggplot(data_imputed, aes(y = !!sym(col))) +
    geom_boxplot(fill = "lightblue", alpha = 0.5, outlier.color = "red", outlier.shape = 1) +
    labs(title = paste0(col," boxplot"), x = "") +
    theme_bw()
  
  #histogram
  p_histogram[[col]] <- ggplot(data_imputed, aes(x = !!sym(col))) +
    geom_histogram(fill = "lightblue", alpha = 0.5) +
    geom_freqpoly(color = "lightblue", size = 0.05) +
    labs(title = paste0(col," hist"), y = "", x = "") +
    theme_bw()
  
  #boxplot with log scaled data
  p_boxplot1[[col]] <- ggplot(log(data_imputed[,numcols1]), aes(y = !!sym(col))) +
    geom_boxplot(fill = "lightblue", alpha = 0.5, outlier.color = "red", outlier.shape = 1) +
    labs(title = paste0("log scaled Boxplot ", col), x = "") +
    theme_bw()
  
  #histogram with scaled data
  p_histogram1[[col]] <- ggplot(log(data_imputed[,numcols1]), aes(x = !!sym(col))) +
    geom_histogram(fill = "lightblue", alpha = 0.5) +
    geom_freqpoly(color = "lightblue", size = 0.05) +
    labs(title = paste0("log scaled hist ", col), y = "", x = "") +
    theme_bw() 
  
  # all takes too much space
  print(plot_grid(p_boxplot, p_histogram, p_boxplot1, p_histogram1, ncol = 4))
}

#showing area and proptax to see remarkably high-skeweness 
print(plot_grid(p_boxplot$area, p_histogram$area, p_boxplot1$area, p_histogram1$area, ncol = 4))
print(plot_grid(p_boxplot$proptax, p_histogram$proptax, p_boxplot1$proptax, p_histogram1$proptax, ncol = 4))


--------------------------------------------------------------------------------------------------------------------------------------------
#### 2.1 Removing the *outliers*: some houses data just don't make sense ####

  # Columns to consider for outlier detection
cols <- c("area", "hoa","rent", "proptax")
data_out <- data_imputed[, cols]

#using z-scores method
z_scores <- apply(data_out, 2, function(x) abs((x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)))

# Identify rows with at least one z-score greater than 3 (considered outliers)
outlier_rows <- row.names(data_out)[apply(z_scores, 1, function(x) any(x > 3))]
# Print the number of outliers detected
cat("Number of outliers detected:", length(outlier_rows), "\n")

#new df without outliers
data3 <- data_imputed[!(rownames(data_imputed) %in% outlier_rows), ]


#For example, we visualize the boxplots and the q-q plots of the rent distribution with and with no outliers, the difference is #striking!
par(mfrow=c(2,2))
options(repr.plot.width=12, repr.plot.height=6)
boxplot(data_imputed$rent, col = "lightblue3", horizontal = T, lwd = 4,
        main = "Rent - Before Removing Outliers")
qqnorm(data_imputed$rent)

boxplot(data3$rent, col = "palegreen3", horizontal = T, lwd = 4,
        main = "Rent - After Removing Outliers")
qqnorm(data3$rent)

--------------------------------------------------------------------------------------------------------------------------------------------
  
#### 3. Correlation among variables: Can it affect our model? ####

#correlation matrix with numericals
corr_matrix <- cor(select_if(data3, is.numeric))

# heatmap with fancy colors (=

corr <- corrplot(corr_matrix, method = "color", type = "lower", order = "hclust",
         addCoef.col = "black", tl.cex = 0.8, cl.cex = 0.8, 
         col = colorRampPalette(c("#313695", "#4575B4", "#74ADD1", "#ABD9E9", 
                                  "#E0F3F8", "#FFFFBF", "#FEE090", "#FDAE61", 
                                  "#F46D43", "#D73027", "#A50026"))(200))

                                               
--------------------------------------------------------------------------------------------------------------------------------------------
  

#### 3.2 Fitting with one predictor - is there linearity? ####

#feats to consider
feat <- c("proptax", "fireins", "rooms", "area")
target <- "rent"

#fitted a simple regression with most correlated as predictors (also conf.int at 95% shown, even though not visible)
p_list <- list() 
for (i in 1:length(feat)) {
  p <- ggplot(data3, aes(x = !!sym(feat[i]), y = !!sym(target))) +
    geom_point(cex = 3, pch = 1, stroke = 2, color="palegreen3") +
    geom_smooth(method = "lm", color = "green4", lwd = 3, formula = "y~x", se = TRUE, level = 0.95, size = 3) +
    theme_light(base_size = 16) +
    ggtitle(paste("Scatter plot of", feat[i], "vs", target))
  
  p_list[[i]] <- p
}

#showing the plots
grid.arrange(grobs = p_list, ncol = 2) 

#a closer look into fireins
p_list[[2]]





--------------------------------------------------------------------------------------------------------------------------------------------
  

#### 3.4 Categorical variables inspection ####

#boxplots for rental prices by category, in red outliers

newcol <- c("furniture", "city")
plots <- list()

for (col in names(data3[newcol])) {
  p_box <- ggplot(data3, aes(x = !!sym(col), y = rent, fill = factor(.data[[col]]))) +
    geom_boxplot(outlier.shape = 1, outlier.size = 2, outlier.color = "red") +
    labs(x = col, y = "Rent Amount") +
    theme_bw()
  
  p_hist <- ggplot(data3, aes(x = !!sym(col))) +
    geom_bar(fill = "blue", alpha = 0.5) +
    labs(title = "Category Frequency", y = "Frequency", x = "Category") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
  plots[[col]] <- arrangeGrob(p_box, p_hist, nrow = 2)
}

# Arrange and display the plots
final_plot <- grid.arrange(grobs = plots, ncol = 2)
print(final_plot)


#Rent ditribution by City (including only those having rent less than 8000)
library(RColorBrewer)
pal <- brewer.pal(7, "Greens")

#average rent by city
mu <- data3 %>%
  filter(rent < 2000) %>%
  group_by(city) %>%
  summarise(grp.mean = mean(area))

options(repr.plot.width = 12, repr.plot.height = 7)

data3 %>%
  filter(rent < 8000) %>%
  ggplot(aes(x = rent, color = city)) +
  geom_density(lwd = 3) +
  scale_color_manual(values = pal) +
  geom_vline(data = mu, aes(xintercept = grp.mean, color = city),
             linetype = "dashed", lwd = 2) +
  labs(title = "Rent Distributions by City", x = "Rent") +
  theme_light(base_size = 18) +
  theme(legend.position = "bottom")


--------------------------------------------------------------------------------------------------------------------------------------------
#### AIC MODELS AND ELASTICNET ####  

#except fireins, what are the most important predictors?
#prepping data
set.seed(123)

# Split the data into training and valid(test) sets (80-20 as usual)
train_indices <- sample(1:nrow(data3), 0.8*nrow(data3))
train_data <- data3[train_indices, ]
test_data <- data3[-train_indices, ]

#excluding fireins
predictors <- setdiff(names(data3),c("fireins", "rent")) 
train_x <- train_data[, predictors]
train_y <- train_data$rent 
test_x <- test_data[,predictors]
test_y <- test_data

#including fireins
train_x2 <- train_data[, -which(names(train_data) == "rent")]
train_y2 <- train_data$rent
test_x2 <- test_data[, -which(names(test_data) == "rent")]
test_y2 <- as.vector(test_data$rent)

#Elastic net with cv (5 folds) including fireins
#Elastic Net cannot have factors! (cat var in general) hence we encode them!
train_city_encoded <- model.matrix(~ city - 1, data = train_data)
test_city_encoded <- model.matrix(~ city - 1, data = test_data)

train_animal_encoded <- model.matrix(~ animal - 1, data = train_data)
test_animal_encoded <- model.matrix(~ animal - 1, data = test_data)

train_furniture_encoded <- model.matrix(~ furniture - 1, data = train_data)
test_furniture_encoded <- model.matrix(~ furniture - 1, data = test_data)

#encoded variables with the remaining predictors
train_x2net <- cbind(train_data[, -which(names(train_data) %in% c("rent", "city", "animal", "furniture"))], train_city_encoded, train_animal_encoded, train_furniture_encoded)
test_x2net <- cbind(test_data[, -which(names(test_data) %in% c("rent", "city", "animal", "furniture"))], test_city_encoded, test_animal_encoded, test_furniture_encoded)

#scaling is important in penalized approaches, we only scale numericals
# numeric variables (we also include those "categoricals", they have too many levels!)
numeric_vars1 <- c("area","rooms","bathroom","parking.spaces","floor","hoa","proptax","fireins")

# Scale the numeric variables
scaled_train_x2net <- train_x2net
scaled_train_x2net[, numeric_vars1] <- scale(train_x2net[, numeric_vars1])
scaled_test_x2net <- test_x2net
scaled_test_x2net[, numeric_vars1] <- scale(test_x2net[, numeric_vars1])


# Convert the data frames to matrix format
train_x2net <- as.matrix(scaled_train_x2net)
test_x2net <- as.matrix(scaled_test_x2net)


#tuning lambda with cv (we don't prioritize L1 or L2, mixed approach is best hence alpha = 0.5)
enet_model <- cv.glmnet(train_x2net, train_y2, alpha = 0.5, nfolds = 10)

#optimal lambda value (we are less conservative and we use min)
lambda_min <- enet_model$lambda.min
#lambda_1se <- enet_model$lambda.1se
#fitting
enet_model_fit <- glmnet(train_x2net, train_y2, alpha = 0.5, lambda = lambda_min)

#preds,RMSE and coefficients
predictions_enet <- predict(enet_model_fit, newx = test_x2net)
# rescaled_predictions_enet <- rescale(scaled_predictions_enet, original_scale)

rmse_enet <- sqrt(mean((predictions_enet - test_y2)^2))


# Function to calculate R2
calculate_R2 <- function(y_true, y_pred) {
  y_mean <- mean(y_true)
  ss_total <- sum((y_true - y_mean)^2)
  ss_residual <- sum((y_true - y_pred)^2)
  R2 <- 1 - (ss_residual / ss_total)
  return(R2)
}

#df to store the model performance indicators
performance_df <- data.frame(Model = character(), RMSE = numeric(), R2 = numeric(), stringsAsFactors = FALSE)


#appending model performance
r2_enet <- calculate_R2(test_y2, predictions_enet)
performance_df <- rbind(performance_df, c("Elastic Net", rmse_enet, r2_enet))


#AIC with validation set (No need to scale data)

#AIC criterion (model with fireins and without it, bac and forward elimination included!)
lm_aic <- stepAIC(lm(rent ~ ., data = train_data), direction = "both", trace = FALSE)
lm_aic2 <- stepAIC(lm(rent ~ hoa + proptax + area + rooms + city + bathroom + floor + parking.spaces + furniture + animal, data = train_data), direction = "both", trace = FALSE)

#model with some feature engineering to reduce predictors (combining correlated)
lm_aic3 <- stepAIC(lm(rent ~ hoa*proptax + area*rooms + city + bathroom +  parking.spaces + fireins + furniture, data = train_data), direction = "both", trace = FALSE)


#complete model performance
predictors1 <- setdiff(names(data3),c("rent")) 
test3 <- test_data[,predictors1]
predictions_aic <- predict(lm_aic, newdata = test3)
rmse_aic <- sqrt(mean((predictions_aic - test_y2)^2))
R2_aic <- calculate_R2(test_y2, predictions_aic)
performance_df <- rbind(performance_df, c("Linear Model AIC complete", rmse_aic, R2_aic))

#no fireins model
predictions_aic2 <- predict(lm_aic2, newdata = test3)
rmse_aic2 <- sqrt(mean((predictions_aic2 - test_y2)^2))
R2_aic2 <- calculate_R2(test_y2, predictions_aic2)
performance_df <- rbind(performance_df, c("Linear Model AIC2", rmse_aic2, R2_aic2))

#model with no animal and new engineered features
predictions_aic3 <- predict(lm_aic3, newdata = test3)
rmse_aic3 <- sqrt(mean((predictions_aic3 - test_y2)^2))
R2_aic3 <- calculate_R2(test_y2, predictions_aic3)
performance_df <- rbind(performance_df, c("Linear Model AIC3", rmse_aic3, R2_aic3))


# Summary of the 4 models (WITH COEFFICIENTS ONLY FOR ENET)
summary(lm_aic)
summary(lm_aic2)
summary(lm_aic3)
#elastic net coefficients
coef(enet_model_fit)


#are residuals normally distributed?

# Best performing Model lm_aic
residuals_aic3 <- residuals(lm_aic3)
#hist
hist(residuals_aic3, breaks = 20, main = "Residuals - Model lm_aic feature engineering", xlab = "Residuals")

#Model lm_aic2
residuals_aic2 <- residuals(lm_aic2)
hist(residuals_aic2, breaks = 20, main = "Residuals - Model lm_aic ", xlab = "Residuals")


#showing the performances of the models
colnames(performance_df) <- c("Model", "RMSE", "R-Squared")
performance_df

--------------------------------------------------------------------------------------------------------------------------------------------

#### 5. Fancier methods: Random Forest and GAM Splines ####


#Random forest
# increasing mtry to equal all the predictor variables is bagging!

m.randomForest.bag <- randomForest(x = train_x2, y = train_y2,
                                   ntrees = 500,
                                   mtry = 4);

#predictions and measures
predictionsrfbag <- predict(m.randomForest.bag, newdata = test3)
rmserfbag <- sqrt(mean((predictionsrfbag - test_data$rent)^2))
r_squaredrf <- cor(predictionsrfbag, test_data$rent)^2
performance_df <- rbind(performance_df, c("Random forest Complete", rmserfbag, r_squaredrf))


#random forest without fireins
m.randomForest.bag2 <- randomForest(x = train_x, y = train_y,
                                    ntrees = 500,
                                    mtry = 4);

#predictions
predictionsrfbag2 <- predict(m.randomForest.bag2, newdata = test3)
rmserfbag2 <- sqrt(mean((predictionsrfbag2 - test_data$rent)^2))
r_squaredrf2 <- cor(predictionsrfbag2, test_data$rent)^2
performance_df <- rbind(performance_df, c("Random forest no fireins", rmserfbag2, r_squaredrf2))



#splines functions applied to variables less correlated with target(likely non-linear relationships!))
m.gam.spline <- gam(rent ~ s(hoa) + s(proptax) + area + rooms + city + bathroom + floor + parking.spaces + fireins + furniture, data = train_data)
#summary(m.gam.spline)

#performance GAM complete
predictionsgam <- predict(m.gam.spline, newdata = test3)
rmserfgam <- sqrt(mean((predictionsgam - test_data$rent)^2))
r_squaredgam <- cor(predictionsgam, test_data$rent)^2

performance_df <- rbind(performance_df, c("Gam1", rmserfgam, r_squaredgam))



#second model aims at mitigating correlation among predictors!
m.gam.spline1 <- gam(rent ~ s(hoa * proptax) + s(area * rooms) + city + bathroom + floor + parking.spaces + s(fireins) + furniture, data = train_data)
#summary(m.gam.spline1)

#predictions gam1
predictionsgam1 <- predict(m.gam.spline1, newdata = test3)
rmserfgam1 <- sqrt(mean((predictionsgam1 - test_data$rent)^2))
r_squaredgam1 <- cor(predictionsgam1, test_data$rent)^2
performance_df <- rbind(performance_df, c("Gam with feature engineering", rmserfgam1, r_squaredgam1))


performance_df

--------------------------------------------------------------------------------------------------------------------------------------------

#### 5.4 The variance-bias trade off, a personalized "bootstrapping". ####



#WE USED A SMALL PORTION OF THE DATA TO DO BOOTSTRAPPING. TAKING THE WHOLE DATASET ROUGHLY
#SHOWED THE SAME PERFORMANCES (SOME IMPROMOVEMENTS FOR RF AS IT CAN TRAIN WITH MORE DATA). THERE IS NO RISK TO EXCLUDE SOME IMPORTANT VALUES, AS 10 ITERATIONS SHOULD ENSURE ENOUGH VARIABILITY IN SAMPLES.
#TAKES LONG BUT IT IS A RELIABLE WAY TO ESTIMATE GENERAL MODELS' PERFORMANCE

#lists to store performance results
plot1 <- list()
plot2 <- list()
plot3 <- list()


# Bootstrapping for each model and compute RMSE and R^2 (not shown but still important)
num_iterations <- 10  
set.seed(123)  
for (i in 1:num_iterations) {
  # Split data into train and test sets (TAKING 30% sample data and splitting 80-20)
  train_datasample <- data3[sample(nrow(data3), size = round(0.3 * nrow(data3))), ]
  train_indices <- sample(nrow(train_datasample), size = floor(0.8 * nrow(train_datasample)), replace = FALSE)
  train_setcv <- train_datasample[train_indices, ]
  test_setcv <- train_datasample[-train_indices, ]
  
  traincv <- train_setcv[,-which(names(train_datasample) == "rent")]
  test_cv <- test_setcv[,-which(names(train_datasample) == "rent")]
  testy_cv <- test_setcv$rent
  
  # Random Forest with bagging
  m.randomForest.bagcv <- randomForest(x = traincv, y = train_setcv$rent,
                                       ntrees = 500,
                                       mtry = 4)
  
  predictionsrfbagcv <- predict(m.randomForest.bagcv, newdata = test_cv)
  rmserfbagcv <- 0
  r_squaredrfcv <- 0
  rmserfbagcv <-   sqrt(mean((predictionsrfbagcv - testy_cv)^2))
  r_squaredrfcv <- cor(predictionsrfbagcv, testy_cv)^2
  plot1[i] <- rmserfbagcv
  
  #Best AIC
  lm_aic3cv <- stepAIC(lm(rent ~ hoa*proptax + area*rooms + city + bathroom +  parking.spaces + fireins + furniture, data = train_setcv), direction = "both", trace = FALSE)
  predictions_aic3cv <- predict(lm_aic3cv, newdata = test_cv)
  rmse_aic3cv <- 0
  R2_aic3cv <- 0
  rmse_aic3cv <- sqrt(mean((predictions_aic3cv - testy_cv)^2))
  R2_aic3cv <- calculate_R2(testy_cv, predictions_aic3cv)
  plot2[i] <- rmse_aic3cv
  
  
  # GAM with interaction terms
  m.gam.spline1cv <- gam(rent ~ s(hoa * proptax) + s(area * rooms) + city + bathroom + floor + parking.spaces + s(fireins) + furniture, data = train_setcv)
  predictionsgam1 <- predict(m.gam.spline1cv, newdata = test_cv)
  rmserfgam1cv <- 0
  r_squaredgam1cv <- 0
  rmserfgam1cv <- sqrt(mean((predictionsgam1 - testy_cv)^2))
  r_squaredgam1cv <- cor(predictionsgam1, testy_cv)^2
  plot3[i] <- rmserfgam1cv
  
}


# Plotting the variation of RMSE for each model
#lists to vector to compute CI
plot1 <- unlist(plot1)
plot2 <- unlist(plot2)
plot3 <- unlist(plot3)

# Calculate mean and standard deviation of predictions for each model
mean_plot1 <- mean(plot1)
sd_plot1 <- sd(plot1)

mean_plot2 <- mean(plot2)
sd_plot2 <- sd(plot2)

mean_plot3 <- mean(plot3)
sd_plot3 <- sd(plot3)

# Calculate confidence intervals (95%)
ci_plot1 <- quantile(plot1, probs = c(0.025, 0.975))
ci_plot2 <- quantile(plot2, probs = c(0.025, 0.975))
ci_plot3 <- quantile(plot3, probs = c(0.025, 0.975))

# Plot mean predictions with confidence intervals
plot_data <- data.frame(
  Model = c("Random Forest", "AIC Linear Model", "GAM spline"),
  Mean = c(mean_plot1, mean_plot2, mean_plot3),
  Lower_CI = c(ci_plot1[1], ci_plot2[1], ci_plot3[1]),
  Upper_CI = c(ci_plot1[2], ci_plot2[2], ci_plot3[2])
)

# plot
p <- ggplot(plot_data, aes(x = Model, y = Mean)) +
  geom_errorbar(aes(ymin = Lower_CI, ymax = Upper_CI), width = 0.2, color = "blue") +
  geom_point(color = "blue") +
  xlab("Model") +
  ylab("Predictions") +
  ggtitle("Average RMSE with Confidence Intervals")

print(p)


--------------------------------------------------------------------------------------------------------------------------------------------


  
#### 6. Clustering: Can we identify the most profitable and convenient houses? ####

### KMEANS ###
  
#scaled data (numeric only)
data_scaled <- scale(data3[,numeric_vars1])
data_scaled <- as.data.frame(data_scaled)

#elbow method
k_values <- 1:15  # Range of k values to consider
withinss <- numeric(length(k_values))

for (i in seq_along(k_values)) {
  k <- k_values[i]
  kmeans_result <- kmeans(data_scaled, centers = k)
  withinss[i] <- kmeans_result$tot.withinss
}

# Plot the elbow curve
elb <- plot(k_values, withinss, type = "b", pch = 19, frame = FALSE,
            xlab = "Number of Clusters (k)", ylab = "Within-cluster Sum of Squares")


#silhouette score by k and elbow printed
silk <- fviz_nbclust(data_scaled, kmeans, method='silhouette')
grid.arrange(elb, silk, ncol = 1)
print(elb)




#### 6.1 The best k for Kmeans.


#2 clusters plot

kmeans_model <- kmeans(data_scaled, centers = 2, nstart = 25)
clus2 <- fviz_cluster(kmeans_model, data = data_scaled, geom = "point",
                      main = paste("K-Means Clustering (k =", 2, ")"))

#3 clusters plot
kmeans_model2 <- kmeans(data_scaled, centers = 3, nstart = 25)
clus3 <- fviz_cluster(kmeans_model2, data = data_scaled, geom = "point",
                      main = paste("K-Means Clustering (k =", 3, ")"))

#plottig results
grid.arrange(clus2, clus3, ncol = 2)

--------------------------------------------------------------------------------------------------------------------------------------------

#### 6.2 Hierarchical clustering. ####


#euclidean distance works best
dist_euc <- dist(data_scaled, method = "euclidean")
hc_euclidean <- hclust(dist_euc, method = "ward.D2")

#dendogram (2 or 3 ideal)
plot(hc_euclidean, cex = 0.6, main = "Dendrogram (Euclidean distance)")

#assessing the results fr different k
silhouette_euclidean <- rep(0, 3)
for (k in 2:4) {
  
  # Compute cluster assignments for euclidean distance
  hc_euclidean_k <- cutree(hc_euclidean, k = k)
  
  # Compute silhouette score for euclidean distance
  sileucscores <- data.frame(silhouette(hc_euclidean_k, dist_euc))
  mean_sil_width <- mean(as.numeric(sileucscores$sil_width))         
  silhouette_euclidean[k - 1] <-  mean_sil_width
  
}
cat("Silhouette scores for euclidean distance:", silhouette_euclidean, "\n")




#### 6.3 Dendogram insights.


# Iterate over different values of k
plot_list3 <- list()  # initialize list for euclidean distance plots

for (k in 2:4) {
  
  # Compute cluster assignments for euclidean distance
  hc_euclidean_k <- cutree(hc_euclidean, k = k)
  
  # Plot clusters for euclidean distance
  plot_title <- paste0("Hierarchical Clusters (Euclidean distance) for k =", k)
  plot_title <- gsub(" ", "_", plot_title)
  plot_list3[[k-1]] <- fviz_cluster(list(data = data_scaled, cluster = hc_euclidean_k), 
                                     geom = "point", 
                                     palette = "jco", 
                                     main = plot_title) + theme_bw()
  
}

#showing plots for k = 2 to 4
grid.arrange(grobs = plot_list3, nrow = 2, ncol = 2, top = "Clusters (Euclidean #distance)") 



--------------------------------------------------------------------------------------------------------------------------------------------
  

#### 7. Do these clusters really tell what we want? ####


data_new <- data3

#3 clusters
kmeans_result3 <- kmeans(data_scaled, centers = 3, nstart = 25)
data_new$clusters3 <- kmeans_result3$cluster

#2 clusters
kmeans_result2 <- kmeans(data_scaled, centers = 2, nstart = 25)
data_new$clusters2 <- kmeans_result2$cluster


#Do clusters regroup well? What are prices by city and area?

# Create the boxplot for city with regrouped clusters2
box1 <- ggplot(data_new, aes(x = interaction(clusters3, city), y = rent)) +
  geom_boxplot(fill = "lightgray", color = "black") +
  labs(x = "clusters2", y = "Rent") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("Rental prices by city and cluster")

#by area
box2 <- ggplot(data_new, aes(x = interaction(clusters3, city), y = area)) +
  geom_boxplot(fill = "lightgray", color = "black") +
  labs(x = "clusters2", y = "area") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("Houses' area by cluster and city")


#by furniture?

box3 <- ggplot(data_new, aes(x = interaction(clusters3, furniture), y = rent)) +
  geom_boxplot(fill = "lightgray", color = "black") +
  labs(x = "clusters2", y = "rent") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("Houses' rent by cluster (furnished or not)")

box4 <- ggplot(data_new, aes(x = interaction(clusters3, furniture), y = area)) +
  geom_boxplot(fill = "lightgray", color = "black") +
  labs(x = "clusters2", y = "area") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("Houses' area by cluster (furnished or not)")


#arranging plots
grid.arrange(box1, box2, box3, box4, ncol = 2)


#############################################--------------------------------------------------------------------------------------------------------------------------------------------

  
  
#### FURTHER ANALYSIS (NOT USED CODE) ####

  
  #Q-Q plots to visualize deviations, ?
  #In the case of data_imputed 
  qqnorm(data3$proptax)
qqnorm(data3$rent)
qqnorm(data3$hoa)
qqnorm(data3$fireins)


### OUTLIERS ###
#method (IQR)

cols <- c("proptax", "area", "hoa", "fireins", "rent")
data_log <- log(data_imputed[, cols])
data_log[data_log == -Inf] <- 0
data_out <- data_log

#Using IQR step to identify outliers.
#IQR for each numeric column
iqr_values <- apply(data_out, 2, stats::IQR)

#lower and upper bounds using IQR method
lower_bounds <- apply(data_out, 2, function(x) quantile(x, 0.25) - 1.5 * stats::IQR(x))
upper_bounds <- apply(data_out, 2, function(x) quantile(x, 0.75) + 1.5 * stats::IQR(x))

#rows with outliers
outlier_rows <- row.names(data_out)[apply(data_out, 1, function(x) any(x < lower_bounds | x > upper_bounds))]

# Print the number of outliers detected (1727!)
cat("Number of outliers detected:", length(outlier_rows), "\n")
#new df without outliers
data3 <- data_imputed[!(rownames(data_imputed) %in% outlier_rows), ]

#For example, we visualize the boxplot and q-q plots of the rent distribution with and with no outliers, the difference is striking!
par(mfrow=c(2,2))
options(repr.plot.width=12, repr.plot.height=6)
boxplot(data_imputed$rent, col = "lightblue3", horizontal = T, lwd = 4,
        main = "Rent - Before Removing Outliers")
qqnorm(data_imputed$rent)

boxplot(data3$rent, col = "palegreen3", horizontal = T, lwd = 4,
        main = "Rent - After Removing Outliers")
qqnorm


#analysis of outliers (lOOKING FOR INSIGHTS)
data_outliers <- data_imputed[outlier_rows,]

summary(data_outliers)
summary(data_outliers[data_outliers$fireins > 400,])
d3 <- data_imputed[data_imputed$rent > 10000, ]



-------------------------------------------------------------------------------------------------------------

#feature selection and linear models tried

#lasso
library(glmnet)

#training and testing sets (80,20 split)
set.seed(123)
train <- sample(nrow(data3), nrow(data3)*0.8)
train_data <- data3[train,]
test_data <- data3[-train,]

#turning categorical into factor
train_data$rooms <- as.factor(train_data$rooms)
train_data$parking.spaces <- as.factor(train_data$parking.spaces)
train_data$bathroom <- as.factor(train_data$bathroom)
train_data$floor <- as.factor(train_data$floor)
#test
test_data$rooms <- as.factor(test_data$rooms)
test_data$parking.spaces <- as.factor(test_data$parking.spaces)
test_data$bathroom <- as.factor(test_data$bathroom)
test_data$floor <- as.factor(test_data$floor)

#data for Lasso regression (excluding the "fireins" column)
train_x <- model.matrix(rent ~ ., data = train_data[, !names(train_data) %in% c("fireins")])
test_x <- model.matrix(rent ~ ., data = test_data[, !names(train_data) %in% c("fireins")])
train_y <- train_data$rent
test_y <- test_data$rent


# Fit Lasso regression model with cross-validation
cvfit <- cv.glmnet(train_x, train_y, alpha=1, family="gaussian", nfolds=10, standardize = TRUE)

# Get variable coefficients for optimal lambda
coef(cvfit, s=cvfit$lambda.min)

# Get variable coefficients for largest lambda (to see which variables are penalized most)
coef(cvfit, s=cvfit$lambda.1se)

#results?
#we could have been more conservative with the se approach, the difference was not striking though
lambda_min <- cvfit$lambda.min
lasso_pred_min <- predict(cvfit, newx = test_x, s = lambda_min)
library(Metrics)
#rmse on test set
rmselasso <- sqrt(mean((lasso_pred_min - test_y)^2))
rmselasso

#seeing the results
# Assuming you have the predicted results (lasso_pred) and the actual results (test_y)

# Scatter plot
plot(test_y, lasso_pred_min, main = "Predicted vs Actual Results", xlab = "Actual Results", ylab = "Predicted Results")
abline(0, 1, col = "red")  # Add a line of perfect prediction

# Line plot
plot(test_y, type = "l", col = "blue", ylim = range(test_y, lasso_pred_min), main = "Predicted vs Actual Results", xlab = "Observations", ylab = "Rent Amount")
lines(lasso_pred, col = "red")

# Legend
legend("bottomright", legend = c("Actual", "Predicted"), col = c("blue", "red"), lty = 1)



#setting control with Cv 10 folds
control <- trainControl(method="repeatedcv", number=10, repeats=10,
                        selectionFunction = "oneSE")

# Random forest to see variable importance

library(randomForest)

#except fireins, what are the most important predictors?
predictors <- setdiff(names(data3),c("rent")) 
predictors1 <- setdiff(names(data3),c("fireins", "rent")) 
train_x <- train_data[, predictors]
train_x2 <- train_data[, predictors1]
train_y <- train_data$rent  

#Random Forest model
rf_model <- randomForest(train_x, train_y, importance = TRUE)

#variable importance
var_importance <- importance(rf_model)
varImpPlot(rf_model, sort = TRUE, main = "Variable Importance Plot")


#linear model with BIC and AIC
library(MASS)
#AIC criterion
lm_aic <- stepAIC(lm(rent ~ ., data = data3), direction = "both", trace = FALSE)
lm_aic2 <- stepAIC(lm(rent ~ ., data = data3[,predictors]), direction = "both", trace = FALSE)

lm_B
#BIC criterion
lm_bic <- stepAIC(lm(rent ~ ., data = data3), direction = "both", k = log(nrow(data3)), trace = FALSE)
lm_bic2 <- stepAIC(lm(rent ~ ., data = data3[,predictors]), direction = "both", k = log(nrow(data3)), trace = FALSE)

# Print the AIC-selected model
summary(lm_aic)
summary(lm_aic2)
AIC(lm_aic)
AIC(lm_aic2)

# Print the BIC-selected model
summary(lm_bic)
summary(lm_bic2)
BIC(lm_bic)
BIC(lm_bic2)

#alternative linear models (penalized lasso and ridge)

#lambda values 
grid <- seq(10, -3, length=100);
lamVals <- 10 ^ grid

# fit the ridge regression model (alpha = 0 for ridge)
m.ridge <- glmnet(x = train_x, y = y_train_y, alpha = 0,
                  lambda = lamVals, standardize=TRUE)

#Cross Validation for optimal lambda (for Prediction)  

m_cv_ridge <- cv.glmnet(x = train_x, y = y_train_y, alpha = 0, 
                        lambda = lamVals, standardize=TRUE);


ridge_fit()

# Function to calculate R2
calculate_R2 <- function(y_true, y_pred) {
  y_mean <- mean(y_true)
  ss_total <- sum((y_true - y_mean)^2)
  ss_residual <- sum((y_true - y_pred)^2)
  R2 <- 1 - (ss_residual / ss_total)
  return(R2)
}

predictions_ridge <- predict(ridge_fit, newx = test_x, s = m_cv_ridge)

#

#------------------------------------------------------------------------#
#non linear models tried

set.seed(123) # Stochastic Gradient Boosting
m.gbm <- gbm(rent~., data=train_data,
             distribution = "gaussian", # "bernoulli" for classification
             verbose=FALSE,
             n.trees = 200, 
             interaction.depth = 9, 
             shrinkage = 0.1,
             n.minobsinnode = 10);


#gam spline
m.gam.spline <- gam(Rent ~                        
                      s(Area) +
                      Bathrooms + 
                      Rooms +
                      Parking +
                      City,
                    data = Train); 
summary(m.gam.spline)

#grid search Random forest
train_control <- trainControl(
  method = "cv",
  number = 10,
  search = "grid"
)

# Train the random forest model with hyperparameter tuning (takes ages)
rf_model <- train(train_y2~.,
                  data = train_data,
                  method = "rf",
                  trControl = train_control,
                  tuneGrid = NULL
)
#parameter grid for tuning
param_grid <- expand.grid(
  mtry = seq(2, 10, by = 2),  # Features randomly sampled as candidates at each split
  ntree = c(100, 200, 300, 400, 500),  # Number of trees
  nodesize = c(1, 10, by = 1)  # Minimum size of terminal nodes
)

# Reorder the columns in param_grid
param_grid <- param_grid[, c("mtry", "ntree", "nodesize")]

# Define the train control with cross-validation
train_control <- trainControl(
  method = "cv",
  number = 10,
  search = "grid"
)

# Train the random forest model with hyperparameter tuning
rf_model <- train(train_data$rent~.,
                  data = train_data,
                  method = "rf",
                  trControl = train_control,
                  tuneGrid = "param_grid"
)

#best model and its performance
print(rf_model$bestTune)
print(rf_model$results)


#random forest with and without bagging
m.randomForest <- randomForest(x = X_train_matrix, y = y_train_vector,
                               ntrees = 500,
                               mtry = 4); 

# a reduced number of mtry is Random Forest - the model can do either
m.randomForest.bag <- randomForest(x = X_train_matrix, y = y_train_vector,
                                   ntrees = 500,
                                   mtry = 11); # include all X vars

# Train the random forest model with default parameters
rf_model <- randomForest(train_data$rent ~ ., data = train_data)

# Make predictions on the test set
predictionsrf <- predict(rf_model, newdata = test3)


-------------------------------------------------------------------------------------------------------
  
#CLUSTERING CODE NOT USED

#MANHATTAN DOES NOT WORK WELL
dist_man <- dist(data_scaled, method = "manhattan")

#euclidean
dist_euc <- dist(data_scaled, method = "euclidean")
hc_euclidean <- hclust(dist_euc, method = "ward.D2")

#1- row correlation
dist_cor <- as.dist(1 - cor(t(data_scaled)))
hc_correlation <- hclust(dist_cor, method = "ward.D2")

# Plot dendograms
plot(hc_euclidean, cex = 0.6, main = "Dendrogram (Euclidean distance)")
plot(hc_correlation, cex = 0.6, main = "Dendrogram (1 - row correlation)")


# Initialize vectors to store silhouette scores
silhouette_euclidean <- rep(0, 3)
silhouette_correlation <- rep(0, 3)


# Iterate over different values of k
for (k in 2:4) {
  
  # Compute cluster assignments for euclidean distance
  hc_euclidean_k <- cutree(hc_euclidean, k = k)
  
  # Compute silhouette score for euclidean distance
  sileucscores <- data.frame(silhouette(hc_euclidean_k, dist_euc))
  mean_sil_width <- mean(as.numeric(sileucscores$sil_width))         
  silhouette_euclidean[k - 1] <-  mean_sil_width
  
  # Compute cluster assignments for 1 - row correlation
  hc_correlation_k <- cutree(hc_correlation, k = k)
  
  # Compute silhouette score for 1 - row correlation
  sileucscores2 <- data.frame(silhouette(hc_correlation_k, dist_cor))
  mean_sil_width2 <- mean(as.numeric(sileucscores2$sil_width))         
  
  silhouette_correlation[k - 1] <- mean_sil_width2
  
}

cat("Silhouette scores for euclidean distance:", silhouette_euclidean, "\n")
cat("Silhouette scores for 1 - row correlation:", silhouette_correlation, "\n") 



#inspecting into clusters (either for k = 2/ k = 3, just need to change clusters 3 t clusters 2)
boh <- data_new[data_new$clusters3 == 2,]
boh1 <- data_new[dadata_new$clusters3 == 1,]
boh3 <- data_new[dadata_new$clusters3 == 3,]
#value counts
table(boh$furniture)
names(data_new)




  
