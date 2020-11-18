library(data.table)
library(dplyr)
library(ggplot2)
library(corrplot)
library(caret)
library(xgboost)
library(cowplot)
train <- fread('train_v9rqX0R.csv')
test <- fread('test_AbJTz2l.csv')
submission <- fread('sample_submission_8RXa3c6.csv')
# Dimension of data
dim(train)
dim(test)
# Getting features name of train and test dataset
names(train)
names(test)
# Structure of data
str(train)
str(test)
# There are 4 numeric and 7 categorical variables
test[, Item_Outlet_Sales := NA]
combi = rbind(train,test)
dim(combi)
###### EXPLORATORY DATA ANALYSIS############
#Univariate Analysis
ggplot(train) + geom_histogram(aes(train$Item_Outlet_Sales), binwidth=100, fill='darkgreen')+xlab('Item_Outlet_Sales')
###Independent Variables(Numeric)####
p1 = ggplot(combi) + geom_histogram(aes(Item_Weight), binwidth = 0.2, fill='blue')
p2 = ggplot(combi) + geom_histogram(aes(Item_Visibility), binwidth = 0.005, fill='blue')
p3 = ggplot(combi) + geom_histogram(aes(Item_MRP), binwidth = 1,fill='blue')
plot_grid(p1,p2,p3,nrow=1)
####Independent variables(Categorical variables)####
ggplot(combi) + geom_bar(aes(Item_Fat_Content),fill='Green')+xlab('Item_Fat_Content')
combi$Item_Fat_Content[combi$Item_Fat_Content == 'LF']='Low Fat'

combi$Item_Fat_Content[combi$Item_Fat_Content== 'low fat'] = 'Low Fat'
combi$Item_Fat_Content[combi$Item_Fat_Content == 'reg'] = 'Regular'
p4 = ggplot(combi) + geom_bar(aes(Item_Fat_Content), fill='Green')+xlab('Item_Fat_Content')
p5 = ggplot(combi) + geom_bar(aes(Item_Type), fill='Green') + xlab('Item_Type')
p6 = ggplot(combi) + geom_bar(aes(Outlet_Identifier), fill='Green') + xlab('Outlet_Identifier')
p7 = ggplot(combi) + geom_bar(aes(Outlet_Size), fill='Green') + xlab('Outlet_Size')
p8 = ggplot(combi) + geom_bar(aes(Outlet_Establishment_Year), fill='Green') + xlab('Outlet_Establishment_Year')
p9 = ggplot(combi) + geom_bar(aes(Outlet_Type), fill='Green') + xlab('Outlet_Type')
plot_grid(p4,p5,p6,nrow=1)
plot_grid(p7,p8,p9,nrow=1)
# Bivariate Analysis
train = combi[1:nrow(train)]
## Target Variables vs Independent Numerical Variables
# Item_weight vs Item_Outlet_Sale
p10 = ggplot(train) + geom_point(aes(Item_Weight, Item_Outlet_Sales), color='violet', alpha=0.3) + theme(axis.title = element_text((size=8)))
#Item_visibility vs Item_Outlet_sale
p11 = ggplot(train) + geom_point(aes(Item_Visibility, Item_Outlet_Sales), color='violet',alpha=0.3) + theme(axis.title = element_text(size =8))
# Item_MRP vs Item_Outlet_Sale
p12 = ggplot(train) + geom_point(aes(Item_MRP, Item_Outlet_Sales), color='violet', alpha=0.3) + theme(axis.title = element_text(size=8))
second_row_2 = plot_grid(p11,p12,ncol=2) 
plot_grid(p10, second_row_2,nrow = 2)
warnings()
#Target variable vs Independent Categorical variable
# Item_Type vs Item_Outlet_Sales
p13 = ggplot(train) + geom_violin(aes(Item_Type, Item_Outlet_Sales), fill='magenta') + theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.text = element_text(size=6), axis.title = element_text(size = 8.5))
# Item_Fat_Content vs Item_Outlet_Sales
p14 = ggplot(train) + geom_violin(aes(Item_Fat_Content, Item_Outlet_Sales), fill='magenta') + theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.text = element_text(size = 8), axis.title = element_text(size = 8.5))
# Outlet_Identifier vs Item_Outlet_Sales
p15 = ggplot(train) + geom_violin(aes(Outlet_Identifier, Item_Outlet_Sales), fill='magenta') + theme(axis.text.x = element_text(angle=45, hjust = 1), axis.text = element_text(size = 8), axis.title = element_text(size = 8.5))
second_row_3 = plot_grid(p14,p15,ncol = 2)
plot_grid(p13, second_row_3, ncol = 1)
ggplot(train) + geom_violin(aes(Outlet_Size, Item_Outlet_Sales), fill='magenta')
p16 = ggplot(train) + geom_violin(aes(Outlet_Location_Type, Item_Outlet_Sales), fill='magenta')
p17 = ggplot(train) + geom_violin(aes(Outlet_Type, Item_Outlet_Sales), fill='magenta')
plot_grid(p16,p17,ncol=1)
# Missing Value Treatment
sum(is.na(combi$Item_Weight))
missing_index <- which(is.na(combi$Item_Weight))
for ( i in missing_index)
  {item = combi$Item_Identifier[i]
  combi$Item_Weight[i] = mean(combi$Item_Weight[combi$Item_Identifier==item], na.rm=T) }
sum(is.na(combi$Item_Weight))
ggplot(combi) + geom_histogram(aes(Item_Visibility), bins=100)
zero_index = which(combi$Item_Visibility==0)
for (i in zero_index) 
  {item =combi$Item_Identifier[i]
  combi$Item_Visibility[i] = mean(combi$Item_Visibility[combi$Item_Identifier == item], np.rm=T)}
ggplot(combi) + geom_histogram(aes(Item_Visibility), bins = 100)
# Feature Engineering
# Item_Type_New <-Broader categories for the variable Item_Type
# Item_category <- Categorical variable derived from Item_Identifier
# Outlet_years <- Years of operation for outlet
# price_per_unit_wt <- Item_MRP/Item_Weight
# Item_MRP_clusters <- Binned feature for Item_MRP
perishable <- c('Breads', 'Breakfast', 'Dairy', 'Fruits and Vegetables', 'Meat', 'Seafood')
non_perishable <- c('Baking Goods', 'Canned', 'Frozen Foods', 'Hard Drinks', 'Health and Hygiene','Household', 'Soft Drinks')
combi[,Item_Type_New := ifelse(Item_Type %in% perishable, 'perishable', ifelse(Item_Type %in% non_perishable,'non_perishable','not_sure'))]
table(combi$Item_Type, substr(combi$Item_Identifier,1,2))
combi[, item_category := substr(combi$Item_Identifier, 1,2)]
combi$Item_Fat_Content[combi$item_category == 'NC'] = 'Non-Edible'
#year of operation for outlet
combi[, Outlet_Years := 2013 - Outlet_Establishment_Year]
combi$Outlet_Establishment_Year = as.factor(combi$Outlet_Establishment_Year)
# price per unit weight
combi[, price_per_unit_wt := Item_MRP/Item_Weight]
# Item_MRP_clusters
combi[, Item_MRP_clusters := ifelse(Item_MRP < 69, '1st',
                             ifelse(Item_MRP >= 69 & Item_MRP < 136, '2nd',
                            ifelse(Item_MRP >= 136 & Item_MRP <203, '3rd', '4th')))]

# Encoding Categorical Variables
combi[,Outlet_Size_num := ifelse(Outlet_Size == 'Small', 0, ifelse(Outlet_Size == 'Medium',1,2))]
combi[, Outlet_Location_Type_num := ifelse(Outlet_Location_Type == 'Tier 3', 0 ,
                                           ifelse(Outlet_Location_Type == 'Tier 2', 1,2))]
combi[,c('Outlet_Size','Outlet_Location_Type'):= NULL]
ohe = dummyVars('~.', data = combi[, -c('Item_Identifier', 'Outlet_Establishment_Year', 'Item_Type')], fullRank = T)
ohe_df = data.table(predict(ohe, combi[, -c('Item_Identifier', 'Outlet_Establishment_Year','Item_Type')]))
combi = cbind(combi[,'Item_Identifier'], ohe_df)
names(combi)
# Pre processing Data#
# Removing Skewness
combi[,Item_Visibility := log(Item_Visibility + 1)]
combi[, price_per_unit_wt := log(price_per_unit_wt + 1)]
# Scaling Numeric Variables
num_vars = which(sapply(combi, is.numeric)) # index of numeric features
num_vars_names = names(num_vars)
combi_numeric = combi[, setdiff(num_vars_names, 'Item_Outlet_Sales'), with = F]
prep_num = preProcess(combi_numeric, method = c('center', 'scale')) 
combi_numeric_norm = predict(prep_num, combi_numeric)
combi[, setdiff(num_vars_names, 'Item_Outlet_Sales') := NULL]
combi = cbind(combi, combi_numeric_norm)
#Splitting the combined (combi) data back to train and test data
train  = combi[1:nrow(train)] 
test = combi[(nrow(train) + 1): nrow(combi)]
test[,Item_Outlet_Sales := NULL]
# Correlated Variables
cor_train = cor(train[, -c('Item_Identifier')])
corrplot(cor_train, method = 'pie' , type = 'lower' , tl.cex = 0.9)
#### Model Building ####
# Linear Regression
# Lasso Regression
# Ridge Regression
# RandomForest
# XGBoost
##### Evaluation Metrics for Regression #####
# Mean Absolute Error
# Mean Squared Error
# Root Mean Squared Error
linear_reg_mod = lm(Item_Outlet_Sales ~ ., data=train[, -c('Item_Identifier')])
# Making Prediction on test data
submission$Item_Outlet_Sales = predict(linear_reg_mod, test[, -c('Item_Identifier')])

write.csv(submission, 'Linear_reg.csv', row.names = F)
# Regularized Linear Regression
# Lasso Regression
set.seed(1235)
my_control = trainControl(method = 'cv', number = 5)
Grid = expand.grid(alpha=1, lambda = seq(0.001, 0.1, by = 0.0002))
lasso_linear_reg_mod = train(x = train[, -c('Item_Identifier', 'Item_Outlet_Sales')], y=train$Item_Outlet_Sales, method = 'glmnet', trControl = my_control,tuneGrid = Grid)
1
install.packages(glmnet)
submission$Item_Outlet_Sales = predict(linear_reg_mod, test[, -c('Item_Identifier')])
write.csv(submission, 'Lasso_Linear_reg.csv', row.names = F)
set.seed(1236)
my_control = trainControl(method = 'cv', number = 5)
Grid = expand.grid(alpha=0,lambda = seq(0.001,0.1, by = 0.0002))
ridge_linear_regression_model = train(x = train[, -c('Item Identifier', 'Item_Outlet_sales')], y = train$Item_Outlet_Sales, method= 'glmnet', trControl = my_control, tuneGrid = Grid)
# Random Forest
set.seed(1237)

my_control = trainControl(method = 'cv', number = 5)
tgrid = expand.grid(
        .mtry = c(3:10),
        .splitrule = 'variance',
        .min.node.size = c(10,15,20)
  )

rf_mod = train(x = train[, -c('Item_Identifier', 'Item_Outlet_Sales')],
               y = train$Item_Outlet_Sales,
               method = 'ranger',
               trControl = my_control, tuneGrid = tgrid, num.trees = 400, importance = 'permutation')
installed.packages('ranger')

