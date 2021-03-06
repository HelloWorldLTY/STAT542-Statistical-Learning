###### Prepare Package ######
mypackages = c("randomForest","glmnet","psych", "xgboost")   # required packages
tmp = setdiff(mypackages, rownames(installed.packages()))  # packages need to be installed
if (length(tmp) > 0) install.packages(tmp)
lapply(mypackages, require, character.only = TRUE)

###### Utility Functions ######

log_RMSE = function (true_value, predicted_value) {
  sqrt(mean((log(predicted_value) - log(true_value))^2))
}

get_RMSE = function (true_value, predicted_value) {
  sqrt(mean((predicted_value - true_value)^2))
}

Winsorization = function (data, filter_by, fraction=.05)
{
  if(length(fraction) != 1 || fraction < 0 ||
     fraction > 0.5) {
    stop("bad value for 'fraction'")
  }
  lim = quantile(data[,filter_by], probs=c(fraction, 1-fraction))
  
  data[data[,filter_by]< lim[1],filter_by] = lim[1]
  data[data[,filter_by] > lim[2], filter_by] = lim[2]
  
  data
}

Winsorization2 = function (data, exclude_fields, multiple=3)
{
  if(length(multiple) != 1 || multiple <= 0) {
    stop("bad value for 'multiple'")
  }
  
  for (i in 1:dim(data)[2]){
    if(!is.numeric(data[,i]) || colnames(data)[i] %in% exclude_fields) next;
    
    med = median(data[,i])
    y = data[,i] - med
    sc = mad(y, center=0) * multiple
    
    y[ y > sc ] = sc
    y[ y < -sc ] = -sc
    
    data[,i] = y + med
  }
  
  data
}

###### Pre-processing and Feature Engineering Functions ######

handle_missing = function(train_data, test_data){
  train_data$Garage_Yr_Blt[is.na(train_data$Garage_Yr_Blt)] =
    train_data$Year_Built[is.na(train_data$Garage_Yr_Blt)]
  test_data$Garage_Yr_Blt[is.na(test_data$Garage_Yr_Blt)] =
    test_data$Year_Built[is.na(test_data$Garage_Yr_Blt)]
  
  list(train_data = train_data, test_data = test_data)
}

remove_outlier = function(train_data, test_data){
  outlier_pid = c(902207130, 910251050, 908154195, 908154205, 908154235)
  
  train_data =  train_data[!train_data$PID %in% outlier_pid,]
  
  list(train_data = train_data, test_data = test_data)
}

add_variables = function(train_data, test_data){
  #Add total bath variable
  train_data$TotBathrooms = train_data$Full_Bath + (train_data$Half_Bath * 0.5) +
    train_data$Bsmt_Full_Bath + (train_data$Bsmt_Half_Bath * 0.5)
  test_data$TotBathrooms = test_data$Full_Bath + (test_data$Half_Bath * 0.5) +
    test_data$Bsmt_Full_Bath + (test_data$Bsmt_Half_Bath * 0.5)
  
  #Add Remodeled variable
  train_data$Remodeled = ifelse(train_data$Year_Built == train_data$Year_Remod_Add, "No", "Yes")
  train_data$Remodeled = factor(train_data$Remodeled)
  test_data$Remodeled = ifelse(test_data$Year_Built == test_data$Year_Remod_Add, "No", "Yes")
  test_data$Remodeled = factor(test_data$Remodeled)
  
  #Add Age variable
  train_data$Age = train_data$Year_Sold - train_data$Year_Remod_Add
  test_data$Age = test_data$Year_Sold - test_data$Year_Remod_Add
  
  #Add IsNew variable
  train_data$IsNew = ifelse(train_data$Year_Sold == train_data$Year_Built, "Yes", "No")
  train_data$IsNew = factor(train_data$IsNew)
  test_data$IsNew = ifelse(test_data$Year_Sold == test_data$Year_Built, "Yes", "No")
  test_data$IsNew = factor(test_data$IsNew)
  
  #Add Total Square Feet
  train_data$TotalSqFeet = train_data$Gr_Liv_Area + train_data$Total_Bsmt_SF
  test_data$TotalSqFeet = test_data$Gr_Liv_Area + test_data$Total_Bsmt_SF
  
  #Consolidating Porch variables
  train_data$TotalPorchSF =  train_data$Open_Porch_SF + train_data$Enclosed_Porch + 
    train_data$Three_season_porch + train_data$Screen_Porch
  test_data$TotalPorchSF =  test_data$Open_Porch_SF + test_data$Enclosed_Porch + 
    test_data$Three_season_porch + test_data$Screen_Porch
  
  list(train_data = train_data, test_data = test_data)
}

remove_variables = function(train_data, test_data){
  #Remove highly correlated, consolidated variables and dominate categorical variables
  dropVars = c('Year_Remod_Add', 'Garage_Yr_Blt', 'Garage_Area', 'Garage_Cond', 'Total_Bsmt_SF', 
               'TotRms_AbvGrd', 'BsmtFin_SF_1',
               'First_Flr_SF', 'Second_Flr_SF', 'Bedroom_AbvGr', #'Gr_Liv_Area', #Extra
               'Full_Bath', 'Half_Bath', 'Bsmt_Full_Bath', 'Bsmt_Half_Bath',
               'Open_Porch_SF', 'Enclosed_Porch', 'Three_season_porch', 'Screen_Porch',skw
               'Street','Utilities','Land_Slope','Condition_2', 'Roof_Matl', 
               'Heating','Pool_QC', 'Misc_Feature', 'Low_Qual_Fin_SF',
               'Pool_Area', 'Misc_Val', 'Longitude', 'Latitude')
  
  train_data = train_data[, !colnames(train_data) %in% dropVars]
  test_data = test_data[, !colnames(test_data) %in% dropVars]
  
  list(train_data = train_data, test_data = test_data)
}

CategoricalLevels = list(
  MS_SubClass = c("One_Story_1946_and_Newer_All_Styles","Two_Story_1946_and_Newer",
                  "One_and_Half_Story_Finished_All_Ages", "One_Story_PUD_1946_and_Newer",
                  "One_Story_1945_and_Older", "Two_Story_PUD_1946_and_Newer",
                  "Two_Story_1945_and_Older","Split_or_Multilevel",
                  "Duplex_All_Styles_and_Ages","Two_Family_conversion_All_Styles_and_Ages", 
                  "Split_Foyer", "Two_and_Half_Story_All_Ages", "One_and_Half_Story_Unfinished_All_Ages",
                  "PUD_Multilevel_Split_Level_Foyer", "Misc"),
  MS_Zoning = c("Residential_Low_Density", "Residential_Medium_Density", 
                "Floating_Village_Residential","Residential_High_Density",
                "C_all", "Misc"),
  Street = c("Pave", "Misc"),
  Alley = c("No_Alley_Access", "Gravel", "Paved", "Misc"),
  Lot_Shape = c("Regular", "Slightly_Irregular", "Misc"),
  Land_Contour = c("Lvl","HLS", "Bnk", "Misc"),
  Utilities = c("AllPub", "Misc"),
  Lot_Config = c("Inside","Corner","CulDSac", "Misc"),
  Land_Slope = c("Gtl","Mod", "Misc"),
  Neighborhood = c("North_Ames","College_Creek","Old_Town","Edwards","Somerset",
                   "Northridge_Heights","Gilbert","Sawyer","Northwest_Ames",
                   "Sawyer_West","Mitchell","Brookside","Crawford",
                   "Iowa_DOT_and_Rail_Road", "Timberland", "Northridge","Stone_Brook",
                   "South_and_West_of_Iowa_State_University", "Clear_Creek", "Meadow_Village",
                   "Briardale", "Bloomington_Heights", "Veenker", "Northpark_Villa", "Blueste",
                   "Misc"),
  Condition_1 = c("Norm","Feedr", "Misc"),
  Condition_2 = c("Norm", "Misc"),
  Bldg_Type = c("OneFam","TwnhsE","Duplex","Twnhs", "Misc"),
  House_Style = c("One_Story","Two_Story","One_and_Half_Fin", 
                  "SLvl","Misc"),
  Overall_Qual = c("Very_Poor", "Poor","Fair","Below_Average",
                   "Average","Above_Average","Good", "Very_Good", 
                   "Excellent", "Very_Excellent"),
  Overall_Cond = c("Very_Poor", "Poor","Fair","Below_Average",
                   "Average","Above_Average","Good", "Very_Good", 
                   "Excellent", "Very_Excellent"),
  Roof_Style = c("Gable","Hip", "Misc"),
  Roof_Matl = c("CompShg", "Misc"),
  Exterior_1st = c("VinylSd","MetalSd","HdBoard","Wd Sdng","Plywood","CemntBd", "Misc"),
  Exterior_2nd = c("VinylSd","MetalSd","HdBoard","Wd Sdng","Plywood","CmentBd", "Misc"),
  Mas_Vnr_Type = c("None","BrkFace","Stone", "Misc"),
  Exter_Qual = c("Poor", "Fair","Typical","Good" ,"Excellent"),
  Exter_Cond = c("Poor", "Fair","Typical","Good" ,"Excellent"),
  Foundation = c("PConc","CBlock", "BrkTil", "Misc"),
  Bsmt_Qual = c("No_Basement","Poor","Fair","Typical","Good","Excellent"),
  Bsmt_Cond = c("No_Basement","Poor","Fair","Typical","Good","Excellent"),
  Bsmt_Exposure = c("No_Basement", "No", "Mn","Av", "Gd"),
  BsmtFin_Type_1 = c("No_Basement","Unf", "LwQ", "Rec", "BLQ", "ALQ", "GLQ"),
  BsmtFin_Type_2 = c("No_Basement","Unf", "LwQ", "Rec", "BLQ", "ALQ", "GLQ"),
  Heating = c("GasA", "Misc"),
  Heating_QC = c("Poor", "Fair","Typical","Good" ,"Excellent"),
  Central_Air = c("Y", "Misc"),
  Electrical = c("SBrkr","FuseA", "Misc"),
  Kitchen_Qual = c("Poor", "Fair","Typical","Good" ,"Excellent"),
  Functional = c("Sal", "Sev", "Maj2", "Maj1", "Mod", "Min2", "Min1", "Typ"),
  Fireplace_Qu = c("No_Fireplace", "Poor", "Fair","Typical","Good","Excellent"),
  Garage_Type = c("Attchd","Detchd", "BuiltIn", "No_Garage", "Misc"),
  Garage_Finish = c("No_Garage", "Unf", "RFn", "Fin"),
  Garage_Qual = c("No_Garage", "Poor", "Fair","Typical","Good","Excellent"),
  Garage_Cond = c("No_Garage", "Poor", "Fair","Typical","Good","Excellent"),
  Paved_Drive = c("Dirt_Gravel","Partial_Pavement", "Paved"),
  Pool_QC = c("No_Pool", "Fair", "Typical", "Good", "Excellent"),
  Fence = c("No_Fence", "Minimum_Wood_Wire", "Good_Wood", "Minimum_Privacy", "Good_Privacy"),
  Misc_Feature = c("None", "Misc"),
  Sale_Type = c("WD ", "New", "Misc"),
  Sale_Condition = c("Normal", "Partial", "Abnorml", "Misc"),
  
  Remodeled = c("Misc"),
  IsNew = c("Misc")
)

process_categorical = function(train_data, test_data){
  for (name in names(CategoricalLevels)) {
    if(!name %in% colnames(train_data)) next;
    
    n = length(CategoricalLevels[[name]])
    
    if(CategoricalLevels[[name]][n] != "Misc") {
      train_levels = CategoricalLevels[[name]][CategoricalLevels[[name]] %in% unique(train_data[,name])]
      test_levels = CategoricalLevels[[name]][CategoricalLevels[[name]] %in% unique(test_data[,name])]
      
      train_data[,name] = ordered(train_data[,name], levels = train_levels)
      
      #Replace with the closest ordered categorical value
      if(sum(!test_levels %in% train_levels) > 0) {
        cat("Undefined test categorical:", name, test_levels[!test_levels %in% train_levels],"\n")
        cat("Training Levels:", train_levels, "\n")
        
        #Reset to full level
        test_data[,name] = ordered(test_data[,name], levels = CategoricalLevels[[name]])
        
        train_i = which(CategoricalLevels[[name]] %in% train_levels)
        for(level_i in 1:length(test_levels)){
          level = test_levels[level_i]
          if(level %in% train_levels) next;
          
          i = which(CategoricalLevels[[name]] == level)
          i = train_i[which.min(abs(train_i - i))]
          test_data[,name][test_data[,name] == test_levels[level_i]] = CategoricalLevels[[name]][i]
        }
        cat("New Level:", CategoricalLevels[[name]][i], "\n")
      }
      
      test_data[,name] = ordered(test_data[,name], levels = train_levels)
      
      if(length(unique(train_data[,name])) != length(levels(train_data[,name]))){
        cat("Unused level in:", name, "\n")
        cat("train:",unique(train_data[,name]), "\n")
        cat("levels:", CategoricalLevels[[name]], "\n")        
      }
      
      # train_data[,name] = as.numeric(train_data[,name])
      # test_data[,name] = as.numeric(test_data[,name])
    } 
    else {
      train_data[,name] = factor(train_data[,name])
      test_data[,name] = factor(test_data[,name], levels=levels(train_data[,name]))
      
      if(sum(is.na(test_data[,name])) > 0) {
        max_level = names(sort(table(train_data[,name]), decreasing = TRUE))[1]
        cat(name, "\tna\t", sum(is.na(test_data[,name])), " replace with", max_level, "\n")
        test_data[,name][is.na(test_data[,name])] = max_level
      }
    }
  }
  
  list(train_data = train_data, test_data = test_data)
}

#Increase the granularity of the categorical predictors
cate_to_numeric = function(train_data, test_data){
  cond_levels = c("Poor", "Average", "Good", "Excellent")
  cond_map=c("Poor", "Poor", "Average", "Average", "Average", "Good", 
             "Good", "Excellent", "Excellent", "Excellent")
  
  train_data$Overall_Qual = ordered(cond_map[as.numeric(train_data$Overall_Qual)], levels=cond_levels)
  train_data$Overall_Cond = ordered(cond_map[as.numeric(train_data$Overall_Cond)], levels=cond_levels)
  
  test_data$Overall_Qual = ordered(cond_map[as.numeric(test_data$Overall_Qual)], levels=cond_levels)
  test_data$Overall_Cond = ordered(cond_map[as.numeric(test_data$Overall_Cond)], levels=cond_levels)
  
  list(train_data = train_data, test_data = test_data)
}

process_numeric = function(train_data, test_data) {
  #Make Year_Sold and Mo_Sold as factor
  train_data$Year_Sold = factor(train_data$Year_Sold)
  train_data$Mo_Sold = factor(train_data$Mo_Sold)
  test_data$Year_Sold = factor(test_data$Year_Sold, levels=levels(train_data$Year_Sold))
  test_data$Mo_Sold = factor(test_data$Mo_Sold, levels=levels(train_data$Mo_Sold))
  
  #Replace na with most frequent value
  max_level = names(sort(table(train_data$Year_Sold), decreasing = TRUE))[1]
  test_data$Year_Sold[is.na(test_data$Year_Sold)] = max_level
  max_level = names(sort(table(train_data$Mo_Sold), decreasing = TRUE))[1]
  test_data$Mo_Sold[is.na(test_data$Mo_Sold)] = max_level
  
  for (i in 2:dim(train_data)[2]){
    col_name = colnames(train_data)[i]
    if(col_name %in% c("Sale_Price", names(CategoricalLevels)) ||
       !is.numeric(train_data[, i])) next;
    
    if(skewness(train_data[, col_name]) > 0.8) {
      #print(col_name)
      train_data[, col_name] = log(train_data[, col_name] + 1)
      test_data[, col_name] = log(test_data[, col_name] + 1)
    }
  }
  
  list(train_data = train_data, test_data = test_data)
}

process_output = function (train_data, test_data) {
  train_data$Sale_Price = log(train_data$Sale_Price)
  if(hasName(test_data, "Sale_Price")) {
    test_data$Sale_Price = log(test_data$Sale_Price)
  }
  
  list(train_data = train_data, test_data = test_data)
}

preprocess_data = function(train_data, test_data){
  r = handle_missing(train_data, test_data)
  #r = remove_outlier(train_data, test_data)
  r = add_variables(r$train_data, r$test_data)
  r = remove_variables(r$train_data, r$test_data)
  r = process_categorical(r$train_data, r$test_data)
  #r = cate_to_numeric(r$train_data, r$test_data)
  r = process_numeric(r$train_data, r$test_data)
  r = process_output(r$train_data, r$test_data)
  # r$train_data = Winsorization2(r$train_data, c("PID", "Overall_Qual","Overall_Cond",
  #                                             "Mas_Vnr_Area","BsmtFin_SF_2", "Second_Flr_SF",
  #                                             "Bedroom_AbvGr", "Kitchen_AbvGr", "Garage_Cars",
  #                                             "Wood_Deck_SF"))
  
  #Remove unnecessary columnes if any
  r$train_data = r$train_data[, !colnames(r$train_data) %in% c('PID')] #Remove 'PID' column
  if(hasName(r$test_data, "Sale_Price")){   #Remove "Sale_Price" column in test data
    true_test_y = r$test_data$Sale_Price
    r$test_data = r$test_data[, !colnames(r$test_data) %in% c('Sale_Price')] 
  } else {
    true_test_y = NULL
  }
  
  
  if(sum(is.na(r$train_data)) > 0 || sum(is.na(r$test_data)) > 0) {
    stop("NA value found!")
  }
  
  list(train_data = r$train_data, test_data = r$test_data, true_test_value = true_test_y)
}

###### Build Model ######
one_step_lasso = function(r, x, lam){
  length(r)
  length(x)
  xx = sum(x^2)
  xr = sum(r*x)
  b = (abs(xr) -lam/2)/xx
  b = sign(xr)*ifelse(b>0, b, 0)
  return(b)
}

mylasso_myfit = function(X, y, lam, beta = NULL)
{
  # X: n-by-p design matrix without the intercept
  # y: n-by-1 response vector
  # lam: lambda value
  # n.iter: number of iterations
  n.iter = 2000
  p = dim(X)[2]
  
  #Remove column with zero variation and later will need to set the corresponsing beta to 0
  full_b = rep(0, p)
  for(i in 1:p) {
    if(length(unique(X[,i])) <= 1) full_b[i] = 1
  }
  p = p - sum(full_b)
  X = X[,full_b ==0]
  
  # Center and scale X and y.
  X = scale(X)
  y = scale(y)
  x_center = attr(X, "scaled:center")
  x_scale = attr(X, "scaled:scale")
  y_center = attr(y, "scaled:center")
  y_scale = attr(y, "scaled:scale")    
  
  # Initial values for residual and coefficient vector b
  if(is.null(beta)) b = rep(0, p)
  else b = beta
  r = y
  
  rss_saved = 0
  rss_changed = 1000
  
  for(step in 1:n.iter){
    if (rss_changed < 1e-08) break;
    for(j in 1:p){
      r = r + X[,j] * b[j]
      b[j] = one_step_lasso(r, X[, j], lam)
      r = r - X[, j] * b[j]
    }
    rss = sum(r^2)
    rss_changed = abs(rss_saved - rss)
    rss_saved = rss
  }
  cat("rss_changed",rss_changed, "step", step, "\n")
  
  # Scale back b and add intercept b0
  b = b * y_scale / x_scale
  b0 = y_center - sum(b * x_center)
  
  #Fill beta of invariate predictor with zero
  j = 1
  for(i in 1:length(full_b)){
    if(full_b[i] == 1) {
      full_b[i] = 0
      next;
    }
    full_b[i] = b[j]
    j = j + 1
  }
  b = full_b
  
  return(c(b0, b))
}

mylasso_mypredict = function(X, beta) {
  beta[1] + as.matrix(X) %*% beta[-1]
}

cv.mylasso = function(train_data, cv_fold = 5) {
  all_lambda = exp(seq(-10, 10, 0.2))
  #all_lambda = c(1, 10, 100)
  X_train = train_data[, !colnames(train_data) %in% c('PID','Sale_Price')]
  X_train = model.matrix(~., X_train)[, -1]
  Y_train = train_data$Sale_Price
  
  cv_rmse = rep(0, length(all_lambda))
  cv_beta = matrix(0, length(all_lambda), dim(X_train)[2] + 1)
  
  all_ids = sample(1:nrow(X_train), nrow(X_train))
  all_test_ids = list()
  size = ceiling(nrow(X_train) / cv_fold)
  start = 1
  for (i in 1:cv_fold){
    end = start + size - 1
    if(end > nrow(X_train)) end = nrow(X_train)
    
    all_test_ids[[i]] = all_ids[start:end]
    
    start = end + 1
  }
  
  for (lambda_i in 1:length(all_lambda)) {
    for (i in 1:cv_fold){
      X_train_split = X_train[-all_test_ids[[i]],]
      Y_train_split = Y_train[-all_test_ids[[i]]]
      X_test_split = X_train[all_test_ids[[i]],]
      Y_test_split = Y_train[all_test_ids[[i]]]
      
      # cv_beta[lambda_i,] = mylasso_myfit(X_train, Y_train, all_lambda[lambda_i], cv_beta[lambda_i,])
      # yhat_test = mylasso_mypredict(X_test_split, cv_beta[lambda_i,])
      beta = mylasso_myfit(X_train_split, Y_train_split, all_lambda[lambda_i])
      yhat_test = mylasso_mypredict(X_test_split, beta)
      cv_rmse[lambda_i] = cv_rmse[lambda_i] + get_RMSE(yhat_test, Y_test_split)
      #cat("lambda", all_lambda[lambda_i], "\trmse", cv_rmse[lambda_i], "\n")
    }
    cv_rmse[lambda_i] = cv_rmse[lambda_i] / cv_fold
    cat("lambda:", all_lambda[lambda_i], "\trmse:", cv_rmse[lambda_i], "\n")
  }
  
  min.lambda = all_lambda[which.min(cv_rmse)]
  print(min.lambda)
  mylasso_myfit(X_train, Y_train, min.lambda)
}

mylasso_predict = function(train_data, test_data) {
  X_train = train_data[, colnames(train_data) != 'Sale_Price']
  X_train = model.matrix(~., X_train)[, -1]
  
  Y_train = train_data$Sale_Price
  
  beta = mylasso_myfit(X_train, Y_train, 10)
  
  X_test = test_data[, colnames(test_data) != 'Sale_Price']
  X_test = model.matrix(~. -PID, X_test)[, -1]
  
  if(sum(names(X_train) != names(X_test)) > 0) stop("Unmatched train and test data")
  mylasso_mypredict(X_test, beta)
}

rf_predict = function(train_data, test_data) {
  x_train = train_data[, !colnames(train_data) %in% c("Sale_Price")]
  
  rfModel = randomForest(x_train, train_data$Sale_Price, ntree=1000);
  predict(rfModel, test_data)
  
  # fitControl = trainControl(method = "repeatedcv", number = 3)
  # rfGrid =  expand.grid(mtry = c(sqrt(dim(train_data)[2]),
  #                                   dim(train_data)[2] / 2,
  #                                    dim(train_data)[2] / 3),
  #                       ntree=c(200, 400, 600))
  # rfFit = train(Sale_Price ~ ., data = train_data, method = "extraTrees",
  #                trControl = fitControl)#, tuneGrid = rfGrid)
  # print(rfFit)
  # predict(rfFit, test_data)
}

gbm_predict = function(train_data, test_data) {
  # gbmModel = gbm(Sale_Price ~ ., data = train_data, distribution = "gaussian", n.trees = 5000,
  #              shrinkage = 0.001, interaction.depth = 4, bag.fraction = 0.5, cv.folds = 5,
  #              verbose = FALSE)
  # 
  # opt.size = gbm.perf(gbmModel, method="cv")
  # predict(gbmModel, test_data, n.trees = opt.size)
  
  fitControl = trainControl(method = "repeatedcv", number = 5)
  gbmGrid =  expand.grid(interaction.depth = c(3, 6, 9),
                         n.trees = (20:40)*50,
                         shrinkage = c(0.1, 0.01),
                         n.minobsinnode = 20)
  
  gbmFit = train(Sale_Price ~ ., data = train_data, method = "gbm",
                 trControl = fitControl, verbose = FALSE,
                 distribution = "gaussian")#, tuneGrid = gbmGrid)
  print(gbmFit)
  
  predict(gbmFit, test_data)
}

lasso_predict = function(train_data, test_data) {
  X_train = train_data[, colnames(train_data) != 'Sale_Price']
  X_train = model.matrix(~., X_train)[, -1]
  
  Y_train = train_data$Sale_Price
  
  cv.out = cv.glmnet(X_train, Y_train, alpha = 1)
  
  X_test = test_data[, colnames(test_data) != 'Sale_Price']
  X_test = model.matrix(~. -PID, X_test)[, -1]
  #print(cv.out$lambda.min)
  predict(cv.out, s = cv.out$lambda.min, newx = X_test)
}

xgb_predict = function(train_data, test_data) {
  X_train = train_data[, colnames(train_data) != 'Sale_Price']
  X_train = model.matrix(~., X_train)[,-1]
  
  Y_train = train_data$Sale_Price
  
  xgb_model = xgboost(data = X_train, label=Y_train, max_depth = 6,
                      eta = 0.03, nrounds = 500,
                      colsample_bytree = 0.6,
                      subsample = 0.75,
                      verbose = FALSE)
  
  X_test = test_data[, colnames(test_data) != 'Sale_Price']
  X_test = model.matrix(~. - PID, X_test)[,-1]
  predict(xgb_model, X_test)
}

top_n_order = function (x, n) {
  x = abs(x)
  index = rep(0, n)
  for (i in 1:n){
    index[i] = which.max(x)
    x[index[i]] = -Inf
  }
  index
}

test_all = function (all_data, all_test_pid, reg_func) {
  rmse = matrix(0, length(all_test_pid), length(reg_func))
  colnames(rmse) = names(reg_func)
  
  for (i in 1:length(all_test_pid)){
    test_pid = all_test_pid[[i]]
    
    train_data = all_data[!all_data$PID %in% test_pid,]
    test_data = all_data[all_data$PID %in% test_pid,]
    
    r = preprocess_data(train_data, test_data)
    
    for (f in 1:length(reg_func)) {
      yhat_test = reg_func[[f]](r$train_data, r$test_data)
      rmse[i, f] = get_RMSE(yhat_test, r$true_test_value)
      
      top_error_id = top_n_order(yhat_test - r$true_test_value, 5)
      cat("Model:", names(reg_func)[f], "\n")
      cat("Top Error ID:", test_data[top_error_id,]$PID, "\n")
      cat("Top Error:", abs(yhat_test - r$true_test_value)[top_error_id], "\n")
      cat(rmse[i, f], "\n")
    }
  }
  
  rmse
}

train_predict = function(train_data, test_data, reg_func, output_filename){
  r = preprocess_data(train_data, test_data)
  
  yhat_test = exp(reg_func(r$train_data, r$test_data))
  
  if(!is.null(r$true_test_value)){
    cat("RMSE:", log_RMSE(yhat_test, exp(r$true_test_value)), "\n")
  }
  
  output = cbind(test_data$PID, yhat_test)
  colnames(output) = c("PID", "Sale_Price")
  
  write.csv(output, output_filename, row.names = FALSE)
}

#######################################
###### Train, Predict and Output ######
#######################################
set.seed(6682)

TRAIN_FILE_NAME = '/Users/mac/Desktop/大四/CS598/project1/train.csv'
TEST_FILE_NAME = '/Users/mac/Desktop/大四/CS598/project1/test.csv'

train_data = read.csv(TRAIN_FILE_NAME)
test_data = read.csv(TEST_FILE_NAME)

start_time = proc.time()

model_functions = list(
  Lasso = lasso_predict,
  Xgboost = xgb_predict
)
output_filenames = c("mysubmission1.txt", "mysubmission2.txt")

for (f in 1:length(model_functions)) {
  train_predict(train_data, test_data, model_functions[[f]], output_filenames[f])
}