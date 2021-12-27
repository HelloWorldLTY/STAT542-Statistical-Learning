library(lubridate)
library(tidyverse)

#refer from https://github.com/vkk2/schoolprojects/
shift=function(test_pred_wk){
  
  weeks2shift=c(49,50,51,52)
  days2shift=1
  test_pred_shift<-test_pred_wk
  for (indx in 1:length(weeks2shift)){
    prevwk <- test_pred_shift %>%
      filter(Wk==(weeks2shift[indx]-1)) %>%
      mutate(offset1=round(Weekly_Pred*days2shift/7,2)) %>%
      select(-Weekly_Pred, -Date, -Wk)
    
    
    nextwk <- test_pred_shift %>%
      filter(Wk==(weeks2shift[indx])) %>%
      mutate(offset2=round(Weekly_Pred*(7-days2shift)/7,2)) %>%
      select(-Weekly_Pred)
    
    combine<-prevwk %>%
      right_join(nextwk, by = c('Store', 'Dept')) %>%
      mutate(Weekly_Pred=offset1+offset2) %>%
      select(-offset1,-offset2)
    
    test_pred_shift<-test_pred_shift%>%
      filter(Wk!=(weeks2shift[indx]))
    test_pred_shift <- rbind(test_pred_shift,combine)
    
  }  
  
  return(test_pred_shift)
}

#refer from campuswire
preprocess.svd <- function(train, n.comp){
  train <- train %>%
    select(Store, Dept, Date, Weekly_Sales) %>%
    spread(Date, Weekly_Sales)
  train[is.na(train)] <- 0
  
  train_svd = NULL
  
  for(mydept in unique(train$Dept)){
    dept_data <- train %>% 
      filter(Dept == mydept)
    
    if (nrow(dept_data) > n.comp){
      tmp_data <- dept_data[, -c(1,2)]
      store_means <- rowMeans(tmp_data)
      tmp_data <- tmp_data - store_means
      z <- svd(tmp_data, nu=n.comp, nv=n.comp)
      s <- diag(z$d[1:n.comp])
      tmp_data <- z$u %*% s %*% t(z$v) + store_means
      tmp_data[tmp_data < 0] <- 0
      dept_data[, -c(1:2)] <- z$u %*% s %*% t(z$v) + store_means
    }
    train_svd = rbind(train_svd, dept_data)
  }
  train_svd <- train_svd %>% 
    gather(Date, Weekly_Sales, -Store, -Dept)
  return(train_svd)
}

#refer from campuswire
linearmodelmethod = function(){
  train <- preprocess.svd(train,8) # preprocessing svd
  start_date <- ymd("2011-03-01") %m+% months(2 * (t - 1))
  end_date <- ymd("2011-05-01") %m+% months(2 * (t - 1))
  test_current <- test %>%
    filter(Date >= start_date & Date < end_date) %>%
    select(-IsHoliday)
  
  # not all depts need prediction
  test_depts <- unique(test_current$Dept)
  test_pred <- NULL
  
  for(dept in test_depts){
    train_dept_data <- train %>% filter(Dept == dept)
    test_dept_data <- test_current %>% filter(Dept == dept)
    
    train_stores <- unique(train_dept_data$Store)
    test_stores <- unique(test_dept_data$Store)
    test_stores <- intersect(train_stores, test_stores)
    
    for(store in test_stores){
      tmp_train <- train_dept_data %>% 
        filter(Store == store) %>%
        mutate(Wk = ifelse(year(Date) == 2010, week(Date)-1, week(Date))) %>%
        mutate(Yr = year(Date))
      tmp_test <- test_dept_data %>% 
        filter(Store == store) %>%
        mutate(Wk = ifelse(year(Date) == 2010, week(Date)-1, week(Date))) %>%
        mutate(Yr = year(Date)) 
      
      tmp_train$Wk = factor(tmp_train$Wk, levels = 1:52)
      tmp_test$Wk = factor(tmp_test$Wk, levels = 1:52)
      
      train_model_matrix <- model.matrix(~ Yr + Wk, tmp_train)
      test_model_matrix <- model.matrix(~ Yr + Wk, tmp_test)
      mycoef <- lm(tmp_train$Weekly_Sales ~ train_model_matrix)$coef
      mycoef[is.na(mycoef)] <- 0
      tmp_pred <- mycoef[1] + test_model_matrix %*% mycoef[-1]
      
      tmp_test_wk <- tmp_test
      test_pred_wk <- test_pred
      
      tmp_test <- tmp_test %>%
        mutate(Weekly_Pred = tmp_pred[,1]) %>%
        #select(-Yr)
        select(-Yr)
      test_pred <- test_pred %>% bind_rows(tmp_test)  
      
    }
  }
  #do a circular shift for fold 5
  if (t==5){
    test_pred<-shift(test_pred_wk)
  }
  
  return(test_pred)
}

#refer from campuswire
naive2 = function(){
  
  start_date <- ymd("2011-03-01") %m+% months(2 * (t - 1))
  end_date <- ymd("2011-05-01") %m+% months(2 * (t - 1))
  test_current <- test %>%
    filter(Date >= start_date & Date < end_date) %>%
    select(-IsHoliday)
  
  start_last_year = min(test_current$Date) - 375
  end_last_year = max(test_current$Date) - 350
  tmp_train <- train %>%
    filter(Date > start_last_year & Date < end_last_year) %>%
    mutate(Wk = ifelse(year(Date) == 2010, week(Date)-1, week(Date))) %>%
    rename(Weekly_Pred = Weekly_Sales) %>%
    select(-Date, -IsHoliday)
  
  test_current <- test_current %>%
    mutate(Wk = week(Date))
  
  test_pred <- test_current %>%
    left_join(tmp_train, by = c('Dept', 'Store', 'Wk')) %>%
    select(-Wk)
  
  test_pred <- test_current %>%
    left_join(tmp_train, by = c('Dept', 'Store', 'Wk')) 

  ###Add shift
  if(t==5){
    test_pred_wk<-test_current %>%
      left_join(tmp_train, by = c('Dept', 'Store', 'Wk'))
    
    test_pred<-shift(test_pred_wk)
  }
  return(test_pred)
}


# My prediction function
mypredict=function(train){
  linear_pred=linearmodelmethod()
  linear_pred<-linear_pred %>% select(-Wk)
  snaive_pred=naive2()
  snaive_pred<-snaive_pred%>% select(-Wk)
  if((t==4)||(t==5)||(t==6)||(t==10)){
    test_pred<-linear_pred %>%
      right_join(snaive_pred, by = c('Store', 'Dept','Date')) %>%
      mutate(Weekly_Pred=((Weekly_Pred.x+Weekly_Pred.y)/2)) 
  }
  else
    test_pred<-linear_pred
  return(test_pred)
}
