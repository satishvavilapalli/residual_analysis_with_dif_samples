# set the path
setwd('D:\\Praxis_Ds\\Projects\\Bigmart_Sales')

# import the data
sales = read.csv('train.csv')

# have a look on the data
head(sales)

#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 7 with a given list of sample sizes
#=============================================================================================

# plotting the ngraphs
siz <- c(20,30,50,70,100,200,350)
ordr = 7

for (i in siz) {
  # set  sample size
  set.seed(0)
  rand = sample(1:nrow(sales),i)
  train = sales[rand, ]
  test = sales[-rand, ]
  
  
  # build model
  m7 <- lm(Item_Outlet_Sales ~ Item_MRP + I(Item_MRP^2) + I(Item_MRP^3) + 
             I(Item_MRP^4) + I(Item_MRP^5)+ I(Item_MRP^5) + I(Item_MRP^6) 
           + I(Item_MRP^6) + I(Item_MRP^7), train)
  
  #TRAIN AND TEST ACCURACY
  sum(m7$residuals^2)
  pred = predict(m7, newdata=test)
  sum((pred-test$Item_Outlet_Sales)^2)
  
  #PLOTTING THE MODEL OVER THE DATA
  jpeg(filename = paste(paste("With_sample",toString(i)),".jpg"))
  plot(train$Item_MRP,train$Item_Outlet_Sales, pch=19, cex=0.5 ,
       main = paste("Sales_with_degree",toString((ordr))),
       sub = paste("Sample size ",toString((i))), xlab = 'Item_MRP',ylab = 'Item_Outlet_Sales')
  
  lines(sort(train$Item_MRP), fitted(m7)[order(train$Item_MRP)], col='brown', type='l',pch=20) 
  dev.off()
  
}

#*******************************************
#*        sample versus error              *
#*******************************************
siz <- c(20,30,50,70,100,200,350)
resi <- vector()

for (i in siz) {
  set.seed(0)
  rand = sample(1:nrow(sales),i)
  train = sales[rand, ]
  test = sales[-rand, ]
  
  
  # build model
  m7 <- lm(Item_Outlet_Sales ~ Item_MRP + I(Item_MRP^2) + I(Item_MRP^3) + 
             I(Item_MRP^4) + I(Item_MRP^5)+ I(Item_MRP^5) + I(Item_MRP^6) 
           + I(Item_MRP^6) + I(Item_MRP^7), train)
  
  #TRAIN AND TEST ACCURACY
  sum(m7$residuals^2)
  pred = predict(m7, newdata=test)
  sum((pred-test$Item_Outlet_Sales)^2)
  
  # get the residuals
  resi <- c(resi,sum((pred-test$Item_Outlet_Sales)^2))
  
}

# create a data frame with sample size and residual
size_error <- data.frame(siz , resi)

# plot the size and error
jpeg("sample_error_trend.jpg")
plot(size_error$siz,size_error$resi,xlab = 'Sample Size',ylab = 'Error')
lines(size_error$siz,size_error$res, col='red', type='l')
dev.off()


#****************************************************************************************
# 2nd Task
#****************************************************************************************
siz <- c(100,20)
ord <- c(1,2,7,8,9,10)
resi <- vector()
rmse <- vector()

for (i in siz) {
  
  set.seed(0)
  rand = sample(1:nrow(sales),i)
  train = sales[rand, ]
  test = sales[-rand, ]
  
  for (j in ord) {
    
    if (j  == 1){
      
      # build a model
      m <- lm(Item_Outlet_Sales ~ Item_MRP, train)
      
    } else if (j == 2) {
      
      # build a model
      m <- lm(Item_Outlet_Sales ~ Item_MRP + I(Item_MRP^2), train)
      
    } else if (j == 7) {
      
      # build a model
      m <- lm(Item_Outlet_Sales ~ Item_MRP + I(Item_MRP^2) + I(Item_MRP^3) + 
                I(Item_MRP^4) + I(Item_MRP^5)+ I(Item_MRP^6) + I(Item_MRP^7) , train)
      
    } else if (j == 8) {
      
      # build a model
      m <- lm(Item_Outlet_Sales ~ Item_MRP + I(Item_MRP^2) + I(Item_MRP^3) + 
                I(Item_MRP^4) + I(Item_MRP^5)+ I(Item_MRP^6) + I(Item_MRP^7) 
              + I(Item_MRP^8) , train)
      
    } else if (j == 9) {
      
      # build a model
      m <- lm(Item_Outlet_Sales ~ Item_MRP + I(Item_MRP^2) + I(Item_MRP^3) + 
                I(Item_MRP^4) + I(Item_MRP^5)+ I(Item_MRP^6) + I(Item_MRP^7) 
              + I(Item_MRP^8) + I(Item_MRP^9) , train)
      
    } else if (j == 10) {
      
      # build a model
      m <- lm(Item_Outlet_Sales ~ Item_MRP + I(Item_MRP^2) + I(Item_MRP^3) + 
                I(Item_MRP^4) + I(Item_MRP^5)+ I(Item_MRP^6) + I(Item_MRP^7) 
              + I(Item_MRP^8) + I(Item_MRP^9)  + I(Item_MRP^10) , train)
    }
    
    
    #TRAIN AND TEST ACCURACY
    
    sum(m$residuals^2)
    pred = predict(m, newdata=test)
    sum((pred-test$Item_Outlet_Sales)^2)
    
    rootmeansquarederror <- sum((pred-test$Item_Outlet_Sales)^2)
    rootmeansquarederror <- sqrt(rootmeansquarederror/(i-2))
    
    
    # get the residuals
    resi <- c(resi,sum((pred-test$Item_Outlet_Sales)^2))
    
    # get the RMSE
    rmse <- c(rmse,rootmeansquarederror)
  }
}

# allocating sample sizes
sampl = vector()
for (k in siz) {
  for (l in ord) {
    sampl = c(sampl, k)
  }
}


# create a data frame with sample size and residual, RMSE
size_error <- data.frame(sampl , resi)
size_rmse  <- data.frame(sampl , rmse)

# plot the size vs error , size vs rmse 
jpeg("sample_residual_trend.jpg")
plot(size_error$sampl, size_error$resi, xlab = 'Sample Size',ylab = 'Residual')
lines(size_error$sampl,size_error$resi, col  = 'red', type='l')
dev.off()

jpeg("sample_RMSE_trend.jpg")
plot(size_rmse$sampl,size_rmse$rmse,xlab = 'Sample Size',ylab = 'RMSE')
lines(size_rmse$sampl,size_rmse$rmse, col='red', type='l')
dev.off()
