rm(list = ls())
library(dplyr)
library(mltools)
library(data.table)
library(ggplot2)
library(lattice)
library(car)
library(randtests)
library(lmtest)

df = read.csv("C:/Users/sambi/OneDrive/Documents/Phone/ndtv_data_final.csv")
df = df[,c(-1,-2,-4,-15)]
View(df)

#df <- df %>% select(-c(Operating.system, X4G..LTE))

colnames(df)


df$Processor = as.factor(df$Processor)

df$logPrice = log(df$Price)

num = c(2,3,5,6,8,9,10,11,19)

pairs(df[, num], pch = 20)



plot(df$logPrice~log(df$Battery.capacity..mAh.), pch = 20)

plot(df$logPrice~df$Battery.capacity..mAh., pch = 20)



#Binning of Brands:
ImpBra = c("Apple", "Samsung", "Xiaomi", "Oppo", "Realme", "OnePlus", "Vivo")

for(i in 1:length(df$Brand))
{
  if(df$Brand[i] %in% ImpBra)
    next
  else
    df$Brand[i] = "Others"
}


dim(df)



#use 75% of dataset as training set and 25% as test set
set.seed(48)
index = 1:nrow(df)
rs = sample(index, size = 0.75*nrow(df), replace = F)
train = df[rs,]
test = df[-rs,]

#train=df%>%sample_frac(0.75)
#test=anti_join(df,train,by='id')
#train=train%>%select(-id)
#test=test%>%select(-id)

View(train)
View(test)
dim(train)
dim(test)





L1 = lm(logPrice~.-logPrice-Price, data = df)
summary(L1)
plot(predict(L1), residuals(L1), pch=20)



plot(L1)


#L2 = update(L1, ~.-Screen.size..inches.-TouchscreenYes-Processor-Operating.systemCyanogen-Operating.systemiOS-Operating.systemSailfish-Operating.systemTizen-Wi.FiYes-BluetoothYes-X3GYes-X4G..LTEYes)
#summary(L2)

unique(train1$RAM..MB.)
unique(train1$Screen.size..inches.)

unique(train1$Processor)
unique(train1$Internal.storage..GB.)
unique(train1$Rear.camera)
unique(train1$Front.camera)









boxplot(train$logPrice)

hist(train$logPrice)



quartiles <- quantile(train$logPrice, probs=c(.25, .75), na.rm = FALSE)
IQR <- IQR(train$logPrice)

Lower <- quartiles[1] - 3*IQR
Upper <- quartiles[2] + 3*IQR 

train1 = subset(train, train$logPrice > Lower & train$logPrice < Upper)

dim(train1)

hist(train1$logPrice)


#Categorical Exploration:

mean(ifelse(train1$Touchscreen ==  "Yes", 1,0))

mean(ifelse(train1$Bluetooth ==  "Yes", 1,0))

mean(ifelse(train1$Wi.Fi ==  "Yes", 1,0))

mean(ifelse(train1$GPS ==  "Yes", 1,0))

mean(ifelse(train1$X3G ==  "Yes", 1,0))

mean(ifelse(train1$X4G..LTE ==  "Yes", 1,0))

mean(ifelse(train1$Number.of.SIMs ==  2, 1,0))

train1 %>% count(Brand)

train1 %>% count(as.factor(Processor))

boxplot(train1$logPrice~train1$Touchscreen)

boxplot(train1$logPrice~train1$Operating.system)

boxplot(train1$logPrice~train1$Processor)

boxplot(train1$logPrice~train1$Brand)

boxplot(train1$logPrice~as.factor(train1$Processor))

boxplot(train1$logPrice~as.factor(train1$RAM..MB.))

t = table(train1[,1], train1[,4])


colnames(train1)

cat = c(1,4,7,12,13,14,15,16,17,18)
pval = matrix(0, ncol = length(cat), nrow=length(cat))

for(i in 1:length(cat)){
  for(j in (i+1):length(cat)){
    pval[i,j] = chisq.test(train1[,cat[i]], train1[,cat[j]])[[3]][1]
  }
}

pval


chisq.test(t)[[3]][1]

chisq.test(train1$Brand, train1$Operating.system, correct=FALSE)

chisq.test(train1$Brand, train1$Touchscreen, correct=FALSE)






#Numerical Exploration:


plot(train1[,2], train1[,20], pch = 20)




colnames(train1)

numerical = colnames(train1)[c(3,5,6,7,8,9,10,11,15,19)]

train1$numerical[1]


pairs(train1[,c(3,5,6,7,8,9,10,11,15,19)], pch = 20)

#front camera has an outlier with logPrice


#Correlation Plot:
colnames(train1)

corr_mat <- round(cor(train1[,num]),2) 
corr_mat
melted_corr_mat <- melt(corr_mat)
head(melted_corr_mat)

# plotting the correlation heatmap
ggplot(data = melted_corr_mat, aes(x=Var1, y=Var2,
                                   fill=value)) +
  geom_tile() +
  geom_text(aes(Var2, Var1, label = value),
            color = "black", size = 4)

#Resolution.x vs Resolution.y - 0.92
#Screen.size.inches vs Battery.capacity.mAh. - 0.74
#Screen.size.inches vs Resolution.y - 0.71
#Screen.size.inches vs RAM.MB. - 0.7
#RAM.MB. vs Resolution.y - 0.78
#Front.camera vs RAM.MB. - 0.72
#Rear.camera vs Internal.storage.GB - 0.71

View(train1)




L1 = lm(logPrice~.-logPrice-Price, data = train1)
summary(L1)
plot(L1)

#VIF:
vif(L1)

L2 =  lm(logPrice~.-logPrice-Price-Resolution.y, data = train1)
summary(L2)
plot(L2) 

vif(L2)  
  
L3 = lm(logPrice~.-logPrice-Price-Resolution.y-Battery.capacity..mAh., data = train1)
summary(L3)
plot(L3) 

vif(L3) 
  


L4 = lm(logPrice~.-logPrice-Price-
          Resolution.y-RAM..MB.-Battery.capacity..mAh.-
          Touchscreen-Processor-Front.camera-Wi.Fi, data = train1)
summary(L4)
plot(L4)


L5 = lm(logPrice~.-logPrice-Price-
          Resolution.y-RAM..MB.-Battery.capacity..mAh.-
          Touchscreen-Processor-Front.camera-Wi.Fi-
          X4G..LTE, data = train1)
summary(L5)


L6 = lm(logPrice~.-logPrice-Price-
          Resolution.y-RAM..MB.-Battery.capacity..mAh.-
          Touchscreen-Processor-Front.camera-Wi.Fi-
          X4G..LTE-Bluetooth, data = train1)
summary(L6)


hist(L4$residuals)

mean(L4$residuals)


plot(predict(L6), L6$residuals, pch = 20)

#Heteroscedasticity absent


gqtest(L6, order.by = ~Rear.camera+Resolution.x, data = train1, fraction = 7)



#Autocorrelation testing:
#plot(L4$residuals[2:length(L4$residuals)], L4$residuals[1:(length(L4$residuals)-1)], pch = 20)

runs.test(L6$residuals)




#Testing the model fitted:
test_pred <- predict.lm(L4, newdata = test)
cor(test_pred, test$logPrice)


plot(test_pred, test_pred-test$logPrice, pch = 20)

mean((test_pred-test$logPrice)^2)









train1 %>% pull(Model) %>% unique() 



sum(train1$Operating.system == 'Sailfish')


#Note that our dataset consists of phones of different models and not the sales of phone so expected to be off-brand companies have higher number of models
#One-Hot Encoding for Brands:

#Brand:
Brandtrain = model.matrix(~Brand, data=train)
train = data.frame(train, Brandtrain)
train$BrandApple = ifelse(train$Brand == "Apple", 1, 0)

#Operating.system
OStrain = model.matrix(~Operating.system, data=train)
train = data.frame(train, OStrain)
train$Operating.systemAndroid = ifelse(train$Operating.system == "Android", 1, 0)

train$Touchscreen = ifelse(train$Touchscreen == "Yes", 1, 0)

train$Wi.Fi = ifelse(train$Wi.Fi == "Yes", 1, 0)

train$Bluetooth = ifelse(train$Bluetooth == "Yes", 1, 0)

train$GPS = ifelse(train$GPS == "Yes", 1, 0)

train$X3G = ifelse(train$X3G == "Yes", 1, 0)

train$X4G..LTE = ifelse(train$X4G..LTE == "Yes", 1, 0)

#Fitting:

coll = colnames(train)[c(-1,-2,-13,-20,-21,-30)]
L = lm(reformulate(coll, "Price"), data = train)
summary(L)























