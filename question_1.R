#Question 1.
#(a)

train <- read.csv("laptop_train.csv")
test  <- read.csv("laptop_test.csv")

train$Company  <- factor(train$Company)
train$TypeName <- factor(train$TypeName)
train$GPU      <- factor(train$GPU)

# These should stay numeric
num_vars <- c("Screen","Memory","Weight","Rating","Price")
train[num_vars]<- lapply(train[num_vars], as.numeric)

# Simple correlation matrix
cor(train[, num_vars], use = "complete.obs")

# Base Model (below)
model1 <- lm(Price ~ Screen + Memory + Weight + Rating + Company + TypeName + GPU, data = train)
summary(model1)

# Model to compare with (below)
model_final <- lm(Price ~ Screen + Memory + Weight + TypeName + GPU, data = train)
summary(model_final)

#(b)

#In the final model, the effects of Memory, Ultrabook type, and GPU
#are managerially sensible, as they align with expectations that higher RAM, premium designs, stronger GPUs3
#increase laptop prices. By contrast, the negative effect of Screen size, the positive effect of Weight, and the negative 
#but insignificant impact of Rating (not in my final model as model gets a higher Out of sample R^2 wihtout it) in earlier models are counterintuitive and likely driven by multicollinearity or market dynamics,
#making them worthy of further investigation.

#I'd like to investigate a few variables. Screen size shows a negative effect on price, which is counterintuitive and may be influenced by its high correlation with weight.
#Similarly, Weight has a positive effect that may reflect gaming laptops but does not capture the premium consumers pay for lighter 
#ultrabooks.


#(c)
#Based on the model, HP has the highest effect on laptop price, commanding the largest premium over Asus, 
#while Lenovo has the smallest effect, showing no meaningful price difference from the baseline.

#*IMPORTANT, CHECK WHETHER TO INCLUDE THE COMPANY VARIABLE IN THE MAIN MODEL OR NOT*

#(d) The out of sample R^2 of the model is 0.5592907.

pred_test <- predict(model_final, newdata=test)
sse <- sum((test$Price - pred_test)^2)
sst <- sum((test$Price - mean(test$Price))^2)
R2_out <- 1 - sse/sst
R2_out  # out-of-sample R²

#Interpretation: This means that when predicting laptop prices on unseen test data,
#the model explains about 55% of the variation in prices compared to simply predicting the mean price for all laptops.


#(e)
newlap <- data.frame(
  Screen = 15.6,
  Memory = 6,
  Weight = 3,
  Company = factor("Asus", levels=levels(train$Company)),
  TypeName = factor("Ultrabook", levels=levels(train$TypeName)),
  GPU = factor("Intel", levels=levels(train$GPU))
)

# Prediction with prediction interval (includes error variance)
pred <- predict(model_final, newdata=newlap, interval="prediction", level=0.95)
pred

p <- predict(model_final, newdata=newlap, se.fit=TRUE)
sigma2 <- summary(model_final)$sigma^2
se_pred <- sqrt(p$se.fit^2 + sigma2)
df <- model_final$df.residual

t_stat <- (1100 - p$fit) / se_pred
prob_gt_1100 <- 1 - pt(t_stat, df=df)
prob_gt_1100

#Assuming normally distributed errors,
#the probability that the actual price exceeds €1,100 is 0.8603419

#(f)

model_mem_gpu <- lm(Price ~ Screen + Memory + Weight + TypeName +
                      GPU + Memory:GPU, data=train)
summary(model_mem_gpu)

#Unlike the main-effects model, this interaction model reveals that the impact of RAM is not constant across GPUs; instead,
#the price premium for memory is significantly higher when Intel or Nvidia GPUs are present.

#For AMD laptops, each extra GB of RAM adds about €56 to price. With Intel and Nvidia GPUs,
#the RAM effect is much stronger—around €96/GB and €92/GB, respectively. The standalone GPU effects are not significant,
#indicating that GPUs mainly influence price through how they enhance the value of added memory.


#(g)
train$Company  <- factor(train$Company,  levels = c("Asus","Dell","HP","Lenovo"))  
train$TypeName <- factor(train$TypeName, levels = c("Gaming","Notebook","Ultrabook"))
train$GPU      <- factor(train$GPU,      levels = c("AMD","Intel","Nvidia"))

# Model with GPU × Company interaction
model_gpu_company <- lm(
  Price ~ Screen + Memory + Weight + TypeName + GPU + GPU:Company,
  data = train
)
summary(model_gpu_company)

#The coefficients show that the price impact of GPUs is not consistent across manufacturers.
#For instance, Intel GPUs are discounted on Asus models but can add value on HP models,
#indicating brand-specific GPU pricing strategies.

#For Asus (baseline), Intel GPUs reduce price by about €279 and Nvidia by €51 (not significant).
#The interaction terms show brand differences: Intel GPUs are less negative for Dell (–145 €),
#turn slightly positive for HP (+18 €), and remain negative for Lenovo (–93 €).
#Nvidia effects also vary by brand but follow a similar adjustment pattern.


