#Data Manipulation
#Diamonds

library(ggplot2)
library(dplyr)

#Ideal Cut Diamonds
diamonds %>% filter(cut == "Ideal") -> Ideal_cut
Ideal_cut %>% filter(price>15000) -> high_price
Ideal_cut %>% filter(price>10000 & price<15000) -> medium_price
Ideal_cut %>% filter(price<10000) -> low_price 

#Diamond dimensions
diamonds %>% select('x','y','z') -> diamond_dimensions
diamond_dimensions %>% filter(x>5 & y>50)

#Diamond color
diamonds %>% filter(color == "D") -> diamond_best_color
diamond_best_color %>% filter(clarity == "IF") -> diamond_best_clarity
diamond_best_clarity %>% summarise(mean_price = mean(price))

#---------------------------------

#Visualization

#Distribution of cut
ggplot(data=diamonds,aes(x=cut))+geom_bar(fill = "lightsalmon3")
ggplot(data=diamonds,aes(x=cut, fill = cut))+geom_bar()

#price vs carat
ggplot(data=diamonds,aes(y=price,x=carat))+geom_point()
ggplot(data=diamonds,aes(y=price,x=carat))+geom_point(col="palegreen4")

#price vs length
ggplot(data=diamonds,aes(y=price,x=x))+geom_point(col="mediumorchid4")

#---------------------------------

#Linear Regression 

library(caTools)
sample.split(diamonds$price,SplitRatio = 0.65) -> my_split

subset(diamonds,my_split==T) -> train_data
subset(diamonds,my_split==F) -> test_data

nrow(train_data)
nrow(test_data)

#Model 1
lm(price~.,data=train_data) -> model1
predict(model1,newdata= test_data) -> result1

cbind(actual=test_data$price,predicted=result1) -> final_data1

as.data.frame(final_data1) -> final_data1
(final_data1$actual-final_data1$predicted) -> error
cbind(final_data1,error) -> final_data1

rmse1 <- sqrt(mean((final_data1$error)^2))
rmse1

## the lower the value of rmse the better the model 

#Model2
lm(price~x+y+z, data = train_data) -> model2
predict(model2,newdata=test_data) -> result2

cbind(actual=test_data$price,predicted=result2) -> final_data2

as.data.frame(final_data2) -> final_data2
(final_data2$actual-final_data2$predicted) -> error
cbind(final_data2,error) -> final_data2

rmse2 <- sqrt(mean((final_data2$error)^2))
rmse2
rmse1
