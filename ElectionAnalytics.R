rm(list = ls())

#install.packages("maps")
#install.packages("devtools")
#install.packages("ggmap")

#load library
library(maps)
library(devtools)
library(ggmap)
register_google(key = "AIzaSyBlCZXGDK9dN3Vf_N1qdI6mPfFFCA34ubs")

#load maps
statesMap = map_data("state")
str(statesMap)
z = table(statesMap$group)
table(z)

#Draw the map
ggplot(statesMap, aes(x = long, y = lat, group = group)) +
       geom_polygon(fill = "white", color = "black")
#read the election data
polling = read.csv("PollingImputed.csv")
# Split the data
#create Testing & Training  data
Train = subset(polling, Year == 2004 | Year == 2008)
Test = subset(polling, Year == 2012)

# Logistic Regression
# log function(x) =1/(1 +exp(-x))
mod2 = glm(Republican~SurveyUSA+DiffCount + Rasmussen + PropR, data=Train, family="binomial")
colnames(Train)
# Make predictions
#this is a set of nos b/w 0 & 3
TestPrediction = predict(mod2, newdata=Test, type="response")
#convert to 0 or 1
binaries = TestPrediction>0.5
ZeroOnes = as.numeric(binaries)

pred = data.frame(Test$State,TestPrediction,binaries,ZeroOnes)
pred$unclear = pred$TestPrediction>0.1 & pred$TestPrediction < 0.9
# view the data
table(pred)
summary(pred)
summary(mod2)

# create new model
# log function(x) =1/(1 +exp(-x))
mod2 = glm(Republican~SurveyUSA+DiffCount, data=Train, family="binomial")
colnames(Train)
# Make predictions
#this is a set of nos b/w 0 & 3
TestPrediction = predict(mod2, newdata=Test, type="response")
#convert to 0 or 1
binaries = TestPrediction>0.5
ZeroOnes = as.numeric(binaries)

pred = data.frame(Test$State,TestPrediction,binaries,ZeroOnes)
pred$unclear = pred$TestPrediction>0.1 & pred$TestPrediction < 0.9
# view the data
table(pred)
summary(pred)
summary(mod2)

#plot the data
pred$region = tolower(pred$Test.State)
# Merge the data
predictionMap = merge(statesMap, pred, by = "region")

#create the map
ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPredictionBinary)) + 
           geom_polygon(color = "black")


ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = pred)) + 
  geom_polygon(color = "black")

# A more aesthetic plot using red/blues
ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = ZeroOnes))+
  geom_polygon(color = "black") + scale_fill_gradient(low = "blue", high = "red", guide = "legend", breaks= c(0,1), labels = c("Democrat", "Republican"), name = "Prediction 2012")


# We could also plot using our surety of prediction. Notice color of Oregon (middle of map).
ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPrediction))+
  geom_polygon(color = "black") + scale_fill_gradient(low = "blue", high = "red", guide = "legend", breaks= c(0,1), labels = c("Democrat", "Republican"), name = "Prediction 2012")

ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPrediction))+
  geom_polygon(color = "black", linetype = 3) + scale_fill_gradient(low = "blue", high = "red", guide = "legend", breaks= c(0,1), labels = c("Democrat", "Republican"), name = "Prediction 2012")

ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPrediction))+
  geom_polygon(color = "black", linetype = 2) + scale_fill_gradient(low = "blue", high = "red", guide = "legend", breaks= c(0,1), labels = c("Democrat", "Republican"), name = "Prediction 2012")


#linetype2 thick lines


ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPrediction))+
  geom_polygon(color = "black", linetype = 2, size=3) + scale_fill_gradient(low = "blue", high = "red", guide = "legend", breaks= c(0,1), labels = c("Democrat", "Republican"), name = "Prediction 2012")

#create transparency for aesthetics
ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPrediction))+
  geom_polygon(color = "black", linetype = 1, alpha=0.3) + scale_fill_gradient(low = "blue", high = "red", guide = "legend", breaks= c(0,1), labels = c("Democrat", "Republican"), name = "Prediction 2012")


#Vector of Republican/Democrat
TestPredictionBinary = as.numeric(TestPrediction > 0.5)
# Store predictions and state labels into a dataframe
predictionDataFrame = data.frame(TestPrediction, TestPredictionBinary, Test$State)

