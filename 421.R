# Xinyu Wu
# IST 421 Final Project
# Source: https://www.kaggle.com/datasets/aleespinosa/apple-watch-and-fitbit-data


install.packages("ggthemes")
library(ggthemes)
library(stringr)
library(RColorBrewer)
library(ggplot2)
library(RWeka) 
library(dplyr)
library(caret)
library(rsample)

## Load file
# file.choose()
fitness <- read.csv("/Users/xinyuwu/Library/CloudStorage/OneDrive-SyracuseUniversity/IST 421/Final Poster/aw_fb_data.csv")
glimpse(fitness)

# I added a unique identifier of each participant based on their demographic information
fitness$user_id <- paste(fitness$age, fitness$gender, fitness$height, fitness$weight, sep="-")

## data descriptive plots
# creating a dataframe for user information
df.users <- data.frame(unique(fitness$user_id))
df.users <- df.users %>% 
  rename("user_id" = "unique.fitness.user_id.")

# split id column
df.users[c('age', 'gender', 'height', 'weight')] <- str_split_fixed(df.users$user_id, '-', 4)
str(df.users)
#  age
age <- barplot(table(df.users$age), main = "Age Distribution"
               , ylab = "Count"
               , xlab = "Age"
               , col = rgb(255/255, 250/255, 178/255)) 

# height vs weight
df.users$height <- as.numeric(df.users$height)
df.users$weight <- as.numeric(df.users$weight)
hw <- ggplot(df.users) +
  aes(x = weight, y = height, color = gender) +
  xlab("Weight (kg)") +
  ylab("Height (cm)") +
  ggtitle("Weight-Height Distrbution") +
  geom_point() + geom_smooth(method = "lm",formula = 'y ~ x', se = FALSE) +
  theme_grey()
hw

# Calorie vs Activity
calorie <- ggplot(fitness) +
  aes(x = calories, y = activity, fill = activity) +
  geom_violin() + ggtitle("Calorie vs Activity") +
  scale_fill_manual(values = brewer.pal(6, "Spectral"))  +
  theme_minimal()
calorie

# Heart Rate vs Activity
heartrate <- ggplot(fitness) +
  aes(x = hear_rate, y = activity, fill = activity) +
  geom_violin() + ggtitle("Heart Rate vs Activity") +
  scale_fill_manual(values = brewer.pal(6, "Spectral"))+
  theme_minimal()
heartrate
## Main plots for the question
# decision tree
apple <- fitness[fitness$device=="apple watch",]
fitbit <- fitness[fitness$device=="fitbit",]

apple.ml = apple |> 
  mutate_if(is.character, as.factor)
apple.ml <- select(apple.ml, -device)
split.apple = initial_split(apple.ml, prop = .7, strata = 'activity')
train.apple = training(split.apple)
test.apple  = testing(split.apple)


fitbit.ml = fitbit |> 
  mutate_if(is.character, as.factor)
fitbit.ml <- select(fitbit.ml, -device)
split.fitbit = initial_split(fitbit.ml, prop = .7, strata = 'activity')
train.fitbit = training(split.fitbit)
test.fitbit  = testing(split.fitbit)

train.control = trainControl(
  method = 'cv', 
  number = 5
)

grid = expand.grid(.M=c(2,3,4,5,6,7,8,9,10), 
                   .C=c(0.01,0.05,0.10,0.15,0.20,0.25,0.30,0.35,0.40,0.45,0.50))
dt.m.apple = train(activity ~ .,
             data = train.apple,
             method = 'J48',
             trControl = train.control,
             tuneGrid = grid)

dt.p.apple = predict(dt.m.apple, newdata = test.apple)
confusionMatrix(dt.m.apple)
cm.apple <- confusionMatrix(dt.p.apple, test.apple$activity)

dt.m.fitbit = train(activity ~ .,
                   data = train.fitbit,
                   method = 'J48',
                   trControl = train.control,
                   tuneGrid = grid)

dt.p.fitbit = predict(dt.m.fitbit, newdata = test.fitbit)
confusionMatrix(dt.m.fitbit)
cm.fitbit <- confusionMatrix(dt.p.fitbit, test.fitbit$activity)

df.apple <- data.frame(cm.apple$table)
hm.apple <- ggplot(df.apple) + 
  aes(x = Reference, y = Prediction, fill = Freq, label = Freq) + 
  ggtitle("Apple Watch Activity Predictions") +
  geom_tile() +
  geom_text(aes(label = Freq), color = "white")
hm.apple 

df.fitbit <- data.frame(cm.fitbit$table)
hm.fitbit <- ggplot(df.fitbit)  + 
  aes(x = Reference, y = Prediction, fill = Freq, label = Freq) + 
  ggtitle("Fitbit Activity Predictions") +
  geom_tile() +
  geom_text(aes(label = Freq), color = "white") # scale_color_ fitbit color schemes
hm.fitbit


## supportive plots
a1 <- aggregate(x = df.apple$Freq, list(Reference = df.apple$Reference), FUN = sum)
a2 <- df.apple[which((df.apple$Prediction==df.apple$Reference)==TRUE),]
a2$accuracy <- 100*a2$Freq/a1$x

f1 <- aggregate(x = df.fitbit$Freq, list(Reference = df.fitbit$Reference), FUN = sum)
f2 <- df.fitbit[which((df.fitbit$Prediction==df.fitbit$Reference)==TRUE),]
f2$accuracy <- 100*f2$Freq/f1$x

s.apple <- barplot(a2$accuracy, col = brewer.pal(6,"Spectral")
        , ylim = c(0, 100)
        , ylab = "Accuracy"
        , main = "Accuracy for Each Activity")

s.fitbit <- barplot(f2$accuracy, col = brewer.pal(6,"Spectral")
        , ylim = c(0, 100)
        , ylab = "Accuracy"
        , main = "Accuracy for Each Activity") 


## plots
# descriptive
age
hw
calorie
heartrate
# main
hm.apple
hm.fitbit
# supportive
s.apple
s.fitbit
