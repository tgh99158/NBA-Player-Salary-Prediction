library(GGally)
library(tidyverse)
library(glmnet)
library(caret)
library(ggplot2)



# Loading main datasets
player_stats <- read_csv('players_stats_by_season_full_details.csv')
player_salaries <- read_csv('NBASalaryData03-17.csv')

#Filtering for NBA players
player_stats <- player_stats %>% filter(.,League == 'NBA')

#Dropping irrelevant columns
player_stats <- player_stats %>% 
  select(.,-c('nationality','high_school','height','weight','draft_round',
              'draft_pick','draft_team','birth_month','birth_date'))

#Creating Age variable for analysis
player_stats <- player_stats %>% separate(.,'Season',c('Season_Start','Season_End'), sep=' - ')
player_stats$age <- (as.numeric(player_stats$Season_End) - as.numeric(player_stats$birth_year))

#Bringing Season column back
player_stats$Season <- str_c(player_stats$Season_Start,'-', player_stats$Season_End)

#Filtering for seasons with salary data
player_stats <- player_stats %>% filter(., Season == '2002-2003'|
                          Season == '2003-2004'|
                          Season == '2004-2005'|
                          Season == '2005-2006'|
                          Season == '2006-2007'|
                          Season == '2007-2008'|
                          Season == '2008-2009'|
                          Season == '2009-2010'|
                          Season == '2010-2011'|
                          Season == '2011-2012'|
                          Season == '2012-2013'|
                          Season == '2013-2014'|
                          Season == '2014-2015'|
                          Season == '2015-2016'|
                          Season == '2016-2017'|
                          Season == '2017-2018')

#Merging both dataframes
NBA <- merge(player_stats, player_salaries, by.x=c('Player','Season'), by.y=c('player','season'))

#Dropping irrelevant columns once more
NBA <- NBA %>% select(., -c('Season_Start', 'Season_End', 'birth_year','Team','League'))

#Dropping Playoff Data
NBA <- NBA %>% filter(.,Stage == 'Regular_Season')

#Dropping more columns
NBA <- NBA %>% select(., -c('Stage'))

#Defining season as a sole year
NBA <- NBA %>% separate(.,'Season',c('Season_Start','Season_End'), sep='-')
NBA <- NBA %>% select(., -c('Season_Start'))
NBA <- rename(NBA, 'Season' = 'Season_End')
NBA$Season <- as.numeric(NBA$Season)
#Transforming metrics to game averages for appropriate scaling

colnames(NBA)[colnames(NBA) == '3PM'] <- 'TPM'

colnames(NBA)[colnames(NBA) == '3PA'] <- 'TPA'


NBA <- NBA %>%
  mutate(MIN = MIN / GP)

NBA <- NBA %>%
  mutate(FGM = FGA / GP)

NBA <- NBA %>%
  mutate(FTM = FTM / GP)

NBA <- NBA %>%
  mutate(FTA = FTA / GP)

NBA <- NBA %>%
  mutate(TOV = TOV / GP)

NBA <- NBA %>%
  mutate(PF = PF / GP)

NBA <- NBA %>%
  mutate(ORB = ORB / GP)

NBA <- NBA %>%
  mutate(DRB = DRB / GP)

NBA <- NBA %>%
  mutate(REB = REB / GP)

NBA <- NBA %>%
  mutate(AST = AST / GP)

NBA <- NBA %>%
  mutate(BLK = BLK / GP)

NBA <- NBA %>%
  mutate(PTS = PTS / GP)

NBA <- NBA %>%
  mutate(TPM = TPM / GP)

NBA <- NBA %>%
  mutate(TPA = TPA / GP)

NBA <- NBA %>%
  mutate(STL = STL / GP)







# creating years in league variable

min_year <- NBA %>%
  group_by(Player) %>%
  summarise_at(vars(Season), list(first_season = min))

NBA = merge(x=NBA,y=min_year,by="Player")

NBA <- NBA %>%
  mutate(years_in_league = Season - first_season)

# creating PER variable
NBA <- NBA %>%
  mutate(PER = (PTS + REB + AST + STL + BLK - (FTA - FTM) - (FGA - FGM) - TOV)/ GP)

#Creating dummy variables for categorical columns

NBA$teamNo <- as.numeric(factor(NBA$team))
NBA$positionNo <- as.numeric(factor(NBA$position))
NBA$seasonNo <- as.numeric(factor(NBA$Season))

#visualizations

ggplot(NBA, aes(x = Season, y = salary)) + geom_point() +
  theme_bw()

ggplot(NBA, aes(x = PTS, y = salary)) + geom_point() +
  theme_bw()

ggplot(NBA, aes(x = age, y = salary)) + geom_point() +
  theme_bw()

# feature correlations
ggcorr(NBA[, 3:19])


#Scaling data

NBA$seasonNo <- scale(NBA$seasonNo)
NBA$years_in_league <- scale(NBA$years_in_league)
NBA$GP <- scale(NBA$GP)
NBA$MIN <- scale(NBA$MIN)
NBA$FGM <- scale(NBA$FGM)
NBA$FGA <- scale(NBA$FGA)
NBA$TPM <- scale(NBA$TPM)
NBA$TPA <- scale(NBA$TPA)
NBA$FTM <- scale(NBA$FTM)
NBA$FTA <- scale(NBA$FTA)
NBA$PF <- scale(NBA$PF)
NBA$ORB <- scale(NBA$ORB)
NBA$DRB <- scale(NBA$DRB)
NBA$AST <- scale(NBA$AST)
NBA$STL <- scale(NBA$STL)
NBA$BLK <- scale(NBA$BLK)
NBA$PTS <- scale(NBA$PTS)
NBA$PER <- scale(NBA$PER)
NBA$height_cm <- scale(NBA$height_cm)
NBA$weight_kg <- scale(NBA$weight_kg)
NBA$age <- scale(NBA$age)
NBA$teamNo <- scale(NBA$teamNo)
NBA$positionNo <- scale(NBA$positionNo)


#Splitting into training and testing datasets
library(caret)
set.seed(18L) 
trainIndex <- createDataPartition(NBA$salary,
                                  p = 0.8,
                                  list = FALSE,
                                  times = 1)
NBA_train <- NBA[trainIndex, ]
NBA_test <- NBA[-trainIndex, ]

player_names <- NBA_test[1]

#define response variable
y <- NBA$salary

#define matrix of predictor variables
x <- data.matrix(NBA[, c('GP', 'MIN', 'FGM', 'FGA', 'TPM', 'TPA', 'FTM', 'FTA' , 'PF', 'ORB',
                         'DRB', 'AST', 'STL',  'BLK', 'PTS', 'height_cm', 'weight_kg' 
                         , 'age' , 'teamNo' , 'seasonNo', 'years_in_league', 'positionNo', 'PER')])


# model induction
model <- train(salary ~ GP + MIN + FGM + FGA + TPM + TPA + 
                 FTM + FTA + PF + ORB + DRB + AST + STL + BLK + PTS + 
                 height_cm + weight_kg + age + teamNo + seasonNo + years_in_league +
                 positionNo + PER, data = NBA_train,
               method = "lm")


#Predicting on testing set
p <- predict(model, NBA_test)

#Evaluating Performance
postResample(pred = p, obs = NBA_test$salary)


# estimate variable importance
control <- trainControl(method="repeatedcv", number=100, repeats=10)
importance <- varImp(model, scale=FALSE)

# feature significance summary
plot(importance)


# improve model

y <- NBA$salary

#define matrix of predictor variables
x <- data.matrix(NBA[, c('GP', 'MIN', 'FGM', 'FGA', 'TPM', 'TPA',
                           'FTM', 'FTA', 'PF', 'ORB', 'DRB', 'AST', 'STL', 'BLK', 'PTS', 
                           'height_cm', 'weight_kg', 'age', 'teamNo', 'Season', 'years_in_league',
                           'positionNo', 'PER' )])


#perform k-fold cross-validation to find optimal lambda value
cv_model <- cv.glmnet(x,y, alpha = 1)

#find optimal lambda value that minimizes test MSE
best_lambda <- cv_model$lambda.min
best_lambda

#produce plot of test MSE by lambda value
plot(cv_model) 


best_model <- train(salary ~ GP + MIN + FGM + FGA + TPM + TPA + 
                      FTM + FTA + PF + ORB + DRB + AST + STL + BLK + PTS + 
                      height_cm + weight_kg + age + teamNo + seasonNo + years_in_league +
                      positionNo + PER, data = NBA_train, # use training set
                    method = "glmnet",
                    # alpha and lambda paramters to try
                    tuneGrid = data.frame(alpha=1,lambda = best_lambda))


#Predicting on testing set
p <- predict(best_model, NBA_test)

#Evaluating Performance
postResample(pred = p, obs = NBA_test$salary)


# plot predictions
plot(predict(best_model, NBA_test),                                # Draw plot using Base R
     NBA_test$salary,
     xlab = "Predicted Values",
     ylab = "Observed Values")
abline(a = 0,                                        # Add straight line
       b = 1,
       col = "red",
       lwd = 2)

NBA_testa <- NBA_test %>%
  mutate(error = predict(best_model, NBA_test) - NBA_test$salary)

NBA_testa$player_name <- player_names

last_season <- max(NBA_testa$seasonNo)

NBA_testa <- NBA_testa %>%
  filter(seasonNo == last_season)
 
#find top 10 undervalued players

undervalued <- NBA_testa %>% slice_max(error, n = 10)
undervalued <- undervalued %>% separate(.,'Player',c('first_name','last_name'), sep=' ')

#find top 10 overvalued players

overvalued <- NBA_testa %>% slice_min(error, n = 10)
overvalued <- overvalued %>% separate(.,'Player',c('first_name','last_name'), sep=' ')

#plot most undervalued players of 2018
ggplot(data=undervalued, aes(x=last_name, y=error)) +
  geom_bar(stat="identity", fill="steelblue")+
  labs(title = "Undervalued players 2018",
       y = "Amount Underpayed",
       x = "Last Name")

#plot most overvalued players of 2018

overvalued[5,2] = 'Pope'
overvalued[33] = -(overvalued[33])


ggplot(data=overvalued, aes(x=last_name, y=error)) +
  geom_bar(stat="identity", fill="red")+
  labs(title = "Overvalued Players 2018",
       y = "Amount Overpayed",
       x = "Last Name")

## plotting error distribution

library(ggplot2)
error_dis <- ggplot(NBA_testa, aes(error))
error_dis + geom_histogram(binwidth = 5000000, fill = "steelblue") +
  labs(y = "Count",
       x = "Error")


