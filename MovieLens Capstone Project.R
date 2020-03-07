## ----setup, include=FALSE----------------------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, fig.align = 'center', cache=FALSE, cache.lazy = FALSE)



## ----include=FALSE, echo=FALSE-----------------------------------------------------------------------------------------------------------------------
### 2.0 The Data set

### 2.1 Downloading the data

################################
## This code is provided by the edx staff to download and create an edx set, validation set
################################

## Loading required libraries

## Install all needed packages if not present
if(!require(tidyverse)) install.packages("tidyverse") 
if(!require(kableExtra)) install.packages("kableExtra")
if(!require(tidyr)) install.packages("tidyr")
if(!require(tidyverse)) install.packages("tidyverse")
if(!require(stringr)) install.packages("stringr")
if(!require(forcats)) install.packages("forcats")
if(!require(ggplot2)) install.packages("ggplot2")
## Loading all needed libraries
library(dplyr)
library(tidyverse)
library(kableExtra)
library(tidyr)
library(stringr)
library(forcats)
library(ggplot2)
library(lubridate)
library(caret)
library(magrittr)
library(tinytex)
library(knitr)
library(latexpdf)

## Downloading files

## MovieLens 10M dataset:
 ## https://grouplens.org/datasets/movielens/10m/
 ## http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
 download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)
 
ratings <- read.table(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                      col.names = c("userId", "movieId", "rating", "timestamp"))



## Build the data set

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
 colnames(movies) <- c("movieId", "title", "genres")
 movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                            title = as.character(title),
                                            genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

## Validation set will be 10% of MovieLens data

set.seed(1, sample.kind="Rounding")
## if using R 3.5 or earlier, use `set.seed(1)` instead

test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
 edx <- movielens[-test_index,]
 temp <- movielens[test_index,]
 
## To make sure that the userId and movieId in validation set are also in edx set

 validation <- temp %>% 
      semi_join(edx, by = "movieId") %>%
      semi_join(edx, by = "userId")
 
## To add rows removed from validation set back into edx set

 removed <- anti_join(temp, validation)
 edx <- rbind(edx, removed)
 
 rm(dl, ratings, movies, test_index, temp, movielens, removed)
 



## ----echo=FALSE, include=TRUE------------------------------------------------------------------------------------------------------------------------
glimpse(edx)



## ----echo=FALSE, include=TRUE------------------------------------------------------------------------------------------------------------------------
glimpse(validation)



## ----echo=FALSE, include=TRUE------------------------------------------------------------------------------------------------------------------------
## How many unique users, movies and genres we are dealing with .

edx %>% summarize(Unique_Users = n_distinct(userId),
Unique_Movies = n_distinct(movieId),
Unique_Genres = n_distinct(genres))



## ----echo=FALSE, include=TRUE------------------------------------------------------------------------------------------------------------------------


## converting 'timestamp' to human readable form and creating 'year_rated' column

edx <- mutate(edx, year_rated = year(as_datetime(timestamp)))
head(edx)


validation <- mutate(validation, year_rated = year(as_datetime(timestamp)))
head(validation)



## ----------------------------------------------------------------------------------------------------------------------------------------------------
edx <- edx %>% mutate(year = as.numeric(str_sub(title,-5,-2)))
head(edx)


## ----echo=FALSE, include=TRUE------------------------------------------------------------------------------------------------------------------------

sapply(edx, function(x) sum(is.na(x))) %>% 
kable() %>%
   kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                 position = "center",
                 font_size = 10,
                 full_width = FALSE)


## ----echo=FALSE, include=TRUE------------------------------------------------------------------------------------------------------------------------
## Now , let us remove unncessary columns from both 'edx' as well as 'validation' sets

edx <- edx %>% select( - title, - timestamp)
head(edx)


validation <- validation %>% select( - title, - timestamp)
head(validation)




## ----echo=FALSE, include=TRUE------------------------------------------------------------------------------------------------------------------------
summary(edx)



## ----echo=FALSE, include=TRUE------------------------------------------------------------------------------------------------------------------------

group <- ifelse((edx$rating == 1|edx$rating == 2|edx$rating == 3|edx$rating == 4|edx$rating == 5), "whole_star","half_star")

rating_distribution <- data.frame(edx$rating,group)

head(rating_distribution)



## ----echo=FALSE, include=TRUE------------------------------------------------------------------------------------------------------------------------
rating_distribution %>% ggplot(aes(edx$rating)) +
geom_histogram(fill = "blue")+
labs(title = "number _of_ratings-for_each_rating",
x = "rating",
y = "number_of_ratings")




## ----echo=FALSE, include=TRUE------------------------------------------------------------------------------------------------------------------------
## Let us now see what response different movies attract going by their count of ratings
edx %>% count(movieId)%>%
ggplot(aes(n))+
geom_histogram(bin = 30, fill= "green")+
scale_x_log10()+
ggtitle("Movies")+  
labs(subtitle = "number_of_ratings_by_movieId",
x = "movieId",
y = "number_of_ratings", caption ="source data : edx set")



## ----echo=FALSE, include=TRUE------------------------------------------------------------------------------------------------------------------------

## Let us now see how each user rates different movies

edx %>% count(userId)%>%
ggplot(aes(n))+
geom_histogram(bins = 30,fill = "green")+
scale_x_log10()+
ggtitle("Users")+
labs(subtitle = "number_of_ratings_by_userId",
x = "userId",
y = " number_of_ratings",caption ="source data : edx set")



## ----echo=FALSE, include=TRUE------------------------------------------------------------------------------------------------------------------------
edx %>% ggplot(aes(year_rated))+
geom_histogram(fill = "green")+
labs(title = "Distribution of yearwise ratings",
subtitle = "Year the rating was given",
x = "Year",
y = "number_of_ratings")



## ----echo=FALSE, include=TRUE------------------------------------------------------------------------------------------------------------------------
edx %>% ggplot(aes(year))+
geom_histogram(fill = "darkgreen")+
labs(title = "Distribution of movie ratings by release year",
subtitle = "number of ratings by release year",
x = "year",
y = "number of ratings")


## ----echo=FALSE, include=TRUE------------------------------------------------------------------------------------------------------------------------

## Rating distribution per genre for top 20 genres by ratings
edx %>%
   group_by(genres) %>%
   summarise(count = n()) %>%
   top_n(20,count) %>%
   arrange(desc(count)) %>%
   kable() %>%
   kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                 position = "center",
                 font_size = 10,
                 full_width = FALSE)





## ----echo=FALSE, include=TRUE------------------------------------------------------------------------------------------------------------------------

edx %>%
   group_by(genres) %>%
   summarise(count = n()) %>%
   top_n(20,count) %>%
   ggplot(aes(genres, count)) +
   theme_classic()  +
   geom_col(fill = "green") +
   theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
   labs(title = "Number of ratings Per Genre",
        x = "Genre",
        y = "Number of ratings", caption ="source data : edx set")



## ----echo=FALSE, include=TRUE------------------------------------------------------------------------------------------------------------------------

RMSE <- function(true_ratings, predicted_ratings){
sqrt(mean((true_ratings - predicted_ratings)^2))}



## ----echo=FALSE, include=TRUE------------------------------------------------------------------------------------------------------------------------

set.seed(1, sample.kind="Rounding")
test_index <- createDataPartition(edx$rating,times = 1, p = 0.1, list = FALSE) # Created 'test_index'
train <- edx[- test_index , ]     # Created 'train' set
test  <- edx[test_index , ]    # Created 'test'set
#

 


## ----echo=FALSE, include=TRUE------------------------------------------------------------------------------------------------------------------------

glimpse(train)
glimpse(test)



## ----echo=FALSE, include=TRUE------------------------------------------------------------------------------------------------------------------------

## Calculating the average accross all movies
mu_hat <- mean(train$rating)
mu_hat



## ----echo=FALSE, include=TRUE------------------------------------------------------------------------------------------------------------------------

base_rmse <- RMSE(test$rating,mu_hat)
base_rmse



## ----echo=FALSE, include=TRUE------------------------------------------------------------------------------------------------------------------------

rmse_results <- data.frame(method = "Baseline approach", RMSE = base_rmse)
rmse_results%>%
knitr::kable()%>%
kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
position = "center",
font_size = 10,
full_width = FALSE)



## ----echo=FALSE, include=TRUE------------------------------------------------------------------------------------------------------------------------

## Computing average of 'rating' accross all the movies 'mu_hat'

mu_hat <- mean(train$rating)

## Computing averages by movie 'b_i'

b_i <- train %>%
group_by(movieId) %>%
summarize(b_i = mean(rating - mu_hat))


## Computing the predicted ratings on test set

predicted_ratings_movie <-test %>%
left_join(b_i, by='movieId') %>%
replace_na(list(b_i=0))%>%
mutate(pred = mu_hat + b_i)
rmse_movie <- RMSE(test$rating,predicted_ratings_movie$pred)
rmse_results <- bind_rows(rmse_results,data_frame(method="Movie Effect ",RMSE = rmse_movie ))
rmse_results
rmse_results %>% knitr::kable()%>%
kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
position = "center",
font_size = 10,
full_width = FALSE)



## ----echo=FALSE, include=TRUE------------------------------------------------------------------------------------------------------------------------

## Mean accross all the movies 'mu_hat'

mu_hat <- mean(train$rating)


## Calculating the average by movie 'b_i'

b_i <- train %>%
   group_by(movieId) %>%
   summarize(b_i = mean(rating - mu_hat))


## Calculating the averages by user 'b_u'

b_u <- train %>%
left_join(b_i , by = "movieId") %>%
group_by(userId) %>%
summarize(b_u = mean(rating - mu_hat -  b_i))


## Computing the predicted ratings on test dataset

predicted_ratings_user <- test %>%
left_join(b_i, by = "movieId") %>%
left_join(b_u,by = "userId") %>%
replace_na(list(b_i=0,b_u=0))%>%
mutate(pred = mu_hat + b_i + b_u ) %>% .$pred
user_movie_rmse <- RMSE(test$rating,predicted_ratings_user)
user_movie_rmse
rmse_results <- rbind(rmse_results, data.frame(method = "User & Movie effect", RMSE = user_movie_rmse ))
rmse_results
rmse_results%>%
knitr::kable()%>%
kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
position = "center",
font_size = 10,
full_width = FALSE)



## ----echo=FALSE, include=TRUE------------------------------------------------------------------------------------------------------------------------

## Calculating the average accross all the movies 'mu_hat'

mu_hat <- mean(edx$rating)

## Calculating the average by movie 'b_i'
b_i <- train %>%
   group_by(movieId) %>%
   summarize(b_i = mean(rating - mu_hat))

## Calculating the average by user 'b_u'
b_u <- train %>%
   left_join(b_i, by='movieId') %>%
   group_by(userId) %>%
   summarize(b_u = mean(rating - mu_hat - b_i))

## Calculate the average by genre 'b_u_g'
b_u_g <- train %>%
   left_join(b_i, by='movieId') %>%
   left_join(b_u, by='userId') %>%
   group_by(genres) %>%
   summarize(b_u_g = mean(rating - mu_hat - b_i - b_u))


## Computing the predicted ratings on test dataset
movie_user_genre_rmse <- test %>%
   left_join(b_i, by='movieId') %>%
   left_join(b_u, by='userId') %>%
   left_join(b_u_g, by='genres') %>%
   replace_na(list(b_i=0,b_u=0,b_u_g = 0))%>%
   mutate(pred = mu_hat + b_i + b_u + b_u_g) %>%
   pull(pred)
movie_user_genre_rmse_result <- RMSE(test$rating, movie_user_genre_rmse)

## Adding the results to the results dataset
rmse_results <- rmse_results %>% add_row(method ="Movie+User+Genre effectl", RMSE = movie_user_genre_rmse_result)
rmse_results
rmse_results %>%
knitr::kable()%>%
kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
position = "center",
font_size = 10,
full_width = FALSE)



## ----echo=FALSE, include=TRUE------------------------------------------------------------------------------------------------------------------------

## Testing 'user_and_movie_model' on the validation set

## Mean accross all the movies 'mu_hat'

mu_hat <- mean(train$rating)


## Calculating the average by movie 'b_i'

b_i <- train %>%
   group_by(movieId) %>%
   summarize(b_i = mean(rating - mu_hat))


## Calculating the averages by user 'b_u'

b_u <- train %>%
left_join(b_i , by = "movieId") %>%
group_by(userId) %>%
summarize(b_u = mean(rating - mu_hat -  b_i))


## Computing the predicted ratings on the validation dataset

predicted_ratings_user_movie <- validation %>%
left_join(b_i, by = "movieId") %>%
left_join(b_u,by = "userId") %>%
replace_na(list(b_i=0,b_u=0))%>%
mutate(pred = mu_hat + b_i + b_u ) %>% .$pred
user_movie_valid_rmse <- RMSE(validation$rating,predicted_ratings_user_movie)
user_movie_valid_rmse

## Adding the results to the results dataset
rmse_results <- rbind(rmse_results, data.frame(method = "User & Movie effect on validation set", RMSE = user_movie_valid_rmse ))
rmse_results
rmse_results%>%
knitr::kable()%>%
kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
position = "center",
font_size = 10,
full_width = FALSE)



## ----echo=FALSE, include=TRUE------------------------------------------------------------------------------------------------------------------------

## Testing on the validation set

## Calculating the average accross all the movies 'mu_hat'
mu_hat <- mean(edx$rating)

## Calculating the average by movie 'b_i'
b_i <- train %>%
   group_by(movieId) %>%
   summarize(b_i = mean(rating - mu_hat))

## Calculating the average by user 'b_u'
b_u <- train %>%
   left_join(b_i, by='movieId') %>%
   group_by(userId) %>%
   summarize(b_u = mean(rating - mu_hat - b_i))

## Calculating the average by genre 'b_u_g'
b_u_g <- train %>%
   left_join(b_i, by='movieId') %>%
   left_join(b_u, by='userId') %>%
   group_by(genres) %>%
   summarize(b_u_g = mean(rating - mu_hat - b_i - b_u))


## Computing the predicted ratings on the validation dataset
predicted_ratings_user_movie_genre  <- validation %>%
   left_join(b_i, by='movieId') %>%
   left_join(b_u, by='userId') %>%
   left_join(b_u_g, by='genres') %>%
   replace_na(list(b_i=0, b_u=0,b_u_g = 0))%>%
   mutate(pred = mu_hat + b_i + b_u + b_u_g) %>%
   pull(pred)
movie_user_genre_valid_rmse <- RMSE(validation$rating,predicted_ratings_user_movie_genre)
movie_user_genre_valid_rmse

## Adding the results to the results dataset
rmse_results <- rbind(rmse_results, data.frame(method = "User & Movie & genre effect on validation set", RMSE = movie_user_genre_valid_rmse ))
rmse_results
rmse_results%>%
knitr::kable()%>%
kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
position = "center",
font_size = 10,
full_width = FALSE)




## ----echo=FALSE, include=TRUE------------------------------------------------------------------------------------------------------------------------

## Testing regularised user_movie model on the validation set

## Defining a table of lambdas
lambdas <- seq(0, 10, 0.25)

## For each lambda we shall be working on b_i, b_u , predict rating and test accuracy.
RMSE_function_reg  <- sapply(lambdas,function(l){

## Calculating average accross all the movies 'mu_hat'
mu_hat <- mean(train$rating)

## Calculating the average by movie 'b_i'

b_i <- train %>%
group_by(movieId)%>%
summarize(b_i = sum(rating - mu_hat)/(n()+ l))

## Calculating the average by user 'b_u'
b_u <- train%>%
left_join(b_i, by = "movieId")%>%
group_by(userId)%>%
summarize(b_u = sum(rating - b_i - mu_hat)/(n()+ l) )

## Computing the predicted ratings on the validation dataset
predicted_ratings_reg <- validation %>%
left_join(b_i , by = "movieId")%>%
left_join(b_u , by = "userId")%>%
replace_na(list(b_i=0, b_u=0))%>%
mutate(pred = mu_hat+b_i+b_u)%>% .$pred
return(RMSE(validation$rating,predicted_ratings_reg))

})
## Plot RMSE_function_reg vs. lambdas to select optimal lambda
qplot(lambdas,RMSE_function_reg)



## ----echo=FALSE, include=TRUE------------------------------------------------------------------------------------------------------------------------
lambda <- lambdas[which.min(RMSE_function_reg)]
lambda



## ----echo=FALSE, include=TRUE------------------------------------------------------------------------------------------------------------------------

## Calculating average accross all the movies 'mu_hat'
mu_hat <- mean(train$rating)

## Computing the regularised estimate by movie 'b_i' using lambda
b_i <- train %>%
group_by(movieId)%>%
summarize(b_i = sum(rating - mu_hat)/(n() + lambda),n_i = n())

## Computing regularised estimate by user 'b_u' using lambda
b_u <- train %>%
left_join(b_i, by = "movieId")%>%
group_by(userId)%>%
summarize(b_u = sum(rating - mu_hat - b_i)/(n() + lambda), n_u = n())

## Computing the predicted ratings on the validation dataset
predicted_ratings_reg <- validation %>%
left_join(b_i, by = "movieId")%>%
left_join(b_u, by = "userId")%>%
replace_na(list(b_i=0, b_u=0))%>%
mutate(pred = mu_hat + b_i +b_u)%>% .$pred

## Test and save results
user_movie_reg_rmse  <- RMSE(validation$rating,predicted_ratings_reg)
user_movie_reg_rmse
rmse_results <- rbind(rmse_results, data.frame(method = "Regularised User & Movie  effect model on validation set" , RMSE = user_movie_reg_rmse  ))
rmse_results%>%
knitr::kable()%>%
kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
position = "center",
font_size = 10,
full_width = FALSE)




## ----echo=FALSE, include=TRUE------------------------------------------------------------------------------------------------------------------------
##  Testing the regularized 'User and Movie and genre' effect model on the validation set

## Defining a table of lambdas
lambdas <- seq(0, 10, 0.25)

## For each lambda we shall be working on b_i, b_u , predict rating and test accuracy.
RMSE_function_reg  <- sapply(lambdas,function(l){
## Calculating average accross all the movies 'b_i'
mu_hat <- mean(train$rating)

## Calculating the average by movie 'b_i'
b_i <- train %>%
group_by(movieId)%>%
summarize(b_i = sum(rating - mu_hat)/(n()+ l))

## Calculating the average by user 'b_u'
b_u <- train%>%
left_join(b_i, by = "movieId")%>%
group_by(userId)%>%
summarize(b_u = sum(rating - b_i - mu_hat)/(n()+ l) )

## Calculating the average by genre'b_U_g'

b_u_g <- train %>%
   left_join(b_i, by='movieId') %>%
   left_join(b_u, by='userId') %>%
   group_by(genres) %>%
   summarize(b_u_g = sum(rating - mu_hat - b_i - b_u)/(n()+ l))

## Computing the predicted ratings on the validation dataset
predicted_ratings_reg <- validation %>%
left_join(b_i , by = "movieId")%>%
left_join(b_u , by = "userId")%>%
left_join(b_u_g, by = "genres") %>%
replace_na(list(b_i=0, b_u=0,b_u_g = 0))%>%
mutate(pred = mu_hat+b_i+b_u+b_u_g)%>% .$pred
return(RMSE(validation$rating, predicted_ratings_reg))
})
## Plot RMSE_function_reg vs. lambdas to select optimal lambda
qplot(lambdas,RMSE_function_reg)




## ----echo=FALSE, include=TRUE------------------------------------------------------------------------------------------------------------------------

lambda <- lambdas[which.min(RMSE_function_reg)]
lambda



## ----echo=FALSE, include=TRUE------------------------------------------------------------------------------------------------------------------------

## Calculating average accross all the movies 'mu_hat'
mu_hat <- mean(train$rating)

## Computing regularised estimate of b_i using lambda
b_i <- train %>%
group_by(movieId)%>%
summarize(b_i = sum(rating - mu_hat)/(n() + lambda),n_i = n())

## Computing regularised estimate of b_u using lambda
b_u <- train %>%
left_join(b_i, by = "movieId")%>%
group_by(userId)%>%
summarize(b_u = sum(rating - mu_hat - b_i)/(n() + lambda), n_u = n())

## Computing regularised estimate of b_u_g using lambda
b_u_g <- train %>%
   left_join(b_i, by='movieId') %>%
   left_join(b_u, by='userId') %>%
   group_by(genres) %>%
   summarize(b_u_g = sum(rating - mu_hat - b_i - b_u)/(n()+ lambda), n_u_g = n())


predicted_ratings_reg <- validation %>%
left_join(b_i, by = "movieId")%>%
left_join(b_u, by = "userId")%>%
left_join(b_u_g, by = "genres")%>%
replace_na(list(b_i=0, b_u=0,b_u_g = 0))%>%
mutate(pred = mu_hat + b_i +b_u + b_u_g)%>% .$pred
## Test and save results
user_movie_genre_reg_rmse  <- RMSE(validation$rating,predicted_ratings_reg)
user_movie_genre_reg_rmse
rmse_results <- rbind(rmse_results, data.frame(method = "Regularised User & Movie & genre  effect model on validation set" , RMSE = user_movie_genre_reg_rmse  ))
rmse_results%>%
knitr::kable()%>%
kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
position = "center",
font_size = 10,
full_width = FALSE)


