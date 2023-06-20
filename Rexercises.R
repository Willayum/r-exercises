#  Exercise 3.1.0.1, creating three random Poisson variables
?rpois # in order to learn about the Poisson distribution, related code
#and syntax
#found command rpois(n, lambda) where n is the number of wanted numbers
#output and lambda is the vector of their means
(rpois(3, 5)) # this command creates three Poisson random variables with
# means of 5 and prints the output, following the exercise guidelines
######
#  Exercise 3.2.0.1, creating three Pois var par = 0,1,2 with length 10
#slightly confused here, as I believe length is supposed to equal n and
# the parameters are supposed to equal the lambda in three rpois commands
(rpois(10, 0))
(rpois(10, 1))
(rpois(10, 2)) # this would be my guess, gives 3 lists of Poisson var's
######
# Exercise 3.3.0.1, create three random binomial vriables with pmap()
?pmap #had to install and lead itdyverse package
?rbinom 
#create a vector with tribble command then put it through rbinom
para <- tribble(
  ~x, ~y, #commas are important
  1, 1, #open comma to show end of numerical input
  1, 1,
  1, 1
)
para %>%
      pmap(rbinom(n = 10, size = 10, prob = .3)) %>%
     str() #couldn't get to work, don't understand the inputs
#needed to develop wanted output
para <- tribble(
  ~x, ~y, 
  1, 1, 
  1, 1,
  1, 1
)
#figured it out, was just missing the tilda below
para %>%
  pmap(~ rbinom(n = 10, size = 10, prob = .3)) %>%
  str()

######
# Exercise 3.3.0.2, run and interpret output
?rnorm #to find correct arguments
params <- tribble(
  ~n, ~mu, ~sigma, 
  10,    0,    1,     
  20,    5,    1,     
  30,    10,   2      
)
params %>% #doesn't run, gives error in pmap and .f
  pmap(rnorm) %>% #missing n, mean, and sd vectors along with ~
  str()
params <- tribble(
  ~n, ~mu, ~sigma, 
  10,    0,    1,     
  20,    5,    1,     
  30,    10,   2      
)
params %>% 
  pmap(~ rnorm(30, 20, 10)) %>% #fixed
  str()
######
# Exercise 3.4.0.1 create tribble with 3 dif dists and params, use invoke
?invoke_map
?rbinom
?Normal
?Beta
ds <- tribble( #followed example format
  ~f, ~para, 
  "rbinom", list(n = 10, size = 10, prob = .3),
  "rnorm", list(n = 12, mean = 5, sd = 2),
  "rbeta", list(n = 11, shape1 = 6, shape2 = 7, ncp = 4)
)
ds %>% 
  mutate(ds = invoke_map(f, para)) %>%
  str() #to see output, it worked first try!!
# Exercise 3.5.0.1, repeat previous code to create bias in rbinom
#confused as to which part of the code I am replicating and changing
sim <- tribble(
  ~f,      ~params,
  "rbinom", list(size = 1, prob = 0.5) #I assume it's this and I just change the prob
)
  sim <- tribble(
    ~f,      ~params,
    "rbinom", list(size = 1, prob = 0.6) %>% #coin is biased, was this it?
      str()
)
###### 
# Exercise 4.1.3.2, change pull to select, what happens?
install.packages("Lahman") #could not get this package to download
  players <- Batting %>% #code in question
    group_by(playerID) %>% 
    summarise(AB_total = sum(AB), 
              H_total = sum(H)) %>% 
    na.omit() %>% 
    filter(H_total>500) %>%  # collect players with a lot of data
    sample_n(size=30) %>%    # randomly sample 30 players
    pull(playerID)           # `select` will produce a dataframe, 
  # `pull` gives a list which will be easier to work with when we `filter` by players
  players <- Batting %>% 
    group_by(playerID) %>% 
    summarise(AB_total = sum(AB), 
              H_total = sum(H)) %>% 
    na.omit() %>% 
    filter(H_total>500) %>%  
    sample_n(size=30) %>%  '  
    select(playerID) #select over pull takes a data frame instead of vector    
  # #when using select instead of pull, the resulting object remains a data frame with 
  # #a single column containing the playerID values. So, players would still 
  # #be a data frame with one column. I don'#t know why this changed colour.
  #when using select instead of pull, the resulting object remains a data frame 
  #with a single column containing the playerID values. So, players would still 
  #be a data frame with one column
######
# Exercise 4.1.4.1, attempt to code James-Stein estimator equation
#try to understand the notation and fully understand what is being estimated
#Googling provided the use of the function command to integrate math into R
   #function_name <- function(parameters){   ----from dataquest.io
      #function body 
    # }
jse <- function(meanj, obvserved, variancej, number ) {
  meanj + (1 - (number - 3) * variancej / sum((observed - meanj)^2) * (obvserved - meanj))
} #follows the same logic as the equation in bookdown, just adapting to R
######
# Exercise 4.1.6, test JS estimator with n=2,3,10,30,1000
#using my code above, I can now provide values to my given variables
number <- c(2, 3, 10, 30, 100, 1000) 
meanj = 2
variancej = .5
observed = 3
#not sure how to proceed from here, does not print solution
jse <- function(meanj = 2, observed = 3, variancej = 0.5, 
                number = c(2, 3, 10, 30, 100, 1000)) {
  meanj + (1 - (number - 3) * variancej / sum((observed - meanj)^2) * 
             (observed - meanj))
} #same issue
jse_result <- jse(meanj = 2, observed = 3, variancej = 0.5, number = c(2, 3, 10, 30, 100, 1000))
print(jse_result) #I think this worked, had to input the data a differnent way?
######
#checked above code with different numbers and inputs to try and verify
#it's following the wanted output for the exercises. Could not find any issues, however,
#my still very limited knowledge of R could cause me to miss errors.