

add_temp_flags <- function(filename, site_column_name) {
  raw <- read_csv(filename)
  
  rawMets <- raw %>% 
    group_by(hour, site_column_name) %>% 
    summarize(hourlyMean = mean(Temperature, na.rm = TRUE),
              hourlyMax = max(Temperature, na.rm = TRUE)) %>% 
    mutate(hourlyMeanDiff = c(0, abs(diff(dailyMean))),
           hourlyMaxDiff = c(0, abs(diff(dailyMax))))
  
  sitehourFlags <- rawMets %>% 
    filter(hourlyMeanDiff > 3 | hourlyMaxDiff > 3) %>% 
    pull(hour, sampleDate)
  
  dailyMets <- raw %>% 
    group_by(sampleDate, site_column_name) %>% 
    summarize(dailyMean = mean(Temperature, na.rm = TRUE),
              dailyMax = max(Temperature, na.rm = TRUE)) %>% 
    mutate(dailyMeanDiff = c(0, abs(diff(dailyMean))),
           dailyMaxDiff = c(0, abs(diff(dailyMax))))
  
  sitedateFlags <- dailyMets %>% 
    filter(dailyMeanDiff > 3 | dailyMaxDiff > 3) %>% 
    pull(sampleDate)

  diff(dailyMets$dailyMean, lag = 1)
  
  raw %>% 
    mutate(flag = case_when(Temperature > 30 ~ 1,
                            Temperature < -1 ~ 2,
                            
           )
  
  
  #final value returned as result, don't need to assign.
}

# Update the function so heads have probability p_head
toss_coin <- function(n_flips, p_head = 0.5) {
  coin_sides <- c("head", "tail")
  # Define a vector of weights
  weights <- c(p_head, 1-p_head)
  # Modify the sampling to be weighted
  sample(coin_sides, n_flips, replace = TRUE, prob = weights)
}

# Generate 10 coin tosses
toss_coin(10, 0.8)

#data arguments don't take a default, but detail args do.
#... ellipsis allows passing argument to other functions.
#checks on user inputs should be at start of function body
#assertive package for errors on argument inputs

if(!is.numeric(x)){
  stop("x is not numeric")
}

assert_is_numeric(x)

#library(zeallot)
# multi-assignment operator %<-%
#c(ab, bc, de) %<-% session()