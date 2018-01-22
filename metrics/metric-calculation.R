# Compute metrics of interest on survey data

# Set up
library(tidyr)
library(dplyr)
library(stringr)

# Load data
results <- read.csv("results.csv", stringsAsFactors = FALSE)

# Reshape the results so that the columns become a row (gather)
results.long <- gather(results, question, score, -X, -Timestamp)

# Manipulatie the results by doing the following:
#   - Add a column that is (just) the course number
#   - Create a column for "measure" (i.e., value being assessed)
results.long <- results.long %>% 
                mutate(course.number = str_extract(question, "(\\d)+")) %>% 
                separate(question, c('stub', 'measure'), '\\.\\.\\.\\.') %>% 
                select(-stub, -X) 

# Reshape the data to have a separate column for each measure being assessed (spread)
# The rows should now represent a response for a given course (you'll have courses * responses rows)
results.wide <- results.long %>% 
                  spread(measure, score) %>% 
                  select(-Timestamp)

# Group your data by course.number and compute the mean of each measure (across respondents)
# The rows should now represent a course, and the columns are the (average) measures of interest
results.wide <- results.wide %>% 
                  group_by(course.number) %>% 
                  summarize_all(mean)

