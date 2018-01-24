# Compute metrics of interest on survey data

# Set up
library(tidyr)
library(dplyr)
library(stringr)

# Load data
results <- read.csv("Course survey.csv", stringsAsFactors = FALSE) %>% 
            mutate(id=row_number()) %>% 
            select(-Timestamp)

# Reshape the results so that the columns become a row (gather)
results.long <- gather(results, question, score, -id)

# Manipulatie the results by doing the following:
#   - Add a column that is (just) the course number
#   - Create a column for "measure" (i.e., value being assessed)
results.long <- results.long %>% 
                mutate(course.number = str_extract(question, "(\\d)+")) %>% 
                separate(question, c('stub', 'measure'), '\\.\\.\\.\\.') %>% 
                select(-stub) 

# Reshape the data to have a separate column for each measure being assessed (spread)
# The rows should now represent a response for a given course (you'll have courses * responses rows)
results.wide <- results.long %>% 
                  spread(measure, score) %>% 

# Group your data by course.number and compute the mean of each measure (across respondents)
# The rows should now represent a course, and the columns are the (average) measures of interest
results.wide <- results.wide %>% 
                  group_by(course.number) %>% 
                  summarize_all(funs(mean(., na.rm = TRUE)))


# Compute in-class metric
final.results <-  results.wide %>% 
                    mutate(course.quality = How.often.did.you.cry..joy../How.often.did.you.cry..frustration.. + What.proportion.of.the.work.was.valuable../How.many.hours.of.work.were.required.. + How.high.was.your.grade..low.to.high.../How.often.did.you.attend.the.course..1...never..5...always..*2 + How.hirable.are.the.acquired.skills..) %>% 
                    arrange(-course.quality)
