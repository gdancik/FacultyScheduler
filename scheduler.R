library(readxl)

source("functions.R")

# read in the data set
courses <- read_excel("courses.xlsx")
View(courses)


# create list of schedules by instructor
instructors <- get_possible_schedules_by_instructor(courses)

# create index across each instructor
n <- lapply(instructors, function(x) 1:nrow(x))
index <- expand.grid(n)

# example of extracting the first possible schedule
schedule1 <- get_schedule_from_instructor_list(instructors, index[1,])
print(schedule1)


