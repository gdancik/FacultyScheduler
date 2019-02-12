library(gtools) # for permutations
library(stringr) # for removing white space

# given a vector of courses and requested times, 
# return vector of possible course times corresponding to the courses;
# example: if times are 9,10,11 for 2 courses, 
# course times can be 9,10; 9,11; 10,9; 10,11; 11,9; or 11;10
get_possible_times <- function(crse, times) {
  if (length(times) < length(crse)) {
    stop ("# of times must be >= # of courses")
  }
  
  if (length(times) == 1) {
    return(times)
  }
  
  
  p <- permutations(length(times), length(crse), times)
  
  # we need to remove duplicates (if multiple sections of a course)
  # reformat as "course at time"
  f <- function(i, c, pp) {
    sort(paste(c, pp[i,], sep = " at "))
  } 
  s <- sapply(1:nrow(p), f, crse, p)
  s <- t(s)
  
  # find a remove duplicates
  d <- duplicated(s)
  p <- p[!d,]
  
  # return vector of strings
  apply(p, 1, paste, collapse = ", ")
  
}

# for a requests table returns a list
# of possible schedules for each faculty member, with
# each element of the list a data frame of possible
# courses for a faculty member
get_possible_schedules_by_instructor <- function(requests) {

  # get vectors of courses and times
  x <- strsplit(requests$Courses, ",")
  y <- strsplit(requests$Times, ",")

  # remove leading / trailing whitespace
  x <- sapply(x, str_trim)
  y <- sapply(y, str_trim)

  # list possible class offerings by instructor
  instructors <- list()
  for (i in 1:nrow(requests)) {
    ti <- get_possible_times(x[[i]], y[[i]])
    c1 <- paste(x[[i]], collapse = ",")
    instructors[[i]] <- data.frame(instructor = requests$Name[i], 
                          courses = c1, 
                          days = requests$Days[i], 
                          times = ti)
  }
  names(instructors) <- requests$Name
  return(instructors)
}


# returns a schedule from an instructor list at a specific set
# of indices; index provides the row index for each instructor
# e.g., if index <- c(1,3,2), we get the first set of courses
# from the 1st faculty member, the 3rd set of courses from the
# second faculty member, and the 2nd set of courses from the 
# third faculty member
get_schedule_from_instructor_list <- function(inst, index) {
  
  if (length(index) != length(inst)) {
    stop ("index must have same length as instructor list")
  }
  
  s <- NULL
  li <- 1
  for (i in index) {
    s <- rbind(s, inst[[li]][i,])
    li <- li + 1
  }
  return(s)
}


