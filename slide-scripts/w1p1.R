## ----vector-types---------------------------------------------------------------------------------
integer <- c(5L, 7L, 3L, 94L) # L explicitly an integer, not double
double <- c(3.27, 8.41, Inf, -Inf)
logical <- c(TRUE, TRUE, TRUE, FALSE, TRUE, FALSE, FALSE)
character <- c("red", "orange", "yellow", "green", "blue", "violet", "rainbow")


## ----imp-coercion---------------------------------------------------------------------------------
c(7L, 3.25)
c(3.24, TRUE, "April")
c(TRUE, 5)


## ----exp-coercion---------------------------------------------------------------------------------
as.integer(c(7L, 3.25))
as.logical(c(3.24, TRUE, "April"))
as.character(c(TRUE, 5)) # still maybe a bit unexpected?


## ----typeof---------------------------------------------------------------------------------------
typeof(c(7L, 3.25))
typeof(as.integer(c(7L, 3.25)))


## ----pipe-----------------------------------------------------------------------------------------
library(magrittr)

typeof(as.integer(c(7L, 3.25)))

c(7L, 3.25) %>%
  as.integer() %>%
  typeof()


## ----imp-coerce2-echo, eval = FALSE---------------------------------------------------------------
## c(1.25, TRUE, 4L)
## c(1L, FALSE)
## c(7L, 6.23, "eight")
## c(TRUE, 1L, 0L, "False")


## ----imp-coerce2-eval-----------------------------------------------------------------------------
typeof(c(1.25, TRUE, 4L))
typeof(c(1L, FALSE))
typeof(c(7L, 6.23, "eight"))
typeof(c(TRUE, 1L, 0L, "False"))


## ----list1----------------------------------------------------------------------------------------
list("a", 7L, 3.25, TRUE)


## ----list2----------------------------------------------------------------------------------------
list(c("a", "b", "c"),
     rnorm(5), 
     c(7L, 2L),
     c(TRUE, TRUE, FALSE, TRUE))

