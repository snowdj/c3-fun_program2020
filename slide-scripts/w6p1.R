## ----setup, include = FALSE---------------------------------------------------------------
library(tidyverse)
theme_set(theme_minimal(20))
update_geom_defaults("point", list(size = 3))
knitr::opts_chunk$set(fig.width = 13, 
                      message = FALSE, 
                      warning = FALSE)


## ----plus-infix1--------------------------------------------------------------------------
3 + 5


## ----plus-fun-----------------------------------------------------------------------------
`+`(3, 5) 
 


## ----plus-infix2--------------------------------------------------------------------------
3 + 5 + 7


## ---- plus-fun2---------------------------------------------------------------------------
`+`(7, `+`(3, 5))


## ---- plus-fun3---------------------------------------------------------------------------
library(magrittr)
`+`(3, 5) %>%
	`+`(7)


## -----------------------------------------------------------------------------------------
a <- 7
a
`<-`(a, 7)
a


## ----bug----------------------------------------------------------------------------------
`+` <- function(x, y) {
	if(runif(1) < 0.01) {
		sum(x, y) * -1
	} else {
		sum(x, y)
	}
}
table(map2_dbl(1:500, 1:500, `+`) > 0)
rm(`+`, envir = globalenv())
table(map2_dbl(1:500, 1:500, `+`) > 0)


## ---- lm-a--------------------------------------------------------------------------------
a <- lm
a(hp ~ drat + wt, data = mtcars)


## ----anonymous-fun------------------------------------------------------------------------
vapply(mtcars, function(x) length(unique(x)), FUN.VALUE = double(1))


## ----fun-list-----------------------------------------------------------------------------
funs <- list(
  quarter = function(x) x / 4,
  half = function(x) x / 2,
  double = function(x) x * 2,
  quadruple = function(x) x * 4
)


## ----fun-list-eval------------------------------------------------------------------------
funs$quarter(100)
funs[["half"]](100)
funs[[4]](100)


## ----smry-list----------------------------------------------------------------------------
smry <- list(n = length, 
             n_miss = function(x) sum(is.na(x)),
             n_valid = function(x) sum(!is.na(x)),
             mean = mean, 
             sd = sd)


## ----smry-loop1---------------------------------------------------------------------------
map_dbl(smry, ~.x(mtcars$mpg))


## ----smry-loop1-fail, error = TRUE--------------------------------------------------------
map_dbl(smry, mtcars$mpg)


## ----smry-all-cols------------------------------------------------------------------------
map_df(mtcars, function(col) map_df(smry, ~.x(col)),
       .id = "column")


## ----formals-fun--------------------------------------------------------------------------
formals(lm)


## ----lm-print-----------------------------------------------------------------------------
body(lm)


## ----env1---------------------------------------------------------------------------------
double <- function(x) x*2
environment(double)
environment(lm)


## ----env2-echo, eval = FALSE--------------------------------------------------------------
## x <- 10
## f1 <- function() {
##   x <- 20
##   x
## }
## 
## f1()


## ----env2-eval, echo = FALSE--------------------------------------------------------------
x <- 10
f1 <- function() {
  x <- 20
  x
}

f1()


## ----env3-echo, eval = FALSE--------------------------------------------------------------
## x <- 10
## y <- 20
## f2 <- function() {
##   x <- 1
##   y <- 2
##   sum(x, y)
## }
## f2()
## 


## ----env3-eval, echo = FALSE--------------------------------------------------------------
x <- 10
y <- 20
f2 <- function() {
  x <- 1
  y <- 2
  sum(x, y)
}
f2()



## ----env4-echo, eval = FALSE--------------------------------------------------------------
## x <- 2
## f3 <- function() {
##   y <- 1
##   sum(x, y)
## }
## 
## f3() #<<
## 
## y #<<


## ----env4-eval, echo = FALSE--------------------------------------------------------------
x <- 2
f3 <- function() {
  y <- 1
  sum(x, y)
}

f3()

y


## ----extract_grades, eval = FALSE---------------------------------------------------------
## extract_grades <- function(dif_mod, items) {
##   item_names <- names(items)
##   delta  <- -2.35*(log(dif_mod$alphaMH))
##   grades <- symnum(abs(delta),
##                    c(0, 1, 1.5, Inf),
##                    symbols = c("A", "B", "C"))
##   tibble(item = item_names, delta, grades) %>%
##     mutate(grades = as.character(grades))
## }
## 


## ----read-data-fun, eval = FALSE----------------------------------------------------------
## read_sub_files <- function(file) {
##   read_csv(file) %>%
##     mutate(content_area =
##              str_extract(file, "[Ee][Ll][Aa]|[Rr]dg|[Ww]ri|[Mm]ath|[Ss]ci"),
##            grade = gsub(".+g(\\d\\d*).+", "\\1", file),
##            grade = as.numeric(grade)) %>%
##     select(content_area, grade, everything()) %>%
##     clean_names()
## }
## 
## ifiles <- map_df(ifiles, read_sub_files)


## ----mods---------------------------------------------------------------------------------
mods <- mtcars %>%
	group_by(cyl) %>%
	nest() %>%
	mutate(model = map(data, ~lm(mpg ~ disp + hp + drat, data = .x)))
mods


## ----coefs--------------------------------------------------------------------------------
m <- mods$model[[1]]
coef(m)
coef(m)["disp"]
coef(m)["(Intercept)"]


## ----pull-coef1---------------------------------------------------------------------------
pull_coef <- function(model, coef_name) {
	coef(model)[coef_name]
}
mods %>%
	mutate(intercept = map_dbl(model, pull_coef, "(Intercept)"),
	       disp      = map_dbl(model, pull_coef, "disp"),
	       hp        = map_dbl(model, pull_coef, "hp"),
	       drat      = map_dbl(model, pull_coef, "drat"))


## ----pull-coef2---------------------------------------------------------------------------
pull_coef <- function(model, coef_name = "(Intercept)") {
	coef(model)[coef_name]
}
mods %>%
	mutate(intercept = map_dbl(model, pull_coef))


## ----pull-coef3---------------------------------------------------------------------------
pull_coef <- function(model) {
	coefs <- coef(model)
	data.frame(coefficient = names(coefs),
	           estimate    = coefs)
}
mods %>%
	mutate(coefs = map(model, pull_coef))


## ----unnest-------------------------------------------------------------------------------
mods %>%
	mutate(coefs = map(model, pull_coef)) %>%
	unnest(coefs)


## ----unnest-better------------------------------------------------------------------------
mods %>%
	mutate(coefs = map(model, pull_coef)) %>%
	select(cyl, coefs) %>%
	unnest(coefs)


## ----unnest-success-table, error = TRUE---------------------------------------------------
mods %>%
	mutate(coefs = map(model, pull_coef)) %>%
	select(cyl, coefs) %>%
	unnest(coefs) %>%
	pivot_wider(names_from = "coefficient", 
	            values_from = "estimate") %>% 
  arrange(cyl)


## ----df-----------------------------------------------------------------------------------
set.seed(42)
df <- tibble::tibble(
  a = rnorm(10, 100, 150),
  b = rnorm(10, 100, 150),
  c = rnorm(10, 100, 150),
  d = rnorm(10, 100, 150)
)

df


## ----scale1-------------------------------------------------------------------------------
df %>%
	mutate(a = (a - min(a, na.rm = TRUE)) / 
                 (max(a, na.rm = TRUE) - min(a, na.rm = TRUE)))


## ----scale2-------------------------------------------------------------------------------
df %>%
	mutate(a = (a - min(a, na.rm = TRUE)) / 
                 (max(a, na.rm = TRUE) - min(a, na.rm = TRUE)),
	       b = (b - min(b, na.rm = TRUE)) / 
                 (max(b, na.rm = TRUE) - min(b, na.rm = TRUE)),
	       c = (c - min(c, na.rm = TRUE)) / 
                 (max(c, na.rm = TRUE) - min(c, na.rm = TRUE)),
	       d = (d - min(d, na.rm = TRUE)) / 
                 (max(d, na.rm = TRUE) - min(d, na.rm = TRUE)))


## ----scale3-------------------------------------------------------------------------------
map_df(df, ~(.x - min(.x, na.rm = TRUE)) / 
              (max(.x, na.rm = TRUE) - min(.x, na.rm = TRUE)))


## ----body-fun, eval = FALSE---------------------------------------------------------------
## (x - min(x, na.rm = TRUE)) /
##   (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))


## ----rescale1-----------------------------------------------------------------------------
rescale01 <- function(x) {
	(x - min(x, na.rm = TRUE)) / 
      (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}


## ----test-rescale1------------------------------------------------------------------------
rescale01(c(0, 5, 10))
rescale01(c(seq(0, 100, 10)))


## ----rescale2-----------------------------------------------------------------------------
rescale01b <- function(x) {
	z <- na.omit(x)
	min_z <- min(z)
	max_z <- max(z)

	(z - min_z) / (max_z - min_z)
}


## ----test-rescale2------------------------------------------------------------------------
rescale01b(c(0, 5, 10))
rescale01b(c(seq(0, 100, 10)))


## -----------------------------------------------------------------------------------------
identical(rescale01(c(0, 1e5, .01)), rescale01b(c(0, 1e5, 0.01)))

rand <- rnorm(1e3)
identical(rescale01(rand), rescale01b(rand))


## ----df-looped----------------------------------------------------------------------------
map_df(df, rescale01b)


## ----condition-template1, eval = FALSE----------------------------------------------------
## 
## function() {
## 	if (condition) {
## 
##   # code executed when condition is TRUE
## 	
## 	} else {
##   # code executed when condition is FALSE
## 	
## 	}
## }


## ----condition-template2, eval = FALSE----------------------------------------------------
## 
## function() {
## 	if (this) {
## 
##   # do this
## 	
## 	} else if (that) {
## 
##   # do that
## 	
## 	} else {
## 
## 	# something else
## 	
## 	}
## }


## ----mean2--------------------------------------------------------------------------------
mean2 <- function(x) {
	if(is.numeric(x)) {
		mean(x)
	}
	else {
		return()
	}
}


## ----mean2-test---------------------------------------------------------------------------
mean2(rnorm(12))
mean2(letters[1:5])


## ----df-mean------------------------------------------------------------------------------
means_df <- function(df) {
	means <- map(df, mean2) # calculate means
	nulls <- map_lgl(means, is.null) # find null values
	means_l <- means[!nulls] # subset list to remove nulls
	
	as.data.frame(means_l) # return a df
}


## -----------------------------------------------------------------------------------------
head(iris)
means_df(iris)


## ----ozone-means--------------------------------------------------------------------------
head(airquality)
means_df(airquality)


## ----means2-redefined---------------------------------------------------------------------
mean2 <- function(x, ...) {
	if(is.numeric(x)) {
		mean(x, ...)
	}
	else {
		return()
	}
}


## ----df-mean-redefined--------------------------------------------------------------------
means_df <- function(df, ...) {
	means <- map(df, mean2, ...) # calculate means
	nulls <- map_lgl(means, is.null) # find null values
	means_l <- means[!nulls] # subset list to remove nulls
	
	as.data.frame(means_l) # return a df
}


## ----ozone-means2, error------------------------------------------------------------------
means_df(airquality)
means_df(airquality, na.rm = TRUE)

