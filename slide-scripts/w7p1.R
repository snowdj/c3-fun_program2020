## ----setup, include = FALSE-----------------------------------------------
library(tidyverse)
theme_set(theme_minimal(20))
update_geom_defaults("point", list(size = 3))
knitr::opts_chunk$set(fig.width = 13, 
                      message = FALSE, 
                      warning = FALSE)


## ----vects1, eval = FALSE-------------------------------------------------
## c(1, NA, NA, 3, 3, 9, NA)
## c(NA, 3, NA, 4, NA, NA, NA)


## ----vects2---------------------------------------------------------------
a <- c(1, NA, NA, 3, 3, 9, NA)
b <- c(NA, 3, NA, 4, NA, NA, NA)


## ----single-case----------------------------------------------------------
is.na(a) 
is.na(b)
is.na(a) & is.na(b)
sum(is.na(a) & is.na(b))


## ----both_na-fun----------------------------------------------------------
both_na <- function(x, y) {
	sum(is.na(x) & is.na(y))
}


## ----test-both_na---------------------------------------------------------
both_na(a, b)
both_na(c(a, a), c(b, b))
both_na(a, c(b, b)) # ???


## ----recycle1-------------------------------------------------------------
data.frame(nums = 1:4,
           lets = c("a", "b"))


## ----recycle-fail, error = TRUE-------------------------------------------
data.frame(nums = 1:3,
           lets = c("a", "b"))


## ----error1a, eval = FALSE------------------------------------------------
## both_na <- function(x, y) {
## 	if(condition) {
## 		stop("message")
## 	}
## 	sum(is.na(x) & is.na(y))
## }


## ----error1b, eval = FALSE------------------------------------------------
## both_na <- function(x, y) {
## 	if(condition) {
## 		stop("message")
## 	}
## 	sum(is.na(x) & is.na(y))
## }


## ----error-message1, error = TRUE-----------------------------------------
both_na <- function(x, y) {
	if(length(x) != length(y)) {
		stop("Vectors are of different lengths")
	}
	sum(is.na(x) & is.na(y))
}
both_na(a, b)
both_na(a, c(b, b))


## ----error-message2, error = TRUE-----------------------------------------
both_na <- function(x, y) {
	if(length(x) != length(y)) {
		v_lngths <- paste0("x = ", length(x), ", y = ", length(y))
		stop("Vectors are of different lengths:", v_lngths)
	}
	sum(is.na(x) & is.na(y))
}
both_na(a, c(b, b))


## ----z-score-stopifnot, error = TRUE--------------------------------------
z_score <- function(x) {
  stopifnot(is.numeric(x))
  x <- x[!is.na(x)]
  (x - mean(x)) / sd(x)
}
z_score(c("a", "b", "c"))
z_score(c(100, 115, 112))


## ----error-warn-----------------------------------------------------------
both_na <- function(x, y) {
	if(length(x) != length(y)) {
		lx <- length(x)
		ly <- length(y)
		
		v_lngths <- paste0("x = ", lx, ", y = ", ly)

		if(lx %% ly == 0 | ly %% lx == 0) {
			warning("Vectors were recycled (", v_lngths, ")")
		}
		else {
			stop("Vectors are of different lengths and are not recyclable:",
			     v_lngths)	
		}
	}
	sum(is.na(x) & is.na(y))
}


## ----test-error-warn, warning = TRUE, error = TRUE------------------------
both_na(a, c(b, b))
both_na(a, c(b, b)[-1])


## ----return-nothing-------------------------------------------------------
add_two <- function(x) {
	result <- x + 2
}


## ----add_two-result-------------------------------------------------------
add_two(7)
add_two(5)


## ----add_two--------------------------------------------------------------
add_two.1 <- function(x) {
	result <- x + 2
	result
}
add_two.2 <- function(x) {
	x + 2
}


## ----add_two.3------------------------------------------------------------
add_two.3 <- function(x) {
	result <- x + 2
	return(result)
}


## ----fun-names------------------------------------------------------------
f <- function(x) {
	x <- sort(x)
	data.frame(value = x, 
	           p = ecdf(x)(x))
}

ptile <- function(x) {
	x <- sort(x)
	data.frame(value = x, 
	           ptile = ecdf(x)(x))
}
percentile_df <- function(x) {
	x <- sort(x)
	data.frame(value = x, 
	           percentile = ecdf(x)(x))
}


## ----percentile_df--------------------------------------------------------
percentile_df <- function(x) {
	arg <- as.list(match.call())
	x <- sort(x)
	d <- data.frame(value = x, 
	                percentile = ecdf(x)(x))
	
	names(d)[1] <- paste0(as.character(arg$x), collapse = "_")
	d
}


## ----precentile_df-example------------------------------------------------
random_vector <- rnorm(100)
tail(percentile_df(random_vector))
head(percentile_df(rnorm(50)))

