## ----setup, include = FALSE-----------------------------------------------
library(tidyverse)
theme_set(theme_minimal(20))
update_geom_defaults("point", list(size = 3))
knitr::opts_chunk$set(fig.width = 13, 
                      message = FALSE, 
                      warning = FALSE)


## ----clean-up1------------------------------------------------------------
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



## ----recyclable-----------------------------------------------------------
recyclable <- function(x, y) {
	test1 <- length(x) %% length(y)
	test2 <- length(y) %% length(x)

	any(c(test1, test2) == 0)
}


## ----recyclable-test------------------------------------------------------
a <- c(1, NA, NA, 3, 3, 9, NA)
b <- c(NA, 3, NA, 4, NA, NA, NA)

recyclable(a, b)
recyclable(a, c(b, b))
recyclable(a, c(b, b, b))
recyclable(c(a, a), c(b, b, b))


## ----both_na-rev1---------------------------------------------------------
both_na <- function(x, y) {

	if(!recyclable(x, y)) {
		stop("Vectors are of different lengths and are not recyclable: ",
		     "(x = ", length(x),
		     ", y = ", length(y), ")")	
	}

	if(length(x) == length(y)) {
		return(sum(is.na(x) & is.na(y)))
	}
	
	if(recyclable(x, y)) {
		warning("Vectors were recycled (", 
		        "x = ", length(x), 
		        ", y = ", length(y), ")")
		return(sum(is.na(x) & is.na(y)))
	}
}


## ----both_na-rev1-test, error = TRUE, warning = TRUE----------------------
both_na(a, b)
both_na(a, c(b, b))
both_na(c(a, b), c(b, b, b))
both_na(c(a, a), b)


## ----check-lengths--------------------------------------------------------
check_lengths <- function(x, y) {
	if(!length(x) == length(y)) {
		if(recyclable(x, y)) {
			warning("Vectors were recycled (", 
			        "x = ", length(x), 
			        ", y = ", length(y), ")")
		}
		else {
			stop("Vectors are of different lengths and are not recyclable: ",
		     "(x = ", length(x),
		     ", y = ", length(y), ")")
		}
	}
}


## ----rev2-----------------------------------------------------------------
both_na <- function(x, y) {
	check_lengths(x, y)
	sum(is.na(x) & is.na(y))
}


## ----both_na-test2, error = TRUE, warning = TRUE--------------------------
both_na(a, b)
both_na(a, c(b, b))
both_na(c(a, b), c(b, b, b))
both_na(c(a, a), b)


## ----nested-funs----------------------------------------------------------
f <- function(a) g(a)
g <- function(b) h(b)
h <- function(c) i(c)
i <- function(d) {
  if (!is.numeric(d)) {
    stop("`d` must be numeric", call. = FALSE)
  }
  d + 10
}


## ----err-traceback, eval = FALSE------------------------------------------
## f("a")
## traceback()


## ----nse-percentile_df----------------------------------------------------
percentile_df <- function(x) {
	sorted <- sort(x)
	d <- data.frame(sorted, percentile = ecdf(sorted)(sorted))
	names(d)[1] <- paste0(substitute(x), collapse = "_")
	d
}
percentile_df(rnorm(100, 5, 0.2)) %>%
	head()


## -------------------------------------------------------------------------
quote(subset(df, select = var))
substitute(subset(df, select = var))
extract_var <- function(df, var) {
	substitute(df)
}
extract_var(mtcars)


## ----substitute-success1--------------------------------------------------
extract_var <- function(df, var) {
	subset(eval(substitute(df)), 
	       select = var)
}
extract_var(mtcars, "mpg")


## ----substitute-success2--------------------------------------------------
extract_var <- function(df, var) {
	eval(substitute(var), envir = df)
}
extract_var(mtcars, mpg)


## ----substitute-success3--------------------------------------------------
extract_var <- function(df, var) {
	df[ ,as.character(substitute(var))]
}
extract_var(mtcars, mpg)


## ----extract_vars---------------------------------------------------------
extract_vars <- function(df, ...) {
  vars <- substitute(alist(...))
	df[ ,as.character(vars)[-1]]
}
head(
  extract_vars(mtcars, mpg, cyl, disp)
)


## ----add_var--------------------------------------------------------------
select(mpg, 
       manufacturer, model, hwy)


## ----group-means-practice-------------------------------------------------
mtcars %>%
	group_by(cyl, gear) %>%
	summarize(mean = mean(mpg, na.rm = TRUE)) %>%
	pivot_wider(names_from = cyl, values_from = mean)


## -------------------------------------------------------------------------
group_means <- function(data, outcome, group_1, group_2) {
	data %>%
		group_by(group_1, group_2) %>%
		summarize(mean = mean(outcome, na.rm = TRUE)) %>%
		pivot_wider(names_from = group_1, values_from = mean)
}


## ----group_means-fail, error = TRUE---------------------------------------
group_means(mtcars, mpg, cyl, gear)
group_means(diamonds, price, cut, clarity)


## ----rlang----------------------------------------------------------------
group_means <- function(data, outcome, group_1, group_2) {
	out <- enquo(outcome) # Quote the inputs
	g1 <- enquo(group_1)
	g2 <- enquo(group_2)

	data %>%
		group_by(!!g1, !!g2) %>% # !! to evaluate (bang-bang)
		summarize(mean = mean(!!out, na.rm = TRUE)) %>%
		pivot_wider(names_from = !!g1, values_from = mean)
}


## ----group_means-success--------------------------------------------------
group_means(mtcars, mpg, cyl, gear)
group_means(diamonds, price, cut, clarity)


## ----pass-dots------------------------------------------------------------
group_means2 <- function(data, outcome, ...) {
	out <- enquo(outcome) # Still have to quote the outcome

	data %>%
		group_by(...) %>% 
		summarize(mean = mean(!!out, na.rm = TRUE)) 
}

group_means2(mtcars, mpg, cyl, gear)
group_means2(diamonds, price, cut, clarity)


## ----pass-dots2-----------------------------------------------------------
group_means2(diamonds, price, cut, clarity, color)


## ----double-curly-syntax, include = FALSE---------------------------------
group_means3 <- function(data, outcome, group_1, group_2) {
	  data %>%
		    group_by({{group_1}}, {{group_2}}) %>% 
		    summarize(mean = mean({{outcome}}, na.rm = TRUE)) %>%
		    pivot_wider(names_from = {{group_1}}, values_from = mean)
}

group_means3(mtcars, mpg, cyl, gear)


## ----decorate-double-curly-syntax, echo = FALSE---------------------------
library(flair)
decorate("double-curly-syntax") %>% 
  flair("{{", color = "#4f8dde") %>% 
  flair("}}", color = "#4f8dde") %>% 
  flair("function", color = "#B854D4") %>% 
  flair("TRUE", color = "#B65610")


## ----pipe-centric-example-------------------------------------------------
diamonds %>% 
  filter(color == "E") %>% 
  select(carat, cut, clarity) %>% 
  group_means3(carat, cut, clarity)


## ----summarize_cols-fun, include = FALSE----------------------------------
summarize_cols <- function(data, ...) {
	data %>%
		select(...) %>%
		pivot_longer(everything(), names_to = "var", values_to = "val") %>%
		group_by(var) %>%
		summarize(mean = mean(val, na.rm = TRUE),
	          	sd = sd(val, na.rm = TRUE),
	          	min = min(val, na.rm = TRUE),
	          	max = max(val, na.rm = TRUE))
}


## ----summarize_cols-example1----------------------------------------------
summarize_cols(diamonds, depth, table, price)


## ----summarize_cols-fun-echo----------------------------------------------
summarize_cols <- function(data, ...) {
	data %>%
		select(...) %>%
		pivot_longer(everything(), names_to = "var", values_to = "val") %>%
		group_by(var) %>%
		summarize(mean = mean(val, na.rm = TRUE),
	          	sd = sd(val, na.rm = TRUE),
	          	min = min(val, na.rm = TRUE),
	          	max = max(val, na.rm = TRUE))
}


## ----smry-cols------------------------------------------------------------
iris %>% 
  select_if(is.numeric) %>% 
  summarize_cols(everything())


## ----scatter-fun-fail, error = TRUE---------------------------------------
check_linear <- function(data, x, y, se = TRUE) {
	p <- ggplot(data, aes(x, y)) +
           geom_point(color = "gray80")
  if(se) {
  	p <- p +
             geom_smooth(method = "lm") +
             geom_smooth() 
  }
  else {
  	p <- p +
             geom_smooth(method = "lm", se = FALSE) +
             geom_smooth(se = FALSE)	
  }
  p
}
check_linear(mtcars, disp, mpg)


## ----scatter-fun-success1, fig.height = 4---------------------------------
check_linear <- function(data, x, y, ...) {
	ggplot(data, aes_string(x, y)) +
	  geom_point(color = "gray80") +
		geom_smooth(method = "lm",
		            color = "magenta", 
			          ...) +
		geom_smooth(...)
}


## ----scatter-fun1---------------------------------------------------------
check_linear(mtcars, "disp", "mpg")


## ----scatter-dots---------------------------------------------------------
check_linear(mtcars, "disp", "mpg", se = FALSE)


## ----scatter-fun-success3-------------------------------------------------
check_linear <- function(data, x, y, ...) {
	ggplot(data, aes({{x}}, {{y}})) +
	  geom_point(color = "gray80") +
		geom_smooth(method = "lm",
		            color = "magenta", 
			          ...) +
		geom_smooth(...)
}


## ----scatter-tidyeval-----------------------------------------------------
check_linear(mtcars, disp, mpg)


## ----ggplot-extend, fig.height = 5----------------------------------------
check_linear(mtcars, disp, mpg, se = FALSE) +
	labs(title = "Checking linearity",
	     subtitle = "Linear vs LOESS fits",
	     x = "Engine Displacement",
	     y = "Miles Per gallon") +
	theme_dark(20)


## ----label, options-------------------------------------------------------
# SE function
se <- function(x) {
  x <- x[!is.na(x)]
  sd(x)/sqrt(length(x))
}


## ----mtcars-mean-se-ex----------------------------------------------------
iris %>% 
  select_if(is.numeric) %>% 
  pivot_longer(everything(),
               names_to = "var",
               values_to = "val") %>% 
  group_by(var) %>% 
  summarize(mean = mean(val, na.rm = TRUE),
            se = se(val))


## ----mean-se-fun----------------------------------------------------------
estimate_means <- function(df) {
  df %>% #<<
    select_if(is.numeric) %>% 
    pivot_longer(everything(),
                 names_to = "var",
                 values_to = "val") %>% 
    group_by(var) %>% 
    summarize(mean = mean(val, na.rm = TRUE),
              se = se(val)) 
}
estimate_means(diamonds)


## ----plot-means-----------------------------------------------------------
plot_means <- function(df) {
  means <- estimate_means(df) %>% 
    mutate(var = reorder(factor(var), mean))
  
  ggplot(means, aes(var, mean)) +
    geom_errorbar(aes(ymin = mean + qnorm(0.025)*se,
                      ymax = mean + qnorm(0.975)*se),
                  width = 0.4,
                  color = "gray40") +
    geom_point() +
    coord_flip()
}


## ----plot-means-ex1-------------------------------------------------------
plot_means(iris)


## ----plot-means-ex2-------------------------------------------------------
plot_means(diamonds)

