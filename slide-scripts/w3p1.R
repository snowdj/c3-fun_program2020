## ----setup, include = FALSE----------------------------------------
library(tidyverse)
theme_set(theme_minimal(25))
knitr::opts_chunk$set(fig.width = 13)


## ----four_cyl, message = FALSE, warning = FALSE--------------------
library(tidyverse)
four_cyl <- filter(mpg, cyl == 4)


## ----ninety-ptile--------------------------------------------------
ninety <- four_cyl %>%
	filter(cty >= quantile(cty, probs = 0.9))
ninety


## ----counts--------------------------------------------------------
count_manufacturer <- count(ninety, manufacturer)
count_model <- count(ninety, manufacturer, model)
count_class <- count(ninety, class)


## ----plots1--------------------------------------------------------
plot_manufacturer <- 
	ggplot(count_manufacturer, aes(fct_reorder(manufacturer, -n), n)) +
		geom_col(aes(fill = manufacturer)) +
		scale_fill_brewer(palette = "Set3") +
		labs(title = "Manufacturers",
		     x = "",
		     y = "") +
		guides(fill = "none")

plot_car <- count_model %>%
	unite(car, manufacturer, model, sep = " ") %>%
	ggplot(aes(fct_reorder(car, -n), n)) +
		geom_col(aes(fill = car)) +
		scale_fill_brewer(palette = "Set3") +
		labs(title = "Top 10% of city mpg",
		     subtitle = "Car frequency",
		     x = "",
		     y = "") +
		guides(fill = "none")

plot_class <-
	ggplot(count_class, aes(fct_reorder(class, -n), n)) +
		geom_col(aes(fill = class)) +
		scale_fill_brewer(palette = "Set3") +
		labs(title = "Car Class",
		     x = "", 
		     y = "") +
		guides(fill = "none")


## ----patchwork, fig.width = 13, fig.height = 6.5-------------------
library(patchwork)
plot_car / (plot_manufacturer + plot_class)


## ----filter--------------------------------------------------------
by_cyl <- split(mpg, mpg$cyl)
top_10 <- lapply(by_cyl, function(x) {
	filter(x, cty >= quantile(cty, probs = 0.9))
})
str(top_10)


## ----all-counts----------------------------------------------------
counts <- lapply(top_10, function(x) {
	count_manufacturer <- count(x, manufacturer)
	count_class <- count(x, class)

	count_model <- count(x, manufacturer, model) %>%
		unite(car, manufacturer, model, sep = " ")
	

	return(list(mfr = count_manufacturer, 
	            car = count_model, 
	            class = count_class))
})
counts


## ----plot_fun------------------------------------------------------
counts_plot <- function(counts_df) {
  var <- names(counts_df)[1]
	
  p <- ggplot(counts_df, aes(fct_reorder(!!sym(var), -n), n)) +
		geom_col(aes(fill = !!sym(var))) +
		scale_fill_brewer(palette = "Set3") +
		labs(title = stringr::str_to_title(var),
		     x = "",
		     y = "") +
		guides(fill = "none")

	if(var == "car") {
		p <- p + labs(title = "Top 10% of city mpg",
		              subtitle = var)
	}
 return(p)
}


## ----indiv_plot1, fig.width = 13-----------------------------------
counts_plot(counts[["4"]]$mfr)


## ----indiv_plot2, fig.width = 13-----------------------------------
counts_plot(counts[["8"]]$car)


## ----indiv_plot3, fig.width = 13-----------------------------------
counts_plot(counts[["6"]]$class)	


## ----plot2---------------------------------------------------------
full_plot <- function(l) {
	counts_plot(l[["car"]]) / (
	  counts_plot(l[["mfr"]]) +
	  counts_plot(l[["class"]])
	)
}


## ----full_plot-test, fig.width = 13--------------------------------
full_plot(counts[[1]])


## ----looped-full-plot----------------------------------------------
plots <- lapply(counts, full_plot)


## ----p1------------------------------------------------------------
plots[[1]]


## ----p2------------------------------------------------------------
plots[[2]]


## ----p3, fig.width = 25--------------------------------------------
plots[[3]]


## ----p4------------------------------------------------------------
plots[[4]]


## ----map-----------------------------------------------------------
library(purrr) # loaded automatically by tidyverse
map(1:3, rnorm)


## ----lapply-map----------------------------------------------------
lapply(1:3, rnorm)


## ----map-lapply----------------------------------------------------
map(1:3, rnorm)


## ----equivalents1, eval = FALSE------------------------------------
## map(mtcars, function(x) length(unique(x)))
## 
## lapply(mtcars, function(x) length(unique(x)))


## ----formula1------------------------------------------------------
map(mtcars, ~length(unique(.x)))


## ----list----------------------------------------------------------
l <- list(
  list(-1, x = 1, y = c(2), z = "a"),
  list(-2, x = 4, y = c(5, 6), z = "b"),
  list(-3, x = 8, y = c(9, 10, 11))
)


## ----map-extract---------------------------------------------------
map(l, 2)


## ----lapply-extract-fail, error = TRUE-----------------------------
lapply(l, 2)


## ----lapply-extract-success1---------------------------------------
lapply(l, function(x) x[[2]])


## ----lapply-extract-success2---------------------------------------
lapply(l, `[[`, 2)


## ----by_name-------------------------------------------------------
map(l, "y")


## ----by_name_arg---------------------------------------------------
map(l, list("y", 1))


## ----show-l--------------------------------------------------------
str(l)


## ----map_dbl1------------------------------------------------------
map_dbl(l, "x")
map_dbl(l, 1)


## ----mismatched-type-coercion--------------------------------------
map_chr(l, "x")


## ----no-element,error = TRUE---------------------------------------
map_chr(l, "z")


## ----no-element-default-val, error = TRUE--------------------------
map_chr(l, "z", .default = NA_character_)


## ----log-na--------------------------------------------------------
typeof(NA)


## ----economics-----------------------------------------------------
econ <- economics %>%
	mutate(year = lubridate::year(date))
econ


## ----by_year-------------------------------------------------------
by_year <- split(econ, econ$year)
str(by_year)


## ----perc----------------------------------------------------------
perc <- map(by_year, ~mutate(.x, percent = unemploy / pop))
str(perc)


## ----many-models---------------------------------------------------
mods <- map(perc, ~lm(percent ~ pce, data = .x))
str(mods)


## ----coefs---------------------------------------------------------
coefs <- map(mods, coef)
coefs[c(1:2, length(coefs))]


## ----slopes--------------------------------------------------------
slopes <- map_dbl(coefs, 2)
slopes


## ----plot-slopes, message = FALSE, fig.height = 4------------------
relation <- tibble(year = names(slopes),
                   slope = slopes)

ggplot(relation, aes(slope)) +
	geom_histogram(fill = "cornflowerblue",
	               color = "white")


## ----pipeline1-----------------------------------------------------
by_year %>%
	map(~mutate(.x, percent = unemploy / pop))


## ----pipeline2-----------------------------------------------------
by_year %>%
	map(~mutate(.x, percent = unemploy / pop)) %>%
	map(~lm(percent ~ pce, data = .x))


## ----pipeline3-----------------------------------------------------
by_year %>%
	map(~mutate(.x, percent = unemploy / pop)) %>%
	map(~lm(percent ~ pce, data = .x)) %>%
	map(coef)


## ----pipeline4-----------------------------------------------------
slopes <- by_year %>%
	map(~mutate(.x, percent = unemploy / pop)) %>%
	map(~lm(percent ~ pce, data = .x)) %>%
	map(coef) %>%
	map_dbl(2)
slopes


## ----formulas------------------------------------------------------
 formulas <- list(mpg ~ disp,
                  mpg ~ I(1 / disp),
                  mpg ~ disp + wt,
                  mpg ~ I(1 / disp) + wt) 


## ----bootstrap-----------------------------------------------------
bootstrap <- function(df) {
  df[sample(nrow(df), replace = TRUE), , drop = FALSE]
}

samples <- map(1:50, ~bootstrap(mtcars))

