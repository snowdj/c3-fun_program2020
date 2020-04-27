## ----setup, include = FALSE----------------------------------------------------
library(tidyverse)
theme_set(theme_minimal(20))
update_geom_defaults("point", list(size = 3))
knitr::opts_chunk$set(fig.width = 13, 
                      message = FALSE, 
                      warning = FALSE)


## ----expand.grid---------------------------------------------------------------
ints <- 1:3
lets <- c("a", "b", "c")
expand.grid(ints, lets)


## ----sim-conditions------------------------------------------------------------
conditions <- expand.grid(n = seq(5, 150, 5),
                          mu = seq(-2, 2, 0.25))
head(conditions)
tail(conditions)


## ----sim1----------------------------------------------------------------------
sim1 <- map2(conditions$n, conditions$mu, ~rnorm(n = .x, mean = .y, sd = 10))
str(sim1)


## ----list-column-sim1----------------------------------------------------------
sim2 <- conditions %>%
	as_tibble() %>% # Not required, but definitely helpful
	mutate(sim = map2(n, mu, ~rnorm(n = .x, mean = .y, sd = 10))) 
sim2


## ----list-column-sim2----------------------------------------------------------
conditions %>%
	as_tibble() %>% # Not required, but definitely helpful
	mutate(sim = map2(n, mu, ~rnorm(.x, .y, sd = 10))) %>%
	unnest()


## ----pulitzer-data-------------------------------------------------------------
library(fivethirtyeight)
pulitzer


## ----pulitzer-prep-------------------------------------------------------------
pulitzer <- pulitzer %>%
  select(newspaper, starts_with("num")) %>%
  pivot_longer(-newspaper,
               names_to = "year_range", 
               values_to = "n",
               names_prefix = "num_finals") %>% 
  mutate(year_range = str_replace_all(year_range, "_", "-")) %>%
  filter(year_range != "1990-2014")

head(pulitzer)


## ----one-plot-pulitzer, fig.height = 3.25--------------------------------------
pulitzer %>%
	filter(newspaper == "Wall Street Journal") %>%
ggplot(aes(year_range, n)) +
	geom_col(aes(fill = n)) +
	scale_fill_distiller(type = "seq", 
	                     limits = c(0, max(pulitzer$n)),
	                     palette = "BuPu",
	                     direction = 1) +
	ylim(0, max(pulitzer$n)) +
	coord_flip() +
	labs(title = "Pulitzer Prize winners: Wall Street Journal",
	     x = "Year Range") 


## ----pulitzer-nested-----------------------------------------------------------
pulitzer %>%
	group_by(newspaper) %>%
	nest()


## ----pulitzer-plots1, eval = FALSE---------------------------------------------
## pulitzer %>%
## 	group_by(newspaper) %>%
## 	nest() %>%
## 	mutate(plot = map(data, ~
##       ggplot(.x, aes(year_range, n)) +
##         geom_col(aes(fill = n)) +
##         scale_fill_distiller(type = "seq",
##                              limits = c(0, max(pulitzer$n)),
##                              palette = "BuPu",
##                              direction = 1) +
##         ylim(0, max(pulitzer$n)) +
##         coord_flip() +
##         labs(title = "Pulitzer Prize winners")
##     )
##   )


## ----pulitzer-plots2-----------------------------------------------------------
library(glue)

p <- pulitzer %>%
	group_by(newspaper) %>%
	nest() %>%
    mutate(plot = map2(data, newspaper, ~ #<<
    ggplot(.x, aes(year_range, n)) +
      geom_col(aes(fill = n)) +
      scale_fill_distiller(type = "seq", 
                           limits = c(0, max(pulitzer$n)),
                           palette = "BuPu",
                           direction = 1) +
      ylim(0, max(pulitzer$n)) +
      coord_flip() +
      labs(title = glue("Pulitzer Prize winners: {.y}")) #<<
    )
  )


## ----show-df-------------------------------------------------------------------
p


## ----p1, fig.height = 5.5------------------------------------------------------
p$plot[[1]]
p$plot[[3]]


## ----p2, fig.height = 5.5------------------------------------------------------
p$plot[[2]]
p$plot[[4]]


## ----full-conditions-----------------------------------------------------------
full_conditions <- expand.grid(n = seq(5, 150, 5),
                               mu = seq(-2, 2, 0.25),
                               sd = seq(1, 3, .1))
head(full_conditions)
tail(full_conditions)


## ----full-sim1-----------------------------------------------------------------
fsim <- pmap(list(number = full_conditions$n,
                  average = full_conditions$mu,
                  stdev = full_conditions$sd), 
             function(number, average, stdev) {
             	rnorm(n = number, mean = average, sd = stdev)
             })
str(fsim)


## ----full-sim2-----------------------------------------------------------------
fsim <- pmap(list(full_conditions$n,
                  full_conditions$mu,
                  full_conditions$sd),
            ~rnorm(n = ..1, mean = ..2, sd = ..3))
str(fsim)


## ----full-sim3-----------------------------------------------------------------
fsim <- pmap(full_conditions, ~rnorm(n = ..1, 
                                     mean = ..2, 
                                     sd = ..3))
str(fsim)


## ----fsim-df1------------------------------------------------------------------
full_conditions %>%
	as_tibble() %>%
	mutate(sim = pmap(list(n, mu, sd), ~rnorm(..1, ..2, ..3)))


## ----fsim-df2------------------------------------------------------------------
full_conditions %>%
	as_tibble() %>%
	mutate(sim = pmap(list(n, mu, sd), ~rnorm(..1, ..2, ..3))) %>%
	unnest()


## ----add-col-------------------------------------------------------------------
pulitzer <- pulitzer %>%
	group_by(newspaper) %>%
	mutate(tot = sum(n))
pulitzer


## ----add-lab-col---------------------------------------------------------------
#install.packages("english")
library(english)
pulitzer <- pulitzer %>%
	mutate(label = 
    glue("{str_to_title(as.english(tot))} Total Pulitzer Awards"))
pulitzer


## ----plot-cap1, fig.height = 2.25----------------------------------------------
tmp <- pulitzer %>%
	filter(newspaper == "Wall Street Journal") 

ggplot(tmp, aes(year_range, n)) +
	geom_col(aes(fill = n)) +
	scale_fill_distiller(type = "seq", 
	                     limits = c(0, max(pulitzer$n)),
	                     palette = "BuPu",
	                     direction = 1) +
	ylim(0, max(pulitzer$n)) +
	coord_flip() +
	labs(title = "Pulitzer prize winners: Wall Street Journal",
	     x = "Year Range",
	     caption = unique(tmp$label)) 


## ----nested--------------------------------------------------------------------
pulitzer %>%
	group_by(newspaper, label) %>%
	nest() 


## ----plot-cap2-----------------------------------------------------------------
final_plots <- pulitzer %>%
	group_by(newspaper, label) %>%
	nest() %>%
	mutate(plots = pmap(list(newspaper, label, data),
    ~ggplot(..3, aes(year_range, n)) + #<<
    	geom_col(aes(fill = n)) +
    	scale_fill_distiller(type = "seq", 
    	                     limits = c(0, max(pulitzer$n)),
    	                     palette = "BuPu",
    	                     direction = 1) +
    	ylim(0, max(pulitzer$n)) +
    	coord_flip() +
    	labs(title = glue("Pulitzer prize winners: {..1}"), #<<
    	     x = "Year Range",
    	     caption = ..2))) #<<


## ----p3, fig.height = 5.5------------------------------------------------------
final_plots$plots[[1]]
final_plots$plots[[3]]


## ----p4, fig.height = 5.5------------------------------------------------------
final_plots$plots[[2]]
final_plots$plots[[4]]


## ----create_dir----------------------------------------------------------------
fs::dir_create(here::here("plots", "pulitzers"))


## ----create_file-paths---------------------------------------------------------
files <- str_replace_all(tolower(final_plots$newspaper), " ", "-")
paths <- here::here("plots", "pulitzers", glue("{files}.png"))
paths


## ----walk2, eval = FALSE-------------------------------------------------------
## walk2(paths, final_plots$plots, ggsave,
##       width = 9.5,
##       height = 6.5,
##       dpi = 500)


## ----map-mtcars----------------------------------------------------------------
map(mtcars, ~as.numeric(scale(.x)))


## ----modify-mtcars-------------------------------------------------------------
modify(mtcars, ~as.numeric(scale(.x)))


## ----map-paste-----------------------------------------------------------------
modify2(LETTERS[1:3], letters[1:3], paste0)
map2(LETTERS[1:3], letters[1:3], paste0)


## ----by_cyl--------------------------------------------------------------------
by_cyl <- mpg %>% 
  group_by(cyl) %>% 
  nest() 
by_cyl


## ----lm-fail, error = TRUE-----------------------------------------------------
by_cyl %>% 
  mutate(mod = map(data, ~lm(hwy ~ displ + drv, data = .x)))


## ----safe-lm-------------------------------------------------------------------
safe_lm <- safely(lm)


## ----loop-safe-lm--------------------------------------------------------------
safe_models <- by_cyl %>% 
  mutate(safe_mod = map(data, ~safe_lm(hwy ~ displ + drv, data = .x)))
safe_models


## ----safe-return---------------------------------------------------------------
safe_models$safe_mod[[1]]
safe_models$safe_mod[[4]]


## ----results-------------------------------------------------------------------
safe_models %>% 
  mutate(results = map(safe_mod, ~.x[["result"]]))


## ----pluck2, options-----------------------------------------------------------
safe_models %>% 
  mutate(results = map(safe_mod, ~pluck(.x, "result")))

