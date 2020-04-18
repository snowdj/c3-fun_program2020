## ----setup, include = FALSE----------------------------------------
library(tidyverse)
theme_set(theme_minimal(20))
knitr::opts_chunk$set(fig.width = 13, 
                      message = FALSE, 
                      warning = FALSE)


## ----simulate------------------------------------------------------
set.seed(8675309)
simulate <- function(n, mean = 0, sd = 1) {
	tibble(sample_id = seq_len(n),
	       sample = rnorm(n, mean, sd))
}
simulate(10)


## ----simulate2-----------------------------------------------------
simulate(3, 100, 10)

simulate(5, -10, 1.5)


## ----simulate-map--------------------------------------------------
library(tidyverse)
sims <- map(seq(10, 150, 5), simulate, 100, 10)


## ----simulate-map1-------------------------------------------------
sims[1]


## ----simulate-map2-------------------------------------------------
sims[2]


## ----simulate-map_df-----------------------------------------------
sims_df <- map_df(seq(10, 150, 5), simulate, 100, 10)
sims_df


## ----simulate-no-id------------------------------------------------
sims_df[1:15, ]


## ----add-id-iteration----------------------------------------------
sims_df2 <- map_df(seq(10, 150, 5), simulate, 100, 10, 
                   .id = "iteration")
sims_df2[1:14, ]


## ----setNames------------------------------------------------------
sample_size <- seq(10, 150, 5)
sample_size
sample_size <- setNames(sample_size, 
                        english::english(seq(10, 150, 5))) 
sample_size[1:15]


## ----add-id-sample_size--------------------------------------------
sims_df3 <- map_df(sample_size, simulate, 100, 10, 
                   .id = "n")
sims_df3[1:14, ]


## ----tidy-model----------------------------------------------------
lm(tvhours ~ age, gss_cat) %>%
	broom::tidy()


## ----by_year-------------------------------------------------------
 split(gss_cat, gss_cat$year) %>%
 	map_df(~lm(tvhours ~ age, .x) %>%
 	         broom::tidy()) 


## ----by_year_id----------------------------------------------------
split(gss_cat, gss_cat$year) %>%
 	map_df(~lm(tvhours ~ age, .x) %>%
 	         broom::tidy(),
 	       .id = "year") 


## ----out.width = "400px", echo = FALSE-----------------------------
knitr::include_graphics("img/pfiles_sim.png")


## ----fs1-----------------------------------------------------------
# install.packages("fs")
library(fs)
dir_ls("../data")


## ----fs2-----------------------------------------------------------
dir_ls("../data", glob = "*.csv")


## ----here-fs-------------------------------------------------------
dir_ls(here::here("data"), glob = "*.csv")


## ----batch_load1, message = FALSE----------------------------------
files <- dir_ls(here::here("data"), glob = "*.csv")
batch <- map_df(files, read_csv)
batch


## ----add-id-batch--------------------------------------------------
batch2 <- map_df(files, read_csv, .id = "file")
batch2


## ----count-file----------------------------------------------------
batch2 %>%
	count(file)


## ----remove-here---------------------------------------------------
batch2 <- batch2 %>%
	mutate(file = str_replace_all(file, here::here("data"), ""))

count(batch2, file)


## ----pull-grade----------------------------------------------------
batch2 %>%
	mutate(grade = str_replace_all(file, "/g(\\d?\\d).+", "\\1")) %>%
	select(file, grade)


## ----grade-parse_number--------------------------------------------
batch2 %>%
	mutate(grade = parse_number(file)) %>% #<<
	select(file, grade)


## ----parse_year----------------------------------------------------
batch2 %>%
	mutate(grade = str_replace_all(file, "/g(\\d?\\d).+", "\\1"),
	       year = str_replace_all(file, ".+files(\\d\\d)_sim.+", "\\1")) %>% #<<
	select(file, grade, year)


## ----content-------------------------------------------------------
batch2 %>%
	mutate(grade = str_replace_all(file, "/g(\\d?\\d).+", "\\1"),
	       year = str_replace_all(file, ".+files(\\d\\d)_sim.+", "\\1"),
	       content = str_replace_all(file, "/g\\d?\\d(.+)pfiles.+", "\\1")) %>% #<<
	select(file, grade, year, content)


## ----grade-double-check--------------------------------------------
batch2 %>%
	mutate(grade = str_replace_all(file, 
	                               "/g(\\d?\\d).+", 
	                               "\\1"),
	       year = str_replace_all(file, 
	                              ".+files(\\d\\d)_sim.+", 
	                              "\\1"),
	       content = str_replace_all(file, 
	                                 "/g\\d?\\d(.+)pfiles.+", 
	                                 "\\1")) %>%
	select(file, grade, year, content) %>%
	count(grade)


## ----year-double-check---------------------------------------------
batch2 %>%
	mutate(grade = str_replace_all(file, 
	                               "/g(\\d?\\d).+", 
	                               "\\1"),
	       year = str_replace_all(file, 
	                              ".+files(\\d\\d)_sim.+", 
	                              "\\1"),
	       content = str_replace_all(file, 
	                                 "/g\\d?\\d(.+)pfiles.+", 
	                                 "\\1")) %>%
	select(file, grade, year, content) %>%
	count(year)


## ----content-double-check------------------------------------------
batch2 %>%
	mutate(grade = str_replace_all(file, 
	                               "/g(\\d?\\d).+", 
	                               "\\1"),
	       year = str_replace_all(file, 
	                              ".+files(\\d\\d)_sim.+", 
	                              "\\1"),
	       content = str_replace_all(file, 
	                                 "/g\\d?\\d(.+)pfiles.+", 
	                                 "\\1")) %>%
	select(file, grade, year, content) %>%
	count(content)


## ----finalize-batch2-----------------------------------------------
d <- batch2 %>%
	mutate(grade = str_replace_all(file, "/g(\\d?\\d).+", "\\1"),
	       grade = as.integer(grade),
	       year = str_replace_all(file, ".+files(\\d\\d)_sim.+", "\\1"),
	       year = as.integer(grade),
	       content = str_replace_all(file, "/g\\d?\\d(.+)pfiles.+", "\\1")) %>%
	select(-file) %>%
	select(ssid, grade, year, content, testeventid, asmtprmrydsbltycd,
	       asmtscndrydsbltycd, Entry:WMLE)


## ----print-final---------------------------------------------------
d


## ----fig, echo = FALSE, fig.height = 8.5---------------------------
library(ggridges)
ggplot(d, aes(x = Theta, y = factor(grade))) +
	geom_density_ridges(fill = "cornflowerblue", 
	                    alpha = 0.8, 
	                    bandwidth = 0.3) +
	facet_wrap(~content)


## ----summary-stats-------------------------------------------------
d %>%
	group_by(grade, content, asmtprmrydsbltycd) %>%
	summarize(mean = mean(Theta)) %>%
	pivot_wider(names_from = content, 
	            values_from = mean)


## ----math----------------------------------------------------------
dir_ls(here::here("data"), regexp = "Math")


## ----g5------------------------------------------------------------
dir_ls(here::here("data"), regexp = "g5")


## ----list.files1---------------------------------------------------
list.files(here::here("data"))


## ----list.files2---------------------------------------------------
list.files(here::here("data"), full.names = TRUE)


## ----list.files-csvs-----------------------------------------------
list.files(here::here("data"), 
           full.names = TRUE,
           pattern = "*.csv")



## ----list.files-id-fail, error = TRUE------------------------------
files <- list.files(here::here("data"), pattern = "*.csv")
batch3 <- map_df(files, read_csv, .id = "file")


## ----files-not-full-names------------------------------------------
files


## ----list.files-id-success, message = FALSE------------------------
files <- list.files(here::here("data"), 
                    pattern = "*.csv", 
                    full.names = TRUE)
batch3 <- map_df(files, read_csv, .id = "file")
batch3


## ----vector-names--------------------------------------------------
names(files)


## ------------------------------------------------------------------
files <- list.files(here::here("data"), 
                    pattern = "*.csv", 
                    full.names = TRUE) 
files <- setNames(files, files)

batch4 <- map_df(files, read_csv, .id = "file")
batch4


## ----split---------------------------------------------------------
splt_content <- split(d, d$content)
str(splt_content)


## ----fit-models-split----------------------------------------------
m1 <- map(splt_content, ~lm(Theta ~ asmtprmrydsbltycd, 
          									data = .x))
m2 <- map(splt_content, ~lm(Theta ~ asmtprmrydsbltycd + 
                               			asmtscndrydsbltycd, 
														data = .x))
m3 <- map(splt_content, ~lm(Theta ~ asmtprmrydsbltycd*asmtscndrydsbltycd, 
          									data = .x))


## ----list-column---------------------------------------------------
d %>%
	nest(-content)


## ----list-column-model---------------------------------------------
mods <- d %>%
	nest(-content) %>%
	mutate(m1 = map(data, ~lm(Theta ~ asmtprmrydsbltycd, 
          									data = .x)),
         m2 = map(data, ~lm(Theta ~ asmtprmrydsbltycd + asmtscndrydsbltycd, 
          									data = .x)),
         m3 = map(data, ~lm(Theta ~ asmtprmrydsbltycd*asmtscndrydsbltycd, 
          									data = .x)))
mods


## ----gather-models-------------------------------------------------
mods %>%
	pivot_longer(m1:m3, names_to = "model", values_to = "output")


## ----extract-r2----------------------------------------------------
mods %>%
	pivot_longer(m1:m3, names_to = "model", values_to = "output") %>% 
	mutate(r2 = map_dbl(output, ~summary(.x)$r.squared))


## ----model-plot, fig.height = 4.5----------------------------------
mods %>%
	pivot_longer(m1:m3, names_to = "model", values_to = "output") %>% 
	mutate(r2 = map_dbl(output, ~summary(.x)$r.squared)) %>%
ggplot(aes(model, r2)) +
	geom_col(aes(fill = model)) +
	facet_wrap(~content) +
	guides(fill = "none") +
	scale_fill_brewer(palette = "Set2")

