## ----setup, include = FALSE-----------------------------------------------------
library(tidyverse)
theme_set(theme_minimal(20))
update_geom_defaults("point", list(size = 3))
knitr::opts_chunk$set(fig.width = 13, 
                      message = FALSE, 
                      warning = FALSE)


## ----import-data----------------------------------------------------------------
library(tidyverse)
library(fs)
files <- dir_ls(here::here("data"), glob = "*.csv")
d <- files %>%
	map_df(read_csv, .id = "file") %>%
	mutate(file = str_replace_all(file, here::here("data"), ""),
	       grade = str_replace_all(file, 
	                               "/g(\\d?\\d).+", "\\1"),
	       grade = as.integer(grade),
	       year = str_replace_all(file, 
	                              ".+files(\\d\\d)_sim.+", 
	                              "\\1"),
	       year = as.integer(year),
	       content = str_replace_all(file, 
	                                 "/g\\d?\\d(.+)pfiles.+", 
	                                 "\\1")) %>%
	select(-file) %>%
	select(ssid, grade, year, content, testeventid, asmtprmrydsbltycd,
	       asmtscndrydsbltycd, Entry:WMLE)


## ----disab-recode---------------------------------------------------------------
d <- d %>%
	mutate(primary = as.factor(asmtprmrydsbltycd),
	       secondary = as.factor(asmtscndrydsbltycd))


## ----split----------------------------------------------------------------------
splt_content <- split(d, d$content)
str(splt_content)


## ----fit-models-split-----------------------------------------------------------
m1 <- map(splt_content, ~lm(Theta ~ asmtprmrydsbltycd, data = .x))

m2 <- map(splt_content, ~lm(Theta ~ asmtprmrydsbltycd + 
                                    asmtscndrydsbltycd, 
                            data = .x))

m3 <- map(splt_content, ~lm(Theta ~ asmtprmrydsbltycd *
                                    asmtscndrydsbltycd, 
                            data = .x))


## ----list-column----------------------------------------------------------------
by_content <- d %>%
	nest(-content)
by_content


## ----list-column-listed---------------------------------------------------------
str(by_content$data)


## ----map-data1------------------------------------------------------------------
map_dbl(by_content$data, nrow)
map_dbl(by_content$data, ncol)
map_dbl(by_content$data, ~mean(.x$Theta))


## ----n-by-content---------------------------------------------------------------
by_content %>%
	mutate(n = map_dbl(data, nrow))


## ----n-by-content-list----------------------------------------------------------
by_content %>%
	mutate(n = map(data, nrow))


## ----list-column-model----------------------------------------------------------
by_content %>%
	mutate(m1 = map(data, ~lm(Theta ~ primary, data = .x)))


## -------------------------------------------------------------------------------
by_content %>%
	mutate(m1 = map(data, ~lm(Theta ~ primary, data = .x)),
         coefs = map(m1, coef))


## ----coefs----------------------------------------------------------------------
by_content %>%
	mutate(m1 = map(data, ~lm(Theta ~ primary, data = .x)),
	       coefs = map(m1, coef), 
	       no_disab = map_dbl(coefs, 1),
	       tbi = no_disab + map_dbl(coefs, "primary74")) %>%
	select(content, no_disab, tbi)


## ----three-mods-----------------------------------------------------------------
mods <- by_content %>%
	mutate(m1 = map(data, ~lm(Theta ~ primary, data = .x)),
         m2 = map(data, ~lm(Theta ~ primary + secondary, data = .x)),
         m3 = map(data, ~lm(Theta ~ primary * secondary, data = .x)))
mods


## ----ela-mods1------------------------------------------------------------------
mods$m1[[1]]


## ----ela-mods2------------------------------------------------------------------
mods$m2[[1]]


## ----anova----------------------------------------------------------------------
compare <- anova(mods$m1[[1]], mods$m2[[1]])
compare


## ----mod_compare1---------------------------------------------------------------
mods %>%
	mutate(comp12 = map2(m1, m2, anova))


## ----str-comp-------------------------------------------------------------------
str(compare)


## ----extract-p1-----------------------------------------------------------------
compare$`Pr(>F)`
compare[["Pr(>F)"]]


## ----extract-p2-----------------------------------------------------------------
compare$`Pr(>F)`[2]
compare[["Pr(>F)"]][2]


## ----pull-pvals12---------------------------------------------------------------
mods %>%
	mutate(comp12 = map2(m1, m2, anova),
	       p12 = map_dbl(comp12, list("Pr(>F)", 2)))


## ----fun-compare----------------------------------------------------------------
extract_p <- function(anova_ob) {
	anova_ob[["Pr(>F)"]][2]
}


## ----pull-pvals12-fun-----------------------------------------------------------
mods %>%
	mutate(comp12 = map2(m1, m2, anova),
	       p12 = map_dbl(comp12, extract_p))


## ----gather-models--------------------------------------------------------------
mods %>%
	pivot_longer(m1:m3, names_to = "model", values_to = "output")


## ----extract-r2-----------------------------------------------------------------
mods %>%
	gather(model, output, m1:m3) %>%
	mutate(r2 = map_dbl(output, ~summary(.x)$r.squared))


## ----model-plot, fig.height = 4.5-----------------------------------------------
mods %>%
	gather(model, output, m1:m3) %>%
	mutate(r2 = map_dbl(output, ~summary(.x)$r.squared)) %>%
ggplot(aes(model, r2)) +
	geom_col(aes(fill = model)) +
	facet_wrap(~content) +
	guides(fill = "none") +
	scale_fill_brewer(palette = "Set2")


## ----tidy-----------------------------------------------------------------------
mods %>%
	gather(model, output, m1:m3) %>%
	mutate(tidied = map(output, broom::tidy))


## ----tidied---------------------------------------------------------------------
tidied <- mods %>%
	gather(model, output, m1:m3) %>%
	mutate(tidied = map(output, broom::tidy)) %>%
	select(content, model, tidied) %>%
	unnest()
tidied


## ----coef-plot-echo, eval = FALSE-----------------------------------------------
## to_plot <- names(coef(mods$m1[[1]]))
## 
## tidied %>%
## 	filter(term %in% to_plot) %>%
## ggplot(aes(estimate, term, color = model)) +
## 	geom_point() +
## 	scale_color_brewer(palette = "Set2") +
## 	facet_wrap(~content)


## ----coef-plot-eval, echo = FALSE, fig.height = 9-------------------------------
to_plot <- names(coef(mods$m1[[1]]))

tidied %>%
	filter(term %in% to_plot) %>%
ggplot(aes(estimate, term, color = model)) +
	geom_point() +
	scale_color_brewer(palette = "Set2") +
	facet_wrap(~content)



## ----by_grade-------------------------------------------------------------------
by_grade_content <- d %>%
	nest(-content, -grade)
by_grade_content


## ----by_grade_mods--------------------------------------------------------------
mods_grade <- by_grade_content %>%
	mutate(m1 = map(data, ~lm(Theta ~ primary, data = .x)),
         m2 = map(data, ~lm(Theta ~ primary + secondary, data = .x)),
         m3 = map(data, ~lm(Theta ~ primary * secondary, data = .x)))
mods_grade


## ----by_grade_r2----------------------------------------------------------------
mods_grade %>%
	gather(model, output, m1:m3) %>%
	mutate(r2 = map_dbl(output, ~summary(.x)$r.squared))


## ----by_grade_r2_plot-echo, eval = FALSE----------------------------------------
## mods_grade %>%
## 	gather(model, output, m1:m3) %>%
## 	mutate(r2 = map_dbl(output, ~summary(.x)$r.squared)) %>%
## ggplot(aes(model, r2)) +
## 	geom_col(aes(fill = model)) +
## 	facet_grid(grade ~ content) +
## 	guides(fill = "none") +
## 	scale_fill_brewer(palette = "Set2")


## ----by_grade_r2_plot-eval, echo = FALSE, fig.height = 9.5----------------------
mods_grade %>%
	gather(model, output, m1:m3) %>%
	mutate(r2 = map_dbl(output, ~summary(.x)$r.squared)) %>%
ggplot(aes(model, r2)) +
	geom_col(aes(fill = model)) +
	facet_grid(grade ~ content) +
	guides(fill = "none") +
	scale_fill_brewer(palette = "Set2")

