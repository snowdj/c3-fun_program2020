## ----setup, include = FALSE-----------------------------------------------------------------------
library(tidyverse)
theme_set(theme_minimal(25))
update_geom_defaults('density', list(size = 1.2))


## ----letters--------------------------------------------------------------------------------------
a <- letters[1:26]
a


## ----first-for-loop-------------------------------------------------------------------------------
for(i in 1:5){
	print(a[i])
}


## ----toss-----------------------------------------------------------------------------------------
sample(c("Heads", "Tails"), 1)


## ----allocate-vector------------------------------------------------------------------------------
result <- rep(NA, 10)
result


## ----for-flip1------------------------------------------------------------------------------------
for(i in seq_along(result)) {
	result[i] <- sample(c("Heads", "Tails"), 1)
}
result


## ----time-for-loop, cache = TRUE------------------------------------------------------------------
library(tictoc)

set.seed(1)
tic()
not_allocated <- sample(c("Heads", "Tails"), 1)
for(i in seq_len(1e5 - 1)) {
	not_allocated <- c(not_allocated, sample(c("Heads", "Tails"), 1))
  
}
toc()

set.seed(1)
tic()
allocated <- rep(NA, 1e5)
for(i in seq_len(1e5)) {
	allocated[i] <- sample(c("Heads", "Tails"), 1)
}
toc()


## ----identical------------------------------------------------------------------------------------
identical(not_allocated, allocated)


## ----alphabet-for-loop----------------------------------------------------------------------------
alphabet <- rep(NA, length(letters))

for(i in seq_along(alphabet)) {
	alphabet[i] <- paste0(LETTERS[i], letters[i])
}
alphabet


## ----seq------------------------------------------------------------------------------------------
x <- data.frame()
1:length(x)
seq_along(x)


## ----loop-seq, error = TRUE-----------------------------------------------------------------------
for(i in 1:length(x)) {
	print(letters[i])
}

for(i in seq_along(x)) {
	print(letters[i])
}


## ----sd-increments--------------------------------------------------------------------------------
increments <- seq(1, 5, by = 0.2)


## ----allocate-list--------------------------------------------------------------------------------
simulated <- vector("list", length(increments))
str(simulated)


## ----simulated-loop-------------------------------------------------------------------------------
for(i in seq_along(simulated)) {
	simulated[[i]] <- rnorm(100, 0, increments[i]) # note use of `[[`
}
str(simulated)


## ----names-simultated-loop------------------------------------------------------------------------
names(simulated) <- paste0("sd_", increments)
sim_d <- data.frame(simulated)
head(sim_d)


## ----plot-echo, eval = FALSE----------------------------------------------------------------------
## library(tidyverse)
## pd <- sim_d %>%
## 	pivot_longer(everything(),
## 	             names_to = "sd",
## 	             values_to = "sim",
## 	             names_prefix = "sd_",
## 	             names_ptypes = list(sd = factor()))
## 
## ggplot(pd, aes(sim)) +
##  geom_density(aes(color = sd)) +
##  guides(color = "none")


## ----plot-eval, echo = FALSE----------------------------------------------------------------------
library(tidyverse)
pd <- sim_d %>%
	pivot_longer(everything(),
	             names_to = "sd", 
	             values_to = "sim",
	             names_prefix = "sd_",
	             names_ptypes = list(sd = factor())) 

ggplot(pd, aes(sim)) +
 geom_density(aes(color = sd)) +
 guides(color = "none") 


## ----densities------------------------------------------------------------------------------------
densities <- vector("list", length(sim_d))
for(i in seq_along(densities)) {
	densities[[i]] <- density(sim_d[ ,i])
}
str(densities)


## ----first-density, fig.height = 6----------------------------------------------------------------
plot(densities[[1]])


## ----all-densities, fig.height = 5----------------------------------------------------------------
plot(densities[[1]],xlim = c(-20, 20))

for(i in seq(2, length(densities))) {
	lines(x = densities[[i]]$x, 
	      y = densities[[i]]$y)	
}


## ----loop-skip, fig.height = 3--------------------------------------------------------------------
plot(densities[[1]],xlim = c(-20, 20))

for(i in seq_along(densities)) {
	if(i == 1) next
	lines(x = densities[[i]]$x, 
	      y = densities[[i]]$y)	
}


## ----loop-break-----------------------------------------------------------------------------------
set.seed(1)

rand_unif <- vector("double", 10)

for(i in seq_along(rand_unif)) {
	rand_unif[i] <- runif(1, 0, 10)
	if(any(rand_unif > 5)) {
		break
	}
}

rand_unif


## ----for-loop-sim---------------------------------------------------------------------------------
increments <- seq(1, 5, by = 0.2)

simulated <- vector("list", length(increments))

for(i in seq_along(simulated)) {
	simulated[[i]] <- rnorm(10, 0, increments[i]) # note use of `[[`
}

simulated


## ----lapply-sim-----------------------------------------------------------------------------------
sim_l <- lapply(seq(1, 5, by = 0.2), function(stdev) rnorm(10, 0, stdev))
sim_l


## ----lapply-df------------------------------------------------------------------------------------
lapply(iris, is.double)


## ----lapply-means---------------------------------------------------------------------------------
lapply(mtcars, mean)


## ----lapply-conditional---------------------------------------------------------------------------
lapply(iris, function(x) {
	if(is.double(x)) {
		mean(x)
	}
})


## ----airqual--------------------------------------------------------------------------------------
head(airquality)

lapply(airquality, mean, na.rm = TRUE)


## ----sim-pass-arg---------------------------------------------------------------------------------
lapply(seq(1, 5, 0.2), rnorm, n = 10, mean = 0)


## ----split----------------------------------------------------------------------------------------
by_cyl <- split(mtcars, mtcars$cyl)
str(by_cyl)


## ----mean_mpg_by_cyl------------------------------------------------------------------------------
lapply(by_cyl, function(x) mean(x$mpg))


## ----plots_by_cyl-echo, eval = FALSE--------------------------------------------------------------
## lapply(by_cyl, function(x) {
## 	ggplot(x, aes(disp, mpg)) +
## 		geom_point() +
## 		geom_smooth()
## })


## ----plots_by_cyl-eval, echo = FALSE, message = FALSE, warning = FALSE, fig.height = 2------------
lapply(by_cyl, function(x) {
	ggplot(x, aes(disp, mpg)) +
		geom_point() +
		geom_smooth()
})


## ----save-plots-----------------------------------------------------------------------------------
plots <- lapply(by_cyl, function(x) {
	ggplot(x, aes(disp, mpg)) +
		geom_point() +
		geom_smooth()
})


## ----filenames------------------------------------------------------------------------------------
filenames <- here::here("plots", 
                        paste0("cyl", names(by_cyl), ".png"))
filenames


## ----save-plots-for, eval = FALSE-----------------------------------------------------------------
## for(i in seq_along(plots)) {
## 	ggsave(filenames[i], # single bracket
## 	       plots[[i]], # double bracket
## 	       device = "png",
## 	       width = 6.5,
## 	       height = 8)
## }


## ----lapply-for, eval = FALSE---------------------------------------------------------------------
## lapply(seq_along(plots), function(i) {
## 	ggsave(filenames[i],
## 	       plots[[i]],
## 	       device = "png",
## 	       width = 6.5,
## 	       height = 8)
## })


## ----sapply-sim-----------------------------------------------------------------------------------
sim_s <- sapply(seq(1, 5, by = 0.2), function(x) rnorm(10, 0, x))
class(sim_s)
dim(sim_s)

sim_s


## ----sapply-means---------------------------------------------------------------------------------
sapply(iris, is.double)


## ----iris-subset----------------------------------------------------------------------------------
head(iris)

head( iris[ ,sapply(iris, is.double)] )


## ----not-double-----------------------------------------------------------------------------------

head( iris[ ,!sapply(iris, is.double), drop = FALSE] )


## ----vapply1, error = TRUE------------------------------------------------------------------------
vapply(mtcars, mean, FUN.VALUE = double(1))
vapply(iris, is.double, FUN.VALUE = character(1))
vapply(iris, is.double, FUN.VALUE = logical(1))


## ----vapply-coerce--------------------------------------------------------------------------------
vapply(iris, is.double, FUN.VALUE = double(1))


## ----count-missing--------------------------------------------------------------------------------
vapply(airquality, 
       function(col) {
         sum(is.na(col))
         }, 
       double(1)
)


## ----sapply-missing-------------------------------------------------------------------------------
sapply(airquality, function(col) sum(is.na(col)))

