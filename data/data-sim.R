library(synthpop)
library(tidyverse)
library(fs)

l <- map(dir_ls(here::here("data", "pfiles18")), read_csv)
l <- map(l, ~mutate(., 
                    asmtprmrydsbltycd = as.factor(asmtprmrydsbltycd),
                    asmtscndrydsbltycd = as.factor(asmtscndrydsbltycd)))
  
sim <- map(l, syn)
sim <- map(sim, "syn")
sim <- map(sim, ~.x[!names(.x) == "enrlgrattst"])
files <- gsub("/pfiles18", "", names(l))
files <- gsub("\\.csv", "_sim\\.csv", files)
walk2(sim, files, write_csv)

