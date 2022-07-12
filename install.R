install.packages('rstan', type = 'source')

install.packages("cmdstanr", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))
cmdstanr::install_cmdstan(dir = "/home/rstudio/")
install.packages(c("coda","mvtnorm","devtools","loo","dagitty","shape"))
packages <- c("ape","Rglpk", "bayesplot", "brms", "broom", "flextable", "GGally", "ggdag", "ggdark", "ggmcmc", "ggrepel", "ggthemes", "ggtree", "ghibli", "gtools", "patchwork", "psych", "rcartocolor", "Rcpp", "remotes", "rstan", "StanHeaders", "statebins", "tidybayes", "tidyverse", "viridis", "viridisLite", "wesanderson")
install.packages(packages, dependencies = T)
devtools::install_github("rmcelreath/rethinking")

devtools::install_github("EdwinTh/dutchmasters")
devtools::install_github("gadenbuie/ggpomological")
devtools::install_github("UrbanInstitute/urbnmapr")
remotes::install_github("stan-dev/posterior")
