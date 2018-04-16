Simulating the Monty Hall Problem
---------------------------------

Lorem ipsum dolor sit amet, no minimum complectitur vim, an enim mandamus complectitur mea. Enim noluisse appareat in est, harum graece at nec. Cu est commune pertinacia omittantur. Viris argumentum reprimique at vel, mazim putant accusata cu mel. Propriae sensibus abhorreant eu has, per id partem veritus civibus, solet phaedrum periculis nam no. Sit et idque inani populo. Quod aeque sadipscing cu cum, pro ex malorum alienum suscipit.

``` r
rm(list = ls())

library(ggplot2)
library(scales)
library(dplyr)
library(magrittr)
library(broom)
library(purrr)
library(tidyr)
library(pander)
library(viridis)

set.seed(2323)
```

Functions for later use
-----------------------

Lorem ipsum dolor sit amet, no minimum complectitur vim, an enim mandamus complectitur mea. Enim noluisse appareat in est, harum graece at nec. Cu est commune pertinacia omittantur. Viris argumentum reprimique at vel, mazim putant accusata cu mel. Propriae sensibus abhorreant eu has, per id partem veritus civibus, solet phaedrum periculis nam no. Sit et idque inani populo. Quod aeque sadipscing cu cum, pro ex malorum alienum suscipit.

``` r
theme_blog <- function(background=FALSE, ...) {
  theme_ <- theme_minimal()
    if (background){
      theme_ <- theme_ + 
        theme(
          plot.background = element_rect(fill = "#f6f1ed", color = NA), 
          panel.background = element_rect(fill = "#f6f1ed", color = NA),
          legend.background = element_rect(fill = "#f6f1ed", color = NA)
          )
    } 
  theme_ +
    theme(
      text = element_text(family = "Source Sans Pro", color = "#173e43", size=12),
      panel.grid.major = element_line(color = "#e3e3e3", size = .5),
      panel.grid.minor = element_blank(),
      legend.position = "bottom",
      panel.border = element_blank(),
      ...
      )
}
```

Getting the data
----------------

``` r
setup_game <- function(n_doors){
  doors <- c(TRUE, rep(FALSE, n_doors-1))
  doors[sample(length(doors), replace=FALSE)]
}

 

pick_door <- function(doors, switch=FALSE){
  init_guess <- runif(1, min=1, max=length(doors))
  init_guess <- init_guess %>% round()
  result <- doors[init_guess]
  if (switch){
    !result
  }else{
   result
  }
}

 

calc_prob <- function(n_doors, n_reps, switch){
  trials <- map_lgl(1:n_reps,
                   ~pick_door(setup_game(n_doors),
                              switch = switch))
  mean(trials)
}

trials <- function(n_doors_trial, switch)
  map_dbl(n_doors_trial, ~calc_prob(.x, 1000, switch))

n_doors_trial <- 2:100

df <- tibble(n_doors = n_doors_trial,
             switch  = trials(n_doors_trial, TRUE),
             rest    = trials(n_doors_trial, FALSE)) %>%
  gather(switch, prop, -n_doors)

df %>%
  ggplot(aes(x = n_doors, y = prop, color = switch)) +
  geom_line(size=1.1) +
  theme_blog()
```

![]({{ "/assets/plots/monty-plot-1.png" | absolute_url }})
