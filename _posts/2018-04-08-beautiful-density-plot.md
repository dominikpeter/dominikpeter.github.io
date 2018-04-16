Bootstrapping Density Function and plotting with ggplot
-------------------------------------------------------

Lorem ipsum dolor sit amet, no minimum complectitur vim, an enim mandamus complectitur mea. Enim noluisse appareat in est, harum graece at nec. Cu est commune pertinacia omittantur. Viris argumentum reprimique at vel, mazim putant accusata cu mel. Propriae sensibus abhorreant eu has, per id partem veritus civibus, solet phaedrum periculis nam no. Sit et idque inani populo. Quod aeque sadipscing cu cum, pro ex malorum alienum suscipit.

``` r
rm(list = ls())

library(ggplot2)
library(scales)
library(dplyr)
library(magrittr)
library(broom)
library(purrr)
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
faithful <- faithful %>%
  as_tibble()

eruptions <- faithful$eruptions

pandoc.table(faithful %>% head(), style = 'rmarkdown')
```

    ## 
    ## 
    ## | eruptions | waiting |
    ## |:---------:|:-------:|
    ## |    3.6    |   79    |
    ## |    1.8    |   54    |
    ## |   3.333   |   74    |
    ## |   2.283   |   62    |
    ## |   4.533   |   85    |
    ## |   2.883   |   55    |

Plotting the Distribution
-------------------------

``` r
faithful %>% 
  ggplot(aes(x = eruptions)) +
  geom_histogram(fill = "#173e43", color="#e3e3e3") +
  theme_blog() +
  labs(title = "Histogram Plot",
       subtitle  = "")  +
  scale_x_continuous(breaks=seq(1,6,by=1)) +
  ylab("Frequency\n") +
  xlab("\nEruptions")
```

![]({{ "/assets/plots/distribution-eruptions-1.png" | absolute_url }})

Bootstrapping
-------------

Lorem ipsum dolor sit amet, no minimum complectitur vim, an enim mandamus complectitur mea. Enim noluisse appareat in est, harum graece at nec. Cu est commune pertinacia omittantur. Viris argumentum reprimique at vel, mazim putant accusata cu mel. Propriae sensibus abhorreant eu has, per id partem veritus civibus, solet phaedrum periculis nam no. Sit et idque inani populo. Quod aeque sadipscing cu cum, pro ex malorum alienum suscipit.

``` r
#init
fit1 <- density(eruptions)

get_bs_sample <- function(x){
  x[sample(length(x), replace = TRUE)]
}

density_from_bs <- function(x, min, max){
  x_sample <- get_bs_sample(x)
  density(x_sample, from = min, to = max)
}

min_ <- min(fit1$x)
max_ <- max(fit1$x)

bs_samples <-  map(1:10000, ~density_from_bs(eruptions, min_, max_)) %>%
  map_df(~tidy(.)) %>% 
  group_by(x) %>% 
  mutate(mean  = mean(y),
         y99   = quantile(y, 0.99),
         y01   = quantile(y, 0.01),
         dist  = abs(y - mean)) %>% 
  ungroup()

bs_samples_samples <- bs_samples %>% 
  sample_n(size = 5000, replace = FALSE)
```

Plotting
--------

``` r
ggplot() +
  # geom_ribbon(data = bs_samples,
  #              aes(x = x,y = y, ymin = y99, ymax = y01),
  #              alpha=0.6, fill="#dddfd4") +
  geom_point(data = bs_samples_samples,
             aes(x=x, y=y, color=dist), alpha=0.6
             # color="#173e43"
             ) +
  geom_line(data=fit1 %>% tidy, aes(x=x, y=y),
            color="#fae596", size=.9) +
  theme_blog() +
  labs(title = "Fitted Density Curve of Eruptions",
       subtitle  = "CI generated with bootstrap resampling R = 10000")  +
  scale_y_continuous(breaks = seq(0,0.5,by=0.1),
                     labels = seq(0,0.5,by=0.1) %>% percent()) +
  scale_x_continuous(breaks = seq(1,6,by=1)) +
  scale_color_viridis(option = "magma", begin = 0.1, guide=FALSE) +
  ylab("Density\n") +
  xlab("\nEruptions")
```

![]({{ "/assets/plots/eruptions-density-1.png" | absolute_url }})
