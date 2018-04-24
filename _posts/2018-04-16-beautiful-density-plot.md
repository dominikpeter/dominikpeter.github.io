---
layout: post
title: Beautiful Density Plot
feature-img: "assets/plots/eruptions-density-1.png"
thumbnail: "assets/plots/eruptions-density-1.pngg"
tags: [density, Lorem]
---

Bootstrapping Density Function and plotting with ggplot
-------------------------------------------------------

Lorem ipsum dolor sit amet, no minimum complectitur vim, an enim mandamus complectitur mea. Enim noluisse appareat in est, harum graece at nec. Cu est commune pertinacia omittantur. Viris argumentum reprimique at vel, mazim putant accusata cu mel. Propriae sensibus abhorreant eu has, per id partem veritus civibus, solet phaedrum periculis nam no. Sit et idque inani populo. Quod aeque sadipscing cu cum, pro ex malorum alienum suscipit.

``` r
rm(list = ls())

library(tidyverse)
library(rsenal) #set of useful functions
library(viridis)
library(broom)

set.seed(2323)
```

Functions for later use
-----------------------

``` r
faithful <- faithful %>% as_tibble()

eruptions <- faithful$eruptions

faithful %>% head()
```

<script data-pagedtable-source type="application/json">
{"columns":[{"label":["eruptions"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["waiting"],"name":[2],"type":["dbl"],"align":["right"]}],"data":[{"1":"3.600","2":"79"},{"1":"1.800","2":"54"},{"1":"3.333","2":"74"},{"1":"2.283","2":"62"},{"1":"4.533","2":"85"},{"1":"2.883","2":"55"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>

Plotting the Distribution
-------------------------

``` r
faithful %>% 
  ggplot(aes(x = eruptions)) +
  geom_histogram(fill = "#3A506B", color="#e3e3e3") +
  theme_blog() +
  labs(title = "Histogram Plot",
       subtitle  = "")  +
  scale_x_continuous(breaks=seq(1,6,by=1)) +
  ylab("Frequency\n") +
  xlab("\nEruptions")
```

![]({{ "/assets/plots/distribution-eruptions-1.png" | absolute_url }})

Set up some functions
---------------------

``` r
get_bs_sample <- function(x){
  x[sample(length(x), replace = TRUE)]
}

density_from_bs <- function(x, min, max){
  x_sample <- get_bs_sample(x)
  density(x_sample, from = min, to = max)
}
```

Bootstrapping
-------------

Lorem ipsum dolor sit amet, no minimum complectitur vim, an enim mandamus complectitur mea. Enim noluisse appareat in est, harum graece at nec. Cu est commune pertinacia omittantur. Viris argumentum reprimique at vel, mazim putant accusata cu mel. Propriae sensibus abhorreant eu has, per id partem veritus civibus, solet phaedrum periculis nam no. Sit et idque inani populo. Quod aeque sadipscing cu cum, pro ex malorum alienum suscipit.

``` r
#init
fit1 <- density(eruptions)

min_ <- min(fit1$x)
max_ <- max(fit1$x)

bs_samples <-  map(1:10000,
                   ~density_from_bs(eruptions,
                                    min_, max_)) %>%
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
  geom_point(data = bs_samples_samples,
             aes(x=x, y=y, color=dist), alpha=0.6
             # color="#173e43"
             ) +
  geom_line(data=fit1 %>% tidy, aes(x=x, y=y),
            color="#F0EFF4", size=1) +
  theme_blog() +
  labs(title = "Fitted Density Curve of Eruptions",
       subtitle  = "CI generated with bootstrap resampling R = 10000")  +
  scale_y_continuous(breaks = seq(0, 0.5, by=0.2),
                     labels = seq(0, 0.5, by=0.2) %>% scales::percent()) +
  scale_x_continuous(breaks = seq(1, 6, by=1)) +
  scale_color_viridis(guide=FALSE, option = "magma", begin = 0.1) +
  ylab("Density\n") +
  xlab("\nEruptions")
```

![]({{ "/assets/plots/eruptions-density-1.png" | absolute_url }})
