Bootstrapping Density Function and plotting with ggplot
-------------------------------------------------------

Lorem ipsum dolor sit amet, no minimum complectitur vim, an enim mandamus complectitur mea. Enim noluisse appareat in est, harum graece at nec. Cu est commune pertinacia omittantur. Viris argumentum reprimique at vel, mazim putant accusata cu mel. Propriae sensibus abhorreant eu has, per id partem veritus civibus, solet phaedrum periculis nam no. Sit et idque inani populo. Quod aeque sadipscing cu cum, pro ex malorum alienum suscipit.

``` r
rm(list = ls())

library(ggplot2)
library(dplyr)
library(magrittr)
library(broom)
library(purrr)
```

Functions for later use
-----------------------

Lorem ipsum dolor sit amet, no minimum complectitur vim, an enim mandamus complectitur mea. Enim noluisse appareat in est, harum graece at nec. Cu est commune pertinacia omittantur. Viris argumentum reprimique at vel, mazim putant accusata cu mel. Propriae sensibus abhorreant eu has, per id partem veritus civibus, solet phaedrum periculis nam no. Sit et idque inani populo. Quod aeque sadipscing cu cum, pro ex malorum alienum suscipit.

``` r
theme_c <- function(...) {
  theme_minimal() +
    theme(
      text = element_text(family = "Source Sans Pro", color = "#173e43"),
      # panel.grid.minor = element_line(color = "#ebebe5", size = 0.2),
      panel.grid.major = element_line(color = "#F5F5F5", size = 0.5),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "#f6f1ed", color = NA), 
      panel.background = element_rect(fill = "#f6f1ed", color = NA), 
      legend.background = element_rect(fill = "#f6f1ed", color = NA),
      panel.border = element_blank(),
      legend.position = "bottom",
      ...
    )
}
```

Getting the data
----------------

Lorem ipsum dolor sit amet, no minimum complectitur vim, an enim mandamus complectitur mea. Enim noluisse appareat in est, harum graece at nec. Cu est commune pertinacia omittantur. Viris argumentum reprimique at vel, mazim putant accusata cu mel. Propriae sensibus abhorreant eu has, per id partem veritus civibus, solet phaedrum periculis nam no. Sit et idque inani populo. Quod aeque sadipscing cu cum, pro ex malorum alienum suscipit.

Lorem ipsum dolor sit amet, no minimum complectitur vim, an enim mandamus complectitur mea. Enim noluisse appareat in est, harum graece at nec. Cu est commune pertinacia omittantur. Viris argumentum reprimique at vel, mazim putant accusata cu mel. Propriae sensibus abhorreant eu has, per id partem veritus civibus, solet phaedrum periculis nam no. Sit et idque inani populo. Quod aeque sadipscing cu cum, pro ex malorum alienum suscipit.

``` r
faithful <- faithful %>% as_tibble()

fit1 <- density(faithful$eruptions)

get_bs <- function(df){
  df[sample(nrow(df), replace = TRUE), ]
}

density_from_bs <- function(df){
  bs <- get_bs(df)
  density(bs$eruptions,
          from=min(fit1$x),
          to=max(fit1$x))
}

r <- map(1:1000, ~density_from_bs(faithful)) %>% 
  map_df(~tidy(.))


r %>% 
  group_by(x) %>% 
  summarise(m = mean(y),
            upper = quantile(y, 0.95),
            lower = quantile(y, 0.05)) %>% 
  ggplot(aes(x=x, y=m)) +
  geom_ribbon(aes(ymin = lower, ymax = upper),
              fill = "grey70", alpha=0.5) +
  geom_line() +
  theme_c() +
  labs(title = "Density Plot")
```

![]({{ "/assets/plots/get-1.png" | absolute_url }})
