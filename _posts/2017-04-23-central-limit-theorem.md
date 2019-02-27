Central Limit Theorem
-----------------------------------

Lorem ipsum dolor sit amet, no minimum complectitur vim, an enim mandamus complectitur mea. Enim noluisse appareat in est, harum graece at nec. Cu est commune pertinacia omittantur. Viris argumentum reprimique at vel, mazim putant accusata cu mel. Propriae sensibus abhorreant eu has, per id partem veritus civibus, solet phaedrum periculis nam no. Sit et idque inani populo. Quod aeque sadipscing cu cum, pro ex malorum alienum suscipit.


``` r
rm(list=ls())

library(rsenal)
library(tidyverse)
library(magick)

set.seed(2323)
```

Functions for later use
-----------------------

Lorem ipsum <mark>dolor sit amet, no minimum complectitur vim, an enim mandamus complectitur mea. Enim noluisse appareat in est, harum graece at nec. Cu est commune pertinacia omittantur.</mark> Viris argumentum reprimique at vel, mazim putant accusata cu mel. Propriae sensibus abhorreant eu has, per id partem veritus civibus, solet phaedrum periculis nam no. Sit et idque inani populo. Quod aeque sadipscing cu cum, pro ex malorum alienum suscipit.

``` r
get_sample_fun <- function(x, .f, size){
  if (size > length(x)) 
    stop("size must be smaller than length of x")
  sample_x <- sample(x, size = size, replace=FALSE)
  .f(sample_x)
}


rep_get_sample_fun <- function(x, .f, n_reps, size){
  stat <- map_dbl(1:n_reps, ~get_sample_fun(x, .f, size))
  stat <- list(stat)
  names(stat) <- n_reps %>% as.character()
  stat
}
```

Getting the data
----------------

Lorem ipsum dolor sit amet, no minimum complectitur vim, an enim mandamus complectitur mea. Enim noluisse appareat in est, harum graece at nec. Cu est commune pertinacia omittantur. Viris argumentum reprimique at vel, mazim putant accusata cu mel. Propriae sensibus abhorreant eu has, per id partem veritus civibus, solet phaedrum periculis nam no. Sit et idque inani populo. Quod aeque sadipscing cu cum, pro ex malorum alienum suscipit.

``` r
x <- faithful$eruptions

sample_size <- c(seq(0, 200, 20), seq(0, 10000, 2000), 50000) %>% unique()
sample_size <- sample_size[sample_size != 0]

df <- map_df(sample_size, ~rep_get_sample_fun(x, mean, n_reps=., size=100)) %>%
  gather(value = mean) %>%
  na.omit() %>% 
  mutate(reps = key %>% as.integer()) %>% 
  dplyr::select(-key)

df_splitted <- df %>%
  split(.$reps)
```

Plotting
--------

Lorem ipsum dolor sit amet, no minimum complectitur vim, an enim mandamus complectitur mea. Enim noluisse appareat in est, harum graece at nec. Cu est commune pertinacia omittantur. Viris argumentum reprimique at vel, mazim putant accusata cu mel. Propriae sensibus abhorreant eu has, per id partem veritus civibus, solet phaedrum periculis nam no. Sit et idque inani populo. Quod aeque sadipscing cu cum, pro ex malorum alienum suscipit.

<script>
$(function(){
  var image = new Image();
  image.src="{{{{{{ "/assets/plots/plot-clt-1.gif" | absolute_url }}" | absolute_url }}" | absolute_url }}";
   $("img").click(function(){
     $(this).attr("src"",image.src);
   }); 
 });
</script>

``` r
img <- image_graph(width = 800, height = 600, res = 120)

out <- map(df_splitted, function(data){
  p <- data %>%
    ggplot(aes(x=mean, y=..count../sum(..count..))) +
    geom_histogram(color="#FFFFFF", fill = "#3A506B") +
    # geom_text(aes(x=3.7, y=0.095, label=paste("Sigma = ", stdw %>% round(4) %>% format(nsmall = 4)))) +
    # geom_text(aes(x=3.7, y=0.090, label=paste("Mean = ", m %>% round(4) %>% format(nsmall = 4)))) +
    theme_blog() +
    labs(title = "CLT Simulation",
         subtitle = paste("Sample Size =", data$reps)) +
    scale_y_continuous(breaks = seq(0, 1, 0.05),
                       labels = seq(0, 1, 0.05) %>% scales::percent(),
                       limits = c(0, 0.10)) +
    scale_x_continuous(breaks = seq(3, 4, 0.2),
                       limits = c(3.2, 3.8)) +
    ylab("Density") +
    xlab("Sample Mean")
  print(p)
})
dev.off()
```

    ## png 
    ##   2

``` r
animation <- image_animate(img, fps = 1, loop = 1)
print(animation)
```




![]({{ "/assets/plots/plot-clt-1.gif" | absolute_url }})

