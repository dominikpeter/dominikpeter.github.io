

library(readr)
library(stringr)

path <- "_posts/2018-04-08-housing-landscape.md"

file <- read_file(path)

file_path <- str_extract(file, "\\(../assets/plots/\\w.+png\\)")
file_name <- basename(str_extract(file_path, "/\\w.+\\w+"))


new_file_path <- paste0('({{ "/assets/plots/',file_name,'" | absolute_url }})')

file <- str_replace(file, file_path, new_file_path)

file %>% write_file(path)
