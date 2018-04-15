

library(readr)
library(stringr)
library(purrr)
library(tools)
library(magrittr)

clean_files <- function(path){
  file <- read_file(path)
  file_path <- str_extract(file, "\\(../assets/plots/\\w.+png\\)")
  if (!is.na(file_path)){
    file_name <- basename(str_extract(file_path, "/\\w.+\\w+"))
    new_file_path <- paste0('{{ "/assets/plots/',file_name,'" | absolute_url }}')
    file <- str_replace(file, file_path, new_file_path)
    file %>% write_file(path)
  }
}


files <- dir("_posts/", full.names = TRUE)
files <- files[tools::file_ext(files) == "md"]

walk(files, clean_files)







