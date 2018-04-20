
rm(list = ls())

library(Rsenal)
library(readr)
library(stringr)
library(purrr)
library(tools)
library(magrittr)
library(rmarkdown)



clean_file <- function(path){
  file <- read_file(path)
  file_path <- str_extract_all(file, "\\(../assets/plots/\\w.+png\\)", simplify = TRUE)
  for (i in file_path){
    if (!is.na(i)){
      file_name <- basename(str_extract(i, "/\\w.+\\w+"))
      new_file_path <- paste0('{{ "/assets/plots/',file_name,'" | absolute_url }}')
      file <- str_replace(file, i, new_file_path)
      if (!is.na(file)){
        file %>% write_file(path)
      }
    }
  }
}


prep_rmds <- function(file, output, all=FALSE){
  output_folder_files <- dir(output,
                             full.names = FALSE) %>% 
    file_path_sans_ext()
  
  file_sans_ext <- file_path_sans_ext(file) %>%
    basename()
  
  output_file <- paste0(
    basename(file %>% 
               file_path_sans_ext()), ".md")
  
  output <- file.path(output, output_file)
  
  if (!all & file_sans_ext %in% output_folder_files){
    print("File already exists")
  } else {
    render(file,
           output_file = basename(output),
           output_dir = dirname(output))
    
    clean_file(output)
  }
}


input_folder <- "_rmarkdown"
output_folder <- "_posts"

rmds <- dir(input_folder, full.names = TRUE)



walk(rmds, ~prep_rmds(file = .,
                      output = output_folder,
                      all = FALSE))










