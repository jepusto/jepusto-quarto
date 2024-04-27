library(tidyverse)

all_classes <- tibble(
  class = list.files("teaching")
) %>%
  mutate(
    class = str_sub(class, 1, -4)
  )


convert_post <- function(class) {
  
  post_text <- readLines(paste("teaching",paste(class, "md", sep = "."), sep = "/"))
  yaml_lines <- which(post_text == "+++")
  yaml_text <- post_text[(yaml_lines[1] + 1):(yaml_lines[2] - 1)]
  title <- 
    yaml_text[str_detect(yaml_text, "^title")] %>%
    str_sub(9,-1) %>%
    str_remove_all("\"")
  
  new_yaml <- list(
    title = title
  ) %>%
    yaml::as.yaml()
  
  post_text_body <- 
    post_text[(yaml_lines[2]+4):length(post_text)] %>%
    str_replace_all("files/syllabi/", paste("teaching",class,"", sep = "/"))
  
  post_text_new <- c(
    "---", 
    new_yaml, 
    "---", 
    "",
    post_text_body,
    ""
  )
  
  new_post_name <- paste("teaching", class, "index.qmd", sep = "/")
  
  dir_path <- paste("teaching", class, sep = "/")
  if (!dir.exists(dir_path)) dir.create(dir_path)
  
  write_lines(post_text_new, file = new_post_name)
}


convert_post("DASPiR")

walk(all_classes$class, convert_post)
