library(tidyverse)

edit_admin <- function(file, replacement = "", multi_replacement = "James E. Pustejovsky") {
  
  readLines(file) |>
    str_replace("author[s]: admin", replacement) |>
    str_replace("- admin", paste("-", multi_replacement)) |>
    write_lines(file = file)
}

list.files(
  "posts", 
  pattern = "index.qmd", 
  full.names = TRUE, 
  recursive = TRUE
) |> 
  walk(edit_admin)
