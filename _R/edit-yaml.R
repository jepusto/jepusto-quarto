library(tidyverse)

edit_admin <- function(file, replacement = "", multi_replacement = "James E. Pustejovsky") {
  
  readLines(file) |>
    str_replace("- admin", paste("-", multi_replacement)) |>
    str_replace("author[s]: admin", replacement) |>
    write_lines(file = file)
}

list.files(
  "posts", 
  pattern = "index.qmd", 
  full.names = TRUE, 
  recursive = TRUE
) |> 
  walk(edit_admin)

list.files(
  "publication", 
  pattern = "index.qmd", 
  full.names = TRUE, 
  recursive = TRUE
) |> 
  walk(
    edit_admin, 
    replacement = "authors: \n - James E. Pustejovsky", 
    multi_replacement = "James E. Pustejovsky"
  )

list.files(
  "software", 
  pattern = "index.qmd", 
  full.names = TRUE, 
  recursive = TRUE
) |> 
  walk(
    edit_admin, 
    replacement = "authors: \n - James E. Pustejovsky", 
    multi_replacement = "James E. Pustejovsky"
  )

list.files(
  "presentations", 
  pattern = ".qmd", 
  full.names = TRUE, 
  recursive = TRUE
) |> 
  walk(
    edit_admin, 
    replacement = "authors: \n - James E. Pustejovsky", 
    multi_replacement = "James E. Pustejovsky"
  )
