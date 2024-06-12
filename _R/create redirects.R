library(stringr)

list.files(path = "../jepusto.com/content", ,full.names = TRUE)

authors <- list.files(path = "../jepusto.com/content/authors")

author_redirects <- paste0(
  "authors/", 
  tolower(authors),
  "/ ",
  c("index.html", paste0("people/", authors[-1]))
)

talks <- list.files(path = "../jepusto.com/content/talk")
talks <- talks[-1]

talk_redirects <- paste0(
  "talk/",
  tolower(str_sub(talks, 1, -4)),
  "/ ",
  "presentations/",
  str_replace(talks, "\\.md", ".html")
)  
