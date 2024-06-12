library(stringr)
library(readr)

list.files(path = "../jepusto.com/content", ,full.names = TRUE)

# authors

authors <- list.files(path = "../jepusto.com/content/authors")
new_authors <- list.files(path = "people")
setdiff(authors, new_authors)

author_redirects <- paste0(
  "/authors/", 
  tolower(authors),
  "/ ",
  c("/index.html", paste0("/people/", authors[-1],"/"))
)

# talk

talks <- list.files(path = "../jepusto.com/content/talk")
talks <- talks[-1]

new_talks <- list.files(path = "presentations")
setdiff(
  str_remove(talks,".md$"), 
  str_remove(new_talks, ".qmd$")
)

talk_redirects <- paste0(
  "/talk/",
  tolower(str_sub(talks, 1, -4)),
  "/ ",
  "/presentations/",
  str_replace(talks, "\\.md", ".html")
)


# post

posts <- list.files(".Rmd$",path = "../jepusto.com/content/post")

get_slug <- function(file) {
  x <- readLines(paste("../jepusto.com/content/post", file, sep = "/"))
  x <- x[str_detect(x, "^slug:")]
  if (length(x) == 0) return("") 
  str_trim(str_sub(x, 6, -1))
}

slugs <- sapply(posts, get_slug)
new_slugs <- slugs
slugs["Crashes-in-Austin-and-Travis-Co.Rmd"] <- "fatal-crashes-in-austin/travis-county"
new_slugs["Crashes-in-Austin-and-Travis-Co.Rmd"] <- "Crashes-in-Austin-and-Travis-Co"

new_posts <- list.files(path = "posts")
setdiff(new_slugs, new_posts)

post_redirects <- paste0(
  "/",
  slugs,
  " ",
  "/posts/",
  new_slugs, 
  "/"
)  

# software

software <- list.files(path = "../jepusto.com/content/software")

new_software <- list.files(path = "software")
setdiff(software, new_software)

# teaching

teaching <- list.files(path = "../jepusto.com/content/teaching")

new_teaching <- list.files(path = "teaching")
setdiff(
  str_remove(teaching, ".md$"),
  new_teaching
)

# home page divs
home_redirects <- c(
  "/authors/ /people/",
  "/post/ /posts/",
  "/publication/ /publications/",
  "/talk/ /presentations/"
)
# Write redirects to file

redirects <- c(home_redirects, author_redirects, talk_redirects, post_redirects)
write_lines(redirects, file = "_redirects")
