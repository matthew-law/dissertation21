library(wordcountaddin)
library(tibble)


diss_word_count <- function() {
  chapters <- c("01-introduction.Rmd", "02-lit-review.Rmd", "03-methods.Rmd", "04-results.Rmd", "05-discussion.Rmd", "06-conclusion.Rmd")
  chapter_words <- tibble(
    chapter = chapters,
    words = 0
  )
  total <- 0
  for (i in 1:length(chapters)) {
    total <- total + word_count(chapters[i])
    chapter_words$words[i] <- word_count(chapters[i])
  }
  clipr::write_clip(chapter_words)
  message <- paste0("Total word count: ",total,". Word count by chapter copied to clipboard.")
  return(message)
}

diss_word_count()



# non-function version: ---------------------------------------------------

# chapters <- c("01-introduction.Rmd", "02-lit-review.Rmd", "03-methods.Rmd", "04-results.Rmd", "05-discussion.Rmd", "06-conclusion.Rmd")
# 
# chapter_words <- tibble(
#   chapter = chapters,
#   words = 0
# )

# total <- 0
# for (i in 1:length(chapters)) {
#   total <- total + word_count(chapters[i])
#   chapter_words$words[i] <- word_count(chapters[i])
# }
# total
# View(chapter_words)
# 
# clipr::write_clip(chapter_words)

# methods <- word_count("03-methods.Rmd")
# methods
# getwd() %>% list.files() %>% dput()
