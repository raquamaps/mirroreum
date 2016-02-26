library("crayon")
library("stringr")

dw <- "
      ____
     |DATA|
     |<:-)|
(____|____|____)
WRAN(<> <>)GLER
     | /  |
     | _  |
      \\__/

"

pad_before <- function(x, n) {
  res <- paste0(str_dup(" ", n), 
    unlist(strsplit(x, split = "\n")))
  return (paste0(collapse = "\n", res))
}

dw <- pad_before(dw, 1)

dw_logo <- function(art = dw) {
  `%+%` <- crayon::`%+%`
  g <- crayon::green $ bgWhite
  res <- g(art)
  return (res)
}

message(dw_logo(dw))

