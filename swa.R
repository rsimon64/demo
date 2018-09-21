seq1 <- "acctaagg"
seq2 <- "ggctcaatca"

string2vec <- function(s) {
  result <- strsplit(s, split = "")[[1]]
  return(result)
}

n_r <- nchar(seq1) + 1
n_c <- nchar(seq2) + 1
smx <- matrix(0, nrow = n_r,  ncol = n_c)

sqv1 <- c(" ", string2vec(seq1))
sqv2 <- c(" ", string2vec(seq2))


gap_penalty <- -2
ltr_match <- 2
ltr_mismt <- -1

colnames(smx) <- sqv2
row.names(smx) <- sqv1


