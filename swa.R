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

# calculate  scores
for (ri in 2:n_r) {
  for (ci in 2:n_c) {
    sf <- ifelse(sqv1[ri] == sqv2[ci], ltr_match, ltr_mismt)
    s_s <- smx[ri - 1, ci - 1] + sf
    s_r <- smx[ri - 1, ci] + gap_penalty
    s_c <- smx[ri, ci - 1] + gap_penalty
    s_a <- max(s_s, s_r, s_c, 0)
    smx[ri, ci] <- s_a
  }
}

