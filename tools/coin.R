coin <- function(n){
  s <- '111101110100000110010000110101110001'
  m <- unlist(strsplit(s, ""))
  r <- n %% length(m)
  if ((r && 36) == 0) {r = 36}
  if (m[r] == '1')
    return('Tails')
  else
    return('Heads')
}
