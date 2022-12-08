library(tidyverse)

inputfile <- 
  here::here("2022","08input.txt")
  #here::here("2022","08input_a.txt")

nn <- 
  inputfile |> 
  readLines(n=1)  |> nchar()

raw <-   
  inputfile |>
  read_fwf(fwf_widths(rep(1,nn)), col_types = rep("i",nn))


mm = raw |> as.matrix()         

rotate_bk <- function(x) t(apply(x, 2, rev))
rotate_cw <- function(x) apply(t(x),2,rev)

rotate_cw_mult <- function(x, n) {
  for (i in seq_len(n) ) {
    x <- rotate_cw(x)
  }
  return(x)
}

rotate_bk_mult <- function(x, n) {
  for (i in seq_len(n) ) {
    x <- rotate_bk(x)
  }
  return(x)
}


can_be_seen_row <- function(xx) {
  c(TRUE,
  seq(2,length(xx)) |> 
    map_lgl(~ all(xx[.x] > xx[1:.x-1]) ) 
  )
}


can_be_seen <- function(mm) {
  apply(mm, 1,can_be_seen_row) |> t()
}

# list of all rotations
m1 = list(mm)

for (i in seq_len(3) ) {
  m1[[i+1]] <- rotate_cw_mult(m1[[1]], i)
  
}

m1 <- m1 |> map(~ .x |> can_be_seen() ) 


# rotate back
for (i in seq_len(3) ) {
  m1[[i+1]] <- rotate_bk_mult(m1[[i+1]], i)
}


#m1 |> map( ~ .x * mm)


m2 <- m1 |> reduce(`+`) 

## solution part1
sum(m2 > 0)


## part2

view_score <- function(ll,h) {
  if (max(ll)< h) { 
    length(ll)
  } else {
    max(1, ll |> detect_index( ~.x>= h) )  
    } 
}


scenic_score <- function(mm, row,col){
  h <- mm[[row,col]]
  #left
  mm[row, 1:(col-1)] |> rev() |> view_score(h)  *
  # right
    mm[row, (col+1):ncol(mm)] |> view_score(h) *
  # up
    mm[1:(row-1), col] |> rev() |> view_score(h) *
  # down
    mm[(row+1):nrow(mm), col] |>  view_score(h)
  
}

list( seq(2,ncol(mm)-1), seq(2, nrow(mm)-1) ) |> 
  expand.grid() |> 
  mutate(sc = map2(Var1, Var2, ~ scenic_score(mm,.x,.y ) ) ) |>
  select(sc) |> unlist() |> max()





