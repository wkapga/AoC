library(tidyverse)

inputfile <- here::here("2022","05input.txt")

raw <-
  inputfile |> 
  readLines()

# find cut in input file
raw_cut <- which(charmatch(raw,"") == 1 )

#stacks
raw_stk <- raw[1:(raw_cut-2)]
#procedures
raw_proc <- raw |> tail(- raw_cut)

raw_pos <- raw[raw_cut-1]

# where are stacks in filestrings? 
stk_pos <- raw_pos |> 
  strsplit("") %>% 
  pluck(1) |>  
  str_detect("\\d")

#nr of stacks
stk_max <- raw_pos |> strsplit(" ") |> unlist() |>  as.numeric() |> max(na.rm=TRUE)

# stk als list of list
stk <-
  raw_stk |> 
  strsplit("") |> 
   map(~ subset(.x, stk_pos )) |> 
   rev() |>
   transpose() |> 
   map(~.x |> unlist() |> head_while(~ .x != " "))  
   
move_from_to <- function(stk, from, to) {
  ele <- stk[[from]] |> tail(1)
  stk[[to]] <- c(stk[[to]], ele)
  stk[[from]] <- stk[[from]] |> head(-1)
  return(stk)
}

mult_move_from_to <- function(stk, from, to, times) {
  for (i in 1:times) { stk <- move_from_to(stk, from, to) }
  return(stk)
}
 
# list of procs
procs <-
  raw_proc |> 
  str_remove_all(pattern="[a-z]+ ") |> 
  strsplit(" ") |> 
  map(as.numeric) 

# loop over procs
for (i in 1:length(procs) ) {

  stk <-  mult_move_from_to(stk, 
                            from = procs |> pluck(i,2), 
                            to = procs |> pluck(i,3),
                            times = procs |> pluck(i,1) ) 
}


# solve: highest element of each stack
stk |> 
  map(~ tail(.x,1)) |> 
  unlist() |> 
  str_c(collapse="") |> 
  clipr::write_clip()
  
  

