library(tidyverse)

raw <-
  here::here("2022","06input.txt") |> 
  readLines()

#raw <- "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg"

puff <- raw |> strsplit(split="") |> unlist()

find_dif_nr <-14

marker <- find_dif_nr

while( ((puff[(marker-find_dif_nr+1):marker] |> unique() |> length() ) < find_dif_nr ) &
       (marker < length(puff)) ){
  marker <- marker +1
 print(c(marker,puff[(marker-find_dif_nr+1):marker] ))  
}

## purrr:::reduce version

my_acc <- function(input, out) { 
  if ( (puff |> 
      magrittr::extract((out-find_dif_nr+1):out) |> 
      unique() |> 
      length() ) == find_dif_nr) { return(done(out))}
  paste(out) 
  }

seq_along(puff) |> 
  tail(-find_dif_nr ) |> 
  reduce(my_acc)
