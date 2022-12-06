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

