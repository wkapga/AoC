library(tidyverse)

# part 1

raw <- 
  read_delim(
#    file=here::here("2022","04input_a.txt"),
   file=here::here("2022","04input.txt"),
    delim =",",col_names = c("e1","e2") )

# part 1
raw |> 
  separate(e1, into = c("e1l","e1h"), convert = TRUE) |> 
  separate(e2, into = c("e2l","e2h"), convert = TRUE) |> 
  filter( ((e1l<=e2l) & (e1h>=e2h)) |
            ((e2l<=e1l) & (e2h>=e1h)) ) |> 
  summarise(n()) |> 
  deframe()  |> 
  clipr::write_clip()
  
# part 2

raw |> 
  separate(e1, into = c("e1l","e1h"), convert = TRUE) |> 
  separate(e2, into = c("e2l","e2h"), convert = TRUE) |> 
  filter( ((e1h >= e2l) & (e1h<=e2h)) |
            ((e2h >=e1l) & (e2h<=e1h)) ) |> 
  summarise(n()) |> 
  deframe()  |> 
  clipr::write_clip()



