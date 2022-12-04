library(tidyverse)


raw <- 
  read_delim(
    #file=here::here("2022","03input_a.txt"),
    file=here::here("2022","03input.txt"),
    skip_empty_rows = FALSE,
    delim =" ",
    col_names = c("rucksack") )
    

find_dupe <- function(ruck_str){
  ll = str_length(ruck_str)
  r <- list()
  r[1] <- str_trunc(ruck_str, width = ll/2, side = "left", ellipsis="")
  r[2]<- str_trunc(ruck_str, width = ll/2, side = "right", ellipsis="")
  r <- r |> map(~ .x |> str_split(pattern=""))
  intersect(r |> pluck(1,1),   r |> pluck(2,1) )
}

lkup <- 
  tibble(let = c(tolower(LETTERS), LETTERS) ) |> 
    mutate(let_num = row_number())


raw |> 
  deframe() |> 
  map_chr(find_dupe) |> 
  enframe(value ="let") |> 
  left_join(lkup) |> 
  summarise(sum(let_num))
  

# part 2

find_dupe2 <- function(ruck_df){
  
  r <-
    ruck_df |> 
    deframe() |> 
    str_split(pattern="")
  
  intersect(r |> pluck(1), r |> pluck(2)) |> 
    intersect(r |> pluck(3))
  
}

raw |> 
  mutate(grp_n = floor((row_number()-1)/3)) |> 
  group_by(grp_n) |> 
  nest() |> 
  mutate( badge = map_chr(data, ~ find_dupe2(.)) ) |>
  transmute(let=badge) |> 
  left_join(lkup) |> 
  ungroup() |> 
  summarise(sum(let_num)) |> 
  deframe() |> clipr::write_clip()
  

