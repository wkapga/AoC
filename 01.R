library(tidyverse)

raw <- 
  read_delim(
    file=here::here("2022","01input.txt"),
    skip_empty_rows = FALSE,
    delim =" ",col_names = "calor")

# part1                  
raw |> mutate(elf = is.na(calor)*1) |> 
  mutate(elf = cumsum(elf) +1 ) |> 
  drop_na() |> 
  group_by(elf) |> 
  summarize(calor = sum(calor) ) |> 
  slice_max(order_by = calor, n=1)

# part2                  
raw |> mutate(elf = is.na(calor)*1) |> 
  mutate(elf = cumsum(elf) +1 ) |> 
  drop_na() |> 
  group_by(elf) |> 
  summarize(calor = sum(calor) ) |> 
  slice_max(order_by = calor, n=3) |> 
  summarise(calor=sum(calor))


raw2 <-
here::here("2022","01input_a.txt") |> 
  readLines() |> as.numeric()

aa <-
raw2 |> map_lgl(is.na) |> which()

split(raw2, cut(raw2, breaks = aa-1) , drop = TRUE)




raw2 |> purr


?scan

