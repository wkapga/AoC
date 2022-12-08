library(tidyverse)


raw <-
  here::here("2022","07input.txt") |> 
  readLines()



command <-
  raw |> 
  tail(-1) |> 
  #str_replace("\\/","root") |> 
  str_remove("\\$") |> 
  str_remove("^\\W+") |> 
  discard(~.x |> str_detect("^[ls|dir]"))  
  

tree1 <- 
  tibble(fname=c("dummy"), fsize=c(0), fpath=(""))

dir_list <- "/"
path <- NULL
for (i in seq_along(command)){
  # is a file
  if (command[i] |> str_detect("^\\d+") ) {
    tibble(fname = command[i] |>str_extract("\\w+$"),
           fsize = command[i] |>str_extract("^\\d+") |> as.integer(),
           fpath = path) |> 
      bind_rows(tree1) ->
      tree1
  }
  # cd to a new dir
  if (command[i] |> str_detect("^cd") ) {
    if (command[i] |> str_detect("\\.\\.$") ) {
      path <- path |> str_remove("/\\w+$") 
    } else {
      path <- path |> paste0("/", command[i] |> str_extract("\\S+$"))
      dir_list <- dir_list |> c(path)
      
    }
  
  }
    
}

#tree1

#dir_list

# part1
dir_sizes <-
  tibble (dir =dir_list) |> 
  mutate(sz =   map_dbl(dir,      ~ tree1 |> 
                               filter(str_detect(fpath, paste(.x)))|> 
                               summarise(fsize = sum(fsize)) |> 
                               deframe()
                     )  ) 

dir_sizes |> 
  filter(sz <= 100000) |> 
 summarise(sum(sz))

#part2

sz_used <- tree1 |> summarise(sum(fsize)) |> deframe()
sz_free <- 70e6 - sz_used
sz_to_be_freed <- 30e6 -sz_free

dir_sizes |> 
  filter(sz >= sz_to_be_freed) |> 
  summarise(min(sz))



