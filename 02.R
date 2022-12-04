library(tidyverse)

# part 1

#matrix of results

# __R_P_C  me
# R_3_6_0
# P_0_3_6
# C_6_0_3
#oppo

raw <- 
  read_delim(
    #file=here::here("2022","02input_a.txt"),
    file=here::here("2022","02input.txt"),
    skip_empty_rows = FALSE,
    delim =" ",col_names = c("oppo","me") )
    

tab1 <- tibble(oppo = LETTERS[1:3], op_num = 1:3)
tab2 <- tibble(me = LETTERS[24:26], me_num = 1:3)

tab_match <-
  expand.grid(a1=c(1:3), a2=c(1:3)) |> 
  mutate(mtch = 10*a1 + a2) |> 
  mutate(mtch_res= c(3,0,6,6,3,0,0,6,3)
  ) |> 
  select(mtch, mtch_res)


raw |> 
  left_join(tab1) |> 
  left_join(tab2) |> 
  mutate(mtch = 10*op_num + me_num) |> 
  left_join(tab_match) |> 
  mutate(score = me_num + mtch_res) |> 
  summarise(total = sum(score))



# part 2

tab3 <- tibble(result_whish = LETTERS[24:26], whish_num = c(0,3,6))

tab_match2 <-
  expand.grid(a1=c(1:3), a2=c(1:3)) |> 
  #mutate(mtch = 10*a1 + a2) |> 
  mutate(mtch_res= c(3,0,6,6,3,0,0,6,3)
  )


raw |>
  rename(result_whish = me ) |> 
  left_join(tab1) |>
  left_join(tab3)  |> 
  left_join(tab_match2, by=c("op_num"="a1", "whish_num"="mtch_res")) |> 
  mutate(score = a2 + whish_num) |> 
  summarise(total = sum(score))
                 