library(tidyverse)

inputfile <- 
  #here::here("2022","09input.txt")
  here::here("2022","09input_a.txt")

raw <-   
  inputfile |>
  read_fwf(fwf_widths(c(1,3)), col_types = "ci")


raw |> 
  mutate(xdir = (X1 %in% c("R","L"))) |> 
  mutate( sign_dir = (X1 %in% c("R","U"))*2-1) |> 
  mutate(x_h =1+  cumsum(xdir*X2*sign_dir)) |> 
  mutate(y_h =1+  cumsum((!xdir)*X2*sign_dir)) |> 
  mutate( movm = pmap(list(X2, xdir, sign_dir),
              ~ tibble(xm = ..2 * rep( ..3, ..1),
                       ym = (!..2) * rep( ..3, ..1))
                          )) |> 
  unnest(col="movm") |> 
  mutate(x_h2 = 1+ cumsum(xm)) |> 
  mutate(y_h2 = 1+ cumsum(ym)) |> 
  tail()

         