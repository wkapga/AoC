#library(tidyverse)
library(readr)
library(magrittr)
library(dplyr)
library(purrr)
library(tidyr)

inputfile <- 
  #here::here("2022","09input.txt")
  here::here("09input.txt")

raw <-   
  inputfile %>% 
  read_fwf(fwf_widths(c(1,3)), col_types = "ci")

h_devel <- 
raw %>%  
  mutate(xdir = (X1 %in% c("R","L"))) %>% 
  mutate( sign_dir = (X1 %in% c("R","U"))*2-1) %>% 
  mutate(x_h =1+  cumsum(xdir*X2*sign_dir)) %>% 
  mutate(y_h =1+  cumsum((!xdir)*X2*sign_dir)) %>%  
  mutate( movm = pmap(list(X2, xdir, sign_dir),
              ~ tibble(xm = ..2 * rep( ..3, ..1),
                       ym = (!..2) * rep( ..3, ..1))
                          )) %>% 
  unnest(col="movm") %>% 
  mutate(x_h2 = 1+ cumsum(xm)) %>%  
  mutate(y_h2 = 1+ cumsum(ym)) 

hd <-
h_devel %>% 
  transmute(x=x_h2,y=y_h2) %>% 
  bind_rows(tibble(x=1,y=1), .) %>% 
  mutate(xt=1, yt=1)

for (i in 2:nrow(hd)) {
  xd <- (hd$x[[i]]-hd$xt[[i-1]])
  yd <- (hd$y[[i]]-hd$yt[[i-1]])
# print(paste(xd,yd))
  if ((abs(xd)<2)& (abs(yd)<2)) {
    hd$xt[[i]] = hd$xt[[i-1]]
    hd$yt[[i]] = hd$yt[[i-1]]
    
  } else {
    if ((abs(xd) == 2) & (yd == 0)) {
      hd$xt[[i]] = hd$xt[[i-1]] + sign(xd)
      hd$yt[[i]] = hd$yt[[i-1]]
    }
    if ((abs(yd) == 2) & (xd == 0)) {
      hd$yt[[i]] = hd$yt[[i-1]] + sign(yd)
      hd$xt[[i]] = hd$xt[[i-1]]
    }
    if ((abs(xd) == 2) & (abs(yd) == 1)) {
      hd$xt[[i]] = hd$xt[[i-1]] + sign(xd)
      hd$yt[[i]] = hd$y[[i-1]]
    }
    if ((abs(yd) == 2) & (abs(xd) == 1)) {
      hd$yt[[i]] = hd$yt[[i-1]] + sign(yd)
      hd$xt[[i]] = hd$x[[i-1]]
    }
    
    
        
  }
}

# part1
hd %>% transmute(dd=paste(xt,yt,sep="X")) %>% distinct() %>% nrow()
