The style guide is build on the tidyverse style guide: https://style.tidyverse.org/pipes.html with the following adaptions:

-  The magrittr operator %<>% will be used

**good**

price <- 323

price %<>% paste(., "€")


**bad**

price <- 323

price <- price %>% paste(., "€")


- We value both expressive as well as short file names. If in conflict the first is preferred over the second.

**good**

share_price

**bad**

shr_prc

- If possible seperate fachlich from technisch. First technisch then fachlich
  -- use pipes to encapsule technical parts 

In the following example this line `splitted <- strsplit(txt, split = " ")` is irrelevant to understand the process. The variable name brings
more confusion than benefit.

**good**

txt <- "25.06.2020 APPLE 323,05 €"

company <- strsplit(txt, split = " ") %>% 
  unlist %>% 
  .[2]


**bad**

txt <- "25.06.2020 APPLE 323,05 €"

splitted <- strsplit(txt, split = " ")

company <- splitted[[1]][2]

