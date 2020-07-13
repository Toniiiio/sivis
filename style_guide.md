The style guide is build on the tidyverse style guide: https://style.tidyverse.org/pipes.html with the following emphasis / adaptions:

- Emphasis1: We value both expressive as well as short file names. If in conflict the first is preferred over the second.

**ok**

    shr_prc

**better**

    share_price


In the context of sivis `hdr` (header) should be fairly expressive, but adding three characters and write `header` would be also ok.
`body` would be preferred over `bdy` as only a single character would have to be added.

-  Adaption1:The magrittr operator %<>% will be used

**good**

    price <- 323
    price %<>% paste(., "€")


**bad**

    price <- 323
    price <- price %>% paste(., "€")


- Emphasis2: If possible seperate fachlich from technisch. First technisch then fachlich
  -- use pipes to encapsule technical parts 

In the following example this line `splitted <- strsplit(txt, split = " ")` is irrelevant to understand the process. The variable name brings
more confusion than benefit.

**good**

    txt <- "25.06.2020 APPLE 323,05 €"
    company <- txt %>% 
      strsplit(split = " ") %>% 
      unlist %>% 
      .[2]


**bad**

    txt <- "25.06.2020 APPLE 323,05 €"
    splitted <- strsplit(txt, split = " ")
    company <- splitted[[1]][2]

- Adaption2: Spaces in if/while

**good**

    if(has_content){
      
      doc <- response %>% read_html
      doc %>% html_nodes(xpath = "/div")
      
    } 

**bad**

    if(has_content){ 
      doc <- response %>% read_html
      doc %>% html_nodes(xpath = "/div")
    } 


- Emphasis3: Expressive names for if and while

**good**
    
      has_content_type <- !is.null(content_type)
      if(has_content_type){
         ...
      }
      
**bad**

      if(!is.null(content_type)){
         ...
      }



