
# Functional vs OO

- the user should have the opportunity to modify parameter, like
-- choice of columns

* 1 user needs to access data from RStudio after the document was rendered:
    * 1.1 access additional column candidates to potentially scraping 
    * 1.2 for updating the document
        * 1.2.1 page_url
        * 1.2.2 req_method
        * 1.2.3 response_string
        * 1.2.4 request body
        * 1.2.5 XPathes
        * 1.2.6 test_eval
        * 1.2.7 use_header

--> then functions have to be recalled and the document re-rendered

Therefore, an environment is required to store this data and make it accessible to the user and enable him/her to modify it.

The parameter 1.2.1 to 1.2.7 are mostly created in the early steps of the scraper creation process. They are used in the beginning, some
of them in the middle and most of them at the end of the process. So they have to be passed through a lot of functions. But they are not
used in a lot of them.

