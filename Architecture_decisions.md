
# Functional vs OO

- the user should have the opportunity to modify parameter, like
-- choice of columns

- user needs to access data from RStudio after the document was rendered:
    - access additional column candidates to potentially scraping 
    - for updating the document
        - page_url
        - req_method
        - response_string
        - response_string
        - request body
        - XPathes
        - test_eval
        - use_header

--> then functions have to be recalled and the document re-rendered

Therefore, an environment should be used to store this data.

- certain parameter are used across functions
