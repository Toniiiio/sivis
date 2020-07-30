
# Definitions
parameters:
variables:
objects:

# Functional vs OO

- the user should have the opportunity to modify parameter, like
-- choice of columns

* 1 user needs to access data from RStudio after the document was rendered:
    * 1.1 access additional column candidates to potentially scraping 
    * 1.2 having the opportunity to manually edit the code, e.g. the request method, headers, xpath and then still be
    able to select new columns without getting the default values back (for request method, headers, etc.)
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

The parameter 1.2.1 to 1.2.7 are mostly created in the early steps of the scraper creation process. They are used in the beginning, some of them in the middle and most of them at the end of the process. 
If a functional approach would be used, this parameters had to be passed through a lot of functions. But they would not be used in a lot of them.

As sivis can not provide the correct scraping code in all cases, the user has to do manual adaptions and debugging from time to time.
- Inspecting the downloaded document in the viewer pane
 -- highlighting the target values within this document
- see which values are yielded if the generated xpath is applied to the document.
- ...

To be able to perform these functions several inputs are required, often multiple per function, e.g.
- the document
- the target values
- the generated xpath
- ...

These value could be
1. exported to the global environment (not recommended)
2. attched to an environment which can be accessed by the user

If 2. is preferred all required input is already binded to an environment (here named "sivis"). An oo based implementation would
be convenient, because one would only have to call
- sivis$inspect_document()
- sivis$hightlight_target_values()
- sivis$apply_xpath()



S3/S4 vs R6

1. generic functions
2. Reference semantics - threading states
3. chaining


1,

2.


3. chaining
Can also be achieved with magrittr.




Assigning variables / paramerts to object
- upside: 
-- do not hand through a lot of functions
-- are not recreated every time

- downside:
-- errors get harder to track, as it is not easy to spot error for these "global variables"

--> take only objects that will not be altered through the process. 
For parameters / Variables that are altered and required to be bind to the object (so that user can access them in the interactive session), only assign at the very end.


all_text: can change over time - e.g. if it is called from get_leaf_pathes, so it is not always equal to sivis$target_values
