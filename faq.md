1. Scraping is usually build on the structure of the DOM - how does sivis differ?

Answer: Sivis analysis the rendered page in the browser. Specifically, this is the version where
the dom is expanded by the content of all rest responses. But this step is just done to enable the user
to select the target data. Then the contents of the rest responses are filtered for the target data.
Therefore, sivis does not analyse the structure of the dom, but beforehand looks in the server responses
that yield the DOM.
