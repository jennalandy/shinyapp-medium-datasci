# shinyapp-medium-datasci

- **Hosted at** https://jennalandy.shinyapps.io/medium_datasci/
- **Based off** of medium data science article dataset from [the R for Data Science Tidy Tuesday data repository](https://github.com/rfordatascience/tidytuesday/tree/master/data/2018/2018-12-04). 

This is a data science article recommending application. I first used the urls provided in the dataset to scrape the full text of each article using the [`rvest` package](https://cran.r-project.org/web/packages/rvest/rvest.pdf). Topics of interest were then determined with latent dirichlet allocation topic modeling using the [`topicmodels` package](https://cran.r-project.org/web/packages/topicmodels/index.html). LDA was only fit on a random 3,000 articles for the sake of time. I then classified each of the 78,388 articles as the most frequent topic found in it. Code for data preprocessing can be found in the [topic modeling](./topic_modeling) directory.
