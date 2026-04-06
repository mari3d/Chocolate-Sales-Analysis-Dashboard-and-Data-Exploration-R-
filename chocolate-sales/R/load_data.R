library(dplyr)
library(lubridate)

# ---- LOAD & MERGE DATA ----
load_and_prepare_data <- function() {
  
  # Load CSV files
  sales <- read.csv("data/sales.csv")
  products <- read.csv("data/products.csv")
  stores <- read.csv("data/stores.csv")
  customers <- read.csv("data/customers.csv")
  
  # Merge datasets
  data <- sales %>%
    left_join(products, by = "product_id") %>%
    left_join(stores, by = "store_id") %>%
    left_join(customers, by = "customer_id") %>%
    mutate(
      order_date = as.Date(order_date),
      margin = profit / revenue
    ) %>%
    na.omit()
  
  return(data)
}
