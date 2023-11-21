## Cart Abandonment  #2
#https://practicaldatascience.co.uk/data-science/how-to-calculate-abandonment-and-completion-rates-using-the-google-analytics-api
library(tidyverse)
library(googleAnalyticsR)
library(janitor)
library(googleAuthR)
#gar_auth("~/tu.httr-oauth")
gar_auth("C:/Users/ThinkErez/Documents/tu.httr-oauth")

account_list <- ga_account_list()
meta <- ga_meta()

# This Month
as.Date(start <- "2021-06-01")
as.Date(end <- "2021-06-30")

mac <- 142253091

# Shopping Behaviour shopping progression data
# Metric	Description
# All sessions	ga:shoppingStage==ALL_VISITS shows sessions from all users and is used in the internal calculations for completion and abandonment rates.
# Sessions with Product Views	ga:shoppingStage==PRODUCT_VIEW shows whether a session included a product page view.
# Sessions with Add to Cart	ga:shoppingStage==ADD_TO_CART shows whether a session included an add-to-cart event.
# Sessions with Checkout	ga:shoppingStage==CHECKOUT shows whether a session included a pageview of the checkout.
# Sessions with Transactions	ga:shoppingStage==TRANSACTION shows whether a session included a successful transaction.


#Shopping Behaviour abandonment data
# Metric	Description
# No Shopping Activity	ga:shoppingStage==NO_SHOPPING_ACTIVITY shows whether a session had no shopping activity.
# Sessions with no Product Views	ga:shoppingStage==NO_PRODUCT_VIEW shows whether a session had no product views.
# No Cart Addition	ga:shoppingStage==NO_CART_ADDITION shows whether a session had no add to carts.
# Cart Abandonment	ga:shoppingStage==CART_ABANDONMENT shows whether a session had a cart abandonment.
# Checkout Abandonment	ga:shoppingStage==CHECKOUT_ABANDONMENT shows whether a session had a checkout abandonment.
# Sessions without Transactions	ga:shoppingStage== shows whether a checkout had no transaction.


# Shopping behaviour completion rates
# Metric	Description
# % Sessions with product views	ga:shoppingStage==PRODUCT_VIEWS / ga:shoppingStage==ALL_VISITS shows the percentage of sessions where the user viewed a product page.
# % Sessions with add to cart	ga:shoppingStage==ADD_TO_CART / ga:shoppingStage==ALL_VISITS shows the percentage of sessions where the user added to cart.
# % Sessions with checkout	ga:shoppingStage==CHECKOUT / ga:shoppingStage==ALL_VISITS shows the percentage of sessions where the user reached the checkout.
# % Sessions with transactions	ga:shoppingStage==TRANSACTION / ga:shoppingStage==ALL_VISITS shows the percentage of sessions where the user successfully ordered.
# 
# Shopping behaviour abandonment rates
# Metric	Description
# % No shopping activity	ga:shoppingStage==NO_SHOPPING_ACTIVITY / ga:shoppingStage==ALL_VISITS shows the percentage of sessions where there was no shopping activity.
# % No cart addition	ga:shoppingStage==NO_CART_ADDITION / ga:shoppingStage==PRODUCT_VIEW shows the percentage of sessions where customers viewed the product page but did not add to cart.
# % Cart abandonment	ga:shoppingStage==CART_ABANDONMENT / ga:shoppingStage==ADD_TO_CART shows the percentage of sessions where customers added to cart and then abandoned the cart.
# % Cart abandonment	ga:shoppingStage==CHECKOUT_ABANDONMENT / ga:shoppingStage==CHECKOUT shows the percentage of sessions where customers reached the checkout but abandoned.


shoppingstagedata <- function(ga_id, start, end, filter=NULL){
  
  gadata <- google_analytics(ga_id, date_range = c(start, end),
                             dimensions = c("date"),
                             metrics = c("sessions"),
                             filtersExpression = filter,
                             max =-1,
                             anti_sample = TRUE)
  
}


#Sessions with product views
sessions_with_productviews <- shoppingstagedata(mac, start, end, "ga:shoppingStage=~^(PRODUCT_VIEW)$")
#Sessions with add to cart
sessions_with_add_to_cart <- shoppingstagedata(mac, start, end, "ga:shoppingStage=~^(ADD_TO_CART)$")
  
#Sessions with checkout
sessions_with_checkout <- shoppingstagedata(mac, start, end, "ga:shoppingStage=~^(CHECKOUT)$")

#Sessions with transactions
sessions_with_transaction <- shoppingstagedata(mac, start, end, "ga:shoppingStage=~^(TRANSACTION)$")
  

#Sessions with no shopping activity
sessions_with_no_activity <- shoppingstagedata(mac, start, end, "ga:shoppingStage=~^(NO_SHOPPING_ACTIVITY)$")
  
#Sessions with no cart addition
sessions_with_no_add_to_cart <- shoppingstagedata(mac, start, end, "ga:shoppingStage=~^(NO_CART_ADDITION)$")

#Sessions with cart abandonment
sessions_with_no_cart_abdn <-  shoppingstagedata(mac, start, end, "ga:shoppingStage=~^(CART_ABANDONMENT)$")

#Sessions with checkout abandonment
sessions_with_checkout_abdn <- shoppingstagedata(mac, start, end, "ga:shoppingStage=~^(CHECKOUT_ABANDONMENT)$")
  


#Sessions by shopping stage
sessions_shopping_stage <- google_analytics(mac, date_range = c(start, end),
                                                dimensions = c("date","shoppingStage"),
                                                metrics = c("sessions"),
                                               # filtersExpression = c("ga:shoppingStage=~^(CHECKOUT_ABANDONMENT)$"),
                                                max =-1,
                                                anti_sample = TRUE)

sessions_shopping_stage_wide <- sessions_shopping_stage %>%
                                pivot_wider("date", names_from = "shoppingStage", values_from="sessions")



#Checkout completion metrics
checkout_funnel <- sessions_shopping_stage %>%
                  filter(str_detect(shoppingStage, regex("date|ALL_VISITS|^PRODUCT_VIEW$|^ADD_TO_CART$|^CHECKOUT$|^TRANSACTION$")))


checkout_funnel_wider <- checkout_funnel %>%
                        pivot_wider("date", names_from="shoppingStage", values_from="sessions")


#Checkout completion rates
checkout_funnel_wider_rates <- checkout_funnel_wider %>%
                                mutate(sessions_with_product_views = (PRODUCT_VIEW/ALL_VISITS)*100,
                                       sessions_with_add_to_cart = (ADD_TO_CART / ALL_VISITS)*100,
                                       sessions_with_checkout = (CHECKOUT / ALL_VISITS)*100,
                                       sessions_with_transactions = (TRANSACTION / ALL_VISITS)*100)

checkout_funnel_rates_only <- checkout_funnel_wider_rates %>% select("date",contains("sessions_with"))


#Checkout abandonment metrics
checkout_abnd_funnel <- sessions_shopping_stage %>%
                          filter(str_detect(shoppingStage, regex("date|ALL_VISITS|^CHECKOUT$|^NO_SHOPPING_ACTIVITY$|^NO_CART_ADDITION$|^CART_ABANDONMENT$|^CHECKOUT_ABANDONMENT$|^PRODUCT_VIEW$|^ADD_TO_CART$")))


checkout_abnd_funnel <- checkout_abnd_funnel %>%
  pivot_wider("date", names_from="shoppingStage", values_from="sessions")


checkout_abnd_funnel_wider_rates <- checkout_abnd_funnel %>%
  mutate(sessions_with_no_shipping_activity  = (NO_SHOPPING_ACTIVITY/ALL_VISITS)*100,
         sessions_with_no_cart_addition  = (NO_CART_ADDITION / PRODUCT_VIEW)*100,
         sessions_with_cart_abandonment  = (CART_ABANDONMENT / ADD_TO_CART)*100,
         sessions_with_checkout_abandonment  = (CHECKOUT_ABANDONMENT / CHECKOUT)*100)

checkout_abnd_funnel_rates_only <- checkout_abnd_funnel_wider_rates %>% select("date",contains("sessions_with"))


