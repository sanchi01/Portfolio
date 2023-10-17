library(ggplot2)
library(arules)
library(arulesViz)
# Inspection of the rules

data2=read.csv("https://raw.githubusercontent.com/ThanhDatIU/datacamp/main/Online_Retail_2011_Q1.csv")
Two_baskets = data2 %>%
  filter(InvoiceNo %in% c(540160, 540017))
# Basket size
Two_baskets %>%
  group_by(InvoiceNo) %>%
  dplyr::summarize(
    n_total = n(),
    n_items = n_distinct(StockCode))
dataclean = data2 %>% filter(complete.cases(.))
# Create dataset with basket counts and inspect results
basket_size = dataclean %>%
  group_by(InvoiceNo) %>%
  dplyr::summarize(n_total = n(),
                   n_items = n_distinct(StockCode))

head(basket_size)


# Calculate average values
basket_size %>% summarize(avg_total_items = mean(n_total), 
                          avg_dist_items = mean(n_items))
# Distribution of distinct items in baskets
ggplot(basket_size, aes(n_items)) +
  geom_bar() + ggtitle("Distribution of basket sizes")
#############################3 SPECIFIC ITEM ###################
dataclean %>%
  filter(Description == "HERB MARKER THYME")  %>%
  dplyr::summarize(n_tot_items = n(),
                   n_basket_item = n_distinct(InvoiceNo))
# Number of total and distinct items for HERB MARKER ROSEMARY
dataclean %>%
  filter(Description == "HERB MARKER ROSEMARY")  %>%
  dplyr::summarize(n_tot_items = n(),
                   n_basket_item = n_distinct(InvoiceNo))
# Number of baskets containing both items
dataclean %>%
  filter(Description %in% c("HERB MARKER ROSEMARY", "HERB MARKER THYME")) %>%
  group_by(InvoiceNo) %>% 
  dplyr::summarize(n = n()) %>% 
  filter(n==2) %>% 
  dplyr::summarize(n_distinct(InvoiceNo))

#########################################Apriory####################3
# Splitting transactions
data_list = split(dataclean$Description,
                  dataclean$InvoiceNo)
# Transform data into a transactional dataset
Online_trx = as(data_list, "transactions")
summary(Online_trx)


inspect(head(Online_trx,3))
rules_online = apriori(Online_trx,
                       parameter = list(supp = 0.01, 
                                        conf = 0.9, 
                                        minlen = 2))
######################################if then ####################
supp_herb_markers = 
  apriori(Online_trx, 
          parameter = list(
            target = "frequent itemsets",
            supp = 0.01),
          appearance = list(
            items = c("HERB MARKER THYME",
                      "HERB MARKER ROSEMARY"))
  )
inspect(supp_herb_markers)
rules_thyme_marker_rhs =
  apriori(Online_trx,
          parameter = list(supp=0.01, conf=0.8, minlen=2),
          appearance = list(rhs = "HERB MARKER THYME"),
          control = list(verbose=F))

# Inspect rules
inspect(rules_thyme_marker_rhs)
rules_thyme_marker_lhs =
  apriori(Online_trx,
          parameter = list(supp=0.01, conf=0.8, minlen=2),
          appearance = list(lhs = "HERB MARKER THYME"),
          control = list (verbose=F))

# Inspect rules
inspect(rules_thyme_marker_lhs)

# Apply the apriori function to the Online retail dataset
rules = apriori(Online_trx,
                parameter = list(supp = 0.01, conf = 0.8, 
                                 minlen = 2))
inspect(head(rules))
redundant_rules = is.redundant(rules)

# Inspect the non redundant rules
non_redundant_rules = rules[!redundant_rules]
inspect(head(non_redundant_rules))
###################################### Visualisation
# Changing the font of the items
itemFrequencyPlot(Online_trx,
                  topN = 10,
                  col = rainbow(10),
                  type = "relative",
                  horiz = TRUE,
                  main = "Relative Item Frequency Plot",
                  xlab = "Frequency",
                  cex.names = 0.8
)
inspectDT(rules_online)
plot(rules_online,
     measure = c("confidence", "lift"),
     shading = "support")
# Plot a matrix plot
plot(tail(sort(rules_online, by="lift"), 5),
     method = "graph",
     engine = "htmlwidget")
# Create an interactive graph visualization
rules_html = plot(rules_online, method = "graph",
                  engine = "htmlwidget")
library(htmlwidgets)
# Save the interactive graph as an html file
saveWidget(rules_html, file = "rules_grocery.html")


