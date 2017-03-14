#---------------------------------------------------------------------------
#---------------------------------------------------------------------------
#  Market Basket
#  Written by Rafael Guimaraes
#  Date March 13th 2017
#  Version 1.0
#  ---------------------------------------------------------------------------
#  ---------------------------------------------------------------------------


#  ---------------------------------------------------------------------------
#  Data preparation
#  ---------------------------------------------------------------------------

# Association Rules for Market Basket Analysis (R)

library(arules)  # association rules
library(arulesViz)  # data visualization of association rules
library(RColorBrewer)  # color palettes for plots

data(Groceries)  # grocery transactions object from arules package

# show the dimensions of the transactions object
print(dim(Groceries))

print(dim(Groceries)[1])  # 9835 market baskets for shopping trips
print(dim(Groceries)[2])  # 169 initial store items  

# explore possibilities for combining similar items
print(head(itemInfo(Groceries))) 
print(levels(itemInfo(Groceries)[["level1"]]))  # 10 levels... too few 
print(levels(itemInfo(Groceries)[["level2"]]))  # 55 distinct levels

# aggregate items using the 55 level2 levels for food categories
# to create a more meaningful set of items
groceries <- aggregate(Groceries, itemInfo(Groceries)[["level2"]])  

print(dim(groceries)[1])  # 9835 market baskets for shopping trips
print(dim(groceries)[2])  # 55 final store items (categories)  


# obtain large set of association rules for items by category and all shoppers
# this is done by setting very low criteria for support and confidence
first.rules <- apriori(groceries, 
                       parameter = list(support = 0.001, confidence = 0.05))
print(summary(first.rules))  # yields 69,921 rules... too many

# select association rules using thresholds for support and confidence 
second.rules <- apriori(groceries, 
                        parameter = list(support = 0.025, confidence = 0.05))
print(summary(second.rules))  # yields 344 rules



#  ---------------------------------------------------------------------------
#  Question 1 - 
#  ---------------------------------------------------------------------------

# select rules with dairy produts in consequent (right-hand-side) item subsets
dairy.rules <- subset(second.rules, subset = rhs %pin% "dairy produce")
inspect(dairy.rules)  # 58 rules - when the support = 0.025. If we use the support of 0.0343 we get 36 rules.



#  ---------------------------------------------------------------------------
#  Question 2 - 
#  ---------------------------------------------------------------------------
# select association rules using thresholds for support and confidence 
second.rules <- apriori(groceries, 
                        parameter = list(support = 0.0343, confidence = 0.05))
print(summary(second.rules))  # yields 201 rules

#  ---------------------------------------------------------------------------
#  Question 3 - 
#  ---------------------------------------------------------------------------

# select association rules using thresholds for support and confidence 
second.rules <- apriori(groceries, 
                        parameter = list(support = 0.0343, confidence = 0.01))
print(summary(second.rules))  # yields 206 rules

#  ---------------------------------------------------------------------------
#  Question 4 - 
#  ---------------------------------------------------------------------------

# examine frequency for each item with support greater than 0.025
pdf(file="fig_market_basket_initial_item_support.pdf", 
    width = 8.5, height = 11)
itemFrequencyPlot(Groceries, support = 0.0343, cex.names=0.8, xlim = c(0,0.3),
                  type = "relative", horiz = TRUE, col = "dark red", las = 1,
                  xlab = paste("Proportion of Market Baskets Containing Item",
                               "\n(Item Relative Frequency or Support)"))
dev.off()    



pdf(file="fig_market_basket_final_item_support.pdf", width = 8.5, height = 11)
itemFrequencyPlot(groceries, support = 0.0343, cex.names=1.0, xlim = c(0,0.5),
                  type = "relative", horiz = TRUE, col = "blue", las = 1,
                  xlab = paste("Proportion of Market Baskets Containing Item",
                               "\n(Item Relative Frequency or Support)"))
dev.off()   



# data visualization of association rules in scatter plot
pdf(file="fig_market_basket_rules.pdf", width = 8.5, height = 8.5)
plot(second.rules, 
     control=list(jitter=2, col = rev(brewer.pal(9, "Greens")[4:9])),
     shading = "lift")   
dev.off()    

# grouped matrix of rules 
pdf(file="fig_market_basket_rules_matrix.pdf", width = 8.5, height = 8.5)
plot(second.rules, method="grouped",   
     control=list(col = rev(brewer.pal(9, "Greens")[4:9])))
dev.off()    


# sort by lift and identify the top 10 rules
top.dairy.rules <- head(sort(dairy.rules, decreasing = TRUE, by = "lift"), 10)
inspect(top.dairy.rules) 

pdf(file="fig_market_basket_farmer_rules.pdf", width = 11, height = 8.5)
plot(top.dairy.rules, method="graph", 
     control=list(type="items"), 
     shading = "lift")
dev.off() 

#test

# Suggestions for the student:
# Suppose your client is someone other than the local farmer,
# a meat producer/butcher, dairy, or brewer perhaps.
# Determine association rules relevant to your client's products
# guided by the market basket model. What recommendations
# would you make about future marketplace actions?