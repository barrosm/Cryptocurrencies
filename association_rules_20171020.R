# program to obtain association rules
# Monica Barros
#
# Date: 20/10/2017
#
# Changing directory to where the PROGRAM is
# 

#===================================================================================
# Set working directory to an appropriate place
#===================================================================================
setwd('C://Users//mbarros_prest//Dropbox///GLOBOSAT')

#==============================================================================
# Definition of auxiliary functions
# =============================================================================
# NEW VERSION - Specifies default mirror
instala_pacote <- function(pacote, mirror = "http://cloud.r-project.org/") {
  novo_pacote = pacote [!(pacote %in% installed.packages()[, "Package"])]
  if (length(novo_pacote))
    install.packages(novo_pacote, dependencies = TRUE, repos = mirror)
  }
# ==================================================================================
# Changes repo to Fundacao Oswaldo Cruz
mirror1="http://cran.fiocruz.br/"


#pacs = c("jsonlite","quantmod", "RCurl", "httr", "zoo", "xts","urca", "ggplot2", "DistributionUtils", "xlsx")
#pacs = c(pacs,"dygraphs", "RColorBrewer", "stringr", "corrplot","PerformanceAnalytics", "TTR", "timeSeries", 'fPortfolio' )

# pacs = c("arules", "arulesViz", "readr", "xlsx")

pacs = c("arules", "arulesViz", "readr")
instala_pacote_results <- sapply(pacs, instala_pacote, mirror = mirror1)
require_results <- sapply(pacs, require, character.only = TRUE)

if (!all(require_results)) print("Erro nos pacotes requeridos!");

#==============================================================================
# Read in csv file using the readr package
#==============================================================================
nome_arq = "C://Users//mbarros_prest//Dropbox//GLOBOSAT//mkt_basket_user_video_info_joined_01.csv"
#nome_arq = "C://Users//mbarros_prest//Dropbox//GLOBOSAT//mkt_basket_user_video_info_joined_01.csv"

# Read data into a TIBBLE using read_csv 
# read_csv and read_csv2 are part of "readr" package
df = read_csv(nome_arq, col_names = TRUE, locale = default_locale(), progress = interactive())
# df$nome_programa_long = paste(df$nome_programa,df$titulo)
# df$nome_programa_long = as.factor(df$nome_programa_long)
# df$mes = as.factor(df$mes)
# df$user = as.factor(df$user)
# df$R_mediaid = as.factor(df$R_mediaid)
# df$body = as.factor(df$body)
# df$nome_canal = as.factor(df$nome_canal)
# df$tipo_midia = as.factor(df$tipo_midia)
# df$nome_produto = as.factor(df$nome_produto)
# df$nome_programa = as.factor(df$nome_programa)
# df$titulo= as.factor(df$titulo)
# df$duracao= as.factor(df$duracao)
# df$hits= as.factor(df$hits) 
# df = data.frame(df)
# tt = read.transactions(df, format = "single", sep = ",", cols = c("user","nome_programa"), rm.duplicates = TRUE, encoding = "UTF-8")

# Read data from csv file into transaction matrix
# The read.transactions command does not accept reading data directly from a dataframe, apparently

#trans = read.transactions(nome_arq, format = "single", sep = ",", cols = c("user","R_mediaid"), rm.duplicates = TRUE, encoding = "UTF-8")
trans = read.transactions(nome_arq, format = "basket", sep = ",", cols = c("user","basket"), rm.duplicates = TRUE, encoding = "UTF-8")


# Descriptive stats of transactions
# =============================================================================
summary(trans)
dim(trans)
size_transactions=size(trans)
quantile(size_transactions, probs=seq(0,1,0.1))

# Check which are the large transactions (more than 10 items)
transactionInfo(trans[size(trans) > 10])


# Calculate item frequencies
# Note that the numbers are VERY SMALL!!!
item_frequency = itemFrequency(trans)
print(paste("Max item frequency = ", max(item_frequency)))
summary(item_frequency)

item_count = (item_frequency/sum(item_frequency))*sum(size_transactions)

# Summary statistics - item counts - all transactions
# =============================================================================
summary(item_count)
quantile(item_count, probs=seq(0,1,0.1))
# Now the upper quantiles
quantile(item_count, probs=seq(0.8,1,0.05))

# Most common items - all transactions
# =============================================================================
ordered_item=sort(item_count, decreasing = T)
# Top 10  items
ordered_item[1:10]

# Save ordered items to Excel file
# =============================================================================
ordered_item=as.data.frame(ordered_item)
ordered_item$mediaid = rownames(ordered_item)
row.names(ordered_item) <-NULL
openxlsx::write.xlsx(ordered_item, 
                     "C://Users//mbarros_prest//Dropbox//GLOBOSAT//ordered_items.xlsx",
                     asTable = FALSE)

# ========================================================================
# Most common items (top 30) - ALL transactions
# ========================================================================
# The default plot is vertical, changed to horizontal
# itemFrequencyPlot(trans,support = 0.7/100, cex.names = 0.8, horiz=TRUE)
itemFrequencyPlot(trans,topN=30, cex.names = 0.8, horiz=TRUE, main="Top 30 Videos", col="indianred")
itemFrequencyPlot(trans,topN=30, cex.names = 0.8, horiz=TRUE, type = "absolute", main="Top 30 Videos", col="steelblue3")

# ========================================================================
# Create matrix where transactions involve more than one item
# ========================================================================
more_one = trans[size(trans) > 1]

summary(more_one)
dim(more_one)

# take a look at the first few transactions
inspect(head(more_one))

# Create matrix where transactions involve more than SIX items
more_six = trans[size(trans) > 6]

# take a look at the first few transactions that include more than SIX items
summary(more_six)
dim(more_six)

# check which are the most frequent items in the transaction set that 
# contains more than one product
# The default plot is vertical, changed to horizontal
itemFrequencyPlot(more_one,topN=20, cex.names = 0.8, horiz=TRUE, main="Top 20 Videos - baskets with 2 or more items", col=69)
itemFrequencyPlot(more_one,topN=20, cex.names = 0.8, horiz=TRUE, type="absolute",
                  main="top 20 Videos - baskets with 2 or more items", col="steelblue2")
# =========================================================================
# Item frequencies and item counts in transaction matrix that includes ONLY
# transactions with more than 1 item
# =========================================================================
size_transactions_more_one = size(more_one)
item_frequency_more_one = itemFrequency(more_one)
item_count_more_one = (item_frequency_more_one/sum(item_frequency_more_one))*sum(size_transactions_more_one)
# =============================================================================
# Summary statistics - item counts - only baskets with more than one item
# =============================================================================
summary(item_count_more_one)
quantile(item_count_more_one, probs=seq(0,1,0.1))
# Now the upper quantiles
quantile(item_count_more_one, probs=seq(0.8,1,0.05))

# =============================================================================
# Most common items - baskets with more than one item
# =============================================================================
ordered_item_more_one=sort(item_count_more_one, decreasing = T)
# Top 10 items - basket with 2 or more items
ordered_item_more_one[1:10]

# =============================================================================
# Cross-tabulation - WILL GENERATE A VERY LARGE OBJECT!!!!!
# =============================================================================
tt = arules::crossTable(trans, sort = TRUE)
# Top 10
tt[1:10, 1:10]
# =============================================================================
# A priori rules
# =============================================================================
rules = apriori(more_one, parameter = list(support = 0.1/100
                                           , confidence = 0.50))
summary(rules)

# if you have just a few rules you can execute this command, otherwise subset
# inspect(rules)
# or inspect a smaller set of rules, sorted by some criteria
inspect(head(sort(rules, by="lift"),3));

rules@info

rules@lhs
nrow(rules@lhs@itemInfo)

# Plot of rules and their lift, support and confidence
plot(rules, measure=c("support","lift"), shading="confidence");
plot(rules, measure=c("support","confidence"), shading="confidence");

plot(rules, shading="order", control=list(main ="Two-key plot"));

subrules = rules[quality(rules)$confidence > 0.8];

subrules

plot(subrules, method="matrix", measure="lift");

plot(subrules, method="matrix", measure="lift", control=list(reorder=TRUE));

plot(subrules, method="matrix3D", measure="lift");

plot(subrules, method="matrix3D", measure="lift", control = list(reorder=TRUE));

plot(subrules, method="matrix", measure=c("lift", "confidence"));

plot(subrules, method="matrix", measure=c("lift","confidence"), control = list(reorder=TRUE));

plot(rules, method="grouped");

plot(rules, method="grouped", control=list(k=50));

# place labels in a character vector
labels_rules=labels(rules)

plot(rules, method = NULL, measure = "support", shading = "lift",interactive = FALSE)#, data = NULL, control = NULL, ...)

# =========================================================================
# Using two measures in the matrix plots
# Create graph with ALL RULES that where created from sets with > 1 product
# ============================================================================
plot(rules, method="matrix", measure=c("lift", "confidence"), control = list(main = "All Rules - Consequent vs Antecedent", cex = 0.6))
plot(rules, method="matrix", measure=c("support", "confidence"), control = list(main = "All Rules - Consequent vs Antecedent", cex = 0.5))


# Build subset of rules according to different criteria
# =========================================================================
# top 50 rules by confidence with support above a certain level
subrules_conf = subset(rules, support > 0.02/1000)
subrules_conf = head(sort(subrules_conf, by="confidence"), 50) 
inspect(subrules_conf)
# place labels in a character vector
labels_subrules_conf=labels(subrules_conf)

# ============================================================================
# Create graph with top 50 rules that where created from sets with > 1 product
# ============================================================================
plot(subrules_conf, method="graph", control=list(main = "Top 50 rules by Confidence based on sets with at least 1 video", cex = 0.5))

plot(subrules_conf, method="matrix", measure="confidence", control=list(reorder=TRUE))

plot(subrules_conf, method="graph", control=list(type="itemsets", cex = 0.6, main = "Top 50 rules by Confidence"))




# # =========================================================================
# # USING THE ENTIRE DATASET
# # THE RESULTS SEEM WORSE THAN WHEN CONSIDERING THE DATASET WITH 
# # TRANSACTIONS THAT INCLUDE MORE THAN ONE ITEM
# # A priori rules
# # =========================================================================
# rules_all = apriori(trans, parameter = list(support = 0.1/1000, confidence = 0.5))
# summary(rules_all)
# inspect(rules_all)
# 
# plot(rules_all, method = NULL, measure = "support", shading = "lift",interactive = FALSE)#, data = NULL, control = NULL, ...)
# 
# plot(rules_all, measure=c("support", "lift"), shading="confidence")
# 
# # order = number of els in rule
# plot(rules_all, shading="order", control=list(main = "Two-key plot"))
# plot(rules_all, shading="order", control=list(main = "Lift vs Confidence arranged by order"), measure=c("confidence", "lift"))
# plot(rules_all, shading="order", control=list(main = "Lift vs Support arranged by order"), measure=c("support", "lift"))
# 
# plot(rules_all, shading="order", control=list(main = "Lift vs Support arranged by order"), measure=c("support", "lift"), interactive = TRUE)
# 
# # Build subset of rules according to different criteria
# subrules_conf_all = subset(rules_all, support > 0.02/1000)
# subrules_conf_all = head(sort(subrules_conf_all, by="confidence"), 30)
# inspect(subrules_conf_all)
# labels(subrules_conf_all)
# 
# # =========================================================================
# # Create graph with top 30 rules
# # =========================================================================
# plot(subrules_conf_all, method="graph", main = "Top 30 rules by Confidence")
# 
# plot(subrules_conf_all, method="matrix", measure="confidence", control=list(reorder=TRUE))
# 
# plot(subrules_conf_all, method="graph", control=list(type="itemsets", cex = 0.6, main = "Top 30 rules by Confidence"))
# 
# # =========================================================================
# # Using two measures in the matrix plots
# # Now we use the ENTIRE SET OF RULES
# # =========================================================================
# plot(rules_all, method="matrix", measure=c("lift", "confidence"), control = list(main = "All Rules - Consequent vs Antecedent", cex = 0.6))
# plot(rules_all, method="matrix", measure=c("support", "confidence"), control = list(main = "All Rules - Consequent vs Antecedent", cex = 0.5))
# 
# 
