## Code for Master Thesis - Construction of a knowledge graph using EU's financial regulations
## Author: Konstantinos Panagiotis Apostolou

# Load necessary library
library(readr)
library(dplyr)
library(corrplot)

library(knitr)
library(kableExtra)
library(magrittr)
library(rstatix)

# Import CSV file into a data frame
data <- read_csv("nodes.csv")
centralities <- data%>%select(!c(`_id`, `_labels`, name))
labels <- c('BC', 'CC', 'DC', "EigenC","HitsAuth","HitsHub", "PageRank")
colnames(centralities)<-labels

# Compute Spearman correlation
spearman_corr <- cor(centralities, method = "spearman")

# Print the correlation
print(signif(spearman_corr),3)
corrplot(spearman_corr, method='number',type="lower", tl.srt=45)

lower_triag <- pull_lower_triangle(spearman_corr, diag=T)
cor_mat<-as.data.frame(lower_triag, row.names = lower_triag$rowname)
cor_mat <- cor_mat %>% select(!rowname)
signif(cor_mat,2) %>%
  kable(format = 'latex', booktabs = TRUE)
