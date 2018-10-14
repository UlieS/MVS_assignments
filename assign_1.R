setwd("/home/ulie/Uni/WS18/Multivariate Statistics/assignments/assign_1/")

drugs <-read.delim("drug_discovery.txt", header = TRUE, sep="",dec = ".")
sub_drugs <- subset(drugs, select = -c(ID)) 
sub_drugs <- data.frame(sub_drugs)
prin_comp <- prcomp(sub_drugs, scale. = T)


explained_var <-((prin_comp$sdev)^2)/sum((prin_comp$sdev)^2)

plot(explained_var, xlab = "Principal Component",
     ylab = "Proportion of Variance Explained",
     type = "b")


# part B