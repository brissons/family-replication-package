library(car)
library(FSA)

#load data
data <-
  read.table(file = "star_count_per_repo.csv", sep = ",", header = TRUE)

#treat each family_id as a factor
data$family_id <- as.factor(data$family_id)

#testing for homogenous variances
levene <- leveneTest(star_count ~ family_id, data = data)
print(levene)

#kruskal test
kruskal <- kruskal.test(star_count ~ family_id, data = data)
print(kruskal)

#dunn test
dunn <-
  dunnTest(star_count ~ family_id, data = data, method = "bonferroni")

write.csv(dunn[2], file = "star_dunn_results.csv")
