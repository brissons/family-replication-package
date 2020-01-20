library(Hmisc)
library(scales)
library(stats)
library(relaimpo)

#load data
metrics <- read.csv("comm_metrics.csv", header=TRUE, sep=",", dec=".", fileEncoding="utf-8")

#hierarchical cluster analysis
predictor_clustering <- varclus(~ depth + 
                                  forks + 
                                  active_forks + 
                                  users_repo + 
                                  users_also_in_family + 
                                  age + 
                                  followers_repo + 
                                  followers_family + 
                                  followers_outside + 
                                  pr_repo + 
                                  pr_family + 
                                  pr_repo_comments + 
                                  pr_family_comments + 
                                  pr_repo_code_comments + 
                                  pr_family_code_comments + 
                                  issue_repo + 
                                  issue_family + 
                                  issue_outside + 
                                  issue_comments_repo + 
                                  issue_comments_family + 
                                  issue_comments_outside + 
                                  issue_subscribed_repo + 
                                  issue_subscribed_family + 
                                  issue_subscribed_outside + 
                                  issue_unsubscribed_repo + 
                                  issue_unsubscribed_family + 
                                  issue_unsubscribed_outside + 
                                  issue_closed_repo + 
                                  issue_closed_family + 
                                  issue_closed_outside + 
                                  issue_reopened_repo + 
                                  issue_reopened_family + 
                                  issue_reopened_outside + 
                                  issue_referenced_repo + 
                                  issue_referenced_family + 
                                  issue_referenced_outside + 
                                  issue_assigned_repo + 
                                  issue_assigned_family + 
                                  issue_assigned_outside + 
                                  issue_mentioned_repo + 
                                  issue_mentioned_family + 
                                  issue_mentioned_outside +
                                  pr_mentioned_repo + 
                                  pr_mentioned_family + 
                                  pr_mentioned_outside + 
                                  issue_mentioned_unique_repo + 
                                  issue_mentioned_unique_family + 
                                  issue_mentioned_unique_outside + 
                                  pr_mentioned_unique_repo + 
                                  pr_mentioned_unique_family + 
                                  pr_mentioned_unique_outside + 
                                  issue_repo_pos + 
                                  issue_repo_neg + 
                                  issue_family_pos + 
                                  issue_family_neg + 
                                  issue_outside_pos + 
                                  issue_outside_neg + 
                                  pr_repo_pos + 
                                  pr_repo_neg + 
                                  pr_family_pos + 
                                  pr_family_neg + 
                                  pr_repo_code_pos + 
                                  pr_repo_code_neg + 
                                  pr_family_code_pos + 
                                  pr_family_code_neg
                                ,similarity="spearman", data=metrics, trans="abs")

#plot variables and threshold
threshold <- 0.7
plot(predictor_clustering)
abline(h = 1 - threshold, col="black", lwd=2, lty=2)
rect.hclust(predictor_clustering$hclust, h=1-threshold, border="red")

#re-scale metrics to compare coefficients
metrics$age <- rescale(metrics$age, to = c(0,1))
metrics$depth <- rescale(metrics$depth, to = c(0,1))
metrics$forks <- rescale(metrics$forks, to = c(0,1))
metrics$users_also_in_family <- rescale(metrics$users_also_in_family, to = c(0,1))
metrics$followers_repo <- rescale(metrics$followers_repo, to = c(0,1))
metrics$followers_family <- rescale(metrics$followers_family, to = c(0,1))
metrics$followers_outside <- rescale(metrics$followers_outside, to = c(0,1))
metrics$pr_repo <- rescale(metrics$pr_repo, to = c(0,1))
metrics$pr_family <- rescale(metrics$pr_family, to = c(0,1))
metrics$pr_repo_comments <- rescale(metrics$pr_repo_comments, to = c(0,1))
metrics$pr_family_comments <- rescale(metrics$pr_family_comments, to = c(0,1))
metrics$pr_repo_code_comments <- rescale(metrics$pr_repo_code_comments, to = c(0,1))
metrics$pr_family_code_comments <- rescale(metrics$pr_family_code_comments, to = c(0,1))
metrics$pr_repo_pos <- rescale(metrics$pr_repo_pos, to = c(0,1))
metrics$pr_repo_neg <- rescale(metrics$pr_repo_neg, to = c(0,1))
metrics$pr_family_pos <- rescale(metrics$pr_family_pos, to = c(0,1))
metrics$pr_family_neg <- rescale(metrics$pr_family_neg, to = c(0,1))
metrics$pr_family_code_neg <- rescale(metrics$pr_family_code_neg, to = c(0,1))
metrics$pr_mentioned_repo <- rescale(metrics$ pr_mentioned_repo, to = c(0,1))
metrics$pr_mentioned_family <- rescale(metrics$pr_mentioned_family, to = c(0,1))
metrics$pr_mentioned_outside <- rescale(metrics$pr_mentioned_outside, to = c(0,1))
metrics$issue_repo <- rescale(metrics$issue_repo, to = c(0,1)) 
metrics$issue_outside <- rescale(metrics$issue_outside, to = c(0,1)) 
metrics$issue_unsubscribed_repo <- rescale(metrics$issue_unsubscribed_repo, to = c(0,1))
metrics$issue_unsubscribed_family <- rescale(metrics$issue_unsubscribed_family, to = c(0,1))
metrics$issue_unsubscribed_outside <- rescale(metrics$issue_unsubscribed_outside, to = c(0,1))
metrics$issue_closed_family <- rescale(metrics$issue_closed_family, to = c(0,1))
metrics$issue_reopened_repo <- rescale(metrics$issue_reopened_repo, to = c(0,1))
metrics$issue_reopened_family <- rescale(metrics$issue_reopened_family, to = c(0,1))
metrics$issue_reopened_outside <- rescale(metrics$issue_reopened_outside, to = c(0,1))
metrics$issue_referenced_family <- rescale(metrics$issue_referenced_family, to = c(0,1))
metrics$issue_referenced_outside <- rescale(metrics$issue_referenced_outside, to = c(0,1))
metrics$issue_assigned_repo <- rescale(metrics$issue_assigned_repo, to = c(0,1))
metrics$issue_assigned_family <- rescale(metrics$issue_assigned_family, to = c(0,1))
metrics$issue_assigned_outside <- rescale(metrics$issue_assigned_outside, to = c(0,1))

#linear regression analysis with highly correlated metrics removed
linear_regression = lm(stars ~ 
                         age + 
                         depth + 
                         forks + 
                         #active_forks + 
                         #users_repo + 
                         users_also_in_family + 
                         followers_repo + 
                         followers_family + 
                         followers_outside + 
                         pr_repo + 
                         pr_family +
                         pr_repo_comments + 
                         pr_family_comments + 
                         pr_repo_code_comments + 
                         pr_family_code_comments + 
                         pr_repo_pos + 
                         pr_repo_neg + 
                         pr_family_pos + 
                         pr_family_neg +  
                         #pr_repo_code_pos + 
                         #pr_repo_code_neg + 
                         #pr_family_code_pos + 
                         pr_family_code_neg +     
                         pr_mentioned_repo + 
                         pr_mentioned_family + 
                         pr_mentioned_outside + 
                         #pr_mentioned_unique_repo + 
                         #pr_mentioned_unique_family + 
                         #pr_mentioned_unique_outside + 
                         issue_repo + 
                         #issue_family + 
                         issue_outside +                          
                         #issue_repo_pos + 
                         #issue_repo_neg + 
                         #issue_family_pos + 
                         #issue_family_neg + 
                         #issue_outside_pos + 
                         #issue_outside_neg + 
                         #issue_comments_repo + 
                         #issue_comments_family + 
                         #issue_comments_outside + 
                         #issue_mentioned_unique_repo + 
                         #issue_mentioned_unique_family + 
                       #issue_mentioned_unique_outside + 
                       #issue_mentioned_repo + 
                       #issue_mentioned_family + 
                       #issue_mentioned_outside +
                       #issue_subscribed_repo + 
                       #issue_subscribed_family + 
                       #issue_subscribed_outside + 
                       issue_unsubscribed_repo + 
                         issue_unsubscribed_family + 
                         issue_unsubscribed_outside + 
                         #issue_closed_repo + 
                         issue_closed_family + 
                         #issue_closed_outside + 
                         issue_reopened_repo + 
                         issue_reopened_family + 
                         issue_reopened_outside + 
                         #issue_referenced_repo + 
                         issue_referenced_family + 
                         issue_referenced_outside + 
                         issue_assigned_repo + 
                         issue_assigned_family + 
                         issue_assigned_outside 
                       , data = metrics)

#results from linear_regression
summary(linear_regression)

#relative importance; non-US version
relative_importance_pmvd <- calc.relimp(linear_regression, type="pmvd", rela=TRUE,
                                        groups=list(
                                          c("age","depth","forks"),
                                          c("followers_repo","pr_repo","pr_repo_comments","pr_repo_code_comments","pr_repo_pos","pr_repo_neg","issue_repo",
                                            "issue_unsubscribed_repo","issue_reopened_repo","issue_assigned_repo","pr_mentioned_repo"), 
                                          c("users_also_in_family","followers_family","pr_family","pr_family_comments","pr_family_code_comments","pr_family_pos",
                                            "pr_family_neg","pr_family_code_neg","issue_unsubscribed_family","issue_reopened_family","issue_referenced_family",
                                            "issue_assigned_family","pr_mentioned_family"), 
                                          c("followers_outside","issue_outside","issue_unsubscribed_outside","issue_closed_family","issue_reopened_outside",
                                            "issue_referenced_outside","issue_assigned_outside","pr_mentioned_outside")), 
                                        groupnames = c("Non-communicative","Repository", "Family", "Outside"))

#results from relative importance testing
relative_importance_pmvd$pmvd