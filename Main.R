source("Model.R")
total_score = 25
score_supremacy = 4
margin = 1.04

odds = GetOddsFromExpectancies(total_score = total_score,
                               score_supremacy = score_supremacy,
                               margin = margin,
                               par = par)
