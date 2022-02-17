library(data.table)
par.csv = fread("parameters.csv")
GetTeamParameter = function(par.csv, team_name){
  p = par.csv[team==team_name]
  list(mu=p[parameter %in% c("kick","try", "conv")],
       N = p[parameter== "N",mean])
}

par = list(home=GetTeamParameter(par.csv, "home"), 
           away=GetTeamParameter(par.csv, "away"))

print("The parameters are loaded as the object par.")
print("For explanation of the model and the parameters, see nfl.html!")
GetScoreFromParameter = function(mu) {
  mu[1]*2 + mu[2] * (5 + 2* mu[3])
}

GetParameterFromExpectancy = function(mu, sigma, score){
  a = 2 * sigma[2] * sigma[3]
  b = 2 * mu[2] * sigma[3] + sigma[2] * (5 + 2 * mu[3]) + 2 * sigma[1]
  c = GetScoreFromParameter(mu) - score
  r = (-b + sqrt(b*b - 4*a*c))/2/a
  mu + r * sigma
}

print("Testing parameter function...")
mu = par[["away"]]$mu$mean
sigma = par[["away"]][["mu"]]$std
for(trials in 1:100){
  score0 = runif(1, 5,15)
  score = GetScoreFromParameter(GetParameterFromExpectancy(mu,sigma,score0))
  if(abs(score-score0)>1e-3){
    print(paste0("Parameter fitting function failed at ", score0))
    break
  }
}

GetTryAndConvScoreDistFromPar = function(mu, N) {
  output = rep(0,71)
  try_dist = dbinom(0:N, size=N,prob=mu[2]/N)
  output[1] = try_dist[1]
  for(i in 1:N){
    output[(0:i) * 2 + i* 5 + 1] = output[(0:i) * 2 + i* 5 + 1] + 
      try_dist[i+1] * dbinom(0:i, size=i,prob=mu[3])
  }
  output
}

GetScoreDistFromPar = function(mu, N){
  try_conv_dist = GetTryAndConvScoreDistFromPar(mu, N)
  kick_dist = dbinom(0:N, size=N, prob=mu[1]/N)
  kick_parsed = rep(0,71)
  kick_parsed[(0:N)*2+1] = kick_dist
  output = convolve(kick_parsed, rev(try_conv_dist),type="o")
  output[output < 1e-17] = 0
  output[1:71]
}

print("Testing dist function...")
mu = par[["away"]]$mu$mean
sigma = par[["away"]]$mu$std
N = par[["away"]]$N
for(trials in 1:100){
  score0 = runif(1, 5,15)
  mu0 = GetParameterFromExpectancy(mu,sigma,score0)
  p = GetScoreDistFromPar(mu0, N)
  score = sum(p* (0:70))
  if(abs(score-score0)>1e-3){
    print(paste0("Parameter fitting function failed at ", score0))
    break
  }
}

GetAggregateScoreDistFromExpectancies = function(total_score, 
                                                 score_supremacy, 
                                                 par){
  score_home = (total_score + score_supremacy) / 2
  score_away = (total_score - score_supremacy) / 2
  mu_home = par[["home"]]$mu$mean
  sigma_home = par[["home"]]$mu$std
  N_home = par[["home"]]$N
  mu_home_0 = GetParameterFromExpectancy(mu_home,sigma_home,score_home)
  p_home = GetScoreDistFromPar(mu_home_0, N_home)
  mu_away = par[["away"]]$mu$mean
  sigma_away = par[["away"]]$mu$std
  N_away = par[["away"]]$N
  mu_away_0 = GetParameterFromExpectancy(mu_away,sigma_away,score_away)
  p_away = GetScoreDistFromPar(mu_away_0, N_away)
  p_home %o% p_away
}

GetOddsFromExpectancies = function(total_score, 
                                   score_supremacy, 
                                   margin, 
                                   par){
  p_all = GetAggregateScoreDistFromExpectancies(total_score, score_supremacy, par)
  home_prob = sum(p_all[lower.tri(p_all, diag=FALSE)])
  away_prob = sum(p_all[upper.tri(p_all,diag=FALSE)])
  return(list(home_odds = (home_prob + away_prob)/home_prob/margin,
              away_odds = (home_prob + away_prob)/away_prob/margin))
}

GetOddsFromExpectancies(total_score = 25, 
                        score_supremacy = 4, 
                        margin = 1.04,
                        par)
