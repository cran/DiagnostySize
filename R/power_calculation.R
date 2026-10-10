##Power calculation 'code-snippits' from sample size calculation skripts written by stark et al.
# single-arm design
# calculating power

#new function name and new defined inputs
exact_power <- function(N, alpha, se, se_ni, sp, sp_ni, prev ,drop) {#input parameters
  # calculate power (formel of Obuchowski reversed)
  calculate.power <- function(n, alpha, theta.0, theta) {

    # the function v estimates the variance of the sensitivity based on the binomial distribution, theta = sensitivity
    v = function(theta){
      return(theta*(1-theta))
    }

    z <- (sqrt(n)*(theta.0-theta) - stats::qnorm(alpha/2)*sqrt(v(theta.0))) / sqrt(v(theta))
    power <- 1-stats::pnorm(z)
    return(power)
  }
  # #Dropout
  N_drop <- N * (1- drop) #calculate sample size after drop out

  power.se <- calculate.power(n = N_drop* prev,alpha = alpha, theta.0 = se_ni, theta = se)
  power.sp <- calculate.power(n = N_drop * (1 - prev),alpha = alpha, theta.0 = sp_ni, theta = sp)
  power.total <- power.se * power.sp


 #round functions added
  return(list( power.total = round(power.total, digits = 2), power.sensitivity = round(power.se, digits = 2), power.specificity = round(power.sp, digits = 2)))
}
exact_power(alpha = 0.05, N =1000 , se = 0.8, se_ni = 0.75, sp = 0.66, sp_ni = 0.6, prev = 0.5, drop = 0 )

