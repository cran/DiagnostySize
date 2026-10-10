# sample size calculation without overpowering, This code is from Maria Stark (https://doi.org/10.1177/0962280220913, private communication) and was adapted for the app.

################################################################################################################################################
#################################################### exact calculation #########################################################################
################################################################################################################################################

ss.exact <- function(alpha, power, se, se.ni, sp, sp.ni, prev, drop) {
  # help functions
    # the function v estimates the variance of the sensitivity based on the binomial distribution, theta = sensitivity
    v = function(theta){
      return(theta*(1-theta))
    }

    # sample size calculation following Obuchowski
    single = function(alpha, beta, theta.0, theta){
      n = ceiling((stats::qnorm(alpha/2)*sqrt(v(theta.0))+stats::qnorm(beta)*sqrt(v(theta)))^2 / (theta.0-theta)^2)
      return(n)
    }

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

  # function for the equal sample size for both endpoints
  f <- function(alpha, power, beta.1, se, se.ni, sp, sp.ni, prev) {

  diff.n <- stats::qnorm(beta.1)*sqrt(v(se))*(sp.ni-sp)*sqrt(1-prev) - stats::qnorm(1-(power/(1-beta.1)))*sqrt(v(sp))*(se.ni-se)*sqrt(prev) -
    stats::qnorm(alpha/2)*sqrt(v(sp.ni))*(se.ni-se)*sqrt(prev) + stats::qnorm(alpha/2)*sqrt(v(se.ni))*(sp.ni-sp)*sqrt(1-prev)

  return(diff.n)
  }

  # solve the sample size for beta.1 and then calculate beta.2
  beta.1 <- stats::uniroot(f, alpha = alpha, power = power, se = se, se.ni = se.ni,
                    sp = sp, sp.ni = sp.ni, prev = prev, lower = 0, upper = 1-power)$root
  beta.2 <- (power+beta.1-1)/(beta.1-1)

  #power.total <- (1-beta.1)*(1-beta.2)

  # calculate sample size with the known beta.1 and beta.2
  n.se <- single(alpha = alpha, beta = beta.1, theta.0 = se.ni, theta = se) #sample size sensitivity
  n.sp <- single(alpha = alpha, beta = beta.2, theta.0 = sp.ni, theta = sp) #sample size specificity
  N.se <- n.se/prev
  N.sp <- n.sp/(1-prev)
  N <- ceiling(max(N.se, N.sp)) #total sample size

  # calculate total power
  power.se <- calculate.power(n = N*prev, alpha = alpha, theta.0 = se.ni, theta = se)
  power.sp <- calculate.power(n = N*(1-prev), alpha = alpha, theta.0 = sp.ni, theta = sp)
  power.total <- power.se * power.sp

  #Janas changes: Dropout
  N_drop_se <- ceiling(N.se / (1-drop)) #sample size sensitivity with dropout
  N_drop_sp <- ceiling(N.sp /(1-drop))  #sample size specificity with dropout
  N_drop <-max(N_drop_sp, N_drop_se) #total sample size with dropout




  # Janas changes: testers wanted a better naming for the output variables and round() and ceiling() functions for the results
  return(list("total sample size (N)"= N_drop, "total sample size to reach the minimum sensitivity (N.se)"= N_drop_se,
              "total sample size to reach the minimum specificity (N.sp)"= N_drop_sp,
              "number of study participants with target condition (n.se)"= ceiling(n.se),
              "number of study participants without target condition(n.sp)"= ceiling(n.sp),
              "power.total "= round(power.total, digits = 2),
              "power.sensitivity"= round(power.se, digits = 2),
             "power.specificity"= round(power.sp, digits = 2)))
  # Marias Original output list: (changes end!-------------------)
 # return(list(N = N, N.se = N.se, N.sp = N.sp, n.se = n.se, n.sp = n.sp, power.total = power.total,digits = 2, power.se = power.se, power.sp = power.sp))

}

#example function call
ss.exact(alpha = 0.05, power = 0.8, se = 0.81, se.ni = 0.75, sp = 0.66, sp.ni = 0.6, prev = 0.054, drop = 0.1)
ss.exact(alpha = 0.05, power = 0.8, se = 0.81, se.ni = 0.75, sp = 0.66, sp.ni = 0.6, prev = 0.46, drop = 0)




