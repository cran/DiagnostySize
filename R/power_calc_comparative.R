#Function to calculate power in a comparative design

power_calc <- function(alpha #significanceniveau
                       ,N #sample size
                       ,se.c #theta.sensitivity.comparator test
                       ,se.e #theta.sensitivity.experimental test
                       ,sp.c #theta.specificity.comparator test
                       ,sp.e #theta.specificity.experimental
                       ,prev #prevalence
                       ,ni.se =0 #limitvalue non-inferiority sensitivity
                       ,ni.sp = 0 #limitvalue non-inferiority specificity
                       ,psi.d= 0 #discordant test results diseased
                       ,psi.nd = 0 #disdordant test results non-diseased
                       ,paired #){#paired = True (paired design), paired = FALSE (unpaired design)
                       ,drop){


  #Variance function
  v <- function(theta.c, theta.e) {

    variance <- theta.c * (1 - theta.c) + theta.e * (1 - theta.e)
    return(variance)
  }

  #z-functions (IF branches)
  if (paired == TRUE){ # PAIRED
    if (ni.se > 0){  #Se NON-INFERIORITY
      if (ni.sp > 0) { # NON-INFERIORITY both endpoints paired
        # Error message ->MOVE TO PLAUSIBILITY CHECKS
        if( se.c - se.e >= ni.se) {stop("it must hold: se.c - se.e < ni.se")} #H0 wenn das der Fall ist geht non inferiority nicht
        if (sp.c - sp.e >= ni.sp) {stop("it must hold: sp.c - sp.e < ni.sp")}

        #calculate power
        calculatepower_se <- function(n, alpha, theta.c, theta.e, ni, psi){

          z <- (sqrt(n)*(theta.c - theta.e - ni)) / sqrt(psi - (theta.c - theta.e)^2) - stats::qnorm(alpha)
          power <- 1-stats::pnorm(z)
          return(power)
        }
        calculatepower_sp <- calculatepower_se

      }#sp non-inf close
      else { #ni.sp not >0 it must be 0 (<0 not allowed) Sp SUPERIORITY and Se NON-INFERIORITY paired

        # calculate power
        calculatepower_se <- function(n, alpha, theta.c, theta.e, ni, psi) {

          z <- (sqrt(n)*(theta.c - theta.e - ni)) / sqrt(psi - (theta.c - theta.e)^2) - stats::qnorm(alpha)
          power <- 1-stats::pnorm(z)
          return(power)
        }
        calculatepower_sp <- function(n, alpha, theta.c, theta.e, ni, psi) {
          delta <- abs(theta.c-theta.e)
          z <- (sqrt(n*psi)*delta - stats::qnorm(1-alpha/2)*psi)/sqrt((psi^2)-0.25*(delta^2)*(3+psi))
          power <- stats::pnorm(z)
          return(power)
        }
      }#sp sup close
    }#SE non-inf bracket close
    else { #ni.se 0 = SUPERIORITY Se
      if (ni.sp > 0){ #sp NON-INFERIORITY & se SUPERIORITY paired

        #calculate power
        calculatepower_se <- function(n, alpha, theta.c, theta.e, ni, psi){

          delta <- abs(theta.c-theta.e)
          z <- (sqrt(n*psi)*delta - stats::qnorm(1-alpha/2)*psi)/sqrt((psi^2)-0.25*(delta^2)*(3+psi))
          power <- stats::pnorm(z)
          return(n)
        }
        calculatepower_sp <- function(n, alpha, theta.c, theta.e, ni, psi) {

          z <- (sqrt(n)*(theta.c - theta.e - ni)) / sqrt(psi - (theta.c - theta.e)^2) - stats::qnorm(alpha)
          power <- 1-stats::pnorm(z)
          return(power)
        }

      } else { # ni.sp 0 = SUPERORITY (both endpoints) paired

        #calculate power
        calculatepower_se <- function(n, alpha, theta.c, theta.e, ni, psi){

          delta <- abs(theta.c-theta.e)
          z <- (sqrt(n*psi)*delta - stats::qnorm(1-alpha/2)*psi)/sqrt((psi^2)-0.25*(delta^2)*(3+psi))
          power <- stats::pnorm(z)
          return(power)
        }
        calculatepower_sp <- calculatepower_se
      }
  }# se Sup close
}# bracket close paired
  else { #UNPAIRED
    if (ni.se > 0){  #Se NON-INFERIORITY
      if (ni.sp > 0) { # NON-INFERIORITY both endpoints unpaired

        #calculate power
        calculatepower_se <- function(n, alpha, theta.c, theta.e, ni, psi) {

          z <- (sqrt(n)*(theta.c - theta.e - ni)) / sqrt(v(theta.c, theta.e)) - stats::qnorm(alpha)
          power <- 1-stats::pnorm(z)
          return(power)
        }
        calculatepower_sp <- calculatepower_se

      }
      else { #ni.sp not >0 it must be 0 (<0 not allowed) Sp SUPERIORITY and Se NON-INFERIORITY unpaired

        #calculate power
        calculatepower_se <- function(n, alpha, theta.c, theta.e, ni, psi) {

          z <- (sqrt(n)*(theta.c - theta.e - ni)) / sqrt(v(theta.c, theta.e)) - stats::qnorm(alpha)
          power <- 1-stats::pnorm(z)
          return(power)
        }
        calculatepower_sp <- function(n, alpha, theta.c, theta.e, ni, psi) {
          z <- (sqrt(n) * (theta.c - theta.e) - stats::qnorm(alpha / 2) * sqrt(v(theta.c, theta.c))) / sqrt(v(theta.c,theta.e))
          power <- 1 - stats::pnorm(z)
          return(power)
        }
      }
    }
    else { #ni.se 0 = SUPERIORITY Se
      if (ni.sp > 0){ #sp NON-INFERIORITY & se SUPERIORITY unpaired

        #calculate power
        calculatepower_se <- function(n, alpha, theta.c, theta.e, ni, psi) {
          z <- (sqrt(n) * (theta.c - theta.e) - stats::qnorm(alpha / 2) * sqrt(v(theta.c, theta.c))) / sqrt(v(theta.c,theta.e))
          power <- 1 - stats::pnorm(z)
          return(power)
        }
        calculatepower_sp <- function(n, alpha, theta.c, theta.e, ni, psi) {

          z <- (sqrt(n)*(theta.c - theta.e - ni)) / sqrt(v(theta.c, theta.e)) - stats::qnorm(alpha)
          power <- 1-stats::pnorm(z)
          return(power)
        }

      } else { # ni.sp 0 = SUPERORITY (both endpoints) unpaired
        #calculate power
        calculatepower_se <- function(n, alpha, theta.c, theta.e, ni, psi){

          z <- (sqrt(n) * (theta.c - theta.e) - stats::qnorm(alpha / 2) * sqrt(v(theta.c, theta.c))) / sqrt(v(theta.c,theta.e))
          power <- 1 - stats::pnorm(z)
          return(power)
        }
        calculatepower_sp <- calculatepower_se
      }
    }

  }
  #Dropout
   N_drop <- N * (1- drop)

  # calculate total power
  power.se <- calculatepower_se(n = N_drop * prev, alpha = alpha, theta.c = se.c, theta.e =
                                  se.e, ni = ni.se, psi = psi.d)
  power.sp <- calculatepower_sp(n = N_drop * (1 - prev), alpha = alpha, theta.c = sp.c, theta.e
                                = sp.e, ni = ni.sp, psi = psi.nd)
  power.total <- power.se * power.sp



  return(list("power.total "= round(power.total, digits = 2), "power.sensitivity"= round(power.se, digits = 2),
              "power.specificity"= round(power.sp, digits = 2)))

}
power_calc(alpha = 0.05
           ,N = 1000
           ,se.c = 0.75
           ,se.e = 0.8
           ,sp.c = 0.6
           ,sp.e = 0.66
           ,prev = 0.5
           ,ni.se =0
           ,ni.sp = 0
           ,psi.d= 0
           ,psi.nd = 0
           ,paired = FALSE
           ,drop = 0)
