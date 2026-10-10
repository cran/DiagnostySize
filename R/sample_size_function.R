## Function to calculate sample size in a comparative design

sample_size_calc<- function (alpha #significanceniveau
                             ,power
                             ,se.c #theta.sensitivity.comparator test
                             ,se.e #theta.sensitivity.experimental test
                             ,sp.c #theta.specificity.comparator test
                             ,sp.e #theta.specificity.experimental
                             ,prev #prevalence
                             ,ni.se =0 #limitvalue non-inferiority sensitivity
                             ,ni.sp = 0 #limitvalue non-inferiority specificity
                             ,psi.d= 0 #discordant test results diseased
                             ,psi.nd = 0 #disdordant test results non-diseased
                             ,paired #paired = True (paired design), paired = FALSE (unpaired design)
                             ,drop){

  #Variance function

  v <- function(theta.c, theta.e){

    variance <- theta.c * (1 - theta.c) + theta.e * (1 - theta.e)
    return(variance)
  }

#Numerator & denominator functions

# Superiority

  #unpaired-----------------------------

#Numerator function superiority unpaired
  num_sup_up <-function(alpha, theta.c, theta.e, beta){

    res <- stats::qnorm(alpha / 2) * sqrt(v(theta.c, theta.c)) + stats::qnorm(beta) * sqrt(v(theta.c, theta.e))
    return(abs(res))
  }

#denominator function superiority unpaired
  deno_sup_up <- function(theta.c, theta.e, prev){

    res <- (theta.c - theta.e) * sqrt(prev)
    return(abs(res))
  }

#paired----------------------------------

#Numerator function superiroity paired
  num_sup_p <- function(alpha, theta.c, theta.e, beta, psi){

    res <- ( stats::qnorm(1 - alpha / 2)* psi) +  (stats::qnorm(beta) * sqrt(psi ^2 - 0.25 *(theta.c - theta.e)^2 *(3 + psi)))
    return(abs(res))
  }
#Denomerator function superirotiy paired
  deno_sup_p <- function(theta.c, theta.e, psi, prev){

    res <- sqrt(psi) * (theta.c - theta.e) * sqrt(prev)
    return(abs(res))
  }
#Non-inferiority

  #unpaired-----------------------------

#Numerator function non-inferiority unpaired
  num_noninf_up <- function(alpha, theta.c, theta.e, beta){

    res <- (stats::qnorm(alpha / 2) + stats::qnorm(beta)) * sqrt(v(theta.c, theta.e))
    return(abs(res))
  }

#denominator function non-inferiority unpaired
  deno_noninf_up <- function(theta.c, theta.e, ni, prev){

    res <- (theta.c - theta.e - ni) * sqrt(prev)
    return(abs(res))
  }
  #paired---------------------------------

  #help function wu
  wu <- function(theta.c, theta.e, ni, psi){

    p01<- (psi - theta.e + theta.c) / 2
    au <- -(theta.e - theta.c) * (1 - ni) - 2 * (p01 + ni)
    bu <- ni * (1 + ni) * p01
    pu01 <- (-au + sqrt(au ^2 - 8 * bu)) / 4
    wu <- (sqrt(2 * p01 + (theta.e - theta.c) - (theta.e - theta.c) ^2)/ sqrt(2 * pu01 - ni - ni ^2))
    return(wu)
  }
#Numerator function non-inferiority paired
  num_noninf_p <- function(alpha, theta.c, theta.e, beta, psi, ni){

    res <- sqrt(psi - (theta.e - theta.c)^2) * (stats::qnorm(alpha/ 2) / wu(theta.c, theta.e, ni, psi) + stats::qnorm(beta))
    return(abs(res))
  }
#denominator function non-inferiority paired
deno_noninf_p <- function(theta.c, theta.e, ni, prev){

  res <- (- ni - (theta.e - theta.c)) * sqrt(prev)
  return(abs(res))
}
#f-functions (IF branches)
if (paired == TRUE){ # PAIRED
  if (ni.se > 0){  #Se NON-INFERIORITY
    if (ni.sp > 0){ # NON-INFERIORITY both endpoints paired
      #sample size calculation
        samplesize_se <- function(alpha, beta, theta.c, theta.e, ni, psi){

          n <- ceiling(((stats::qnorm(alpha/2) + stats::qnorm(beta))^2 * (psi- (theta.c - theta.e)^2)) / (theta.c - theta.e - ni)^2)
          return(n)
        }
        samplesize_sp <- samplesize_se
        #calculate power
        calculatepower_se <- function(n, alpha, theta.c, theta.e, ni, psi){

          z <- (sqrt(n)*(theta.c - theta.e - ni)) / sqrt(psi - (theta.c - theta.e)^2) - stats::qnorm(alpha/2)
          power <- 1-stats::pnorm(z)
          return(power)
        }
        calculatepower_sp <- calculatepower_se
      #function for optimal sample size
      f <- function(alpha, power, beta.se, se.c, se.e, sp.c, sp.e, prev, ni.se, ni.sp, psi.d, psi.nd){

          diff.n <- num_noninf_p(alpha = alpha, theta.c = se.c, theta.e =  se.e, beta =  beta.se,psi = psi.d, ni= ni.se) *
            deno_noninf_p(theta.c = sp.c, theta.e = sp.e, ni = ni.sp, prev=(1-prev)) -
            deno_noninf_p(theta.c =se.c,theta.e = se.e, ni= ni.se, prev= prev) *
            num_noninf_p(alpha = alpha, theta.e =sp.e,  theta.c = sp.c, beta= ((1-beta.se - power) / (1- beta.se)),psi= psi.nd,ni = ni.sp)
          return(diff.n)
        }
    }
    else { #ni.sp not >0 it must be 0 (<0 not allowed) Sp SUPERIORITY and Se NON-INFERIORITY paired
       #sample size calculation
      samplesize_se <- function(alpha, beta, theta.c, theta.e, ni, psi){

        n <- ceiling(((stats::qnorm(alpha/2) + stats::qnorm(beta))^2 * (psi- (theta.c - theta.e)^2)) / (theta.c - theta.e - ni)^2)
        return(n)
      }
      samplesize_sp <- function(alpha, beta, theta.c, theta.e, ni, psi){

          delta <- abs(theta.c-theta.e)
          n <- (stats::qnorm(1-alpha/2)*psi+stats::qnorm(1-beta)*sqrt((psi^2)-0.25*(delta^2)*(3+psi)))^2 /
            (psi*(delta^2))
          return(n)
        }
      # calculate power
      calculatepower_se <- function(n, alpha, theta.c, theta.e, ni, psi){

        z <- (sqrt(n)*(theta.c - theta.e - ni)) / sqrt(psi - (theta.c - theta.e)^2) - stats::qnorm(alpha/2)

        power <- 1-stats::pnorm(z)
        return(power)
      }
      calculatepower_sp <- function(n, alpha, theta.c, theta.e, ni, psi){
        delta <- abs(theta.c-theta.e)
        z <- (sqrt(n*psi)*delta - stats::qnorm(1-alpha/2)*psi)/sqrt((psi^2)-0.25*(delta^2)*(3+psi))
        power <- stats::pnorm(z)
        return(power)
      }
      # function for optimal sample size
        f <- function(alpha, power, beta.se, se.c, se.e, sp.c, sp.e, prev, ni.se, ni.sp, psi.d, psi.nd){

          diff.n <- num_noninf_p(alpha = alpha, theta.c = se.c, theta.e = se.e, beta = beta.se, psi= psi.d, ni= ni.se) *
            deno_sup_p(theta.c = sp.c, theta.e = sp.e, psi =  psi.nd, prev=(1-prev)) -
            deno_noninf_p(theta.c = se.c, theta.e = se.e, prev= prev, ni= ni.se) *
            num_sup_p(theta.e = sp.e, theta.c = sp.c, alpha = alpha, psi = psi.nd, beta =(power/ (1- beta.se)))
          return(diff.n)
          }
        }
  }
  else { #ni.se 0 = SUPERIORITY Se
    if (ni.sp > 0){ #sp NON-INFERIORITY & se SUPERIORITY paired
      #sample size calculation
      samplesize_se <- function(alpha, beta, theta.c, theta.e, ni, psi){

        delta <- abs(theta.c-theta.e)
        n <- (stats::qnorm(1-alpha/2)*psi+stats::qnorm(1-beta)*sqrt((psi^2)-0.25*(delta^2)*(3+psi)))^2 /
          (psi*(delta^2))
        return(n)
      }
      samplesize_sp <- function(alpha, beta, theta.c, theta.e, ni, psi){

          n <- ceiling(((stats::qnorm(alpha/2) + stats::qnorm(beta))^2 * (psi- (theta.c - theta.e)^2)) / (theta.c - theta.e - ni)^2)

      }
      #calculate power --------------------------!
      calculatepower_se <- function(n, alpha, theta.c, theta.e, ni, psi){

        delta <- abs(theta.c-theta.e)
        z <- (sqrt(n*psi)*delta - stats::qnorm(1-alpha/2)*psi)/sqrt((psi^2)-0.25*(delta^2)*(3+psi))
        power <- stats::pnorm(z)
        return(power)
      }
      calculatepower_sp <- function(n, alpha, theta.c, theta.e, ni, psi){

        z <- (sqrt(n)*(theta.c - theta.e - ni)) / sqrt(psi - (theta.c - theta.e)^2) - stats::qnorm(alpha/2)
        power <- 1-stats::pnorm(z)
        return(power)
      }
      #function for optimal sample size
      f <- function(alpha, power, beta.se, se.c, se.e, sp.c, sp.e, prev, ni.se, ni.sp, psi.d, psi.nd){

        diff.n <- num_sup_p(alpha = alpha, theta.c = se.c, theta.e =  se.e, beta= beta.se,psi= psi.d) *
          deno_noninf_p(theta.c =sp.c, theta.e =  sp.e, ni= ni.sp, prev =(1-prev)) -
          deno_sup_p(theta.c = se.c, theta.e = se.e, prev = prev, psi= psi.d) *
          num_noninf_p(alpha = alpha, theta.e = sp.e, theta.c = sp.c, ni= ni.sp, psi = psi.nd, beta =((1- beta.se - power)/(1- beta.se)))
        return(diff.n)
      }
    } else { # ni.sp 0 = SUPERORITY (both endpoints) paired
      #sample size calculation
      samplesize_se <- function(alpha, beta, theta.c, theta.e, ni, psi){

        delta <- abs(theta.c-theta.e)
        n <- (stats::qnorm(1-alpha/2)*psi+stats::qnorm(1-beta)*sqrt((psi^2)-0.25*(delta^2)*(3+psi)))^2 /
          (psi*(delta^2))
        return(n)
      }
      samplesize_sp <- samplesize_se
      #calculate power
      calculatepower_se <- function(n, alpha, theta.c, theta.e, ni, psi){

        delta <- abs(theta.c-theta.e)
        z <- (sqrt(n*psi)*delta - stats::qnorm(1-alpha/2)*psi)/sqrt((psi^2)-0.25*(delta^2)*(3+psi))
        power <- stats::pnorm(z)
        return(power)
      }
      calculatepower_sp <- calculatepower_se
      #function for optimal sample size
      f <- function(alpha, power, beta.se, se.c, se.e, sp.c, sp.e, prev, ni.se, ni.sp, psi.d, psi.nd){

        diff.n <- num_sup_p(alpha = alpha, theta.c = se.c, theta.e =  se.e, beta =(1- beta.se), psi= psi.d) *
          deno_sup_p(theta.c = sp.c, theta.e =sp.e,psi = psi.nd, prev= (1-prev)) -
          deno_sup_p(theta.c = se.c, theta.e = se.e,psi = psi.d, prev = prev) *
          num_sup_p(alpha = alpha, theta.c = sp.c, theta.e =  sp.e,  beta= (power / (1 - beta.se)), psi =psi.nd)


        return(diff.n)
      }
    }
  }
}
else { #UNPAIRED
  if (ni.se > 0){  #Se NON-INFERIORITY
    if (ni.sp > 0){ # NON-INFERIORITY both endpoints unpaired
      #sample size calculation
      samplesize_se <- function(alpha, beta, theta.c, theta.e, ni, psi){

        n <- ceiling(((stats::qnorm(alpha/2) + stats::qnorm(beta))^2 * v(theta.c, theta.e)) / (theta.c - theta.e - ni)^2)
      }
      samplesize_sp <- samplesize_se
      #calculate power
      calculatepower_se <- function(n, alpha, theta.c, theta.e, ni, psi){

        z <- (sqrt(n)*(theta.c - theta.e - ni)) / sqrt(v(theta.c, theta.e)) - stats::qnorm(alpha/2)
        power <- 1-stats::pnorm(z)
        return(power)
      }
      calculatepower_sp <- calculatepower_se
      #function for optimal sample size
      f <- function(alpha, power, beta.se, se.c, se.e, sp.c, sp.e, prev, ni.se, ni.sp, psi.d, psi.nd){

        diff.n <- num_noninf_up(alpha =alpha, theta.c = se.c, theta.e = se.e, beta =beta.se) *
          deno_noninf_up(theta.c = sp.c, theta.e = sp.e, ni=  ni.sp, prev = (1-prev)) -
          deno_noninf_up(theta.c = se.c, theta.e = se.e, ni= ni.se, prev= prev) *
          num_noninf_up(alpha = alpha, theta.e = sp.e, theta.c = sp.c, beta = ((1 - beta.se - power)/ (1- beta.se)))
        return(diff.n)
      }
    }
    else { #ni.sp not >0 it must be 0 (<0 not allowed) Sp SUPERIORITY and Se NON-INFERIORITY unpaired
      #sample size calculation
      samplesize_se <- function(alpha, beta, theta.c, theta.e, ni, psi){

        n <- ceiling(((stats::qnorm(alpha/2) + stats::qnorm(beta))^2 * v(theta.c, theta.e)) / (theta.c - theta.e - ni)^2)
      }
      samplesize_sp <- function(alpha, beta, theta.c, theta.e, ni, psi){

        n <- ceiling((stats::qnorm(alpha / 2) * sqrt(v(theta.c, theta.c)) + stats::qnorm(beta) * sqrt(v(theta.c, theta.e))) ^
                       2 / (theta.c - theta.e) ^ 2)
      }
      #calculate power
      calculatepower_se <- function(n, alpha, theta.c, theta.e, ni, psi){

        z <- (sqrt(n)*(theta.c - theta.e - ni)) / sqrt(v(theta.c, theta.e)) - stats::qnorm(alpha/2)
        power <- 1-stats::pnorm(z)
        return(power)
      }
      calculatepower_sp <- function(n, alpha, theta.c, theta.e, ni, psi){
        z <- (sqrt(n) * (theta.c - theta.e) - stats::qnorm(alpha / 2) * sqrt(v(theta.c, theta.c))) / sqrt(v(theta.c,theta.e))
        power <- 1 - stats::pnorm(z)
        return(power)
      }
      #function for optimal sample size
      f <- function(alpha, power, beta.se, se.c, se.e, sp.c, sp.e, prev, ni.se, ni.sp, psi.d, psi.nd){

        diff.n <- num_noninf_up(alpha= alpha, theta.e = se.e, theta.c = se.c, beta = beta.se) *
          deno_sup_up(theta.c = sp.c, theta.e = sp.e, prev= (1-prev))-
          deno_noninf_up(theta.c = se.c, theta.e = se.e, prev= prev, ni= ni.se) *
          num_sup_up(theta.e = sp.e, theta.c = sp.c, alpha = alpha, beta = ((1 - beta.se - power)/(1- beta.se)))
        return(diff.n)
      }
    }
  }
  else { #ni.se 0 = SUPERIORITY Se
    if (ni.sp > 0){ #sp NON-INFERIORITY & se SUPERIORITY unpaired
      #sample size
      samplesize_se <- function(alpha, beta, theta.c, theta.e, ni, psi){

        n <- ceiling((stats::qnorm(alpha / 2) * sqrt(v(theta.c, theta.c)) + stats::qnorm(beta) * sqrt(v(theta.c, theta.e))) ^
                       2 / (theta.c - theta.e) ^ 2)
      }
      samplesize_sp <-  function(alpha, beta, theta.c, theta.e, ni, psi){

        n <- ceiling(((stats::qnorm(alpha/2) + stats::qnorm(beta))^2 * v(theta.c, theta.e)) / (theta.c - theta.e - ni)^2)
      }
      #calculate power
      calculatepower_se <- function(n, alpha, theta.c, theta.e, ni, psi){
        z <- (sqrt(n) * (theta.c - theta.e) - stats::qnorm(alpha / 2) * sqrt(v(theta.c, theta.c))) / sqrt(v(theta.c,theta.e))
        power <- 1 - stats::pnorm(z)
        return(power)
      }
      calculatepower_sp <- function(n, alpha, theta.c, theta.e, ni, psi){

        z <- (sqrt(n)*(theta.c - theta.e - ni)) / sqrt(v(theta.c, theta.e)) - stats::qnorm(alpha/2)
        power <- 1-stats::pnorm(z)
        return(power)
      }
      #function for optimal sample size
      f <- function(alpha, power, beta.se, se.c, se.e, sp.c, sp.e, prev, ni.se, ni.sp, psi.d, psi.nd){

        diff.n <- num_sup_up(alpha = alpha, theta.c = se.c, theta.e = se.e, beta =beta.se) *
          deno_noninf_up(theta.c = sp.c, theta.e =sp.e, ni =ni.sp, prev= (1-prev)) -
          deno_sup_up(theta.c = se.c, theta.e = se.e, prev = prev) *
          num_noninf_up(alpha = alpha, theta.e = sp.e, theta.c = sp.c, beta =((1- beta.se -power)/(1- beta.se)))
        return(diff.n)

      }
    } else { # ni.sp 0 = SUPERORITY (both endpoints) unpaired
      #sample size calculation
      samplesize_se <- function(alpha, beta, theta.c, theta.e, ni, psi){

        n <- ceiling((stats::qnorm(alpha / 2) * sqrt(v(theta.c, theta.c)) + stats::qnorm(beta) * sqrt(v(theta.c, theta.e))) ^
                       2 / (theta.c - theta.e) ^ 2)
      }
      samplesize_sp <- samplesize_se
      #calculate power
      calculatepower_se <- function(n, alpha, theta.c, theta.e, ni, psi){

        z <- (sqrt(n) * (theta.c - theta.e) - stats::qnorm(alpha / 2) * sqrt(v(theta.c, theta.c))) / sqrt(v(theta.c,theta.e))
        power <- 1 - stats::pnorm(z)
        return(power)
      }
      calculatepower_sp <- calculatepower_se
      # function for opitmal sample size
      f <- function(alpha, power, beta.se, se.c, se.e, sp.c, sp.e, prev, ni.se, ni.sp, psi.d, psi.nd){

        diff.n <-  num_sup_up(alpha = alpha, theta.c = se.c, theta.e = se.e, beta =beta.se) *
          deno_sup_up( theta.c = sp.c, theta.e =  sp.e, prev = (1-prev)) -
          num_sup_up(alpha = alpha,  theta.c = sp.c, theta.e = sp.e, beta = (1- beta.se - power) / (1- beta.se)) *
          deno_sup_up(theta.c =se.c, theta.e = se.e, prev = prev)
        return(diff.n)
      }
    }
  }

}


# approximate the sample size for beta.se and then calculate beta.sp
beta.se <- stats::uniroot(f, alpha = alpha, power = power, se.c = se.c, se.e = se.e,
                   sp.c = sp.c, sp.e = sp.e, prev = prev, psi.d = psi.d, psi.nd = psi.nd,
                   ni.se = ni.se, ni.sp = ni.sp,
                   lower = 0, upper = 1-power)$root

beta.sp <- (power+beta.se-1)/(beta.se-1)
power.total <- (1-beta.se)*(1-beta.sp)


# calculate the sample size with beta.se and beta.sp
n.se <- samplesize_se(alpha = alpha, beta = beta.se, theta.c = se.c, theta.e = se.e, ni = ni.se, psi = psi.d)
n.sp <- samplesize_sp(alpha = alpha, beta = beta.sp, theta.c = sp.c, theta.e = sp.e, ni = ni.sp, psi = psi.nd)
N.se <- n.se / prev #sample size endpoint sensitivity
N.sp <- n.sp / (1 - prev) #sample size specificity
N <- ceiling(max(N.se, N.sp)) # total sample size

#Dropout
N_drop_se <- ceiling(N.se / (1-drop)) #sample size sensitivity with dropout
N_drop_sp <- ceiling(N.sp /(1-drop))  #sample size specificity with dropout
N_drop <-max(N_drop_se, N_drop_sp) #total sample size with dropout

# calculate total power
power.se <- calculatepower_se(n = N * prev, alpha = alpha, theta.c = se.c, theta.e =
                                       se.e, ni = ni.se, psi = psi.d)
power.sp <- calculatepower_sp(n = N * (1 - prev), alpha = alpha, theta.c = sp.c, theta.e
                                     = sp.e, ni = ni.sp, psi = psi.nd)
power.total <- power.se * power.sp

#return list

return(list("total sample size (N)"= N_drop, "total sample size to reach sensitivity of the comparator test (N.se)"= N_drop_se,
            "total sample size to reach specificity of the comparator test (N.sp)"= N_drop_sp,
            "number of study participants with target condition (n.se)"= ceiling(n.se),
            "number of study participants without target condition(n.sp)"= ceiling(n.sp),
            "power.total "= round(power.total, digits = 2), "power.sensitivity"= round(power.se, digits = 2),
            "power.specificity"= round(power.sp, digits = 2), "beta.1 type II error " = round(beta.se,digits = 2),
            "beta.2 type II error " = round(beta.sp, digits = 2)))

}
#example call of function
sample_size_calc (alpha = 0.05
                  ,power = 0.8
                  ,se.c = 0.75
                  ,se.e = 0.8
                  ,sp.c = 0.6
                  ,sp.e = 0.66
                  ,prev = 0.5
                  ,ni.se =0.02
                  ,ni.sp =0.02
                  ,psi.d= 0.15
                  ,psi.nd = 0.2
                  ,paired = TRUE
                  ,drop = 0)

sample_size_calc(alpha = 0.05,
                 power = 0.8,
                 ni.se = 0.05,
                 ni.sp = 0.05,
                 se.c = 0.75,
                 se.e = 0.78,
                sp.c = 0.6,
                sp.e = 0.66,
                prev = 0.5,
                paired = FALSE,
                drop= 0)
