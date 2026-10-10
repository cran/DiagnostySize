##Plausibility checks for input values
#sample size calculation #######################################################################################
# single
plausibility_single <- function(prev, power, alpha, se_ni, se, sp, sp_ni, drop ){


  if(prev <= 0|| prev >= 1){
    shiny::validate( "The prevalence input must be between 0 and 1.")
  }
  if( power <= 0||power >=1){
    shiny::validate("The power input must be between 0 and 1." )
  }
  if( alpha <= 0||alpha >=1 ) {
    shiny::validate("The alpha input must be between 0 and 1.")
  }
  if ( se_ni <= 0||se_ni >=1 ) {
    shiny::validate("The minimum sensitvity must be between 0 and 1.")
  }
  if ( se <= 0||se >=1 || se <= se_ni){         #ni = non-inferiority (minimum value) se= sensitivity new test. new test value must be higher than minimum value
    shiny::validate("The sensitivity of the new test must be between 0 and 1.
             Also the sensitivity of the new test must be higher than the minimum sensitivity.")
  }
  if ( sp <= 0||sp >= 1 || sp <= sp_ni){
    shiny::validate("The specificity of the new test must be between 0 and 1.
             Also the specificity of the new test must be higher than the minimum specificity." )
  }
  if ( sp_ni <= 0||sp_ni >=1){
    shiny::validate("The minimum specificity must be between 0 and 1.")
  }
  if ( drop < 0|| drop >=1 ){
    shiny::validate("The dropout rate must be 0 or between 0 and 1.")
  }
}
#unpaired
plausibility_unpaired <- function(prev, power, alpha, se.e, sp.e, sp.c, se.c, ni.se, ni.sp, drop){

  # noninf
  # variable to check- if non-inf
  is_ni_se <- !is.null(ni.se) && ni.se > 0 # noninf for sensitivity
  is_ni_sp <- !is.null(ni.sp) && ni.sp > 0 # noninf for specificity
  # noninf se
  if (is_ni_se){
    if( se.c - se.e > ni.se) {
      shiny::validate("Testing for non-inferiority: The non-inferiority limit must be greater than the difference between comparator test and experimental(se.c - se.e)") #if limit is not greater than difference non-inferiority is not given
  }}
  else {
     if(se.e < (se.c - ni.se)){#superiority
       shiny::validate("The sensitivity of the new test must be higher than the comparator test")
     }
  }

  #noninf sp
  if (is_ni_sp){
    if (sp.c -sp.e > ni.sp) {
      shiny::validate("Testing for non-inferiority: The non-inferiority limit must be greater than the difference between comparator test and experimental(se.c - se.e)")
    }}
  else {
      if(sp.e < (sp.c - ni.sp)){#superiority
        shiny::validate("The specificity of the new test must be higher than the comparator test")
      }
    }

     #if (sp.c - sp.e >= ni.sp) {
     # shiny::validate("Testing for superiority: The experimental test value must be greater than the comparator test value. Testing for non-inferiority: The non-inferiority limit must be greater than the difference between comparator test and experimental(sp.c - sp.e)")}
  #}

  if (prev <= 0 || prev >= 1 ){
    shiny::validate("The prevalence input must be between 0 and 1.")
  }
  if (power <= 0 ||  power >=1 ){
    shiny::validate("The power input must be between 0 and 1.")
  }
  if (alpha <= 0 || alpha >= 1){
    shiny::validate("The alpha input must be between 0 and 1.")
  }
  if (se.e <= 0 || se.e >= 1  ){
    shiny::validate("The sensitivity of the new test must be between 0 and 1.")

  }
  if (sp.e <=0 || sp.e >= 1 ){
    shiny::validate("The specificity of the new test must be between 0 and 1.")

  }
  if (se.c <= 0 || se.c >=1){
    shiny::validate("The sensitvity of the comparator test must be between 0 and 1.")
  }
  if (sp.c <= 0 || sp.c >= 1){
    shiny::validate("The specificity of the comparator test input must be between 0 and 1.")
  }
  if ( drop <0|| drop >=1 ){
    shiny::validate("The dropout rate must be 0 or between 0 and 1.")
  }
  if ( ni.se <0 || ni.se >=1){
    shiny::validate("the non-inferiority limit for sensitivity must be between 0 and 1.")
  }
  if (ni.sp <0 || ni.sp >=1 ){
    shiny::validate("the non-inferiority limit for sensitivity must be between 0 and 1.")
  }

}
#plausibility build in (paired)
plausi_paired <- function(prev, power, alpha, se.e, sp.e, sp.c, se.c, ni.se, ni.sp, psi.d, psi.nd, drop ){

  # noninf
  # variable to check- if non-inf
  is_ni_se <- !is.null(ni.se) && ni.se > 0 # noninf for sensitivity
  is_ni_sp <- !is.null(ni.sp) && ni.sp > 0 # noninf for specificity
  # noninf se
  if (is_ni_se){
    if( se.c - se.e > ni.se) {
      shiny::validate("Testing for non-inferiority: The non-inferiority limit must be greater than the difference between comparator test and experimental(se.c - se.e)") #if limit is not greater than difference non-inferiority is not given
    }}
  else {
    if(se.e < (se.c - ni.se)){#superiority
      shiny::validate("The sensitivity of the new test must be higher than the comparator test")
    }
  }

  #noninf sp
  if (is_ni_sp){
    if (sp.c -sp.e > ni.sp) {
      shiny::validate("Testing for non-inferiority: The non-inferiority limit must be greater than the difference between comparator test and experimental(se.c - se.e)")
    }}
  else {
    if(sp.e < (sp.c - ni.sp)){#superiority
      shiny::validate("The specificity of the new test must be higher than the comparator test")
    }
  }

  if (prev <= 0 || prev >= 1 ){
    shiny::validate("The prevalence input must be between 0 and 1.")
  }
  if (power <= 0 ||  power >=1 ){
    shiny::validate("The power input must be between 0 and 1.")
  }
  if (alpha <= 0 || alpha >= 1){
    shiny::validate("The alpha input must be between 0 and 1.")
  }
  if (se.e <= 0 || se.e >=1  ){
    shiny::validate("The sensitivity of the new test must be between 0 and 1.")
  }
  if (sp.e <= 0  || sp.e >=1){
    shiny::validate("The specificity of the new test must be between 0 and 1.")
  }
  if (se.c <= 0  || se.c >=1){
    shiny::validate("The sensitvity of the comparator test must be between 0 and 1.")
  }
  if (sp.c <= 0  || sp.c >=1){
    shiny::validate("The specificity of the comparator test input must be between 0 and 1.")
  }
  if ( psi.d <0 || psi.d >=1){
    shiny::validate("The discordant results in the diseased must be a positive value between minimum |SeC - SeE| and 1.")
  }# Discordant minimum sec-seE
  if (psi.d < abs(se.c - se.e)){
    shiny::validate("The minimum discordant value must be the difference between the comparative and the experimental test |SeC-SeE|.")
  }
  # discordant maimum: abs(se.c+se.e - 2*se.c * se.e)
  if (psi.d > abs(se.c+se.e - 2*se.c * se.e)){
    shiny::validate("The discordant results in the diseased population and the sensitivity of the new test are dependent.
             The entered value exceeds the maximum possible discordant proportion.
             For more details about the interval of the proportion of discordant test results have a look in the paper linked below.")
  }# Minimum sp discordant
  if (psi.nd < abs(sp.c - sp.e)){
    shiny::validate("The minimum discordant value must be the difference between the comparative and the experimental test |SpC-SpE|.")
  }
  if ( psi.nd <0 || psi.nd >=1){
    shiny::validate("The discordant results in the non-diseased  must be a positive value between minimum |SpC - SpE| and 1.")
  }
  #discordant maximum: abs(sp.c+sp.e - 2*sp.c*sp.e)
  if (psi.nd > abs(sp.c+sp.e - 2*sp.c*sp.e) ){
    shiny::validate("The discordant results in the non-diseased population and the specificity of the new test are dependent.
              The entered value exceeds the maximum possible discordant proportion.
             For more details about the interval of the proportion of discordant test results have a look in the paper linked below.")
  }
  if ( drop <0|| drop >=1 ){
    shiny::validate("The dropout rate must be 0 or between 0 and 1.")
  }
  if ( ni.se <0 || ni.se >=1 ){
    shiny::validate("the non-inferiority limit for sensitivity must be between 0 and 1.")
  }
  if ( ni.sp <0 || ni.sp >=1 ){
    shiny::validate("the non-inferiority limit for sensitivity must be between 0 and 1.")
  }

}

#####################################################################################################################
#power calculation
#single
plausi_pow_single <- function(prev_pow, alpha_pow, se_ni_pow,sp_ni_pow, se_pow, sp_pow, samplesize, drop){
  if (prev_pow <= 0 || prev_pow >=1){
    shiny::validate("The prevalence input must be between 0 and 1.")
  }
  if (alpha_pow <= 0  ||alpha_pow >=1){
    shiny::validate("The alpha input must be between 0 and 1.")
  }
  if (se_ni_pow <= 0  || se_ni_pow >=1){
    shiny::validate("The sensitivity of the comparator test must be between 0 and 1.")
  }
  if (se_pow <= 0  || se_pow >=1 || se_pow <= se_ni_pow){
    shiny::validate("The sensitivity of the new test must be between 0 and 1.
             Also the sensitivity of the new test must be higher than the minimum sensitivity.")
  }
  if (sp_pow <= 0  || sp_pow >=1|| sp_pow <= sp_ni_pow){
    shiny::validate("The specificity of the new test must be between 0 and 1.
             Also the specificity of the new test must be higher than the minimum specificity.")
  }
  if (sp_ni_pow <= 0  || sp_ni_pow >=1){
    shiny::validate("The specificity of the comparator test must be between 0 and 1.")
  }
  if (samplesize  <= 0  || samplesize >=99999999){
    shiny::validate("The sample size input must be between 0 and 99999999.")
  }
  if ( drop <0|| drop >=1 ){
    shiny::validate("The dropout rate must be 0 or between 0 and 1.")
  }

}
#unpaired
plausi_pow_unpaired <- function(prev, alpha, se.e, sp.e, sp.c, se.c, samplesize, drop, ni.se, ni.sp){

  # noninf
  # variable to check- if non-inf
  is_ni_se <- !is.null(ni.se) && ni.se > 0 # noninf for sensitivity
  is_ni_sp <- !is.null(ni.sp) && ni.sp > 0 # noninf for specificity
  # noninf se
  if (is_ni_se){
    if( se.c - se.e > ni.se) {
      shiny::validate("Testing for non-inferiority: The non-inferiority limit must be greater than the difference between comparator test and experimental(se.c - se.e)") #if limit is not greater than difference non-inferiority is not given
    }}
  else {
    if(se.e < (se.c - ni.se)){#superiority
      shiny::validate("The sensitivity of the new test must be higher than the comparator test")
    }
  }

  #noninf sp
  if (is_ni_sp){
    if (sp.c -sp.e > ni.sp) {
      shiny::validate("Testing for non-inferiority: The non-inferiority limit must be greater than the difference between comparator test and experimental(se.c - se.e)")
    }}
  else {
    if(sp.e < (sp.c - ni.sp)){#superiority
      shiny::validate("The specificity of the new test must be higher than the comparator test")
    }
  }
  if (prev <= 0 || prev  >= 1){
    shiny::validate("The prevalence input must be between 0 and 1.")
  }
  if (samplesize <= 0  || samplesize  >=99999999 ){
    shiny::validate("The sample size input must be between 0 and 99999999.")
  }
  if (alpha  <= 0  || alpha  >=1 ){
    shiny::validate("The alpha input must be between 0 and 1.")
  }
  if (se.e <= 0 || se.e >=1 ){
    shiny::validate("The sensitivity of the new test must be between 0 and 1.")

  }
  if (sp.e <= 0 ||  sp.e >=1 ){
    shiny::validate("The specificity of the new test must be between 0 and 1.")
  }
  if (se.c  <= 0 || se.c  >=1){
    shiny::validate("The sensitvity of the comparator test must be between 0 and 1.")
  }
  if (sp.c <= 0  || sp.c  >=1){
    shiny::validate("The specificity of the comparator test input must be between 0 and 1.")
  }
  if (ni.se  <0 || ni.se >=1 ){
    shiny::validate("the non-inferiority limit for sensitivity must be between 0 and 1.")
  }
  if (ni.sp <0 || ni.sp >=1 ){
    shiny::validate("the non-inferiority limit for sensitivity must be between 0 and 1.")
  }
  if ( drop <0|| drop >=1 ){
    shiny::validate("The dropout rate must be 0 or between 0 and 1.")
  }


}
#paired

plausi_pow_paired <- function(prev, alpha, se.e, sp.e, sp.c, se.c, samplesize, drop, psi.d, psi.nd, ni.se, ni.sp){

  # noninf
  # variable to check- if non-inf
  is_ni_se <- !is.null(ni.se) && ni.se > 0 # noninf for sensitivity
  is_ni_sp <- !is.null(ni.sp) && ni.sp > 0 # noninf for specificity
  # noninf se
  if (is_ni_se){
    if( se.c - se.e > ni.se) {
      shiny::validate("Testing for non-inferiority: The non-inferiority limit must be greater than the difference between comparator test and experimental(se.c - se.e)") #if limit is not greater than difference non-inferiority is not given
    }}
  else {
    if(se.e < (se.c - ni.se)){#superiority
      shiny::validate("The sensitivity of the new test must be higher than the comparator test")
    }
  }

  #noninf sp
  if (is_ni_sp){
    if (sp.c -sp.e > ni.sp) {
      shiny::validate("Testing for non-inferiority: The non-inferiority limit must be greater than the difference between comparator test and experimental(se.c - se.e)")
    }}
  else {
    if(sp.e < (sp.c - ni.sp)){#superiority
      shiny::validate("The specificity of the new test must be higher than the comparator test")
    }
  }
  if (prev <= 0 ||  prev  >=1 ){
    shiny::validate("The prevalence input must be between 0 and 1.")
  }
  if (samplesize <= 0 || samplesize  >=99999999){
    shiny::validate("The sample size input must be between 0 and 99999999.")
  }
  if (alpha  <= 0 || alpha  >=1 ){
    shiny::validate("The alpha input must be between 0 and 1.")
  }
  if (se.e <= 0 || se.e >=1  ){
    shiny::validate("The sensitivity of the new test must be between 0 and 1.")

  }
  if (sp.e <= 0 || sp.e >=1 ){
    shiny::validate("The specificity of the new test must be between 0 and 1.")
  }
  if (se.c  <= 0 || se.c  >=1){
    shiny::validate("The sensitvity of the comparator test must be between 0 and 1.")
  }
  if (sp.c <= 0 ||  sp.c  >=1){
    shiny::validate("The specificity of the comparator test input must be between 0 and 1.")
  }
  if (ni.se  <0 || ni.se >=1){
    shiny::validate("the non-inferiority limit for sensitivity must be between 0 and 1.")
  }
  if (ni.sp <0 || ni.sp >=1 ){
    shiny::validate("the non-inferiority limit for sensitivity must be between 0 and 1.")
  }
  if ( drop <0|| drop >=1 ){
    shiny::validate("The dropout rate must be 0 or between 0 and 1.")
  }
  if ( psi.d <0 || psi.d >=1){
    shiny::validate("The discordant results in the diseased must be a positive value between minimum |SeC - SeE| and 1.")
  }# Discordant minimum sec-seE
  if (psi.d < abs(se.c - se.e)){
    shiny::validate("The minimum discordant value must be the difference between the comparative and the experimental test |SeC-SeE|.")
  }
  # discordant maximum : abs(se.c+se.e - 2*se.c * se.e)
  if (psi.d > abs(se.c+se.e - 2*se.c * se.e) ){
    shiny::validate("The discordant results in the diseased population and the sensitivity of the new test are dependent.
             The entered value exceeds the maximum possible discordant proportion.
             For more details about the interval of the proportion of discordant test results have a look in the paper linked below. ")
  }# Minimum sp discordant
  if (psi.nd < abs(sp.c - sp.e)){
    shiny::validate("The minimum discordant value must be the difference between the comparative and the experimental test |SpC-SpE|.")
  }
  if ( psi.nd <0 || psi.nd >=1){
    shiny::validate("The discordant results in the non-diseased  must be a positive value between minimum |SpC - SpE| and 1.")
  }#discordant maximum:  abs(sp.c+sp.e - 2*sp.c * sp.e)
  if (psi.nd > abs(sp.c+sp.e - 2*sp.c * sp.e) ){
    shiny::validate("The discordant results in the non-diseased population and the specificity of the new test are dependent.
             The entered value exceeds the maximum possible discordant proportion.
             For more details about the interval of the proportion of discordant test results have a look in the paper linked below.")
  }

}


