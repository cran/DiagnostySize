#source("optimal_paper1.R") # necessary for the access of the functions defined in the skript
#source("power_calculation.R")
#source("plausibility.R")
#source("sample_size_function.R")
#source("power_calc_comparative.R")


server <- function(input, output) {
  #Info Buttons----------------------------------------------------------------------------------------------------------------------------------
  #sample size
  #explanation of unpaired and paired
  shiny::observeEvent(input$designhelp_comparative,{
    shiny::showModal(shiny::modalDialog(
      title = "Explanation of unpaired and paired",
      "The comparative design is used to compare the experimental and the pre-evaluated comparator test.
      There are further decisions to be made in the comparative design. One option is a paired design, where each subject receives all three tests.
      The experimental test, the comparator test and the reference standard test. The other option is an unpaired design, in which the subjects
      are divided into two groups. One group receives the experimental test and the other receives the comparator test.
      Both groups receives the reference standard test."
    ))
  } )
  #Info about the study designs Comparative & single
  shiny::observeEvent(input$designhelp_samplesize,{
    shiny::showModal(shiny::modalDialog(
      title = "Explanation of the design choices",
      "In the single-arm trial, each subject receives the experimental test and the reference standard. The expected and the minimum values must be entered and are compared.
      The expected sensitivity and specificity are the values
      that the user expects from the experimental test. The minimum sensitivity and specificity are the values that the experimental test has to reach at least.
      The comparative design is used to compare the experimental and the pre-evaluated comparator test, based on the reference standard.
      Once the comparative design has been selected, there are two design options: the paired design and the unpaired design."
    ))
  } )
  #Power ########################################################
  #Info about the study designs Comparative & single
  shiny::observeEvent(input$designhelp,{
    shiny::showModal(shiny::modalDialog(
      title = "Explanation of the design choices",
      "In the single-arm trial, each subject receives the experimental test and the reference standard. The expected and the minimum values must be entered and are compared.
      The expected sensitivity and specificity are the values
      that the user expects from the experimental test. The minimum sensitivity and specificity are the values that the experimental test has to reach at least.
      The comparative design is used to compare the experimental and the pre-evaluvated comparator test, based on the reference standard.
      Once the comparative design has been selected, there are two design options: the paired design and the unpaired design."
    ))
  })
  #explanation of unpaired and paired
  shiny::observeEvent(input$designhelp_comparative_p,{
    shiny::showModal(shiny::modalDialog(
      title = "Explanation of unpaired and paired",
      "The comparative design is used to compare the experimental and the pre-evaluvated comparator test.
      There are further decisions to be made in the comparative design. One option is a paired design, where each subject receives all three tests.
      The experimental test, the comparator test and the reference standard test. The other option is an unpaired design, in which the subjects
      are divided into two groups. One group receives the experimental test and the other receives the comparator test.
      Both groups receives the reference standard test."
    ))
  } )

  # Links-----------------------------------------------------------------------------------------------------------------------------------
  #single arm paper
  doi_singlearm <- shiny::tags$a("Single-arm sample size calculation paper (Stark et al.)", href="https://doi.org/10.1177/0962280220913588")
  #comparative paper
  doi_comparative <- shiny::tags$a("Sample size calculation paper in a comparative diagnostic accuracy study (Stark et al.)", href="https://doi.org/10.1186/s12874-022-01564-2")

  #Info about Paper------------------------------------------------------------------------------------------------------------------------
  #single
  output$singlearmtxt <- shiny::renderUI({
    txt <-HTML("<h4 style = 'color: black;'>The exact sample size calculations are based on the article:</h4>
               <p style='font-size: 14px;'> Sample size calculation and re-estimation based on the prevalence in a single-arm
               confirmatory diagnostic accuracy study <br>
               written by<strong> Maria Stark </strong> and <strong>Antonia Zapf</strong>.</p> " )
    link <- tagList("URL Link:", doi_singlearm)

    HTML(paste(txt,link))
  })
  #comparative
  #paired
  output$comptxt <- shiny::renderUI({
    txt <-HTML("<h4 style = 'color: black;'>The calculation of unpaired and paired sample sizes in a comparative study design is based on the article: </h4>
                <p style='font-size: 14px;'>Blinded sample size re-estimation in a comparative diagnostic accuracy study<br>
                written by <strong> Maria Stark, Mailin Hesse, Werner Brannath</strong> and <strong>Antonia Zapf</strong>.</p>")
    link <- tagList("URL Link:", doi_comparative)

    HTML(paste(txt, link))
  })
  #unpaired
  output$comptxt2 <- shiny::renderUI({
    txt <-HTML("<h4 style = 'color: black;'>The calculation of unpaired and paired sample sizes in a comparative study design is based on the article: </h4>
                <p style='font-size: 14px;'>Blinded sample size re-estimation in a comparative diagnostic accuracy study<br>
                written by <strong>Maria Stark, Mailin Hesse, Werner Brannath</strong>  and <strong>Antonia Zapf</strong>.</p>")
    link <- tagList("URL Link:", doi_comparative)
    HTML(paste(txt,link))
  })

  #Calculation results#################################################################################################################################################################################
  #Sample size
  # single
  #  exact

  output$exact <- shiny::renderPrint({
    #Input field can't be empty (Messages for users to insert a value)
    shiny::validate(
      shiny::need(input$prev != "", label = "Prevalenc")
    )
    shiny::validate(
      shiny::need(input$power != "", label = "Power")
    )
    shiny::validate(
      shiny::need(input$alpha != "", label = "Alpha ")
    )
    shiny::validate(
      shiny::need(input$se_ni != "", label = "Minimum sensitivity ")
    )
    shiny::validate(
      shiny::need(input$se != "", label = "New test sensitivity ")
    )
    shiny::validate(
      shiny::need(input$sp != "", label = "New test specificity")
    )
    shiny::validate(
      shiny::need(input$sp_ni != "", label = "Minimum specifitcity ")
    )
    shiny::validate(
      shiny::need(input$drop_s_n != "", label = "Dropout")
    )
    plausibility_single(prev = input$prev
                        ,power = input$power
                        ,alpha = input$alpha
                        ,se_ni = input$se_ni
                        ,se = input$se
                        ,sp = input$sp
                        ,sp_ni = input$sp_ni
                        ,drop = input$drop_s_n) #Plausibility_single function with plausibility checks for input values
    print("The exact sample size calculation")
    # before call function ss.exact from stark et al., call plausibility checks for input values
    ss.exact(alpha = input$alpha
             ,power = input$power
             ,se = input$se
             ,se.ni = input$se_ni
             ,sp = input$sp
             ,sp.ni = input$sp_ni
             ,prev = input$prev
             ,drop = input$drop_s_n)
  })


  # Calculation results in text

  output$singleresult <- shiny::renderUI({
    plausibility_single(prev = input$prev
                        ,power = input$power
                        ,alpha = input$alpha
                        ,se_ni = input$se_ni
                        ,se = input$se
                        ,sp = input$sp
                        ,sp_ni = input$sp_ni
                        ,drop = input$drop_s_n)

    #new variable 'exact_list' with result values in List
    exact_list = ss.exact(alpha = input$alpha
                          ,power = input$power
                          ,se = input$se
                          ,se.ni = input$se_ni
                          ,sp = input$sp
                          ,sp.ni = input$sp_ni
                          ,prev = input$prev
                          ,drop = input$drop_s_n)
    #Text
    #input values rounded
    inptxt <- HTML("<h4 style = 'color: black;'>The calculation is based on the following input values:</h4>
                   <p style='font-size: 14px;'>prevalence: ", round((input$prev*100),digits=2),"%",
                   "<br> alpha (two-sided):",round((input$alpha*100),digits = 2),"% (in the calculations the one-sided alpha is used (alpha/2): ", (round((input$alpha*100),digits = 2)/2),"%)",
                   "<br> power:", round((input$power*100),digits =2),"%",
                   "<br> minimum sensitivity: ", round((input$se_ni*100),digits=2),"%",
                   "<br> minimum specificity:", round((input$sp_ni*100),digits=2),"%",
                   "<br> sensitivity of the new test:", round((input$se*100),digits=2), "%",
                   "<br> specificity of the new test:", round((input$sp*100),digits = 2),"%",
                   "<br> the dropout rate:", round((input$drop_s_n*100),digits = 2),"%",
                   "<br> </p><h4 style = 'color:black;'> The results of the exact calculation:</h4>")
    # calculations results via list access
    resulttxt<- HTML("<p style='font-size: 14px;'>For a single arm diagnostic accuracy study, to show the entered sensitivity and specificity
                     <br>the needed number of study participants without the target condition is:  ",exact_list[[5]],
                     "<br> and the number of study participants with the target condition is:", exact_list[[4]],".<br> The total sample size is: ",exact_list[[1]],
                     "<br> and the total sample size to show the minimum sensitivity is: ", exact_list[[2]],
                     "<br> to show the minimum specificity the total sample size is: ", exact_list[[3]],
                     "<br> These numbers of probands make sure that the desired overall power of:",(input$power*100),"% is reached. ",
                     "<br> The result of the calculation for the overall  power is: ", (exact_list[[6]]*100),"%",
                     "<br> The overall power can be split in sensitvity and specificity. The power of sensitivity is : ",(exact_list[[7]]*100),"%",
                     "<br> and the power of specificity is:", (exact_list[[8]]*100),"%",
                     "<br> </p>" )

    HTML(paste( inptxt, resulttxt))
  })
  # Table single arm trial

    table_function_N_single <-function(){
      plausibility_single(prev = input$prev
                          ,power = input$power
                          ,alpha = input$alpha
                          ,se_ni = input$se_ni
                          ,se = input$se
                          ,sp = input$sp
                          ,sp_ni = input$sp_ni
                          ,drop = input$drop_s_n)

      exact_list = ss.exact(alpha = input$alpha
                            ,power = input$power
                            ,se = input$se
                            ,se.ni = input$se_ni
                            ,sp = input$sp
                            ,sp.ni = input$sp_ni
                            ,prev = input$prev
                            ,drop = input$drop_s_n)

      table_results_single <- data.frame(
        Names = c("prevalence"
                  ,"alpha (two-sided)"
                  ,"power"
                  ,"minimum sensitivity"
                  ,"minimum specificity"
                  ,"sensitivity of the new test"
                  ,"specificity of the new test"
                  ,"the dropout rate"
                  ,"total sample size(N)"
                  ,"total sample size to reach the minimum sensitivity(N.se)"
                  ,"total sample size to reach the minimum specificity (N.sp)"
                  ,"number of study participants with target condition (n.se)"
                  ,"number of study participants without target condition (n.sp)"
                  ,"power total"
                  ,"power sensitivity"
                  ,"power specificity"),
        values = c(round((input$prev*100),digits=2)
                   ,round((input$alpha*100),digits = 2)
                   ,round((input$power*100),digits =2)
                   ,round((input$se_ni*100),digits=2)
                   ,round((input$sp_ni*100),digits=2)
                   ,round((input$se*100),digits=2)
                   ,round((input$sp*100),digits = 2)
                   ,round((input$drop_s_n*100),digits = 2)
                   ,exact_list[[1]]
                   ,exact_list[[2]]
                   ,exact_list[[3]]
                   ,exact_list[[4]]
                   ,exact_list[[5]]
                   ,(exact_list[[6]]*100)
                   ,(exact_list[[7]]*100)
                   ,(exact_list[[8]]*100))
      )
    }

    output$singletable <- DT::renderDT({
      table_function_N_single()
    })


  ##############################################################################################################################################################################################
  #Calculation results, Sample size
  # comparative, unpaired
  output$unpaired <- shiny::renderPrint({
    #Input field can't be empty (Messages for users to insert a value)
    shiny::validate(
      shiny::need(input$prev_up != "", label = "Prevalenc")
    )
    shiny::validate(
      shiny::need(input$power_up != "", label = "Power")
    )
    shiny::validate(
      shiny::need(input$alpha_up != "", label = "Alpha ")
    )
    shiny::validate(
      shiny::need(input$se_ni_up != "", label = "Comparator test sensitivity ")
    )
    shiny::validate(
      shiny::need(input$se_up != "", label = "New test sensitivity ")
    )
    shiny::validate(
      shiny::need(input$sp_up != "", label = "New test specificity")
    )
    shiny::validate(
      shiny::need(input$sp_ni_up != "", label = "Comparator test specifitcity ")
    )
    shiny::validate(
      shiny::need(input$noninf_se != "", label = "Non-inferiority margin sensitivity ")
    )
    shiny::validate(
      shiny::need(input$noninf_sp != "", label = "Non-inferiority margin specificity ")
    )
    shiny::validate(
      shiny::need(input$drop_up_n != "", label = "Dropout")
    )
  # plausibility check
   plausibility_unpaired(alpha = input$alpha_up
                         ,power = input$power_up
                         ,prev = input$prev_up
                          ,se.c = input$se_ni_up
                          ,se.e = input$se_up
                          ,sp.c = input$sp_ni_up
                          ,sp.e = input$sp_up
                          ,ni.se = input$noninf_se
                          ,ni.sp = input$noninf_sp
                          ,drop = input$drop_up_n)

    sample_size_calc (alpha = input$alpha_up
                                 ,power = input$power_up
                                 ,se.c = input$se_ni_up
                                 ,se.e = input$se_up
                                 ,sp.c = input$sp_ni_up
                                 ,sp.e = input$sp_up
                                 ,prev = input$prev_up
                                 ,ni.se = input$noninf_se
                                 ,ni.sp = input$noninf_sp
                                 ,psi.d= 0
                                 ,psi.nd = 0
                                 ,paired = FALSE
                                 ,drop = input$drop_up_n)
  })
    output$comp_upresult <- shiny::renderUI({
      plausibility_unpaired(alpha = input$alpha_up
                            ,power = input$power_up
                            ,prev = input$prev_up
                            ,se.c = input$se_ni_up
                            ,se.e = input$se_up
                            ,sp.c = input$sp_ni_up
                            ,sp.e = input$sp_up
                            ,ni.se = input$noninf_se
                            ,ni.sp = input$noninf_sp
                            ,drop = input$drop_up_n)

      up_list = sample_size_calc(alpha = input$alpha_up
                                ,power = input$power_up
                                ,prev = input$prev_up
                                ,se.c = input$se_ni_up
                                ,se.e = input$se_up
                                ,sp.c = input$sp_ni_up
                                ,sp.e = input$sp_up
                                ,ni.se = input$noninf_se
                                ,ni.sp = input$noninf_sp
                                ,psi.d = 0
                                ,psi.nd = 0
                                ,paired = FALSE
                                ,drop = input$drop_up_n)
      #Text
      inptxt <- HTML("<h4 style = 'color: black;'>The calculation is based on the following input values: </h4>
                      <p style='font-size: 14px;'> prevalence: ", round((input$prev_up*100),digits=2),"%",
                     "<br> alpha:", round((input$alpha_up*100),digits=2),"% (in the calculations the one-sided alpha is used (alpha/2): ", (round((input$alpha_up*100),digits = 2)/2),"%)",
                      "<br> power:", round((input$power_up*100),digits=2),"%",
                      "<br> comparator test sensitivity: ", round((input$se_ni_up*100),digits=2),"%",
                      "<br> comparator test specificity:", round((input$sp_ni_up*100), digits=2),"%",
                      "<br> sensitivity of the new test:", round((input$se_up*100),digits=2),"%",
                      "<br> specificity of the new test:", round((input$sp_up*100),digits=2),"%",
                     "<br> the non-inferiority limit value for sensitivity:", round((input$noninf_se*100),digits = 2),"%",
                     "<br> the non-inferiority limit value for specificity:", round((input$noninf_sp*100),digits = 2),"%",
                     "<br> the dropout rate:", round((input$drop_up_n*100),digits = 2),"%",
                      "<br></p><h4 style = 'color:black;'> The results of the unpaired calculation:</h4>")
      #resulttext
      resulttxt<- HTML("<p style='font-size: 14px;'>For a comparative diagnostic accuracy study design, to show the entered sensitivity and specificity
                      <br>the needed number of study participants without the target condition is:  ",up_list[[5]],
                      "<br> and the number of study participants with the target condition is:", up_list[[4]],
                      ".<br> The total sample size is: ",up_list[[1]],
                      "--In the unpaired comparative design the total sample size is split equally between the two groups (total participants/2), so each test is performed:"
                      , (up_list[[1]])/2, "participants",
                      ".<br> The total sample size to show the minimum sensitivity is: ", up_list[[2]],
                      "<br> to show the minimum specificity the total sample size is: ", up_list[[3]],
                      "<br> These numbers of probands make sure that the desired overall power of:",(input$power*100),"% is reached. ",
                      "<br> The result of the calculation for the overall  power is: ", (up_list[[6]]*100),"%",
                      "<br> The overall power can be split in sensitvity and specificity. The power of sensitivity is : ",(up_list[[7]]*100),"%",
                      "<br> and the power of specificity is:", (up_list[[8]]*100),"%", "<br> </p>")
    HTML(paste(inptxt, resulttxt))
    })

  # Table comparative unpaired ---------------------------------------------------------------------------

  table_function_N_unpaired <-function(){

    plausibility_unpaired(alpha = input$alpha_up
                          ,power = input$power_up
                          ,prev = input$prev_up
                          ,se.c = input$se_ni_up
                          ,se.e = input$se_up
                          ,sp.c = input$sp_ni_up
                          ,sp.e = input$sp_up
                          ,ni.se = input$noninf_se
                          ,ni.sp = input$noninf_sp
                          ,drop = input$drop_up_n)

      up_list = sample_size_calc(alpha = input$alpha_up
                                 ,power = input$power_up
                                 ,prev = input$prev_up
                                 ,se.c = input$se_ni_up
                                 ,se.e = input$se_up
                                 ,sp.c = input$sp_ni_up
                                 ,sp.e = input$sp_up
                                 ,ni.se = input$noninf_se
                                 ,ni.sp = input$noninf_sp
                                 ,psi.d = 0
                                 ,psi.nd = 0
                                 ,paired = FALSE
                                 ,drop = input$drop_up_n)

    table_results_N_unpaired <- data.frame(
      Names = c("prevalence"
                ,"alpha (two-sided)"
                ,"power"
                ,"comparator test sensitivity"
                ,"comparator test specificity"
                ,"sensitivity of the new test"
                ,"specificity of the new test"
                ,"the non-inferiority limit value for sensitivity"
                ,"the non-inferiority limit value for sensitivity"
                ,"the dropout rate"
                ,"total sample size(N)"
                ,"total sample size to reach the comparator test sensitivity(N.se)"
                ,"total sample size to reach the comparator test specificity (N.sp)"
                ,"number of study participants with target condition (n.se)"
                ,"number of study participants without target condition (n.sp)"
                ,"power total"
                ,"power sensitivity"
                ,"power specificity"),
      values = c(round((input$prev_up*100),digits=2)
                 ,round((input$alpha_up*100),digits = 2)
                 ,round((input$power_up*100),digits =2)
                 ,round((input$se_ni_up*100),digits=2)
                 ,round((input$sp_ni_up*100),digits=2)
                 ,round((input$se_up*100),digits=2)
                 ,round((input$sp_up*100),digits = 2)
                 ,round((input$noninf_se*100),digits = 2)
                 ,round((input$noninf_sp*100),digits = 2)
                 ,round((input$drop_up_n*100),digits =2)
                 ,up_list[[1]]
                 ,up_list[[2]]
                 ,up_list[[3]]
                 ,up_list[[4]]
                 ,up_list[[5]]
                 ,(up_list[[6]]*100)
                 ,(up_list[[7]]*100)
                 ,(up_list[[8]]*100))
    )

  }

  output$unpairedtable <- DT::renderDT({

      table_function_N_unpaired()

  })
  ###########################################################################################################################################################################################
  #Calculation results, Sample size
  # comparative, paired

  output$paired <- shiny::renderPrint({
    #Input field can't be empty (Messages for users to insert a value)
    shiny::validate(
      shiny::need(input$prev_up != "", label = "Prevalenc")
    )
    shiny::validate(
      shiny::need(input$power_p != "", label = "Power")
    )
    shiny::validate(
      shiny::need(input$alpha_p != "", label = "Alpha ")
    )
    shiny::validate(
      shiny::need(input$se_ni_p != "", label = "Comparator test sensitivity ")
    )
    shiny::validate(
      shiny::need(input$se_p != "", label = "New test sensitivity ")
    )
    shiny::validate(
      shiny::need(input$sp_p != "", label = "New test specificity")
    )
    shiny::validate(
      shiny::need(input$sp_ni_p != "", label = "Comparator test specifitcity ")
    )
    shiny::validate(
      shiny::need(input$noninf_se != "", label = "Non-inferiority margin sensitivity ")
    )
    shiny::validate(
      shiny::need(input$noninf_sp != "", label = "Non-inferiority margin specificity ")
    )
    shiny::validate(
      shiny::need(input$psi_d_p != "", label = "Discordant test results diseased ")
    )
    shiny::validate(
      shiny::need(input$psi_nd_p != "", label = "Discordant test results non-diseased")
    )
    shiny::validate(
      shiny::need(input$drop_p_n != "", label = "Dropout")
    )
    plausi_paired(alpha = input$alpha_p
                  ,power = input$power_p
                  ,prev = input$prev_p
                  ,se.c = input$se_ni_p
                  ,se.e = input$se_p
                  ,sp.c = input$sp_ni_p
                  ,sp.e = input$sp_p
                  ,ni.se = input$noninf_se
                  ,ni.sp = input$noninf_sp
                  ,psi.d = input$psi_d_p
                  ,psi.nd = input$psi_nd_p
                  ,drop = input$drop_p_n)

      sample_size_calc (alpha = input$alpha_p
                        ,power = input$power_p
                        ,se.c = input$se_ni_p
                        ,se.e = input$se_p
                        ,sp.c = input$sp_ni_p
                        ,sp.e = input$sp_p
                        ,prev = input$prev_p
                        ,ni.se = input$noninf_se
                        ,ni.sp = input$noninf_sp
                        ,psi.d= input$psi_d_p
                        ,psi.nd = input$psi_nd_p
                        ,paired = TRUE
                        ,drop = input$drop_p_n)
    })
    output$comp_presult <- shiny::renderUI({
      plausi_paired(alpha = input$alpha_p
                    ,power = input$power_p
                    ,prev = input$prev_p
                    ,se.c = input$se_ni_p
                    ,se.e = input$se_p
                    ,sp.c = input$sp_ni_p
                    ,sp.e = input$sp_p
                    ,ni.se = input$noninf_se
                    ,ni.sp = input$noninf_sp
                    ,psi.d = input$psi_d_p
                    ,psi.nd = input$psi_nd_p
                    ,drop = input$drop_up_n)

      p_list = sample_size_calc(alpha = input$alpha_p
                                 ,power = input$power_p
                                 ,prev = input$prev_p
                                 ,se.c = input$se_ni_p
                                 ,se.e = input$se_p
                                 ,sp.c = input$sp_ni_p
                                 ,sp.e = input$sp_p
                                 ,ni.se = input$noninf_se
                                 ,ni.sp = input$noninf_sp
                                 ,psi.d = input$psi_d_p
                                 ,psi.nd = input$psi_nd_p
                                 ,paired = TRUE
                                 ,drop = input$drop_p_n)
      #text
      inptxt <-HTML("<h4 style = 'color: black;'>The calculation is based on the following input values: </h4>
                            <p style='font-size: 14px;'> prevalence: ", round((input$prev_p*100),digits=2), "%",
           "<br> alpha ", round((input$alpha_p*100),digits = 2),"% (in the calculations the one-sided alpha is used (alpha/2): ", (round((input$alpha_p*100),digits = 2)/2),"%)",
           "<br> power:", round((input$power_p*100),digits = 2),"%",
           "<br> comparator test sensitivity: ", round((input$se_ni_p*100),digits = 2),"%",
           "<br> comparator test specificity:", round((input$sp_ni_p*100),digits = 2),"%",
           "<br> sensitivity of the new test:", round((input$se_p*100),digits = 2),"%",
           "<br> specificity of the new test:", round((input$sp_p*100),digits = 2),"%",
           "<br> the discordant results of the diseased: ", round((input$psi_d_p*100), digits = 2),"%",
           "<br> the discordant results of the non-diseased: ", round((input$psi_nd_p*100),digits = 2),"%",
           "<br> the non-inferiority limit value for sensitivity:", round((input$noninf_se*100),digits = 2),"%",
           "<br> the non-inferiority limit value for specificity:", round((input$noninf_sp*100),digits = 2),"%",
           "<br> the dropout rate:",round((input$drop_p_n*100),digits=2),"%",
           "<br> </p><h4 style = 'color: black;'>The results of the paired calculation tested for non-inferiority:</h4>")

      resulttxt<- HTML("<p style='font-size: 14px;'>For a comparative diagnostic accuracy study design, to show the entered sensitivity and specificity
                              <br>the needed number of non-diseased subjects is:  ",p_list[[5]],
                              "<br> and the number of the diseased is:", p_list[[4]],".<br> The total sample size is: ",p_list[[1]],
                              "<br> and the total sample size to show the minimum sensitivity is: ", p_list[[2]],
                              "<br> to show the minimum specificity the total sample size is: ", p_list[[3]],
                              "<br> These numbers of probands make sure that the desired overall power of:",(input$power*100),"% is reached. ",
                              "<br> The result of the calculation for the overall  power is: ", (p_list[[6]]*100),"%",
                              "<br> The overall power can be split in sensitvity and specificity. The power of sensitivity is : ",(p_list[[7]]*100),"%",
                              "<br> and the power of specificity is:", (p_list[[8]]*100),"%",
                              "<br> </p>")

      HTML(paste(inptxt, resulttxt))
    })

  # Table comparative paired ------------------------------------------------------------------------------

  table_function_N_paired <-function(){
    plausi_paired(alpha = input$alpha_p
                  ,power = input$power_p
                  ,prev = input$prev_p
                  ,se.c = input$se_ni_p
                  ,se.e = input$se_p
                  ,sp.c = input$sp_ni_p
                  ,sp.e = input$sp_p
                  ,ni.se = input$noninf_se
                  ,ni.sp = input$noninf_sp
                  ,psi.d = input$psi_d_p
                  ,psi.nd = input$psi_nd_p
                  ,drop = input$drop_up_n)

    p_list = sample_size_calc (alpha = input$alpha_p
                      ,power = input$power_p
                      ,se.c = input$se_ni_p
                      ,se.e = input$se_p
                      ,sp.c = input$sp_ni_p
                      ,sp.e = input$sp_p
                      ,prev = input$prev_p
                      ,ni.se = input$noninf_se
                      ,ni.sp = input$noninf_sp
                      ,psi.d= input$psi_d_p
                      ,psi.nd = input$psi_nd_p
                      ,paired = TRUE
                      ,drop = input$drop_p_n)

  table_results_N_paired <- data.frame(
          Names = c("prevalence"
                    ,"alpha (two-sided)"
                    ,"power"
                    ,"comparator test sensitivity"
                    ,"comparator test specificity"
                    ,"sensitivity of the new test"
                    ,"specificity of the new test"
                    ,"the discordant results of the diseased"
                    ,"the discordant results of the non-diseased"
                    ,"the non-inferiority limit value for sensitivity"
                    ,"the non-inferiority limit value for specificity"
                    ,"the dropout rate"
                    ,"total sample size(N)"
                    ,"total sample size to reach the comparator test sensitivity(N.se)"
                    ,"total sample size to reach the comparator test specificity (N.sp)"
                    ,"number of study participants with target condition (n.se)"
                    ,"number of study participants without target condition (n.sp)"
                    ,"power total"
                    ,"power sensitivity"
                    ,"power specificity"),
          values = c(round((input$prev_p*100),digits=2)
                     ,round((input$alpha_p*100),digits = 2)
                     ,round((input$power_p*100),digits =2)
                     ,round((input$se_ni_p*100),digits=2)
                     ,round((input$sp_ni_p*100),digits=2)
                     ,round((input$se_p*100),digits=2)
                     ,round((input$sp_p*100),digits = 2)
                     ,round((input$psi_d_p*100), digits = 2)
                     ,round((input$psi_nd_p*100), digits = 2)
                     ,round((input$noninf_se*100),digits = 2)
                     ,round((input$noninf_sp*100),digits = 2)
                     ,round((input$drop_p_n*100),digits =2)
                     ,p_list[[1]]
                     ,p_list[[2]]
                     ,p_list[[3]]
                     ,p_list[[4]]
                     ,p_list[[5]]
                     ,(p_list[[6]]*100)
                     ,(p_list[[7]]*100)
                     ,(p_list[[8]]*100))
        )
      }

output$pairedtable <- DT::renderDT({

    table_function_N_paired()
 })



  #Calculation results####################################################################################################################################################################
  #Power
  # single
  #  exact

  output$power_single <- shiny::renderPrint({
    shiny::validate(
      shiny::need(input$prev_pow != "", label = "Prevalenc")
    )
    shiny::validate(
      shiny::need(input$samplesize != "", label = "Sample size")
    )
    shiny::validate(
      shiny::need(input$alpha_pow != "", label = "Alpha ")
    )
    shiny::validate(
      shiny::need(input$se_ni_pow != "", label = "Minimum sensitivity ")
    )
    shiny::validate(
      shiny::need(input$se_pow != "", label = "New test sensitivity ")
    )
    shiny::validate(
      shiny::need(input$sp_pow != "", label = "New test specificity")
    )
    shiny::validate(
      shiny::need(input$sp_ni_pow != "", label = "Minimum specifitcity ")
    )
    shiny::validate(
      shiny::need(input$drop_s_pow != "", label = "Dropout")
    )
    #plausibility checks of the input values
    plausi_pow_single(prev_pow = input$prev_pow
                      ,alpha_pow = input$alpha_pow
                      ,samplesize = input$samplesize
                      ,se_pow =input$se_pow
                      ,sp_pow = input$sp_pow
                      ,se_ni_pow = input$se_ni_pow
                      ,sp_ni_pow = input$sp_ni_pow
                      ,drop = input$drop_s_pow)

    print("The exact power calculation")
    exact_power(alpha = input$alpha_pow
                ,N = input$samplesize
                ,se = input$se_pow
                ,se_ni= input$se_ni_pow
                ,sp = input$sp_pow
                ,sp_ni = input$sp_ni_pow
                ,prev = input$prev_pow
                ,drop = input$drop_s_pow) #call function exact_power -calculates the power
  })
  # Info about the Paper

  output$singleexact <- shiny::renderUI({
    txt <- HTML("<h4 style = 'color: black;'>The power calculation is based on the article:</h4>
                <p style='font-size: 14px;'>Sample size calculation and re-estimation based on the prevalence in a single-arm confirmatory diagnostic accuracy study
                <br>written by <strong>Maria Stark</strong> and <strong>Antonia Zapf</strong>.</p>")
    link <- tagList("URL Link:", doi_singlearm)
    HTML(paste(txt,link))
  })
  #Calculation results in text

  output$singlepowerresult <- shiny::renderUI({
    plausi_pow_single(prev_pow = input$prev_pow
                      ,alpha_pow = input$alpha_pow
                      ,samplesize = input$samplesize
                      ,se_pow =input$se_pow
                      ,sp_pow = input$sp_pow
                      ,se_ni_pow = input$se_ni_pow
                      ,sp_ni_pow = input$sp_ni_pow
                      ,drop = input$drop_s_pow)
    #exact calculation
    exact_list = exact_power(alpha = input$alpha_pow
                             ,se = input$se_pow
                             ,se_ni = input$se_ni_pow
                             ,N = input$samplesize
                             ,sp = input$sp_pow
                             ,sp_ni = input$sp_ni_pow
                             ,prev = input$prev_pow
                             ,drop = input$drop_s_pow)
    #recursive function not implemented-
    #Text
    inptvars <- HTML("<h4 style = 'color: black;'>The calculation is based on the following input values:</h4>
                      <p style='font-size: 14px;'> prevalence: ", round((input$prev_pow*100),digits=2),"%",
                     "<br>alpha:", round(( input$alpha_pow *100),digits=2),"% (in the calculations the one-sided alpha is used (alpha/2): ", (round((input$alpha_pow*100),digits = 2)/2),"%)",
                     "<br>sample size:",ceiling(input$samplesize),
                     "<br>minimum sensitivity: ", round((input$se_ni_pow  *100),digits=2),"%",
                     "<br> minimum specificity:", round(( input$sp_ni_pow *100),digits=2),"%",
                     "<br>sensitivity of the new test:", round(( input$se_pow *100),digits=2),"%",
                     "<br> specificity of the new test:", round((input$sp_pow *100),digits=2),"%",
                     "<br> the dropout rate:",round((input$drop_s_pow *100), digits = 2), "%",
                     "<br></p><h4 style = 'color: black;'>The results of the exact calculations:</h4>")

    resulttxt<- HTML("<p style = 'font-size: 14px;'>With a sample size of: ", ceiling(input$samplesize), "an overall power of:", (exact_list[[1]] *100),"% can be reached.",
                     "<br> The overall power can be split in sensitvity and specificity. The power of sensitivity is:",( exact_list[[2]]*100),"%",
                     "<br> and the power of  specificity is:", (exact_list[[3]]*100),"%")

    HTML(paste(inptvars, resulttxt))
  })
  #Table  power single--------------------------------------------------------------------------------
  table_function_pow_single <-function(){
    plausi_pow_single(prev_pow = input$prev_pow
                      ,alpha_pow = input$alpha_pow
                      ,samplesize = input$samplesize
                      ,se_pow =input$se_pow
                      ,sp_pow = input$sp_pow
                      ,se_ni_pow = input$se_ni_pow
                      ,sp_ni_pow = input$sp_ni_pow
                      ,drop = input$drop_s_pow)

    exact_list = exact_power(alpha = input$alpha_pow
                             ,se = input$se_pow
                             ,se_ni = input$se_ni_pow
                             ,N = input$samplesize
                             ,sp = input$sp_pow
                             ,sp_ni = input$sp_ni_pow
                             ,prev = input$prev_pow
                            ,drop = input$drop_s_pow)
    table_results_single_pow <- data.frame(
      Names = c("prevalence"
                ,"alpha (two-sided)"
                ,"sample size"
                ,"minimum sensitivity"
                ,"minimum specificity"
                ,"sensitivity of the new test"
                ,"specificity of the new test"
                ,"the dropout rate"
                ,"power total"
                ,"power sensitivity"
                ,"power specificity"),
      values = c(round((input$prev_pow*100),digits=2)
                 ,round((input$alpha_pow*100),digits = 2)
                 ,ceiling(input$samplesize)
                 ,round((input$se_ni_pow*100),digits=2)
                 ,round((input$sp_ni_pow*100),digits=2)
                 ,round((input$se_pow*100),digits=2)
                 ,round((input$sp_pow*100),digits = 2)
                 ,round((input$drop_s_pow *100),digits = 2)
                 ,(exact_list[[1]]*100)
                 ,(exact_list[[2]]*100)
                 ,(exact_list[[3]]*100))
    )
  }

  output$singlepowtable <- DT::renderDT({
    table_function_pow_single()
  })


  ########################################################################################################################################################################################
  #Calculation results
  # Power
  # comparative
  #  unpaired

  #Info about Paper
  output$compow1 <- shiny::renderUI({
    txt<- HTML("<h4 style = 'color:black;'>The power calculation in a comparative trial design is based on the article:</h4>
               <p style='font-size: 14px;'>Blinded sample size re-estimation in a comparative diagnostic accuracy study
     <br>written by <strong>Maria Stark, Mailin Hesse, Werner Brannath </strong> and <strong>Antonia Zapf</strong>.</p>")
    link <- tagList("URL Link:",doi_comparative )
    HTML(paste(txt,link))
  })

  #Calculation results
  output$power_unpaired <- shiny::renderPrint({
    shiny::validate(
      shiny::need(input$prev_up_pow != "", label = "Prevalenc")
    )
    shiny::validate(
      shiny::need(input$samplesize_up != "", label = "Sample size")
    )
    shiny::validate(
      shiny::need(input$alpha_up_pow != "", label = "Alpha ")
    )
    shiny::validate(
      shiny::need(input$se_ni_up_pow != "", label = "Comparator test sensitivity ")
    )
    shiny::validate(
      shiny::need(input$se_up_pow != "", label = "New test sensitivity ")
    )
    shiny::validate(
      shiny::need(input$sp_up_pow != "", label = "New test specificity")
    )
    shiny::validate(
      shiny::need(input$sp_ni_up_pow != "", label = "Comparator test specifitcity ")
    )
    shiny::validate(
      shiny::need(input$noninf_se_pow != "", label = "Non-inferiority margin sensitivity ")
    )
    shiny::validate(
      shiny::need(input$noninf_sp_pow != "", label = "Non-inferiority margin specificity ")
    )
    shiny::validate(
      shiny::need(input$drop_up_pow != "", label = "Dropout")
    )
    plausi_pow_unpaired(alpha = input$alpha_up_pow
                        ,prev = input$prev_up_pow
                        ,se.c = input$se_ni_up_pow
                        ,se.e = input$se_up_pow
                        ,sp.c = input$sp_ni_up_pow
                        ,sp.e = input$sp_up_pow
                        ,samplesize =input$samplesize_up
                        ,ni.se = input$noninf_se_pow
                        ,ni.sp = input$noninf_sp_pow
                        ,drop = input$drop_up_pow)
      print("The unpaired power calculation")
      power_calc(alpha = input$alpha_up_pow
                  ,prev = input$prev_up_pow
                  ,se.c = input$se_ni_up_pow
                  ,se.e = input$se_up_pow
                  ,sp.c = input$sp_ni_up_pow
                  ,sp.e = input$sp_up_pow
                  ,N =input$samplesize_up
                  ,ni.se = input$noninf_se_pow
                  ,ni.sp = input$noninf_sp_pow
                  ,paired = FALSE
                  ,psi.d = 0
                  ,psi.nd = 0
                  ,drop = input$drop_up_pow)

  })

  #Calculation results in text

  output$unpairedpower <- shiny::renderUI({
    plausi_pow_unpaired(alpha = input$alpha_up_pow
                        ,prev = input$prev_up_pow
                        ,se.c = input$se_ni_up_pow
                        ,se.e = input$se_up_pow
                        ,sp.c = input$sp_ni_up_pow
                        ,sp.e = input$sp_up_pow
                        ,samplesize =input$samplesize_up
                        ,ni.se = input$noninf_se_pow
                        ,ni.sp = input$noninf_sp_pow
                        ,drop = input$drop_up_pow)
      up_list = power_calc(alpha = input$alpha_up_pow
                                     ,prev = input$prev_up_pow
                                     ,se.c = input$se_ni_up_pow
                                     ,se.e = input$se_up_pow
                                     ,sp.c = input$sp_ni_up_pow
                                     ,sp.e = input$sp_up_pow
                                     ,N =input$samplesize_up
                                     ,ni.se = input$noninf_se_pow
                                     ,ni.sp = input$noninf_sp_pow
                                     ,paired = FALSE
                                     ,psi.d = 0
                                     ,psi.nd = 0
                                     ,drop = input$drop_up_pow)

      #Text
      inptxt <- HTML("<h4 style ='color: black;'>The calculation is based on the following input values:</h4>
                   <p style = 'font-size: 14px;'>prevalence: ",round((  input$prev_up_pow *100),digits=2),"%",
                     "<br> alpha:", round(( input$alpha_up_pow *100),digits=2),"% (in the calculations the one-sided alpha is used (alpha/2): ", (round((input$alpha_up_pow*100),digits = 2)/2),"%)",
                     "<br>sample size:", ceiling(input$samplesize_up),
                     "<br> comparator test sensitivity: ",round((input$se_ni_up_pow  *100),digits=2),"%",
                     "<br> comparator test specificity:", round((input$sp_ni_up_pow  *100),digits=2),"%",
                     "<br> sensitivity of the new test:", round((input$se_up_pow  *100),digits=2),"%",
                     "<br> specificity of the new test:", round((input$sp_up_pow  *100),digits=2),"%",
                     "<br> the non-inferiority limit for specificity:", round((input$noninf_sp_pow*100),digits =2),"%",
                     "<br> and the non-inferiority limit for the sensitvity:", round((input$noninf_se_pow*100),digits = 2), "%",
                    "<br> the dropout rate: ", round((input$drop_up_pow *100),digits = 2), "%",
                     "<br></p><h4 style = 'color:black;'> The results of the power calculation in a paired comaprative design:</h4>")

      resulttxt<- HTML("<p style = 'font-size: 14px;'>With a sample size of: ", ceiling(input$samplesize_up), "an overall power of:", (up_list[[1]]  *100),"% can be reached.",
                      "<br> The overall power can be split in sensitvity and specificity. The power of sensitivity is:",( up_list[[2]] *100),"%",
                      "<br> and the power of  specificity is:", (up_list[[3]]  *100),"%")

      HTML(paste( inptxt, resulttxt))


  })
  #Table  power comparative unpaired-----------------------------------------------------------------
  table_function_pow_unpaired <-function(){
    plausi_pow_unpaired(alpha = input$alpha_up_pow
                        ,prev = input$prev_up_pow
                        ,se.c = input$se_ni_up_pow
                        ,se.e = input$se_up_pow
                        ,sp.c = input$sp_ni_up_pow
                        ,sp.e = input$sp_up_pow
                        ,samplesize =input$samplesize_up
                        ,ni.se = input$noninf_se_pow
                        ,ni.sp = input$noninf_sp_pow
                        ,drop = input$drop_up_pow)

    up_list = power_calc(alpha = input$alpha_up_pow
                         ,prev = input$prev_up_pow
                         ,se.c = input$se_ni_up_pow
                         ,se.e = input$se_up_pow
                         ,sp.c = input$sp_ni_up_pow
                         ,sp.e = input$sp_up_pow
                         ,N =input$samplesize_up
                         ,ni.se = input$noninf_se_pow
                         ,ni.sp = input$noninf_sp_pow
                         ,paired = FALSE
                         ,psi.d = 0
                         ,psi.nd = 0
                         ,drop = input$drop_up_pow)

    table_results_pow_unpaired <- data.frame(
      Names = c("prevalence"
                ,"alpha (two-sided)"
                ,"sample size"
                ,"comparator test sensitivity"
                ,"comparator test specificity"
                ,"sensitivity of the new test"
                ,"specificity of the new test"
                ,"the non-inferiority limit for sensitivity"
                ,"the non-inferiority limit for specificity"
                ,"the dropout rate"
                ,"power total"
                ,"power sensitivity"
                ,"power specificity"),
      values = c(round((input$prev_up_pow*100),digits=2)
                 ,round((input$alpha_up_pow*100),digits = 2)
                 ,ceiling(input$samplesize_up)
                 ,round((input$se_ni_up_pow*100),digits=2)
                 ,round((input$sp_ni_up_pow*100),digits=2)
                 ,round((input$se_up_pow*100),digits=2)
                 ,round((input$sp_up_pow*100),digits = 2)
                 ,round((input$noninf_se_pow*100),digits=2)
                 ,round((input$noninf_sp_pow*100),digits =2)
                 ,round((input$drop_up_pow *100),digits = 2)
                 ,(up_list[[1]]*100)
                 ,(up_list[[2]]*100)
                 ,(up_list[[3]]*100))
    )
  }

  output$unpairedpowtable <- DT::renderDT({

    table_function_pow_unpaired()

  })
  ########################################################################################################################################################################################
  #Calculation results
  # Power
  # comparative
  #  paired

  # Info about Paper
  output$compow <- shiny::renderUI({
    txt<- HTML("<h4 style = 'color:black;'>The power calculation in a comparative trial design is based on the article:</h4>
                <p style = 'font-size: 14;'> Blinded sample size re-estimation in a comparative diagnostic accuracy study<br>
                written by <strong> Maria Stark, Mailin Hesse, Werner Brannath </strong> and <strong>Antonia Zapf</strong>.</p>")
    link <- tagList("URL Link:",doi_comparative )
    HTML(paste(txt, link))
  })
  #Results
  output$power_paired <- shiny::renderPrint({
    shiny::validate(
      shiny::need(input$prev_p_pow != "", label = "Prevalenc")
    )
    shiny::validate(
      shiny::need(input$samplesize_p != "", label = "Sample size")
    )
    shiny::validate(
      shiny::need(input$alpha_p_pow != "", label = "Alpha ")
    )
    shiny::validate(
      shiny::need(input$se_ni_p_pow != "", label = "Comparator test sensitivity ")
    )
    shiny::validate(
      shiny::need(input$se_p_pow != "", label = "New test sensitivity ")
    )
    shiny::validate(
      shiny::need(input$sp_p_pow != "", label = "New test specificity")
    )
    shiny::validate(
      shiny::need(input$sp_ni_p_pow != "", label = "Comparator test specifitcity ")
    )
    shiny::validate(
      shiny::need(input$noninf_se_pow != "", label = "Non-inferiority margin sensitivity ")
    )
    shiny::validate(
      shiny::need(input$noninf_sp_pow != "", label = "Non-inferiority margin specificity ")
    )
    shiny::validate(
      shiny::need(input$psi_d_p_pow != "", label = "Discordant test results diseased ")
    )
    shiny::validate(
      shiny::need(input$psi_nd_p_pow != "", label = "Discordant test results non-diseased")
    )
    shiny::validate(
      shiny::need(input$drop_p_pow != "", label = "Dropout")
    )
    plausi_pow_paired(alpha = input$alpha_p_pow
                        ,prev = input$prev_p_pow
                        ,se.c = input$se_ni_p_pow
                        ,se.e = input$se_p_pow
                        ,sp.c = input$sp_ni_p_pow
                        ,sp.e = input$sp_p_pow
                        ,samplesize =input$samplesize_p
                        ,ni.se = input$noninf_se_pow
                        ,ni.sp = input$noninf_sp_pow
                        ,psi.d = input$psi_d_p_pow
                        ,psi.nd = input$psi_nd_p_pow
                        ,drop = input$drop_p_pow)

      print("The paired power calculation")
      power_calc(alpha = input$alpha_p_pow
                   ,N = input$samplesize_p
                   ,prev = input$prev_p_pow
                   ,se.e = input$se_p_pow
                   ,se.c= input$se_ni_p_pow
                   ,sp.c = input$sp_ni_p_pow
                   ,sp.e= input$sp_p_pow
                   ,psi.d =input$psi_d_p_pow
                   ,psi.nd =input$psi_nd_p_pow
                   ,ni.se = input$noninf_se_pow
                   ,ni.sp = input$noninf_sp_pow
                   ,paired = TRUE
                   ,drop = input$drop_p_pow)

  })

  #Calculation results in text

  output$pairedpower <- shiny::renderUI({
    plausi_pow_paired(alpha = input$alpha_p_pow
                      ,prev = input$prev_p_pow
                      ,se.c = input$se_ni_p_pow
                      ,se.e = input$se_p_pow
                      ,sp.c = input$sp_ni_p_pow
                      ,sp.e = input$sp_p_pow
                      ,samplesize =input$samplesize_p
                      ,ni.se = input$noninf_se_pow
                      ,ni.sp = input$noninf_sp_pow
                      ,psi.d = input$psi_d_p_pow
                      ,psi.nd = input$psi_nd_p_pow
                      ,drop = input$drop_p_pow)

      p_list = power_calc(alpha = input$alpha_p_pow
                            ,N = input$samplesize_p
                            ,prev = input$prev_p_pow
                            ,se.e = input$se_p_pow
                            ,se.c= input$se_ni_p_pow
                            ,sp.c = input$sp_ni_p_pow
                            ,sp.e= input$sp_p_pow
                            ,psi.d =input$psi_d_p_pow
                            ,psi.nd =input$psi_nd_p_pow
                            ,ni.se = input$noninf_se_pow
                            ,ni.sp = input$noninf_sp_pow
                            ,paired = TRUE
                            ,drop = input$drop_p_pow)
      #Text
      inptxt <- HTML("<h4 style ='color: black;'> The calculation is based on the following input values:</h4>
                   <p style = 'font-size: 14;'> prevalence: ",round((input$prev_p_pow  *100),digits=2),"%",
                     "<br> alpha:", round((input$alpha_p_pow *100),digits=2),"% (in the calculations the one-sided alpha is used (alpha/2): ", (round((input$alpha_p_pow*100),digits = 2)/2),"%)",
                     "<br> sample size:", ceiling(input$samplesize_p),
                     "<br> comparator test sensitivity: ",round((input$se_ni_p_pow  *100),digits=2),"%",
                     "<br> comparator test specificity:",round(( input$sp_ni_p_pow *100),digits=2),"%",
                     "<br> sensitivity of the new test:",round((input$se_p_pow  *100),digits=2),"%",
                     "<br> specificity of the new test:", round((input$sp_p_pow  *100),digits=2),"%",
                     "<br> the discordant results of the diseased: ",round((input$psi_d_p_pow*100),digits = 2), "%",
                     "<br> the discordant results of the non-diseased: ",round((input$psi_nd_p_pow*100),digits = 2), "%",
                     "<br> the non-inferiority limit for specificity:", round((input$noninf_sp_pow*100),digits =2),"%",
                     "<br> and the non-inferiority limit for the sensitvity:", round((input$noninf_se_pow*100),digits = 2), "%",
                    "<br> the dropout rate: ", round((input$drop_p_pow *100), digits = 2), "%",
                     "<br></p><h4 style = 'color:black;'> The results of the power calculation in a paired comaprative design:</h4>")

      resulttxt<- HTML("<p style = 'font-size: 14px;'>With a sample size of: ", ceiling(input$samplesize_p), "an overall power of:", (p_list[[1]]  *100),"% can be reached.",
                       "<br> The overall power can be split in sensitvity and specificity. The power of sensitivity is:",( p_list[[2]] *100),"%",
                       "<br> and the power of  specificity is:", (p_list[[3]]  *100),"%")
      HTML(paste( inptxt, resulttxt))

  })

  #Table  power comparative paired-------------------------------------------------------------
  table_function_pow_paired <-function(){
    plausi_pow_paired(alpha = input$alpha_p_pow
                      ,prev = input$prev_p_pow
                      ,se.c = input$se_ni_p_pow
                      ,se.e = input$se_p_pow
                      ,sp.c = input$sp_ni_p_pow
                      ,sp.e = input$sp_p_pow
                      ,samplesize =input$samplesize_p
                      ,ni.se = input$noninf_se_pow
                      ,ni.sp = input$noninf_sp_pow
                      ,psi.d = input$psi_d_p_pow
                      ,psi.nd = input$psi_nd_p_pow
                      ,drop = input$drop_p_pow)

    p_list = power_calc(alpha = input$alpha_p_pow
                          ,N = input$samplesize_p
                          ,prev = input$prev_p_pow
                          ,se.e = input$se_p_pow
                          ,se.c= input$se_ni_p_pow
                          ,sp.c = input$sp_ni_p_pow
                          ,sp.e= input$sp_p_pow
                          ,psi.d =input$psi_d_p_pow
                          ,psi.nd =input$psi_nd_p_pow
                          ,ni.se = input$noninf_se_pow
                          ,ni.sp = input$noninf_sp_pow
                          ,paired = TRUE
                          ,drop = input$drop_p_pow)

    table_results_pow_paired <- data.frame(
      Names = c("prevalence"
                ,"alpha (two-sided)"
                ,"sample size"
                ,"comparator test sensitivity"
                ,"comparator test specificity"
                ,"sensitivity of the new test"
                ,"specificity of the new test"
                ,"the discordant reuslts of the diseased"
                ,"the discordant results of the non-diseased"
                ,"the non-inferiority limit for sensitivity"
                ,"the non-inferiority limit for specificity"
                ,"the dropout rate"
                ,"power total"
                ,"power sensitivity"
                ,"power specificity"),
      values = c(round((input$prev_p_pow*100),digits=2)
                 ,round((input$alpha_p_pow*100),digits = 2)
                 ,ceiling(input$samplesize_p)
                 ,round((input$se_ni_p_pow*100),digits=2)
                 ,round((input$sp_ni_p_pow*100),digits=2)
                 ,round((input$se_p_pow*100),digits=2)
                 ,round((input$sp_p_pow*100),digits = 2)
                 ,round((input$psi_d_p_pow*100),digits = 2)
                 ,round((input$psi_nd_p_pow*100),digits = 2)
                 ,round((input$noninf_se_pow*100),digits=2)
                 ,round((input$noninf_sp_pow*100),digits =2)
                ,round((input$drop_p_pow *100),digits =2)
                 ,(p_list[[1]]*100)
                 ,(p_list[[2]]*100)
                 ,(p_list[[3]]*100))
    )
  }

  output$pairedpowtable <- DT::renderDT({

      table_function_pow_paired()

  })
} #server bracket
