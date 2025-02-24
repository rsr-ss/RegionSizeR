
library(tidyverse)

library(plotly)

library(parallel)
library(snowfall)
library(DT)


# rm(list = ls())

#Function1:
#sample size(ss): number of subjects of each part, non_cn, cn_in, cn_out
ss <- function (n, #sample size in global study
                ratio=1, #randomization ration trt/con, ratio>=1
                ratio_cn=1, #Chinese randomization ration trt/con, ratio>=1
                pct, #number of Chinese subjects/ number of subjects in global study
                pct_in = 1 #Number of Chinese subjects in global study/Number of Chinese subjects,
                # 0 means no Chinese subjects in global study, all Chinese subjects in a separate study
)               # 1 means all Chinese subjects in global study
{

  n_chinese = ceiling(n * round(pct, 6))
  n_cn_in = ceiling(n_chinese * round(pct_in, 6))

  n_noncn = n - n_cn_in
  n_cn_out = n_chinese - n_cn_in


  trt_pct = ratio / (1 + ratio)

  #number of global subjects
  n_trt = ceiling(n * trt_pct)
  n_ctrl = n - n_trt

  trt_pct_cn = ratio_cn / (1 + ratio_cn)
  #number of Chinese subjects in global study
  n_cn_in_trt = ceiling(n_cn_in * trt_pct_cn)
  n_cn_in_ctrl = n_cn_in - n_cn_in_trt

  #number of non-Chinese subjects
  n_noncn_trt = n_trt-n_cn_in_trt
  n_noncn_ctrl = n_noncn - n_noncn_trt

  #number of Chinese subjects outside of global study
  n_cn_out_trt = ceiling(n_cn_out * trt_pct_cn)
  n_cn_out_ctrl = n_cn_out - n_cn_out_trt


  list(
    n_noncn_trt  = n_noncn_trt,
    n_noncn_ctrl = n_noncn_ctrl,

    n_cn_in_trt = n_cn_in_trt,
    n_cn_in_ctrl = n_cn_in_ctrl,

    n_cn_out_trt = n_cn_out_trt,
    n_cn_out_ctrl = n_cn_out_ctrl,

    n_chinese=n_chinese,
    n=n,

    para = c(
      global = n,
      ratio = ratio,
      chinese_pct = pct,
      chinese_in_global_pct = pct_in
    )

  )
}


# ss_a = ss(300, ratio = 1, pct = 0.1, pct_in = 1)

# ss_b = ss(232, ratio = 1, pct = 0.2, pct_in = 0.5)



#Function2:
#simulating data and performing analysis
SimuData<- function (ss,        #sample size
                     seed=9527,      #Seed
                     prop_trt,  #Proportion in active drug arm
                     prop_ctrl, #Proportion in control arm
                     nsim=100, #number of simulation
                     SigL=0.025, #significance level, one side? two side? TBD later
                     thd=0.5 #the judgement threshold for China vs global. Use 0.5 as defined in Japanese guideline.
)
{
  set.seed(seed)

  single_simu<-function(nsim=1,ss=ss,prop_trt=prop_trt,prop_ctrl=prop_ctrl,
                        SigL=SigL,thd=thd)
  {
    # generate non-Chinese patients
    noncn_trt <- rbinom(ss$n_noncn_trt, 1, prop_trt)
    noncn_ctrl <- rbinom(ss$n_noncn_ctrl, 1, prop_ctrl)

    # generate Chinese patients in global study
    cn_in_trt <- rbinom(ss$n_cn_in_trt, 1, prop_trt)
    cn_in_ctrl <- rbinom(ss$n_cn_in_ctrl, 1, prop_ctrl)

    # combine into global patients
    global_trt = if(ss$n_cn_in_trt == 0) {noncn_trt} else {c(noncn_trt, cn_in_trt)}
    global_ctrl = if(ss$n_cn_in_ctrl == 0) { noncn_ctrl} else { c(noncn_ctrl, cn_in_ctrl)}

    yesgbtrt <- (ss$n_cn_in_trt + ss$n_noncn_trt)*mean(global_trt==1)
    yesgbctrl <- (ss$n_cn_in_ctrl + ss$n_noncn_ctrl)*mean(global_ctrl==1)
    nogbtrt <- (ss$n_cn_in_trt + ss$n_noncn_trt)*mean(global_trt==0)
    nogbctrl <- (ss$n_cn_in_ctrl + ss$n_noncn_ctrl)*mean(global_ctrl==0)

    gbchisqdata <- matrix(c(yesgbtrt,yesgbctrl,nogbtrt,nogbctrl),nrow=2,ncol=2)
    gbchisq <- chisq.test(gbchisqdata,correct=FALSE)

    # Check cells with less than 5 expected subject number in global
    gbexp <- chisq.test(gbchisqdata,correct=FALSE)$expected<5
    gbcheck <- gbexp[1,1]==TRUE|gbexp[1,2]==TRUE|gbexp[2,1]==TRUE|gbexp[2,2]==TRUE

    # generate Chinese patients outside of global study
    if(ss$n_cn_out_trt > 0 && ss$n_cn_out_ctrl > 0) {
      cn_out_trt <- rbinom(ss$n_cn_out_trt, 1, prop_trt)
      cn_out_ctrl <- rbinom(ss$n_cn_out_ctrl, 1, prop_ctrl)
    }

    cn_trt = if(ss$n_cn_out_trt == 0) {cn_in_trt} else {c(cn_out_trt, cn_in_trt)}
    cn_ctrl = if(ss$n_cn_out_ctrl == 0) {cn_in_ctrl} else {c(cn_out_ctrl, cn_in_ctrl)}

    yescntrt <- (ss$n_cn_in_trt + ss$n_cn_out_trt)*mean(cn_trt==1)
    yescnctrl <- (ss$n_cn_in_ctrl + ss$n_cn_out_ctrl)*mean(cn_ctrl==1)
    nocntrt <- (ss$n_cn_in_trt + ss$n_cn_out_trt)*mean(cn_trt==0)
    nocnctrl <- (ss$n_cn_in_ctrl + ss$n_cn_out_ctrl)*mean(cn_ctrl==0)

    cnchisqdata <- matrix(c(yescntrt,yescnctrl,nocntrt,nocnctrl),nrow=2,ncol=2)
    cnchisq <- chisq.test(cnchisqdata,correct=FALSE)

    # Check cells with less than 5 expected subject number in China
    cnexp <- chisq.test(cnchisqdata,correct=FALSE)$expected<5
    cncheck <- cnexp[1,1]==TRUE|cnexp[1,2]==TRUE|cnexp[2,1]==TRUE|cnexp[2,2]==TRUE


    # Calculate the proportion difference between two arms in China and in global
    # Then, the ratio of China proportion should be no less than 0.5 of global proportion.
    propcnctrl <- mean(cn_ctrl==1)
    propcntrt  <- mean(cn_trt==1)
    propgbctrl <- mean(global_ctrl==1)
    propgbtrt <- mean(global_trt==1)


    ratio = (propcnctrl-propcntrt) / (propgbctrl-propgbtrt)

    ## conditions
    cond = ratio > thd & gbchisq$p.value < SigL
    uncond = ratio > thd

    list(Global_p=gbchisq$p.value, CN_p=cnchisq$p.value, Ratio = ratio,
         cond=cond, uncond=uncond, gbcheck=gbcheck, cncheck=cncheck)
  }


  res<-replicate(nsim,single_simu(ss=ss,prop_trt=prop_trt,prop_ctrl=prop_ctrl,
                                  SigL=SigL,thd=thd),simplify=FALSE)

  return(data.frame(do.call(rbind,res)))
}


# test=SimuData(ss=ss_a, prop_trt = 0.3,prop_ctrl = 0.4, nsim = 1)



#
# #Function3:
# #Final combo
#
ConsistecncyP_CE <- function(n, seed,ratio=1,ratio_cn=1, pct = 0.2, pct_in = 1,
                             prop_trt=0.3,
                             prop_ctrl=0.4,
                             nsim=200,
                             thd=0.5,
                             SigL=0.025) {

  ns = ss(n=n,ratio=ratio, ratio_cn=ratio_cn, pct=pct, pct_in = pct_in)


  res = SimuData(ss=ns, seed,prop_trt=prop_trt, prop_ctrl=prop_ctrl, nsim = nsim,
                 SigL=SigL,
                 thd=thd)

  #delete na results
  del <- which(is.na(res$Ratio))
  if(identical(del,integer(0))!=TRUE) {res = res[-del,]}

  #conditional power
  conp = round(sum(res$cond==TRUE) / sum(res$Global_p < SigL),2)

  #unconditional power
  unconp = round(sum(res$uncond==TRUE) / nrow(res),2)


  power_global=round(sum(res$Global_p < SigL)/ nrow(res),2)
  power_cn=round(sum(res$CN_p < SigL)/ nrow(res),2)
  pct=pct*100
  pct_in=pct_in*100
  conditional_p = conp
  unconditional_p  = unconp

  gbcheck = sum(res$gbcheck==TRUE)
  cncheck = sum(res$cncheck==TRUE)

  c(n_global=ns$n,n_chinese=ns$n_chinese,pct=pct,pct_in=pct_in,
    power_global=power_global,power_cn=power_cn, threshold=thd,
    conditional_p = conditional_p, unconditional_p = unconditional_p, gbcheck=gbcheck, cncheck=cncheck)

}

#ConsistecncyP_CE(n=300, ratio=1,ratio_cn=1, pct = 0.2, pct_in = 1,prop_trt=0.3,prop_ctrl=0.4,nsim=2)







# chinese_all_in = All Chinese subjects are in global study?
#                  "yes, completely in" mapping with "yes"
#                  "no,  completely out" mapping with "no"
#                  "partially in" mapping with "part"


plotSim <- function(chinese_all_in = NULL,  #G
                    pct_seq = NULL,# Range of Number of Chinese subjects/Number of subjects in global study(%)
                    # Note: only show as a parameter if "yes, completely in" or "no,  completely out" for chinese_all_in selected
                    pct_in_seq = NULL,# Range of Number of Chinese subjects in global study/Number of Chinese subjects(%)
                    # Note: only show as parameter if "partially in" for Chinese_all_in selected
                    pct = NULL, # Number of Chinese subjects in global study/Number of Chinese subjects(%)
                    # Note only show as parameter if "partially in" for Chinese_all_in selected
                    n=300, # Sample size in global study G
                    seed=123, # Randomization seed
                    ratio=1, # Randomization ratio active vs. control G
                    ratio_cn=1,# Chinese randomization ratio active vs. control G
                    prop_trt=0.4, # Proportion of treatment group
                    prop_ctrl=0.3, # Proportion of placebo treatment
                    nsim=100, # Number of simulations G
                    thd=0.5, # Threshold for Dcn/Doverall G
                    SigL=0.025
) {

  if(chinese_all_in == "yes" || chinese_all_in == "no") {

    if(chinese_all_in == "yes" ) {pct_in = 1} else {pct_in = 0}

    if (n*nsim<=10000){
      result_df <- map_dfr(pct_seq, ~ConsistecncyP_CE(n=n, seed=seed,ratio=ratio, ratio_cn=ratio_cn,
                                                      pct = .x,
                                                      pct_in= pct_in,
                                                      prop_trt=prop_trt,
                                                      prop_ctrl=prop_ctrl,
                                                      nsim=nsim,
                                                      thd=thd,
                                                      SigL=SigL))
    }else{
      process <- function(parallel=FALSE,cpus=NULL){
        sfInit(parallel=parallel,cpus = detectCores() - 1)
        sfExport("ss")
        sfExport("SimuData")
        sfExport("ConsistecncyP_CE")
        sfExport("n")
        sfExport("ratio")
        sfExport("ratio_cn")
        sfExport("pct_in")
        sfExport("prop_trt")
        sfExport("prop_ctrl")
        sfExport("SigL")
        sfExport("thd")
        inner_ce<-partial(ConsistecncyP_CE,n=n, seed=seed,ratio=ratio, ratio_cn=ratio_cn,pct_in= pct_in,
                          prop_trt=prop_trt, prop_ctrl=prop_ctrl,
                          nsim=nsim, thd=thd, SigL=SigL)
        res<-sfLapply(pct_seq,inner_ce)
        sfStop()

        res
      }

      print(system.time({res = process(parallel = TRUE,cpus=ncpus)}))
      result_df<-data.frame(do.call(rbind,res))
    }


    #result_df
    result_df1 = result_df %>%
      pivot_longer(cols = c("conditional_p", "unconditional_p"),
                   names_to = "consistency",
                   values_to = "prob")

    g = ggplot(result_df1, aes(x=pct)) +
      geom_point(aes(y=prob, color = consistency) ) +
      scale_y_continuous(limits = c(0.5,1)) +
      labs(x = "Number of Chinese subjects/Number of subjects in global study(%)",
           y = "Probability of consistency") +
      theme_minimal()

    #ggplotly(g)

  } else {  # chinese_all_in == "part"
    if(n*nsim<=10000){
      result_df <- map_dfr(pct_in_seq, ~ConsistecncyP_CE(n=n, seed=seed, ratio=ratio, ratio_cn=ratio_cn,
                                                         pct = pct,
                                                         pct_in= .x,
                                                         prop_trt=prop_trt,
                                                         prop_ctrl=prop_ctrl,
                                                         nsim=nsim,
                                                         thd=thd,
                                                         SigL=SigL))
    }else{
      process <- function(parallel=FALSE,cpus=NULL){
        sfInit(parallel=parallel,cpus = detectCores() - 1)
        sfExport("ss")
        sfExport("SimuData")
        sfExport("n")
        sfExport("ratio")
        sfExport("ratio_cn")
        sfExport("pct")
        sfExport("prop_trt")
        sfExport("prop_ctrl")
        sfExport("SigL")
        sfExport("thd")
        sfExport("nsim")
        inner_ce<-partial(ConsistecncyP_CE,n=n, seed=seed, ratio=ratio,ratio_cn=ratio_cn,pct= pct,
                          prop_trt=prop_trt, prop_ctrl=prop_ctrl,
                          nsim=nsim, thd=thd, SigL=SigL)
        res<-sfLapply(pct_in_seq,inner_ce)
        sfStop()

        res
      }

      print(system.time({res = process(parallel = TRUE,cpus=ncpus)}))
      result_df<-data.frame(do.call(rbind,res))
    }


    #result_df
    result_df1 = result_df %>%
      pivot_longer(cols = c("conditional_p", "unconditional_p"),
                   names_to = "consistency",
                   values_to = "prob")

    g = ggplot(result_df1, aes(x=pct_in)) +
      geom_point(aes(y=prob, color = consistency) ) +
      scale_y_continuous(limits = c(0.5,1)) +
      labs(x = "Number of Chinese subjects in global study/Number of Chinese subjects(%)",
           y = "Probability of consistency") +
      theme_minimal()
  }
  # ggplotly(g)

  data2down <- result_df
  data2disp <- datatable(result_df, colnames = c('N of Global', 'N of Subpopulation', 'N of Subpopulation/N of Global',
                                                       'N of Subpopulation within Global/N of Subpopulation',
                                                      'Power Global', 'Power Subpopulation','Proportion of treatment effect preserved in Subpopulation',
                                                     'conditional probability', 'Unconditional probability', 'Number of simulation with less than 5 expected cell count in global',
                                                    'Number of simulation with less than 5 expected cell count in subpopulation'),

                        options = list(scrollX = TRUE))
  return(list(df1=data2down, df2=data2disp,plot=g))

}




#plotSim(chinese_all_in = "part", pct_in_seq = seq(0, 1, by = 0.1), pct = 0.2)

#plotSim(chinese_all_in = "yes", pct_seq = seq(0.2,0.8, by = 0.2), n=700, nsim = 10000, seed=22467)

