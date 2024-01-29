
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
  n_chinese = ceiling(n * pct)
  n_cn_in = ceiling(n_chinese * pct_in)

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
  #Changed the trt_pct variable to trt_pct_cn as in Ran's email on May 17th
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


#ss_a = ss(300, ratio = 1, pct = 0.1, pct_in = 1)


 #Function2:
 #simulating data and performing analysis
 SimuData<- function (ss,        #sample size
                      seed=9527,      #Seed
                      prop_trt,  #Proportion in active drug arm
                      prop_ctrl, #Proportion in control arm
                      nsim=100, #number of simulation
                      SigL=0.05, #significance level
                      thd=0.5, #the judgement threshold for China vs global. Use 0.5 as defined in Japanese guideline.
                      hma=1, #higher means are
                      nim=0.05 #non-inferiority margin
 )
 {
   set.seed(seed)

   single_simu<-function(nsim=1,ss=ss,prop_trt=prop_trt,prop_ctrl=prop_ctrl,
                         SigL=SigL,thd=thd,hma=hma,nim=nim)
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


     # generate Chinese patients outside of global study
     if(ss$n_cn_out_trt > 0 && ss$n_cn_out_ctrl > 0)
     {
       cn_out_trt <- rbinom(ss$n_cn_out_trt, 1, prop_trt)
       cn_out_ctrl <- rbinom(ss$n_cn_out_ctrl, 1, prop_ctrl)
     }

     cn_trt = if(ss$n_cn_out_trt == 0) {cn_in_trt} else {c(cn_out_trt, cn_in_trt)}
     cn_ctrl = if(ss$n_cn_out_ctrl == 0) {cn_in_ctrl} else {c(cn_out_ctrl, cn_in_ctrl)}

     yescntrt <- (ss$n_cn_in_trt + ss$n_cn_out_trt)*mean(cn_trt==1)
     yescnctrl <- (ss$n_cn_in_ctrl + ss$n_cn_out_ctrl)*mean(cn_ctrl==1)
     nocntrt <- (ss$n_cn_in_trt + ss$n_cn_out_trt)*mean(cn_trt==0)
     nocnctrl <- (ss$n_cn_in_ctrl + ss$n_cn_out_ctrl)*mean(cn_ctrl==0)


     # Calculate the 95% CI for proportions for China and global
     # Replace R prop.test function by binomial formula to match standard results
     # In this program, pool variance is used to match PASS and SAS outputs.
     # Like PASS, add 0.0001 to zero cell counts.

     #GB results with adjustment on zero counts
     yesgbtrt = if(yesgbtrt==0){0.0001} else {yesgbtrt}
     yesgbctrl = if(yesgbctrl==0){0.0001} else {yesgbctrl}
     nogbtrt = if(nogbtrt==0){0.0001} else {nogbtrt}
     nogbctrl = if(nogbctrl==0){0.0001} else {nogbctrl}

     gbtrtp <- yesgbtrt/(yesgbtrt+nogbtrt)
     gbctrlp <- yesgbctrl/(yesgbctrl+nogbctrl)
     gbdiffp <- gbtrtp-gbctrlp

     gbtrt <- yesgbtrt+nogbtrt
     gbctrl <- yesgbctrl+nogbctrl
     gbpmean <- (gbtrtp*gbtrt+gbctrlp*gbctrl)/(gbtrt+gbctrl)
     gbvar <- sqrt(gbpmean*(1-gbpmean)*(1/gbtrt+1/gbctrl))
     gbLL <- gbdiffp-qnorm(0.975,0,1)*gbvar
     gbUL <- gbdiffp+qnorm(0.975,0,1)*gbvar



     #CN results with adjustment on zero counts
     yescntrt = if(yescntrt==0){0.0001} else {yescntrt}
     yescnctrl = if(yescnctrl==0){0.0001} else {yescnctrl}
     nocntrt = if(nocntrt==0){0.0001} else {nocntrt}
     nocnctrl = if(nocnctrl==0){0.0001} else {nocnctrl}

     cntrtp <- yescntrt/(yescntrt+nocntrt)
     cnctrlp <- yescnctrl/(yescnctrl+nocnctrl)
     cndiffp <- cntrtp-cnctrlp

     cntrt <- yescntrt+nocntrt
     cnctrl <- yescnctrl+nocnctrl
     cnpmean <- (cntrtp*cntrt+cnctrlp*cnctrl)/(cntrt+cnctrl)
     cnvar <- sqrt(cnpmean*(1-cnpmean)*(1/cntrt+1/cnctrl))
     cnLL <- cndiffp-qnorm(0.975,0,1)*cnvar
     cnUL <- cndiffp+qnorm(0.975,0,1)*cnvar


     # Set condition to upper bound of 95% CI less than nim when hma = 1 "worse"
     # or lower bound of 95% CI larger than -nim when hma = 0 "better"

     GNI = (hma == 1 && gbUL < nim)|(hma == 0 && gbLL > -nim)
     CNI = (hma == 1 && cnUL < nim)|(hma == 0 && cnLL > -nim)

     # Calculate the consistency ratio for non-inferiority test
     ratio = (cndiffp-nim) / (gbdiffp-nim)

     ## conditions

     cond = (ratio > thd & GNI == TRUE)
     uncond = (ratio > thd)

     list(GNI=GNI, CNI=CNI, Ratio=ratio, cond=cond, uncond=uncond)
   }

   res<-replicate(nsim,single_simu(ss=ss,prop_trt=prop_trt,prop_ctrl=prop_ctrl,
                                   SigL=SigL,thd=thd,nim=nim,hma=hma),simplify=FALSE)

   return(data.frame(do.call(rbind,res)))
 }


#test=SimuData(ss=ss_a, prop_trt = 0.3,prop_ctrl = 0.4, nsim = 10,hma=1,nim=0.1)



#
# #Function3:
# #Final combo
#
ConsistecncyP_CE <- function(n, seed,ratio=1,ratio_cn=1, pct = 0.2, pct_in = 1,
                             prop_trt=0.3,
                             prop_ctrl=0.4,
                             nsim=200,
                             thd=0.5,
                             SigL=0.05,
                             hma=1,
                             nim=0.05)
{

  ns = ss(n=n,ratio=ratio, ratio_cn=ratio_cn, pct=pct, pct_in = pct_in)

  res = SimuData(ss=ns, seed,prop_trt=prop_trt, prop_ctrl=prop_ctrl, nsim = nsim,
                 SigL=SigL,thd=thd,hma=hma,nim=nim )

  #delete na results
  del <- which(is.na(res$Ratio))
  if(identical(del,integer(0))!=TRUE) {res=res[-del,]}
  #print(del) there are some NA results when sample size is small.

  #conditional power
  conp = round(sum(res$cond==TRUE)/sum(res$GNI==TRUE),2)

  #unconditional power
  unconp = round(sum(res$uncond==TRUE)/nrow(res),2)

  power_global=round(sum(res$GNI==TRUE)/ nrow(res),2)
  power_cn=round(sum(res$CNI==TRUE)/ nrow(res),2)
  pct=pct*100
  pct_in=pct_in*100
  conditional_p = conp
  unconditional_p  = unconp

  c(n_global=ns$n,n_chinese=ns$n_chinese,pct=pct,pct_in=pct_in,
    power_global=power_global,power_cn=power_cn, threshold=thd,
    conditional_p = conditional_p, unconditional_p = unconditional_p)

}

#ConsistecncyP_CE(n=100, seed=22467,ratio=1,ratio_cn=1, pct = 0.20, pct_in = 1,prop_trt=0.3,prop_ctrl=0.4,nsim=10000,hma=1,nim=0.1)



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
                    SigL=0.025,
                    hma=1,
                    nim=0.05
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
                                                      SigL=SigL,
                                                      hma=hma,
                                                      nim=nim))
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
        sfExport("hma")
        sfExport("nim")
        inner_ce<-partial(ConsistecncyP_CE,n=n, seed=seed,ratio=ratio, ratio_cn=ratio_cn,pct_in= pct_in,
                          prop_trt=prop_trt, prop_ctrl=prop_ctrl,
                          nsim=nsim, thd=thd, SigL=SigL,hma=hma,
                          nim=nim)
        res<-sfLapply(pct_seq,inner_ce)
        sfStop()
        res
      }

      print(system.time(res = process(parallel = TRUE,cpus=ncpus)))
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
                                                         SigL=SigL,
                                                         hma=hma,
                                                         nim=nim))
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
        sfExport("hma")
        sfExport("nim")
        inner_ce<-partial(ConsistecncyP_CE,n=n, seed=seed, ratio=ratio,ratio_cn=ratio_cn,pct= pct,
                          prop_trt=prop_trt, prop_ctrl=prop_ctrl,
                          nsim=nsim, thd=thd, SigL=SigL,hma=hma,
                          nim=nim)
        res<-sfLapply(pct_in_seq,inner_ce)
        sfStop()

        res
      }

      print(system.time(res = process(parallel = TRUE,cpus=ncpus)))
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
                                                     'conditional probability', 'Unconditional probability'),

                        options = list(scrollX = TRUE))
  return(list(df1=data2down, df2=data2disp,plot=g))

}




#Small sample size
#plotSim(chinese_all_in = "yes", pct_seq = seq(0.1,0.5, by = 0.1), n=200,
#         prop_trt=0.4,prop_ctrl=0.4, nsim = 10000, seed=22467,thd=0.5, SigL=0.05, hma=1,nim=0.1)






