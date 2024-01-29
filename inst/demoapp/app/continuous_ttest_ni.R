library(plotly)
# library(lmom)
# library(EnvStats)
library(tidyverse)
#library(caret)
library(DT)
library(snowfall)
library(parallel)



# process <- function(parallel=FALSE,cpus=NULL){
#   sfInit(parallel=parallel,cpus=cpus)
#   #your function call
#   sfLapply(1:10^6,log10)
#   sfStop()
# }
#
# system.time(process(parallel = TRUE,cpus=4))

# rm(list = ls())

ncpus<-ifelse(detectCores()>2, detectCores()-1,1)


#' Title sample size(ss): number of subjects of each part, non_cn, cn_in, cn_out
#'
#' @param n sample size in global study
#' @param ratio randomization ratio trt/ctrl, ratio>=1
#' @param pct number of Chinese subjects/ number of subjects in global study
#' @param pct_in Number of Chinese subjects in global study/Number of Chinese subjects,
#'              # 0 means no Chinese subjects in global study, all Chinese subjects in a separate study
#'              # 1 means all Chinese subjects in global study
#' @param ratio_cn randomization ratio of CN subjects in global study, trt/ctrl
#'
#' @return a list
#' @export
#'
#' @examples
#' \dontrun{
#' ss_a = ss(232, ratio = 1, pct = 0.3, pct_in = 0)
#' }
ss <- function (n, ratio=1,ratio_cn=1, pct, pct_in = 1) {


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
    n_cn_out_trt = ceiling(n_cn_out * trt_pct)
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


#' simulating data and performing analysis
#'
#' @param ss The return list from function ss
#' @param mean_trt the mean of active group
#' @param mean_ctrl the mean of control group
#' @param nsim simulation times
#' @param SigL significance level
#' @param altHypo T test hypothesis
#' @param thd The consistency threshold
#' @param sd_ctrl SD of control group
#' @param sd_trt SD of active group
#' @param seed Simulaiton seed
#'
#' @return data.frame
#' @export
#'
#' @examples
#' \dontrun{
#' SimuData(ss=ss_a, mean_trt = 2,mean_ctrl = 3, sd = 2, nsim = 10)
#' }
SimuData<- function (ss,        # sample size
                     mean_trt,  #mean value of active drug
                     mean_ctrl, # mean value of control group
                     sd_ctrl= sd_ctrl,
                     sd_trt=sd_trt,
                     nsim=1000, #number of simulation
                     SigL=0.025,
                     altHypo="l",
                     thd=0.5,
                     nim=2, #non-inferiority margin(positive number)
                     hma=1,#higher means are 1=worse 0= better
                     seed=1212
)
{
    set.seed(seed = seed)

    single_simu<-function(nsim=1,ss=ss,mean_trt=mean_trt,mean_ctrl=mean_ctrl,
                          sd_ctrl= sd_ctrl,sd_trt=sd_trt,SigL=SigL,altHypo=altHypo,thd=thd,nim=nim,hma=hma)
    {
        # generate non-Chinese patients

        noncn_trt <- rnorm(ss$n_noncn_trt, mean_trt, sd_trt)
        noncn_ctrl <- rnorm(ss$n_noncn_ctrl, mean_ctrl, sd_ctrl)



        # generate Chinese patients in global study
        cn_in_trt <- rnorm(ss$n_cn_in_trt, mean_trt, sd_trt)
        cn_in_ctrl <- rnorm(ss$n_cn_in_ctrl, mean_ctrl, sd_ctrl)

        global_trt = if(ss$n_cn_in_trt == 0) {noncn_trt} else {c(noncn_trt, cn_in_trt)}
        global_ctrl = if(ss$n_cn_in_ctrl == 0) { noncn_ctrl} else { c(noncn_ctrl, cn_in_ctrl)}



        all <- t.test(global_trt, global_ctrl, alternative=altHypo, conf.level=1-SigL, conf.int = TRUE)

        # generate Chinese patients outside of global study
        cn_out_trt <- rnorm(ss$n_cn_out_trt, mean_trt, sd_trt)
        cn_out_ctrl <- rnorm(ss$n_cn_out_ctrl, mean_ctrl, sd_ctrl)

        cn_trt = if(ss$n_cn_out_trt == 0) {cn_in_trt} else {c(cn_out_trt, cn_in_trt)}
        cn_ctrl = if(ss$n_cn_out_ctrl == 0) {cn_in_ctrl} else {c(cn_out_ctrl, cn_in_ctrl)}


        cn <- t.test(cn_trt, cn_ctrl, alternative=altHypo, conf.level=1-SigL, conf.int = TRUE)

        cl_cn=cn$conf.int[1]
        cu_cn=cn$conf.int[2]
        cl_all=all$conf.int[1]
        cu_all=all$conf.int[2]

        #if higher means are worse(hma)=1, use upper limit;
        #if higher means are better=0, use lower limit;
        ci_cn=if(hma==1){cu_cn<nim}else{cl_cn>-nim}
        ci_all=if(hma==1){cu_all<nim}else{cl_all>-nim}

        ratio = if (hma==1){((cn$estimate[1]-cn$estimate[2])-nim) / ((all$estimate[1]-all$estimate[2])-nim)}
        else {((cn$estimate[1]-cn$estimate[2])+nim) / ((all$estimate[1]-all$estimate[2])+nim)}

        # conditions
        cond = ratio > thd & ci_all ==TRUE
        uncond = ratio > thd
        names(ratio)<-NULL
        names(cond)<-NULL
        names(uncond)<-NULL
        names(ci_cn)<-NULL
        names(ci_all)<-NULL

        list(pvalue=all$p.value, pi = ratio, cond=cond, uncond=uncond,pvalue_cn=cn$p.value,
             ci_cn=ci_cn,ci_all=ci_all)
    }

    rr<-replicate(nsim,single_simu(ss=ss,mean_trt=mean_trt,mean_ctrl=mean_ctrl,
                                   sd_ctrl= sd_ctrl, sd_trt=sd_trt,SigL=SigL,altHypo=altHypo,thd=thd,nim=nim,hma=hma),simplify=FALSE)


    return(data.frame(do.call(rbind,rr)))

    # process <- function(parallel=FALSE,cpus=NULL){
    #   sfInit(parallel=parallel,cpus=cpus)
    #   sfExport("ss")
    #   sfExport("mean_trt")
    #   sfExport("mean_ctrl")
    #   sfExport("sd")
    #   sfExport("SigL")
    #   sfExport("altHypo")
    #   sfExport("thd")
    #   sfClusterSetupRNGstream(seed = rep(1212,ncpus))
    #   res<<-sfLapply(1:nsim,single_simu,ss,mean_trt,mean_ctrl,sd,SigL,altHypo,thd)
    #   sfStop()
    # }
    #
    # print(system.time(process(parallel = TRUE,cpus=ncpus)))
    # return(data.frame(do.call(rbind,res)))
}

#
#' Do the final combo and test the consistency
#'
#' @param n sample size in global study
#' @param ratio randomization ration trt/ctrl, ratio>=1
#' @param pct number of Chinese subjects/ number of subjects in global study
#' @param pct_in Number of Chinese subjects in global study/Number of Chinese subjects,
#'              # 0 means no Chinese subjects in global study, all Chinese subjects in a separate study
#'              # 1 means all Chinese subjects in global study
#' @param mean_ctrl mean in control group
#' @param mean_trt mean in active group
#' @param nsim count of simulation
#' @param thd threshold
#' @param SigL significance level
#' @param altHypo alternative hypnosis
#' @param ratio_cn
#' @param sd_ctrl SD of control group
#' @param sd_trt SD of active group
#' @param seed simulation seed
#'
#' @return
#' @export
#'
#' @examples
ConsistecncyP_CE <- function(n, ratio=1, ratio_cn=1,pct = 0.2, pct_in = 1,
                             mean_ctrl=2.29, sd_ctrl=3.5,sd_trt=3.5,
                             mean_trt=3.5,
                             nsim=1000,
                             thd=0.5,
                             nim=2, #non-inferiority margin
                             hma=1,
                             SigL=0.025,
                             altHypo="l",
                             seed=1212) {

    ns = ss(n=n,ratio=ratio, ratio_cn=ratio_cn, pct=pct, pct_in = pct_in)


    res = SimuData(ss=ns, sd_ctrl= sd_ctrl, sd_trt=sd_trt,mean_ctrl = mean_ctrl,
                   mean_trt = mean_trt, nsim = nsim,
                   SigL=SigL,
                   altHypo=altHypo,
                   thd=thd,
                   nim=nim,
                   hma=hma,
                   seed=seed)


    #conditional power

    power_global=sum(res$ci_all==TRUE)/ nsim
    power_cn=sum(res$ci_cn==TRUE)/ nsim
    conp = round(sum(res$cond==TRUE) / sum(res$ci_all==TRUE),2)

    #unconditional power
    unconp = round(sum(res$uncond==TRUE) / nsim,2)
    pct=pct*100
    pct_in=pct_in*100
    conditional_p = conp
    unconditional_p  = unconp

    c(n_global=ns$n,n_chinese=ns$n_chinese,pct=pct,pct_in=pct_in,
      power_global=power_global,power_cn=power_cn, threshold=thd,
      conditional_p = conditional_p, unconditional_p = unconditional_p)

}


#' Final call for output
#'
#' @param chinese_all_in All Chinese subjects are in global study?, Global parameter
#'                        "yes, completely in" mapping with "yes"
#                         "no,  completely out" mapping with "no"
#                         "partially in" mapping with "part"
#' @param pct_seq Range of Number of Chinese subjects/Number of subjects in global study(%)
#'                Note: only show as a parameter if "yes, completely in" or "no,  completely out" for chinese_all_in selected
#'
#' @param pct_in_seq Range of Number of Chinese subjects in global study/Number of Chinese subjects(%)
#'                   Note: only show as parameter if "partially in" for Chinese_all_in selected
#' @param pct_in Number of Chinese subjects in global study/Number of Chinese subjects(%)
#'            Note only show as parameter if "partially in" for Chinese_all_in selected
#' @param n Sample size in global study ,Global parameter
#' @param ratio Randomization ratio active vs. control, Global parameter
#' @param mean_ctrl  Mean of control group
#' @param mean_trt Mean of active treatment
#' @param nsim Number of simulations, Global parameter
#' @param thd Threshold for Dcn/Doverall, Global parameter
#' @param SigL Significance level in global study, Global parameter
#' @param altHypo Alternative hypothesis :"two.sided" "greater" or "less"
#' @param chinese_all_in
#' @param ratio_cn
#' @param sd_ctrl SD of control group
#' @param sd_trt SD of active group
#' @param seed Simulaiton seed
#'
#' @return list, one data.frame and one plotly object
#' @export
#'
#' @examples
#' \dontrun{
#' plotSim(chinese_all_in = "yes", pct_seq = seq(0.1,0.2, by = 0.01),nsim=100000)
#' }
plotSim <- function(chinese_all_in = NULL,
                    pct_seq = NULL,
                    pct_in_seq = NULL,
                    pct = NULL,
                    n=232,
                    ratio=1,
                    ratio_cn=1,
                    mean_ctrl=4.5,
                    mean_trt=3.5,
                    sd_ctrl= 2.5,
                    sd_trt=2.5,
                    nsim=100,
                    thd=0.5,
                    nim=2, #non-inferiority margin
                    hma=1,
                    SigL=0.025,
                    altHypo="l" ,
                    seed=1212
) {

    if(chinese_all_in == "yes" || chinese_all_in == "no") {

        if(chinese_all_in == "yes" ) {pct_in = 1} else {pct_in = 0}

        if (nsim<=1000){

            result_df <- map_dfr(pct_seq, ~ConsistecncyP_CE(n=n, ratio=ratio,ratio_cn=ratio_cn,
                                                            pct = .x,
                                                            pct_in= pct_in,
                                                            mean_ctrl=mean_ctrl,
                                                            mean_trt=mean_trt,
                                                            sd_ctrl= sd_ctrl, sd_trt=sd_trt,
                                                            nsim=nsim,
                                                            thd=thd,
                                                            nim=nim,
                                                            hma=hma,
                                                            SigL=SigL,
                                                            altHypo=altHypo,
                                                            seed=seed))
        }else{
            process <- function(parallel=FALSE,cpus=NULL){
                sfInit(parallel=parallel,cpus=cpus)
                sfExport("ss")
                sfExport("SimuData")
                sfExport("ConsistecncyP_CE")
                sfExport("n")
                sfExport("ratio")
                sfExport("ratio_cn")
                sfExport("pct_in")
                sfExport("mean_trt")
                sfExport("mean_ctrl")
                sfExport("sd_ctrl")
                sfExport("sd_trt")
                sfExport("SigL")
                sfExport("altHypo")
                sfExport("thd")
                sfExport("nim")
                sfExport("hma")
                inner_ce<-partial(ConsistecncyP_CE,n=n, ratio=ratio, ratio_cn=ratio_cn,pct_in= pct_in,
                                  mean_ctrl=mean_ctrl, mean_trt=mean_trt, sd_ctrl= sd_ctrl, sd_trt=sd_trt,
                                  nsim=nsim, thd=thd, nim=nim, hma=hma,SigL=SigL, altHypo=altHypo, seed=seed)
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
            scale_y_continuous(limits = c(min(result_df1$prob),1)) +
            labs(x = "Number of Chinese subjects/Number of subjects in global study(%)",
                 y = "Probability of consistency") +
            theme_minimal()

        #ggplotly(g)



    } else {  # chinese_all_in == "part"

        if(nsim<=1000){
            result_df <- map_dfr(pct_in_seq, ~ConsistecncyP_CE(n=n, ratio=ratio,ratio_cn=ratio_cn,
                                                               pct = pct,
                                                               pct_in= .x,
                                                               mean_ctrl=mean_ctrl,
                                                               mean_trt=mean_trt,
                                                               sd_ctrl= sd_ctrl,
                                                               sd_trt=sd_trt,
                                                               nsim=nsim,
                                                               thd=thd,
                                                               nim=nim,
                                                               hma=hma,
                                                               SigL=SigL,
                                                               altHypo=altHypo,
                                                               seed=seed))
        }else{
            process <- function(parallel=FALSE,cpus=NULL){
                sfInit(parallel=parallel,cpus=cpus)
                sfExport("ss")
                sfExport("SimuData")
                sfExport("n")
                sfExport("ratio")
                sfExport("ratio_cn")
                sfExport("pct")
                sfExport("mean_trt")
                sfExport("mean_ctrl")
                sfExport("sd_ctrl")
                sfExport("sd_trt")
                sfExport("SigL")
                sfExport("altHypo")
                sfExport("thd")
                sfExport("nim")
                sfExport("hma")
                sfExport("nsim")
                inner_ce<-partial(ConsistecncyP_CE,n=n, ratio=ratio,ratio_cn=ratio_cn,pct= pct,
                                  mean_ctrl=mean_ctrl, mean_trt=mean_trt, sd_ctrl= sd_ctrl, sd_trt=sd_trt,
                                  nsim=nsim, thd=thd, nim=nim,hma=hma,SigL=SigL, altHypo=altHypo, seed=seed)
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
            scale_y_continuous(limits = c(min(result_df1$prob),1)) +
            labs(x = "Number of Chinese subjects in global study/Number of Chinese subjects(%)",
                 y = "Probability of consistency") +
            theme_minimal()
    }

    data2down <- result_df
    data2disp <- datatable(result_df, colnames = c("N of Global", "N of Subpopulation", "N of Subpopulation /N of Global (%)",
                                                   "N of Subpopulation within Global /N of Subpopulation (%)", "Power Global", "Power Subpopulation",
                                                   "Proportion of treatment effect preserved in Subpopulation",  "Conditional probability",
                                                   "Unconditional probability"),
                           options = list(scrollX = TRUE))

    #ggplotly(g)
    return(list(df1=data2down, df2=data2disp, plot=g))


}


#plotSim(chinese_all_in = "yes", ratio_cn=1,sd_ctrl= 2.5, sd_trt=3.5,pct_seq = seq(0.1,0.2, by = 0.01),nsim=500, seed=1212)

#plotSim(chinese_all_in = "part", pct_in_seq = seq(0,1, by = 0.1), pct = 0.2)

#test again
