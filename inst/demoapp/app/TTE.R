#######################Leading parameters#######################
## @param pattern		            CN_in has the same recruitment pattern with global main study?
#1 - same pattern; 0 different pattern
## @param chinese_all_in:       All CN subjects are in global study?
#0 - completely out; 1 - completely in; 2 - partially in
#######################Need to be set up for all scenarios#######################
## @param n:                    overall sample size (global main study)
## @param ratio:                randomization ratio (trt vs ctrl, e.g., 1 means 1:1, 2 means 2:1)
## @param ratio_cn:             randomization ratio - CN_in (trt vs ctrl, e.g., 1 means 1:1, 2 means 2:1)
## @param enroll_len:           accrual time (month) - global main study
## @param ms0:                  median survival time - control group
## @param hr:                   target hazard ratio - trt vs. ctrl
## @param drop_ctrl:            dropout rate at 12 month - control group
## @param drop_trt:             dropout rate at 12 month - trt group
## @param n_targetevent:        number of target event - global main study
## @param nsim:                 number of simulation times
## @param SigL:                 significant level
## @param thd:                  threshold for consistency
## @param seed：                  Simulation seed

###########
#pattern#
###########
#######################When pattern = 1#######################
## @param unif_global:          uniform recruitment - global main study: YES or NO
## @param recruit_global:       customized recruitment - global main study (when unif_global = 0)
#######################When pattern = 0#######################
## @param unif_noncn:           uniform recruitment - Non-CN in global main study: YES or NO
## @param recruit_noncn:        customized recruitment - Non-CN in global main study (when unif_noncn = 0)
#######################When pattern = 0 & chinese_all_in = (1||2)#######################
## @param unif_cn_in:           uniform recruitment - CN in global main study: YES or NO
## @param recruit_cn_in:        customized recruitment - CN in global main study (when unif_cn_in = 0)

##################
#chinese_all_in#
##################
#######################When chinese_all_in = 1#######################
## @param enroll_st_cn:         starting point (month) of recruitment - CN subjects in global main study
## @param pct_seq:              Range of Number of CN subjects/Number of subjects in global main study(%)
#######################When chinese_all_in = 0#######################
## @param enroll_len_out:       accrual time (month) - CN subjects out of global main study
## @param fu_len_out:           follow up time (month) - CN subjects out of global main study
## @param unif_cn_out:          uniform recruitment - CN out of global main study: YES or NO
## @param recruit_cn_out:       customized recruitment - CN out of global main study(when unif_cn_out = 0)
## @param pct_seq:              Range of Number of CN subjects/Number of subjects in global main study(%)
#######################When chinese_all_in = 2#######################
## @param enroll_st_cn:         starting point (month) of recruitment - CN subjects in global main study
## @param enroll_len_out:       accrual time (month) - CN subjects out of global main study
## @param fu_len_out:           follow up time (month) - CN subjects out of global main study
## @param unif_cn_out:          uniform recruitment - CN out of global main study: YES or NO
## @param recruit_cn_out:       customized recruitment - CN out of global main study(when unif_cn_out = 0)
## @param pct:                  number of CN subjects / number of overall sample size
## @param pct_in_seq:           Range of Number of CN subjects in global main study/Number of CN subjects(%)
library(survival)
library(tidyverse)

library(parallel)
library(snowfall)

# rm(list = ls())
##########################################################################
#Sample size#
##########################################################################
Samplesize <- function(n, ratio, ratio_cn, pct, pct_in)
{
    n_chinese = ceiling(n * round(pct, 6))
    n_cn_in = ceiling(n_chinese * round(pct_in, 6))

    n_noncn <- n - n_cn_in
    n_cn_out <- n_chinese - n_cn_in

    trt_pct <- ratio / (1 + ratio)
    trt_pct_cn <- ratio_cn / (1 + ratio_cn)

    #number of CN subjects in global study
    n_cn_in_trt <- ceiling(n_cn_in * trt_pct_cn)
    n_cn_in_ctrl <- n_cn_in - n_cn_in_trt

    #number of non-CN subjects
    n_noncn_trt <- ceiling(n * trt_pct) - n_cn_in_trt
    n_noncn_ctrl <- n_noncn - n_noncn_trt

    #number of CN subjects outside of global study
    n_cn_out_trt <- ceiling(n_cn_out * trt_pct_cn)
    n_cn_out_ctrl <- n_cn_out - n_cn_out_trt

    list (n = n, n_noncn = n_noncn,
          n_chinese = n_chinese, n_cn_in = n_cn_in, n_cn_out = n_cn_out,
          n_noncn_ctrl = n_noncn_ctrl, n_noncn_trt = n_noncn_trt,
          n_cn_in_ctrl = n_cn_in_ctrl, n_cn_in_trt = n_cn_in_trt,
          n_cn_out_ctrl = n_cn_out_ctrl, n_cn_out_trt = n_cn_out_trt,
          pct_in=pct_in)
}


################################################################################
#Customized recruitment#
################################################################################
gen_ttr <- function(enroll_start, enroll_len, month_recruit){
    a <- rep((enroll_start-1):(enroll_len-1), month_recruit)
    b <- runif(sum(month_recruit))
    a+b
}

################################################################################
#Simulating data PART and performing analysis#
################################################################################

SimuData <- function (pattern, ss,#sample size
                      enroll_len, enroll_st_cn = NULL,
                      enroll_len_out = NULL, fu_len_out = NULL,
                      unif_global = NULL, recruit_global = NULL,
                      unif_noncn = NULL, recruit_noncn = NULL,
                      unif_cn_in = NULL, recruit_cn_in = NULL,
                      unif_cn_out = NULL, recruit_cn_out = NULL,
                      ms0, hr, drop_ctrl, drop_trt, n_targetevent)
{
    ############################################################
    ####Simulating data####

    ##Time to recruitment (ttr)##
    ##CN_in has same recruitment pattern with global main study##
    if(pattern == 1){
        #recruitment of global main study
        if(unif_global == 1){
            ttr_main <- c(seq(0, enroll_len, length.out = ss$n))
        }else{
            ttr_main = gen_ttr(1, enroll_len, recruit_global)
        }

        # select CN_in from global main study
        cn_index <- sample(which(ttr_main >= enroll_st_cn - 1), ss$n_cn_in)
        CN_in <- rep(0, ss$n)
        CN_in[cn_index] <- 1

        main_global <- c(rep(1, ss$n))
    }

    ##CN_in has different recruitment pattern with global main study##
    if(pattern == 0){
        #recruitment of Non-Chinese
        if(unif_noncn == 1){
            ttr_noncn <- c(seq(0, enroll_len, length.out = ss$n_noncn))
        }else{
            ttr_noncn = gen_ttr(1, enroll_len, recruit_noncn)
        }

        #recruitment of CN within global main study
        if(ss$pct_in != 0){
            if(unif_cn_in == 1){
                ttr_cn_in <- c(seq(enroll_st_cn - 1, enroll_len, length.out = ss$n_cn_in))
            }else{
                ttr_cn_in = gen_ttr(enroll_st_cn, enroll_len, recruit_cn_in)
            }

            ttr_main <- c(ttr_noncn, ttr_cn_in)
        }else{
            ttr_main <- ttr_noncn
        }

        CN_in <- flatten_int(map2(c(0L,1L),c(ss$n_noncn,ss$n_cn_in),rep))
        main_global <- c(rep(1, ss$n))
    }

    #recruitment of CN outside global main study
    if(ss$pct_in != 1){
        if(unif_cn_out == 1){
            ttr_cn_out <- c(seq(0, enroll_len_out, length.out = ss$n_cn_out))
        }else{
            ttr_cn_out = gen_ttr(1, enroll_len_out, recruit_cn_out)
        }

        CN_out <- c(rep(1, ss$n_cn_out))
        main_out <- c(rep(0, ss$n_cn_out))

        #Recruitment - All: main + CN outside
        ttr <- c(ttr_main, ttr_cn_out)
        CN <- c(CN_in, CN_out)
        main <- c(main_global, main_out)
    }else{
        ttr<- ttr_main
        CN <- CN_in
        main <- main_global
    }
    #data_ttr <- data.frame(ttr, CN, main)
    data_ttr <- tibble(ttr, CN, main) %>%
        arrange(desc(main), CN, runif(ss$n + ss$n_cn_out))

    ##Treatment: 0="Control", 1="Test"##
    treat_noncn <- flatten_int(map2(c(0L,1L),c(ss$n_noncn_ctrl,ss$n_noncn_trt),rep))
    treat_cn_in <- flatten_int(map2(c(0L,1L),c(ss$n_cn_in_ctrl,ss$n_cn_in_trt),rep))
    treat_cn_out <- flatten_int(map2(c(0L,1L),c(ss$n_cn_out_ctrl,ss$n_cn_out_trt),rep))
    treat <- c(treat_noncn, treat_cn_in, treat_cn_out)

    ##Time to event (tte)##
    tte_noncn <- flatten_dbl(map2(c(ss$n_noncn_ctrl,ss$n_noncn_trt),
                                  c(-log(0.5)/ms0,-log(0.5)/(ms0/hr)),rexp))
    tte_cn_in <- flatten_dbl(map2(c(ss$n_cn_in_ctrl,ss$n_cn_in_trt),
                                  c(-log(0.5)/ms0,-log(0.5)/(ms0/hr)),rexp))
    tte_cn_out <- flatten_dbl(map2(c(ss$n_cn_out_ctrl,ss$n_cn_out_trt),
                                   c(-log(0.5)/ms0,-log(0.5)/(ms0/hr)),rexp))

    tte <- c(tte_noncn, tte_cn_in, tte_cn_out)

    ##Time to dropout (ttd)##
    ttd_noncn <- flatten_dbl(map2(c(ss$n_noncn_ctrl,ss$n_noncn_trt),
                                  c(-log(1 - drop_ctrl)/12,-log(1 - drop_trt)/12),rexp))
    ttd_cn_in <- flatten_dbl(map2(c(ss$n_cn_in_ctrl,ss$n_cn_in_trt),
                                  c(-log(1 - drop_ctrl)/12,-log(1 - drop_trt)/12),rexp))
    ttd_cn_out <- flatten_dbl(map2(c(ss$n_cn_out_ctrl,ss$n_cn_out_trt),
                                   c(-log(1 - drop_ctrl)/12,-log(1 - drop_trt)/12),rexp))
    ttd <- c(ttd_noncn, ttd_cn_in, ttd_cn_out)

    #Define dropout:0="No dropout", 1="Dropout"
    #Adjust tte, Create time to event from first subject recruitment (ttre)
    data_tte <- tibble(treat, tte, ttd)
    data_all <- bind_cols(data_ttr, data_tte)

    data_all = data_all %>%
        mutate(drop = ifelse(tte > ttd & !is.nan(ttd), 1, 0), #consider dropout rate=0
               tte_a = ifelse(tte > ttd & !is.nan(ttd), ttd, tte),
               ttre = tte_a + ttr) %>%
        arrange(ttre)

    ##Find Target event date, Re-define time to event, censor(1="event", 0="censor")##
    #Find the target event date for global main study
    target_eventdate <- data_all %>%
        filter(drop == 0,main == 1) %>%
        summarize(
            nth(ttre, n_targetevent)
        ) %>% as.double()

    data_all = data_all %>%
        mutate(
            targeteventdate = ifelse(main == 1, target_eventdate, enroll_len_out + fu_len_out),
            ttre_a = ifelse(ttre > targeteventdate, targeteventdate, ttre),
            aval = case_when(ttre > targeteventdate ~ targeteventdate - ttr,
                             drop == 0 ~ tte,
                             drop == 1 ~ tte_a),
            censor = ifelse(ttre <= targeteventdate & drop == 0, 1, 0)
        )

    ############################################################
    ####Performing analysis####

    ##Results - cox regression##
    #Main study
    subdata_main <- data_all%>%filter(main == 1)
    cox_ph_main <- summary(coxph(Surv(aval, censor) ~ as.factor(treat),
                                 data = subdata_main))

    #CN subjects
    subdata_cn <- data_all%>%filter(CN == 1)
    cox_ph_cn <- summary(coxph(Surv(aval, censor)~as.factor(treat),
                               data = subdata_cn))

    coef <- c(cox_ph_main$coefficients[1], cox_ph_cn$coefficients[1]) #log(HR)
    expcoef <- c(cox_ph_main$coefficients[2], cox_ph_cn$coefficients[2]) #Hazard ratio
    p_main <- cox_ph_main$sctest["pvalue"]
    p_cn <- cox_ph_cn$sctest["pvalue"]
    n_event <- c(cox_ph_main$nevent, cox_ph_cn$nevent)
    median_dur <- c(median(data_all$aval), median(subdata_cn$aval))

    ##Results - coef, expcoef, p_main, n_event##
    statistics <- c(coef, expcoef, p_main, p_cn, n_event, target_eventdate, median_dur)
    names(statistics) <- c("logHR_main", "logHR_cn", "HR_main", "HR_cn",
                           "p_main", "p_cn", "n_event_main", "n_event_cn", "targeteventdate",
                           "median_dur_main", "median_dur_cn")
    return(statistics)
}


#########################
## Simulation Results ##
#########################
ConsistecncyP_CE <- function(pattern, n, ratio, ratio_cn, pct, pct_in,
                             enroll_len, enroll_st_cn = NULL,
                             enroll_len_out = NULL, fu_len_out = NULL,
                             unif_global = NULL, recruit_global = NULL,
                             unif_noncn = NULL, recruit_noncn = NULL,
                             unif_cn_in = NULL, recruit_cn_in = NULL,
                             unif_cn_out = NULL, recruit_cn_out = NULL,
                             ms0, hr, drop_ctrl, drop_trt,
                             n_targetevent, nsim, SigL, thd, seed)
{
    set.seed(seed)

    ss <- Samplesize(n = n, ratio = ratio, ratio_cn = ratio_cn, pct = pct, pct_in = pct_in)

    res <- sfSapply(1:nsim, function(x) {
        SimuData(pattern=pattern, ss = ss,
                 enroll_len = enroll_len, enroll_st_cn = enroll_st_cn,
                 enroll_len_out = enroll_len_out, fu_len_out = fu_len_out,
                 unif_global = unif_global, recruit_global = recruit_global,
                 unif_noncn = unif_noncn, recruit_noncn = recruit_noncn,
                 unif_cn_in = unif_cn_in, recruit_cn_in = recruit_cn_in,
                 unif_cn_out = unif_cn_out, recruit_cn_out = recruit_cn_out,
                 ms0 = ms0, hr = hr, drop_ctrl = drop_ctrl, drop_trt = drop_trt,
                 n_targetevent = n_targetevent)
    })
    final = t(res)

    c(n_global = n
      , n_chinese = ss$n_chinese
      , pct = pct*100
      , pct_in = pct_in*100
      , n_event_g = mean(final[, "n_event_main"])
      , n_event_cn = round(mean(final[, "n_event_cn"]), 0)
      , conditional_p = round(sum((final[, "p_main"]< SigL) &
                                      (final[, "logHR_cn"]/final[, "logHR_main"] > thd))/
                                  sum(final[, "p_main"] < SigL), 3)
      , unconditional_p = round(sum(final[, "logHR_cn"]/final[, "logHR_main"] > thd)/nsim, 3)
      , cond_p_red = round(sum((final[, "p_main"]< SigL) &
                                   ((1-final[, "HR_cn"])/(1-final[, "HR_main"]) > thd))/
                               sum(final[, "p_main"] < SigL), 3)
      , uncond_p_red = round(sum((1-final[, "HR_cn"])/(1-final[, "HR_main"]) > thd)/nsim, 3)
      , targeteventdate = round(mean(final[, "targeteventdate"]), 3)
      , median_dur_g = round(mean(final[, "median_dur_main"]), 3)
      , median_dur_cn = round(mean(final[, "median_dur_cn"]), 3)
      , power_global = round(sum(final[, "p_main"] < SigL)/nsim, 3)
      , power_cn = round(sum(final[, "p_cn"] < SigL)/nsim, 3)
      , threshold = thd
    )
}


#########################
##Plot##
#########################
plotSim <- function(pattern = 1, chinese_all_in = 1 , pct_seq = NULL,
                    pct_in_seq = NULL, n = 555, ratio = 2, ratio_cn=2, pct = NULL,
                    enroll_len = 15, enroll_st_cn  = NULL,
                    enroll_len_out = NULL, fu_len_out = NULL,
                    unif_global = NULL, recruit_global = NULL,
                    unif_noncn  = NULL, recruit_noncn = NULL,
                    unif_cn_in = NULL, recruit_cn_in = NULL,
                    unif_cn_out = NULL, recruit_cn_out = NULL,
                    ms0 = 20, hr = 0.625, drop_ctrl = 0.2, drop_trt = 0.2,
                    n_targetevent = 227, nsim = 10, SigL = 0.05, thd = 0.5, seed = 12345
) {
    # snowfall support
    sfInit(parallel = TRUE, cpus = detectCores() - 1)

    sfLibrary(survival)
    sfLibrary(tidyverse)
    sfExport("gen_ttr", "SimuData")

    if(chinese_all_in == 1 || chinese_all_in == 0) {
        if(chinese_all_in == 1 ) {pct_in = 1} else {pct_in = 0}
        result_df <- map_dfr(pct_seq,
                             ~ConsistecncyP_CE(pattern=pattern, n = n,
                                               ratio = ratio, ratio_cn = ratio_cn,
                                               pct = .x, pct_in = pct_in,
                                               enroll_len = enroll_len,
                                               enroll_st_cn = enroll_st_cn,
                                               enroll_len_out = enroll_len_out,
                                               fu_len_out = fu_len_out,
                                               unif_global = unif_global,
                                               recruit_global = recruit_global,
                                               unif_noncn = unif_noncn,
                                               recruit_noncn = recruit_noncn,
                                               unif_cn_in = unif_cn_in,
                                               recruit_cn_in = recruit_cn_in,
                                               unif_cn_out = unif_cn_out,
                                               recruit_cn_out = recruit_cn_out,
                                               ms0 = ms0, hr = hr,
                                               drop_ctrl = drop_ctrl, drop_trt = drop_trt,
                                               n_targetevent = n_targetevent,
                                               nsim = nsim, SigL = SigL, thd = thd,
                                               seed = seed))
        #result_df
        result_df1 = result_df %>%
            pivot_longer(cols = c("conditional_p", "unconditional_p"),
                         names_to = "consistency",
                         values_to = "prob")

        g = ggplot(result_df1, aes(x=pct)) +
            geom_point(aes(y = prob, color = consistency) ) +
            scale_y_continuous(limits = c(min(result_df1$prob),1)) +
            labs(x = "Number of Chinese subjects/Number of subjects in global study(%)",
                 y = "Probability of consistency") +
            theme_minimal()
    }else{# chinese_all_in == 2
        result_df <- map_dfr(pct_in_seq,
                             ~ConsistecncyP_CE(pattern=pattern, n = n,
                                               ratio = ratio, ratio_cn = ratio_cn,
                                               pct = pct, pct_in = .x,
                                               enroll_len = enroll_len,
                                               enroll_st_cn = enroll_st_cn,
                                               enroll_len_out = enroll_len_out,
                                               fu_len_out = fu_len_out,
                                               unif_global = unif_global,
                                               recruit_global = recruit_global,
                                               unif_noncn = unif_noncn,
                                               recruit_noncn = recruit_noncn,
                                               unif_cn_in = unif_cn_in,
                                               recruit_cn_in = recruit_cn_in,
                                               unif_cn_out = unif_cn_out,
                                               recruit_cn_out = recruit_cn_out,
                                               ms0 = ms0, hr = hr,
                                               drop_ctrl = drop_ctrl, drop_trt = drop_trt,
                                               n_targetevent = n_targetevent,
                                               nsim = nsim, SigL = SigL, thd = thd,
                                               seed = seed))
        #result_df
        result_df1 = result_df %>%
            pivot_longer(cols = c("conditional_p", "unconditional_p"),
                         names_to = "consistency",
                         values_to = "prob")

        g = ggplot(result_df1, aes(x = pct_in)) +
            geom_point(aes(y = prob, color = consistency) ) +
            scale_y_continuous(limits = c(min(result_df1$prob),1)) +
            labs(x = "Number of Chinese subjects in global study/Number of Chinese subjects(%)",
                 y = "Probability of consistency") +
            theme_minimal()
    }

    sfStop()

    data2down <- result_df
    data2disp <- datatable(result_df,
                           colnames = c("N of Global", "N of Subpopulation",
                                        "N of Subpopulation /N of Global (%)",
                                        "N of Subpopulation within Global /N of Subpopulation (%)",
                                        "N of events Global", "N of events Subpopulation ",
                                        "Conditional probability (logHR)", "Unconditional probability (logHR)",
                                        "Conditional probability (risk reduction)", "Unconditional probability (risk reduction)",
                                        "Time to target N of events (mths)", "Median duration Global (mths)",
                                        "Median duration Subpopulation (mths)", "Power Global", "Power Subpopulation",
                                        "Proportion of treatment effect preserved in Subpopulation"),
                           options = list(scrollX = TRUE))

    return(list(df1=data2down, df2=data2disp,plot=g))

}

#show_sim_time = function(sim_time) {
#  cat(">>>>>>>>>>>>>>>>>>>>>>>>>\n")
#  print(sim_time)
#  cat("<<<<<<<<<<<<<<<<<<<<<<<<<\n")
#}
#sim_time = system.time({

#  plotSim(pattern=1, chinese_all_in = 1, pct_seq = c(0.2),
#          n = 15000, ratio = 1, ratio_cn = 1, enroll_len = 24, enroll_st_cn = 7, unif_global = 0,
#          recruit_global = c(10, 25, 51, 127, 254, 279, 431, 557, 684, 684,
#                             760, 760, 760, 811, 811, 811, 836, 861, 861, 861,
#                             902, 938, 963, 963),
#          ms0 = 433.6, hr = 0.7, drop_ctrl = 0.00001, drop_trt = 0.00001,
#          n_targetevent = 333, nsim = 100, SigL = 0.05, thd = 0.5)

#})

# display the total simulation time.
#show_sim_time(sim_time)
