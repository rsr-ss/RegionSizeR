
########################

library(DoseFinding)
library(dplyr)

###################################### for binary endpoint with binomial distribution #############################################

logit <- function(p) log(p / (1 - p))
inv_logit <- function(y) 1 / (1 + exp(-y))

m_mcp_mod_bin <- function(doses 
                          , fmodels 
                          , delta  = NULL
                          , n_arm   
                          , truemod = 1
                          , alpha = 0.05
                          , power_opt = 1
                          , direction 
                          , pi=0.5
                          , n_arm_reg   
                          , iter = 100
                          , alter="one.sided") {
    
    
    # simulate results given candidate models 
    res <- replicate(iter, {
        n <- sum(n_arm)
        resp <- getResp(fmodels, doses = doses)
        # simulate data
        prob <- rep(inv_logit (resp[, truemod]), n_arm)
        sim <- rbinom (n, 1, prob)
        # criterion 1: significant contrast test for at least one model - PoC
        dose <- rep(doses,n_arm)
        
        fit <- glm(sim~as.factor(dose)-1, family = binomial)
        mu <- coef(fit)
        S <- vcov(fit)
        if (!is.null(delta)) {
            Res <- MCPMod(doses, mu, models = fmodels, type="general",S = S, Delta=delta, selModel="maxT", alternative=alter, alpha = alpha)
            selectmod <- which.max(Res$MCTtest$tStat)
            
            if (!is.null(Res$selMod)) {
                
                if (power_opt == 1)
                    crit1  <- attr(Res$MCTtest$tStat, "pVal")[selectmod] < alpha 
                if (power_opt == 2 & direction == "decreasing")
                    crit1  <- attr(Res$MCTtest$tStat, "pVal")[selectmod] < alpha & min(mu) < -delta+mu[1]
                if (power_opt == 2 & direction == "increasing")
                    crit1  <- attr(Res$MCTtest$tStat, "pVal")[selectmod] < alpha & max(mu) > delta+mu[1]
                
                ### criterion for China consistency using 'local consistency in contrast statistics' method
                
                ind_CN_all = sample(1:n_arm[1], n_arm_reg[1])
                for (i in 2: length(doses))
                    ind_CN_all = c(ind_CN_all,sample(1:n_arm[i], n_arm_reg[i])+cumsum(n_arm)[i-1])
                CN_all = numeric(n)
                CN_all[ind_CN_all] = 1
                fit_CN <- glm(sim[ind_CN_all]~as.factor(dose[ind_CN_all])-1, family = binomial)
                mu_CN <- coef(fit_CN)
                
                contr<- Res$MCTtest$contMat[,selectmod]  ##
                
                contall <- contr %*% mu ## average response per dose (vector)
                cont_CN <- contr %*% mu_CN
                
                consis <- cont_CN/contall > pi 
                
                med = Res$doseEst[names(Res$doseEst)==Res$selMod]
                if (power_opt == 3 & direction == "decreasing" )
                    crit1  <- attr(Res$MCTtest$tStat, "pVal")[selectmod] < alpha & min(mu) < -delta+mu[1] & !is.na(med) & between(med,doses[1],tail(doses,1))
                if (power_opt == 3 & direction == "increasing")
                    crit1  <- attr(Res$MCTtest$tStat, "pVal")[selectmod] < alpha & max(mu) > delta+mu[1] & !is.na(med) & between(med,doses[1],tail(doses,1))
                
                
                
            } else {
                crit1  <- FALSE
                consis <- NA
                med = NA
            }
            if (!crit1) med = NA
        } else {
            Res <- MCTtest(doses, mu, models = fmodels, type="general",S = S, alternative=alter, alpha = alpha)
            selectmod <- which.max(Res$tStat)
            
            crit1  <- min(attr(Res$tStat, "pVal"))< alpha 
            
            ### criterion for China consistency using 'local consistency in contrast statistics' method
            
            ind_CN_all = sample(1:n_arm[1], n_arm_reg[1])
            for (i in 2: length(doses))
                ind_CN_all = c(ind_CN_all,sample(1:n_arm[i], n_arm_reg[i])+cumsum(n_arm)[i-1])
            CN_all = numeric(n)
            CN_all[ind_CN_all] = 1
            fit_CN <- glm(sim[ind_CN_all]~as.factor(dose[ind_CN_all])-1, family = binomial)
            mu_CN <- coef(fit_CN)
            
            contr<- Res$contMat[,selectmod]  ##
            
            contall <- contr %*% mu  ## treatment contrast in logit scale
            cont_CN <- contr %*% mu_CN
            
            #contall_res <- sum(inv_logit(contr * mu))  ## inverse back to response rates
            #cont_CN_res <- sum(inv_logit(contr * mu_CN))
            #contall_res <- contr %*% inv_logit(mu)  ## inverse back to response rates
            #cont_CN_res <- contr %*% inv_logit(mu_CN)
            #contall_res <- inv_logit(contr %*% mu)  ## inverse back to response rates
            #cont_CN_res <- inv_logit(contr %*% mu_CN)
            
            #consis <-  cont_CN_res/contall_res > pi 
            consis <-  cont_CN/contall > pi
            
            med = NA
        }
        return(c(crit1, crit1 & consis, med))
    }, simplify = 'matrix')
    
    return(c(rowSums(res[1:2,], na.rm = T)/iter, quantile(res[3,], probs = seq(0.05, 0.95, 0.45), na.rm = T,  digits = 2)))
}


result_df <- function (doses, fmodels,  n_arm, n_arm_reg, delta = NULL, type = 1, pi=1/2, n_model, iter=10, direction, alpha=0.05, alter='one.sided', seed=12356) {
    
    set.seed(seed = seed)
    
    data_res = NULL
    
    for (j in 1:n_model) {
        
        result =  m_mcp_mod_bin(n_arm=n_arm, doses= doses, delta = delta, fmodels = fmodels, truemod = j, pi=pi, n_arm_reg=n_arm_reg, iter = iter, power_opt = type, direction = direction, alpha=alpha, alter=alter)
        power = round(result [1],3)
        m_med= round(result[5],3)
        cons_CN = round(result[2]/power,3)
        
        data_res = rbind(data_res, c(j, sum(n_arm),sum(n_arm_reg),round(sum(n_arm_reg)/sum(n_arm),3)*100, power, type,  pi, m_med, cons_CN))
    }
    colnames(data_res)= c('True Model', 'N of Global','N of Subpopulation', 'N of Subpopulation/N of Global',"Power Global","type of power",
                          'Proportion of treatment effect preserved in Subpopulation', 'Median MED', 
                          "Conditional prob_contrast")
    return(list(df1=data_res, df2=data_res))
}

