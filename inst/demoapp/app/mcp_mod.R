# For Phase 2b dose finding study based on MCP-Mod

########################################################################
## parameters needed ##
# doses: a vector of dose levels, eg: c(0, 12.5, 25, 50, 100);
# fmodels: an object of class ‘⁠"Mods"⁠ in DoseFinding package to define a set of dose-response models; 
# n_model: number of candidate models;
# sigma: the Standard deviation for the response simulation with normality distribution;
# delta: the clinically relevent treatment effect, default is NULL;
# n_arm: a vector of numbers of participants corresponding to the dose level; 
# n_arm_reg: a vector of numbers of participants in a certain region corresponding to the dose level;
# alpha: type I error rate, default is 0.05; 
# alter: 'one.sided' or 'two.sided' of the alternative hypothesis, default is 'one.sided';
# type: 3 types of global power (1 for PoC only, 2 for PoC and max treatment effect > delta, and 3 for PoC & max treatment effect > delta & med <= highest dose), default is 1;
# pi: treatment effect preservation ratio, default is 0.5;
# iter: number of iterations in the simulation process, default is 10;
# direction: whether an 'increasing' or 'decreasing' of the response variable is beneficial. 

########################

library(DoseFinding)
library(dplyr)

#### for continuous endpoint with normality distribution ####
m_mcp_mod <- function(doses 
                      , fmodels 
                      , sigma   
                      , delta   = NULL
                      , n_arm   
                      , truemod 
                      , alpha = 0.05
                      , power_opt = 1
                      , direction 
                      , pi=0.5
                      , n_arm_reg  
                      , iter = 10
                      , alter="one.sided") {
    
    
    # simulate results given candidate models 
    res <- replicate(iter, {
        n <- sum(n_arm)
        resp <- getResp(fmodels, doses = doses)
        # simulate data
        sim <- rep(resp[, truemod], n_arm) + rnorm(n, mean = 0, sd = sigma)
        
        # criterion 1: significant contrast test for at least one model - PoC
        dose <- rep(doses,n_arm)
        if (!is.null(delta)) {
            Res <- MCPMod(dose, sim, models = fmodels, type="normal",addCovars = ~1, Delta=delta, selModel="maxT", alternative=alter, alpha = alpha)
            
            selectmod <- which.max(Res$MCTtest$tStat)
            
            fit <- lm(sim~as.factor(dose)-1)
            mu <- coef(fit)
            
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
                fit_CN <- lm(sim[ind_CN_all]~as.factor(dose[ind_CN_all])-1)
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
        }
        else {
            Res <- MCTtest(dose, sim, models = fmodels, type="normal",addCovars = ~1,  alternative=alter, alpha = alpha)
            
            selectmod <- which.max(Res$tStat)
            
            fit <- lm(sim~as.factor(dose)-1)
            mu <- coef(fit)
            
            crit1  <- attr(Res$tStat, "pVal")[selectmod] < alpha 
            
            ### criterion for China consistency using 'local consistency in contrast statistics' method
            
            ind_CN_all = sample(1:n_arm[1], n_arm_reg[1])
            for (i in 2: length(doses))
                ind_CN_all = c(ind_CN_all,sample(1:n_arm[i], n_arm_reg[i])+cumsum(n_arm)[i-1])
            CN_all = numeric(n)
            CN_all[ind_CN_all] = 1
            fit_CN <- lm(sim[ind_CN_all]~as.factor(dose[ind_CN_all])-1)
            mu_CN <- coef(fit_CN)
            
            contr<- Res$contMat[,selectmod]  ##
            
            contall <- contr %*% mu ## average response per dose (vector)
            cont_CN <- contr %*% mu_CN
            
            consis <- cont_CN/contall > pi 
            
            med = NA
        }
        return(c(crit1, crit1 & consis,  med))
    })
    
    return(c(rowMeans(res[1:2,], na.rm = T), quantile(res[3,], probs = seq(0.05, 0.95, 0.45), na.rm = T,  digits = 2))) 
    
}


result_df <- function (doses, fmodels, sigma, n_arm, n_arm_reg, delta = NULL, type = 1, pi=1/2, n_model, iter=10, direction, alpha=0.05, alter='one.sided', seed=12356) {
    
    set.seed(seed = seed)
    
    data_res = NULL
    
    for (j in 1:n_model) {
        
        result =  m_mcp_mod(n_arm=n_arm, doses= doses,sigma=sigma, delta = delta, fmodels = fmodels, truemod = j, pi=pi, n_arm_reg=n_arm_reg, iter = iter, power_opt = type, direction = direction, alpha=alpha, alter=alter)
        power = round(result [1],3)
        m_med= round(result[4],3)
        cons_CN = round(result[2]/power,3)
        
        data_res = rbind(data_res, c(j, sum(n_arm),sum(n_arm_reg),round(sum(n_arm_reg)/sum(n_arm),3)*100, power, type,  pi, m_med, cons_CN))
    }
    colnames(data_res)= c('True Model', 'N of Global','N of Subpopulation', 'N of Subpopulation/N of Global',"Power Global","type of power",
                          'Proportion of treatment effect preserved in Subpopulation', 'Median MED', 
                          "Conditional prob_contrast")
    return(list(df1=data_res, df2=data_res))
}


