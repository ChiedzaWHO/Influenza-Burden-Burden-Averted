####################################################################################################
## WHO: assessment of the influenza vaccine impact
##
## Program to estimate the confidence intervals for estimates of influenza prevented by vaccination
##
####################################################################################################

## Setting the seed and the number of simulations######
set.seed(123)
nsim <- 50

####################################################################################################
## The program has 4 steps:
## 1. Input data from the Excel tool that estimates vaccine impact
##    Data inputs include:
##    a. Disease burden
##    b. Vaccine coverage
##    c. Vaccine effectiveness
##    d. Size of target population
##    e. data need to estimate multipliers
##
## 2. Generate distributions for each data input
##    a. Disease burden (rates of hospitalization or number of hospitalizations) -- assumed to follow Poisson distribution
##    b. Vaccine coverage -- assumed to follow a normal distribution
##    c. Vaccine effectiveness -- assumed to follow a normal distribution, on the log scale
##    d. Size of target population -- assumed constant
##    e. multipliers -- from Poisson distributed hospitalized and no hospitalized infections
##
##
## 3. Run the model using random draws from each of the distributions
##
## 4. Summarize the confidence intervals
####################################################################################################



####################################################################################################
## 1. Generate distributions for each data input
####################################################################################################

## The inputs that remain constant across the year: Multipliers, coverage, and VE

temp_season <- tableforCI() %>%
  filter(month == 1) %>%
  select(pvinc_hosp, pvinc_nhosp, pvinc_ma,avinc_hosp,avinc_nhosp,avinc_ma, total_coverage, coverage_se, adjve, adjve_lcl, adjve_ucl)


## Replicate the dataset nsim times
temp_season_sim <- do.call("rbind", replicate(nsim, temp_season, simplify = FALSE))

## Create a simulation index to make sure everything makes sense
temp_season_sim$simindex  <- rep(seq(1:nsim))

## Create simulated values
temp_season_sim2 <- temp_season_sim %>%
  ## Incidences from tool, to estimate multipliers
  mutate(pvsim_nhosp   = rpois(n(), lambda = pvinc_nhosp),
         pvsim_ma    = rpois(n(), lambda = pvinc_ma),
         pvsim_hosp  = rpois(n(), lambda = pvinc_hosp),
         avsim_nhosp = rpois(n(),lambda = avinc_nhosp),
         avsim_ma = rpois(n(),lambda = avinc_ma),
         avsim_hosp = rpois(n(),lambda = avinc_hosp))%>%
  ## Calculate multipliers
  mutate(pvsimnum_nhosp = as.numeric(pvsim_nhosp),
         pvsimnum_hosp  = as.numeric(pvsim_hosp),
         avsimnum_nhosp = as.numeric(avsim_nhosp),
         avsimnum_hosp  = as.numeric(avsim_hosp),
         pvsim_hnhratio = case_when(pvsimnum_hosp < 1  ~ pvsimnum_nhosp,
                                    pvsimnum_hosp >= 1 ~ pvsimnum_nhosp / pvsimnum_hosp),
         avsim_hnhratio = case_when(avsimnum_hosp < 1  ~ avsimnum_nhosp,
                                    avsimnum_hosp >= 1 ~ avsimnum_nhosp / avsimnum_hosp),
         pvsim_maprop       = pvsim_ma / (pvsim_hosp+pvsim_nhosp),
         avsim_maprop       = avsim_ma / (avsim_hosp+avsim_nhosp)) %>%
  ## Total coverage estimates
  mutate(senorm_cov = rnorm(n(), mean = 0, sd = .75),
         sim_cov_se = coverage_se * senorm_cov,
         sim_cov    = total_coverage + sim_cov_se,
         adj_sim_vc = case_when(sim_cov >= 0 ~ sim_cov,
                                sim_cov < 0 ~ 0)) %>%
  ## VE estimates
  mutate(ve_adj = case_when(adjve <0 ~ 0,
                            adjve >=0 ~ adjve),
         ve_lcl_adj = case_when(adjve_lcl <0 ~ 0,
                                adjve_lcl >=0 ~ adjve_lcl),
         beta_ve = log(1 - ve_adj),
         beta_ve_lb = log(1 - ve_lcl_adj),
         beta_ve_ub = log(1 - adjve_ucl),
         se_beta_lb = (beta_ve_lb - beta_ve) / 1.96,
         se_beta_ub = (beta_ve - beta_ve_ub) / 1.96,
         se_beta_ve = max(se_beta_lb, se_beta_ub)) %>%
  mutate(senorm_beta_ve = rnorm(n(), mean = 0, sd = 0.75),
         sim_betave_se = se_beta_ve * senorm_beta_ve,
         sim_betave = beta_ve + sim_betave_se,
         sim_ve = 1 - exp(sim_betave),
         adj_sim_ve = case_when(sim_ve >= 0 ~ sim_ve,
                                sim_ve < 0 ~ 0))


## The inputs that change each month: hospital burden

temp_set <- tableforCI() %>%
  select(month, population, adj_hosprate, pvn_hosp,avn_hosp, mnth_coverage, mult_frac)

## Replicate the dataframe "nsim" times
temp_set_sim <- do.call("rbind", replicate(nsim, temp_set, simplify = FALSE))

## Create a simulation index to make sure everything makes sense
temp_set_sim2 <- temp_set_sim %>%
  group_by(month) %>%
  mutate(simindex = 1:n())

## Create simulated values
temp_set_simhosp <- temp_set_sim2 %>%
  mutate(pvsimn_hosp = rpois(n(), lambda = pvn_hosp),
         pvsimn_hosprate = pvsimn_hosp / population,
         avsimn_hosp = rpois(n(), lambda = avn_hosp),
         avsimn_hosprate = avsimn_hosp / population)


## Merge the yearly constants with the values that change each month...merge on the "simindex"

temp_sim <- left_join(temp_set_simhosp, temp_season_sim2, by = "simindex") %>%
  mutate(propvax = mnth_coverage / total_coverage,
         sim_mnthvc = adj_sim_vc * propvax)


####################################################################################################
## 3. Run the model using random draws from each of the distributions
####################################################################################################

temp_sim2 <- temp_sim %>%
  ## Estimate illnesses and medically-attended illnesses using simulated multipliers and hospitalization counts
  mutate(pvsimn_nhosp = pvsimn_hosp * pvsim_hnhratio,
         pvsimn_inf = pvsimn_nhosp + pvsimn_hosp,
         pvsimn_mainf = pvsimn_inf * pvsim_maprop,
         avsimn_nhosp = avsimn_hosp * avsim_hnhratio,
         avsimn_inf = avsimn_nhosp + avsimn_hosp,
         avsimn_mainf = avsimn_inf * avsim_maprop)


## Run the impact model

### estimate risk of events using number of events, the effectively vaccinated and susceptible population in pv, for month 1 first


temp_sim2$pveffvac<-NA
temp_sim2$pvsuspop<-NA
temp_sim2$pvrisknhosp<-NA
temp_sim2$pvhosprisk<-NA

for (month in 1:nrow(temp_sim2)){
  if(temp_sim2$month[month]==1){
    temp_sim2$pveffvac[month]<-temp_sim2$population[month]*(temp_sim2$sim_mnthvc[month]*temp_sim2$adj_sim_ve[month])
    temp_sim2$pvsuspop[month]<-temp_sim2$population[month] - temp_sim2$pvsimn_inf[month] - temp_sim2$pveffvac[month]
    temp_sim2$pvrisknhosp[month]<-temp_sim2$pvsimn_nhosp[month] / temp_sim2$population[month]
    temp_sim2$pvhosprisk[month]<-temp_sim2$pvsimn_hosp[month] / temp_sim2$population[month]

  }else{
    temp_sim2$pveffvac[month]<-temp_sim2$population[month]*(temp_sim2$sim_mnthvc[month]*temp_sim2$adj_sim_ve[month])
    temp_sim2$pvsuspop[month]<-(temp_sim2$pvsuspop[month-1]) - temp_sim2$pvsimn_inf[month] - temp_sim2$pveffvac[month]
    temp_sim2$pvrisknhosp[month]<-temp_sim2$pvsimn_nhosp[month] / (temp_sim2$pvsuspop[month-1])
    temp_sim2$pvhosprisk[month]<-temp_sim2$pvsimn_hosp[month] / (temp_sim2$pvsuspop[month-1])
  }
}




### estimate risk of events using total infections and susceptible population in av, for month 1 first



temp_sim2$avsuspop<-NA
temp_sim2$avrisknhosp<-NA
temp_sim2$avhosprisk<-NA


for (month in 1:nrow(temp_sim2)){

  if(temp_sim2$month[month]==1){
    temp_sim2$avsuspop[month]<-temp_sim2$population[month] - temp_sim2$avsimn_inf[month]
    temp_sim2$avrisknhosp[month]<-temp_sim2$avsimn_nhosp[month] / temp_sim2$population[month]
    temp_sim2$avhosprisk[month]<-temp_sim2$avsimn_hosp[month] / temp_sim2$population[month]

  }else{
    temp_sim2$avsuspop[month]<-(temp_sim2$avsuspop[month-1]) - temp_sim2$avsimn_inf[month]
    temp_sim2$avrisknhosp[month]<-temp_sim2$avsimn_nhosp[month] / (temp_sim2$avsuspop[month-1])
    temp_sim2$avhosprisk[month]<-temp_sim2$avsimn_hosp[month] / (temp_sim2$avsuspop[month-1])
  }

}



temp_sim2 <- temp_sim2 %>%
  rename("pvmsuspop" = "pvsuspop",
         "avmsuspop" = "avsuspop",
         "pvmeffvac" = "pveffvac",
         "pvmrisknhosp" = "pvrisknhosp",
         "avmrisknhosp" = "avrisknhosp",
         "avmhosprisk" = "avhosprisk",
         "pvmhosprisk" = "pvhosprisk")


### Use the case risk to estimate hypothetical cases and the Monthly population expected in the absence of vaccination

temp_sim2$novaxnhosp<-NA
temp_sim2$novaxhosp<-NA
temp_sim2$novaxpop<-NA

for (month in 1:nrow(temp_sim2)){

  if(temp_sim2$month[month]==1){
    temp_sim2$novaxnhosp[month]<-(temp_sim2$population[month]) * temp_sim2$pvmrisknhosp[month]
    temp_sim2$novaxhosp[month]<-(temp_sim2$population[month]) * temp_sim2$pvmhosprisk[month]
    temp_sim2$novaxpop[month]<-temp_sim2$population[month] - temp_sim2$novaxnhosp[month] - temp_sim2$novaxhosp[month]
  } else{
    temp_sim2$novaxnhosp[month]<- (temp_sim2$novaxpop[month-1]) * temp_sim2$pvmrisknhosp[month]
    temp_sim2$novaxhosp[month]<- (temp_sim2$novaxpop[month-1]) * temp_sim2$pvmhosprisk[month]
    temp_sim2$novaxpop[month]<- (temp_sim2$novaxpop[month-1]) - temp_sim2$novaxnhosp[month] - temp_sim2$novaxhosp[month]

  }
}





### Use the case risk to estimate hypothetical cases and the Monthly population expected in the presence of vaccination

temp_sim2$yvaxnhosp<-NA
temp_sim2$yvaxhosp<-NA
temp_sim2$aveffvac<-NA
temp_sim2$yvaxpop<-NA



for (month in 1:nrow(temp_sim2)){

  if(temp_sim2$month[month]==1){
    temp_sim2$yvaxnhosp[month]<- (temp_sim2$population[month]) * temp_sim2$avmrisknhosp[month]
    temp_sim2$yvaxhosp[month]<-(temp_sim2$population[month]) * temp_sim2$avmhosprisk[month]
    temp_sim2$aveffvac[month]<-(temp_sim2$population[month])*(temp_sim2$adj_sim_ve[month]*temp_sim2$sim_mnthvc[month])
    temp_sim2$yvaxpop[month]<-temp_sim2$population[month] - temp_sim2$yvaxnhosp[month] - temp_sim2$yvaxhosp[month] - temp_sim2$aveffvac[month]
  } else{
    temp_sim2$yvaxnhosp[month]<- (temp_sim2$yvaxpop[month-1]) * temp_sim2$avmrisknhosp[month]
    temp_sim2$yvaxhosp[month]<- (temp_sim2$yvaxpop[month-1]) * temp_sim2$avmhosprisk[month]
    temp_sim2$aveffvac[month]<- (temp_sim2$population[month])*(temp_sim2$adj_sim_ve[month]*temp_sim2$sim_mnthvc[month])
    temp_sim2$yvaxpop[month]<- (temp_sim2$yvaxpop[month-1]) - temp_sim2$yvaxnhosp[month] - temp_sim2$yvaxhosp[month] - temp_sim2$aveffvac[month]
  }
}


temp_sim2$novaxinf <- temp_sim2$novaxhosp + temp_sim2$novaxnhosp
temp_sim2$yvaxinf <- temp_sim2$yvaxhosp + temp_sim2$yvaxnhosp
temp_sim2$novaxmainf <- temp_sim2$novaxinf * temp_sim2$pvsim_maprop
temp_sim2$yvaxmainf <- temp_sim2$yvaxinf * temp_sim2$avsim_maprop
temp_sim2$adj_novaxmainf <-temp_sim2$novaxmainf * temp_sim2$mult_frac
temp_sim2$adj_novaxhosp <- temp_sim2$novaxhosp * temp_sim2$mult_frac
temp_sim2$adj_novaxnhosp <-temp_sim2$novaxnhosp * temp_sim2$mult_frac
temp_sim2$adj_novaxinf <- temp_sim2$novaxinf * temp_sim2$mult_frac
temp_sim2$adj_yvaxmainf <- temp_sim2$yvaxmainf * temp_sim2$mult_frac
temp_sim2$adj_yvaxhosp <- temp_sim2$yvaxhosp * temp_sim2$mult_frac
temp_sim2$adj_yvaxnhosp <- temp_sim2$yvaxnhosp * temp_sim2$mult_frac
temp_sim2$adj_yvaxinf <- temp_sim2$yvaxinf * temp_sim2$mult_frac
temp_sim2$adj_avsimn_inf <- temp_sim2$avsimn_inf * temp_sim2$mult_frac
temp_sim2$adj_avsimn_mainf <- temp_sim2$avsimn_mainf * temp_sim2$mult_frac
temp_sim2$adj_avsimn_hosp <- temp_sim2$avsimn_hosp * temp_sim2$mult_frac
temp_sim2$adj_avsimn_nhosp <- temp_sim2$avsimn_nhosp * temp_sim2$mult_frac

### Calculate the averted outcomes
temp_sim2$pvav_inf <- temp_sim2$novaxinf - temp_sim2$pvsimn_inf
temp_sim2$pvav_mainf = temp_sim2$novaxmainf - temp_sim2$pvsimn_mainf
temp_sim2$pvav_hosp = temp_sim2$novaxhosp - temp_sim2$pvsimn_hosp
temp_sim2$pvav_nhosp = temp_sim2$novaxnhosp - temp_sim2$pvsimn_nhosp
temp_sim2$avav_inf = temp_sim2$avsimn_inf - temp_sim2$yvaxinf
temp_sim2$avav_mainf = temp_sim2$avsimn_mainf - temp_sim2$yvaxmainf
temp_sim2$avav_hosp = temp_sim2$avsimn_hosp - temp_sim2$yvaxhosp
temp_sim2$avav_nhosp = temp_sim2$avsimn_nhosp - temp_sim2$yvaxnhosp
temp_sim2$pre_pvnnv_inf = temp_sim2$adj_novaxinf * temp_sim2$adj_sim_ve / temp_sim2$population
temp_sim2$pre_pvnnv_mainf = temp_sim2$adj_novaxmainf * temp_sim2$adj_sim_ve / temp_sim2$population
temp_sim2$pre_pvnnv_hosp = temp_sim2$adj_novaxhosp * temp_sim2$adj_sim_ve / temp_sim2$population
temp_sim2$pre_pvnnv_nhosp = temp_sim2$adj_novaxnhosp * temp_sim2$adj_sim_ve / temp_sim2$population
temp_sim2$pre_avnnv_inf = temp_sim2$adj_avsimn_inf * temp_sim2$adj_sim_ve /temp_sim2$population
temp_sim2$pre_avnnv_mainf = temp_sim2$adj_avsimn_mainf * temp_sim2$adj_sim_ve / temp_sim2$population
temp_sim2$pre_avnnv_hosp = temp_sim2$adj_avsimn_hosp * temp_sim2$adj_sim_ve / temp_sim2$population
temp_sim2$pre_avnnv_nhosp = temp_sim2$adj_avsimn_nhosp *temp_sim2$adj_sim_ve / temp_sim2$population




## Drop unneeded variables
temp_sim2<-temp_sim2%>%
  select(month, simindex, pvav_inf, pvav_mainf, pvav_hosp, pvav_nhosp, avav_inf, avav_mainf, avav_hosp, avav_nhosp, adj_novaxinf, population,  sim_mnthvc,
         adj_sim_ve, adj_novaxhosp, adj_novaxnhosp, adj_avsimn_hosp, adj_avsimn_nhosp, adj_avsimn_inf, pre_pvnnv_inf, pre_pvnnv_mainf,
         pre_pvnnv_hosp, pre_pvnnv_nhosp, pre_avnnv_inf, pre_avnnv_mainf, pre_avnnv_hosp, pre_avnnv_nhosp)

write.csv(temp_sim2, "temp_sim2.csv")
####################################################################################################
## 4. Summarize the primary averted burden model
####################################################################################################

## sum across months of the model
setup_summary <- temp_sim2 %>%
  group_by(simindex) %>%
  summarize(sum_pvavinf   = sum(pvav_inf),
            sum_pvavmainf = sum(pvav_mainf),
            sum_pvavhosp   = sum(pvav_hosp),
            sum_pvavnhosp = sum(pvav_nhosp),
            sum_avavinf   = sum(avav_inf),
            sum_avavmainf = sum(avav_mainf),
            sum_avavhosp   = sum(avav_hosp),
            sum_avavnhosp = sum(avav_nhosp),
            sum_adj_novaxinf = sum(adj_novaxinf),
            min_pop      = min(population),
            total_VC     = sum(sim_mnthvc),
            ve           = min(adj_sim_ve),
            sum_adj_novaxhosp = sum(adj_novaxhosp),
            sum_adj_novaxnhosp = sum(adj_novaxnhosp),
            sum_adj_avsimn_hosp = sum(adj_avsimn_hosp),
            sum_adj_avsimn_nhosp = sum(adj_avsimn_nhosp),
            sum_adj_avsimn_inf = sum(adj_avsimn_inf),
            sum_pre_pvnnv_inf = sum(pre_pvnnv_inf),
            sum_pre_pvnnv_mainf = sum(pre_pvnnv_mainf),
            sum_pre_pvnnv_hosp = sum(pre_pvnnv_hosp),
            sum_pre_pvnnv_nhosp = sum(pre_pvnnv_nhosp),
            sum_pre_avnnv_inf = sum(pre_avnnv_inf),
            sum_pre_avnnv_mainf = sum(pre_avnnv_mainf),
            sum_pre_avnnv_hosp = sum(pre_avnnv_hosp),
            sum_pre_avnnv_nhosp = sum(pre_avnnv_nhosp)) %>%

  mutate(pvav_inf_perc = sum_pvavinf / sum_adj_novaxinf,
         pvav_hosp_perc = sum_pvavhosp / sum_adj_novaxhosp,
         pvav_nhosp_perc = sum_pvavnhosp / sum_adj_novaxnhosp,
         avav_inf_perc = sum_avavinf / sum_adj_avsimn_inf,
         avav_hosp_perc = sum_avavhosp / sum_adj_avsimn_hosp,
         avav_nhosp_perc = sum_avavnhosp / sum_adj_avsimn_nhosp,
         pvnnv_inf   = case_when(sum_pre_pvnnv_inf >0 ~ 1 / sum_pre_pvnnv_inf,
                                 sum_pre_pvnnv_inf <= 0 ~ NA),
         pvnnv_mainf   = case_when(sum_pre_pvnnv_mainf >0 ~ 1 / sum_pre_pvnnv_mainf,
                                   sum_pre_pvnnv_mainf <= 0 ~ NA),
         pvnnv_nhosp   = case_when(sum_pre_pvnnv_nhosp >0 ~ 1 / sum_pre_pvnnv_nhosp,
                                   sum_pre_pvnnv_hosp <= 0 ~ NA),
         pvnnv_hosp   = case_when(sum_pre_pvnnv_hosp >0 ~ 1 / sum_pre_pvnnv_hosp,
                                  sum_pre_pvnnv_hosp <= 0 ~ NA),
         avnnv_inf   = case_when(sum_pre_avnnv_inf >0 ~ 1 / sum_pre_avnnv_inf,
                                 sum_pre_avnnv_inf <= 0 ~ NA),
         avnnv_mainf   = case_when(sum_pre_avnnv_mainf >0 ~ 1 / sum_pre_avnnv_mainf,
                                   sum_pre_avnnv_mainf <= 0 ~ NA),
         avnnv_nhosp   = case_when(sum_pre_avnnv_nhosp >0 ~ 1 / sum_pre_avnnv_nhosp,
                                   sum_pre_avnnv_hosp <= 0 ~ NA),
         avnnv_hosp   = case_when(sum_pre_avnnv_hosp >0 ~ 1 / sum_pre_avnnv_hosp,
                                  sum_pre_avnnv_hosp <= 0 ~ NA))


#print(setup_summary$sum_pre_pvnnv_inf)
#print(setup_summary$sum_pre_pvnnv_mainf)
#print(setup_summary$sum_pre_pvnnv_nhosp)
#print(setup_summary$sum_pre_pvnnv_hosp)
#print(setup_summary$sum_pre_avnnv_inf)
#print(setup_summary$sum_pre_avnnv_mainf)
#print(setup_summary$sum_pre_avnnv_nhosp)
#print(setup_summary$sum_pre_avnnv_hosp)


#print(setup_summary%>%as.data.frame())

#write.csv(setup_summary, "setup_summary.csv")



## summarize the simulations
summary <- setup_summary %>%
  summarize(ve_lbound = quantile(ve, probs = 0.025, na.rm = TRUE),
            ve_ubound = quantile(ve, probs = 0.975, na.rm = TRUE),

            vc_lbound = quantile(total_VC, probs = 0.025, na.rm = TRUE),
            vc_ubound = quantile(total_VC, probs = 0.975, na.rm = TRUE),

            pvavinf_lbound = quantile(sum_pvavinf, probs = 0.025, na.rm = TRUE),
            pvavinf_mbound = quantile(sum_pvavinf, probs = 0.5, na.rm = TRUE),
            pvavinf_ubound = quantile(sum_pvavinf, probs = 0.975, na.rm = TRUE),

            pvavmainf_lbound = quantile(sum_pvavmainf, probs = 0.025, na.rm = TRUE),
            pvavmainf_mbound = quantile(sum_pvavmainf, probs = 0.5, na.rm = TRUE),
            pvavmainf_ubound = quantile(sum_pvavmainf, probs = 0.975, na.rm = TRUE),

            pvavhosp_lbound = quantile(sum_pvavhosp, probs = 0.025, na.rm = TRUE),
            pvavhosp_mbound = quantile(sum_pvavhosp, probs = 0.5, na.rm = TRUE),
            pvavhosp_ubound = quantile(sum_pvavhosp, probs = 0.975, na.rm = TRUE),

            pvavnhosp_lbound = quantile(sum_pvavnhosp, probs = 0.025, na.rm = TRUE),
            pvavnhosp_mbound = quantile(sum_pvavnhosp, probs = 0.5, na.rm = TRUE),
            pvavnhosp_ubound = quantile(sum_pvavnhosp, probs = 0.975, na.rm = TRUE),

            pvnnv_inf_lbound = quantile(pvnnv_inf, probs = 0.025, na.rm = TRUE),
            pvnnv_inf_mbound = quantile(pvnnv_inf, probs = 0.5, na.rm = TRUE),
            pvnnv_inf_ubound = quantile(pvnnv_inf, probs = 0.975, na.rm = TRUE),

            pvnnv_mainf_lbound = quantile(pvnnv_mainf, probs = 0.025, na.rm = TRUE),
            pvnnv_mainf_mbound = quantile(pvnnv_mainf, probs = 0.5, na.rm = TRUE),
            pvnnv_mainf_ubound = quantile(pvnnv_mainf, probs = 0.975, na.rm = TRUE),

            pvnnv_hosp_lbound = quantile(pvnnv_hosp, probs = 0.025, na.rm = TRUE),
            pvnnv_hosp_mbound = quantile(pvnnv_hosp, probs = 0.5, na.rm = TRUE),
            pvnnv_hosp_ubound = quantile(pvnnv_hosp, probs = 0.975, na.rm = TRUE),

            pvnnv_nhosp_lbound = quantile(pvnnv_nhosp, probs = 0.025, na.rm = TRUE),
            pvnnv_nhosp_mbound = quantile(pvnnv_nhosp, probs = 0.5, na.rm = TRUE),
            pvnnv_nhosp_ubound = quantile(pvnnv_nhosp, probs = 0.975, na.rm = TRUE),

            pvavinfperc_lbound = quantile(pvav_inf_perc, probs = 0.025, na.rm = TRUE),
            pvavinfperc_mbound = quantile(pvav_inf_perc, probs = 0.5, na.rm = TRUE),
            pvavinfperc_ubound = quantile(pvav_inf_perc, probs = 0.975, na.rm = TRUE),

            pvavhospperc_lbound = quantile(pvav_hosp_perc, probs = 0.025, na.rm = TRUE),
            pvavhospperc_mbound = quantile(pvav_hosp_perc, probs = 0.500, na.rm = TRUE),
            pvavhospperc_ubound = quantile(pvav_hosp_perc, probs = 0.975, na.rm = TRUE),

            pvavnhospperc_lbound = quantile(pvav_nhosp_perc, probs = 0.025, na.rm = TRUE),
            pvavnhospperc_mbound = quantile(pvav_nhosp_perc, probs = 0.500, na.rm = TRUE),
            pvavnhospperc_ubound = quantile(pvav_nhosp_perc, probs = 0.975, na.rm = TRUE),

            avavinf_lbound = quantile(sum_avavinf, probs = 0.025, na.rm = TRUE),
            avavinf_mbound = quantile(sum_avavinf, probs = 0.5, na.rm = TRUE),
            avavinf_ubound = quantile(sum_avavinf, probs = 0.975, na.rm = TRUE),

            avavmainf_lbound = quantile(sum_avavmainf, probs = 0.025, na.rm = TRUE),
            avavmainf_mbound = quantile(sum_avavmainf, probs = 0.5, na.rm = TRUE),
            avavmainf_ubound = quantile(sum_avavmainf, probs = 0.975, na.rm = TRUE),

            avavhosp_lbound = quantile(sum_avavhosp, probs = 0.025, na.rm = TRUE),
            avavhosp_mbound = quantile(sum_avavhosp, probs = 0.5, na.rm = TRUE),
            avavhosp_ubound = quantile(sum_avavhosp, probs = 0.975, na.rm = TRUE),

            avavnhosp_lbound = quantile(sum_avavnhosp, probs = 0.025, na.rm = TRUE),
            avavnhosp_mbound = quantile(sum_avavnhosp, probs = 0.5, na.rm = TRUE),
            avavnhosp_ubound = quantile(sum_avavnhosp, probs = 0.975, na.rm = TRUE),

            avnnv_inf_lbound = quantile(avnnv_inf, probs = 0.025, na.rm = TRUE),
            avnnv_inf_mbound = quantile(avnnv_inf, probs = 0.5, na.rm = TRUE),
            avnnv_inf_ubound = quantile(avnnv_inf, probs = 0.975, na.rm = TRUE),

            avnnv_mainf_lbound = quantile(avnnv_mainf, probs = 0.025, na.rm = TRUE),
            avnnv_mainf_mbound = quantile(avnnv_mainf, probs = 0.5, na.rm = TRUE),
            avnnv_mainf_ubound = quantile(avnnv_mainf, probs = 0.975, na.rm = TRUE),

            avnnv_hosp_lbound = quantile(avnnv_hosp, probs = 0.025, na.rm = TRUE),
            avnnv_hosp_mbound = quantile(avnnv_hosp, probs = 0.5, na.rm = TRUE),
            avnnv_hosp_ubound = quantile(avnnv_hosp, probs = 0.975, na.rm = TRUE),

            avnnv_nhosp_lbound = quantile(avnnv_nhosp, probs = 0.025, na.rm = TRUE),
            avnnv_nhosp_mbound = quantile(avnnv_nhosp, probs = 0.5, na.rm = TRUE),
            avnnv_nhosp_ubound = quantile(avnnv_nhosp, probs = 0.975, na.rm = TRUE),

            avavinfperc_lbound = quantile(avav_inf_perc, probs = 0.025, na.rm = TRUE),
            avavinfperc_mbound = quantile(avav_inf_perc, probs = 0.5, na.rm = TRUE),
            avavinfperc_ubound = quantile(avav_inf_perc, probs = 0.975, na.rm = TRUE),

            avavhospperc_lbound = quantile(avav_hosp_perc, probs = 0.025, na.rm = TRUE),
            avavhospperc_mbound = quantile(avav_hosp_perc, probs = 0.500, na.rm = TRUE),
            avavhospperc_ubound = quantile(avav_hosp_perc, probs = 0.975, na.rm = TRUE),

            avavnhospperc_lbound = quantile(avav_nhosp_perc, probs = 0.025, na.rm = TRUE),
            avavnhospperc_mbound = quantile(avav_nhosp_perc, probs = 0.500, na.rm = TRUE),
            avavnhospperc_ubound = quantile(avav_nhosp_perc, probs = 0.975, na.rm = TRUE))

#######################################################################################################
#######################################################################################################
