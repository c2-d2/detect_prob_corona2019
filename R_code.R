# libraries
library(tidyverse)
library(doParallel)
pkgs <- c('doParallel', 'foreach')
lapply(pkgs, require, character.only = T)
registerDoParallel(cores = 4)
library(rstan)
library(bayesplot)
rstan_options(auto_write = TRUE)


# functions
# inspired by: https://stackoverflow.com/questions/17922637/prediction-intervals-for-poisson-regression-on-r
boot_pi <- function(model, pdata, n, p) {
                odata <- model$data
                lp <- (1 - p) / 2
                up <- 1 - lp
                set.seed(2020)
                seeds <- round(runif(n, 1, 1000), 0)
                boot_y <- foreach(i = 1:n, .combine = rbind) %dopar% {
                                set.seed(seeds[i])
                                bdata <- odata[sample(seq(nrow(odata)), size = nrow(odata), replace = TRUE), ]
                                bpred <- predict(update(model, data = bdata), type = "response", newdata = pdata)
                                rpois(length(bpred), lambda = bpred)
                }
                boot_ci <- t(apply(boot_y, 2, quantile, c(lp, up)))
                return(data.frame(pred = predict(model, newdata = pdata, type = "response"), lower = boot_ci[, 1], upper = boot_ci[, 2]))
}

plot_case_vol <- function( PI, df ) {
                p1 <- PI %>% as_tibble() %>% mutate(Cases_real=df$Cases, 
                                                    Air_travel_volume=df$Wuhan_airtravel,
                                                    Country=df$Country) %>% 
                                ggplot( aes(x=Air_travel_volume) ) +
                                geom_point( aes(y=Cases_real), size=1.2 ) +
                                geom_line( aes(y=pred), linetype=2 ) +
                                geom_ribbon(aes(ymin=lower,ymax=upper), alpha=0.3 ) +
                                theme(panel.grid.minor = element_blank()) +
                                labs(y="")
                return(p1)
}

# load data for cases + flight volume
load(file="~/Desktop/Corona/epidata.Rdata") # 30
df_fix <- df %>% left_join( tibble(Country_new=c("United States", # 30 x 3
                                                 "United Kingdom",
                                                 "United Arab Emirates",
                                                 "Sweden",
                                                 "New Zealand",
                                                 "South Korea"),
                                   Country=c("US",
                                             "UK",
                                             "UAE",
                                             "Sweeden",
                                             "New Zeland",
                                             "Rep  Korea") ) ) %>% 
                mutate( Country=ifelse(is.na(Country_new),Country,Country_new) ) %>% 
                select(-Country_new)
                
# load travel data new data entry
dfr <- read_csv(file="~/Desktop/Corona/flight_lai.csv",col_names = F) %>% 
                rename( Country=X1, Wuhan_airtravel_r=X2 ) # 30 x 2

# divide by 30 days of a month (should not matter)
# able to compare Pablo and my data entry
df_r2 <- full_join(dfr,df_fix, by="Country") %>% # 33 x 4
                mutate( ratio=Wuhan_airtravel_r/Wuhan_airtravel) %>% 
                mutate(Wuhan_airtravel=Wuhan_airtravel_r/30) %>% 
                select(-Wuhan_airtravel_r,-ratio) %>% 
                mutate( Cases_lm=replace_na(Cases_lm, replace = 0) )

df_r2 <- df_r2 %>% filter(!(Country%in%c("Taiwan","Hong Kong", "Macao")))

# load ghs data (195 x 2)
df_ghs <- read_csv(file="~/Desktop/Corona/ghs_index.csv",col_names = F) %>% 
                rename(Country=X1,ghs=X2) 
# bind datasets 
df_all0 <- full_join( df_ghs,df_r2, by="Country" ) %>% mutate( Cases_lm=replace_na(Cases_lm,replace=0),
                                                               Wuhan_airtravel=replace_na(Wuhan_airtravel,replace=0))  
df_all0 <- df_all0 %>% filter( Country!="China") # filter out epidmic origin  
                                                            
# settings
min_airtravel <- 1.5 
ghs_quantile_1 <- 1/3
ghs_quantile_2 <- 2/3
# setting air travel volume to min_airtravel
# giving binary label according to GHS index
ghs_limit <- df_all0$ghs %>% quantile(c(ghs_quantile_1,
                                        ghs_quantile_2)) 

df_all <- df_all0 %>% 
                mutate( Wuhan_airtravel_gamma=ifelse(Wuhan_airtravel<3,
                                                     min_airtravel,
                                                     Wuhan_airtravel) ) %>% 
                mutate( ghs_high=ifelse(ghs>ghs_limit[2], 2,
                                        ifelse(ghs>ghs_limit[1], 3 , 4 ))) # df_all$ghs_high %>% table()

df_all <- df_all %>% mutate( country_c=ifelse(Country=="Singapore",1,
                                              ghs_high) ) %>% 
                mutate( flight_weight=ifelse(Country=="Singapore",0,Wuhan_airtravel_gamma),
                        flight_weight=flight_weight/sum(flight_weight)) 

df_fit <- df_all # filter if needed



# make list for stan
stan_df <- list(
                N=nrow(df_fit),
                o=df_fit$Cases_lm,
                flight=df_fit$Wuhan_airtravel_gamma,
                country_c=df_fit$country_c,
                flight_weight=df_fit$flight_weight,
                #
                N_pred=nrow(df_all),
                flight_pred=df_all$Wuhan_airtravel_gamma,
                country_c_pred=df_all$country_c
)





# run stan
m00 <- stan(file="corona_export_03b.stan",
            data = stan_df , 
            chains=n_chains, iter=3000, warmup=200, thin=1,
            control=list(max_treedepth=10,adapt_delta=0.9)) # m00 %>% print(pars="beta_lg")
posterior <- m00 %>% as.array() # posterior %>% dimnames()

color_scheme_set("mix-blue-red")
mcmc_areas(
                posterior, 
                pars = c("c_weighted"),
                prob = 0.8, # 80% intervals
                prob_outer = 0.95, # 99%
                point_est = "median"
) + scale_x_continuous(limits=c(-0.01,+1.01)) +
                theme(axis.ticks = element_blank(),
                      axis.text.y = element_blank(),
                      axis.text.x = element_text(size=9, family="Helvetica"),
                      axis.title = element_text(size=12, family="Helvetica"),
                      axis.line.y=element_blank(),
                      panel.grid.major.x = element_line(linetype=2, color="lightgrey")) 
                      
                      color_scheme_set("gray")
mcmc_areas(
                posterior, 
                pars = c("c_high","c_med","c_low"),
                prob = 0.8, # 80% intervals
                prob_outer = 0.95, # 99%
                point_est = "median"
) + scale_x_continuous(limits=c(-0.01,+1.01)) +
                theme(axis.ticks = element_blank(),
                      axis.text.y = element_blank(),
                      axis.text.x = element_text(size=9, family="Helvetica"),
                      axis.title = element_text(size=12, family="Helvetica"),
                      axis.line.y=element_blank(),
                      panel.grid.major.x = element_line(linetype=2, color="lightgrey"))

# make supplementary Table 1
df_non_sing <- df_fit %>% 
                select( Country,ghs_high,Wuhan_airtravel,Cases_lm) %>% 
                mutate( Wuhan_airtravel=ifelse(Wuhan_airtravel>0,"Top 27","Lower 167") ) %>% 
                mutate( ghs_high=ifelse(ghs_high==2,"high",
                                        ifelse(ghs_high==3,"medium","low") ) ) %>% 
                filter(Country!="Singapore")
df_sing <- df_fit %>% 
                select( Country,ghs_high,Wuhan_airtravel,Cases_lm) %>% 
                mutate( Wuhan_airtravel=ifelse(Wuhan_airtravel>0,"Top 27","Lower 167") ) %>% 
                mutate( ghs_high=ifelse(ghs_high==2,"high",
                                        ifelse(ghs_high==3,"medium","low") ) ) %>% 
                filter(Country=="Singapore")
bind_rows(df_sing,df_non_sing) %>% rename(`GHS surveillance capacity`=ghs_high,
                                          `Flight connectivity to Wuhan`=Wuhan_airtravel,
                                          `Imported and reported cases`=Cases_lm) %>% 
                mutate( `GHS surveillance capacity`=ifelse(Country=="Singapore","best",
                                                           `GHS surveillance capacity` ) ) %>% 
                mutate( i=1:n() ) %>% 
                write_csv("~/Desktop/Corona/Out/Suppl_tabl_ghs.csv")


# locations with connectivity and medium/low surveillance
df_fit %>% 
                filter(Wuhan_airtravel>0 & ghs_high%in%c(4,3)) %>% 
                select(Country)


###################### smaller calculations #############################
# compute the duration in which cases might have been exported from Wuhan
library(lubridate)
dmy("8-12-2019") - dmy("4-2-2020") # 58 days
1/(31 * 58)
            
# 9 locations with over 500 cases
dfr %>% filter(Country%in% c("South Korea","Iran","Italy","Japan","France","Germany","Spain","Singapore","United States")) %>% nrow() # 8 countries with top 27 
# all but Iran
# what are the odds of 7of8 top connection or more (8of8)
(27/194)^7 * (1-27/194) + (27/194)^8 # 1.011426e-06
