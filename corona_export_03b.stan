data {
   int<lower=0> N;
   int<lower=0> o[N];
   real<lower=0> flight[N];
   real<lower=0, upper=1> flight_weight[N] ;
   int<lower=1,upper=4> country_c[N]; // indicator: 1=Singaport, 2=other
   // for predictions
   int<lower=0> N_pred;
   real<lower=0> flight_pred[N_pred];
   int<lower=1,upper=4> country_c_pred[N_pred]; // indicator: 1=Singaport, 2=other
 }
 
 parameters {
   // Define parameters to estimate
   real beta_lg ; // beta on log scale
   real<lower=0, upper=1> c_high ; // c for strong dcountries 
   real<lower=0, upper=1> c_med ; // c for weak
   real<lower=0, upper=1> c_low ; // c for weak
 }
 
 transformed parameters  {
   // define transformed parameters
   vector[N] import_exp ;
   vector[4] c_v ; 
   real<lower=0> beta ;
   real<lower=0> c_weighted ; // 
   real<lower=0> c_weighted_v[N] ;
   //real<lower=0,upper=1> c;
   // assign transformed parameters
   //c = inv_logit(c_logiti) ;
   c_v[1] = 1 ;
   c_v[2] = c_high ;
   c_v[3] = c_med ;
   c_v[4] = c_low ;
   //
   beta = exp(beta_lg) ; 
   //
   for (i in 1:N) {
                   import_exp[i] = beta*c_v[ country_c[i] ]*flight[i] ;
                   // across countries (not Singapore) sum their cs and weigh by flight
                   c_weighted_v[i] = c_v[ country_c[i] ] * flight_weight[i] ;
   }
   c_weighted = sum(c_weighted_v);
 }

 model {
   // Prior part of Bayesian inference
   // (no need to specify if non-informative)
   o ~ poisson(import_exp) ;
   
   beta_lg ~ normal(0,50); // prior slope on 
   c_high ~ uniform(0,1);
   c_med ~ uniform(0,1);
   c_low ~ uniform(0,1);
   //
   
 }
   
 generated quantities {
   // define predicted vector
   int<lower=0> o_pred_1[N_pred] ;
   real<lower=0> o_pred_exp_1[N_pred] ;
   //
   int<lower=0> o_pred_2[N_pred] ;
   real<lower=0> o_pred_exp_2[N_pred] ;
   //
   real c_logiti_pred ;
   real c_pred_beta ; 
   real c_pred ;
   vector[N] log_lik;
   //
   c_logiti_pred = normal_rng(0,50);
   c_pred = uniform_rng(0,1) ;
   c_pred_beta = beta_rng(1,1) ;
   //
   for (i in 1:N_pred) o_pred_1[i] = poisson_rng( beta*1*flight_pred[i] ) ;
   for (i in 1:N_pred) o_pred_exp_1[i] =  beta*1*flight_pred[i] ; 
   //
   for (i in 1:N_pred) o_pred_2[i] = poisson_rng( beta*c_weighted*flight_pred[i] ) ;
   for (i in 1:N_pred) o_pred_exp_2[i] =  beta*c_weighted*flight_pred[i] ; 
   // log likelihood computed only among the dataset for fit
   for (i in 1:N) log_lik[i] = poisson_lpmf( o[i] | import_exp[i] ) ;
   }
