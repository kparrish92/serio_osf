

data_is = data.frame(answer = rbinom(n = 20, size = 1, prob = .5), answer_t = "is")
data_is_not = data.frame(answer = rbinom(n = 20, size = 1, prob = .5), answer_t = "is_not")
data_both = data.frame(answer = rbinom(n = 20, size = 1, prob = .5), answer_t = "both")



prior = set_prior("normal(1, 1)", class = "Intercept")


prior_check = c(set_prior("normal(-2, 1)", class = "Intercept", dpar = "muis"),
                set_prior("normal(-1, 1)", class = "Intercept", dpar = "muisnot"),
                set_prior("normal(-3, 1)", class = "b", dpar = "muIs", coef = "CategoryTypeNatural"))

          
mod_priors = brm(answer ~ 1, family = bernoulli(), prior = prior, data = coin_50)

summary(mod)


plogis(.33)

?rmultinom

# nppts, noreps, probs of cond 
## Vectors are ppts 


abs_probs = c(0.34,0.30,0.36)

nat_probs = c(0.22,0.04,0.74)

art_probs = c(0.35,0.1,0.55)

ppt_name = "ppt_1"
cond_name = "Abstract" 
  
sim_ppt = function(probs, ppt_name, cond_name)
{
pptdf = rmultinom(1, size = 6, prob = probs) %>% as.data.frame() %>% 
  mutate(answer = c("both", "is", "is_not")) %>% 
  rename(n = "V1") %>% 
  uncount(n)
pptdf$ppt = ppt_name
pptdf$condition = cond_name
return(pptdf)
}

df_list = list()

for (i in 1:48) {
adf = sim_ppt(abs_probs, ppt_name = paste0("ppt_",i), "abstract")
nkdf = sim_ppt(nat_probs, ppt_name = paste0("ppt_",i), "natural_kind")  
atdf = sim_ppt(art_probs, ppt_name = paste0("ppt_",i), "artifact")  

binddf = rbind(adf,nkdf,atdf)

df_list[[i]] = binddf
}  

sim_df = do.call(rbind,df_list)

