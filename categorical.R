library(MASS)

mod <- chisq.test(Cars93$Type, Cars93$Origin)
mod_f <- fisher.test(Cars93$Type, Cars93$Origin)  # residuals same 

mod$stdres > 1.96  # significant!!!

z.test <- function(z){
  2 * pnorm(-abs(z))
}


z.test(mod$stdres)


df
