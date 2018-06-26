library(ggplot2)
library(gridExtra)

# Define model
dat <- vio_duld_ftpt
# Marginal effects plots for significant interactions

emp <- unique(fulldf$arsinftpt[fulldf$year != "2012"]) # Define x axis
xaxis <- unique(fulldf$ftptemp[fulldf$year != "2012"])

coefs <- NULL
ci_upper <- NULL
ci_lower <- NULL

# Populate model with coefs and CIs
for(j in 1:length(emp)) {
  
  coefs <- append(coefs, dat$coefficients["arsinduld"] + dat$coefficients["arsinduld:arsinftpt"]*emp[j])
  
  ci_upper <- append(ci_upper, coefs[j] + 1.96*sqrt(dat$vcov["arsinduld","arsinduld"] + 
                                                      dat$vcov["arsinduld:arsinftpt","arsinduld:arsinftpt"]*emp[j]^2 +
                                                      2*emp[j]*dat$vcov["arsinduld","arsinduld:arsinftpt"]))
  
  ci_lower <- append(ci_lower, coefs[j] - 1.96*sqrt(dat$vcov["arsinduld","arsinduld"] + 
                                                      dat$vcov["arsinduld:arsinftpt","arsinduld:arsinftpt"]*emp[j]^2 +
                                                      2*emp[j]*dat$vcov["arsinduld","arsinduld:arsinftpt"]))
}

# Combine to DF
plotdf <- as.data.frame(cbind(coefs, ci_upper, ci_lower, xaxis))

p1 <- ggplot(data = plotdf) + geom_line(aes(x = xaxis, y = coefs)) +
  geom_ribbon(aes(x = xaxis, ymin=ci_lower, ymax=ci_upper), alpha=0.3) + 
  geom_hline(yintercept=0) + xlim(0,1) + ylim(-.6,.4) + theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Share full-time or part-time employed") +
  ylab("Elasticity of share with \n temporary stay of deportation") 

dat <- vio_duld_mini
# Marginal effects plots for significant interactions

emp <- unique(fulldf$arsinmini[fulldf$year != "2012"]) # Define x axis
xaxis <- unique(fulldf$miniemp[fulldf$year != "2012"])

coefs <- NULL
ci_upper <- NULL
ci_lower <- NULL

# Populate model with coefs and CIs
for(j in 1:length(emp)) {
  
  coefs <- append(coefs, dat$coefficients["arsinduld"] + dat$coefficients["arsinduld:arsinmini"]*emp[j])
  
  ci_upper <- append(ci_upper, coefs[j] + 1.96*sqrt(dat$vcov["arsinduld","arsinduld"] + 
                                                      dat$vcov["arsinduld:arsinmini","arsinduld:arsinmini"]*emp[j]^2 +
                                                      2*emp[j]*dat$vcov["arsinduld","arsinduld:arsinmini"]))
  
  ci_lower <- append(ci_lower, coefs[j] - 1.96*sqrt(dat$vcov["arsinduld","arsinduld"] + 
                                                      dat$vcov["arsinduld:arsinmini","arsinduld:arsinmini"]*emp[j]^2 +
                                                      2*emp[j]*dat$vcov["arsinduld","arsinduld:arsinmini"]))
}

# Combine to DF
plotdf <- as.data.frame(cbind(coefs, ci_upper, ci_lower, xaxis))

p2 <- ggplot(data = plotdf) + geom_line(aes(x = xaxis, y = coefs)) +
  geom_ribbon(aes(x = xaxis, ymin=ci_lower, ymax=ci_upper), alpha=0.3) + 
  geom_hline(yintercept=0) + xlim(0,1) + ylim(-.6,.4) + theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Share marginally employed") +
  ylab("Elasticity of share with \n temporary stay of deportation") + ggtitle("Violent Crime")

dat <- drug_duld_mini
# Marginal effects plots for significant interactions

emp <- unique(fulldf$arsinmini[fulldf$year != "2012"]) # Define x axis
xaxis <- unique(fulldf$miniemp[fulldf$year != "2012"])

coefs <- NULL
ci_upper <- NULL
ci_lower <- NULL

# Populate model with coefs and CIs
for(j in 1:length(emp)) {
  
  coefs <- append(coefs, dat$coefficients["arsinduld"] + dat$coefficients["arsinduld:arsinmini"]*emp[j])
  
  ci_upper <- append(ci_upper, coefs[j] + 1.96*sqrt(dat$vcov["arsinduld","arsinduld"] + 
                                                      dat$vcov["arsinduld:arsinmini","arsinduld:arsinmini"]*emp[j]^2 +
                                                      2*emp[j]*dat$vcov["arsinduld","arsinduld:arsinmini"]))
  
  ci_lower <- append(ci_lower, coefs[j] - 1.96*sqrt(dat$vcov["arsinduld","arsinduld"] + 
                                                      dat$vcov["arsinduld:arsinmini","arsinduld:arsinmini"]*emp[j]^2 +
                                                      2*emp[j]*dat$vcov["arsinduld","arsinduld:arsinmini"]))
}

# Combine to DF
plotdf <- as.data.frame(cbind(coefs, ci_upper, ci_lower, xaxis))

p3 <- ggplot(data = plotdf) + geom_line(aes(x = xaxis, y = coefs)) +
  geom_ribbon(aes(x = xaxis, ymin=ci_lower, ymax=ci_upper), alpha=0.3) + 
  geom_hline(yintercept=0) + xlim(0,1) + ylim(-.6,.40) + theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Share marginally employed") +
  ylab("Elasticity of share with \n temporary stay of deportation") + 
  ggtitle("Drug Crime")

grid.arrange(p2, p3, p1, nrow = 2)

plotdf %>% 
  filter(ci_upper < 0) %>% 
  summarize(min(xaxis))

ecdf(fulldf$miniemp)(0.185)
quantile(fulldf$miniemp, na.rm = TRUE)


