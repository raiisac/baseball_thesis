# Here, we test whether 3 or 4 factors for REPS works better
df <- data_cfa_reps %>%
  dplyr::select(-play_in_rain, -wash_hands)
df <- df[complete.cases(df[, -c(1, 2)]), -c(1, 2)]
pca <- prcomp(df)
screeplot(pca, type = "lines")#2-4 factors is best
covmat <- cov(df)
cormat <- cor(df)
fa.parallel(cormat, n.obs = nrow(df))#2 factors is best
vss(cormat, n.obs = nrow(df)) #-> 2 or 4 factors is best

fa_table2 <- function(x, cut) {
  #get sorted loadings
  loadings <- fa.sort(x)$loadings %>% round(3)
  #supress loadings
  loadings[loadings < cut] <- ""
  #get additional info
  add_info <- cbind(x$communalities,
                    x$uniquenesses,
                    x$complexity) %>%
    # make it a data frame
    as.data.frame() %>%
    # column names
    rename("Communality" = V1,
           "Uniqueness" = V2,
           "Complexity" = V3) %>%
    #get the item names from the vector
    rownames_to_column("item")
  #build table
  loadings %>%
    unclass() %>%
    as.data.frame() %>%
    rownames_to_column("item") %>%
    left_join(add_info) %>%
    mutate(across(where(is.numeric), round, 3))
}
#-------------------------3 factors----------------------------------#
efa <- fa(cormat,
                   covar = FALSE,
                   3, rotate = "oblimin",
                   fm = "ml", n.obs = nrow(df))
fa_table2(efa, .33)
model_reps <- "protection_from_injury1 =~ 1*prevention_importance + importance_supervision + limit_dangerous_activities + chance_of_injury
protection_from_injury2 =~ 1*concerned_injury + concerned_hazards + avoid_risk
          risk_engagement =~ 1*promote_physical_challenges + physical_limits + explore_new_environments + benefits_outweigh_risk + importance_managing_risk + risk_self_confidence"
fit_free_reps <- cfa(model_reps,
                     data = likert_data_reps %>%
                       filter(community != "Dutch speaking.French speaking"),
                     group = "community")
fit_sameloadings_reps <- cfa(model_reps,
                             data = likert_data_reps %>%
                               filter(community !=
                                        "Dutch speaking.French speaking"),
                             group = "community",
                             group.equal = c("loadings"))
fit_sameloadingsinterc_reps <- cfa(model_reps,
                                   data = likert_data_reps %>%
                                     filter(community !=
                                              "Dutch speaking.French speaking"),
                                   group = "community",
                                   group.equal = c("loadings", "intercepts"))

anov_reps_metric <- anova(fit_free_reps, fit_sameloadings_reps) #metric model cannot be rejected
anov_reps_strong <- anova(fit_free_reps, fit_sameloadingsinterc_reps)
#interpret the fit:
interpret(fit_sameloadings_reps) #they all show poor fit
fitmeasures(fit_sameloadings_reps, c("chisq", "df", "pvalue", "cfi",
                                     "tli", "rmsea", "srmr", "AIC",
                                     "BIC"))
cr_omega_reps <- compRelSEM(fit_sameloadings_reps, tau.eq = F, obs.var = T)
cr_alpha_reps <- compRelSEM(fit_sameloadings_reps, tau.eq = T, obs.var = T)

#-------------------------4 factors----------------------------------#
efa <- fa(cormat,
          covar = FALSE,
          4, rotate = "oblimin",
          fm = "ml", n.obs = nrow(df))
fa_table2(efa, .3)
model_reps <- "protection_from_injury1 =~ 1*prevention_importance + importance_supervision + limit_dangerous_activities + chance_of_injury
protection_from_injury2 =~ 1*concerned_injury + concerned_hazards + avoid_risk
          risk_engagement =~ 1*physical_limits + explore_new_environments + benefits_outweigh_risk + importance_managing_risk + risk_self_confidence
risk_engagement2 =~ 1*promote_physical_challenges"
fit_free_reps <- cfa(model_reps,
                     data = likert_data_reps %>%
                       filter(community != "Dutch speaking.French speaking"),
                     group = "community")
fit_sameloadings_reps <- cfa(model_reps,
                             data = likert_data_reps %>%
                               filter(community !=
                                        "Dutch speaking.French speaking"),
                             group = "community",
                             group.equal = c("loadings"))
fit_sameloadingsinterc_reps <- cfa(model_reps,
                                   data = likert_data_reps %>%
                                     filter(community !=
                                              "Dutch speaking.French speaking"),
                                   group = "community",
                                   group.equal = c("loadings", "intercepts"))

anov_reps_metric <- anova(fit_free_reps, fit_sameloadings_reps) #metric model cannot be rejected
anov_reps_strong <- anova(fit_free_reps, fit_sameloadingsinterc_reps)
#interpret the fit:
interpret(fit_sameloadings_reps) #they all show poor fit
fitmeasures(fit_sameloadings_reps, c("chisq", "df", "pvalue", "cfi",
                                     "tli", "rmsea", "srmr", "AIC",
                                     "BIC"))
cr_omega_reps <- compRelSEM(fit_sameloadings_reps, tau.eq = F, obs.var = T)
cr_alpha_reps <- compRelSEM(fit_sameloadings_reps, tau.eq = T, obs.var = T)
