#https://lavaan.ugent.be/tutorial/groups.html

library(frictionless) # to load data from the data package
library(ggstats) # for better likert plots
library(here)
library(kableExtra)
library(likert)
library(nFactors)
library(patchwork)
library(psych)
library(rprojroot)
library(tidyverse)
library(lavaan)
library(semTools)
source(find_root_file("source", "R", "fa_table.R",
                      criterion = has_file("baseball_thesis.Rproj")))
baseball_package <- read_package(
  find_root_file(
    "data", "processed", "datapackage_analysis",
    "datapackage.json",
    criterion = has_file("baseball_thesis.Rproj")))
children <- read_resource(package = baseball_package,
                          resource_name = "wp2_participants_metadata") %>%
  left_join(read_resource(package = baseball_package,
                          resource_name = "wp2_school_data") %>%
              dplyr::select(-n_students))
# schools <- read_resource(package = baseball_package,
#                          resource_name = "wp2_school_data")#only additional info here is the number of studentes in the school
landscape <- read_resource(package = baseball_package,
                         resource_name = "wp1_landscape_level_data")
baseball_derived_package <- read_package(
  find_root_file(
    "data", "processed", "datapackage_derived_data",
    "datapackage.json",
    criterion = has_file("baseball_thesis.Rproj")))
schools <- read_resource(package = baseball_derived_package,
              resource_name = "wp1_school_level_key_variables")

#---------------------------------ATOP-----------------------------------------#
# a description of how to view and read the data from the data package
atop_data <- read_resource(package = baseball_package,
                           resource_name = "wp5_atop_data")
atop_key <- read_resource(package = baseball_package,
                          resource_name = "wp5_atop_questions")
#inspect the description
get_schema(package = baseball_package,
           resource_name = "wp5_atop_data")$fields %>%
  bind_rows() %>%
  kable(booktabs = TRUE) %>%
  kableExtra::kable_styling()

likert_data_atop <- atop_data %>%
  as.data.frame() %>%
  mutate(interpretation = ifelse(interpretation == "No response",
                                 NA, interpretation),
         interpretation = factor(as.factor(interpretation),
                                 levels = c("Disagree", "Slightly disagree",
                                            "Moderately agree", "Agree"),
                                 ordered = TRUE)) %>%
  dplyr::select(question_short, interpretation, child_id) %>%
  pivot_wider(names_from = question_short, values_from = interpretation,
              id_cols = child_id) %>%
  left_join(children) %>%
  left_join(schools) #high or low naturalness & greenness (school_context)
# likert_data_atop2 <- as.data.frame(do.call(cbind,
#                                            sapply(1:ncol(likert_data_atop),
#                                                   FUN = function(x) {
#                                                     likert_data_atop[[x]]
#                                                   })))
# colnames(likert_data_atop2) <- names(likert_data_atop)
# likert_data_atop2 <- likert_data_atop %>%
#   mutate(across(think_clearly:getting_hurt,
#                 function(x) factor(as.factor(x),
#                                    levels = 1:5,
#                                    labels = c("Disagree", "Slightly disagree",
#                                               "No response", "Moderately agree",
#                                               "Agree"))))


#names(likert_data_atop) <- atop_key$question_text_english


#-----------------------ATOP-multilevel-cfa-school-----------------------------#
model <- "ATOP_benefits =~ think_clearly + good_for_health + calm_down + learn_new_things + feeling_free + imagination + explore
          ATOP_fears =~ get_lost + unknown_persons + wild_animals + getting_hurt"
fit_free <- cfa(model,
           data = likert_data_atop,
           group = "school_id") #some variables have no variance in group 1: good_for_health
  #lavaan WARNING: small number of observations (nobs < nvar) in group 1
  #nobs = 8 nvar = 11
likert_data_atop %>% group_by(school_id) %>% summarize(n = n()) %>%
  dplyr::pull(n) %>% summary()# We have as little as 3 students in some schools. -> not enough variability
students_per_school <- likert_data_atop %>%
  group_by(school_id) %>%
  summarize(n = n(),
            good_for_health_nb_distinct = n_distinct(good_for_health)) %>%
  filter(n >= 12)
fit_filter12students <- cfa(model,
                        data = likert_data_atop %>%
                          filter(school_id %in% students_per_school$school_id) %>%
                          mutate(across(think_clearly:getting_hurt,
                                        function(x) as.numeric(x))),
                        group = "school_id")
#lavaan ERROR: some variables have no variance in group 14: good_for_health
fit_sameloadings <- cfa(model,
                            data = likert_data_atop %>%
                            filter(school_id %in% students_per_school$school_id) %>%
                              mutate(school_id = as.factor(school_id),
                                     across(think_clearly:getting_hurt,
                                            function(x) as.numeric(x))) %>%
                          filter(school_id %in% students_per_school$school_id &
                                   !(school_id %in% c("19"))),#school 19 has only one disticnt score for good_for health
                        group = "school_id",
                        group.equal = c("loadings"))
#Error in lav_samplestats_step1(Y = Data, wt = wt, ov.names = ov.names,  :
#lavaan ERROR: some categories of variable `good_for_health' are empty in group 1; frequencies are [1 0 4 10]
#this will be very hard to fix; there are only 4 school in which we have each of the 4 answers

#----------------------ATOP-multilevel-cfa-language----------------------------#
fit_free <- cfa(model,
                data = likert_data_atop %>%
                  filter(community != "Dutch speaking.French speaking") %>%
                  mutate(across(think_clearly:getting_hurt,
                         function(x) as.numeric(x))),
                group = "community")

fit_sameloadings <- cfa(model,
                        data = likert_data_atop %>%
                          filter(community !=
                                   "Dutch speaking.French speaking") %>%
                          mutate(across(think_clearly:getting_hurt,
                                        function(x) as.numeric(x))),
                        group = "community",
                        group.equal = c("loadings"))

fit_sameloadings_sameintercepts <-
  cfa(model,
      data = likert_data_atop %>%
        filter(
          community != "Dutch speaking.French speaking") %>%
        mutate(across(think_clearly:getting_hurt,
                      function(x) as.numeric(x))),
      group = "community",
      group.equal = c("loadings", "intercepts"))
fit_sameintercepts <- cfa(model,
                          data = likert_data_atop %>%
                            filter(community !=
                                     "Dutch speaking.French speaking") %>%
                            mutate(across(think_clearly:getting_hurt,
                                          function(x) as.numeric(x))),
                          group = "community",
                          group.equal = c("intercepts"))
summary(fit_sameloadings, standardized = TRUE, fit.measures = TRUE)
summary(fit_free, standardized = TRUE, fit.measures = TRUE)
#only get fit measures
fitmeasures(fit_free, c("chisq","df","pvalue","cfi","tli","rmsea","srmr",
                         "AIC", "BIC"))
# RMSEA and SRMR show good fit (<0.08) while CFI and TLI show less than good fit (<0.95) -> we check midificationindices
modificationIndices(fit_free)
modindices(fit_free, minimum.value = 10, sort = TRUE) # better use of the function
# this shows that, in group 2, "unknown_persons" may load on ATOP_benefits ->
# this also shows, in group 1  calm_down ~~ learn_new_things
# further model adjustments also show the following high modification index: calm_down ~~ feeling_free for group 2
# we can get a better fit with the following model:
model4 <- "ATOP_benefits =~ think_clearly + good_for_health + calm_down + learn_new_things +   feeling_free + imagination + explore + unknown_persons
  ATOP_fears =~ get_lost + unknown_persons + wild_animals + getting_hurt
  calm_down ~~ learn_new_things
  calm_down ~~ feeling_free"
fit_free4 <- cfa(model4,
                 data = likert_data_atop %>%
                   filter(community != "Dutch speaking.French speaking") %>%
                   mutate(across(think_clearly:getting_hurt,
                                 function(x) as.numeric(x))),
                 group = "community")
fitmeasures(fit_free4, c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "srmr",
                         "AIC", "BIC"))
compRelSEM(fit_free4)
# CFI an TLI are now > 0.95 and RMSEA and SRMR are even lower now. --> all fit measures show good fit.


#The measurementInvariance function will soon be depreciated
measurementInvariance(model = model4,
                      data = likert_data_atop %>%
                        filter(community !=
                                 "Dutch speaking.French speaking") %>%
                        mutate(across(think_clearly:getting_hurt,
                                      function(x) as.numeric(x))),
                      group = "community")
# this now shows that fit.loadings is equally good as fit.configural.
fit4_sameloadings <- cfa(model4,
                        data = likert_data_atop %>%
                          filter(community !=
                                   "Dutch speaking.French speaking") %>%
                          mutate(across(think_clearly:getting_hurt,
                                        function(x) as.numeric(x))),
                        group = "community",
                        group.equal = c("loadings"))
fitmeasures(fit4_sameloadings, c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "srmr",
                         "AIC", "BIC")) #still all show good fit
compRelSEM(fit4_sameloadings)
# alternative function for measurement invariance, also from the semTools package.
measEq.syntax(fit_free)# I can't find a good explanation to know how this works :-s
# Measurement invariance testing
lavTestLRT(fit_free, fit_sameloadings, fit_sameloadings_sameintercepts) # slightly significant -> A model with equal loadings is slightly worse than the free model. A model with equal intercept and equal loadings IS worse. (if I tested this with categorical rather than numeric scores, the equal loadings model was as good as the free model). the second thest is highly significant: Therefore, it is unwise to directly compare the values of the latent means across the two groups.
#separate test since we can only compare nested models
lavTestLRT(fit_free, fit_sameintercepts, fit_sameloadings_sameintercepts) #significant: model with the same intercepts is significantly worse.

#------------------ATOP-multilevel-cfa-school-environment----------------------#
fit_free <- cfa(model,
                data = likert_data_atop %>%
                  mutate(across(think_clearly:getting_hurt,
                                function(x) as.numeric(x))),
                group = "school_context")
fitmeasures(fit_free, c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "srmr",
                         "AIC", "BIC"))
modindices(fit_free, minimum.value = 10, sort = TRUE) # this shows evidence for calm_down ~~ imagination in group 2 and explore ~~ unknown_persons in group 1
#later tests also show evidence for explore ~~ wild_animals in group 1
model_schools <- "ATOP_benefits =~ think_clearly + good_for_health + calm_down + learn_new_things +   feeling_free + imagination + explore
  ATOP_fears =~ get_lost + unknown_persons + wild_animals + getting_hurt
  calm_down ~~ imagination
  explore ~~ unknown_persons
  explore ~~ wild_animals"
fit_free_schools <- cfa(model_schools,
                 data = likert_data_atop %>%
                   mutate(across(think_clearly:getting_hurt,
                                 function(x) as.numeric(x))),
                 group = "school_context")
fitmeasures(fit_free_schools, c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "srmr",
                        "AIC", "BIC")) #cfi and tli >0.95 and "rmsea", "srmr"<0.08
compRelSEM(fit_free_schools)
modindices(fit_free_schools, minimum.value = 10, sort = TRUE) #
lavTestLRT(fit_free, fit_free_schools) #extended model is significantly better
measurementInvariance(model = model_schools,
                      data = likert_data_atop %>%
                        mutate(across(think_clearly:getting_hurt,
                                      function(x) as.numeric(x))),
                      group = "school_context") #fit.loadings en fit.means are equivalent

fit_sameloadings <- cfa(model_schools,
                        data = likert_data_atop %>%
                          mutate(across(think_clearly:getting_hurt,
                                        function(x) as.numeric(x))),
                        group = "school_context",
                        group.equal = c("loadings"))
# Measurement invariance testing
lavTestLRT(fit_free_schools, fit_sameloadings) #same loadings is supported, same intercepts is not
fitmeasures(fit_free_schools, c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "srmr",
                                "AIC", "BIC"))
fitmeasures(fit_sameloadings, c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "srmr",
                                "AIC", "BIC"))
compRelSEM(fit_sameloadings)
summary(fit_sameloadings, standardized = TRUE, fit.measures = TRUE)
#this model shows that the "high greenness" has a higher intercept for think_clearly, good_for_health, calm_down, feeling free, and lower for learn nex things, get lost, and unknown persons
summary(fit_free, standardized = TRUE, fit.measures = TRUE)
lavTestLRT(fit_free, fit_sameintercepts) #same intercepts is not supported


#-------------------ATOP-multilevel-cfa-home-environment-----------------------#
fit_free <- cfa(model,
                data = likert_data_atop %>%
                  mutate(across(think_clearly:getting_hurt,
                                function(x) as.numeric(x))),
                group = "landscape_type")
fitmeasures(fit_free, c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "srmr",
                        "AIC", "BIC"))
modindices(fit_free, minimum.value = 10, sort = TRUE) # this shows evidence for unknown_persons also loading on ATOP_benefits in group 2
model_home <- "ATOP_benefits =~ think_clearly + good_for_health + calm_down + learn_new_things +   feeling_free + imagination + explore + unknown_persons
  ATOP_fears =~ get_lost + unknown_persons + wild_animals + getting_hurt
  calm_down ~~ imagination
  explore ~~ unknown_persons
  explore ~~ wild_animals"
fit_free_home <- cfa(model_home,
                        data = likert_data_atop %>%
                          mutate(across(think_clearly:getting_hurt,
                                        function(x) as.numeric(x))),
                        group = "landscape_type")
fitmeasures(fit_free_home, c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "srmr",
                                "AIC", "BIC")) #cfi and tli >0.95 and "rmsea", "srmr"<0.08
modindices(fit_free_home, minimum.value = 10, sort = TRUE) #
lavTestLRT(fit_free, fit_free_schools) #extended model is significantly better
measurementInvariance(model = model_schools,
                      data = likert_data_atop %>%
                        mutate(across(think_clearly:getting_hurt,
                                      function(x) as.numeric(x))),
                      group = "school_context") #fit.loadings en fit.means are equivalent

fit_sameloadings <- cfa(model_home,
                        data = likert_data_atop %>%
                          mutate(across(think_clearly:getting_hurt,
                                        function(x) as.numeric(x))),
                        group = "landscape_type",
                        group.equal = c("loadings"))
# Measurement invariance testing
lavTestLRT(fit_free_home, fit_sameloadings) #same loadings is supported, same intercepts is not
fitmeasures(fit_free_home, c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "srmr",
                                "AIC", "BIC"))
fitmeasures(fit_sameloadings, c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "srmr",
                                "AIC", "BIC"))
compRelSEM(fit_free_home)
compRelSEM(fit_sameloadings)
summary(fit_sameloadings, standardized = TRUE, fit.measures = TRUE)

#----------ATOP-multilevel-cfa-language- school-and-home-environment-----------#
likert_data_atop <- likert_data_atop %>%
  mutate(grouping = paste0(community, school_context, landscape_type,
                           sep = "-"))
likert_data_atop %>%
  group_by(grouping) %>%
  summarize(n = n())

fit_free <- cfa(model,
                data = likert_data_atop %>%
                  filter(community !=
                           "Dutch speaking.French speaking") %>%
                  mutate(across(think_clearly:getting_hurt,
                                function(x) as.numeric(x))),
                group = "grouping")
fitmeasures(fit_free, c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "srmr",
                        "AIC", "BIC")) #cfi and tli >0.95 and "rmsea", "srmr"<0.08
modindices(fit_free, minimum.value = 10, sort = TRUE) #think_clearly ~~ unknown_persons for group 8,
# WARNING: some estimated ov variances are negative --> only gets worse if modification indices are introduced -> abandon this

likert_data_atop <- likert_data_atop %>%
  mutate(grouping = paste0(community, school_context, sep = "-"))
likert_data_atop %>%
  group_by(grouping) %>%
  summarize(n = n())

fit_free <- cfa(model,
                data = likert_data_atop %>%
                  filter(community !=
                           "Dutch speaking.French speaking") %>%
                  mutate(across(think_clearly:getting_hurt,
                                function(x) as.numeric(x))),
                group = "grouping")
fitmeasures(fit_free, c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "srmr",
                        "AIC", "BIC")) #cfi and tli >0.95 and "rmsea", "srmr"<0.08
modindices(fit_free, minimum.value = 10, sort = TRUE) #think_clearly ~~ unknown_persons for group 3, explore ~~ unknown_persons for group 4, ATOP_fears =~ explore for group 4
model_grouping <- "ATOP_benefits =~ think_clearly + good_for_health + calm_down + learn_new_things +   feeling_free + imagination + explore
  ATOP_fears =~ get_lost + unknown_persons + wild_animals + getting_hurt + explore
  think_clearly ~~ unknown_persons
  explore ~~ unknown_persons"
fit_grouping <- cfa(model_grouping,
                data = likert_data_atop %>%
                  filter(community !=
                           "Dutch speaking.French speaking") %>%
                  mutate(across(think_clearly:getting_hurt,
                                function(x) as.numeric(x))),
                group = "grouping")
fitmeasures(fit_grouping, c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "srmr",
                        "AIC", "BIC")) #cfi and tli >0.95 and "rmsea", "srmr"<0.08
modindices(fit_grouping, minimum.value = 10, sort = TRUE)

#------------------------ATOP-external-validation------------------------------#
#----------using true outdoor play / independent mobility questions?-----------#
# We decide to use the community (language of the questionnaire) as a grouping variable:
fit_atop <- cfa(model,
                data = likert_data_atop %>%
                  filter(community != "Dutch speaking.French speaking") %>%
                  mutate(across(think_clearly:getting_hurt,
                                function(x) as.numeric(x))),
                group = "community")
likert_data_atop_factorscores <- likert_data_atop %>%
  dplyr::filter(community != "Dutch speaking.French speaking") %>%
  filter(!if_any(think_clearly:getting_hurt,is.na)) %>%
  arrange(community)
factorscores <- lavPredict(fit_atop, newdata = likert_data_atop_factorscores,
                           append = TRUE)
likert_data_atop_factorscores <- likert_data_atop_factorscores %>%
  mutate(ATOP_benefits = c(factorscores$`Dutch speaking`[,1],
                          factorscores$`French speaking`[,1]),
         ATOP_fears =  c(factorscores$`Dutch speaking`[,2],
                         factorscores$`French speaking`[,2])) %>%
  mutate(ATOP_benefits_cat = cut_number(ATOP_benefits, 4),
         ATOP_fears_cat = cut_number(ATOP_fears, 4))

atop_data_p1 <- read_resource(package = baseball_package,
                              resource_name = "wp5_parents_outdoor_play_data_part1")
atop_data_p2 <- read_resource(package = baseball_package,
                              resource_name = "wp5_parents_outdoor_play_data_part2")
atop_data_p3 <- read_resource(package = baseball_package,
                              resource_name = "wp5_parents_outdoor_play_data_part3")
atop_data_p4 <- read_resource(package = baseball_package,
                              resource_name = "wp5_parents_outdoor_play_data_part4")
atop_key_p <- read_resource(package = baseball_package,
                            resource_name = "wp5_parents_outdoor_play_key")
#ATOP_benefits vs organised activities
likert_data_atop_factorscores %>%
  left_join(atop_data_p1) %>%
  ggplot() +
  geom_violin(aes(x = activity, y = response, color = ATOP_benefits_cat,
                  fill = ATOP_benefits_cat),
              alpha = 0.4) +
  geom_boxplot(aes(x = activity, y = response, color = ATOP_benefits_cat,
                  fill = ATOP_benefits_cat),
               width = 0.5) +
  scale_x_discrete(breaks = unique(atop_data_p1$activity),
                   labels = str_wrap(c("outdoor sports in nature",
                              "outdoor sports, artificial surface",
                              "other organized outdoor activities",
                              "organised indoor sports and activities"), 20)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_color_brewer(palette = "YlOrRd") +
  scale_fill_brewer(palette = "YlOrRd")
#ATOP_fears vs organised activities
likert_data_atop_factorscores %>%
  left_join(atop_data_p1) %>%
  ggplot() +
  geom_violin(aes(x = activity, y = response, color = ATOP_fears_cat,
                  fill = ATOP_fears_cat),
              alpha = 0.4) +
  geom_boxplot(aes(x = activity, y = response, color = ATOP_fears_cat,
                   fill = ATOP_fears_cat),
               width = 0.5) +
  scale_x_discrete(breaks = unique(atop_data_p1$activity),
                   labels = str_wrap(c("outdoor sports in nature",
                                       "outdoor sports, artificial surface",
                                       "other organized outdoor activities",
                                       "organised indoor sports and activities"), 20)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_color_brewer(palette = "YlOrRd") +
  scale_fill_brewer(palette = "YlOrRd")

#ATOP_benefits vs outdoor play activities
likert_data_atop_factorscores %>%
  left_join(atop_data_p2) %>%
  mutate(response = 1*(!str_detect(response, "no"))) %>%
  group_by(question_short, ATOP_benefits_cat) %>%
  summarize(prop = mean(response),
            nb =n()) %>%
  ungroup() %>%
  mutate(upper = prop + 1.96*(sqrt(prop*(1-prop)) / nb),
         lower = prop - 1.96*(sqrt(prop*(1-prop)) / nb)) %>%
  ggplot(aes(x = question_short)) +
  geom_point(aes(y = prop, color = ATOP_benefits_cat)) +
  geom_errorbar(aes(ymin = lower, ymax = upper, color = ATOP_benefits_cat),
                width = 0.2) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_color_brewer(palette = "YlOrRd")

#ATOP_fears vs outdoor play activities
likert_data_atop_factorscores %>%
  left_join(atop_data_p2) %>%
  mutate(response = 1*(!str_detect(response, "no"))) %>%
  group_by(question_short, ATOP_fears_cat) %>%
  summarize(prop = mean(response),
            nb =n()) %>%
  ungroup() %>%
  mutate(upper = prop + 1.96*(sqrt(prop*(1-prop)) / nb),
         lower = prop - 1.96*(sqrt(prop*(1-prop)) / nb)) %>%
  ggplot(aes(x = question_short)) +
  geom_point(aes(y = prop, color = ATOP_fears_cat)) +
  geom_errorbar(aes(ymin = lower, ymax = upper, color = ATOP_fears_cat),
                width = 0.2) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_color_brewer(palette = "YlOrRd")

#--------------------------NATURE CONNECTEDNESS--------------------------------#
nc_data <- read_resource(package = baseball_package,
                         resource_name = "wp5_nc_data")
nc_key <- read_resource(package = baseball_package,
                        resource_name = "wp5_nc_key")
#inspect the description
get_schema(package = baseball_package,
           resource_name = "wp5_nc_data")$fields %>%
  bind_rows() %>%
  kable(booktabs = TRUE) %>%
  kableExtra::kable_styling()

likert_data_nc <- nc_data %>%
  as.data.frame() %>%
  mutate(interpretation = ifelse(interpretation == "No response",
                                 NA, interpretation),
         interpretation = factor(as.factor(interpretation),
                                 levels = c("Disagree", "Slightly disagree",
                                            "Don\'t know",
                                            "Moderately agree", "Agree"),
                                 ordered = TRUE)) %>%
  dplyr::select(question_short, interpretation, child_id) %>%
  mutate(interpretation = as.numeric(interpretation)) %>%
  group_by(child_id, question_short) %>%
  summarize(interpretation = mean(interpretation, na.rm = TRUE)) %>%
  ungroup() %>%
  pivot_wider(names_from = question_short, values_from = interpretation,
              id_cols = child_id) %>%
  left_join(children) %>%
  left_join(schools)

#----------------------NatCon-multilevel-cfa-school----------------------------#
model <- "NC =~ beauty + respect + happy + spending_time + enjoy + part_of"
fit_free <- cfa(model,
                data = likert_data_nc,
                group = "school_id")
#lavaan ERROR:
#  some variables have no variance in group 2: happy enjoy

#---------------------NatCon-multilevel-cfa-language---------------------------#
fit_free <- cfa(model,
                data = likert_data_nc %>%
                  filter(community != "Dutch speaking.French speaking"),
                group = "community")
fitmeasures(fit_free, c("chisq","df","pvalue","cfi","tli","rmsea","srmr",
                        "AIC", "BIC"))
modificationIndices(fit_free, minimum.value = 10, sort = TRUE) # we should add beauty ~~ respect for group 2 (french speaking)
model_nc_community <- "NC =~ beauty + respect + happy + spending_time + enjoy + part_of
  beauty ~~ respect"
fit_free_community <- cfa(model_nc_community,
                data = likert_data_nc %>%
                  filter(community != "Dutch speaking.French speaking"),
                group = "community")
modificationIndices(fit_free_community, minimum.value = 10, sort = TRUE)
fitmeasures(fit_free_community, c("chisq","df","pvalue","cfi","tli","rmsea","srmr",
                        "AIC", "BIC"))
compRelSEM(fit_free_community)
measurementInvariance(model = model_nc_community,
                    data = likert_data_nc %>%
                      filter(community != "Dutch speaking.French speaking"),
                    group = "community")
fit_sameloadings_community <- cfa(model_nc_community,
                          data = likert_data_nc %>%
                            filter(community != "Dutch speaking.French speaking"),
                          group = "community",
                          group.equal = "loadings")
summary(fit_sameloadings, standardized = TRUE, fit.measures = TRUE)
summary(fit_free, standardized = TRUE, fit.measures = TRUE)
#only get fit measures
fitmeasures(fit_sameloadings_community, c("chisq","df","pvalue","cfi","tli","rmsea","srmr",
                        "AIC", "BIC"))
compRelSEM(fit_sameloadings_community)
lavTestLRT(fit_free_community, fit_sameloadings_community) #same loadings isequal to full model
lavTestLRT(fit_free, fit_free_community) #additional covariance is significantly better

#-----------------NatCon-multilevel-cfa-school-environment---------------------#
fit_free <- cfa(model,
                data = likert_data_nc,
                group = "school_context")
fitmeasures(fit_free, c("chisq","df","pvalue","cfi","tli","rmsea","srmr",
                        "AIC", "BIC"))
modificationIndices(fit_free, minimum.value = 10, sort = TRUE) # we should add beauty ~~ respect for group 1 (grijze speelplaats)
model_nc_community <- "NC =~ beauty + respect + happy + spending_time + enjoy + part_of
  beauty ~~ respect"
fit_free_school <- cfa(model_nc_community,
                          data = likert_data_nc,
                          group = "school_context")
modificationIndices(fit_free_school, minimum.value = 10, sort = TRUE)
fitmeasures(fit_free_school, c("chisq","df","pvalue","cfi","tli","rmsea","srmr",
                                  "AIC", "BIC"))
compRelSEM(fit_free_school)
measurementInvariance(model = model_nc_community,
                      data = likert_data_nc,
                      group = "school_context")
fit_sameloadings_school <- cfa(model_nc_community,
                                  data = likert_data_nc,
                                  group = "school_context",
                                  group.equal = "loadings")
summary(fit_sameloadings_school, standardized = TRUE, fit.measures = TRUE)
summary(fit_free, standardized = TRUE, fit.measures = TRUE)
#only get fit measures
fitmeasures(fit_sameloadings_school, c("chisq","df","pvalue","cfi","tli","rmsea","srmr",
                                          "AIC", "BIC"))
compRelSEM(fit_sameloadings_school)
lavTestLRT(fit_free_school, fit_sameloadings_school) #same loadings is equal to full model
lavTestLRT(fit_free, fit_free_school) #additional covariance is significantly better


#------------------NatCon-multilevel-cfa-home-environment----------------------#
fit_free <- cfa(model,
                data = likert_data_nc,
                group = "landscape_type")
fitmeasures(fit_free, c("chisq","df","pvalue","cfi","tli","rmsea","srmr",
                        "AIC", "BIC"))
modificationIndices(fit_free, minimum.value = 10, sort = TRUE) # we should add beauty ~~ respect for group 2 (low naturalness)
model_nc_community <- "NC =~ beauty + respect + happy + spending_time + enjoy + part_of
  beauty ~~ respect"
fit_free_landscape <- cfa(model_nc_community,
                       data = likert_data_nc,
                       group = "landscape_type")
modificationIndices(fit_free_landscape, minimum.value = 10, sort = TRUE)
fitmeasures(fit_free_landscape, c("chisq","df","pvalue","cfi","tli","rmsea","srmr",
                               "AIC", "BIC"))
compRelSEM(fit_free_landscape)
measurementInvariance(model = model_nc_community,
                      data = likert_data_nc,
                      group = "landscape_type") # only same intercepts is supported
fit_sameintercept_landscape <- cfa(model_nc_community,
                               data = likert_data_nc,
                               group = "landscape_type",
                               group.equal = "intercepts")
summary(fit_sameintercept_landscape, standardized = TRUE, fit.measures = TRUE)
summary(fit_free, standardized = TRUE, fit.measures = TRUE)
#only get fit measures
fitmeasures(fit_sameintercept_landscape, c("chisq","df","pvalue","cfi","tli","rmsea","srmr",
                                       "AIC", "BIC"))
lavTestLRT(fit_free_landscape, fit_sameintercept_landscape) #same loadings is equal to full model
lavTestLRT(fit_free, fit_free_landscape) #additional correlation makes sense; the model is significantly better

#----------------------NatCon-multilevel-cfa-grouping--------------------------#
likert_data_nc <- likert_data_nc %>%
  mutate(grouping = paste0(community, school_context, landscape_type,
                           sep = "-"))
likert_data_nc %>%
  group_by(grouping) %>%
  summarize(n = n())

fit_free <- cfa(model,
                data = likert_data_nc %>%
                  filter(community !=
                           "Dutch speaking.French speaking"),
                group = "grouping")
fitmeasures(fit_free, c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "srmr",
                        "AIC", "BIC")) #cfi and tli >0.95 and "rmsea", "srmr"<0.08
modindices(fit_free, minimum.value = 10, sort = TRUE) #no modifications
measurementInvariance(model = model,
                      data = likert_data_nc %>%
                        filter(community !=
                                 "Dutch speaking.French speaking"),
                      group = "grouping") #configural model is best.

likert_data_nc <- likert_data_nc %>%
  mutate(grouping = paste0(community, school_context, sep = "-"))
likert_data_nc %>%
  group_by(grouping) %>%
  summarize(n = n())

fit_free <- cfa(model,
                data = likert_data_nc %>%
                  filter(community !=
                           "Dutch speaking.French speaking"),
                group = "grouping")
fitmeasures(fit_free, c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "srmr",
                        "AIC", "BIC")) #cfi and tli >0.95 and "rmsea", "srmr"<0.08; even better fit then with 8 groups
modindices(fit_free, minimum.value = 10, sort = TRUE)#no modifications

#-----------------------NatCon-external-validation-----------------------------#
#----------using true outdoor play / independent mobility questions?-----------#



#---------------------------------REPS-----------------------------------------#
# a description of how to view and read the data from the data package
reps_data1 <- read_resource(package = baseball_package,
                            resource_name =
                              "wp5_parents_risk_protection_data_part1") %>%
  mutate(response = factor(as.factor(response),
                           levels = c("Strongly disagree", "Disagree",
                                      "Slightly disagree",
                                      "Neigher agree nor disagree",
                                      "Slightly agree", "Agree",
                                      "Strongly agree")))

reps_data2 <- read_resource(package = baseball_package,
                            resource_name = "wp5_parents_risk_protection_data_part2") %>%
  mutate(response = factor(as.factor(response),
                           levels = c("Never", "Seldom", "Occassionally",
                                      "Sometimes", "Regularly", "Often",
                                      "Always")))
reps_data <- reps_data1 %>%
  mutate(response = as.numeric(response)) %>%
  rbind(reps_data2 %>%
          mutate(response = as.numeric(response)))
likert_data_reps <- reps_data %>%
  as.data.frame() %>%
  mutate(interpretation = response) %>%
  rbind(c("10-5X-03", "1", "Single response", "wash_hands", "x6_14", NA,
          NA)) %>% #this line is missing from the data
  dplyr::select(question_short, interpretation, child_id) %>%
  mutate(interpretation = as.numeric(interpretation)) %>%#for some reason, this is changed to character
  dplyr::group_by(child_id, question_short) %>% #There are children whose parents filled it in more than once
  dplyr::summarise(interpretation = mean(interpretation, na.rm = TRUE),
                   .groups = "drop") %>%
  pivot_wider(names_from = question_short, values_from = interpretation,
              id_cols = child_id) %>%
  left_join(children) %>%
  left_join(schools)

reps_key <- read_resource(package = baseball_package,
                          resource_name = "wp5_parents_risk_protection_key")
reps_key <- cbind(reps_key, factor = c("protection_from_injury", "engagement_with_risk", "protection_from_injury", "engagement_with_risk", "protection_from_injury", "engagement_with_risk", "protection_from_injury", "engagement_with_risk", "protection_from_injury", "engagement_with_risk", "protection_from_injury", "engagement_with_risk", "rain", "wash_hands", "protection_from_injury","protection_from_injury"))
#-----------------------REPS-multilevel-cfa-school-----------------------------#
model <- "protection_from_injury =~ concerned_injury + prevention_importance + concerned_hazards + avoid_risk + importance_supervision + chance_of_injury + encourage_safe_activities + limit_dangerous_activities
          risk_engagement =~ promote_physical_challenges + physical_limits + explore_new_environments + benefits_outweigh_risk + importance_managing_risk + risk_self_confidence"
fit_free <- cfa(model,
                data = likert_data_reps,
                group = "school_id")
#lavaan ERROR:
#some variables have no variance in group 5: importance_supervision promote_physical_challenges

#----------------------REPS-multilevel-cfa-language----------------------------#
fit_free <- cfa(model,
                data = likert_data_reps %>%
                  filter(community != "Dutch speaking.French speaking"),
                group = "community")
# Number of observations per group:               Used       Total
# Dutch speaking                                 118         123
# French speaking                                 95         107

fitmeasures(fit_free, c("chisq","df","pvalue","cfi","tli","rmsea","srmr",
                        "AIC", "BIC")) #bad fit! We know from EFA that encourage_safe_activities is a problematic statement
model <- "protection_from_injury =~ concerned_injury + prevention_importance + concerned_hazards + avoid_risk + importance_supervision + chance_of_injury + limit_dangerous_activities + play_in_rain
          risk_engagement =~ promote_physical_challenges + physical_limits + explore_new_environments + benefits_outweigh_risk + importance_managing_risk + risk_self_confidence"
fit_free <- cfa(model,
                data = likert_data_reps %>%
                  filter(community != "Dutch speaking.French speaking") %>%
                  mutate(play_in_rain = -1*(play_in_rain - 8)),
                group = "community")
fitmeasures(fit_free, c("chisq","df","pvalue","cfi","tli","rmsea","srmr",
                        "AIC", "BIC")) #slightly better but still bad!
modificationIndices(fit_free, minimum.value = 10, sort = TRUE) # we should add prevention_importance ~~  chance_of_injury, concerned_injury ~~ concerned_hazards, concerned_hazards ~~ avoid_risk and concerned_hazards ~~   physical_limits for group 2, we should add prevention_importance ~~ avoid_risk for group1
model_reps_community <- "protection_from_injury =~ prevention_importance + concerned_injury + concerned_hazards + avoid_risk + importance_supervision + chance_of_injury + limit_dangerous_activities + play_in_rain
          risk_engagement =~ promote_physical_challenges + physical_limits + explore_new_environments + benefits_outweigh_risk + importance_managing_risk + risk_self_confidence
          prevention_importance ~~  chance_of_injury
          concerned_injury ~~ concerned_hazards
          concerned_hazards ~~ avoid_risk
          concerned_hazards ~~   physical_limits
          prevention_importance ~~ avoid_risk"
fit_free_community <- cfa(model_reps_community,
                          data = likert_data_reps %>%
                            filter(community !=
                                     "Dutch speaking.French speaking") %>%
                            mutate(play_in_rain = -1*(play_in_rain - 8)),
                          group = "community")
modificationIndices(fit_free_community, minimum.value = 10, sort = TRUE)
fitmeasures(fit_free_community, c("chisq","df","pvalue","cfi","tli","rmsea","srmr",
                                  "AIC", "BIC")) #fit is still not great
compRelSEM(fit_free_community) #fit is still not great
measurementInvariance(model = model_reps_community,
                      data = likert_data_reps %>%
                        filter(community != "Dutch speaking.French speaking") %>%
                        mutate(play_in_rain = -1*(play_in_rain - 8)),
                      group = "community") # we cannot simplify the model

#only get fit measures
lavTestLRT(fit_free, fit_free_community) #additional covariance is significantly better
fit_free_community_nogroup <- cfa(model_reps_community,
                          data = likert_data_reps %>%
                            filter(community !=
                                     "Dutch speaking.French speaking") %>%
                            mutate(play_in_rain = -1*(play_in_rain - 8)))
fitmeasures(fit_free_community_nogroup, c("chisq","df","pvalue","cfi","tli","rmsea","srmr",
                                  "AIC", "BIC")) #performance is better without grouping but still not great
compRelSEM(fit_free_community_nogroup)
#tests with a model that has only some loadings between groups don't consistently impove model fit.
# fit_free_community2 <- cfa(model_reps_community,
#                           data = likert_data_reps %>%
#                             filter(community !=
#                                      "Dutch speaking.French speaking") %>%
#                             mutate(play_in_rain = -1*(play_in_rain - 8)),
#                           group = "community",
#                           group.equal = "loadings",
#                           group.partial = c("protection_from_injury =~ concerned_injury",
#                                             "protection_from_injury =~ concerned_hazards",
#                                             "protection_from_injury =~ play_in_rain",
#                                             "risk_engagement =~ importance_managing_risk",
#                                             "risk_engagement =~ risk_self_confidence"))
# fit_free_community_sameloadings <- cfa(model_reps_community,
#                            data = likert_data_reps %>%
#                              filter(community !=
#                                       "Dutch speaking.French speaking") %>%
#                              mutate(play_in_rain = -1*(play_in_rain - 8)),
#                            group = "community",
#                            group.equal = "loadings")
# lavTestLRT(fit_free_community2, fit_free_community,fit_free_community_sameloadings) #additional covariance is significantly better

#------------------REPS-multilevel-cfa-school-environment----------------------#
fit_free <- cfa(model,
                data = likert_data_reps,
                group = "school_context")
#  Number of observations per group:               Used       Total
# low greenness                                  140         148
# high greenness                                  73          82
fitmeasures(fit_free, c("chisq","df","pvalue","cfi","tli","rmsea","srmr",
                        "AIC", "BIC"))
modificationIndices(fit_free, minimum.value = 10, sort = TRUE) # we should add for group1: concerned_injury ~~    physical_limits, concerned_injury ~~ concerned_hazards, concerned_hazards ~~ limit_dangerous_activities, risk_engagement =~ prevention_importance (watch out; this is not a correlation!), concerned_injury ~~ chance_of_injury, concerned_injury ~~ explore_new_environments . For group 2, we should add prevention_importance ~~  chance_of_injury. Later, we also add concerned_hazards ~~ physical_limits, concerned_hazards ~~ physical_limits, and concerned_hazards ~~ physical_limits for group1
model_reps_school <- "protection_from_injury =~ prevention_importance + concerned_injury + concerned_hazards + avoid_risk + importance_supervision + chance_of_injury + limit_dangerous_activities + play_in_rain
          risk_engagement =~ promote_physical_challenges + physical_limits + explore_new_environments + benefits_outweigh_risk + importance_managing_risk + risk_self_confidence + prevention_importance
          concerned_injury ~~ physical_limits
          concerned_injury ~~ concerned_hazards
          concerned_hazards ~~ limit_dangerous_activities
          concerned_injury ~~ chance_of_injury
          concerned_injury ~~ explore_new_environments
          prevention_importance ~~ chance_of_injury
          concerned_hazards ~~ physical_limits"
fit_free_school <- cfa(model_reps_school,
                       data = likert_data_reps,
                       group = "school_context")
modificationIndices(fit_free_school, minimum.value = 10, sort = TRUE)
fitmeasures(fit_free_school, c("chisq","df","pvalue","cfi","tli","rmsea","srmr",
                               "AIC", "BIC")) #better fit than the languages
compRelSEM(fit_free_school)
measurementInvariance(model = model_reps_school,
                      data = likert_data_reps,
                      group = "school_context") #intercepts can be the same over the groups
fit_sameintercepts_school <- cfa(model_reps_school,
                               data = likert_data_reps,
                               group = "school_context",
                               group.equal = "intercepts")
summary(fit_sameintercepts_school, standardized = TRUE, fit.measures = TRUE)
summary(fit_free, standardized = TRUE, fit.measures = TRUE)
#only get fit measures
fitmeasures(fit_sameintercepts_school, c("chisq","df","pvalue","cfi","tli","rmsea","srmr",
                                       "AIC", "BIC"))
compRelSEM(fit_sameintercepts_school)
lavTestLRT(fit_free_school, fit_sameintercepts_school) #same loadings is equal to full model
lavTestLRT(fit_free, fit_free_school) #additional covariance is significantly better

#-------------------REPS-multilevel-cfa-home-environment-----------------------#
fit_free <- cfa(model,
                data = likert_data_reps,
                group = "landscape_type")
fitmeasures(fit_free, c("chisq","df","pvalue","cfi","tli","rmsea","srmr",
                        "AIC", "BIC"))
modificationIndices(fit_free, minimum.value = 10, sort = TRUE) # we should add chance_of_injury ~~          physical_limits and concerned_injury ~~ explore_new_environments for group 2 and prevention_importance ~~         chance_of_injury for group1
model_reps_landscape <- "protection_from_injury =~ prevention_importance + concerned_injury + concerned_hazards + avoid_risk + importance_supervision + chance_of_injury + limit_dangerous_activities + play_in_rain
          risk_engagement =~ promote_physical_challenges + physical_limits + explore_new_environments + benefits_outweigh_risk + importance_managing_risk + risk_self_confidence
          chance_of_injury ~~ physical_limits
          concerned_injury ~~ explore_new_environments
          prevention_importance ~~ chance_of_injury"
fit_free_landscape <- cfa(model_reps_landscape,
                          data = likert_data_reps,
                          group = "landscape_type")
modificationIndices(fit_free_landscape, minimum.value = 10, sort = TRUE)
fitmeasures(fit_free_landscape, c("chisq","df","pvalue","cfi","tli","rmsea","srmr",
                                  "AIC", "BIC")) #approximately the same as for community - less good than school_context
compRelSEM(fit_free_landscape)
measurementInvariance(model = model_reps_landscape,
                      data = likert_data_reps,
                      group = "landscape_type") # both same loading and same mean is supported
fit_sameloading_landscape <- cfa(model_reps_community,
                                   data = likert_data_reps,
                                   group = "landscape_type",
                                   group.equal = "loadings")
fit_samemean_landscape <- cfa(model_reps_community,
                                 data = likert_data_reps,
                                 group = "landscape_type",
                                 group.equal = "means")
fit_sameloadingmeans_landscape <- cfa(model_reps_community,
                                 data = likert_data_reps,
                                 group = "landscape_type",
                                 group.equal = c("loadings", "means"))
lavTestLRT(fit_free_landscape, fit_sameloading_landscape, fit_sameloadingmeans_landscape)
lavTestLRT(fit_free_landscape, fit_samemean_landscape, fit_sameloadingmeans_landscape)
compRelSEM(fit_sameloading_landscape)
compRelSEM(fit_samemean_landscape)
compRelSEM(fit_sameloadingmeans_landscape)
summary(fit_sameloading_landscape, standardized = TRUE, fit.measures = TRUE)
summary(fit_free, standardized = TRUE, fit.measures = TRUE)
#only get fit measures
fitmeasures(fit_sameloading_landscape, c("chisq","df","pvalue","cfi","tli","rmsea","srmr",
                                           "AIC", "BIC"))# all fit measure still show bad fit.

#------------------------REPS-external-validation------------------------------#
#----------using true outdoor play / independent mobility questions?-----------#
