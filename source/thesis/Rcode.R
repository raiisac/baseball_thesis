## ----setup, include=FALSE---------------------------------------------------------------------------------
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
library(effectsize)
library(semPlot)
library(semTools)
source(find_root_file("source", "R", "fa_table.R",
                       criterion = has_file("baseball_thesis.Rproj")))

knitr::opts_chunk$set(echo = FALSE)

baseball_package <- read_package(
  find_root_file(
    "data", "processed", "datapackage_analysis",
    "datapackage.json",
    criterion = has_file("baseball_thesis.Rproj")))
atop_key <- read_resource(package = baseball_package,
              resource_name = "wp5_atop_questions")
nc_key <- read_resource(package = baseball_package,
              resource_name = "wp5_nc_key")
atop_key_p <- read_resource(package = baseball_package,
              resource_name = "wp5_parents_outdoor_play_key")
reps_key <- read_resource(package = baseball_package,
              resource_name = "wp5_parents_risk_protection_key")
im_key <- read_resource(package = baseball_package,
              resource_name = "wp5_parents_independent_mobility_key")
children <- read_resource(package = baseball_package,
              resource_name = "wp2_participants_metadata") %>%
  left_join(read_resource(package = baseball_package,
                          resource_name = "wp2_school_data") %>%
              dplyr::select(-n_students))

## ----message=FALSE, warning=FALSE, include=FALSE----------------------------------------------------------
######-------------------------DATA EXPLORATION----------------------------#####
# a description of how to view and read the data from the data package
baseball_package <- read_package(
  find_root_file("data", "processed", "datapackage_analysis",
                 "datapackage.json",
                 criterion = has_file("baseball_thesis.Rproj")))
#overview of all resources (=datasets) in the data package
resources(baseball_package)
#Extract a single resource and assign it to a R object
wp3_ua_sirm_data <- read_resource(package = baseball_package,
                                  resource_name = "wp3_ua_sirm_data")
#inspect the description
get_schema(package = baseball_package,
           resource_name = "wp3_ua_sirm_data")$fields %>%
  bind_rows() %>%
  kable(booktabs = TRUE) %>%
  kableExtra::kable_styling()

#General data about the parents
gen_data <- read_resource(package = baseball_package,
                          resource_name = "wp5_parents_general_questions_data")
gen_key <- read_resource(package = baseball_package,
                         resource_name = "wp5_parents_general_questions_key")

#allergy
allergy_data <- read_resource(package = baseball_package,
                              resource_name = "wp5_parents_allergy_related_questions_data")
allergy_key <- read_resource(package = baseball_package,
                             resource_name = "wp5_parents_allergy_related_questions_key")

#culture
culture_data <- read_resource(package = baseball_package,
                              resource_name = "wp5_parents_cultural_background_data")
culture_key <- read_resource(package = baseball_package,
                             resource_name = "wp5_parents_cultural_background_key")

#socio-economic status
ses_data <- read_resource(package = baseball_package,
                          resource_name = "wp5_parents_ses_questions_data")
ses_key <- read_resource(package = baseball_package,
                         resource_name = "wp5_parents_ses_questions_key")

#living environment
liv_envir_data <- read_resource(package = baseball_package,
                                resource_name = "wp5_parents_living_environment_data")
liv_envir_key <- read_resource(package = baseball_package,
                               resource_name = "wp5_parents_living_environment_key")


## ----message=FALSE, warning=FALSE, include=FALSE----------------------------------------------------------
data_school <- read_resource(package = baseball_package,
                             resource_name = "wp2_school_data")
data_particip <- read_resource(package = baseball_package,
                               resource_name = "wp2_participants_metadata")


## ----schools-size, fig.cap = "Number of schools per province (number on the left) and school size.", message=FALSE, warning=FALSE, echo=FALSE, fig.height = 4----
ordered_provinces <- c(data_school %>%
                         filter(province_name != "Brussels Capital Region") %>%
                         arrange(community, province_name) %>%
                         dplyr::pull(province_name) %>%
                         unique(), "Brussels Capital Region")
school_by_province <- data_school %>%
  group_by(province_name) %>%
  summarize(n = n())
school_by_community <- data_school %>%
  group_by(community) %>%
  summarize(n = n())
data_school %>%
  mutate(province_name = factor(as.factor(province_name),
                                levels = ordered_provinces,
                                ordered = TRUE)) %>%
  ggplot(aes(x = n_students, y = province_name)) +
  geom_point(aes(color = community)) +
  geom_text(data = school_by_province,
            aes(x = min(data_school$n_students) - 20,
                y = province_name,
                label = n)) +
  xlab("number of students in the school") +
  ylab("province") +
  theme_bw() +
  scale_color_discrete("Language",
                       breaks = c("French speaking", "Dutch speaking",
                                  "Dutch speaking.French speaking"),
                       labels = c("French-speaking", "Dutch-speaking",
                                  "Bilingual"))


## ----paticip-age-sex, fig.cap = "Year of birth (if known) and sex of the participating child. The numbers on the left show the total number of children in each group.", fig.height = 6, warning = FALSE, message = FALSE----
data_particip <- data_particip %>%
  left_join(data_school) %>%
  mutate(province_name = factor(as.factor(province_name),
                                levels = ordered_provinces,
                                ordered = TRUE))
nb_child_by_province <-
  data_particip %>%
  filter(!is.na(biological_sex)) %>%
  group_by(biological_sex, province_name) %>%
  summarize(n = n()) %>%
  ungroup()
labels <- c("Antw", "O-Vl", "Vl-Br",
            "W-Vl", "Hain", "Liège", "Luxem",
            "Namur", "Brus")
names(labels) <- c("Antwerpen", "Oost-Vlaanderen", "Vlaams-Brabant",
                   "West-Vlaanderen", "Hainaut", "Liège", "Luxembourg",
                   "Namur", "Brussels Capital Region")
data_particip %>%
  filter(!is.na(biological_sex)) %>%
  mutate(birth_year = ifelse(is.na(birth_year), "Unknown",
                             as.character(birth_year))) %>%
  ggplot(aes(x = birth_year, fill = biological_sex), position = dodge) +
  geom_bar() +
  geom_text(data = nb_child_by_province, aes(x = "2010", label = n,  y = 35)) +
  facet_grid(rows = vars(province_name),
             cols = vars(biological_sex),
             labeller = labeller(province_name = labels)) +
  ylab("") + xlab("birth year") +
  theme(legend.position = "bottom") + theme_bw()


## ----nb-years-in-school, fig.cap = "The figure on the left shows how long the children have been at their current school and how many years they spent in primary school. The right figure shows the  relation of the parent to the child.", message=FALSE, warning=FALSE----
p1 <- gen_data %>%
  group_by(years_at_school, years_primary) %>%
  summarize(n = n()) %>%
  ungroup() %>%
  ggplot() +
  geom_raster(aes(x = years_at_school, y = years_primary, fill = n)) +
  geom_text(aes(x = years_at_school, y = years_primary, label = n,
                color = (n < 75))) +
  scale_color_manual(values = c("black", "white"),
                     breaks = c(FALSE, TRUE),
                     guide = "none") +
  scale_fill_continuous(guide = "none") +
  xlab("number of years in their current school") +
  ylab("number of years in primary school") +
  theme_bw()

p2 <- gen_data %>%
  ggplot() +
  geom_bar(aes(x = respondent_relation)) +
  xlab("relation to the child") +
  ylab("number of parents") +
  theme_bw() +
  scale_x_discrete(labels = function(x) str_remove(str_wrap(x, width = 10),
                                                   "\\(fill in\\)"))
p1 + p2 +  plot_layout(widths = c(2, 1))


## ----message=FALSE, warning=FALSE, include = FALSE--------------------------------------------------------
#get schools data
baseball_derived_package <- read_package(
  find_root_file(
    "data", "processed", "datapackage_derived_data",
    "datapackage.json",
    criterion = has_file("baseball_thesis.Rproj")))
schools <- read_resource(package = baseball_derived_package,
                         resource_name = "wp1_school_level_key_variables")
child_derived <- read_resource(package = baseball_derived_package,
                               resource_name = "wp1_child_level_key_variables")


## ----eda-school-greenery, fig.cap="Number of schools (left) and number of children with a high or low naturalness and greenness.", message=FALSE, warning=FALSE, fig.height = 4----
p1 <- schools %>%
  group_by(landscape_type, school_context) %>%
  summarize(n = n()) %>%
  ungroup() %>%
  ggplot(aes(x = as.factor(school_context), y = as.factor(landscape_type))) +
  geom_tile(aes(fill = n)
  ) +
  geom_text(aes(label = n), color = "white") +
  theme_bw() +
  xlab("school context (playground)") +
  ylab("landscape type (school surroundings)") +
  theme(legend.position = "none")
p2 <- children %>%
  left_join(schools %>% dplyr::select(school_id, landscape_type,
                                      school_context)) %>%
  group_by(landscape_type, school_context) %>%
  summarize(n = n()) %>%
  ungroup() %>%
  ggplot(aes(x = as.factor(school_context), y = as.factor(landscape_type))) +
  geom_tile(aes(fill = n)
  ) +
  geom_text(aes(label = n), color = "white") +
  theme_bw() +
  xlab("school context (playground)") +
  ylab("landscape type (school surroundings)") +
  theme(legend.position = "none",
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())
p1 + p2 +
  plot_layout(widths = c(1, 1))


## ----eda-ses, fig.cap = "Socio-economic status distributions in each of the schools.", message=FALSE, warning=FALSE----
no_ses <-
  child_derived %>%
  group_by(school_id) %>%
  summarize(n = n(),
            nb_missing = sum(is.na(ses))) %>%
  ungroup() %>%
  mutate(perc_missing = sprintf("%.0f%%", nb_missing / n * 100))
child_derived %>%
  left_join(schools %>%
              dplyr::select(school_id, landscape_type,
                            school_context, ses) %>%
              rename(ses_school = ses)) %>%
  mutate(school_type = sprintf("%s - %s", school_context, landscape_type)) %>%
  ggplot(aes(y = school_id, color = as.factor(school_type))) +
  geom_point(aes(x = ses)) +
  geom_text(data = no_ses, aes(label = perc_missing, x = 1.1), color = "black",
            size = 2.5) +
  geom_point(data = schools, aes(x = ses), color = "red", size = 2) +
  scale_color_manual("",
                     breaks = c("low greenness - low naturalness",
                                "low greenness - high naturalness",
                                "high greenness - low naturalness",
                                "high greenness - high naturalness"),
                     values = c("gray73", "khaki3", "olivedrab3",
                                "darkgreen")) +
  theme_bw() +
  ylab("school") +
  xlab("socio-economic status") +
  scale_x_continuous(breaks = c(0, 0.25, .5, .75, 1)) +
  scale_y_discrete(limits = schools %>% arrange(ses) %>%
                     dplyr::pull(school_id)) +
  theme(legend.position = "bottom") +
  guides(color = guide_legend(nrow = 2, byrow = TRUE))




## ----read_atop, message=FALSE, warning=FALSE, include=FALSE-----------------------------------------------
#####-------------------ANALYSING SCALE QUESTIONS:ATOP---------------------#####
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


## ----atopkids-descriptives, message=FALSE, warning=FALSE, include=TRUE, echo = FALSE----------------------
nb <- length(unique(atop_data$child_id))
tbl <- atop_data %>%
  group_by(question_short, interpretation) %>%
  summarize(n = n()) %>%
  ungroup() %>%
  mutate(summ = sprintf("%.0f (%.0f%%)", n, n/nb*100)) %>%
  left_join(atop_key %>%
              dplyr::select(question_short, question_text_english)) %>%
  dplyr::select(-n, -question_short) %>%
  pivot_wider(names_from = interpretation,
              values_from = summ)  %>%
  mutate_at(2:6, ~replace_na(.,"")) %>%
  dplyr::select(question_text_english, Disagree, `Slightly disagree`,
                `Moderately agree`, Agree, `No response`)
tbl[order(match(tbl$question_text_english,
                atop_key$question_text_english)),] %>%
  kable(booktabs = TRUE,
        caption = "Descriptive statistics for Attitude Towards Outdoor Play
        scale items in children.",
        col.names = c("Item", "Disagree", "Slightly disagree",
                      "Moderately agree", "Agree", "No response")) %>%
  kableExtra::kable_styling(font_size = 9) %>%
  kableExtra::column_spec(1, width = "2in") %>%
  kableExtra::column_spec(2:6, width = "0.6in")


## ----atop-fit-model, message=FALSE, warning=FALSE, include = FALSE----------------------------------------
#inspect the description
get_schema(package = baseball_package,
           resource_name = "wp5_atop_data")$fields %>%
  bind_rows() %>%
  kable(booktabs = TRUE) %>%
  kableExtra::kable_styling()
#reshape the data for cfa
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
  left_join(schools)
model_atop <- "ATOP_benefits =~ 1*think_clearly + good_for_health + calm_down + learn_new_things +   feeling_free + imagination + explore
  ATOP_fears =~ 1*get_lost + unknown_persons + wild_animals + getting_hurt
  calm_down ~~ learn_new_things
  calm_down ~~ feeling_free"
fit_free_atop <- cfa(model_atop,
                     data = likert_data_atop %>%
                       filter(community != "Dutch speaking.French speaking") %>%
                       mutate(across(think_clearly:getting_hurt,
                                     function(x) as.numeric(x))),
                     group = "community")
fit_sameloadings_atop <- cfa(model_atop,
                             data = likert_data_atop %>%
                               filter(community !=
                                        "Dutch speaking.French speaking") %>%
                               mutate(across(think_clearly:getting_hurt,
                                             function(x) as.numeric(x))),
                             group = "community",
                             group.equal = c("loadings"))
fit_sameloadingsinterc_atop <- cfa(model_atop,
                                   data = likert_data_atop %>%
                                     filter(community !=
                                              "Dutch speaking.French speaking") %>%
                                     mutate(across(think_clearly:getting_hurt,
                                                   function(x) as.numeric(x))),
                                   group = "community",
                                   group.equal = c("loadings", "intercepts"))

anov_atop_metric <- anova(fit_free_atop, fit_sameloadings_atop) #metric model cannot be rejected
anov_atop_strong <- anova(fit_free_atop, fit_sameloadingsinterc_atop)
#interpret the fit:
interpret(fit_sameloadings_atop)
#TLI need to be >0.95
#A non-significant p-value indicates that the model fits the data well.
# As the chi-square statistic is proportional to the number of observations
#(n), the test will become significant if the sample size is large, even if
#there is only a small discrepancy between S and the fitted matrix implied
#Σ .

cr_omega_atop <- compRelSEM(fit_sameloadings_atop, tau.eq = F, obs.var = T)
cr_alpha_atop <- compRelSEM(fit_sameloadings_atop, tau.eq = T, obs.var = T)


## ----atop-cfa-fit-----------------------------------------------------------------------------------------
tab <- fitmeasures(fit_free_atop, c("chisq", "df", "pvalue", "cfi", "tli",
                                    "rmsea", "srmr", "AIC", "BIC")) %>%
  as.data.frame() %>%
  cbind(fitmeasures(fit_sameloadings_atop, c("chisq", "df", "pvalue", "cfi",
                                             "tli", "rmsea", "srmr", "AIC",
                                             "BIC")) %>%
          as.data.frame()) %>%
  cbind(fitmeasures(fit_sameloadingsinterc_atop,
                    c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "srmr",
                      "AIC", "BIC")) %>%
          as.data.frame())


colnames(tab) <- c("configural", "metric", "strong")
avoid_scientif_notation <- function(x){as.character(signif(x, 4))}
tab %>%
  dplyr::mutate_if(is.numeric, avoid_scientif_notation) %>%
  kable(booktabs = TRUE,
        col.names = c("configural", "metric", "strong"),
        caption = "Fit measures for the three ATOP models.") %>%
  kableExtra::kable_styling()


## ----read_nc, message=FALSE, warning=FALSE, include=FALSE-------------------------------------------------
#####--------------------ANALYSING SCALE QUESTIONS:NC----------------------#####
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


## ----nc-descriptives, message=FALSE, warning=FALSE, include=TRUE, echo = FALSE----------------------------
nb <- length(unique(nc_data$child_id))
tbl <- nc_data %>%
  group_by(question_short, interpretation) %>%
  summarize(n = n()) %>%
  ungroup() %>%
  mutate(summ = sprintf("%.0f (%.0f%%)", n, n / nb*100)) %>%
  left_join(nc_key %>% dplyr::select(question_short, question_text_english)) %>%
  dplyr::select(-n, -question_short) %>%
  pivot_wider(names_from = interpretation,
              values_from = summ)  %>%
  mutate_at(2:6, ~replace_na(.,"")) %>%
  dplyr::select(question_text_english, Disagree, `Slightly disagree`,
                `Don't know`, `Moderately agree`, Agree, `No response`)
tbl[order(match(tbl$question_text_english, nc_key$question_text_english)),] %>%
  kable(booktabs = TRUE,
        caption = "Descriptive statistics for NC
        scale items in children.",
        col.names = c("Item", "Disagree", "Slightly disagree", "Don't know",
                      "Moderately agree", "Agree", "No response")) %>%
  kableExtra::kable_styling(font_size = 9) %>%
  kableExtra::column_spec(1, width = "2in") %>%
  kableExtra::column_spec(2:6, width = "0.6in")


## ----nc-fit-model, message=FALSE, warning=FALSE, include = FALSE------------------------------------------
#reshape the data for cfa
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
model_nc <- "NC =~ 1*beauty + respect + happy + spending_time + enjoy + part_of
  beauty ~~ respect"
fit_free_nc <- cfa(model_nc,
                   data = likert_data_nc %>%
                     filter(community != "Dutch speaking.French speaking"),
                   group = "community")
fit_sameloadings_nc <- cfa(model_nc,
                           data = likert_data_nc %>%
                             filter(community !=
                                      "Dutch speaking.French speaking"),
                           group = "community",
                           group.equal = c("loadings"))
fit_sameloadingsinterc_nc <- cfa(model_nc,
                                 data = likert_data_nc %>%
                                   filter(community !=
                                            "Dutch speaking.French speaking"),
                                 group = "community",
                                 group.equal = c("loadings", "intercepts"))

anov_nc_metric <- anova(fit_free_nc, fit_sameloadings_nc) #metric model cannot be rejected
anov_nc_strong <- anova(fit_free_nc, fit_sameloadingsinterc_nc)
#interpret the fit:
interpret(fit_sameloadings_nc)
#TLI need to be >0.95
#A non-significant p-value indicates that the model fits the data well.
# As the chi-square statistic is proportional to the number of observations
#(n), the test will become significant if the sample size is large, even if
#there is only a small discrepancy between S and the fitted matrix implied
#Σ .

cr_omega_nc <- compRelSEM(fit_sameloadings_nc, tau.eq = F, obs.var = T)
cr_alpha_nc <- compRelSEM(fit_sameloadings_nc, tau.eq = T, obs.var = T)


## ----nc-cfa-fit-------------------------------------------------------------------------------------------
tab <- fitmeasures(fit_free_nc, c("chisq", "df", "pvalue", "cfi", "tli",
                                  "rmsea", "srmr", "AIC", "BIC")) %>%
  as.data.frame() %>%
  cbind(fitmeasures(fit_sameloadings_nc, c("chisq", "df", "pvalue", "cfi",
                                           "tli", "rmsea", "srmr", "AIC",
                                           "BIC")) %>%
          as.data.frame()) %>%
  cbind(fitmeasures(fit_sameloadingsinterc_nc,
                    c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "srmr",
                      "AIC", "BIC")) %>%
          as.data.frame())


colnames(tab) <- c("configural", "metric", "strong")
avoid_scientif_notation <- function(x){as.character(signif(x, 3))}
tab %>%
  dplyr::mutate_if(is.numeric, avoid_scientif_notation) %>%
  kable(booktabs = TRUE,
        col.names = c("configural", "metric", "strong"),
        caption = "Fit measures for the three NC models.") %>%
  kableExtra::kable_styling()


## ----read_reps, message=FALSE, warning=FALSE, include=FALSE-----------------------------------------------
#####-------------------ANALYSING SCALE QUESTIONS:REPS---------------------#####
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
nb_parents_reps <- length(
  unique(c(
    str_c(reps_data1$child_id, reps_data1$replicate_id),
    str_c(reps_data2$child_id, reps_data2$replicate_id))))
nb_children_reps <- length(
  unique(c(reps_data1$child_id, reps_data2$child_id, reps_data2$replicate_id)))
reps_key <-
  cbind(reps_key, factor = c("protection_from_injury", "engagement_with_risk",
                             "protection_from_injury", "engagement_with_risk",
                             "protection_from_injury", "engagement_with_risk",
                             "protection_from_injury", "engagement_with_risk",
                             "protection_from_injury", "engagement_with_risk",
                             "protection_from_injury", "engagement_with_risk",
                             "rain", "wash_hands",
                             "protection_from_injury","protection_from_injury"))


## ----reps-descriptives, message=FALSE, warning=FALSE, include=TRUE, echo = FALSE--------------------------
nb <- length(unique(reps_data$child_id))
reps_data %>%
  # full_join(expand_grid(
  #   child_id = unique(reps_data$child_id),
  #   question_short = unique(reps_data$question_short))) %>%
  group_by(question_short, response) %>%
  summarize(n = n()) %>%
  ungroup() %>%
  mutate(summ = sprintf("%.0f (%.0f%%)", n, n/nb*100)) %>%
  left_join(reps_key %>% dplyr::select(question_short, question_text_english)) %>%
  dplyr::select(-n, -question_short) %>%
  pivot_wider(names_from = response,
              values_from = summ) %>%
  mutate_at(2:8, ~replace_na(.,"")) %>%
  kable(booktabs = TRUE,
        caption = "Descriptive statistics for REPS in parents.",
        col.names = c("Original statement", as.character(1:7),
                      "Not answered")) %>%
  kableExtra::kable_styling(font_size = 8) %>%
  kableExtra::column_spec(1, width = "1.6in") %>%
  kableExtra::column_spec(2:9, width = "0.45in")


## ----message=FALSE, warning=FALSE, include=TRUE, echo = FALSE, fig.width = 6.5, fig.height = 6.5, eval = TRUE----
#------------------------Prepare REPS data for cfa-----------------------------#
#there are no parents who filled it in more than once
answer_more_than_once <- reps_data %>%
  group_by(child_id, replicate_id, question_short) %>%
  summarise(n = dplyr::n(), .groups = "drop") %>%
  dplyr::filter(n > 1L)
#reshape the data into the proper format
data_cfa_reps <- reps_data %>%
  # full_join(expand_grid(#add a row for the missing "wash hands" data
  #   child_id = unique(reps_data$child_id),
  #   question_short = unique(reps_data$question_short))) %>%
  #   dplyr::select(child_id, replicate_id, question_short, response) %>%
  group_by(child_id, replicate_id, question_short, response) %>%
  summarize(response = first(response)) %>%
  pivot_wider(id_cols = c(child_id, replicate_id), names_from = question_short,
              values_from = response) %>%
  dplyr::select(child_id, replicate_id, reps_key$question_short)
data_cfa_reps_unscaled <- data_cfa_reps
data_cfa_reps[, -c(1, 2)] <- scale(data_cfa_reps[, -c(1, 2)],
                                   center = TRUE, scale = FALSE)


## ----message=FALSE, warning=FALSE, include=FALSE, echo = FALSE, eval = TRUE-------------------------------
#-----------1. EFA Analysing only complete data (no missing answers)-----------#
df <- data_cfa_reps[complete.cases(data_cfa_reps[, -c(1, 2)]), -c(1, 2)]
pca <- prcomp(df)
scree1 <- screeplot(pca, type = "lines")#2 factors is best
covmat <- cov(data_cfa_reps[complete.cases(data_cfa_reps), -c(1, 2)])
cormat <- cor(data_cfa_reps[complete.cases(data_cfa_reps), -c(1, 2)])
efa_complete <- fa(cormat,
                   covar = FALSE,
                   2, rotate = "oblimin",
                   fm = "ml", n.obs = nrow(df))
tables_efa_complete <- fa_table(efa_complete,
                                title = "Factor analysis results with oblique rotation
                         and two factors.",
                                varlabels = str_wrap(reps_key$question_text_english,
                                                     15))
tables_efa_complete$ind_table
#the two additional statements do not fit in either of the factors.
#the EFA further uncovers the original two factors although the statement "I encourage my child to do physical activity" is now part of the engagement with risk factor in stead of the protection factor.

#--------------------------------2. EFA with MI--------------------------------#
library(mifa)
mi <- mifa(data = data_cfa_reps[,-c(1, 2)])
fit <- fa(mi$cov_combined, n.obs = nrow(data_cfa_reps), nfactors = 2)
fa.diagram(fit)
tables_atop <- fa_table(fit,
                        title = "Factor analysis results with oblique rotation
                         and two factors.",
                        varlabels = str_wrap(reps_key$question_text_english,
                                             15))
tables_atop$ind_table
#Again, the washing hands and playing in the rain statements don't fit with
# either factor. "I encourage my child to do physical activity (with the least
# risk of injury)." also does not fit well with either.


## ----reps-fit-model, message=FALSE, warning=FALSE, include = FALSE----------------------------------------
reps_data1 <- read_resource(package = baseball_package,
                            resource_name =
                              "wp5_parents_risk_protection_data_part1") %>%
  mutate(response = factor(as.factor(response),
                           levels = c("Strongly disagree", "Disagree",
                                      "Slightly disagree",
                                      "Neigher agree nor disagree",
                                      "Slightly agree", "Agree",
                                      "Strongly agree")))

reps_data2 <- read_resource(
  package = baseball_package,
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
reps_key <-
  cbind(reps_key, factor = c("protection_from_injury", "engagement_with_risk",
                             "protection_from_injury", "engagement_with_risk",
                             "protection_from_injury", "engagement_with_risk",
                             "protection_from_injury", "engagement_with_risk",
                             "protection_from_injury", "engagement_with_risk",
                             "protection_from_injury", "engagement_with_risk",
                             "rain", "wash_hands",
                             "protection_from_injury","protection_from_injury"))

model_reps <- "protection_from_injury =~ 1*prevention_importance + concerned_injury + concerned_hazards + avoid_risk + importance_supervision + chance_of_injury + limit_dangerous_activities + encourage_safe_activities
          risk_engagement =~ 1*promote_physical_challenges + physical_limits + explore_new_environments + benefits_outweigh_risk + importance_managing_risk + risk_self_confidence"
# prevention_importance ~~  chance_of_injury
# concerned_injury ~~ concerned_hazards
# concerned_hazards ~~ avoid_risk
# prevention_importance ~~ avoid_risk
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
#TLI need to be >0.95
#A non-significant p-value indicates that the model fits the data well.
# As the chi-square statistic is proportional to the number of observations
#(n), the test will become significant if the sample size is large, even if
#there is only a small discrepancy between S and the fitted matrix implied
#Σ .


cr_omega_reps <- compRelSEM(fit_sameloadings_reps, tau.eq = F, obs.var = T)
cr_alpha_reps <- compRelSEM(fit_sameloadings_reps, tau.eq = T, obs.var = T)


## ----reps-cfa-fit-----------------------------------------------------------------------------------------
tab <- fitmeasures(fit_free_reps, c("chisq", "df", "pvalue", "cfi", "tli",
                                    "rmsea", "srmr", "AIC", "BIC")) %>%
  as.data.frame() %>%
  cbind(fitmeasures(fit_sameloadings_reps, c("chisq", "df", "pvalue", "cfi",
                                             "tli", "rmsea", "srmr", "AIC",
                                             "BIC")) %>%
          as.data.frame()) %>%
  cbind(fitmeasures(fit_sameloadingsinterc_reps,
                    c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "srmr",
                      "AIC", "BIC")) %>%
          as.data.frame())


colnames(tab) <- c("configural", "metric", "strong")
avoid_scientif_notation <- function(x){as.character(signif(x, 4))}
tab %>%
  dplyr::mutate_if(is.numeric, avoid_scientif_notation) %>%
  kable(booktabs = TRUE,
        col.names = c("configural", "metric", "strong"),
        caption = "Fit measures for the three REPS models.") %>%
  kableExtra::kable_styling()


## ----calc-factor-scores-----------------------------------------------------------------------------------
#####-------------------------VALIDITY ANALYSIS----------------------------#####
full_atop_scores <- likert_data_atop %>%
  filter(community != "Dutch speaking.French speaking") %>%
  dplyr::select(child_id, community, think_clearly:getting_hurt) %>%
  na.omit() %>%
  arrange(community)#first all dutch, then french
scores_atop <- lavPredict(fit_sameloadings_atop,
                          newdata = full_atop_scores)
full_atop_scores <- full_atop_scores %>%
  mutate(ATOP_benefits = c(scores_atop$`Dutch speaking`[, 1],
                           scores_atop$`French speaking`[, 1]),
         ATOP_fears = c(scores_atop$`Dutch speaking`[, 2],
                        scores_atop$`French speaking`[, 2]),
  )

full_nc_scores <- likert_data_nc %>%
  filter(community != "Dutch speaking.French speaking") %>%
  dplyr::select(child_id, community, beauty:spending_time) %>%
  na.omit() %>%
  arrange(community)#first all dutch, then french
scores_nc <- lavPredict(fit_sameloadings_nc,
                        newdata = full_nc_scores)
full_nc_scores <- full_nc_scores %>%
  mutate(NC = c(scores_nc$`Dutch speaking`[, 1],
                scores_nc$`French speaking`[, 1])
  )


full_reps_scores <- likert_data_reps %>%
  filter(community != "Dutch speaking.French speaking") %>%
  dplyr::select(child_id, community, avoid_risk:risk_self_confidence) %>%
  dplyr::select(-play_in_rain) %>%
  na.omit() %>%
  arrange(community)#first all dutch, then french
scores_reps <- lavPredict(fit_free_reps,
                          newdata = full_reps_scores)
full_reps_scores <- full_reps_scores %>%
  mutate(REPS_protection_from_injury = c(scores_reps$`Dutch speaking`[, 1],
                                         scores_reps$`French speaking`[, 1]),
         REPS_risk_engagement = c(scores_reps$`Dutch speaking`[, 2],
                                  scores_reps$`French speaking`[, 2]),
  )




## ----read_atop_p, message=FALSE, warning=FALSE, include=FALSE---------------------------------------------
#####-------------------VALIDITY ANALYSIS:OUTDOOR PLAY---------------------#####
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


## ----atop-p-descriptives, message=FALSE, warning=FALSE, include=TRUE, echo = FALSE------------------------
nb <- length(unique(c(atop_data_p1$child_id, atop_data_p2$child_id,
                      atop_data_p2$child_id, atop_data_p4$child_id)))
tbl1 <- atop_data_p1 %>%
  group_by(question_short) %>%
  summarize(unit = str_remove(unique(unit), "number of "),
            s = sprintf("mean %.2f, (sd %.2f) %s", mean(response), sd(response), unit),
            n = n_distinct(child_id)) %>%
  ungroup() %>%
  dplyr::select(-unit, -n)
tbl2 <- atop_data_p2 %>%
  mutate(response = ifelse(response == "association", "yes",
                           ifelse(response == "no association",
                                  "no",
                                  "maybe"))) %>%
  group_by(question_short, response) %>%
  summarize(n = n_distinct(child_id)) %>%
  mutate(s = sprintf("%s: %d", response, n)) %>%
  arrange(question_short, desc(s)) %>%
  summarize(s = str_c(s, collapse = ", ")) %>%
  dplyr::filter(question_short != "other_outdoor_play")
tbl3 <- atop_data_p3 %>%
  dplyr::filter(str_length(response) < 5) %>% #only keep cases with 1 response or where both responses agree
  mutate(response = factor(as.factor(response),
                           levels = c("0", "< 1", "1-2", "2-3", "3-4", "> 4"))
  ) %>%
  group_by(question_short, unit, response) %>%
  summarize(n = n_distinct(child_id)) %>%
  mutate(s = sprintf("%s: %d", response, n)) %>%
  arrange(question_short, response) %>%
  summarize(s = str_c(s, collapse = ", ")) %>%
  mutate(s = str_c(unit, ":", s)) %>%
  dplyr::select(-unit) %>%
  ungroup()
tbl4 <- atop_data_p4 %>%
  dplyr::filter(!str_detect(response, "-")) %>% #only keep cases with 1 response or where both responses agree
  group_by(question_short, response) %>%
  summarize(n = n_distinct(child_id)) %>%
  mutate(s = sprintf("%s: %d", response, n)) %>%
  arrange(question_short, response) %>%
  summarize(s = str_c(s, collapse = ", "))

tbl1 %>%
  rbind(tbl2) %>%
  rbind(tbl3) %>%
  rbind(tbl4) %>%
  left_join(atop_key_p %>%
              dplyr::select(question_short, question_text_english)) %>%
  mutate(question_text_english =
           ifelse(startsWith(question_text_english, "How often"),
                  str_split_i(question_text_english,
                              pattern = "/", i = 2),
                  question_text_english)) %>%
  dplyr::select(question_text_english, s) %>%
  kable(booktabs = TRUE, longtable = TRUE,
        caption = "Descriptive statistics for outdoor play activities
        according to the parents.",
        col.names = c("Question", "Summary")) %>%
  kableExtra::kable_styling(latex_options = c("repeat_header"),
                            font_size = 9) %>%
  kableExtra::column_spec(1, width = "3in") %>%
  kableExtra::column_spec(2, width = "3in")


## ----atop-p-response-difference, message=FALSE, warning=FALSE, include=TRUE, echo = FALSE-----------------
diff_atop <- children %>%
  left_join(full_atop_scores) %>%
  left_join(full_nc_scores) %>%
  mutate(response = ifelse(child_id %in% atop_data_p2$child_id &
                             !(child_id %in% atop_data_p4$child_id),
                           "long",
                           ifelse(child_id %in% atop_data_p4$child_id,
                                  "short", "none")),
         response = factor(as.factor(response),
                           levels = c("long", "short", "none"))) %>%
  group_by(response) %>%
  summarize(ATOP_benefits =
              sprintf("%.3f [%.3f, %.3f]",
                      mean(ATOP_benefits, na.rm = T),
                      mean(ATOP_benefits, na.rm = T) -
                        qt(p = 0.025, df = n() - 1, lower.tail = F) *
                        sd(ATOP_benefits, na.rm = T) / sqrt(n()),
                      mean(ATOP_benefits, na.rm = T) +
                        qt(p = 0.025, df = n() - 1, lower.tail = F) *
                        sd(ATOP_benefits, na.rm = T) / sqrt(n())),
            ATOP_fears =
              sprintf("%.3f [%.3f, %.3f]",
                      mean(ATOP_fears, na.rm = T),
                      mean(ATOP_fears, na.rm = T) -
                        qt(p = 0.025, df = n() - 1, lower.tail = F) *
                        sd(ATOP_fears, na.rm = T) / sqrt(n()),
                      mean(ATOP_fears, na.rm = T) +
                        qt(p = 0.025, df = n() - 1, lower.tail = F) *
                        sd(ATOP_fears, na.rm = T) / sqrt(n())),
            NC =
              sprintf("%.3f [%.3f, %.3f]",
                      mean(NC, na.rm = T),
                      mean(NC, na.rm = T) -
                        qt(p = 0.025, df = n() - 1, lower.tail = F) *
                        sd(NC, na.rm = T) / sqrt(n()),
                      mean(NC, na.rm = T) +
                        qt(p = 0.025, df = n() - 1, lower.tail = F) *
                        sd(NC, na.rm = T) / sqrt(n()))) %>%
  pivot_longer(
    cols = !response,
    names_to = "variable",
    values_to = "nb_perc") %>%
  pivot_wider(names_from = response,
              values_from = `nb_perc`)

children %>%
  mutate(response = ifelse(child_id %in% atop_data_p2$child_id &
                             !(child_id %in% atop_data_p4$child_id),
                           "long",
                           ifelse(child_id %in% atop_data_p4$child_id,
                                  "short", "none")),
         response = factor(as.factor(response),
                           levels = c("long", "short", "none"))) %>%
  group_by(response) %>%
  summarize(Number = n(),
            #age = mean(age_202205),#age is not known if the parents did not fill in the survey
            Dutch = sum(community == "Dutch speaking"),
            French = sum(community == "French speaking"),
            Bilingual = sum(community == "Dutch speaking.French speaking"),
            `West-Vlaanderen` = sum(province_name == "West-Vlaanderen"),
            `Vlaams-Brabant` = sum(province_name == "Vlaams-Brabant"),
            `Oost-Vlaanderen` = sum(province_name == "Oost-Vlaanderen"),
            Antwerpen = sum(province_name == "Antwerpen"),
            Brussels = sum(province_name == "Brussels Capital Region"),
            Hainaut = sum(province_name == "Hainaut"),
            Liege = sum(province_name == "Liège"),
            Namur = sum(province_name == "Namur"),
            Luxembourg = sum(province_name == "Luxembourg")) %>%
  ungroup() %>%
  # mutate(across(Dutch : Luxembourg, ~ sprintf("%d, (%.2f%%)", .x, 100*.x / nb)),
  #        Number = as.character(Number)) %>%
  pivot_longer(
    cols = !response,
    names_to = "variable",
    values_to = "nb_perc") %>%
  pivot_wider(names_from = response,
              values_from = `nb_perc`) %>%
  mutate(rowsum = rowSums(across(where(is.numeric)))) %>%
  mutate(across(long:none, ~ sprintf("%d (%.2f%%)", .x, 100*.x / rowsum))) %>%
  dplyr::select(-rowsum) %>%
  rbind(diff_atop) %>%
  kable(booktabs = TRUE,
        font_size = 9,
        caption = "Demographics for children whose parents filled in the long
        questionnaire, short questionnaire, or no questionnaire.",
        col.names = c("", "Long questionnaire", "Short questionnaire",
                      "No response")) %>%
  kableExtra::kable_styling(font_size = 9) %>%
  kableExtra::group_rows(group_label = "Number of children",
                         start_row = 1, end_row = 1) %>%
  kableExtra::group_rows(group_label = "Community",
                         start_row = 2, end_row = 4) %>%
  kableExtra::group_rows(group_label = "Province",
                         start_row = 5, end_row = 13) %>%
  kableExtra::group_rows(group_label = "ATOP factor scores (mean [.95 CI])",
                         start_row = 14, end_row = 15) %>%
  kableExtra::group_rows(group_label = "NC factor scores (mean [.95 CI])",
                         start_row = 16, end_row = 16)


## ----externalval-getfactorscores, message=FALSE, warning=FALSE, include=FALSE, echo = FALSE---------------
#these all have numeric answers: how many days per week/ how often x5_1 -> w5_4
atop_p_cntn <- atop_data_p1  %>%
  group_by(child_id, question_short) %>%
  summarize(response = first(response)) %>% # keep only one answer per child
  ungroup() %>%
  pivot_wider(id_cols = child_id, names_from = question_short,
              values_from = response)
atop_p_cntn <- atop_p_cntn %>%
  dplyr::filter(complete.cases(atop_p_cntn[,-1]))

# these are association or no association -> binary anwers x5_1 -> x5_19
atop_p_binary <- atop_data_p2  %>%
  mutate(response = ifelse(response == "no association - association",
                           "association", response),
         response = 1 * (response == "association")) %>%
  group_by(child_id, question_short) %>%
  summarize(response = first(response)) %>% # keep only one answer per child
  ungroup() %>%
  pivot_wider(id_cols = child_id, names_from = question_short,
              values_from = response) %>%
  dplyr::select(-other_outdoor_play) #too many NA
atop_p_binary <- atop_p_binary %>%
  dplyr::filter(complete.cases(atop_p_binary[,-1]))
atop_p_binary2 <- atop_p_binary %>%
  left_join(atop_p_cntn %>%
              mutate(indoor_activities = 1*(indoor_activities!=0),
                     other_activities = 1*(other_activities!=0),
                     sports_artificial = 1*(sports_artificial!=0),
                     sports_green = 1*(sports_green!=0)))
atop_p_binary2 <- atop_p_binary2 %>%
  dplyr::filter(complete.cases(atop_p_binary2[,-1]))
# ordinal response but can be converted to continuous x5_20 -> x5_23
atop_p_cntn2 <- atop_data_p3 %>%
  mutate(
    response =
      ifelse(str_detect(response, "0"),
             0,
             ifelse(response == "< 1",
                    0.5,
                    ifelse(str_detect(response, "1-2"),
                           1.5,
                           ifelse(str_detect(response, "2-3"),
                                  2.5,
                                  ifelse(str_detect(response,
                                                    "3-4"),
                                         3.5, 5)))))) %>%
  group_by(child_id, question_short) %>%
  summarize(response = first(response)) %>% # keep only one answer per child
  ungroup() %>%
  pivot_wider(id_cols = child_id, names_from = question_short,
              values_from = response)
atop_p_cntn2 <- atop_p_cntn2 %>%
  dplyr::filter(complete.cases(atop_p_cntn2[,-1]))

#------------------------binary responses--------------------------------------#
df_matrix <- atop_p_binary[,-1] %>%
  as.matrix()
atop_p_binary_cor <- tetrachoric(df_matrix)
fa.parallel(atop_p_binary_cor$rho, n.obs = nrow(atop_p_binary))#decide on the number of factors
vss(atop_p_binary_cor$rho, n.obs = nrow(atop_p_binary)) #-> check minimum average partial criterion
efa_atop_p_binary <- fa(atop_p_binary_cor$rho,
                        covar = FALSE,
                        5, rotate = "oblimin",
                        fm = "ml", n.obs = nrow(atop_p_binary),
                        oblique.scores = TRUE)

#----------------------------continuous response-------------------------------#
t <- atop_p_cntn %>%
  left_join(atop_p_cntn2)
t <- t %>% dplyr::filter(complete.cases(t[,-1]))
df_matrix <- t[-1] %>%
  as.matrix()
atop_p_mixed_cor <- cor(df_matrix)
fa.parallel(atop_p_mixed_cor, n.obs = nrow(t))#decide on the number of factors
vss(atop_p_mixed_cor, n.obs = nrow(t)) #-> check minimum average partial criterion
efa_atop_p_mixed <- fa(atop_p_mixed_cor,
                       covar = FALSE,
                       4, rotate = "oblimin",
                       fm = "ml", n.obs = nrow(t))

#------------------------------get factor scores-------------------------------#
scores_cntn <- factor.scores(x = t[,-1], efa_atop_p_mixed, method = "tenBerge")
t <- t %>%
  cbind(scores_cntn$scores %>%
          as.data.frame() %>%
          rename(f_screentime = ML1,
                 f_indoor_activities = ML2,
                 f_homework = ML3,
                 f_organized_sports = ML4))

scores_binary <- factor.scores(x = atop_p_binary[,-1], efa_atop_p_binary,
                               method = "tenBerge")
atop_p_binary <- atop_p_binary %>%
  cbind(scores_binary$scores %>%
          as.data.frame() %>%
          rename(f_natural_play = ML4,
                 f_semi_natural_play = ML5,
                 f_made_up_games_lingering = ML1,
                 f_hide_and_seek = ML2,
                 f_unorganized_sports = ML3))



## ----externalval, message=FALSE, warning=FALSE, include=TRUE, echo = FALSE--------------------------------
tbl2 <- t %>%
  dplyr::select(child_id, starts_with("f_")) %>%
  left_join(atop_p_binary %>%
              dplyr::select(child_id, starts_with("f_"))) %>%
  left_join(full_atop_scores %>%
              dplyr::select(child_id, starts_with("ATOP"))) %>%
  left_join(full_nc_scores %>%
              dplyr::select(child_id, starts_with("NC"))) %>%
  left_join(full_reps_scores %>%
              dplyr::select(child_id, starts_with("REPS")))
# a function to get the correlations
test_diff_cont <- function(x, y){
  correlation <- cor(tbl2[, y],
                     tbl2[, x],
                     use = "complete.obs",
                     method = "pearson")
  pvalue <- cor.test(unlist(tbl2[, y]),
                     unlist(tbl2[, x]),
                     method = "pearson")
  result <- sprintf("%.3f %s",
                    unname(correlation),
                    gtools::stars.pval(pvalue$p.value))
  return(result)}

data.frame(
  question_short = colnames(tbl2)[2:10],
  ATOP_benefits = mapply(test_diff_cont, 2:10, "ATOP_benefits"),
  ATOP_fears = mapply(test_diff_cont, 2:10, "ATOP_fears"),
  NC = mapply(test_diff_cont, 2:10, "NC"),
  REPS_risk_engagement = mapply(test_diff_cont, 2:10, "REPS_risk_engagement"),
  REPS_protection_from_injury = mapply(test_diff_cont, 2:10,
                                       "REPS_protection_from_injury")) %>%
  mutate(question_short = str_remove(question_short, "f_")) %>%
  kable(booktabs = TRUE, longtable = TRUE,
        caption = "Associations of ATOP, NC, and REPS factor scores with related indoor and outdoor activity latent factors (. p < 0.1, * p <.05, ** p <.01, *** p <.001).",
        col.names = c("", "ATOP benefits", "ATOP fears", "NC",
                      "REPS risk engagement", "REPS protection from injury")) %>%
  kableExtra::kable_styling(latex_options = c("repeat_header"),
                            font_size = 9) %>%
  kableExtra::column_spec(1, width = "1.4in") %>%
  kableExtra::column_spec(2:6, width = "0.7in")


## ----read_im, message=FALSE, warning=FALSE, include=FALSE-------------------------------------------------
#####---------------VALIDITY ANALYSIS:INDEPENDENT MOBILITY-----------------#####
im_data <- read_resource(package = baseball_package,
                         resource_name =
                           "wp5_parents_independent_mobility_data") %>%
  mutate(response = factor(as.factor(response),
                           levels = c("No", "Yes - No", "Yes")))


## ----im-descriptives, message=FALSE, warning=FALSE, include=TRUE, echo = FALSE----------------------------
nb <- length(unique(paste0(im_data$child_id, im_data$replicate_id)))
tbl <- im_data %>%
  group_by(question_short, response) %>%
  summarize(n = n()) %>%
  ungroup() %>%
  mutate(summ = sprintf("%.0f (%.0f%%)", n, n/nb*100)) %>%
  left_join(im_key %>% dplyr::select(question_short, question_text_english)) %>%
  dplyr::select(-n, -question_short) %>%
  pivot_wider(names_from = response,
              values_from = summ)  %>%
  mutate_at(2:4, ~replace_na(.,"")) %>%
  dplyr::select(question_text_english, No, `Yes - No`, Yes)
tbl[order(match(tbl$question_text_english, im_key$question_text_english)),] %>%
  kable(booktabs = TRUE,
        caption = "Descriptive statistics for Independent Mobility.",
        col.names = c("Item", "No", "Yes - No", "Yes")) %>%
  kableExtra::kable_styling(font_size = 9) %>%
  kableExtra::column_spec(1, width = "2in") %>%
  kableExtra::column_spec(2:4, width = "0.6in")


## ----externalval-im-getscores, message=FALSE, warning=FALSE, include=FALSE, echo = FALSE, eval = TRUE-----
im_data2 <- im_data  %>%
  mutate(response = as.character(response),
         response = ifelse(response == "Yes - No",
                           "Yes", response),
         response = 1 * (response == "Yes")) %>%
  group_by(child_id, question_short) %>%
  summarize(response = first(response)) %>% # keep only one answer per child
  ungroup() %>%
  pivot_wider(id_cols = child_id, names_from = question_short,
              values_from = response)
im_data2 <- im_data2 %>%
  dplyr::filter(complete.cases(im_data2[,-1]))
library(psych)
df_matrix <- im_data2[,-1] %>%
  as.matrix()
im_data2_cor <- tetrachoric(df_matrix)
fa.parallel(im_data2_cor$rho, n.obs = nrow(im_data2))#decide on the number of factors
vss(im_data2_cor$rho, n.obs = nrow(im_data2)) #-> check minimum average partial criterion
efa_im_data2 <- fa(im_data2_cor$rho,
                   covar = FALSE,
                   4, rotate = "oblimin",
                   fm = "ml", n.obs = nrow(im_data2),
                   oblique.scores = TRUE)

scores_im <- factor.scores(x = im_data2[,-1], efa_im_data2,
                           method = "tenBerge")
im_data2 <- im_data2 %>%
  cbind(scores_im$scores %>%
          as.data.frame() %>%
          rename(f_explore = ML2,
                 f_engage_with_traffic = ML4,
                 f_after_dark = ML1,
                 f_public_transport = ML3))


## ----externalval-im, message=FALSE, warning=FALSE, include=TRUE, echo = FALSE, eval = TRUE----------------
tbl2 <- im_data2 %>%
  dplyr::select(child_id, starts_with("f_")) %>%
  left_join(full_atop_scores %>%
              dplyr::select(child_id, starts_with("ATOP"))) %>%
  left_join(full_nc_scores %>%
              dplyr::select(child_id, starts_with("NC"))) %>%
  left_join(full_reps_scores %>%
              dplyr::select(child_id, starts_with("REPS")))

data.frame(
  question_short = colnames(tbl2)[2:5],
  ATOP_benefits = mapply(test_diff_cont, 2:5, "ATOP_benefits"),
  ATOP_fears = mapply(test_diff_cont, 2:5, "ATOP_fears"),
  NC = mapply(test_diff_cont, 2:5, "NC"),
  REPS_risk_engagement = mapply(test_diff_cont, 2:5,
                                "REPS_risk_engagement"),
  REPS_protection_from_injury = mapply(test_diff_cont, 2:5,
                                       "REPS_protection_from_injury")) %>%
  mutate(question_short = str_remove(question_short, "f_")) %>%
  kable(booktabs = TRUE, longtable = TRUE,
        caption = "Associations of ATOP, NC, and REPS factor scores with
        independent mobility latent factors (. p < 0.1, * p <.05, ** p <.01,
        *** p <.001).",
        col.names = c("", "ATOP benefits", "ATOP fears", "NC",
                      "REPS risk engagement",
                      "REPS protection from injury")) %>%
  kableExtra::kable_styling(latex_options = c("repeat_header"),
                            font_size = 9) %>%
  kableExtra::column_spec(1, width = "1.4in") %>%
  kableExtra::column_spec(2:6, width = "0.7in")


## ----fit-sem, warning = FALSE, message = FALSE, include = FALSE-------------------------------------------
#####---------------------STRUCTURAL EQUATION MODEL------------------------#####
landscape_data <- read_resource(
  package = baseball_package,
  resource_name = "wp1_landscape_level_data")
sem_data <- likert_data_atop %>%
  mutate(across(think_clearly:getting_hurt,
                function(x) as.numeric(x))) %>%
  left_join(likert_data_nc %>%
              dplyr::select(child_id:spending_time)) %>%
  left_join(child_derived %>%
              dplyr::select(child_id, ses) %>%
              rename(ses_child = ses)) %>%
  dplyr::select(-class_id, -birth_year, -birth_month, -age_202205,
                -province_name, -pm25, -sirm_mean, -starts_with("playground"),
                -starts_with("extra"), -in_school_prop_green,
                -out_school_prop_green, -natural_elements_score,
                -artificial_elements_score, -tree_number, -allergenic_potential,
                -case_control_id) %>%#remove redundant columns
  mutate(sex_male = 1 * (biological_sex == "Male"),
         landscape_type_high_naturalness =
           1 * (landscape_type == "high naturalness"),
         school_context_high_greenness =
           1 * (school_context == "high greenness"),
         interaction =
           as.factor(ifelse(landscape_type == "high naturalness",
                            ifelse(school_context == "high greenness",
                                   "h-h", "h-l"),
                            ifelse(school_context == "high greenness",
                                   "l-h", "l-l")
           )),
         interaction_h_h = 1 * (interaction == "h-h"),
         interaction_h_l = 1 * (interaction == "h-l"),
         interaction_l_h = 1 * (interaction == "l-h"),
         interaction_l_l = 1 * (interaction == "l-l"),
         ses_child = ifelse(is.na(ses_child), ses, ses_child),
         French = str_detect(community, "French")) %>%
  left_join(
    landscape_data %>%
      select(school_id, c(landscape_type, bc, no2, pm10_min_pm25, pm25))
  )
#define lavaan models
model_sem <- "
  #latent variable definitions
  ATOP_benefits =~ 1*think_clearly + good_for_health + calm_down + learn_new_things +   feeling_free + imagination + explore
  ATOP_fears =~ 1*get_lost + unknown_persons + wild_animals + getting_hurt
  calm_down ~~ learn_new_things
  calm_down ~~ feeling_free
  NC =~ 1*beauty + respect + happy + spending_time + enjoy + part_of
  beauty ~~ respect

  #Add correlations of interest
  ATOP_benefits ~~ NC
  ATOP_fears ~~ NC
  ATOP_benefits ~~ ATOP_fears

  #regressions
  ATOP_benefits ~ sex_male + landscape_type_high_naturalness + school_context_high_greenness + ses_child
  ATOP_fears ~  sex_male + landscape_type_high_naturalness + school_context_high_greenness + ses_child
  NC ~ sex_male + landscape_type_high_naturalness + school_context_high_greenness + ses_child
"
#evaluate model
sem_output <- sem(model = model_sem, data = sem_data)
sem_sum <- summary(sem_output, fit.measures = FALSE, standardized = TRUE)
sem_sum_std <- standardizedSolution(sem_output) %>%
  rename(est = `est.std`)
sem_fit <- fitmeasures(sem_output, c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "srmr",
                                     "AIC", "BIC"))
interpret(sem_output)
cr_omega_sem <- compRelSEM(sem_output, tau.eq = F, obs.var = T)
cr_alpha_sem <- compRelSEM(sem_output, tau.eq = T, obs.var = T)


## ----sem-fit----------------------------------------------------------------------------------------------
avoid_scientif_notation <- function(x){as.character(signif(x, 3))}
sem_fit %>%
  as.data.frame() %>%
  dplyr::mutate_if(is.numeric, avoid_scientif_notation) %>%
  kable(booktabs = TRUE,
        col.names = c(""),
        caption = "Fit measures for the SEM.") %>%
  kableExtra::kable_styling()


## ----sem-viz, fig.cap= "A graphical presentation of the SEM model, including standardized parameter estimates", warning = FALSE, message = FALSE, fig.width = 8----
nodelabels <- c(think_clearly = "th_cl", good_for_health = "gd_hlth",
                calm_down = "calm", learn_new_things = "lrn",
                feeling_free = "free", imagination = "img", explore = "exp",
                get_lost = "gt_l", unknown_persons = "unknwn",
                wild_animals = "wld_an", getting_hurt = "gt_hrt",
                beauty = "bty",
                respect = "rspc", happy = "hpp", spending_time = "sp_time",
                enjoy = "enj", part_of = "partof", sex_male = "male",
                landscape_type_high_naturalness = "hgh_natur",
                school_context_high_greenness = "hgh_green", ses_child = "SES",
                ATOP_benefits = "ATOP_b", ATOP_fears = "ATOP_f", NC = "NC")
semPaths(sem_output, "Standardized", "Standardized",
         style = "lisrel", layout = "tree",
         residuals = FALSE, groups = "latents", pastel = 1, rotation = 2,
         fade = FALSE, optimizeLatRes = TRUE,
         fade = TRUE, edge.label.bg = TRUE, #edgeLabels = edgelabels
         nodeLabels = nodelabels)
#p$graphAttributes$Nodes$labels


## ----sem-regression, warning = FALSE, message = FALSE-----------------------------------------------------
parameterEstimates(sem_output) %>%
  dplyr::filter(op == "~") %>%
  mutate(estimate = sprintf("%.3f (%.3f)%s",
                            est, se, gtools::stars.pval(pvalue))) %>%
  dplyr::select(lhs, rhs, estimate) %>%
  pivot_wider(names_from = lhs,
              values_from = estimate) %>%
  kable(booktabs = TRUE,
        caption = "Regression results from the SEM.",
        col.names = c("covariate", "ATOP_benefits", "ATOP_fears", "NC")) %>%
  kableExtra::kable_styling() %>%
  add_header_above(c("", "Dependent variable" = 3)) %>%
  kableExtra::add_footnote(label = "* p <.05, ** p <.01, *** p <.001")


## ----eval = FALSE-----------------------------------------------------------------------------------------
## #####---------------------SEM: TEST GROUPED ANALYSIS-----------------------#####
## sem_data <- sem_data %>%
##   filter(community != "Dutch speaking.French speaking")
## #---------------------evaluate a model grouped by language---------------------#
## sem_output_grouped <- sem(model = model_sem, data = sem_data, group = "community")
## summary(sem_output_grouped, standardized = TRUE)
## fitmeasures(sem_output_grouped, c("chisq", "df", "pvalue", "cfi", "tli",
##                                   "rmsea", "srmr", "AIC", "BIC"))
## interpret(sem_output_grouped)#similar fit
## compRelSEM(sem_output_grouped) # okay reliability of the latent factors
##
## ### -----------------------metric measurement invariance----------------------##
## sem_output_grouped_metric <- sem(model = model_sem, data = sem_data,
##                           group = "community",group.equal = c("loadings")
##                           )
## summary(sem_output_grouped_metric, standardized = TRUE)
## fitmeasures(sem_output_grouped_metric, c("chisq", "df", "pvalue", "cfi", "tli",
##                                          "rmsea", "srmr", "AIC", "BIC"))
## interpret(sem_output_grouped_metric)#similar fit
## compRelSEM(sem_output_grouped_metric) # okay reliability of the latent factors
##
##
## ### --------------------strong measurement invariance-------------------------##
## sem_output_grouped_strong <- sem(model = model_sem, data = sem_data,
##                           group = "community",
##                           group.equal = c("loadings", "intercepts")
##                           )
## summary(sem_output_grouped_strong, standardized = TRUE)
## fitmeasures(sem_output_grouped_strong, c("chisq", "df", "pvalue", "cfi", "tli",
##                                          "rmsea", "srmr", "AIC", "BIC"))
## interpret(sem_output_grouped_strong)#similar fit
## compRelSEM(sem_output_grouped_strong)
##
## anov_atop_metric <- anova(sem_output_grouped, sem_output_grouped_metric)
## anov_atop_strong <- anova(sem_output_grouped, sem_output_grouped_strong)
##

## ----atop-translation-kids, message=FALSE, warning=FALSE, include=TRUE,  echo = FALSE---------------------
#####----------------APPENDIX: visualized the translations-----------------#####
atop_key_groups <- atop_key %>%
  arrange(question_group) %>%
  mutate(row_number = row_number()) %>%
  group_by(question_group) %>%
  summarize(first = min(row_number),
            last = max(row_number)) %>% as.data.frame()
tbl <- atop_key %>%
  dplyr::select(question_text_english, question_text_french,
                question_text_dutch) %>%
  kable("latex", longtable = TRUE, booktabs = TRUE,
        caption = "The ATOP scale questions for the children's survey.",
        col.names = c("Original", "French", "Dutch")) %>%
  kable_styling(latex_options = c("repeat_header"), font_size = 9) %>%
  column_spec(1:3, width = "3in") %>%
  landscape()
for (row in seq(1, nrow(atop_key_groups))) {
  tbl <- tbl %>%
    kableExtra::group_rows(atop_key_groups[row,1], start_row = atop_key_groups[row,2],
                           end_row = atop_key_groups[row,3], underline = TRUE)
}
tbl


## ----nc-translation, message=FALSE, warning=FALSE, include=TRUE,  echo = FALSE----------------------------
tbl <- nc_key %>%
  dplyr::select(question_text_english, question_text_french,
                question_text_dutch) %>%
  kable("latex", longtable = TRUE, booktabs = TRUE,
        caption = "The NC scale questions for the children's survey.",
        col.names = c("Original", "French", "Dutch")) %>%
  kable_styling(latex_options = c("repeat_header"), font_size = 9) %>%
  column_spec(1:3, width = "3in") %>%
  landscape()
tbl


## ----reps-translation, message=FALSE, warning=FALSE, include=TRUE,  echo = FALSE--------------------------
tbl <- reps_key %>%
  mutate(question_text_english = ifelse(
    ommit_if_short_survey,
    paste0(question_text_english, "*"),
    question_text_english
  )) %>%
  dplyr::select(question_text_english, question_text_french,
                question_text_dutch) %>%
  kable("latex", longtable = TRUE, booktabs = TRUE,
        caption = "The REPS scale questions for parents and guardians. Questions with a * are omitted in the shortened survey.",
        col.names = c("Original", "French", "Dutch")) %>%
  kable_styling(latex_options = c("repeat_header"), font_size = 9) %>%
  column_spec(1:3, width = "3in") %>%
  landscape()
tbl


## ----atop-translation, message=FALSE, warning=FALSE, include=TRUE,  echo = FALSE--------------------------
tbl <- atop_key_p %>%
  mutate(question_text_english = ifelse(
    ommit_if_short_survey,
    paste0(question_text_english, "*"),
    question_text_english
  )) %>%
  dplyr::select(question_text_english, question_text_french,
                question_text_dutch) %>%
  kable("latex", longtable = TRUE, booktabs = TRUE,
        caption = "The outdoor play questions for the parents and guardians. Questions with a * are omitted in the shortened survey.",
        col.names = c("Original", "French", "Dutch")) %>%
  kable_styling(latex_options = c("repeat_header"), font_size = 9) %>%
  column_spec(1:3, width = "3in") %>%
  landscape()
tbl


## ----atopkids-descriptives2, message=FALSE, warning=FALSE, include=TRUE, echo = FALSE, fig.cap= "Descriptive statistics for Dutch-speaking (left) and French-speaking (right) children for the ATOP scale items. Non-responses are not shown but taken into account when calculating the percentages.", fig.width = 6.5, fig.height = 6.5----
#####----------------------------APPENDIX:ATOP-----------------------------#####
likert_data_atop <- atop_data %>%
  as.data.frame() %>%
  left_join(children) %>%
  dplyr::filter(str_detect(community, "Dutch speaking")) %>%
  mutate(interpretation = factor(as.factor(interpretation),
                                 levels = c("Disagree", "Slightly disagree",
                                            "No response",
                                            "Moderately agree", "Agree"))) %>%
  dplyr::select(question_short, interpretation) %>%
  pivot_wider(names_from = question_short, values_from = interpretation,
              values_fn = list)
likert_data_atop2 <- as.data.frame(do.call(cbind,
                                           sapply(1:ncol(likert_data_atop),
                                                  FUN = function(x) {
                                                    likert_data_atop[[x]]
                                                  })))
colnames(likert_data_atop2) <- names(likert_data_atop)
likert_data_atop <- likert_data_atop2 %>%
  mutate(across(think_clearly:getting_hurt, function(x) factor(as.factor(x),
                                                               levels = 1:5,
                                                               labels = c("Disagree",
                                                                          "Slightly disagree",
                                                                          "No response",
                                                                          "Moderately agree",
                                                                          "Agree"))))
names(likert_data_atop) <- atop_key$question_text_english
p_nl <- gglikert(likert_data_atop, sort = "none", y_label_wrap = 20,
                 labels_size = 2)


likert_data_atop <- atop_data %>%
  as.data.frame() %>%
  left_join(children) %>%
  dplyr::filter(str_detect(community, "French speaking")) %>%
  mutate(interpretation = factor(as.factor(interpretation),
                                 levels = c("Disagree", "Slightly disagree",
                                            "No response",
                                            "Moderately agree", "Agree"))) %>%
  dplyr::select(question_short, interpretation) %>%
  pivot_wider(names_from = question_short, values_from = interpretation,
              values_fn = list)
likert_data_atop2 <- as.data.frame(do.call(cbind,
                                           sapply(1:ncol(likert_data_atop),
                                                  FUN = function(x) {
                                                    likert_data_atop[[x]]
                                                  })))
colnames(likert_data_atop2) <- names(likert_data_atop)
likert_data_atop <- likert_data_atop2 %>%
  mutate(across(think_clearly:getting_hurt, function(x) factor(as.factor(x),
                                                               levels = 1:5,
                                                               labels = c("Disagree",
                                                                          "Slightly disagree",
                                                                          "No response",
                                                                          "Moderately agree",
                                                                          "Agree"))))
names(likert_data_atop) <- atop_key$question_text_english

p_fr <- gglikert(likert_data_atop, sort = "none", y_label_wrap = 20,
                 labels_size = 2) +
  theme(axis.text.y = element_blank())
p_nl + p_fr + plot_layout(guides = "collect") &
  theme(legend.position = "bottom",
        text = element_text(size = 9))


## ----atop-loadings----------------------------------------------------------------------------------------
est <- parameterEstimates(fit_sameloadings_atop) %>%
  mutate(type = ifelse(op == "=~",
                       "loading",
                       ifelse(op == "~1",
                              "intercept",
                              ifelse(lhs == rhs,
                                     "variance",
                                     "covariance"))),
         estimate = sprintf("%.3f %s", est, gtools::stars.pval(pvalue))) %>%
  mutate(link = ifelse(type %in% c("loading", "covariance"),
                       sprintf("%s %s %s", lhs, op, rhs),
                       lhs)) %>%
  dplyr::select(type, link, estimate, se)
t <- cbind(est[1:(nrow(est)/2),], est[(nrow(est)/2+1):nrow(est),-c(1,2)])
t[, -1] %>%
  kable(booktabs = TRUE,
        caption = "Unstandardized coefficient estimates and standard errors for the metric measurement invariance model for ATOP to compare Dutch-speaking versus French-speaking respondents",
        col.names = c("", "Estimate", "SE", "Estimate", "SE"),
        digits = 3) %>%
  kableExtra::kable_styling(font_size = 9) %>%
  kableExtra::add_header_above(c("", "Dutch speaking" = 2,
                                 "French speaking" = 2)) %>%
  kableExtra::group_rows(start_row = which(t$type == "loading")[1],
                         end_row = max(which(t$type == "loading")),
                         "Loadings") %>%
  kableExtra::group_rows(start_row = which(t$type == "intercept")[1],
                         end_row = max(which(t$type == "intercept")),
                         "Intercept") %>%
  kableExtra::group_rows(start_row = 14,
                         end_row = 26,
                         "Variance") %>%
  kableExtra::group_rows(start_row = 12,
                         end_row = 13,
                         "Covariance") %>%
  kableExtra::group_rows(start_row = 27,
                         end_row = 27,
                         "Covariance") %>%
  kableExtra::add_footnote(
    label = ". p < 0.1, * p <.05, ** p <.01, *** p <.001")
#which(t$type == "covariance")
#which(t$type == "variance")


## ----atop-loadings2---------------------------------------------------------------------------------------
est <- standardizedSolution(fit_sameloadings_atop) %>%
  mutate(type = ifelse(op == "=~",
                       "loading",
                       ifelse(op == "~1",
                              "intercept",
                              ifelse(lhs == rhs,
                                     "variance",
                                     "covariance"))),
         estimate = sprintf("%.3f %s", `est.std`, gtools::stars.pval(pvalue))) %>%
  mutate(link = ifelse(type %in% c("loading", "covariance"),
                       sprintf("%s %s %s", lhs, op, rhs),
                       lhs)) %>%
  dplyr::select(type, link, estimate, se)
t <- cbind(est[1:(nrow(est)/2),], est[(nrow(est)/2+1):nrow(est),-c(1,2)])
t[, -1] %>%
  kable(booktabs = TRUE,
        caption = "Standardized coefficient estimates and standard errors for the metric measurement invariance model for ATOP to compare Dutch-speaking versus French-speaking respondents",
        col.names = c("", "Estimate", "SE", "Estimate", "SE"),
        digits = 3) %>%
  kableExtra::kable_styling(font_size = 9) %>%
  kableExtra::add_header_above(c("", "Dutch speaking" = 2,
                                 "French speaking" = 2)) %>%
  kableExtra::group_rows(start_row = which(t$type == "loading")[1],
                         end_row = max(which(t$type == "loading")),
                         "Loadings") %>%
  kableExtra::group_rows(start_row = which(t$type == "intercept")[1],
                         end_row = max(which(t$type == "intercept")),
                         "Intercept") %>%
  kableExtra::group_rows(start_row = 14,
                         end_row = 26,
                         "Variance") %>%
  kableExtra::group_rows(start_row = 12,
                         end_row = 13,
                         "Covariance") %>%
  kableExtra::group_rows(start_row = 27,
                         end_row = 27,
                         "Covariance") %>%
  kableExtra::add_footnote(
    label = ". p < 0.1, * p <.05, ** p <.01, *** p <.001")
#which(t$type == "covariance")
#which(t$type == "variance")


## ----nc-descriptives2, message=FALSE, warning=FALSE, include=TRUE, echo = FALSE, fig.cap = "Descriptive statistics for Dutch-speaking (left) and French-speaking (right) children for the NC scale items. Non-responses are not shown but taken into account when calculating the percentages.", fig.width = 6.5, fig.height = 5----
#####--------------------APPENDIX: NATURE CONNECTEDNESS--------------------#####
likert_data_nc <- nc_data %>%
  as.data.frame() %>%
  left_join(children) %>%
  dplyr::filter(str_detect(community, "Dutch speaking")) %>%
  mutate(interpretation = factor(as.factor(interpretation),
                                 levels = c("Disagree", "Slightly disagree",
                                            "No response", "Don't know",
                                            "Moderately agree", "Agree"))) %>%
  dplyr::select(question_short, interpretation) %>%
  pivot_wider(names_from = question_short, values_from = interpretation,
              values_fn = list)
likert_data_nc2 <- as.data.frame(do.call(cbind,
                                         sapply(1:ncol(likert_data_nc),
                                                FUN = function(x) {
                                                  likert_data_nc[[x]]
                                                })))
colnames(likert_data_nc2) <- names(likert_data_nc)
likert_data_nc <- likert_data_nc2 %>%
  mutate(across(beauty:part_of, function(x) factor(as.factor(x),
                                                   levels = 1:6,
                                                   labels = c("Disagree",
                                                              "Slightly disagree",
                                                              "No response", "Don't know",
                                                              "Moderately agree", "Agree"))))
names(likert_data_nc) <- nc_key$question_text_english
p_nl <- gglikert(likert_data_nc,
                 exclude_fill_values = "No response",
                 sort = "none", y_label_wrap = 20,
                 labels_size = 2)


likert_data_nc <- nc_data %>%
  as.data.frame() %>%
  left_join(children) %>%
  dplyr::filter(str_detect(community, "French speaking")) %>%
  mutate(interpretation = factor(as.factor(interpretation),
                                 levels = c("Disagree", "Slightly disagree",
                                            "No response", "Don't know",
                                            "Moderately agree", "Agree"))) %>%
  dplyr::select(question_short, interpretation) %>%
  pivot_wider(names_from = question_short, values_from = interpretation,
              values_fn = list)
likert_data_nc2 <- as.data.frame(do.call(cbind,
                                         sapply(1:ncol(likert_data_nc),
                                                FUN = function(x) {
                                                  likert_data_nc[[x]]
                                                })))
colnames(likert_data_nc2) <- names(likert_data_nc)
likert_data_nc <- likert_data_nc2 %>%
  mutate(across(beauty:part_of, function(x) factor(as.factor(x),
                                                   levels = 1:6,
                                                   labels = c("Disagree",
                                                              "Slightly disagree",
                                                              "No response", "Don't know",
                                                              "Moderately agree", "Agree"))))
names(likert_data_nc) <- nc_key$question_text_english

p_fr <- gglikert(likert_data_nc, sort = "none",
                 exclude_fill_values = "No response", y_label_wrap = 20,
                 labels_size = 2) +
  theme(axis.text.y = element_blank())
p_nl + p_fr + plot_layout(guides = "collect") &
  theme(legend.position = "bottom",
        text = element_text(size = 9))


## ----nc-loadings------------------------------------------------------------------------------------------
est <- parameterEstimates(fit_sameloadings_nc) %>%
  mutate(type = ifelse(op == "=~",
                       "loading",
                       ifelse(op == "~1",
                              "intercept",
                              ifelse(lhs == rhs,
                                     "variance",
                                     "covariance"))),
         estimate = sprintf("%.3f %s", est, gtools::stars.pval(pvalue))) %>%
  mutate(link = ifelse(type %in% c("loading", "covariance"),
                       sprintf("%s %s %s", lhs, op, rhs),
                       lhs)) %>%
  dplyr::select(type, link, estimate, se)
t <- cbind(est[1:(nrow(est)/2),], est[(nrow(est)/2+1):nrow(est),-c(1,2)])
t[, -1] %>%
  kable(booktabs = TRUE,
        caption = "Unstandardized coefficient estimates and standard errors for the metric measurement invariance model for NC to compare Dutch-speaking versus French-speaking respondents",
        col.names = c("", "Estimate", "SE", "Estimate", "SE"),
        digits = 3) %>%
  kableExtra::kable_styling(font_size = 9) %>%
  kableExtra::add_header_above(c("", "Dutch speaking" = 2,
                                 "French speaking" = 2)) %>%
  kableExtra::group_rows(start_row = which(t$type == "loading")[1],
                         end_row = max(which(t$type == "loading")),
                         "Loadings") %>%
  kableExtra::group_rows(start_row = which(t$type == "intercept")[1],
                         end_row = max(which(t$type == "intercept")),
                         "Intercept") %>%
  kableExtra::group_rows(start_row = 8,
                         end_row = 14,
                         "Variance") %>%
  kableExtra::group_rows(start_row = 7,
                         end_row = 7,
                         "Covariance") %>%
  kableExtra::add_footnote(label = "* p <.05, ** p <.01, *** p <.001")
#which(t$type == "covariance")
#which(t$type == "variance")


## ----nc-loadings2-----------------------------------------------------------------------------------------
est <- standardizedSolution(fit_sameloadings_nc) %>%
  mutate(type = ifelse(op == "=~",
                       "loading",
                       ifelse(op == "~1",
                              "intercept",
                              ifelse(lhs == rhs,
                                     "variance",
                                     "covariance"))),
         estimate = sprintf("%.3f %s", `est.std`, gtools::stars.pval(pvalue))) %>%
  mutate(link = ifelse(type %in% c("loading", "covariance"),
                       sprintf("%s %s %s", lhs, op, rhs),
                       lhs)) %>%
  dplyr::select(type, link, estimate, se)
t <- cbind(est[1:(nrow(est)/2),], est[(nrow(est)/2+1):nrow(est),-c(1,2)])
t[, -1] %>%
  kable(booktabs = TRUE,
        caption = "Standardized coefficient estimates and standard errors for the metric measurement invariance model for NC to compare Dutch-speaking versus French-speaking respondents",
        col.names = c("", "Estimate", "SE", "Estimate", "SE"),
        digits = 3) %>%
  kableExtra::kable_styling(font_size = 9) %>%
  kableExtra::add_header_above(c("", "Dutch speaking" = 2,
                                 "French speaking" = 2)) %>%
  kableExtra::group_rows(start_row = which(t$type == "loading")[1],
                         end_row = max(which(t$type == "loading")),
                         "Loadings") %>%
  kableExtra::group_rows(start_row = which(t$type == "intercept")[1],
                         end_row = max(which(t$type == "intercept")),
                         "Intercept") %>%
  kableExtra::group_rows(start_row = 8,
                         end_row = 14,
                         "Variance") %>%
  kableExtra::group_rows(start_row = 7,
                         end_row = 7,
                         "Covariance") %>%
  kableExtra::add_footnote(label = "* p <.05, ** p <.01, *** p <.001")
#which(t$type == "covariance")
#which(t$type == "variance")


## ----reps-descriptives2, message=FALSE, warning=FALSE, include=TRUE, echo = FALSE, fig.cap = "Descriptive statistics for Dutch-speaking (left) and French-speaking (right) parents for the REPS scale items.", fig.width = 6.5, fig.height = 9, eval = TRUE----
#####----------------------------APPENDIX: REPS----------------------------#####
likert_data_reps <- reps_data %>%
  as.data.frame() %>%
  rbind(c("10-5X-03", "1", "Single response", "wash_hands", "x6_14", NA)) %>% #this line is missing from the data
  left_join(children) %>%
  dplyr::filter(str_detect(community, "Dutch speaking")) %>%
  dplyr::select(question_short, response) %>%
  pivot_wider(names_from = question_short, values_from = response,
              values_fn = list)
likert_data_reps2 <- as.data.frame(do.call(cbind,
                                           sapply(1:ncol(likert_data_reps),
                                                  FUN = function(x) {
                                                    likert_data_reps[[x]]
                                                  })))
colnames(likert_data_reps2) <- names(likert_data_reps)
likert_data_reps <- likert_data_reps2 %>%
  dplyr::select(reps_key %>%
                  arrange(factor) %>%
                  dplyr::pull(question_short))#order according to the factor
names(likert_data_reps) <-
  unname(unlist(reps_key[
    match(names(likert_data_reps),reps_key$question_short),
    "question_text_english"]))
p_nl <- gglikert(likert_data_reps, sort = "none", y_label_wrap = 25,
                 labels_size = 1.5)


#How often is each question answered in each of the communities?
nb_answered <- reps_data %>%
  left_join(children) %>%
  group_by(question_code, question_short, community) %>%
  summarize(n = n_distinct(child_id)) %>%
  ungroup()
#wash_hands is never answered in the french community


full_available <- reps_data %>%
  group_by(child_id) %>%
  summarize(n = n()) %>%
  mutate(rest = (n %% 16)) %>%
  left_join(children) %>%
  filter((community == "Dutch speaking" & n %% 16 == 0) |
           (community == "French speaking" & n %% 15 == 0)) %>%
  dplyr::pull(child_id) #parents of these children filled in all questions


likert_data_reps <- reps_data %>%
  as.data.frame() %>%
  left_join(children) %>%
  dplyr::filter(str_detect(community, "French speaking") &
                  child_id %in% full_available) %>%
  dplyr::select(question_short, response) %>%
  pivot_wider(names_from = question_short, values_from = response,
              values_fn = list)
likert_data_reps2 <- as.data.frame(do.call(cbind,
                                           sapply(1:(ncol(likert_data_reps)),
                                                  FUN = function(x) {
                                                    likert_data_reps[[x]]
                                                  })))
colnames(likert_data_reps2) <- names(likert_data_reps)
likert_data_reps <- likert_data_reps2 %>%
  dplyr::select(reps_key %>%
                  dplyr::filter(reps_key$question_short != "wash_hands") %>%
                  arrange(factor) %>%
                  dplyr::pull(question_short))
names(likert_data_reps) <-
  unname(unlist(reps_key[
    match(names(likert_data_reps),reps_key$question_short),
    "question_text_english"]))
factorize <- function(x){
  out <- factor(as.factor(x), levels = as.character(seq(1,7)))
  return(out)
}
p_fr <- likert_data_reps %>%
  mutate_at(1:15, factorize) %>%
  gglikert(sort = "none", y_label_wrap = 25,
           labels_size = 1.5)

p_nl + p_fr + plot_layout(guides = "collect") &
  theme(legend.position = "bottom",
        text = element_text(size = 8))


## ----reps-loadings----------------------------------------------------------------------------------------
est <- parameterEstimates(fit_free_reps) %>%
  mutate(type = ifelse(op == "=~",
                       "loading",
                       ifelse(op == "~1",
                              "intercept",
                              ifelse(lhs == rhs,
                                     "variance",
                                     "covariance"))),
         estimate = sprintf("%.3f %s", est, gtools::stars.pval(pvalue))) %>%
  mutate(link = ifelse(type %in% c("loading", "covariance"),
                       sprintf("%s %s %s", lhs, op, rhs),
                       lhs),
         type = factor(as.factor(type),
                       levels = c("loading", "covariance", "intercept",
                                  "variance"),
                       ordered = TRUE)) %>%
  dplyr::select(type, link, estimate, se)
t <- cbind(est[1:(nrow(est)/2), ], est[(nrow(est) / 2 + 1):nrow(est), -c(1, 2)])

t[1:31, -1] %>%
  kable(booktabs = TRUE,
        caption = "Unstandardized coefficient estimates and standard errors for the metric measurement invariance model for REPS to compare Dutch-speaking versus French-speaking respondents.",
        col.names = c("", "Estimate", "SE", "Estimate", "SE"),
        digits = 3) %>%
  kableExtra::kable_styling(font_size = 9) %>%
  kableExtra::add_header_above(c("", "Dutch speaking" = 2,
                                 "French speaking" = 2)) %>%
  kableExtra::group_rows(start_row = which(t$type == "loading")[1],
                         end_row = max(which(t$type == "loading")),
                         "Loadings") %>%
  kableExtra::group_rows(start_row = which(t$type == "variance")[1],
                         end_row = max(which(t$type == "variance")),
                         "Variance") %>%
  kableExtra::group_rows(start_row = which(t$type == "covariance")[1],
                         end_row = max(which(t$type == "covariance")),
                         "Covariance") %>%
  kableExtra::add_footnote(label = "* p <.05, ** p <.01, *** p <.001")
#which(t$type == "covariance")
#which(t$type == "variance")


## ----reps-loadings2---------------------------------------------------------------------------------------
t[32:47, -1] %>%
  kable(booktabs = TRUE,
        caption = "Unstandardized coefficient estimates and standard errors for the metric measurement invariance model for REPS to compare Dutch-speaking versus French-speaking respondents (continued).",
        col.names = c("", "Estimate", "SE", "Estimate", "SE"),
        digits = 3,
        row.names = FALSE) %>%
  kableExtra::kable_styling(font_size = 9) %>%
  kableExtra::add_header_above(c("", "Dutch speaking" = 2,
                                 "French speaking" = 2)) %>%
  kableExtra::group_rows(start_row = 1,
                         end_row = 16,
                         "Intercept") %>%
  kableExtra::add_footnote(label = "* p <.05, ** p <.01, *** p <.001")
#which(t$type == "covariance")
#which(t$type == "variance")


## ----reps-loadings3---------------------------------------------------------------------------------------
est <- standardizedSolution(fit_free_reps) %>%
  mutate(type = ifelse(op == "=~",
                       "loading",
                       ifelse(op == "~1",
                              "intercept",
                              ifelse(lhs == rhs,
                                     "variance",
                                     "covariance"))),
         estimate = sprintf("%.3f %s", `est.std`, gtools::stars.pval(pvalue))) %>%
  mutate(link = ifelse(type %in% c("loading", "covariance"),
                       sprintf("%s %s %s", lhs, op, rhs),
                       lhs),
         type = factor(as.factor(type),
                       levels = c("loading", "covariance", "intercept",
                                  "variance"),
                       ordered = TRUE)) %>%
  dplyr::select(type, link, estimate, se)
t <- cbind(est[1:(nrow(est)/2),], est[(nrow(est)/2+1):nrow(est),-c(1,2)])
t[1:31, -1] %>%
  kable(booktabs = TRUE,
        caption = "Standardized coefficient estimates and standard errors for the metric measurement invariance model for REPS to compare Dutch-speaking versus French-speaking respondents.",
        col.names = c("", "Estimate", "SE", "Estimate", "SE"),
        digits = 3) %>%
  kableExtra::kable_styling(font_size = 9) %>%
  kableExtra::add_header_above(c("", "Dutch speaking" = 2,
                                 "French speaking" = 2)) %>%
  kableExtra::group_rows(start_row = which(t$type == "loading")[1],
                         end_row = max(which(t$type == "loading")),
                         "Loadings") %>%
  kableExtra::group_rows(start_row = which(t$type == "variance")[1],
                         end_row = max(which(t$type == "variance")),
                         "Variance") %>%
  kableExtra::group_rows(start_row = which(t$type == "covariance")[1],
                         end_row = max(which(t$type == "covariance")),
                         "Covariance") %>%
  kableExtra::add_footnote(label = "* p <.05, ** p <.01, *** p <.001")
#which(t$type == "covariance")
#which(t$type == "variance")


## ----reps-loadings4---------------------------------------------------------------------------------------
t[32:47, -1] %>%
  kable(booktabs = TRUE,
        caption = "Standardized coefficient estimates and standard errors for the metric measurement invariance model for REPS to compare Dutch-speaking versus French-speaking respondents (continued).",
        col.names = c("", "Estimate", "SE", "Estimate", "SE"),
        digits = 3,
        row.names = FALSE) %>%
  kableExtra::kable_styling(font_size = 9) %>%
  kableExtra::add_header_above(c("", "Dutch speaking" = 2,
                                 "French speaking" = 2)) %>%
  kableExtra::group_rows(start_row = 1,
                         end_row = 16,
                         "Intercept") %>%
  kableExtra::add_footnote(label = "* p <.05, ** p <.01, *** p <.001")
#which(t$type == "covariance")
#which(t$type == "variance")


## ----message=FALSE, warning=FALSE, include=FALSE, echo = FALSE, eval = TRUE-------------------------------
#-------------1. Analysing only complete data (no missing answers)-------------#
df <- data_cfa_reps[complete.cases(data_cfa_reps[, -c(1, 2)]), -c(1, 2)]
pca <- prcomp(df)
scree1 <- screeplot(pca, type = "lines")#2 factors is best
covmat <- cov(data_cfa_reps[complete.cases(data_cfa_reps), -c(1, 2)])
cormat <- cor(data_cfa_reps[complete.cases(data_cfa_reps), -c(1, 2)])
efa_complete <- fa(cormat,
                   covar = FALSE,
                   2, rotate = "oblimin",
                   fm = "ml", n.obs = nrow(df))
tables_efa_complete <- fa_table(efa_complete,
                                title = "Results for the EFA and oblique rotation
                         and two factors, based on complete observations only.",
                                varlabels = str_wrap(reps_key$question_short,
                                                     15))
tables_efa_complete$ind_table
#the two additional statements do not fit in either of the factors.
#the EFA further uncovers the original two factors although the statement "I encourage my child to do physical activity" is now part of the engagement with risk factor in stead of the protection factor.

#--------------------------------2. EFA with MI--------------------------------#
library(mifa)
set.seed(4)
mi <- mifa(data = data_cfa_reps[,-c(1, 2)])
fit <- fa(mi$cov_combined, n.obs = nrow(data_cfa_reps), nfactors = 2)
#fa.diagram(fit)
tables_atop <- fa_table(fit,
                        title = "Results for the EFA with multiple imputation and oblique rotation
                         and two factors.",
                        varlabels = str_wrap(reps_key$question_short,
                                             15))
tables_atop$ind_table
#Again, the washing hands and playing in the rain statements don't fit with
# either factor. "I encourage my child to do physical activity (with the least
# risk of injury)." also does not fit well with either.


## ----reps-efa1--------------------------------------------------------------------------------------------
tables_efa_complete$ind_table


## ----reps-efa2--------------------------------------------------------------------------------------------
tables_atop$ind_table# %>%
# row_spec(15, background = "#D93B3B") %>%
# row_spec(16, background = "#D93B3B")


## ----include = FALSE, warning = FALSE, message = FALSE----------------------------------------------------
#---------------------------------REPS with 3 or 4 factors---------------------#
df <- data_cfa_reps %>%
  dplyr::select(-play_in_rain, -wash_hands)
df <- df[complete.cases(df[, -c(1, 2)]), -c(1, 2)]
pca <- prcomp(df)
screeplot(pca, type = "lines")#2-4 factors is best
covmat <- cov(df)
cormat <- cor(df)
fa.parallel(cormat, n.obs = nrow(df))#2 factors is best
vss(cormat, n.obs = nrow(df)) #-> 2 or 4 is best

#-------------------------------3 factor model---------------------------------#
efa_3 <- fa(cormat,
            covar = FALSE,
            3, rotate = "oblimin",
            fm = "ml", n.obs = nrow(df))
model_reps <- "protection_from_injury1 =~ 1*prevention_importance + importance_supervision + limit_dangerous_activities + chance_of_injury
protection_from_injury2 =~ 1*concerned_injury + concerned_hazards + avoid_risk
          risk_engagement =~ 1*promote_physical_challenges + physical_limits + explore_new_environments + benefits_outweigh_risk + importance_managing_risk + risk_self_confidence"
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
fit_free_reps3 <- cfa(model_reps,
                      data = likert_data_reps %>%
                        filter(community != "Dutch speaking.French speaking"),
                      group = "community")
fit_sameloadings_reps3 <- cfa(model_reps,
                              data = likert_data_reps %>%
                                filter(community !=
                                         "Dutch speaking.French speaking"),
                              group = "community",
                              group.equal = c("loadings"))
fit_sameloadingsinterc_reps3 <- cfa(model_reps,
                                    data = likert_data_reps %>%
                                      filter(community !=
                                               "Dutch speaking.French speaking"),
                                    group = "community",
                                    group.equal = c("loadings", "intercepts"))

anov_reps_metric <- anova(fit_free_reps3, fit_sameloadings_reps3) #metric model cannot be rejected
anov_reps_strong <- anova(fit_free_reps3, fit_sameloadingsinterc_reps3)
#interpret the fit:
interpret(fit_sameloadings_reps3) #they almost all show poor fit
fitmeasures(fit_sameloadings_reps3, c("chisq", "df", "pvalue", "cfi",
                                      "tli", "rmsea", "srmr", "AIC",
                                      "BIC"))
cr_omega_reps3 <- compRelSEM(fit_sameloadings_reps3, tau.eq = F, obs.var = T)
cr_alpha_reps3 <- compRelSEM(fit_sameloadings_reps3, tau.eq = T, obs.var = T)
#-------------------------------4 factor model---------------------------------#
efa_4 <- fa(cormat,
            covar = FALSE,
            4, rotate = "oblimin",
            fm = "ml", n.obs = nrow(df))
model_reps <- "protection_from_injury1 =~ 1*prevention_importance + importance_supervision + limit_dangerous_activities + chance_of_injury
protection_from_injury2 =~ 1*concerned_injury + concerned_hazards + avoid_risk
          risk_engagement =~ 1*physical_limits + explore_new_environments + benefits_outweigh_risk + importance_managing_risk + risk_self_confidence
risk_engagement2 =~ 1*promote_physical_challenges"
fit_free_reps4 <- cfa(model_reps,
                      data = likert_data_reps %>%
                        filter(community != "Dutch speaking.French speaking"),
                      group = "community")
fit_sameloadings_reps4 <- cfa(model_reps,
                              data = likert_data_reps %>%
                                filter(community !=
                                         "Dutch speaking.French speaking"),
                              group = "community",
                              group.equal = c("loadings"))
fit_sameloadingsinterc_reps4 <- cfa(model_reps,
                                    data = likert_data_reps %>%
                                      filter(community !=
                                               "Dutch speaking.French speaking"),
                                    group = "community",
                                    group.equal = c("loadings", "intercepts"))

anov_reps_metric <- anova(fit_free_reps4, fit_sameloadings_reps4) #metric model cannot be rejected
anov_reps_strong <- anova(fit_free_reps4, fit_sameloadingsinterc_reps4)
#interpret the fit:
interpret(fit_sameloadings_reps4) #they all show poor fit
fitmeasures(fit_sameloadings_reps4, c("chisq", "df", "pvalue", "cfi",
                                      "tli", "rmsea", "srmr", "AIC",
                                      "BIC"))
cr_omega_reps4 <- compRelSEM(fit_sameloadings_reps4, tau.eq = F, obs.var = T)
cr_alpha_reps4 <- compRelSEM(fit_sameloadings_reps4, tau.eq = T, obs.var = T)


## ----reps-efa-3-------------------------------------------------------------------------------------------
tables_reps_3 <-
  fa_table(efa_3,
           fnames = c("risk engagement", "protection from injury1", "protection from injury2"),
           title = "Results for the EFA of the REPS scale, using oblique rotation and three factors."
  )
tables_reps_3$ind_table %>%
  row_spec(13, background = "#D93B3B") %>%
  kableExtra::column_spec(1, width = "1.2in") %>%
  kableExtra::column_spec(2:7, width = "0.6in")


## ----reps-efa-4-------------------------------------------------------------------------------------------
tables_reps_4 <-
  fa_table(efa_4,
           fnames = c("risk engagement1", "protection from injury1", "protection from injury2", "risk engagement2"),
           title = "Results for the EFA of the REPS scale, using oblique rotation and four factors."
  )
tables_reps_4$ind_table %>%
  row_spec(5, background = "#D93B3B") %>%
  kableExtra::column_spec(1, width = "1.2in") %>%
  kableExtra::column_spec(2:7, width = "0.6in")


## ----reps-34-fit------------------------------------------------------------------------------------------
data.frame(
  factor3 = fitmeasures(fit_sameloadings_reps3,
                        c("chisq", "df", "pvalue", "cfi", "tli", "rmsea",
                          "srmr", "AIC", "BIC")),
  factor4 = fitmeasures(fit_sameloadings_reps4,
                        c("chisq", "df", "pvalue", "cfi", "tli", "rmsea",
                          "srmr", "AIC", "BIC"))
) %>%
  dplyr::mutate_if(is.numeric, avoid_scientif_notation) %>%
  kable(booktabs = TRUE,
        col.names = c("3-factor model", "4-factor model"),
        caption = "Fit measures for the REPS models with three or four factors.") %>%
  kableExtra::kable_styling()


## ----reps-34-cr-------------------------------------------------------------------------------------------
cr_omega_reps3 %>%
  rbind(cr_omega_reps4) %>%
  kable(booktabs = TRUE,
        col.names = c("", "protection_from_injury1", "protection_from_injury2", "risk_engagement1"),
        caption = "Composite reliability (omega) for the REPS models with three or four factors.") %>%
  kableExtra::kable_styling() %>%
  kableExtra::group_rows("3-factor model", start_row = 1, end_row = 2) %>%
  kableExtra::group_rows("4-factor model", start_row = 3, end_row = 4)


## ----opp1, message=FALSE, warning=FALSE, include=TRUE, echo = FALSE, fig.cap = "Descriptive statistics for Dutch-speaking (left) and French-speaking (right) parents for the questions on how many days per week their child participate in...", fig.width = 6.5, fig.height = 4----
#####-------------------APPENDIX: OUTDOOR PLAY FREQUENCY-------------------#####
likert_data_atop <- atop_data_p1 %>%
  as.data.frame() %>%
  left_join(children) %>%
  dplyr::filter(!str_detect(community, "French")) %>%
  mutate(response = round(response)) %>%
  dplyr::select(activity, response) %>%
  pivot_wider(names_from = activity, values_from = response,
              values_fn = list)
likert_data_atop2 <- as.data.frame(do.call(cbind,
                                           sapply(1:ncol(likert_data_atop),
                                                  FUN = function(x) {
                                                    likert_data_atop[[x]]
                                                  })))
colnames(likert_data_atop2) <- names(likert_data_atop)
likert_data_atop2 <- likert_data_atop2 %>%
  mutate_all(.funs = ~factor(as.factor(.x), levels = as.character(seq(0,7))))
p_nl <- gglikert(likert_data_atop2, sort = "none", y_label_wrap = 20,
                 labels_size = 2)


likert_data_atop <- atop_data_p1 %>%
  as.data.frame() %>%
  left_join(children) %>%
  dplyr::filter(str_detect(community, "French")) %>%
  mutate(response = round(response)) %>%
  dplyr::select(activity, response) %>%
  pivot_wider(names_from = activity, values_from = response,
              values_fn = list)
likert_data_atop2 <- as.data.frame(do.call(cbind,
                                           sapply(1:ncol(likert_data_atop),
                                                  FUN = function(x) {
                                                    likert_data_atop[[x]]
                                                  })))
colnames(likert_data_atop2) <- names(likert_data_atop)
likert_data_atop2 <- likert_data_atop2 %>%
  mutate_all(.funs = ~factor(as.factor(.x), levels = as.character(seq(0,7))))

p_fr <- gglikert(likert_data_atop2, sort = "none", y_label_wrap = 20,
                 labels_size = 2)
p_nl + p_fr + plot_layout(guides = "collect") &
  theme(legend.position = "bottom",
        text = element_text(size = 9))


## ----opp2, message=FALSE, warning=FALSE, include=TRUE, echo = FALSE, fig.cap = "Descriptive statistics for Dutch-speaking (left) and French-speaking (right) parents for the questions 'Do you associate the following activity as typical outdoor playing behavior of you child?'", fig.width = 6.5, fig.height = 6.5----
likert_data_atop <- atop_data_p2 %>%
  as.data.frame() %>%
  left_join(children) %>%
  dplyr::filter(!str_detect(community, "French")) %>%
  mutate(response = ifelse(response == "association",
                           "Yes",
                           ifelse(response == "no association",
                                  "No", "Maybe"))) %>%
  dplyr::select(activity, response) %>%
  pivot_wider(names_from = activity, values_from = response,
              values_fn = list)
likert_data_atop2 <- as.data.frame(do.call(cbind,
                                           sapply(1:ncol(likert_data_atop),
                                                  FUN = function(x) {
                                                    likert_data_atop[[x]]
                                                  })))
colnames(likert_data_atop2) <- names(likert_data_atop)
likert_data_atop2 <- likert_data_atop2 %>%
  mutate_all(.funs = ~factor(as.factor(.x),
                             levels = c("Yes", "Maybe", "No")))
p_nl <- gglikert(likert_data_atop2, sort = "none", y_label_wrap = 25,
                 labels_size = 2)


likert_data_atop <- atop_data_p2 %>%
  as.data.frame() %>%
  left_join(children) %>%
  dplyr::filter(str_detect(community, "French")) %>%
  mutate(response = ifelse(response == "association",
                           "Yes",
                           ifelse(response == "no association",
                                  "No", "Maybe"))) %>%
  dplyr::select(activity, response) %>%
  pivot_wider(names_from = activity, values_from = response,
              values_fn = list)
likert_data_atop2 <- as.data.frame(do.call(cbind,
                                           sapply(1:ncol(likert_data_atop),
                                                  FUN = function(x) {
                                                    likert_data_atop[[x]]
                                                  })))
colnames(likert_data_atop2) <- names(likert_data_atop)
likert_data_atop2 <- likert_data_atop2 %>%
  mutate_all(.funs = ~factor(as.factor(.x),
                             levels = c("Yes", "Maybe", "No")))

p_fr <- gglikert(likert_data_atop2, sort = "none", y_label_wrap = 25,
                 labels_size = 2)
p_nl + p_fr + plot_layout(guides = "collect") &
  theme(legend.position = "bottom",
        text = element_text(size = 9))


## ----opp3, message=FALSE, warning=FALSE, include=TRUE, echo = FALSE, fig.cap = "Descriptive statistics for Dutch-speaking (left) and French-speaking (right) parents for the questions 'How many hours does your child spend on ... on an average weekday/weekend day?'", fig.width = 6.5, fig.height = 4----
likert_data_atop <- atop_data_p3 %>%
  as.data.frame() %>%
  left_join(children) %>%
  dplyr::filter(!str_detect(community, "French")) %>%
  mutate(
    activity = paste0(activity, "_", during),
    response = ifelse(response == "0 - < 1",
                      "0",
                      ifelse(response == "< 1 - 1-2",
                             "1-2", ifelse(response == "1-2 - 2-3",
                                           "2-3",
                                           ifelse(str_detect(response, "3-4"),
                                                  "3-4", response))))) %>%
  dplyr::select(activity, response) %>%
  pivot_wider(names_from = activity, values_from = response,
              values_fn = list)
likert_data_atop2 <- as.data.frame(do.call(cbind,
                                           sapply(1:ncol(likert_data_atop),
                                                  FUN = function(x) {
                                                    likert_data_atop[[x]]
                                                  })))
colnames(likert_data_atop2) <- names(likert_data_atop)
likert_data_atop2 <- likert_data_atop2 %>%
  mutate_all(.funs = ~factor(as.factor(.x),
                             levels = c("0", "< 1", "1-2", "2-3", "3-4",
                                        "> 4")))
p_nl <- gglikert(likert_data_atop2, sort = "none", y_label_wrap = 25,
                 labels_size = 2)

likert_data_atop <- atop_data_p3 %>%
  as.data.frame() %>%
  left_join(children) %>%
  dplyr::filter(str_detect(community, "French")) %>%
  mutate(
    activity = paste0(activity, "_", during),
    response = ifelse(response == "0 - < 1",
                      "0",
                      ifelse(response == "< 1 - 1-2",
                             "1-2", ifelse(response == "1-2 - 2-3",
                                           "2-3",
                                           ifelse(str_detect(response, "3-4"),
                                                  "3-4", response))))) %>%
  dplyr::select(activity, response) %>%
  pivot_wider(names_from = activity, values_from = response,
              values_fn = list)
likert_data_atop2 <- as.data.frame(do.call(cbind,
                                           sapply(1:ncol(likert_data_atop),
                                                  FUN = function(x) {
                                                    likert_data_atop[[x]]
                                                  })))
colnames(likert_data_atop2) <- names(likert_data_atop)
likert_data_atop2 <- likert_data_atop2 %>%
  mutate_all(.funs = ~factor(as.factor(.x),
                             levels = c("0", "< 1", "1-2", "2-3", "3-4",
                                        "> 4")))

p_fr <- gglikert(likert_data_atop2, sort = "none", y_label_wrap = 25,
                 labels_size = 2)
p_nl + p_fr + plot_layout(guides = "collect") &
  theme(legend.position = "bottom",
        text = element_text(size = 9))


## ----efa-opp1---------------------------------------------------------------------------------------------
tables_atop_p_mixed <-
  fa_table(efa_atop_p_mixed,
           fnames = c("screentime", "indoor activities", "homework",
                      "organized sports"),
           title = "Results for the EFA on the statements with continuous answers, using oblique rotation and four factors."
  )
tables_atop_p_mixed$ind_table %>%
  row_spec(4, background = "#D93B3B") %>%
  kableExtra::column_spec(1, width = "01in") %>%
  kableExtra::column_spec(2:8, width = "0.55in")


## ----efa-opp2---------------------------------------------------------------------------------------------
tables_atop_p_binary <-
  fa_table(efa_atop_p_binary,
           fnames = c("natural play", "water games",
                      "made up games lingering",
                      "hide and seek", "unorganized sports"),
           title = "Results for the EFA on the statements with binary answers, using oblique rotation and five factors.")
tables_atop_p_binary$ind_table %>%
  row_spec(3, background = "#D93B3B") %>%
  row_spec(4, background = "#D93B3B") %>%
  row_spec(7, background = "#D93B3B") %>%
  kableExtra::column_spec(1, width = "0.8in") %>%
  kableExtra::column_spec(2:9, width = "0.5in")


## ----im-nlvsfr, message=FALSE, warning=FALSE, include=TRUE, echo = FALSE, fig.cap = "Descriptive statistics for Dutch-speaking (left) and French-speaking (right) parents for the questions on independent mobility.", fig.width = 6.5, fig.height = 5----
#####-------------------APPENDIX: INDEPENDENT MOBILITY---------------------#####
likert_data_im <- im_data %>%
  as.data.frame() %>%
  left_join(children) %>%
  dplyr::filter(!str_detect(community, "French")) %>%
  group_by(child_id, question_short) %>%
  summarize(response = first(response)) %>% # keep only one answer per child
  ungroup() %>%
  dplyr::select(question_short, response, child_id) %>%
  pivot_wider(names_from = question_short, values_from = response,
              id_cols = child_id)
p_nl <- gglikert(likert_data_im[, -1], sort = "none", y_label_wrap = 25,
                 labels_size = 2)


likert_data_im <- im_data %>%
  as.data.frame() %>%
  left_join(children) %>%
  dplyr::filter(!str_detect(community, "Dutch")) %>%
  group_by(child_id, question_short) %>%
  summarize(response = first(response)) %>% # keep only one answer per child
  ungroup() %>%
  dplyr::select(question_short, response, child_id) %>%
  pivot_wider(names_from = question_short, values_from = response,
              id_cols = child_id)

p_fr <- gglikert(likert_data_im[, -1], sort = "none", y_label_wrap = 25,
                 labels_size = 2)
p_nl + p_fr + plot_layout(guides = "collect") &
  theme(legend.position = "bottom",
        text = element_text(size = 9))


## ----efa-im-----------------------------------------------------------------------------------------------
tables_im_data2 <-
  fa_table(efa_im_data2,
           fnames = c("explore", "engage with traffic",
                      "after dark",
                      "public transport"),
           title = "Results for the EFA and oblique rotation and four factors for the independent mobility questions."
  )
tables_im_data2$ind_table %>%
  row_spec(6, background = "#D93B3B") %>%
  kableExtra::column_spec(1, width = "1.25in") %>%
  kableExtra::column_spec(2:8, width = "0.5in")


## ----sem-results, warning = FALSE, message = FALSE--------------------------------------------------------
#####------------------APPENDIX: STRUCTURAL EQUATION MODEL-----------------#####
est <- parameterEstimates(sem_output) %>%
  mutate(type = ifelse(op == "=~",
                       "loading",
                       ifelse(op == "~1",
                              "intercept",
                              ifelse(op == "~",
                                     "regression",
                                     ifelse(lhs == rhs,
                                            "variance",
                                            "covariance")))),
         estimate = sprintf("%.3f %s", est, gtools::stars.pval(pvalue))) %>%
  mutate(link = ifelse(type %in% c("loading", "covariance", "regression"),
                       sprintf("%s %s %s", lhs, op, rhs),
                       lhs)) %>%
  dplyr::select(type, link, estimate, se) %>%
  mutate(type = factor(as.factor(type),
                       levels = c("loading", "covariance", "regression",
                                  "variance"),
                       ordered = TRUE)) %>%
  arrange(type)
est[, -1] %>%
  kable(booktabs = TRUE,
        caption = "Unstandardized coefficient estimates and standard errors for the SEM.",
        col.names = c("", "Estimate", "SE"),
        digits = 3,
        longtable = TRUE) %>%
  kableExtra::kable_styling(font_size = 9) %>%
  kableExtra::group_rows(start_row = which(est$type == "loading")[1],
                         end_row = max(which(est$type == "loading")),
                         "Loadings") %>%
  kableExtra::group_rows(start_row = which(est$type == "variance")[1],
                         end_row = max(which(est$type == "variance")),
                         "Variance") %>%
  kableExtra::group_rows(start_row = which(est$type == "covariance")[1],
                         end_row = max(which(est$type == "covariance")),
                         "Covariance") %>%
  kableExtra::group_rows(start_row = which(est$type == "regression")[1],
                         end_row = max(which(est$type == "regression")),
                         "Regression") %>%
  kable_styling(latex_options = c("repeat_header"), font_size = 9) %>%
  kableExtra::add_footnote(label = "* p <.05, ** p <.01, *** p <.001")
#which(t$type == "covariance")
#which(t$type == "variance")


## ----sem-results-std, warning = FALSE, message = FALSE----------------------------------------------------
est <- standardizedSolution(sem_output) %>%
  mutate(type = ifelse(op == "=~",
                       "loading",
                       ifelse(op == "~1",
                              "intercept",
                              ifelse(op == "~",
                                     "regression",
                                     ifelse(lhs == rhs,
                                            "variance",
                                            "covariance")))),
         estimate = sprintf("%.3f %s", `est.std`, gtools::stars.pval(pvalue))) %>%
  mutate(link = ifelse(type %in% c("loading", "covariance", "regression"),
                       sprintf("%s %s %s", lhs, op, rhs),
                       lhs)) %>%
  dplyr::select(type, link, estimate, se) %>%
  mutate(type = factor(as.factor(type),
                       levels = c("loading", "covariance", "regression",
                                  "variance"),
                       ordered = TRUE)) %>%
  arrange(type)
est[, -1] %>%
  kable(booktabs = TRUE,
        caption = "Standardized coefficient estimates and standard errors for the SEM.",
        col.names = c("", "Estimate", "SE"),
        digits = 3,
        longtable = TRUE) %>%
  kableExtra::kable_styling(font_size = 9) %>%
  kableExtra::group_rows(start_row = which(est$type == "loading")[1],
                         end_row = max(which(est$type == "loading")),
                         "Loadings") %>%
  kableExtra::group_rows(start_row = which(est$type == "variance")[1],
                         end_row = max(which(est$type == "variance")),
                         "Variance") %>%
  kableExtra::group_rows(start_row = which(est$type == "covariance")[1],
                         end_row = max(which(est$type == "covariance")),
                         "Covariance") %>%
  kableExtra::group_rows(start_row = which(est$type == "regression")[1],
                         end_row = max(which(est$type == "regression")),
                         "Regression")  %>%
  kable_styling(latex_options = c("repeat_header"), font_size = 9) %>%
  kableExtra::add_footnote(label = "* p <.05, ** p <.01, *** p <.001")
#which(t$type == "covariance")
#which(t$type == "variance")



