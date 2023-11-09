options(scipen=999)
library(tidyverse)
library(readxl)
library(geobr)
library(sf)
library(ggspatial)

data <-  readxl::read_xlsx("Malaria_expenditures.xlsx")

#############################################
## Figures and tables from the article text##
#############################################

# Fig1. Spatial distribution of groups of municipalities defined according to the apportionment strategy
# shape
municipaly <- read_municipality(code_muni = 'all', year=2019)
municipaly$code_muni <- substr(municipaly$code_muni,1, 6)
municipaly$code_muni <- as.numeric(municipaly$code_muni)

ufs <- read_state() %>% 
  filter(code_region == 1 | name_state == "Maranhão" | name_state == "Mato Grosso")

colnames(data)[2] <- "code_muni"

map <- data %>% 
  left_join(municipaly, by="code_muni") %>% 
  select(-c("abbrev_state"))

map <- map %>% 
  mutate(categoria_positivo = factor(tipology, levels=c("high incidence | high and average dispersion",
                                                        "high incidence | low dispersion",
                                                        "average incidence | high dispersion", 
                                                        "average incidence | average dispersion",
                                                        "average incidence | low dispersion",
                                                        "low incidence | high dispersion",
                                                        "low incidence | low and average dispersion",
                                                        "zero incidence | no dispersion", 
                                                        "no malaria")))


map <- st_as_sf(map)


p <- map %>%
  filter(year == 2019) %>% 
  ggplot() +
  geom_sf(aes(fill = tipology), linewidth = 0.1) +
  scale_fill_manual(values = c("darkred", "red","#B87C4C","#E2B659","#F9F871","#00BAA2","#00DE83","#00FF40","lightgray"))+
  geom_sf(data = ufs, fill = NA, linewidth = 1.1) +
  geom_sf_label(data = ufs, aes(label = abbrev_state), alpha = .6) +
  labs(x = NULL, y = NULL, fill = NULL) +
  annotation_scale(location="br") +
  annotation_north_arrow(location = "br",
                         which_north = "true",
                         height = unit(2, "cm"),
                         width = unit(2, "cm"),
                         pad_x = unit(0.0, "in"),
                         pad_y = unit(0.2, "in"),
                         style = north_arrow_fancy_orienteering) +
  theme_minimal() +
  theme(legend.text = element_text(size = 16),
        legend.position = c(.21, .1),
        axis.title = element_blank(),
        axis.text=element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        legend.key.width=unit(2.5,"cm"))

ggsave("maps/fig1.pdf", width = 13, height = 10)
ggsave("maps/fig1.png", width = 13, height = 10, dpi = 300)


# Table 1. Total malaria expenditure from public health system perspective by cost components, between 2015-2019 (PPP-US$ 2020 million).
table1 <- data %>% 
  mutate(across(exp_cons:exp_surveillance, ~.x*ppp),
         Insecticide_Bednets = exp_inseticides + exp_LLIN) %>% 
  group_by(year) %>% 
  summarise(Diagnostic_tests = sum(exp_diag, na.rm = TRUE),
            Doctor_appointments = sum(exp_cons, na.rm = TRUE),
            Inpatient_care = sum(exp_hospitalizations, na.rm = TRUE),
            Drugs = sum(exp_med, na.rm = TRUE),
            Insecticide_Bednets = sum(Insecticide_Bednets, na.rm = TRUE),
            Blood_screening = sum(exp_blood, na.rm = TRUE),
            Surveillance = sum(exp_surveillance, na.rm = TRUE),
            Agents_Microscopists = sum(exp_agents, na.rm = TRUE),
            Total = sum(Diagnostic_tests,Doctor_appointments,Inpatient_care,Drugs,Insecticide_Bednets,Blood_screening,Surveillance,Agents_Microscopists),
            cases = sum(cases),
            notifications= sum(notifications),
            population = sum(population)) %>%
  mutate(Notifications_per_1000_inhab= (notifications/population)*1000) %>% 
  write_excel_csv(.,"tables/tab1.csv")



# Table 2. Malaria expenditure from public health system perspective, between 2015-2019, per notification and per capita (PPP-US$ 2020), by states of the Brazilian Amazon.
table2 <- data %>% 
  mutate(across(exp_cons:exp_surveillance, ~.x*ppp),
         Insecticide_Bednets = exp_inseticides + exp_LLIN) %>% 
  group_by(year, FS) %>% 
  summarise(cases = sum(cases),
            population= sum(population),
            notifications= sum(notifications),
            Diagnostic_tests = sum(exp_diag, na.rm = TRUE),
            Doctor_appointments = sum(exp_cons, na.rm = TRUE),
            Inpatient_care = sum(exp_hospitalizations, na.rm = TRUE),
            Drugs = sum(exp_med, na.rm = TRUE),
            Insecticide_Bednets = sum(Insecticide_Bednets, na.rm = TRUE),
            Blood_screening = sum(exp_blood, na.rm = TRUE),
            Agents_Microscopists = sum(exp_agents, na.rm = TRUE),
            Surveillance = sum(exp_surveillance, na.rm = TRUE)) %>% 
  transmute(year= year,
            state = FS,
            total_expenditures = Diagnostic_tests + Doctor_appointments + Inpatient_care + Drugs + Insecticide_Bednets + Blood_screening + Agents_Microscopists + Surveillance,
            expenditure_total_percapita = total_expenditures/population,
            expenditure_total_notification = total_expenditures/notifications) %>% 
  select(-total_expenditures) %>% 
  write_excel_csv(., "tables/tab2.csv")


# Table 3. Distribution of malaria expenses from public healthcare system perspective by domains for each state of the Brazilian Amazon Region, 2015 and 2019.
# Table S5: Composition of Malaria expenditures (PPP-US$ 2020) from public healthcare system perspective and number of malaria cases, by states of the Brazilian Amazon Region, 2016-2018

table3 <- data %>% 
  mutate(across(exp_cons:exp_surveillance, ~.x*ppp),
         Insecticide_Bednets = exp_inseticides + exp_LLIN) %>% 
  group_by(year, FS) %>% 
  summarise(cases = sum(cases),
            population= sum(population),
            notifications= sum(notifications),
            Diagnostic_tests = sum(exp_diag, na.rm = TRUE),
            Doctor_appointments = sum(exp_cons, na.rm = TRUE),
            Inpatient_care = sum(exp_hospitalizations, na.rm = TRUE),
            Drugs = sum(exp_med, na.rm = TRUE),
            Insecticide_Bednets = sum(Insecticide_Bednets, na.rm = TRUE),
            Blood_screening = sum(exp_blood, na.rm = TRUE),
            Agents_Microscopists = sum(exp_agents, na.rm = TRUE),
            Surveillance = sum(exp_surveillance, na.rm = TRUE)) %>% 
  transmute(year=year,
            state=FS,
            cases = cases,
            Illness_Treatment = Diagnostic_tests + Doctor_appointments + Inpatient_care + Drugs,
            Control_Preventive = Insecticide_Bednets + Blood_screening + Surveillance,
            Human_Resources = Agents_Microscopists,
            total = Illness_Treatment+Control_Preventive+Human_Resources,
            Illness_Treatment = Illness_Treatment/total*100,
            Control_Preventive = Control_Preventive/total*100,
            Human_Resources = Human_Resources/total*100) %>% 
  select(year, state, total, Illness_Treatment, Control_Preventive, Human_Resources, cases)

table3 %>% 
  filter(year == 2015 | year == 2019) %>% 
  write_excel_csv(., "tables/tab3.csv")

table3 %>% 
  filter(year != 2015 & year != 2019) %>% 
  write_excel_csv(., "tables/tabS5.csv")

####################################################
## Figures and tables from Material Supplementary ##
####################################################

# Fig 2. Malaria expenditures per notification from public healthcare system perspective between 2015-2019, (PPP-US$ 2020).


tmp <- map %>% 
  mutate(expenditure_total_notification = as.double(expenditure_total_notification)*ppp,
         expenditure_total_notification = ifelse(!is.finite(expenditure_total_notification), NA, expenditure_total_notification))

quantile.interval = quantile(tmp$expenditure_total_notification, probs=seq(0, 1, by = 1/5), na.rm = TRUE)
tmp$`Gasto por notificação` = cut(as.integer(tmp$expenditure_total_notification), breaks = as.integer(quantile.interval), dig.lab = 7)

scale_params <- tibble::tibble(ano = 2019)

p <- ggplot(tmp) +
  geom_sf(aes(fill = `Gasto por notificação`), linewidth = 0.1) +
  scale_fill_brewer(palette = 'Oranges', na.value = "lightgray") +
  facet_wrap(~year) +
  geom_sf(data = ufs, fill = NA, linewidth = .8) +
  annotation_north_arrow(data = scale_params,
                         location = "br",
                         which_north = "true",
                         height = unit(1, "cm"),
                         width = unit(1, "cm"),
                         pad_x = unit(0.0, "in"),
                         pad_y = unit(0.2, "in"),
                         style = north_arrow_fancy_orienteering) +
  annotation_scale(data = scale_params) +
  labs(fill="Expenses per notification")+
  theme_bw() + 
  theme(legend.text = element_text(size = 16),
        legend.position = c(.85, .3),
        axis.title = element_blank(),
        axis.text=element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        legend.key.width=unit(2.5,"cm"),
        strip.text = element_text(size = 16))

ggsave("maps/fig2.pdf",  width = 12, height = 7)
ggsave("maps/fig2.png",  width = 12, height = 7, dpi = 300)

# S3 Table. Mean and median share of malaria cases among all notifiable diseases
category <- read_xlsx("data_surveillance_category.xlsx")

tableS3 <- category %>% group_by(tipology) %>% 
  summarise(n = n()/5,
            Average_share = mean(mean, na.rm = TRUE),
            sd = sd(mean, na.rm = TRUE),
            Median_share = median(mean, na.rm = TRUE)) %>% 
  write_excel_csv(., "tables/tabS3.csv")


# Table S4: Malaria expenditures (PPP-US$ 2020) from a public healthcare system perspective and its share on total SUS health expenditure estimated according to SHA accounts, 2015-2019
tableS4 <- data %>% 
  mutate(across(exp_cons:exp_surveillance, ~.x*ppp),
         Insecticide_Bednets = exp_inseticides + exp_LLIN) %>% 
  group_by(year) %>% 
  summarise(cases = sum(cases),
            population= sum(population),
            notifications= sum(notifications),
            Diagnostic_tests = sum(exp_diag, na.rm = TRUE),
            Doctor_appointments = sum(exp_cons, na.rm = TRUE),
            Inpatient_care = sum(exp_hospitalizations, na.rm = TRUE),
            Drugs = sum(exp_med, na.rm = TRUE),
            Insecticide_Bednets = sum(Insecticide_Bednets, na.rm = TRUE),
            Blood_screening = sum(exp_blood, na.rm = TRUE),
            Agents_Microscopists = sum(exp_agents, na.rm = TRUE),
            Surveillance = sum(exp_surveillance, na.rm = TRUE)) %>% 
  transmute(year= year,
            total_expenditures = Diagnostic_tests + Doctor_appointments + Inpatient_care + Drugs + Insecticide_Bednets + Blood_screening + Agents_Microscopists + Surveillance,
            expenditure_total_percapita = total_expenditures/population,
            expenditure_total_notification = total_expenditures/notifications,
            cases=cases) 

# Total expenditure SUS ppp
year<- 2015:2019
expenditureSUS <-  c(119039000000,129319510000, 131289240000, 132873000000,136894700000 )
expenditureSUS <- data.frame(year = year, exp_sus = expenditureSUS)

tableS4 <- tableS4 %>%
  left_join(expenditureSUS, by="year") %>%
  mutate(perc_sus = (total_expenditures/exp_sus)*100) %>% 
  write_excel_csv(., "tables/tabS4.csv")

# Figure S2: Malaria expenditures per capita from public healthcare system perspective between 2015-2019, (PPP-US$ 2020)
tmp <- data %>% 
  mutate(percapita_ppp = (total_expenditures*ppp)/population,
         percapita_ppp_2= ifelse(!is.finite(percapita_ppp), NA, percapita_ppp))


tmp <- tmp %>% 
  mutate(exp_percapita = cut(percapita_ppp_2, 
                                  breaks = c(-Inf, 0, 2, 8, 17, 37, +Inf),
                                  labels = c("0", "< 2", "< 8", "< 17", "< 37", "> 37")))


scale_params <- tibble::tibble(
  ano = 2019
)

map <- tmp %>%
  left_join(municipaly, by = "code_muni") %>% 
  select(-c("abbrev_state"))

map <- st_as_sf(map)


p <- ggplot(map) +
  geom_sf(aes(fill = exp_percapita), linewidth = .1) +
  scale_fill_manual(values = c("white", "#FCCA9B", "#FCA567", "#FC833F", "#E14D1E", "#9B3012")) +
  facet_wrap(~year) +
  geom_sf(data = ufs, fill = NA, linewidth = .8) +
  annotation_north_arrow(data = scale_params,
                         location = "br",
                         which_north = "true",
                         height = unit(1, "cm"),
                         width = unit(1, "cm"),
                         pad_x = unit(0.0, "in"),
                         pad_y = unit(0.2, "in"),
                         style = north_arrow_fancy_orienteering) +
  annotation_scale(data = scale_params) +
  labs(fill="Expenses per capita")+
  theme_bw() + 
  theme(legend.text = element_text(size = 16),
        legend.position = c(.85, .3),
        axis.title = element_blank(),
        axis.text=element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        legend.key.width=unit(2.5,"cm"),
        strip.text = element_text(size = 16))

ggsave("maps/figS2.pdf",  width = 12, height = 7)
ggsave("maps/figS2.png",  width = 12, height = 7, dpi = 300)


# Figure S3: Spatial distribution of total malaria notifications, Brazilian Amazon, 2015-2019


tmp <- map %>% 
  mutate(notifications = ifelse(!is.finite(notifications), NA, notifications),
         notifications = cut(notifications, 
                              breaks = c(-Inf, 0, 3994, 15112, 36200, 92525, +Inf),
                              labels = c("0", "< 3,994", "< 15,112", "< 36,200", "< 92,525", "> 92,525")))



p <- ggplot(tmp) +
  geom_sf(aes(fill = notifications), linewidth = .1) +
  scale_fill_manual(values = c("white", "#FCCA9B", "#FCA567", "#FC833F", "#E14D1E", "#9B3012")) +
  facet_wrap(~year) +
  geom_sf(data = ufs, fill = NA, linewidth = .8) +
  annotation_north_arrow(data = scale_params,
                         location = "br",
                         which_north = "true",
                         height = unit(1, "cm"),
                         width = unit(1, "cm"),
                         pad_x = unit(0.0, "in"),
                         pad_y = unit(0.2, "in"),
                         style = north_arrow_fancy_orienteering) +
  annotation_scale(data = scale_params) +
  labs(fill="Notifications")+
  theme_bw() + 
  theme(legend.text = element_text(size = 16),
        legend.position = c(.85, .3),
        axis.title = element_blank(),
        axis.text=element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        legend.key.width=unit(2.5,"cm"),
        strip.text = element_text(size = 16))


ggsave("maps/figS3.pdf", width = 12, height = 7)
ggsave("maps/figS3.png", width = 12, height = 7, dpi = 300)
