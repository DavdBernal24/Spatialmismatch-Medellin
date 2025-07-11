### Project: Better or worse job accessibility? Understanding changes 
#in spatial mismatch at the intra-urban level: evidence from  Medell�n, Colombia
### Authors: David Bernal; Gustavo Garcia; Jorge Perez
### Code created by : Gustavo Garcia
### Last modification : 
### Modified by: 



#### This code organizes GEIH data to get proportion by commune


#Required libraries


library(readr); library(summarytools); library(tidyverse); library(writexl)
library(moder)


## Working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

rm(list = ls())



##############################################################################

#-------------------------------------------------------------#
#        1. Loading Data                                      #  
#-------------------------------------------------------------#                                                             

geih11 <- read_delim("Data/GEIH_Data/Microdato Gran Encuesta Integrada de Hogares 2011.txt", 
                     delim = "\t", escape_double = FALSE,
                     col_types = cols(FEX_C = col_character()),
                     trim_ws = TRUE) |> 
  mutate(FEX_C = as.numeric(sub(",", ".", FEX_C)))

geih17 <- read_delim("Data/GEIH_Data/Microdato Gran Encuesta Integrada de Hogares 2017.txt", 
                   delim = "\t", escape_double = FALSE,
                   col_types = cols(FEX_C = col_character()),
                   trim_ws = TRUE) |> 
  mutate(FEX_C = as.numeric(sub(",", ".", FEX_C)))

# Population: pop
geih11 <- geih11 |> mutate(x=1)
x <- ctable(x = geih11$comuna, 
       y = geih11$x,
       prop = "n",
       weights = geih11$FEX_C)

pop11 <- data.frame(x[["cross_table"]])|> 
  filter(x=="1" & !comuna=="Total") |> 
  select(Freq) |> 
  rename(pop11=Freq)

rm(x)

geih17 <- geih17 |> mutate(x=1)
x <-  ctable(x=geih17$comuna, 
       geih17$x,
       prop = "n",
       weights = geih17$FEX_C)

pop17 <- data.frame(x[["cross_table"]])|> 
  filter(x=="1" & !comuna=="Total")|> 
  select(comuna, Freq) |> 
  rename(pop17=Freq)

rm(x)

# Gender: Men (1) - Women (2) (P6020)
freq(geih11$P6020)
freq(geih17$P6020, weights = geih17$FEX_C)

x <- ctable(x=geih11$comuna, 
            geih11$P6020,
            prop = "n",
            weights = geih11$FEX_C)

men11 <- data.frame(x[["cross_table"]]) |> 
  filter(P6020=="1" & !comuna=="Total")|> 
  select(Freq) |> 
  rename(men11=Freq)

rm(x)

x <- ctable(x=geih11$comuna, 
            geih11$P6020,
            prop = "n",
            weights = geih11$FEX_C)

women11 <- data.frame(x[["cross_table"]])|> 
  filter(P6020=="2" & !comuna=="Total")|> 
  select(Freq) |> 
  rename(women11=Freq)

rm(x)

x <- ctable(x=geih17$comuna, 
            geih17$P6020,
            prop = "n",
            weights = geih17$FEX_C)

men17 <- data.frame(x[["cross_table"]]) |> 
  filter(P6020=="1" & !comuna=="Total")|> 
  select(Freq) |> 
  rename(men17=Freq)

rm(x)

x <- ctable(x=geih17$comuna, 
            geih17$P6020,
            prop = "n",
            weights = geih17$FEX_C)

women17 <- data.frame(x[["cross_table"]])|> 
  filter(P6020=="2" & !comuna=="Total")|> 
  select(Freq) |> 
  rename(women17=Freq)

rm(x)

# Age (P6040)
freq(geih11$P6040)
freq(geih17$P6040)

geih11 <- geih11 |> 
  mutate(age_g = case_when(P6040>=0 & P6040<=5 ~ 1,
                            P6040>=6 & P6040<=10 ~ 2,
                            P6040>=11 & P6040<=15 ~ 3,
                            P6040>=16 & P6040<=17 ~ 4,
                            P6040>=18 & P6040<=24 ~ 5,
                            P6040>=25 & P6040<=40 ~ 6,
                            P6040>=41 & P6040<=65 ~ 7,
                            P6040>65 ~ 8))

geih17 <- geih17 |> 
  mutate(age_g = case_when(P6040>=0 & P6040<=5 ~ 1,
                            P6040>=6 & P6040<=10 ~ 2,
                            P6040>=11 & P6040<=15 ~ 3,
                            P6040>=16 & P6040<=17 ~ 4,
                            P6040>=18 & P6040<=24 ~ 5,
                            P6040>=25 & P6040<=40 ~ 6,
                            P6040>=41 & P6040<=65 ~ 7,
                            P6040>65 ~ 8))

freq(geih11$age_g)
freq(geih17$age_g)

x <- ctable(x=geih11$comuna, 
            geih11$age_g,
            prop = "n",
            weights = geih11$FEX_C)

age_g11 <- data.frame(x[["cross_table"]])|> 
  filter(!age_g=="Total" & !comuna=="Total")|> 
  rename(age_=Freq) |> 
  mutate(age_g = case_when(age_g==1 ~ "age_0_511",
                           age_g==2 ~ "age_6_1011",
                           age_g==3 ~ "age_11_1511",
                           age_g==4 ~ "age_16_1711",
                           age_g==5 ~ "age_18_2411",
                           age_g==6 ~ "age_25_4011",
                           age_g==7 ~ "age_41_6511",
                           age_g==8 ~ "age_65m11")) |> 
  pivot_wider(names_from = age_g, values_from = age_) |> 
  select(!comuna)

rm(x)

x <- ctable(x=geih17$comuna, 
            geih17$age_g,
            prop = "n",
            weights = geih17$FEX_C)

age_g17 <- data.frame(x[["cross_table"]])|> 
  filter(!age_g=="Total" & !comuna=="Total")|> 
  rename(age_=Freq) |> 
  mutate(age_g = case_when(age_g==1 ~ "age_0_517",
                           age_g==2 ~ "age_6_1017",
                           age_g==3 ~ "age_11_1517",
                           age_g==4 ~ "age_16_1717",
                           age_g==5 ~ "age_18_2417",
                           age_g==6 ~ "age_25_4017",
                           age_g==7 ~ "age_41_6517",
                           age_g==8 ~ "age_65m17")) |> 
  pivot_wider(names_from = age_g, values_from = age_)|> 
  select(!comuna)

rm(x)

# Employees
freq(geih11$OCI, weights = geih11$FEX_C)
freq(geih17$OCI, weights = geih17$FEX_C)

x <- ctable(x=geih11$comuna, 
            geih11$OCI,
            prop = "n",
            weights = geih11$FEX_C)

employees11 <- data.frame(x[["cross_table"]])|> 
  filter(OCI=="1" & !comuna=="Total") |> 
  select(Freq) |> 
  rename(employees11=Freq)

rm(x)

x <- ctable(x=geih17$comuna, 
            geih17$OCI,
            prop = "n",
            weights = geih17$FEX_C)

employees17 <- data.frame(x[["cross_table"]])|> 
  filter(OCI=="1" & !comuna=="Total") |> 
  select(Freq) |> 
  rename(employees17=Freq)

rm(x)

# Labor income (INGLABO)
labor_income11 <- geih11 |> group_by(comuna) |> 
  dplyr::summarize(laborincome11 = datawizard::weighted_mean(INGLABO, 
                                                            weights = FEX_C)) |> 
  select(laborincome11)

labor_income17 <- geih17 |> group_by(comuna) |> 
  dplyr::summarize(laborincome17 = datawizard::weighted_mean(INGLAB, 
                                                            weights = FEX_C))|> 
  select(laborincome17)

# Estrato para tarifa de energia electrica (Economic stratification)
freq(geih11$P4030S1A1)
freq(geih17$P4030S1A1)

geih11 <- geih11 |> 
  mutate(estrato = case_when(P4030S1A1 %in% c(0,9) ~ NA,
                             TRUE ~ P4030S1A1))

geih17 <- geih17 |> 
  mutate(estrato = case_when(P4030S1A1 %in% c(0,9) ~ NA,
                             TRUE ~ P4030S1A1))

freq(geih11$estrato)
freq(geih17$estrato)

estrato_moda11 <- geih11 |> group_by(comuna) |> 
  dplyr::summarize(estratomoda11 = mode_first(estrato, na.rm = TRUE))|> 
  select(estratomoda11)

estrato_moda17 <- geih17 |> group_by(comuna) |> 
  dplyr::summarize(estratomoda17 = mode_first(estrato, na.rm = TRUE))|> 
  select(estratomoda17)


df <- cbind(pop11, pop17, 
            men11, men17, women11, women17,
            age_g11, age_g17,
            employees11, employees17,
            labor_income11, labor_income17,
            estrato_moda11, estrato_moda17)|> 
  relocate(comuna)|> 
  pivot_longer(
  cols = -comuna,
  names_to = c(".value", "year"),
  names_pattern = "(.+)?(\\d{2})$") |> 
  arrange(year, comuna) |> 
  mutate(pmen=(men/pop)*100,
         pwomen=(women/pop)*100,
         page_0_5=(age_0_5/pop)*100,
         page_6_10=(age_6_10/pop)*100,
         page_11_15=(age_11_15/pop)*100,
         page_16_17=(age_16_17/pop)*100,
         page_18_24=(age_18_24/pop)*100,
         page_25_40=(age_25_40/pop)*100,
         page_41_65=(age_41_65/pop)*100,
         page_65m=(age_65m/pop)*100) |>
  mutate(name_comuna = case_when(comuna=="1"	~ "Popular",
                                 comuna=="2"  ~ "Santa Cruz",
                                 comuna=="3"	~ "Manrique",
                                 comuna=="4"	~ "Aranjuez",
                                 comuna=="5"	~ "Castilla",
                                 comuna=="6"	~ "Doce de Octubre",
                                 comuna=="7"	~ "Robledo",
                                 comuna=="8"	~ "Villa Hermosa",
                                 comuna=="9"	~ "Buenos Aires",
                                 comuna=="10"	~ "La Candelaria",
                                 comuna=="11"	~ "Laureles—Estadio",
                                 comuna=="12"	~ "La América",
                                 comuna=="13"	~ "San Javier",
                                 comuna=="14"	~ "Poblado",
                                 comuna=="15"	~ "Guayabal",
                                 comuna=="16"	~ "Belén"),
         year = case_when(year=="11" ~ 2011, year=="17" ~ 2017)) |> 
  relocate(comuna, name_comuna, year, pop, men, women, pmen, pwomen,
           age_0_5, age_6_10, age_11_15, age_16_17, age_18_24, age_25_40, age_41_65, age_65m,
           page_0_5, page_6_10, page_11_15, page_16_17, page_18_24, page_25_40, page_41_65, page_65m) 
 
write_xlsx(df,"Base/MedXcomunaGEIH2011_2017.xlsx") 


