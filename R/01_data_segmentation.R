#' Perform a segmentation analysis on the raw data
#'
#' @param data_SVV raw data (as a \code{data.frame})
#'
#' @return data set after segmentation analysis
#' @export
segmentation_analysis <- function(data_SVV) {
  # Q14
  data_SVV$Q14r1[data_SVV$Q14r1 == 5] <- 1
  data_SVV$Q14r2[data_SVV$Q14r2 == 5] <- 1
  data_SVV$Q14r3[data_SVV$Q14r3 == 5] <- 1
  data_SVV$Q14r4[data_SVV$Q14r4 == 5] <- 1
  data_SVV$Q14r5[data_SVV$Q14r5 == 5] <- 1
  data_SVV$Q14r6[data_SVV$Q14r6 == 5] <- 1
  data_SVV$Q14r7[data_SVV$Q14r7 == 5] <- 1
  data_SVV$Q14r8[data_SVV$Q14r8 == 5] <- 1

  # Q19
  data_SVV$Q19r1[data_SVV$Q19r1 == 5] <- 1
  data_SVV$Q19r2[data_SVV$Q19r2 == 5] <- 1
  data_SVV$Q19r3[data_SVV$Q19r3 == 5] <- 1
  data_SVV$Q19r4[data_SVV$Q19r4 == 5] <- 1
  data_SVV$Q19r5[data_SVV$Q19r5 == 5] <- 1
  data_SVV$Q19r6[data_SVV$Q19r6 == 5] <- 1
  data_SVV$Q19r7[data_SVV$Q19r7 == 5] <- 1
  data_SVV$Q19r8[data_SVV$Q19r8 == 5] <- 1
  data_SVV$Q19r9[data_SVV$Q19r9 == 5] <- 1
  data_SVV$Q19r10[data_SVV$Q19r10 == 5] <- 1
  data_SVV$Q19r11[data_SVV$Q19r11 == 5] <- 1

  ### Sjekk at 5 er borte fra disse variablene
  summary(data_SVV) # riktig
  # Segmenteringsvariabler for Q16 - Kommunikasjon og samhandling
  mydata16 <- recode_q16(data_SVV)
  # Segmenteringsvariabler for Q17 -Informasjonssikkerhet og personvern
  mydata32 <- recode_q17(mydata16)
  ################################################################################
  ##################        Segmenteringsvariabler for        ####################
  ##################         Q14: Bruk av programvare         ####################
  ################################################################################
  ## Diktomisere samtlige variabler - kriterie: må ha svart 3 eller 4 for å få
  ## verdi 1, 0 om annet

  #### Starter med grunnleggende nivå
  ### Q14r1
  mydata33 <- mydata32 %>%
    dplyr::mutate(prog1 = dplyr::if_else(Q14r1 %in% c(3,4), 1, 0))

  as.factor(mydata33$prog1)

  # Sjekk at det ser riktig ut: sammenligne opprinnelig var med dikotom var
  table(data_SVV$Q14r1, useNA = "always")
  table(mydata33$prog1, useNA = "always") # Blir riktig
  #    0    1 <NA>
  #  145 1780    0


  ### Q14r2
  mydata34 <- mydata33 %>%
    dplyr::mutate(prog2 = dplyr::if_else(Q14r2 %in% c(3,4), 1, 0))

  as.factor(mydata34$prog2)
  table(mydata34$prog2, useNA = "always")
  #  0    1 <NA>
  # 33 1892    0

  ### Q14r3
  mydata35 <- mydata34 %>%
    dplyr::mutate(prog3 = dplyr::if_else(Q14r3 %in% c(3,4), 1, 0))

  as.factor(mydata35$prog3)
  table(mydata35$prog3, useNA = "always")
  #    0    1 <NA>
  #  301 1624    0

  ################ Videregående nivå
  ### Q14r4
  mydata36 <- mydata35 %>%
    dplyr::mutate(prog4 = dplyr::if_else(Q14r4 %in% c(3,4), 1, 0))

  as.factor(mydata36$prog4)
  # Sjekk at det ser riktig ut: sammenligne opprinnelig var med dikotom var
  table(data_SVV$Q14r4)
  table(mydata36$prog4, useNA = "always") # Blir riktig
  #    0    1 <NA>
  #  710 1215    0

  ### Q14r5
  mydata37 <- mydata36 %>%
    dplyr::mutate(prog5 = dplyr::if_else(Q14r5 %in% c(3,4), 1, 0))

  as.factor(mydata37$prog5)
  table(mydata37$prog5, useNA = "always")
  #    0    1 <NA>
  # 1470  455    0

  ### Q14r6
  mydata38 <- mydata37 %>%
    dplyr::mutate(prog6 = dplyr::if_else(Q14r6 %in% c(3,4), 1, 0))

  as.factor(mydata38$prog6)
  table(mydata38$prog6, useNA = "always")
  #   0    1 <NA>
  # 786 1139    0


  ############## Avansert nivå

  ### Q14r7
  mydata39 <- mydata38 %>%
    dplyr::mutate(prog7 = dplyr::if_else(Q14r7 %in% c(3,4), 1, 0))

  as.factor(mydata39$prog7)
  table(mydata39$prog7, useNA = "always")
  #     0    1 <NA>
  #  1370  555    0


  ### Q14r8
  mydata40 <- mydata39 %>%
    dplyr::mutate(prog8 = dplyr::if_else(Q14r8 %in% c(3,4), 1, 0))

  as.factor(mydata40$prog8)
  table(mydata40$prog8, useNA = "always")
  #    0    1 <NA>
  # 1528  397   0

  ################################### GENERERE SEGMENT FOR HVERT NIVÅ
  ################# Generere samlet variabel for grunnleggende prog ##############

  #### Generere ny variabel som tar summen av de 3 variablene i grunnleggende
  mydata41 <- mydata40 %>%
    dplyr::mutate(grunnleg_prog_sum = rowSums(dplyr::select(., prog1, prog2, prog3)))

  table(mydata41$grunnleg_prog_sum, useNA = "always")
  #    0    1    2    3 <NA>
  #   15   58  318 1534    0

  #### Generere ny variabel som sier at score på grunnlegg_prog må være 2 eller
  #### høyere, så verdi = 1, 0 hvis ikke
  mydata42 <- mydata41 %>%
    dplyr::mutate(grunn_prog1 = dplyr::if_else(grunnleg_prog_sum >= 2, 1, 0))

  as.factor(mydata42$grunn_prog1)
  table(mydata42$grunn_prog1, useNA = "always")

  ######################## Lage en samlet variabel for videregående ##############
  # Først summere variablene i videregående

  mydata43 <- mydata42 %>%
    dplyr::mutate(videre_prog_sum = rowSums(dplyr::select(., prog4, prog5, prog6)))

  table(mydata43$videre_prog_sum, useNA = "always")
  #    0    1    2    3 <NA>
  #  491  422  649  363    0

  #### Generere ny variabel som sier at score på videre_info_sum må være 3, 0 hvis ikke

  mydata44 <- mydata43 %>%
    dplyr::mutate(videre_prog = dplyr::if_else(videre_prog_sum == 3, 1, 0))

  as.factor(mydata44$videre_prog)
  table(mydata44$videre_prog)

  ################### Lage en samlet variabel for avansert #######################
  mydata45 <- mydata44 %>%
    dplyr::mutate(avan_prog = dplyr::if_else(prog7 == 1 & prog8 == 1, 1, 0))

  as.factor(mydata45$avan_prog)
  table(mydata45$avan_prog, useNA = "always")
  #    0    1 <NA>
  # 1640  285    0

  #### Koding av kat_programmer1
  mydata46 <- mydata45 %>%
    dplyr::mutate(kat_programmer1 = dplyr::case_when(
      (grunn_prog1 == 0) ~ 0L,
      (grunn_prog1 == 1 & videre_prog == 0 & avan_prog == 0) ~ 1L,
      (grunn_prog1 == 1 & videre_prog == 1 & avan_prog == 0 |
         grunn_prog1 == 1 & videre_prog == 0 & avan_prog == 1) ~ 2L,
      (grunn_prog1 == 1 & videre_prog == 1 & avan_prog == 1) ~ 3L,
      TRUE ~ NA_integer_))

  table(mydata46$kat_programmer1, useNA = "always")
  #   0    1    2    3 <NA>
  #  73 1377  303  172    0



  mydata46$kat_programmer1 <- factor(mydata46$kat_programmer1, levels = c(0,1,2,3),
                                     labels = c("Uerfaren", "Grunnleggende", "Videregående",
                                                "Avansert"))

  ################################################################################
  ##################        Segmenteringsvariabler for        ####################
  ##################         Q19: Bruk av teknologi           ####################
  ################################################################################

  ## Diktomisere samtlige variabler - kriterie: må ha svart 3 eller 4 for å få
  ## verdi 1, 0 om annet

  #### Starter med grunnleggende nivå
  ### Q19r1
  mydata47 <- mydata46 %>%
    dplyr::mutate(utstyr1 = dplyr::if_else(Q19r1 %in% c(3,4), 1, 0))

  as.factor(mydata47$utstyr1)

  # Sjekk at det ser riktig ut: sammenligne opprinnelig var med dikotom var
  table(data_SVV$Q19r1, useNA = "always")
  table(mydata47$utstyr1, useNA = "always") # Blir riktig
  #    0    1 <NA>
  # 197 1728    0


  ### Q19r2
  mydata48 <- mydata47 %>%
    dplyr::mutate(utstyr2 = dplyr::if_else(Q19r2 %in% c(3,4), 1, 0))

  as.factor(mydata48$utstyr2)
  table(mydata48$utstyr2, useNA = "always")
  #   0    1 <NA>
  # 122 1803    0


  ### Q19r3
  mydata49 <- mydata48 %>%
    dplyr::mutate(utstyr3 = dplyr::if_else(Q19r3 %in% c(3,4), 1, 0))

  as.factor(mydata49$utstyr3)
  table(mydata49$utstyr3, useNA = "always")
  #   0    1 <NA>
  # 243 1682    0

  ################ Videregående nivå
  ### Q19r4
  mydata50 <- mydata49 %>%
    dplyr::mutate(utstyr4 = dplyr::if_else(Q19r4 %in% c(3,4), 1, 0))

  as.factor(mydata50$utstyr4)
  # Sjekk at det ser riktig ut: sammenligne opprinnelig var med dikotom var
  table(data_SVV$Q19r4)
  table(mydata50$utstyr4, useNA = "always") # Blir riktig
  #    0    1 <NA>
  #  166 1759    0


  ### Q19r5
  mydata51 <- mydata50 %>%
    dplyr::mutate(utstyr5 = dplyr::if_else(Q19r5 %in% c(3,4), 1, 0))

  as.factor(mydata51$utstyr5)
  table(mydata51$utstyr5, useNA = "always")
  #    0    1 <NA>
  #  593 1332    0


  ### Q19r6
  mydata52 <- mydata51 %>%
    dplyr::mutate(utstyr6 = dplyr::if_else(Q19r6 %in% c(3,4), 1, 0))

  as.factor(mydata52$utstyr6)
  table(mydata52$utstyr6, useNA = "always")
  #    0    1 <NA>
  #  676 1249    0


  ### Q19r7
  mydata53 <- mydata52 %>%
    dplyr::mutate(utstyr7 = dplyr::if_else(Q19r7 %in% c(3,4), 1, 0))

  as.factor(mydata53$utstyr7)
  table(mydata53$utstyr7, useNA = "always")
  #    0    1 <NA>
  #  138 1787    0


  ### Q19r8
  mydata54 <- mydata53 %>%
    dplyr::mutate(utstyr8 = dplyr::if_else(Q19r8 %in% c(3,4), 1, 0))

  as.factor(mydata54$utstyr8)
  table(mydata54$utstyr8, useNA = "always")
  #    0    1 <NA>
  #  482 1443    0

  ############### AVANSERT NIVÅ
  ### Q19r9
  mydata55 <- mydata54 %>%
    dplyr::mutate(utstyr9 = dplyr::if_else(Q19r9 %in% c(3,4), 1, 0))

  as.factor(mydata55$utstyr9)
  table(mydata55$utstyr9, useNA = "always")
  #    0    1 <NA>
  # 1515  410    0


  ### Q19r10
  mydata56 <- mydata55%>%
    dplyr::mutate(utstyr10 = dplyr::if_else(Q19r10 %in% c(3,4), 1, 0))

  as.factor(mydata56$utstyr10)
  table(mydata56$utstyr10, useNA = "always")
  #    0    1 <NA>
  # 1820  105    0


  ### Q19r11
  mydata57 <- mydata56 %>%
    dplyr::mutate(utstyr11 = dplyr::if_else(Q19r11 %in% c(3,4), 1, 0))

  as.factor(mydata57$utstyr11)
  table(mydata57$utstyr11, useNA = "always")
  #    0    1 <NA>
  # 1664  261    0

  ################################### GENERERE SEGMENT FOR HVERT NIVÅ
  ################# Generere samlet variabel for grunnleggende prog ##############

  #### Generere ny variabel som tar summen av de 3 variablene i grunnleggende
  mydata58 <- mydata57 %>%
    dplyr::mutate(grunnleg_utstyr_sum = rowSums(dplyr::select(., utstyr1, utstyr2, utstyr3)))

  table(mydata58$grunnleg_utstyr_sum, useNA = "always")
  #    0    1    2    3 <NA>
  #   27   94  293 1511    0

  #### Generere ny variabel som sier at score på grunnlegg_utstyr må være 2 eller
  #### høyere, så verdi = 1, 0 hvis ikke
  mydata59 <- mydata58 %>%
    dplyr::mutate(grunn_utstyr1 = dplyr::if_else(grunnleg_utstyr_sum >= 2, 1, 0))

  as.factor(mydata59$grunn_utstyr1)
  table(mydata59$grunn_utstyr1, useNA = "always")
  #    0    1 <NA>
  #  121 1804    0

  ######################## Lage en samlet variabel for videregående ##############
  # Først summere variablene i videregående

  mydata60 <- mydata59 %>%
    dplyr::mutate(videre_utstyr_sum = rowSums(dplyr::select(., utstyr4, utstyr5, utstyr6,
                                              utstyr7, utstyr8)))

  table(mydata60$videre_utstyr_sum, useNA = "always")
  #   0    1    2    3    4    5 <NA>
  #  27   78  183  312  435  890    0

  #### Generere ny variabel som sier at score på videre_info_sum må være 5, 0 hvis ikke

  mydata61 <- mydata60 %>%
    dplyr::mutate(videre_utstyr = dplyr::if_else(videre_utstyr_sum == 5, 1, 0))

  as.factor(mydata61$videre_utstyr)
  table(mydata61$videre_utstyr, useNA = "always")
  #     0    1 <NA>
  #  1035  890    0

  ################### Lage en samlet variabel for avansert #######################
  mydata62 <- mydata61 %>%
    dplyr::mutate(avan_utstyr = dplyr::if_else(utstyr9 == 1 & utstyr10 == 1 & utstyr11 == 1, 1, 0))

  as.factor(mydata62$avan_utstyr)
  table(mydata62$avan_utstyr, useNA = "always")
  #     0    1  <NA>
  #  1888   37    0     # kan ikke ha mindre strenge kriterier på denne

  #################### samlet kategorisk variabel for Q14 ########################

  #### Koding av kat_utstyr1
  mydata63 <- mydata62 %>%
    dplyr::mutate(kat_utstyr1 = dplyr::case_when(
      (grunn_utstyr1 == 0) ~ 0L,
      (grunn_utstyr1 == 1 & videre_utstyr == 0 & avan_utstyr == 0) ~ 1L,
      (grunn_utstyr1 == 1 & videre_utstyr == 1 & avan_utstyr == 0 |
         grunn_utstyr1 == 1 & videre_utstyr == 0 & avan_utstyr == 1) ~ 2L,
      (grunn_utstyr1 == 1 & videre_utstyr == 1 & avan_utstyr == 1) ~ 3L,
      TRUE ~ NA_integer_))

  table(mydata63$kat_utstyr1, useNA = "always")
  #    0    1    2    3 <NA>
  #  121  911  861   32    0

  mydata63$kat_utstyr1 <- factor(mydata63$kat_utstyr1, levels = c(0,1,2,3),
                                 labels = c("Uerfaren", "Grunnleggende", "Videregående",
                                            "Avansert"))

  return(data_segmentation = mydata63)
}
