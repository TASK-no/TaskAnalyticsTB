#' Perform a segmentation analysis on the raw data
#'
#' @param data_SVV raw data (as a \code{data.frame})
#'
#' @return data set after segmentation analysis
#' @export
segmentation_analysis <- function(data_SVV) {
  #### Koding av kat_kommunikasjon
  #### Setter verdien 5 til 1 på ALLE indikatorvariabler
  # Q16
  data_SVV$Q16r1[data_SVV$Q16r1 == 5] <- 1
  data_SVV$Q16r2[data_SVV$Q16r2 == 5] <- 1
  data_SVV$Q16r3[data_SVV$Q16r3 == 5] <- 1
  data_SVV$Q16r4[data_SVV$Q16r4 == 5] <- 1
  data_SVV$Q16r5[data_SVV$Q16r5 == 5] <- 1
  data_SVV$Q16r6[data_SVV$Q16r6 == 5] <- 1
  data_SVV$Q16r7[data_SVV$Q16r7 == 5] <- 1
  data_SVV$Q16r8[data_SVV$Q16r8 == 5] <- 1
  data_SVV$Q16r9[data_SVV$Q16r9 == 5] <- 1
  data_SVV$Q16r10[data_SVV$Q16r10 == 5] <- 1
  data_SVV$Q16r11[data_SVV$Q16r11 == 5] <- 1

  # Q17
  data_SVV$Q17r1[data_SVV$Q17r1 == 5] <- 1
  data_SVV$Q17r2[data_SVV$Q17r2 == 5] <- 1
  data_SVV$Q17r3[data_SVV$Q17r3 == 5] <- 1
  data_SVV$Q17r4[data_SVV$Q17r4 == 5] <- 1
  data_SVV$Q17r5[data_SVV$Q17r5 == 5] <- 1
  data_SVV$Q17r6[data_SVV$Q17r6 == 5] <- 1
  data_SVV$Q17r7[data_SVV$Q17r7 == 5] <- 1
  data_SVV$Q17r8[data_SVV$Q17r8 == 5] <- 1
  data_SVV$Q17r9[data_SVV$Q17r9 == 5] <- 1
  data_SVV$Q17r10[data_SVV$Q17r10 == 5] <- 1

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


  ################################################################################
  ##################        Segmenteringsvariabler for        ####################
  ##################    Q16: Kommunikasjon og samhandling     ####################
  ################################################################################

  ## Lage nye dikotome variabler hvor verdi 3 og 4 på indikator får verdi 1, og 0 annet

  #### Starter med grunnleggende nivå
  ### Q16r1
  mydata <- data_SVV %>%
    dplyr::mutate(kom1 = dplyr::if_else(Q16r1 %in% c(3,4), 1, 0))

  as.factor(mydata$kom1)

  # Sjekk at det ser riktig ut: sammenligne opprinnelig var med dikotom var
  table(data_SVV$Q16r1, useNA = "always")
  table(mydata$kom1, useNA = "always") # Blir riktig

  ### Q16r2
  mydata1 <- mydata %>%
    dplyr::mutate(kom2 = dplyr::if_else(Q16r2 %in% c(3,4), 1, 0))

  as.factor(mydata1$kom2)

  ### Q16r3
  mydata2 <- mydata1 %>%
    dplyr::mutate(kom3 = dplyr::if_else(Q16r3 %in% c(3,4), 1, 0))

  as.factor(mydata2$kom3)

  ### Q16r4
  mydata3 <- mydata2 %>%
    dplyr::mutate(kom4 = dplyr::if_else(Q16r4 %in% c(3,4), 1, 0))

  as.factor(mydata3$kom4)

  ### Q16r5
  mydata4 <- mydata3 %>%
    dplyr::mutate(kom5 = dplyr::if_else(Q16r5 %in% c(3,4), 1, 0))

  as.factor(mydata4$kom5)

  ### Q16r6
  mydata5 <- mydata4 %>%
    dplyr::mutate(kom6 = dplyr::if_else(Q16r6 %in% c(3,4), 1, 0))

  as.factor(mydata5$kom6)

  ################ Videregående nivå
  ### Q16r7
  mydata6 <- mydata5 %>%
    dplyr::mutate(kom7 = dplyr::if_else(Q16r7 %in% c(3,4), 1, 0))

  as.factor(mydata6$kom7)

  # Sjekk at det ser riktig ut: sammenligne opprinnelig var med dikotom var
  table(data_SVV$Q16r7)
  table(mydata6$kom7) # Blir riktig

  ### Q16r8
  mydata7 <- mydata6 %>%
    dplyr::mutate(kom8 = dplyr::if_else(Q16r8 %in% c(3,4), 1, 0))

  as.factor(mydata7$kom8)
  table(mydata7$kom8, useNA = "always")


  ### Q16r9
  mydata8 <- mydata7 %>%
    dplyr::mutate(kom9 = dplyr::if_else(Q16r9 %in% c(3,4), 1, 0))

  as.factor(mydata8$kom9)
  table(mydata8$kom9, useNA = "always")


  ############## Avansert nivå
  ### Q16r10
  mydata9 <- mydata8%>%
    dplyr::mutate(kom10 = dplyr::if_else(Q16r10 %in% c(3,4), 1, 0))

  as.factor(mydata9$kom10)
  table(mydata9$kom10, useNA = "always")
  #    0    1 <NA>
  # 1334  591    0

  ### Q16r11
  mydata10 <- mydata9 %>%
    dplyr::mutate(kom11 = dplyr::if_else(Q16r11 %in% c(3,4), 1, 0))

  as.factor(mydata10$kom11)
  table(mydata10$kom11, useNA = "always")
  #    0    1 <NA>
  # 1697  228    0


  #### Generere ny variabel som tar summen av de 6 variablene i grunnleggende
  mydata11 <- mydata10 %>%
    dplyr::mutate(grunnleg_kom_sum = rowSums(dplyr::select(., kom1, kom2, kom3, kom4, kom5, kom6)))

  table(mydata11$grunnleg_kom_sum, useNA = "always")
  # table(mydata10$grunnleg_kom_sum, useNA = "always")
  #   0    1    2    3    4    5    6 <NA>
  #  39  101  132  196  437  487  533    0

  #### Generere ny variabel som sier at dersom score på grunnlegg_kom er 4 eller
  #### høyere, så verdi = 1, 0 hvis ikke

  mydata12 <- mydata11 %>%
    dplyr::mutate(grunn_kom = dplyr::if_else(grunnleg_kom_sum >= 4, 1, 0))

  as.factor(mydata12$grunn_kom)

  table(mydata12$grunn_kom, useNA = "always")
  #    0    1 <NA>
  #  468 1457    0

  ### Lage en samlet variabel for videregående
  # Prøve en for videregående med mindre strenge kriterier:
  # Lage en hvor du må ha svart 3/4 på 2 av 3 for å havne i kategorien

  mydata13 <- mydata12 %>%
    dplyr::mutate(videre_kom_sum = rowSums(dplyr::select(., kom7, kom8, kom9)))

  table(mydata13$videre_kom_sum, useNA = "always")
  #    0    1    2    3 <NA>
  #  971  499  279  176    0

  #### Generere ny variabel som sier at dersom score på videre_kom er 2 eller
  #### høyere, så verdi = 1, 0 hvis ikke

  mydata14 <- mydata13 %>%
    dplyr::mutate(videre_kom = dplyr::if_else(videre_kom_sum >= 2, 1, 0))

  as.factor(mydata14$videre_kom)
  table(mydata14$videre_kom, useNA = "always")
  #    0    1 <NA>
  # 1470  455    0

  ### Lage en samlet variabel for avansert
  mydata15 <- mydata14 %>%
    dplyr::mutate(avan_kom = dplyr::if_else(kom10 == 1 & kom11 == 1, 1, 0))

  as.factor(mydata15$avan_kom)
  table(mydata15$avan_kom, useNA = "always")
  #    0    1 <NA>
  # 1773  152    0

  #################### samlet kategorisk variabel for Q16 ########################

  #### Dette er riktig måte å gjøre det på
  mydata16 <- mydata15 %>%
    dplyr::mutate(kat_kommunikasjon = dplyr::case_when(
      (grunn_kom == 0) ~ 0L,
      (grunn_kom == 1 & videre_kom == 0 & avan_kom == 0) ~ 1L,
      (grunn_kom == 1 & videre_kom == 1 & avan_kom == 0 |
         grunn_kom == 1 & videre_kom == 0 & avan_kom == 1) ~ 2L,
      (grunn_kom == 1 & videre_kom == 1 & avan_kom == 1) ~ 3L,
      TRUE ~ NA_integer_))

  table(mydata16$kat_kommunikasjon, useNA = "always")
  #    0    1    2    3 <NA>
  #  468  965  383  109   0

  mydata16$kat_kommunikasjon <- factor(mydata16$kat_kommunikasjon, levels = c(0,1,2,3),
                                       labels = c("Uerfaren", "Grunnleggende", "Videregående",
                                                  "Avansert"))


  # Eksportere dataframe til SPSS fil for Vegard:
  #library(haven) # loade relevant pakke

  #write_sav(mydata16, "kat_kommunikasjon_data_SVV.sav")



  ################################################################################
  ##################        Segmenteringsvariabler for        ####################
  ################## Q17: Informasjonssikkerhet og personvern ####################
  ################################################################################

  ## Diktomisere samtlige variabler - kriterie: må ha svart 3 eller 4 for å få
  ## verdi 1, 0 om annet

  #### Starter med grunnleggende nivå
  ### Q17r1
  mydata17 <- mydata16 %>%
    dplyr::mutate(info1 = dplyr::if_else(Q17r1 %in% c(3,4), 1, 0))

  as.factor(mydata17$info1)

  # Sjekk at det ser riktig ut: sammenligne opprinnelig var med dikotom var
  table(data_SVV$Q17r1, useNA = "always")
  table(mydata17$info1, useNA = "always") # Blir riktig

  ### Q17r2
  mydata18 <- mydata17 %>%
    dplyr::mutate(info2 = dplyr::if_else(Q17r2 %in% c(3,4), 1, 0))

  as.factor(mydata18$info2)

  ### Q17r3
  mydata19 <- mydata18 %>%
    dplyr::mutate(info3 = dplyr::if_else(Q17r3 %in% c(3,4), 1, 0))

  as.factor(mydata19$info3)

  ### Q17r4
  mydata20 <- mydata19 %>%
    dplyr::mutate(info4 = dplyr::if_else(Q17r4 %in% c(3,4), 1, 0))

  as.factor(mydata20$info4)


  ################ Videregående nivå
  ### Q17r5
  mydata21 <- mydata20 %>%
    dplyr::mutate(info5 = dplyr::if_else(Q17r5 %in% c(3,4), 1, 0))

  as.factor(mydata21$info5)

  # Sjekk at det ser riktig ut: sammenligne opprinnelig var med dikotom var
  table(data_SVV$Q17r5)
  table(mydata21$info5, useNA = "always") # Blir riktig
  #    0    1 <NA>
  # 1461  464   0


  ### Q17r6
  mydata22 <- mydata21 %>%
    dplyr::mutate(info6 = dplyr::if_else(Q17r6 %in% c(3,4), 1, 0))

  as.factor(mydata22$info6)
  table(mydata22$info6, useNA = "always")
  #   0    1 <NA>
  # 983  942    0


  ### Q17r7
  mydata23 <- mydata22 %>%
    dplyr::mutate(info7 = dplyr::if_else(Q17r7 %in% c(3,4), 1, 0))

  as.factor(mydata23$info7)
  table(mydata23$info7, useNA = "always")
  #    0    1 <NA>
  #  682 1243    0



  ############## Avansert nivå
  ### Q17r8
  mydata24 <- mydata23 %>%
    dplyr::mutate(info8 = dplyr::if_else(Q17r8 %in% c(3,4), 1, 0))

  as.factor(mydata24$info8)
  table(mydata24$info8, useNA = "always")
  #    0    1 <NA>
  # 1511  414    0


  ### Q17r9
  mydata25 <- mydata24 %>%
    dplyr::mutate(info9 = dplyr::if_else(Q17r9 %in% c(3,4), 1, 0))

  as.factor(mydata25$info9)
  table(mydata25$info9, useNA = "always")
  #    0    1 <NA>
  # 1459  466    0


  ### Q17r10
  mydata26 <- mydata25 %>%
    dplyr::mutate(info10 = dplyr::if_else(Q17r10 %in% c(3,4), 1, 0))

  as.factor(mydata26$info10)
  table(mydata26$info10, useNA = "always")
  #     0    1 <NA>
  #  1420  505    0



  ################## Generere samlet variabel for grunnleggende info #############

  #### Generere ny variabel som tar summen av de 4 variablene i grunnleggende
  mydata27 <- mydata26 %>%
    dplyr::mutate(grunnleg_info_sum = rowSums(dplyr::select(., info1, info2, info3, info4)))

  table(mydata27$grunnleg_info_sum, useNA = "always")
  #   0    1    2    3    4 <NA>
  #  66  168  348  625  718    0

  #### Generere ny variabel som sier at score på grunnlegg_info må være 3 eller
  #### høyere, så verdi = 1, 0 hvis ikke
  mydata28 <- mydata27 %>%
    dplyr::mutate(grunn_info1 = dplyr::if_else(grunnleg_info_sum >= 3, 1, 0))

  as.factor(mydata28$grunn_info1)
  table(mydata28$grunn_info1)
  #    0    1
  #  582 1343

  ######################## Lage en samlet variabel for videregående ##############
  # Lage en hvor du må ha svart 3/4 på 2 av 3 for å havne i kategorien (likt som i kom)

  mydata29 <- mydata28 %>%
    dplyr::mutate(videre_info_sum = rowSums(dplyr::select(., info5, info6, info7)))

  table(mydata29$videre_info_sum, useNA = "always")
  #    0    1    2    3 <NA>
  #  483  584  509  349    0

  #### Generere ny variabel som sier at score på videre_info_sum må være 3, 0 hvis ikke

  mydata30 <- mydata29 %>%
    dplyr::mutate(videre_info = dplyr::if_else(videre_info_sum == 3, 1, 0))

  as.factor(mydata30$videre_info)
  table(mydata30$videre_info)

  ################### Lage en samlet variabel for avansert #######################
  mydata31 <- mydata30 %>%
    dplyr::mutate(avan_info = dplyr::if_else(info8 == 1 & info9 == 1 & info10 == 1, 1, 0))

  as.factor(mydata31$avan_info)
  table(mydata31$avan_info, useNA = "always")
  #    0    1 <NA>
  # 1756  169   0     # ser plausibelt ut og samsvarer ish med avan på kom


  #################### samlet kategorisk variabel for Q17 ########################
  #### Koding av kat_informasjon1
  mydata32 <- mydata31 %>%
    dplyr::mutate(kat_informasjon1 = dplyr::case_when(
      (grunn_info1 == 0) ~ 0L,
      (grunn_info1 == 1 & videre_info == 0 & avan_info == 0) ~ 1L,
      (grunn_info1 == 1 & videre_info == 1 & avan_info == 0 |
         grunn_info1 == 1 & videre_info == 0 & avan_info == 1) ~ 2L,
      (grunn_info1 == 1 & videre_info == 1 & avan_info == 1) ~ 3L,
      TRUE ~ NA_integer_))

  table(mydata32$kat_informasjon1, useNA = "always")
  #   0    1    2    3 <NA>
  # 582  974  235  134   0          # ser ut til å være plausibel


  mydata32$kat_informasjon1 <- factor(mydata32$kat_informasjon1, levels = c(0,1,2,3),
                                      labels = c("Uerfaren", "Grunnleggende", "Videregående",
                                                 "Avansert"))

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
