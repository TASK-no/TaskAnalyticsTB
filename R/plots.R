task_col_scheme <-  c(light_blue = "#189BC4", dark_blue = "#3F505A",
                      orange  = "#F59331", light_green = "#54B64E",
                      green = "#58B02C", dark_green = "#356A1A",
                      black1 = "#1F282D", black2 = "#263036")
#
pb <- ggplot2::ggplot(out_data_report,
             aes(x = kategorier,
                 y = total_freq_perc,
                 fill = year,
                 label = scales::percent(total_freq_perc))) +
  geom_bar(position="dodge",
           stat="identity") +
  geom_text(position = position_dodge(width = .9),    # move to center of bars
            hjust = -0.5,    # nudge above top of bar
            size = 3)  +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = unname(task_col_scheme[c("light_blue", "dark_blue")])) +
  ggplot2::ylab("") +
  ggplot2::xlab("Digital Kompetanse") +
  theme(axis.text=element_text(size=10),
        axis.title = element_text(size = 14))+
  coord_flip()
# pb
#
out_data_report21 <- out_data_report[-12]
out_data_report21 <- out_data_report21[out_data_report21$year == "2021",]
# out_data_report21 <- out_data_report21[c(2, 3, 1, 4), ]
out_data_report21$kategorier <- factor(out_data_report21$kategorier,
                                       levels = c("Avansert",
                                                  "Videregående",
                                                  "Grunnleggende",
                                                  "Uerfaren"), ordered = TRUE)
out_data_report21 <- pivot_longer(out_data_report21,
                                  cols = tidyr::contains("perc"),
                                  names_to = "Kompetanseområder")
out_data_report21$Kompetanseområder   <- factor(out_data_report21$Kompetanseområder ,
                                                levels = c("utstyr_freq_perc",
                                                           "programmer_freq_perc",
                                                           "informasjon_freq_perc",
                                                           "kommunikasjon_freq_perc"),
                                                ordered = TRUE)
#
#
# out_data_report22 <- out_data_report[-12]
# out_data_report22 <- out_data_report22[out_data_report22$year == "2022",]
#
#
#
# # out_data_report22 <- out_data_report22[c(2, 3, 1, 4), ]
# out_data_report22$kategorier <- factor(out_data_report22$kategorier,
#                                        levels = c("Avansert",
#                                                   "Videregående",
#                                                   "Grunnleggende",
#                                                   "Uerfaren"), ordered = TRUE)
# out_data_report22 <- pivot_longer(out_data_report22,
#                                   cols = tidyr::contains("perc"),
#                                   names_to = "Kompetanseområder")
# out_data_report22$Kompetanseområder   <- factor(out_data_report22$Kompetanseområder,
#                                                 levels = c("utstyr_freq_perc",
#                                                            "programmer_freq_perc",
#                                                            "informasjon_freq_perc",
#                                                            "kommunikasjon_freq_perc"),
#                                                 ordered = TRUE)
#
# # out_data_report22 <- out_data_report22[c(2, 3, 1, 4), ]
# out_data_report22$kategorier <- factor(out_data_report22$kategorier,
#                                        levels = c("Avansert",
#                                                   "Videregående",
#                                                   "Grunnleggende",
#                                                   "Uerfaren"), ordered = TRUE)
# out_data_report22 <- pivot_longer(out_data_report22,
#                                   cols = tidyr::contains("perc"),
#                                   names_to = "Kompetanseområder")
# out_data_report22$Kompetanseområder   <- factor(out_data_report22$Kompetanseområder,
#                                                 levels = c("utstyr_freq_perc",
#                                                            "programmer_freq_perc",
#                                                            "informasjon_freq_perc",
#                                                            "kommunikasjon_freq_perc"),
#                                                 ordered = TRUE)
#
#
#
# out_data_report22_SamAnsi <-  out_data_summaries2022_SamAnsi1[[2]][-12]
# out_data_report22_SamAnsi$kategorier <- factor(out_data_report22_SamAnsi$kategorier,
#                                        levels = c("Avansert",
#                                                   "Videregående",
#                                                   "Grunnleggende",
#                                                   "Uerfaren"), ordered = TRUE)
# out_data_report22_SamAnsi <- pivot_longer(out_data_report22_SamAnsi,
#                                   cols = tidyr::contains("perc"),
#                                   names_to = "Kompetanseområder")
# out_data_report22_SamAnsi$Kompetanseområder   <- factor(out_data_report22_SamAnsi$Kompetanseområder,
#                                                 levels = c("utstyr_freq_perc",
#                                                            "programmer_freq_perc",
#                                                            "informasjon_freq_perc",
#                                                            "kommunikasjon_freq_perc"),
#                                                 ordered = TRUE)
# pb22_SamAnsi <- ggplot(out_data_report22_SamAnsi,
#                aes(x = kategorier,
#                    y = value,
#                    fill = Kompetanseområder,
#                    label = scales::percent(value))) +
#   geom_bar(position="dodge",
#            stat="identity") +
#   geom_text(position = position_dodge(width = .9),    # move to center of bars
#             hjust = -0.5,    # nudge above top of bar
#             size = 3)  +
#   scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
#   scale_fill_manual(values = unname(task_col_scheme[c("dark_green",
#                                                       "black2",
#                                                       "black1",
#                                                       "green")]),
#   labels = c("Kommunikasjon og samhandling",
#              "Informasjonssikkerhet og personvern",
#              "Bruk av programmvare",
#              "Bruk av teknologi")) +
#   ggplot2::ylab("") +
#   ggplot2::xlab("Digital Kompetanse") +
#   ggplot2::ggtitle("År 2022 - SamAnsi") +
#   coord_flip() +
#   theme(axis.text=element_text(size=10),
#         axis.title = element_text(size = 14),
#         legend.justification = c(1,0),
#         legend.position = c(1,0),
#         legend.text = element_text("Kompetanseområder"),
#         legend.title = element_text("Kompetanseområder"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         plot.title = element_text(hjust = 0.5, size = 20))
# # pb21
# # pb22
#
#
#
#








# # out_data_report22 <- out_data_report22[c(2, 3, 1, 4), ]
# out_data_report22Sa$kategorier <- factor(out_data_report22$kategorier,
#                                        levels = c("Avansert",
#                                                   "Videregående",
#                                                   "Grunnleggende",
#                                                   "Uerfaren"), ordered = TRUE)
# out_data_report22 <- pivot_longer(out_data_report22,
#                                   cols = tidyr::contains("perc"),
#                                   names_to = "Kompetanseområder")
# out_data_report22$Kompetanseområder   <- factor(out_data_report22$Kompetanseområder,
#                                                 levels = c("utstyr_freq_perc",
#                                                            "programmer_freq_perc",
#                                                            "informasjon_freq_perc",
#                                                            "kommunikasjon_freq_perc"),
#                                                 ordered = TRUE)
# pb21 <- ggplot(out_data_report21,
#                aes(x = kategorier,
#                    y = value,
#                    fill = Kompetanseområder,
#                    label = scales::percent(value))) +
#   geom_bar(position="dodge",
#            stat="identity") +
#   geom_text(position = position_dodge(width = .9),    # move to center of bars
#             hjust = -0.5,    # nudge above top of bar
#             size = 3)  +
#   scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
#   scale_fill_manual(values = unname(task_col_scheme[c("dark_green",
#                                                       "black2",
#                                                       "black1",
#                                                       "green"
#   )]),
#   labels = c("Kommunikasjon og samhandling",
#              "Informasjonssikkerhet og personvern",
#              "Bruk av programmvare",
#              "Bruk av teknologi")) +
#   ggplot2::ylab("") +
#   ggplot2::xlab("Digital Kompetanse") +
#   ggplot2::ggtitle("År 2021") +
#   coord_flip() +
#   theme(axis.text=element_text(size=10),
#         axis.title = element_text(size = 14),
#         legend.justification = c(1,0),
#         legend.position = c(1,0),
#         legend.text = element_text("Kompetanseområder"),
#         legend.title = element_text("Kompetanseområder"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         plot.title = element_text(hjust = 0.5, size = 20))
# pb21
# pb22
