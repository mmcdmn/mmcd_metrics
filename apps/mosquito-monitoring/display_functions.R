# =============================================================================
# DISPLAY FUNCTIONS: Mosquito Monitoring App
# =============================================================================
# Chart creation code - kept exactly as from working app
# =============================================================================

library(ggplot2)
library(dplyr)

# Color and shape mappings - same as original app
mosquito_colors <- c(
  "Total_Ae_+_Cq" = "#000000", Total_Ae_springs = "#008000", Total_Ae_summers = "#ffa500",
  Cq_perturbans_42 = "#800080", Total_Cx_vectors = "#FF0000", Cx_erraticus_32 = "#000000",
  Cx_pipiens_33 = "#0000FF", Cx_restuans_34 = "#008000", Cx_salinarius_35 = "#87cefa",
  Cx_tarsalis_36 = "#a52a2a", Cx_territans_37 = "#00ff7f", "Cx_restuans/pipiens_372" = "#40e0d0",
  Cx_unknown_371 = "#ffa500", An_barberi_27 = "#FFFF00", An_earlei_28 = "#ffc0cb",
  An_punctipennis_29 = "#0000FF", An_quadrimaculatus_30 = "#FF0000", An_walkeri_31 = "#ffa500",
  sp311an_un = "#800080", Total_Anopheles = "#87cefa", sp01_abser = "#FF0000", sp03_aurif = "#FFFF00",
  sp04_euedes = "#f08080", sp05_campest = "#adff2f", sp08_commun = "#483d8b", sp09_diant = "#00FFFF",
  sp118abpun = "#800080", sp11_excru = "#ffa500", sp12_fitch = "#a52a2a", sp13_flave = "#800000",
  sp14_imple = "#7fff00", sp15_intrud = "#ffd700", sp17_pioni = "#FF00FF", sp18_punct = "#0000FF",
  sp19_ripar = "#008000", sp20_spenc = "#ff1493", sp22_stimu = "#708090", sp23_provo = "#ff6347",
  Ae_cinereus_7 = "#006400", Ae_triseriatus_24 = "#0000FF", Ae_vexans_26 = "#FF0000",
  sp02_atrop = "#ff1493", Ae_canadensis_6 = "#000000", Ae_dorsalis_10 = "#808080", sp16_nigro = "#ffd700",
  sp21_stict = "#FF00FF", sp25_trivi = "#800080", sp261ae_unid = "#000000", sp262spr_unid = "#008000",
  sp264summ_unid = "#ffa500", sp50_hende = "#7fff00", Ae_albopictus_51 = "#FF0000",
  Ae_japonicus_52 = "#008000", Ps_ciliata_44 = "#a52a2a", Ps_columbiae_45 = "#008000",
  Ps_ferox_46 = "#000000", sp471ps_un = "#808080", Ps_horrida_47 = "#FF0000", sp38_inorn = "#0000FF",
  Total_Psorophora = "#00FFFF", Culiseta_melanura = "#FF0000", sp40_minne = "#ffa500", sp41_morsi = "#a52a2a",
  sp411cs_un = "#808080", Or_signifera_43 = "#87cefa", Ur_sapphirina_48 = "#00008b", sp49_smith = "#0000FF"
)

mosquito_shapes <- c(
  "Total_Ae_+_Cq" = 1, Total_Ae_springs = 1, Total_Ae_summers = 1, Cq_perturbans_42 = 1,
  Total_Cx_vectors = 1, Cx_erraticus_32 = 15, Cx_pipiens_33 = 15, Cx_restuans_34 = 15,
  Cx_salinarius_35 = 15, Cx_tarsalis_36 = 15, Cx_territans_37 = 15, "Cx_restuans/pipiens_372" = 15,
  Cx_unknown_371 = 15, An_barberi_27 = 4, An_earlei_28 = 4, An_punctipennis_29 = 4,
  An_quadrimaculatus_30 = 4, An_walkeri_31 = 4, sp311an_un = 4, Total_Anopheles = 4,
  sp01_abser = 19, sp03_aurif = 19, sp04_euedes = 19, sp05_campest = 19, sp08_commun = 19,
  sp09_diant = 19, sp118abpun = 19, sp11_excru = 19, sp12_fitch = 19, sp13_flave = 19,
  sp14_imple = 19, sp15_intrud = 19, sp17_pioni = 19, sp18_punct = 19, sp19_ripar = 19,
  sp20_spenc = 19, sp22_stimu = 19, sp23_provo = 19, Ae_cinereus_7 = 19, Ae_triseriatus_24 = 19,
  Ae_vexans_26 = 19, sp02_atrop = 19, Ae_canadensis_6 = 19, Ae_dorsalis_10 = 19, sp16_nigro = 19,
  sp21_stict = 19, sp25_trivi = 19, sp261ae_unid = 19, sp262spr_unid = 19, sp264summ_unid = 19,
  sp50_hende = 19, Ae_albopictus_51 = 19, Ae_japonicus_52 = 19, Ps_ciliata_44 = 3,
  Ps_columbiae_45 = 3, Ps_ferox_46 = 3, sp471ps_un = 3, Ps_horrida_47 = 3, sp38_inorn = 3,
  Total_Psorophora = 3, Culiseta_melanura = 18, sp40_minne = 18, sp41_morsi = 18, sp411cs_un = 18,
  Or_signifera_43 = 18, Ur_sapphirina_48 = 18, sp49_smith = 18
)

#' Create mosquito chart - WITH error bars
#' @export
create_mosquito_chart_with_error <- function(data, date_breaks = "1 month", y_limits = NULL, scale_trans = "identity", y_label = "Mosquitos (Average/Trap) Zone 1") {
  # Handle log transformation: replace 0/NA with small positive value
  plot_data <- data %>%
    filter(!is.na(avg), !is.na(inspdate)) %>%
    mutate(avg = ifelse(avg <= 0, 0.01, avg),
           sd = ifelse(is.na(sd), 0, sd))
  
  # Adjust y_limits for log scale
  if (!is.null(y_limits) && scale_trans == "log10") {
    y_limits <- c(0.01, y_limits[2])
  }
  
  ggplot(plot_data, aes(x = inspdate, y = avg)) +
    scale_x_date(position = "top", date_breaks = date_breaks, date_labels = "%Y/%m/%d") +
    scale_y_continuous(limits = y_limits, trans = scale_trans) +
    theme(axis.text.x = element_text(size = 15, angle = -45, hjust = 1),
          axis.title.x = element_text(size = 15, margin = margin(t = 20, r = 0, b = 5, l = 0, unit = "pt")),
          axis.text.y = element_text(size = 20),
          axis.title.y = element_text(size = 15, margin = margin(t = 0, r = 15, b = 0, l = 0, unit = "pt")),
          legend.text = element_text(size = 20), legend.title = element_text(size = 25),
          legend.position = "right") +
    xlab("Inspection Date") +
    ylab(y_label) +
    geom_line(aes(color = spp_name)) +
    scale_color_manual(name = "Species", values = mosquito_colors) +
    scale_shape_manual(name = "Species", values = mosquito_shapes) +
    geom_point(aes(color = spp_name, shape = spp_name), size = 3)
}

#' Create mosquito chart - WITHOUT error bars (lines only)
#' @export
create_mosquito_chart_no_error <- function(data, date_breaks = "1 month", y_limits = NULL, scale_trans = "identity", y_label = "Mosquitos (Average/Trap) Zone 1") {
  # Handle log transformation: replace 0/NA with small positive value
  plot_data <- data %>%
    filter(!is.na(avg), !is.na(inspdate)) %>%
    mutate(avg = ifelse(avg <= 0, 0.01, avg))
  
  # Adjust y_limits for log scale
  if (!is.null(y_limits) && scale_trans == "log10") {
    y_limits <- c(0.01, y_limits[2])
  }
  
  ggplot(plot_data, aes(x = inspdate, y = avg)) +
    scale_x_date(position = "top", date_breaks = date_breaks, date_labels = "%Y/%m/%d") +
    scale_y_continuous(limits = y_limits, trans = scale_trans) +
    theme(axis.text.x = element_text(size = 15, angle = -45, hjust = 1),
          axis.title.x = element_text(size = 15, margin = margin(t = 20, r = 0, b = 5, l = 0, unit = "pt")),
          axis.text.y = element_text(size = 20),
          axis.title.y = element_text(size = 15, margin = margin(t = 0, r = 15, b = 0, l = 0, unit = "pt")),
          legend.text = element_text(size = 20), legend.title = element_text(size = 25),
          legend.position = "right") +
    xlab("Inspection Date") +
    ylab(y_label) +
    geom_line(aes(color = spp_name)) +
    scale_color_manual(name = "Species", values = mosquito_colors) +
    scale_shape_manual(name = "Species", values = mosquito_shapes) +
    geom_point(aes(color = spp_name, shape = spp_name), size = 3)
}

#' Create mosquito chart for "All" tab
#' @export
create_mosquito_chart_all <- function(data, zone_label = "All Zones", date_breaks = "1 month", y_limits = NULL, scale_trans = "identity", y_label = NULL) {
  # Handle log transformation: replace 0/NA with small positive value
  plot_data <- data %>%
    filter(!is.na(avg), !is.na(inspdate)) %>%
    mutate(avg = ifelse(avg <= 0, 0.01, avg))
  
  # Adjust y_limits for log scale
  if (!is.null(y_limits) && scale_trans == "log10") {
    y_limits <- c(0.01, y_limits[2])
  }
  
  ggplot(plot_data, aes(x = inspdate, y = avg)) +
    scale_x_date(position = "top", date_breaks = date_breaks, date_labels = "%Y/%m/%d") +
    scale_y_continuous(limits = y_limits, trans = scale_trans) +
    theme(axis.text.x = element_text(size = 15, angle = -45, hjust = 1),
          axis.title.x = element_text(size = 15, margin = margin(t = 20, r = 0, b = 5, l = 0, unit = "pt")),
          axis.text.y = element_text(size = 20),
          axis.title.y = element_text(size = 15, margin = margin(t = 0, r = 15, b = 0, l = 0, unit = "pt")),
          legend.text = element_text(size = 20), legend.title = element_text(size = 25),
          legend.position = "right") +
    xlab("Inspection Date") +
    ylab(if (is.null(y_label)) paste("Mosquitos (Average/Trap)", zone_label) else y_label) +
    geom_line(aes(color = spp_name)) +
    scale_color_manual(name = "Species", values = mosquito_colors) +
    scale_shape_manual(name = "Species", values = mosquito_shapes) +
    geom_point(aes(color = spp_name, shape = spp_name), size = 3)
}

message(" display_functions.R loaded")
