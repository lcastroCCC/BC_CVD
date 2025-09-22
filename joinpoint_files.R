library(ggplot2)
library(stringr)
library(ggrepel)
library(tidyr)
library(cowplot)
library(gridExtra)
library(grid)
library(scales)
library(forcats)
library(showtext)
library(shadowtext)
library(dplyr)
library(conflicted)

conflicts_prefer(dplyr::filter)

font_import(pattern = "Times", prompt = FALSE)
font_add("Times New Roman", regular = "/System/Library/Fonts/Supplemental/Times New Roman.ttf")
showtext_auto()
showtext_opts(dpi = 300)

# Inputs -------------------------------------------------------------------

base_path_oac <- "~/Library/CloudStorage/Box-Box/Sanchez-Diaz Lab/Obesity Related Cancers/Analysis Results"

read_data <- function(sub_path, file_prefix) {
  list(
    AAPC = read.delim(file.path(base_path_oac, sub_path, paste0(file_prefix, ".Export.AAPC.txt"))),
    Data = read.delim(file.path(base_path_oac, sub_path, paste0(file_prefix, ".Export.Data.txt")))
  )
}

# Load datasets
All_Overall <- read_data("All overall", "JP Obesity All Overall Age") # Individual cancer types, F and M, yearly
Female_50 <- read_data("Female 50+", "JP Obesity Female 50+")
All_20_49 <- read_data("All 20-49", "JP Obesity All 20-49")
Male_50 <- read_data("Male 50+", "JP Obesity Male 50+")
Overall <- read_data("Overall OAC Yes vs No/Obesity Yes vs No Overall", "jp obesity yes vs no all") # OAC vs Non-OAC, F and M
Overall_50 <- read_data("Overall OAC Yes vs No/Obesity Yes vs No 50+", "JP Obesity Yes vs No All 50+") # OAC vs Non-OAC, F and M, 50+
Overall_20_49 <- read_data("Overall OAC Yes vs No/Obesity Yes vs No 20-49", "jp obesity yes vs no all 20-49")

Female_50$AAPC <- Female_50$AAPC %>%
  rename(
    Age.Group = Age.50..by.10
  ) %>%
  mutate(M..F = "Female") 

All_20_49$AAPC <- All_20_49$AAPC %>%
  rename(
    Obesity.associated.cancers = Obesity.associated.cancers..20.49.,
    Age.Group = Age.20.49)

Male.50plus.Renamed <- Male_50$Data %>%
  rename(
    Obesity.associated.cancers..20.49. = Obesity.associated.cancers,
    Age.20.49 = Age.50..by.10
  ) %>%
  mutate(M..F = "Male") 


Male_50$AAPC <- Male_50$AAPC %>%
  mutate(M..F = "Male") %>%
  rename(Age.Group = Age.50..by.10)

Female.50plus.Renamed <- Female_50$Data %>%
  rename(
    Obesity.associated.cancers..20.49. = Obesity.associated.cancers,
    Age.20.49 = Age.50..by.10
  ) %>%
  mutate(M..F = "Female") 


AAPC_combined_females <- bind_rows(All_20_49$AAPC, Female_50$AAPC) %>% 
  filter(trimws(M..F) == "Female") %>%
  mutate(significant = ifelse(P.Value < 0.05, "*", "")) %>%
  select(M..F, Obesity.associated.cancers, Age.Group, End.Obs, AAPC, significant)

AAPC_combined_males <- bind_rows(All_20_49$AAPC, Male_50$AAPC) %>% 
  filter(trimws(M..F) == "Male") %>%
  mutate(significant = ifelse(P.Value < 0.05, "*", "")) %>%
  select(M..F, Obesity.associated.cancers, Age.Group, End.Obs, AAPC, significant)

cancer_order <- c(
  "Obesity-associated cancers (OVERALL))", "Colorectal", "Pancreas", "Kidney", 
  "Thyroid", "Liver", "Multiple Myeloma", "Gastric Cardia", "Gallbladder", 
  "Esophageal \n Adenocarcinoma", "Postmenopausal \n Breast", "Ovary", 
  "Corpus and Uterus, \n NOS", "Meningioma")

combined_data_females <- bind_rows(All_20_49$Data, Female.50plus.Renamed) %>%
  filter(trimws(M..F) == "Female") %>%
  rename(Obesity.associated.cancers = Obesity.associated.cancers..20.49.,
         Age.Group = Age.20.49) %>%
  left_join(AAPC_combined_females, by = c("Obesity.associated.cancers", "Age.Group")) %>%
  mutate(
    Obesity.associated.cancers = str_replace_all(
      Obesity.associated.cancers,
      c("Corrpus and Uterus, NOS" = "Corpus and Uterus, \n NOS", "Colon and Rectum" = "Colorectal",
        "MM" = "Multiple Myeloma", "Adenorcacinoma" = "\n Adenocarcinoma",
        "Postmenopausal Breast" = "Postmenopausal \n Breast")),
    Obesity.associated.cancers = factor(Obesity.associated.cancers, levels = cancer_order))
    

combined_data_males <- bind_rows(All_20_49$Data, Male.50plus.Renamed) %>%
  filter(trimws(M..F) == "Male") %>%
  rename(Age.Group = Age.20.49,
         Obesity.associated.cancers = Obesity.associated.cancers..20.49.) %>%
  left_join(AAPC_combined_males, by = c("Obesity.associated.cancers", "Age.Group")) %>%
  mutate(Obesity.associated.cancers = str_replace_all(
    Obesity.associated.cancers,
    c("Corrpus" = "Corpus", "Colon and Rectum" = "Colorectal",
      MM = "Multiple Myeloma",
      "Adenorcacinoma" = "\n Adenocarcinoma")),
    Obesity.associated.cancers = factor(Obesity.associated.cancers, levels = cancer_order))

label_data_females <- combined_data_females %>%
  filter(!is.na(significant),
         !Obesity.associated.cancers %in% c("Obesity-associated cancers (OVERALL))", "Esophageal Adenocarcinoma", "Meningioma")) %>%
  filter(Year.2000.2022 == 2022) %>%
  distinct(Obesity.associated.cancers, Age.Group, Model, AAPC, significant) %>%
  mutate(Year.2000.2022 = 2022,
         AAPC = replace_na(AAPC, NA_real_),
         significant = replace_na(significant, ""),
         Age.Group = factor(Age.Group, levels = c("20-39 years", "40-49 years", 
                                                  "50-59 years", "60-69 years", 
                                                  "70-79 years", "80+ years")))

label_data_males <- combined_data_males %>%
  filter(!is.na(significant),
         !Obesity.associated.cancers %in% c("Obesity-associated cancers (OVERALL))", "Esophageal Adenocarcinoma", "Meningioma")) %>%
  filter(Year.2000.2022 == 2022) %>%
  distinct(Obesity.associated.cancers, Age.Group, Model, AAPC, significant) %>%
  mutate(Year.2000.2022 = 2022,
         AAPC = replace_na(AAPC, NA_real_),
         significant = replace_na(significant, ""),
         Age.Group = factor(Age.Group, levels = c("20-39 years", "40-49 years", 
                                                  "50-59 years", "60-69 years", 
                                                  "70-79 years", "80+ years")))


Overall$Data <- Overall$Data %>%
  mutate(CancerType = ifelse(
    Obesity.associated.cancers..rel.vs.no.rel..Overall.Oficial == "Obesity-associated cancers (OVERALL))",
    "OAC", "Non-OAC")) %>%
  mutate(CancerType = factor(CancerType, levels = c( "OAC", "Non-OAC")))

# Overall Plots ----------------------------------------------------------

plot_by_sex <- function(sex, filename) {
  labels <- Overall$AAPC %>%
    rename(CancerType = Obesity.associated.cancers..rel.vs.no.rel..Overall.Oficial) %>%
    mutate(
      CancerType = ifelse(CancerType == "Obesity-associated cancers (OVERALL))", "OAC", "Non-OAC"),
      label = paste0(round(AAPC, 2), "%", ifelse(P.Value < 0.05, "*", ""))
    ) %>%
    filter(trimws(Sex2) == sex) %>%
    select(CancerType, label)
  
  label_oac <- labels %>% filter(CancerType == "OAC") %>% pull(label)
  label_non_oac <- labels %>% filter(CancerType == "Non-OAC") %>% pull(label)
  
  data_filtered <- Overall$Data %>% filter(trimws(Sex2) == sex)
  
  # Annotation settings: Female layout used for Female and Overall
  if (sex == "Male") {
    rect_ymin <- 265; rect_ymax <- 320
    text_y_aapc <- 315; text_y_oac <- 300; text_y_non_oac <- 285
  } else if (sex == "Female") {
    rect_ymin <- 215; rect_ymax <- 240
    text_y_aapc <- 237.5; text_y_oac <- 230; text_y_non_oac <- 222.5
  } else {
    rect_ymin <- 170; rect_ymax <- 195
    text_y_aapc <- 190; text_y_oac <- 185; text_y_non_oac <- 180
  }
  
  plot <- ggplot(data_filtered, aes(x = Year.of.diagnosis.2000.2022.JP, y = Model,
                                    group = CancerType, color = CancerType)) +
    geom_line(linewidth = 0.6, linetype = "dotted") +
    geom_point(size = 2) +
    scale_x_continuous(breaks = seq(2000, 2022, by = 1)) +
    scale_color_manual(values = c("OAC" = "#5A8F2A", "Non-OAC" = "#B76BA3")) +
    annotate("rect", xmin = 2014, xmax = 2021, ymin = rect_ymin, ymax = rect_ymax,
             fill = "white", alpha = 1) +
    annotate("text", x = 2015, y = text_y_aapc, label = "AAPC", hjust = 0, vjust = 1,
             size = 3.5, fontface = "bold", color = "black") +
    annotate("text", x = 2015, y = text_y_oac, label = paste("OAC:", label_oac), hjust = 0, vjust = 1,
             size = 3.5, fontface = "bold", color = "#5A8F2A") +
    annotate("text", x = 2015, y = text_y_non_oac, label = paste("Non-OAC:", label_non_oac), hjust = 0, vjust = 1,
             size = 3.5, fontface = "bold", color = "#B76BA3") +
    labs(x = "Year", y = "Age-Adjusted Rate", color = "Cancer Type") +
    theme_minimal(base_family = "librebaskerville") +
    theme(legend.position = "right",
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
  
  
  print(plot)
  ggsave(file.path(base_path_oac, filename), plot = plot, bg = "white", width = 10, height = 6)
}

# Generate and preview plots
plot_by_sex("Male", "Figures/Overall_Male.png")
plot_by_sex("Female", "Figures/Overall_Female.png")
plot_by_sex("Overall", "Figures/Overall_Combined.png")

# Overall - Faceted by Sex ---------------------------------------------

{data_filtered <- Overall$Data %>%
  filter(Sex2 != "Overall") %>%
  mutate(Sex2 = case_when(
          str_trim(Sex2) == "Female" ~ "Females",
          str_trim(Sex2) == "Male" ~ "Males",
          TRUE ~ as.character(Sex2)), 
    CancerType = ifelse(Obesity.associated.cancers..rel.vs.no.rel..Overall.Oficial == 
                          "Obesity-associated cancers (OVERALL))", "OAC", "Non-OAC"),
    CancerType = factor(CancerType, levels = c("OAC", "Non-OAC")),
    Sex2 = factor(Sex2)
  )

labels <- Overall$AAPC %>%
  rename(CancerType = Obesity.associated.cancers..rel.vs.no.rel..Overall.Oficial) %>%
  mutate(
    CancerType = ifelse(CancerType == "Obesity-associated cancers (OVERALL))", "OAC", "Non-OAC"),
    Sex2 = case_when(
      str_trim(Sex2) == "Female" ~ "Females",
      str_trim(Sex2) == "Male" ~ "Males",
      TRUE ~ as.character(Sex2)
    ),
    label = paste0("AAPC 2000–2022 = ", round(AAPC, 2), "%", ifelse(P.Value < 0.05, "*", ""))
  ) %>%
  filter(Sex2 != "Overall") %>%
  select(Sex2, CancerType, label)


label_data <- data_filtered %>%
  group_by(Sex2, CancerType) %>%
  summarise(label_y = max(Model, na.rm = TRUE) + 20, .groups = "drop") %>%
  left_join(labels, by = c("Sex2", "CancerType")) %>%
  mutate(
    Sex2 = case_when(
      str_trim(Sex2) == "Female" ~ "Females",
      str_trim(Sex2) == "Male" ~ "Males",
      TRUE ~ as.character(Sex2)))

# Plot

plot <- ggplot(data_filtered, aes(x = Year.of.diagnosis.2000.2022.JP, y = Model, group = CancerType)) +
  geom_line(aes(color = CancerType), linewidth = 0.6, linetype = "dotted", show.legend = FALSE) +
  geom_point(aes(color = CancerType, fill = CancerType, shape = CancerType),
             size = 3, stroke = 0.3, show.legend = TRUE) +
  geom_point(data = subset(data_filtered, Year.of.diagnosis.2000.2022.JP == 2020),
             aes(color = CancerType, shape = CancerType),
             fill = "white", color = "black", size = 3, stroke = 0.4, show.legend = FALSE) +
  geom_text(data = label_data, aes(x = 2012, y = label_y, label = label, color = CancerType),
            hjust = 0, vjust = 0, size = 7, fontface = "bold", family = "Times New Roman", show.legend = FALSE) +
  scale_shape_manual(values = c("OAC" = 21, "Non-OAC" = 24)) +
  scale_color_manual(values = c("OAC" = "#5A8F2A", "Non-OAC" = "#B76BA3")) +  
  scale_fill_manual(values = c("OAC" = "#5A8F2A", "Non-OAC" = "#B76BA3")) +
  scale_x_continuous(breaks = seq(2000, 2022, by = 1)) +
  scale_y_continuous(limits = c(100, 475)) +
  labs(x = "Year of diagnosis", y = "Rate per 100,000",
       shape = "Cancer Type", fill = "Cancer Type", color = "Cancer Type") +
  facet_wrap(~Sex2, nrow = 1) +
  theme_minimal() +
  theme(text = element_text(family = "Times New Roman"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
    legend.position = "bottom",
    legend.title = element_text(size = 25, face = "bold"),
    legend.text = element_text(size = 20, face = "bold"),
    axis.text.x = element_text(angle = 90, size = 20, face = "bold"),
    axis.text.y = element_text(size = 20, face = "bold"),
    axis.title.x = element_text(size = 20, face = "bold", margin = margin(t = 20)),
    axis.title.y = element_text(size = 20, face = "bold", margin = margin(r = 20)),
    strip.text = element_text(size = 30, face = "bold"),
    panel.spacing = unit(2, "lines"),
    legend.box.margin = margin(t = 30),
    panel.grid = element_blank(),
    axis.line = element_line(color = "black", linewidth = 0.8),
    plot.margin = margin(t = 20, r = 50, b = 20, l = 20))
 #   plot.background = element_rect(color = "#657356", linewidth = 2))

ggsave(file.path(base_path_oac, "Figures/Overall_FacetBySex.png"),
       plot = plot, bg = "white", width = 18, height = 10, dpi = 300)
}

# Master Graphs -------------------------------------------------------

generate_cancer_plot <- function(data, label_data, excluded_sites, output_path) {
  cancer_order <- c(
    "Obesity-associated cancers (OVERALL))", "Colorectal", "Pancreas", "Kidney", 
    "Thyroid", "Liver", "Multiple Myeloma", "Gastric Cardia", "Gallbladder", 
    "Esophageal \n Adenocarcinoma", "Postmenopausal \n Breast", "Ovary", 
    "Corpus and Uterus, \n NOS", "Meningioma"
  )
  
  age_colors <- c(
    "20-39 years" = "#0f3c55", "40-49 years" = "#ebc845", "50-59 years" = "#f16c21",
    "60-69 years" = "#c02e1d", "70-79 years" = "#1595ba", "80+ years" = "#a18fc7"
  )
  
  # Ensure Age.Group is a factor with correct levels
  data$Age.Group <- factor(data$Age.Group, levels = names(age_colors))
  label_data$Age.Group <- factor(label_data$Age.Group, levels = names(age_colors))
  
  # Ensure cancer factor levels are consistent
  data$Obesity.associated.cancers <- factor(data$Obesity.associated.cancers, levels = cancer_order)
  label_data$Obesity.associated.cancers <- factor(label_data$Obesity.associated.cancers, levels = cancer_order)
  
  # Filter data
  filtered_data <- data %>%
    filter(!Obesity.associated.cancers %in% excluded_sites, Age.Adjusted.Rate != 0)
  
  # Only use available levels in filtered data
  available_levels <- intersect(cancer_order, unique(filtered_data$Obesity.associated.cancers))
  
  plot_list <- lapply(available_levels, function(cancer) {
    data_subset <- filtered_data %>%
      filter(Obesity.associated.cancers == cancer) %>%
      filter(!is.na(Model), !is.na(Year.2000.2022))
    
    label_subset <- label_data %>%
      filter(Obesity.associated.cancers == cancer) %>%
      mutate(label = paste0(round(AAPC, 2), "% ", significant))
    
    p <- ggplot(data_subset, aes(x = Year.2000.2022, y = Model, color = Age.Group)) +
      geom_point(size = 0.6) +
      geom_line(linewidth = 0.2, linetype = "dashed") +
      scale_color_manual(values = age_colors) +
      scale_x_continuous(breaks = round(seq(2000, 2022, length.out = 3))) +
      labs(title = cancer) +
      coord_cartesian(expand = TRUE, clip = "off", xlim = c(2000, 2022)) +
      theme_minimal(base_family = "librebaskerville") +
      theme(
        legend.position = "none",
        axis.title = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 10)
      )
    
    # Label table
    full_age_groups <- names(age_colors)
    
    present_labels <- label_subset %>%
      filter(Age.Group %in% full_age_groups) %>%
      mutate(Age.Group = factor(Age.Group, levels = full_age_groups)) %>%
      arrange(Age.Group)
    
    missing_groups <- setdiff(full_age_groups, present_labels$Age.Group)
    
    empty_rows <- data.frame(
      Age.Group = factor(missing_groups, levels = full_age_groups),
      label = rep("", length(missing_groups)),
      stringsAsFactors = FALSE
    )
    
    padded_labels <- bind_rows(present_labels %>% select(Age.Group, label), empty_rows)
    
    table_data <- data.frame(AAPC = padded_labels$label, stringsAsFactors = FALSE)
    rownames(table_data) <- seq_len(nrow(table_data))
    
    table_grob <- tableGrob(
      table_data,
      rows = NULL,
      theme = ttheme_minimal(
        core = list(
          fg_params = list(
            col = age_colors[as.character(padded_labels$Age.Group)],
            fontface = "bold", fontsize = 6
          ),
          padding = unit(c(1.25, 1.25), "mm")
        ),
        colhead = list(
          fg_params = list(fontface = "plain", fontsize = 8),
          padding = unit(c(1.25, 2), "mm")
        )
      )
    )
    
    plot_grid(p, table_grob, ncol = 2, rel_widths = c(3, 1))
  })
  
  # Legend
  legend <- get_legend(
    ggplot(filtered_data, aes(x = Year.2000.2022, y = Model, color = Age.Group)) +
      geom_point() +
      scale_color_manual(values = age_colors) +
      theme_minimal(base_family = "librebaskerville") +
      theme(
        legend.position = "right",
        legend.key.size = unit(0.4, "cm"),
        legend.text = element_text(size = 8)
      )
  )
  
  plots_grid <- plot_grid(plotlist = plot_list, ncol = 3, align = "v")
  plots_with_legend <- plot_grid(plots_grid, legend, ncol = 2, rel_widths = c(4, 0.6))
  
  final_plot <- ggdraw() +
    draw_plot(plots_with_legend, x = 0.05, y = 0.05, width = 0.9, height = 0.9) +
    draw_label("Year", x = 0.5, y = 0.01, vjust = 0, size = 12) +
    draw_label("Age-Adjusted Rate", x = 0.01, y = 0.5, angle = 90, vjust = 1, size = 12)
  
  print(final_plot)
  
  
  ggsave(
    filename = output_path,
    plot = final_plot,
    bg = "white",
    width = 10,
    height = if (length(plot_list) > 9) 8 else 6)
  #ggsave(filename = output_path, plot = final_plot, bg = "white", width = 10, height = 8)
} 

generate_cancer_plot(
  data = combined_data_males,
  label_data = label_data_males,
  excluded_sites = c(
    "Obesity-associated cancers (OVERALL))",
    "Corpus and Uterus, \n NOS",
    "Ovary",
    "Postmenopausal \n Breast",
    "Gallbladder",
    "Meningioma"),
  output_path = file.path(base_path_oac,"Figures/Master_Males.png"))

generate_cancer_plot(
  data = combined_data_females,
  label_data = label_data_females,
  excluded_sites = c("Obesity-associated cancers (OVERALL))", "Esophageal \n Adenocarcinoma", "Meningioma"),
  output_path = file.path(base_path_oac,"Figures/Master_Females.png"))




# Bubble plots ------------------------------------------------------------

# Define cancer order
cancer_order2 <- c(
  "Colorectal", "Pancreas", "Kidney", 
  "Thyroid", "Liver", "Multiple Myeloma", "Gastric Cardia", "Gallbladder", 
  "Esophageal Adenocarcinoma", "Meningioma", "Ovary", "Postmenopausal Breast", 
  "Corpus and Uterus, NOS", "All cancer types")
cancer_order2 <- break_long_labels(cancer_order2)

# Remove "years" from Age.Group
AAPC_combined_females$Age.Group <- gsub(" years", "", AAPC_combined_females$Age.Group)
AAPC_combined_males$Age.Group <- gsub(" years", "", AAPC_combined_males$Age.Group)

# Break long cancer type labels into two lines
break_long_labels <- function(x) {
  ifelse(nchar(x) > 12, gsub(" ", "\n", x), x)
}

AAPC_combined_females$Obesity.associated.cancers <- break_long_labels(as.character(AAPC_combined_females$Obesity.associated.cancers))
AAPC_combined_males$Obesity.associated.cancers <- break_long_labels(as.character(AAPC_combined_males$Obesity.associated.cancers))

# Reorder factor levels
AAPC_combined_females$Obesity.associated.cancers <- factor(AAPC_combined_females$Obesity.associated.cancers, levels = break_long_labels(cancer_order2))
AAPC_combined_males$Obesity.associated.cancers <- factor(AAPC_combined_males$Obesity.associated.cancers, levels = break_long_labels(cancer_order2))

plot_aapc_bubbles <- function(female_data = NULL, male_data = NULL,
                              show_non_significant = TRUE,
                              save_path = NULL) {
  
  library(ggplot2)
  library(cowplot)
  library(forcats)
  library(shadowtext)
  
  aapc_breaks <- c(-8, -4, 0, 4, 8)
  aapc_breaks <- c(-8, -6, -4, -2, 0, 2, 4, 6, 8)
  size_colors <- c("#3288bd", "#84b8d7", "#c1d4b1", "#fee08b", "#fc8d59")
  size_colors <- c("#3288bd", "#5a9cc7", "#84b8d7", "#a2c6c4", "#c1d4b1", "#d9dc9e", "#fee08b", "#fd9772", "#fc8d59")
  
  create_single_plot <- function(data, sex) {
    text_data <- if (show_non_significant) data else data[grepl("\\*", data$significant), ]
    
    p <- ggplot(data, aes(x = Obesity.associated.cancers, y = fct_rev(Age.Group))) +
      geom_point(aes(size = abs(AAPC), fill = AAPC), color = "white", alpha = 1, shape = 21, stroke = 0.8) +
      shadowtext::geom_shadowtext(
        data = text_data,
        aes(label = paste0(round(AAPC, 2)),
            fontface = ifelse(grepl("\\*", paste0(round(AAPC, 2))), "bold", "plain")),
        size = 4.5, family = "librebaskerville", color = "black", bg.color = NA, bg.r = 0.07
      ) +
      scale_fill_gradientn(colors = size_colors, limits = c(-8, max(data$AAPC, na.rm = TRUE)),
                           breaks = aapc_breaks, labels = as.character(aapc_breaks),
                           name = "AAPC (%)", guide = "none") +
      scale_size_continuous(range = c(2, 22), limits = c(0, 9),
                            breaks = abs(aapc_breaks), labels = aapc_breaks,
                            name = "AAPC (%)") +
      scale_x_discrete(drop = TRUE) +
      labs(title = sex, x = "Cancer Type", y = "Age Group") +
      theme_minimal(base_size = 11, base_family = "librebaskerville") +
      theme(
        plot.title = element_text(size = 22, hjust = 0.5, face = "bold", margin = margin(b = 15)),
        axis.title.x = element_text(size = 15, face = "bold", margin = margin(t = 10)),
        axis.title.y = element_text(size = 15, face = "bold", margin = margin(r = 25)),
        axis.text.x = element_text(angle = 47, hjust = 1, size = 12, face = "bold", color = "black"),
        axis.text.y = element_text(size = 12, face = "bold", color = "black"),
        axis.line.x = element_line(color = "black", linewidth = 0.6),
        axis.line.y = element_line(color = "black", linewidth = 0.6),
        legend.title = element_text(size = 14, face = "bold"),
        legend.text = element_text(size = 10),
        legend.key.size = unit(0.5, "lines"),
        legend.position = "bottom", 
        legend.box = "horizontal",
        legend.spacing.x = unit(0.02, "cm"),
        legend.margin = margin(t = 5, b = 0),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        plot.margin = margin(15, 20, 15, 15),
        plot.background = element_blank()
      )
    
    if (sex == "Female" || sex == "Females") {
      p <- p + guides(
        size = guide_legend(
          title = "AAPC (%)",
          title.position = "left", 
          title.hjust = 0.5,
          override.aes = list(
            fill = size_colors,
            size = scales::rescale(c(1, 0.7, 0, 0.7, 1), to = c(4, 20)),
            color = "white"
          ),
          nrow = 1,
          label.theme = element_text(size = 12),
          keywidth = unit(0.3, "cm"),
          keyheight = unit(0.2, "cm")
        )
      )
    } else {
      p <- p + guides(size = "none")
    }
    
    return(p)
  }
  
  # Create plots
  female_plot <- if (!is.null(female_data)) create_single_plot(female_data, "Females") else NULL
  male_plot   <- if (!is.null(male_data))   create_single_plot(male_data, "Males")   else NULL
  
  # Save individual plots
  if (!is.null(save_path)) {
    if (!is.null(female_plot)) ggsave(gsub(".png", "_female.png", save_path), female_plot, width = 10.5, height = 7.5, dpi = 300, bg = "white")
    if (!is.null(male_plot))   ggsave(gsub(".png", "_male.png", save_path), male_plot, width = 10.5, height = 6.5, dpi = 300, bg = "white")
  }
  
  # Create and save combined plot
  if (!is.null(female_plot) && !is.null(male_plot)) {
    combined_plot <- plot_grid(female_plot, male_plot, ncol = 1, rel_heights = c(1, 0.85))
    final_plot <- ggdraw(combined_plot) +
      theme(plot.margin = margin(2, 2, 2, 2))  # No border or background
    
    if (!is.null(save_path)) {
      ggsave(save_path, final_plot, width = 10, height = 14, dpi = 300, bg = "white")
    }
    
    return(list(female = female_plot, male = male_plot, combined = final_plot))
  }
  
  return(list(female = female_plot, male = male_plot))
}


plot_aapc_bubbles(
  female_data = AAPC_combined_females,
  male_data = AAPC_combined_males,
  show_non_significant = FALSE,
  save_path = file.path(base_path_oac, "Figures", "combined_aapc_plot.png")
)
