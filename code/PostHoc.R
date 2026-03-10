# Load libraries 
library(tidyverse) 
library(slider)
library(patchwork)

# Load data
data <- read.csv(file = "~/Documents/GitHub/CSIA_lab_work/data/final/states.csv")
PDO <- read.csv(file = "~/Documents/GitHub/Environmental Data/pdo.csv") # the Pacific Decadal Oscillation available at NOAA National Center for Environmental Information (https://www.ncei.noaa.gov/pub/data/cmb/ersst/v5/index/ersst.v5.pdo.dat )
NPGO <- read.csv(file = "~/Documents/GitHub/Environmental Data/npgo.csv") # NOAA Physical Science Laboratory (https://psl.noaa.gov/data/timeseries/month/DS/NPGO/)


# Plot data
PDO_plot <- ggplot(PDO, aes(x = Year, y = PDO, fill = PDO > 0)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_vline(xintercept = 1977, linetype = "dashed", color = "black") +
  geom_col() +
  scale_x_continuous(breaks = c(1965, 1975, 1985, 1995, 2005, 2015, 2022)) +
  scale_fill_manual(values = c("TRUE" = "grey80", "FALSE" = "grey60")) +
  labs(title = "Pacific Decadal Oscillation (PDO) Index",
       x = "Year",
       y = "PDO Index") +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.line.y = element_blank(),
    plot.title = element_text(hjust = 0.5, family = "Times New Roman"), 
    text = element_text(family = "Times New Roman") 
  ) + 
  annotate("text", x = 1980.5, y = -1.79, label = bquote(bold("1977 Shift")), family = "Times New Roman") + 
  annotate(
    "text",
    x = 1965,
    y = max(PDO$PDO),
    label = "(a)",
    hjust = 0,
    vjust = 0.3,
    size = 5,                
    fontface = "bold",       
    family = "Times New Roman"
  )

NPGO_plot <- ggplot(NPGO, aes(x = Year, y = NPGO, fill = NPGO > 0)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_col() +
  scale_x_continuous(breaks = c(1965, 1975, 1985, 1995, 2005, 2015, 2022)) +
  scale_fill_manual(values = c("TRUE" = "grey80", "FALSE" = "grey60")) +
  labs(title = "North Pacific Gyre Oscillation (NPGO) Index",
       x = "Year",
       y = "NPGO Index") +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.line.y = element_blank(),
    plot.title = element_text(hjust = 0.5, family = "Times New Roman"), 
    text = element_text(family = "Times New Roman") 
  ) +
  annotate(
    "text",
    x = 1965,
    y = max(PDO$PDO),
    label = "(b)",
    hjust = 0,
    vjust = 0.3,
    size = 5,                
    fontface = "bold",       
    family = "Times New Roman"
  )

combo <- PDO_plot / NPGO_plot 


# Format data 
years <- 1965:2022
states <- data %>% 
  select(BB.tp, BB.PHE, BB.GLU)

analysis_df <- tibble(
  Year = years,
  TP_state  = states$BB.tp,
  PHE_state = states$BB.PHE,
  GLU_state = states$BB.GLU
) %>%
  left_join(PDO,  by = "Year") %>%
  left_join(NPGO, by = "Year")

# Scale 
analysis_df <- analysis_df %>%
  mutate(across(
    c(TP_state, PHE_state, PDO, NPGO),
    ~ as.numeric(scale(.))
  ))

# Calculate 10-year rolling correlations (TP)
rolling_corr <- analysis_df %>%
  arrange(Year) %>%
  mutate(
    cor_TP_PDO = slide_dbl(
      .x = seq_along(Year),
      .f = ~ cor(
        TP_state[.x],
        PDO[.x],
        use = "complete.obs"
      ),
      .before = 9,   # 10-year window
      .complete = TRUE
    ),
    
    cor_TP_NPGO = slide_dbl(
      .x = seq_along(Year),
      .f = ~ cor(
        TP_state[.x],
        NPGO[.x],
        use = "complete.obs"
      ),
      .before = 9,
      .complete = TRUE
    )
  )

# Plot 
TP <- ggplot(rolling_corr, aes(x = Year)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  
  # Moderate
  geom_hline(yintercept = c(-0.4, 0.4),
             linetype = "dotted",
             linewidth = 0.6,
             color = "grey40") +
  
  # Strong
  geom_hline(yintercept = c(-0.6, 0.6),
             linetype = "longdash",
             linewidth = 0.8,
             color = "grey40") +
  
  geom_line(aes(y = cor_TP_PDO, color = "PDO"), linewidth = 1) +
  geom_line(aes(y = cor_TP_NPGO, color = "NPGO"), linewidth = 1) +
  scale_color_manual(values = c(
    "PDO"  = "steelblue",
    "NPGO" = "firebrick"
  )) +
  
  labs(
    title = "Trophic Position",
    y = "10-year rolling correlation",
    x = "Year",
    color = NULL
  ) + 
  theme_classic() +
  theme(axis.title.x = element_blank(),
        plot.title = element_text(hjust = 0.5, family = "Times New Roman"), 
        text = element_text(family = "Times New Roman"),
        axis.line.y = element_blank(),
        plot.margin = margin(10, 10, 10, 10)
  ) + annotate(
    "text",
    x = -Inf, y = Inf,
    label = "a)",
    hjust = -0.2, vjust = 1.2,
    size = 5,
    family = "Times New Roman",
    fontface = "bold"
  )

# Calculate 10-year rolling correlations (PHE)
rolling_corr_PHE <- analysis_df %>%
  arrange(Year) %>%
  mutate(
    cor_PHE_PDO = slide_dbl(
      .x = seq_along(Year),
      .f = ~ cor(
        PHE_state[.x],
        PDO[.x],
        use = "complete.obs"
      ),
      .before = 9,   # 10-year window
      .complete = TRUE
    ),
    
    cor_PHE_NPGO = slide_dbl(
      .x = seq_along(Year),
      .f = ~ cor(
        PHE_state[.x],
        NPGO[.x],
        use = "complete.obs"
      ),
      .before = 9,
      .complete = TRUE
    )
  )

# Plot 
PHE <- ggplot(rolling_corr_PHE, aes(x = Year)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  
  # Moderate
  geom_hline(yintercept = c(-0.4, 0.4),
             linetype = "dotted",
             linewidth = 0.6,
             color = "grey40") +
  
  # Strong
  geom_hline(yintercept = c(-0.6, 0.6),
             linetype = "longdash",
             linewidth = 0.8,
             color = "grey40") +
  
  geom_line(aes(y = cor_PHE_PDO,  color = "PDO"),  linewidth = 1) +
  geom_line(aes(y = cor_PHE_NPGO, color = "NPGO"), linewidth = 1) +
  scale_color_manual(values = c(
    "PDO"  = "steelblue",
    "NPGO" = "firebrick"
  )) + 
    labs(title = "Phenylalanine - Baseline",
    y = "10-year rolling correlation",
    x = "Year",
    color = NULL
  ) + 
  theme_classic() +
  theme(axis.title.x = element_blank(),
        plot.title = element_text(hjust = 0.5, family = "Times New Roman"), 
        text = element_text(family = "Times New Roman"),
        axis.line.y = element_blank(),
        plot.margin = margin(10, 10, 10, 10)
  )+ annotate(
    "text",
    x = -Inf, y = Inf,
    label = "b)",
    hjust = -0.2, vjust = 1.2,
    size = 5,
    family = "Times New Roman",
    fontface = "bold"
  )

# Correlation between TP and PHE 
rolling_corr_states <- analysis_df %>%
  arrange(Year) %>%
  mutate(
    cor_TP_PHE = slide_dbl(
      .x = seq_along(Year),
      .f = ~ cor(
        TP_state[.x],
        PHE_state[.x],
        use = "complete.obs"
      ),
      .before = 9,   # 10-year window
      .complete = TRUE
    )
  )

# Plot 
corr <- ggplot(rolling_corr_states, aes(x = Year, y = cor_TP_PHE)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  
  # Moderate
  geom_hline(yintercept = c(-0.4, 0.4),
             linetype = "dotted",
             linewidth = 0.6,
             color = "grey40") +
  
  # Strong
  geom_hline(yintercept = c(-0.6, 0.6),
             linetype = "longdash",
             linewidth = 0.8,
             color = "grey40") +
  geom_line(color = "darkgreen", linewidth = 1) +
  labs(
    title = "Trophic Position – Phenylalanine coupling",
    y = "10-year rolling correlation",
    x = "Year"
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5, family = "Times New Roman"),
    text = element_text(family = "Times New Roman"),
    axis.line.y = element_blank()
  )

combined <- TP / PHE




