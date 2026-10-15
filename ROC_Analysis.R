library(ggplot2)
library(dplyr)
library(pROC)
library(rpart)
library(tidyr)
library(rms)

#Reading Dataset
ROC_Data = read.csv(<Data Souce Here>)

##Summary Statistics of Number of Visits
n_mri <- tapply(ROC_Data$redcap_repeat_instance.ImgMetrics_MPRAGE, ROC_Data$ptid,
                max, na.rm = TRUE)
summary(n_mri)            # gives Median, Min, Max
length(n_mri)



class(ROC_Data$img_metrics_scan_date.ImgMetrics_MPRAGE)
class(ROC_Data$Date_Visit.Header_all)
head(ROC_Data$img_metrics_scan_date.ImgMetrics_MPRAGE)
head(ROC_Data$Date_Visit.Header_all)

ROC_Data <- ROC_Data %>%
  group_by(ptid) %>%
  filter(redcap_repeat_instance.ImgMetrics_MPRAGE == max(redcap_repeat_instance.ImgMetrics_MPRAGE)) %>%
  ungroup()

head(ROC_Data)

d <- ROC_Data
nrow(d)                                   # should be 465

mri <- as.Date(ROC_Data$img_metrics_scan_date.ImgMetrics_MPRAGE)
vis <- ROC_Data$Date_Visit.Header_all
vis[vis == ""] <- NA
dx  <- as.Date(vis)

sum(is.na(mri)); sum(is.na(dx))           # how many dates are missing

gap <- abs(as.numeric(difftime(dx, mri, units = "days")))

sum(!is.na(gap))                          # participants with both dates
median(gap, na.rm = TRUE)
quantile(gap, c(0.25, 0.75), na.rm = TRUE)
max(gap, na.rm = TRUE)
sum(gap > 365, na.rm = TRUE)

# Keep variables of interest
ROC_Data1 <- ROC_Data[, c("ptid", "gm_mm3.ImgMetrics_MPRAGE", "wm_mm3.ImgMetrics_MPRAGE", "ad_signature_pctgm.ImgMetrics_MPRAGE", "hoc_avg.ImgMetrics_MPRAGE", "primary_Dx.Header_all",
                          "wmh_mm3.ImgMetrics_MPRAGE", "Age_atVisit.Header_all", "prestat.SPDemog_A1", "sex.SPDemog_A1", "race.SPDemog_A1", "educ.SPDemog_A1", "ticv_mm3.ImgMetrics_MPRAGE")]


#Checking the type for Columns
str(ROC_Data1)

#Treating primary_Dx.Header as factor
ROC_Data1$primary_Dx.Header_all <- factor(ROC_Data1$primary_Dx.Header_all)
class(ROC_Data1$primary_Dx.Header_all)




# Changing Variable Names
ROC_Data1 <- ROC_Data1[, c(
  "ptid",
  "gm_mm3.ImgMetrics_MPRAGE",
  "wm_mm3.ImgMetrics_MPRAGE",
  "ad_signature_pctgm.ImgMetrics_MPRAGE",
  "hoc_avg.ImgMetrics_MPRAGE",
  "primary_Dx.Header_all",
  "wmh_mm3.ImgMetrics_MPRAGE",
  "Age_atVisit.Header_all",
  "prestat.SPDemog_A1",
  "sex.SPDemog_A1",
  "race.SPDemog_A1",
  "educ.SPDemog_A1",
  "ticv_mm3.ImgMetrics_MPRAGE"
)]

names(ROC_Data1) <- c(
  "ptid",
  "GM",
  "WM",
  "AD_Signature",
  "HOC_avg_norm",
  "Cognitive_Status",
  "WMH",
  "Age_at_Visit",
  "Presumed_Disease_Status_at_Enrollment",
  "Sex",
  "Race",
  "Education_Level",
  "TICV"
)

# Delete rows with empty cells for Cognitive_Status.
ROC_Data1 <- ROC_Data1 %>%
  filter(!is.na(Cognitive_Status) & Cognitive_Status != "")

# Distribution for the levels of Cognitive Status
table(ROC_Data1$Cognitive_Status)

#Checking the type for Columns
str(ROC_Data1)

#Presumed_Disease_Status_at_Enrollment, Sex and Race as factor Variables
ROC_Data1$Presumed_Disease_Status_at_Enrollment <- factor(ROC_Data1$Presumed_Disease_Status_at_Enrollment)
ROC_Data1$Sex <- factor(ROC_Data1$Sex)
ROC_Data1$Race <- factor(ROC_Data1$Race)

class(ROC_Data1$Presumed_Disease_Status_at_Enrollment)
class(ROC_Data1$Sex)
class(ROC_Data1$Race)

# Age range (min–max) by group, rounded to whole years
age_range <- aggregate(Age_at_Visit ~ Cognitive_Status, data = ROC_Data1,
                       FUN = function(x) c(min = floor(min(x)), max = floor(max(x)), n = length(x)))
print(age_range)

sort(ROC_Data1$Age_at_Visit[ROC_Data1$Cognitive_Status == "NL"])[1:3]

# Check: any NL participant younger than 60?
sum(mydata$age[mydata$Cognitive_Status == "NL"] < 60, na.rm = TRUE)

#Descriptives for Continuous Variables across the levels of Cognitive Status
summary_table <- ROC_Data1 %>%
  group_by(Cognitive_Status) %>%
  summarise(
    GM_N = sum(!is.na(GM)),
    GM_Mean = mean(GM, na.rm = TRUE),
    GM_SD = sd(GM, na.rm = TRUE),
   
    
    WM_N = sum(!is.na(WM)),
    WM_Mean = mean(WM, na.rm = TRUE),
    WM_SD = sd(WM, na.rm = TRUE),
    
    
    AD_Signature_N = sum(!is.na(AD_Signature)),
    AD_Signature_Mean = mean(AD_Signature, na.rm = TRUE),
    AD_Signature_SD = sd(AD_Signature, na.rm = TRUE),
    
    
    HOC_avg_norm_N = sum(!is.na(HOC_avg_norm)),
    HOC_avg_norm_Mean = mean(HOC_avg_norm, na.rm = TRUE),
    HOC_avg_norm_SD = sd(HOC_avg_norm, na.rm = TRUE)
    
  )

summary_table$HOC_avg_norm_N

#Boxplots
plot_data <- ROC_Data1 %>%
  select(Cognitive_Status, GM, WM, AD_Signature, HOC_avg_norm) %>%
  pivot_longer(
    cols = c(GM, WM, AD_Signature, HOC_avg_norm),
    names_to = "Variable",
    values_to = "Value"
  )

ggplot(plot_data, aes(x = Cognitive_Status, y = Value,
                      fill = Cognitive_Status)) +
  geom_boxplot(outlier.shape = 16, outlier.size = 1.5) +
  facet_wrap(~Variable, scales = "free_y") +
  labs(
    title = "Distribution of MRI and AD-Related Measures by Primary Diagnosis",
    x = "Primary Diagnosis",
    y = "Value"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, face = "bold"),
    strip.text = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


############
# BoxPlot Publication Ready #
############

# ----------------------------------------------------------
# 1. Settings (names and order match Table 1)
# ----------------------------------------------------------

dx_levels <- c("NL", "Impaired", "MCI", "Demented")   # as coded in the data
dx_labels <- c("NL", "Impaired", "MCI", "Dementia")   # as shown in Table 1

marker_labels <- c(
  GM           = "Gray matter volume (mm³)",
  WM           = "White matter volume (mm³)",
  WMH          = "White matter hyperintensity (mm³)",
  AD_Signature = "AD signature",
  HOC_avg_norm = "HOC",
  TICV         = "Total intracranial volume (mm³)"
)

pal <- setNames(c("#0072B2", "#56B4E9", "#E69F00", "#D55E00"), dx_labels)


# ----------------------------------------------------------
# 2. Data
# ----------------------------------------------------------

plot_data <- ROC_Data1 %>%
  filter(Cognitive_Status %in% dx_levels) %>%
  mutate(Dx = factor(Cognitive_Status, levels = dx_levels, labels = dx_labels)) %>%
  select(Dx, all_of(names(marker_labels))) %>%
  pivot_longer(-Dx, names_to = "Variable", values_to = "Value") %>%
  filter(!is.na(Value)) %>%
  mutate(Variable = factor(Variable, levels = names(marker_labels),
                           labels = marker_labels))


# ----------------------------------------------------------
# 3. Plot
# ----------------------------------------------------------

p <- ggplot(plot_data, aes(x = Dx, y = Value, fill = Dx)) +
  geom_boxplot(width = 0.6, linewidth = 0.45, colour = "grey15",
               outlier.shape = NA) +
  facet_wrap(~ Variable, scales = "free_y", ncol = 3) +
  scale_fill_manual(values = pal) +
  labs(x = NULL, y = NULL) +
  theme_classic(base_size = 11) +
  theme(
    legend.position  = "none",
    strip.background = element_blank(),
    strip.text       = element_text(face = "bold", size = 10.5, hjust = 0),
    axis.text        = element_text(colour = "black"),
    panel.spacing    = unit(1.1, "lines")
  )

p


# ----------------------------------------------------------
# 4. Export
# ----------------------------------------------------------

ggsave("Figure2_Marker_Boxplots.tiff", p, width = 9, height = 6,
       units = "in", dpi = 600, compression = "lzw")
ggsave("Figure2_Marker_Boxplots.pdf", p, width = 9, height = 6,
       device = cairo_pdf)

ggsave("Figure2_Marker_Boxplots.png", p, width = 9, height = 6, dpi = 300)

##Anova and Tukey HSD Plots
anova_GM <- aov(GM ~ Cognitive_Status, data = ROC_Data1)
summary(anova_GM)

anova_WM <- aov(WM ~ Cognitive_Status, data = ROC_Data1)
summary(anova_WM)

anova_WMH <- aov(WMH ~ Cognitive_Status, data = ROC_Data1)
summary(anova_WMH)

anova_HOC <- aov(HOC_avg_norm ~ Cognitive_Status, data = ROC_Data1)
summary(anova_HOC)

anova_AD <- aov(AD_Signature ~ Cognitive_Status, data = ROC_Data1)
summary(anova_AD)


tukey_GM <- TukeyHSD(anova_GM)
tukey_WM <- TukeyHSD(anova_WM)
tukey_WMH <- TukeyHSD(anova_WMH)
tukey_HOC <- TukeyHSD(anova_HOC)
tukey_AD <- TukeyHSD(anova_AD)


get_tukey <- function(tukey_result, biomarker) {
  
  x <- as.data.frame(tukey_result$Cognitive_Status)
  
  x$Comparison <- rownames(x)
  x$Biomarker <- biomarker
  
  rownames(x) <- NULL
  
  x
}

tukey_data <- bind_rows(
  get_tukey(tukey_GM, "GM"),
  get_tukey(tukey_WM, "WM"),
  get_tukey(tukey_WMH, "WMH"),
  get_tukey(tukey_HOC, "HOC"),
  get_tukey(tukey_AD, "AD Signature")
)


ggplot(tukey_data, aes(x = diff, y = Comparison)) +
  geom_errorbar(
    aes(xmin = lwr, xmax = upr),
    orientation = "y",
    height = 0.15
  ) +
  geom_point(size = 2) +
  geom_vline(
    xintercept = 0,
    linetype = "dashed"
  ) +
  facet_wrap(
    ~ Biomarker,
    nrow = 1,
    scales = "free_x"
  ) +
  labs(
    title = "Tukey HSD Comparisons by Cognitive Status",
    x = "Difference in Mean",
    y = NULL
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    strip.text = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

# Merging MCI and Demented (MCI_Demeneted) vrs NL
ROC_Data2 <- ROC_Data1 %>%
  filter(Cognitive_Status %in% c("MCI", "Demented", "NL")) %>%
  mutate(
    Cognitive_Status_New = case_when(
      Cognitive_Status %in% c("MCI", "Demented") ~ "MCI_Demented",
      Cognitive_Status == "NL" ~ "NL"
    )
  )

# Distribution for the levels of Cognitive Status New
table(ROC_Data2$Cognitive_Status_New)

# Summary Statistics (Mean and SD) for the Merged Data; MCI and Demented (MCI_Demeneted) vrs NL
summary_table <- ROC_Data2 %>%
  group_by(Cognitive_Status_New) %>%
  summarise(
    GM_N = sum(!is.na(GM)),
    GM_Mean = mean(GM, na.rm = TRUE),
    GM_SD = sd(GM, na.rm = TRUE),
    
    
    WM_N = sum(!is.na(WM)),
    WM_Mean = mean(WM, na.rm = TRUE),
    WM_SD = sd(WM, na.rm = TRUE),
    
    
    AD_Signature_N = sum(!is.na(AD_Signature)),
    AD_Signature_Mean = mean(AD_Signature, na.rm = TRUE),
    AD_Signature_SD = sd(AD_Signature, na.rm = TRUE),
    
    
    HOC_avg_norm_N = sum(!is.na(HOC_avg_norm)),
    HOC_avg_norm_Mean = mean(HOC_avg_norm, na.rm = TRUE),
    HOC_avg_norm_SD = sd(HOC_avg_norm, na.rm = TRUE),
    
    
    Age_at_Visit_N = sum(!is.na(Age_at_Visit)),
    Age_at_Visit_Mean = mean(Age_at_Visit, na.rm = TRUE),
    Age_at_Visit_SD = sd(Age_at_Visit, na.rm = TRUE),
    
    
    Education_Level_N = sum(!is.na(Education_Level)),
    Education_Level_Mean = mean(Education_Level, na.rm = TRUE),
    Education_Level_SD = sd(Education_Level, na.rm = TRUE),
    
    
    WMH_N = sum(!is.na(WMH)),
    WMH_Mean = mean(WMH, na.rm = TRUE),
    WMH_SD = sd(WMH, na.rm = TRUE),
  )

summary_table$HOC_avg_norm_SD


# Distribution of Categorical Variables
table(ROC_Data2$Sex, ROC_Data2$Cognitive_Status_New)
table(ROC_Data2$Race, ROC_Data2$Cognitive_Status_New)
table(ROC_Data2$Presumed_Disease_Status_at_Enrollment, ROC_Data2$Cognitive_Status_New)


#ANOVA For Table 1
anova_result <- aov(Age_at_Visit ~ Cognitive_Status_New, data = ROC_Data2)
summary(anova_result)

anova_result <- aov(Education_Level ~ Cognitive_Status_New, data = ROC_Data2)
summary(anova_result)

#Categorical Variable Analyses for Table 1
tab <- table(ROC_Data2$Race, ROC_Data2$Cognitive_Status_New)
fisher.test(tab)

chisq.test(table(ROC_Data2$Sex, ROC_Data2$Cognitive_Status_New))

chisq.test(table(ROC_Data2$Presumed_Disease_Status_at_Enrollment, ROC_Data2$Cognitive_Status_New))

#Adjusted Analyses for Table 1
model <- lm(GM ~ Cognitive_Status_New + TICV + Age_at_Visit + Sex + Race + Presumed_Disease_Status_at_Enrollment + Education_Level, data = ROC_Data2)
summary(model)

model <- lm(WM ~ Cognitive_Status_New + TICV + Age_at_Visit + Sex + Race + Presumed_Disease_Status_at_Enrollment + Education_Level , data = ROC_Data2)
summary(model)

model <- lm(WMH ~ Cognitive_Status_New + TICV + Age_at_Visit + Sex + Race + Presumed_Disease_Status_at_Enrollment + Education_Level, data = ROC_Data2)
summary(model)

model <- lm(AD_Signature ~ Cognitive_Status_New + TICV + Age_at_Visit + Sex + Race + Presumed_Disease_Status_at_Enrollment + Education_Level, data = ROC_Data2)
summary(model)

model <- lm(HOC_avg_norm ~ Cognitive_Status_New + TICV+ Age_at_Visit + Sex + Race + Presumed_Disease_Status_at_Enrollment + Education_Level, data = ROC_Data2)
summary(model)

# Creating MCI vrs Demented Dataset
ROC_Data3 <- ROC_Data1 %>%
  filter(Cognitive_Status %in% c("MCI", "Demented")) %>%
  droplevels()

# Distribution for the levels of Cognitive Status New
table(ROC_Data3$Cognitive_Status)

# Summary Statistics (Mean and SD) for the MCI vrs Demented Dataset
summary_table <- ROC_Data3 %>%
  group_by(Cognitive_Status) %>%
  summarise(
    GM_N = sum(!is.na(GM)),
    GM_Mean = mean(GM, na.rm = TRUE),
    GM_SD = sd(GM, na.rm = TRUE),
    
    
    WM_N = sum(!is.na(WM)),
    WM_Mean = mean(WM, na.rm = TRUE),
    WM_SD = sd(WM, na.rm = TRUE),
    
    
    AD_Signature_N = sum(!is.na(AD_Signature)),
    AD_Signature_Mean = mean(AD_Signature, na.rm = TRUE),
    AD_Signature_SD = sd(AD_Signature, na.rm = TRUE),
    
    
    HOC_avg_norm_N = sum(!is.na(HOC_avg_norm)),
    HOC_avg_norm_Mean = mean(HOC_avg_norm, na.rm = TRUE),
    HOC_avg_norm_SD = sd(HOC_avg_norm, na.rm = TRUE),
    
    
    Age_at_Visit_N = sum(!is.na(Age_at_Visit)),
    Age_at_Visit_Mean = mean(Age_at_Visit, na.rm = TRUE),
    Age_at_Visit_SD = sd(Age_at_Visit, na.rm = TRUE),
    
    
    Education_Level_N = sum(!is.na(Education_Level)),
    Education_Level_Mean = mean(Education_Level, na.rm = TRUE),
    Education_Level_SD = sd(Education_Level, na.rm = TRUE),
    
    
    WMH_N = sum(!is.na(WMH)),
    WMH_Mean = mean(WMH, na.rm = TRUE),
    WMH_SD = sd(WMH, na.rm = TRUE),
  )

summary_table$HOC_avg_norm_SD

# Distribution of Categorical Variables
table(ROC_Data3$Sex, ROC_Data3$Cognitive_Status)
table(ROC_Data3$Race, ROC_Data3$Cognitive_Status)
table(ROC_Data3$Presumed_Disease_Status_at_Enrollment, ROC_Data3$Cognitive_Status)

#ANOVA For Table 1
anova_result <- aov(Age_at_Visit ~ Cognitive_Status, data = ROC_Data3)
summary(anova_result)

anova_result <- aov(Education_Level ~ Cognitive_Status, data = ROC_Data3)
summary(anova_result)

#Categorical Variable Analyses for Table 1
tab <- table(ROC_Data3$Race, ROC_Data3$Cognitive_Status)
fisher.test(tab)

chisq.test(table(ROC_Data3$Sex, ROC_Data3$Cognitive_Status))

tab= table(ROC_Data3$Presumed_Disease_Status_at_Enrollment, ROC_Data3$Cognitive_Status)
fisher.test(tab)

#Adjusted Analyses for Table 1
model <- lm(GM ~ Cognitive_Status + TICV + Age_at_Visit + Sex + Race + Presumed_Disease_Status_at_Enrollment + Education_Level, data = ROC_Data3)
summary(model)

model <- lm(WM ~ Cognitive_Status + TICV + Age_at_Visit + Sex + Race + Presumed_Disease_Status_at_Enrollment + Education_Level, data = ROC_Data3)
summary(model)

model <- lm(WMH ~ Cognitive_Status + TICV + Age_at_Visit + Sex + Race + Presumed_Disease_Status_at_Enrollment + Education_Level, data = ROC_Data3)
summary(model)

model <- lm(AD_Signature ~ Cognitive_Status + TICV + Age_at_Visit + Sex + Race + Presumed_Disease_Status_at_Enrollment + Education_Level, data = ROC_Data3)
summary(model)

model <- lm(HOC_avg_norm ~ Cognitive_Status + TICV + Age_at_Visit + Sex + Race + Presumed_Disease_Status_at_Enrollment + Education_Level, data = ROC_Data3)
summary(model)


# Creating Demented vrs NL Dataset
ROC_Data4 <- ROC_Data1 %>%
  filter(Cognitive_Status %in% c("NL", "Demented")) %>%
  droplevels()

# Distribution for the levels of Cognitive Status New
table(ROC_Data4$Cognitive_Status)

# Summary Statistics (Mean and SD) for the NL vrs Demented Dataset
summary_table <- ROC_Data4 %>%
  group_by(Cognitive_Status) %>%
  summarise(
    GM_N = sum(!is.na(GM)),
    GM_Mean = mean(GM, na.rm = TRUE),
    GM_SD = sd(GM, na.rm = TRUE),
    
    
    WM_N = sum(!is.na(WM)),
    WM_Mean = mean(WM, na.rm = TRUE),
    WM_SD = sd(WM, na.rm = TRUE),
    
    
    AD_Signature_N = sum(!is.na(AD_Signature)),
    AD_Signature_Mean = mean(AD_Signature, na.rm = TRUE),
    AD_Signature_SD = sd(AD_Signature, na.rm = TRUE),
    
    
    HOC_avg_norm_N = sum(!is.na(HOC_avg_norm)),
    HOC_avg_norm_Mean = mean(HOC_avg_norm, na.rm = TRUE),
    HOC_avg_norm_SD = sd(HOC_avg_norm, na.rm = TRUE),
    
    
    Age_at_Visit_N = sum(!is.na(Age_at_Visit)),
    Age_at_Visit_Mean = mean(Age_at_Visit, na.rm = TRUE),
    Age_at_Visit_SD = sd(Age_at_Visit, na.rm = TRUE),
    
    
    Education_Level_N = sum(!is.na(Education_Level)),
    Education_Level_Mean = mean(Education_Level, na.rm = TRUE),
    Education_Level_SD = sd(Education_Level, na.rm = TRUE),
    
    
    WMH_N = sum(!is.na(WMH)),
    WMH_Mean = mean(WMH, na.rm = TRUE),
    WMH_SD = sd(WMH, na.rm = TRUE),
  )

summary_table$Age_at_Visit

# Distribution of Categorical Variables
table(ROC_Data4$Sex, ROC_Data4$Cognitive_Status)
table(ROC_Data4$Race, ROC_Data4$Cognitive_Status)
table(ROC_Data4$Presumed_Disease_Status_at_Enrollment, ROC_Data4$Cognitive_Status)

#ANOVA For Table 1
anova_result <- aov(Age_at_Visit ~ Cognitive_Status, data = ROC_Data4)
summary(anova_result)

anova_result <- aov(Education_Level ~ Cognitive_Status, data = ROC_Data4)
summary(anova_result)

#Categorical Variable Analyses for Table 1
tab <- table(ROC_Data4$Race, ROC_Data4$Cognitive_Status)
fisher.test(tab)

chisq.test(table(ROC_Data4$Sex, ROC_Data4$Cognitive_Status))

chisq.test(table(ROC_Data4$Presumed_Disease_Status_at_Enrollment, ROC_Data4$Cognitive_Status))

#Adjusted Analyses for Table 1
model <- lm(GM ~ Cognitive_Status + TICV + Age_at_Visit + Sex + Race + Presumed_Disease_Status_at_Enrollment + Education_Level, data = ROC_Data4)
summary(model)

model <- lm(WM ~ Cognitive_Status + TICV + Age_at_Visit + Sex + Race + Presumed_Disease_Status_at_Enrollment + Education_Level, data = ROC_Data4)
summary(model)

model <- lm(WMH ~ Cognitive_Status + TICV + Age_at_Visit + Sex + Race + Presumed_Disease_Status_at_Enrollment + Education_Level, data = ROC_Data4)
summary(model)

model <- lm(AD_Signature ~ Cognitive_Status + TICV + Age_at_Visit + Sex + Race + Presumed_Disease_Status_at_Enrollment + Education_Level, data = ROC_Data4)
summary(model)

model <- lm(HOC_avg_norm ~ Cognitive_Status + TICV + Age_at_Visit + Sex + Race + Presumed_Disease_Status_at_Enrollment + Education_Level, data = ROC_Data4)
summary(model)

## Summary Statistics
GM_summary <- ROC_Data4 %>%
  group_by(Cognitive_Status) %>%
  summarise(
    N = sum(!is.na(GM)),
    Mean = mean(GM, na.rm = TRUE),
    SD = sd(GM, na.rm = TRUE),
    Median = median(GM, na.rm = TRUE),
    IQR = IQR(GM, na.rm = TRUE),
    Q1 = quantile(GM, 0.25, na.rm = TRUE),
    Q3 = quantile(GM, 0.75, na.rm = TRUE),
    Min = min(GM, na.rm = TRUE),
    Max = max(GM, na.rm = TRUE)
  )

GM_summary$Max

# ==========================================================
# UNIVARIATE ROC ANALYSIS: ALL MARKERS
# NL vs Demented
# Youden cutoff + stratified bootstrap internal validation
# ==========================================================



# ----------------------------------------------------------
# 1. Settings
# ----------------------------------------------------------

data_in     <- ROC_Data4                 # dataset
outcome_col <- "Cognitive_Status"        # outcome column
NEG         <- "NL"                      # negative (control) level
POS         <- "Demented"                # positive (case) level
POS_label   <- "Dementia"                # label used in tables
file_prefix <- "ROC_NL_vs_Dementia"      # prefix for saved files

# Direction for each marker (prespecified):
#   ">"  lower values indicate dementia   (marker <= cutoff -> case)
#   "<"  higher values indicate dementia  (marker >= cutoff -> case)

markers <- c(
  GM           = ">",
  WM           = ">",
  WMH          = "<",
  AD_Signature = ">",
  HOC_avg_norm = ">"
)

B    <- 1000   # bootstrap replicates per marker
seed <- 123    # same seed for every marker (reproducible, order-independent)

# ----------------------------------------------------------
# 2. Helper functions
# ----------------------------------------------------------

run_roc <- function(x, status, direction) {
  roc(
    response  = status,
    predictor = x,
    levels    = c("NL", "Demented"),
    direction = direction,
    quiet     = TRUE
  )
}

# Youden-optimal cutoff; keeps the first if several tie
get_youden <- function(roc_obj) {
  best <- coords(
    roc_obj,
    x = "best",
    best.method = "youden",
    ret = "threshold",
    transpose = FALSE
  )
  list(
    cutoff = as.numeric(best$threshold[1]),
    n_ties = nrow(best)
  )
}

# Classify as Demented according to the marker's direction
predict_demented <- function(x, cutoff, direction) {
  if (direction == ">") x <= cutoff else x >= cutoff
}

# Performance of a fixed cutoff (NA when a denominator is 0)
perf_at_cutoff <- function(x, status, cutoff, direction) {
  
  pred_dem <- predict_demented(x, cutoff, direction)
  true_dem <- status == "Demented"
  
  TP <- sum(pred_dem & true_dem)
  FN <- sum(!pred_dem & true_dem)
  TN <- sum(!pred_dem & !true_dem)
  FP <- sum(pred_dem & !true_dem)
  
  safe_div <- function(a, b) if (b > 0) a / b else NA_real_
  
  c(
    sens = safe_div(TP, TP + FN),
    spec = safe_div(TN, TN + FP),
    PPV  = safe_div(TP, TP + FP),
    NPV  = safe_div(TN, TN + FN),
    TP = TP, FN = FN, TN = TN, FP = FP
  )
}

metric_keys <- c("sens", "spec", "PPV", "NPV")

out_names <- c(
  "AUC_boot", "cutoff",
  "sens_boot", "spec_boot", "PPV_boot", "NPV_boot",
  "sens_test", "spec_test", "PPV_test", "NPV_test",
  "AUC_oob",
  "sens_oob",  "spec_oob",  "PPV_oob",  "NPV_oob",
  "n_oob"
)


# ----------------------------------------------------------
# 3. One stratified bootstrap replicate
# ----------------------------------------------------------
#
# Re-derives the Youden cutoff in the bootstrap sample, then
# scores it in: the bootstrap sample (apparent), the original
# sample (test), and out-of-bag participants (OOB).
# ----------------------------------------------------------

boot_one <- function(x, status, direction) {
  
  idx_NL  <- which(status == "NL")
  idx_Dem <- which(status == "Demented")
  
  boot_idx <- c(
    idx_NL[sample.int(length(idx_NL), replace = TRUE)],
    idx_Dem[sample.int(length(idx_Dem), replace = TRUE)]
  )
  
  oob_idx <- setdiff(seq_along(x), boot_idx)
  
  tryCatch({
    
    xb <- x[boot_idx]
    sb <- status[boot_idx]
    
    r_boot   <- run_roc(xb, sb, direction)
    auc_boot <- as.numeric(auc(r_boot))
    cut_boot <- get_youden(r_boot)$cutoff
    
    p_boot <- perf_at_cutoff(xb, sb, cut_boot, direction)
    p_test <- perf_at_cutoff(x, status, cut_boot, direction)
    
    xo <- x[oob_idx]
    so <- status[oob_idx]
    
    p_oob <- perf_at_cutoff(xo, so, cut_boot, direction)
    
    auc_oob <- if (all(c("NL", "Demented") %in% so)) {
      as.numeric(auc(run_roc(xo, so, direction)))
    } else {
      NA_real_
    }
    
    out <- c(
      auc_boot, cut_boot,
      p_boot[metric_keys],
      p_test[metric_keys],
      auc_oob,
      p_oob[metric_keys],
      length(oob_idx)
    )
    names(out) <- out_names
    out
    
  }, error = function(e) {
    setNames(rep(NA_real_, length(out_names)), out_names)
  })
}


# ----------------------------------------------------------
# 4. Full analysis for one marker
# ----------------------------------------------------------

analyze_marker <- function(data, marker, direction, B, seed) {
  
  if (!marker %in% names(data)) {
    stop("Column '", marker, "' not found in the data.")
  }
  
  # Analysis sample (complete cases for this marker)
  d <- data %>%
    filter(
      Cognitive_Status %in% c("NL", "Demented"),
      !is.na(.data[[marker]])
    ) %>%
    mutate(
      Cognitive_Status = factor(
        Cognitive_Status,
        levels = c("NL", "Demented")
      )
    )
  
  x      <- d[[marker]]
  status <- d$Cognitive_Status
  
  if (!is.numeric(x)) stop(marker, " must be numeric.")
  
  N_NL  <- sum(status == "NL")
  N_Dem <- sum(status == "Demented")
  
  if (N_NL == 0 || N_Dem == 0) {
    stop(marker, ": need participants in both groups.")
  }
  
  # Full-sample ROC, AUC and DeLong CI
  roc_full <- run_roc(x, status, direction)
  auc_full <- as.numeric(auc(roc_full))
  ci_full  <- as.numeric(ci.auc(roc_full, conf.level = 0.95))
  
  # Youden cutoff and apparent performance
  yd <- get_youden(roc_full)
  
  if (yd$n_ties > 1) {
    cat("  Note:", yd$n_ties,
        "thresholds tie on Youden; using the first.\n")
  }
  
  p_full <- perf_at_cutoff(x, status, yd$cutoff, direction)
  
  # Bootstrap
  set.seed(seed)
  boot <- as.data.frame(t(replicate(B, boot_one(x, status, direction))))
  
  valid   <- !is.na(boot$AUC_boot)
  boot_ok <- boot[valid, ]
  
  # Optimism (Harrell) and corrected estimates
  apparent <- c(AUC = auc_full, p_full[metric_keys])
  
  optimism <- c(
    AUC = mean(boot_ok$AUC_boot - auc_full),
    sapply(metric_keys, function(m)
      mean(boot_ok[[paste0(m, "_boot")]] - boot_ok[[paste0(m, "_test")]],
           na.rm = TRUE))
  )
  
  corrected <- apparent - optimism
  
  # Out-of-bag summaries
  oob_keys <- c(AUC = "AUC_oob",
                setNames(paste0(metric_keys, "_oob"), metric_keys))
  
  oob_mean <- sapply(oob_keys, function(v) mean(boot_ok[[v]], na.rm = TRUE))
  oob_lo   <- sapply(oob_keys, function(v)
    quantile(boot_ok[[v]], 0.025, na.rm = TRUE, names = FALSE))
  oob_hi   <- sapply(oob_keys, function(v)
    quantile(boot_ok[[v]], 0.975, na.rm = TRUE, names = FALSE))
  
  # Cutoff stability
  cut_ci <- quantile(boot_ok$cutoff, c(0.025, 0.975),
                     na.rm = TRUE, names = FALSE)
  
  # One row of numeric results
  summary_row <- data.frame(
    Marker    = marker,
    Direction = direction,
    N         = length(x),
    N_NL      = N_NL,
    N_Dem     = N_Dem,
    AUC       = auc_full,
    AUC_lo    = ci_full[1],
    AUC_hi    = ci_full[3],
    Cutoff    = yd$cutoff,
    Cutoff_lo = cut_ci[1],
    Cutoff_hi = cut_ci[2],
    TP = p_full[["TP"]], FP = p_full[["FP"]],
    FN = p_full[["FN"]], TN = p_full[["TN"]],
    stringsAsFactors = FALSE
  )
  
  for (m in c("AUC", metric_keys)) {
    if (m != "AUC") summary_row[[m]] <- apparent[[m]]
    summary_row[[paste0(m, "_optimism")]]  <- optimism[[m]]
    summary_row[[paste0(m, "_corrected")]] <- corrected[[m]]
    summary_row[[paste0(m, "_oob")]]       <- oob_mean[[m]]
    summary_row[[paste0(m, "_oob_lo")]]    <- oob_lo[[m]]
    summary_row[[paste0(m, "_oob_hi")]]    <- oob_hi[[m]]
  }
  
  summary_row$Boot_valid  <- sum(valid)
  summary_row$Boot_failed <- sum(!valid)
  summary_row$Mean_n_oob  <- mean(boot_ok$n_oob)
  
  list(summary = summary_row, roc = roc_full, boot = boot)
}


# ----------------------------------------------------------
# 5. Run all markers
# ----------------------------------------------------------

results <- list()

for (m in names(markers)) {
  cat("\nRunning", m, "(direction", markers[[m]], ") ...\n")
  results[[m]] <- analyze_marker(ROC_Data4, m, markers[[m]], B, seed)
  cat("  AUC =", round(results[[m]]$summary$AUC, 3),
      "| cutoff =", signif(results[[m]]$summary$Cutoff, 5), "\n")
}

summary_all <- bind_rows(lapply(results, `[[`, "summary"))

# Full numeric results (one row per marker)
write.csv(summary_all, "ROC_AllMarkers_Numeric.csv", row.names = FALSE)


# ==========================================================
# 6. Publication tables
# ==========================================================

fmt_cut <- function(x) {
  sapply(x, function(v) {
    if (is.na(v)) return("")
    if (abs(v) >= 100) sprintf("%.2f", v)
    else if (abs(v) >= 1) sprintf("%.3f", v)
    else sprintf("%.4f", v)
  })
}

fmt_pct   <- function(p) ifelse(is.na(p), "", sprintf("%.1f", 100 * p))
fmt_pct_n <- function(p, n, N) sprintf("%.1f (%d/%d)", 100 * p, n, N)

s <- summary_all


# ----------------------------------------------------------
# Table 1: apparent (full-sample) performance
# ----------------------------------------------------------

table1 <- data.frame(
  Marker = s$Marker,
  
  `Dementia indicated by` = ifelse(s$Direction == ">",
                                   "Lower values", "Higher values"),
  
  `N (NL/Demented)` = sprintf("%d (%d/%d)", s$N, s$N_NL, s$N_Dem),
  
  `AUC (95% CI)` = sprintf("%.3f (%.3f–%.3f)", s$AUC, s$AUC_lo, s$AUC_hi),
  
  `Cutoff (95% bootstrap interval)` = paste0(
    fmt_cut(s$Cutoff), " (", fmt_cut(s$Cutoff_lo), "–",
    fmt_cut(s$Cutoff_hi), ")"
  ),
  
  `Sensitivity, % (n/N)` = fmt_pct_n(s$sens, s$TP, s$TP + s$FN),
  `Specificity, % (n/N)` = fmt_pct_n(s$spec, s$TN, s$TN + s$FP),
  `PPV, % (n/N)`         = fmt_pct_n(s$PPV,  s$TP, s$TP + s$FP),
  `NPV, % (n/N)`         = fmt_pct_n(s$NPV,  s$TN, s$TN + s$FN),
  
  check.names = FALSE,
  stringsAsFactors = FALSE
)


# ----------------------------------------------------------
# Table 2: internal validation (long format, one block per marker)
# ----------------------------------------------------------

metric_labels <- c(AUC = "AUC", sens = "Sensitivity, %",
                   spec = "Specificity, %", PPV = "PPV, %", NPV = "NPV, %")

table2 <- bind_rows(lapply(seq_len(nrow(s)), function(i) {
  
  r <- s[i, ]
  
  bind_rows(lapply(names(metric_labels), function(m) {
    
    is_auc <- m == "AUC"
    f  <- if (is_auc) function(v) sprintf("%.3f", v) else fmt_pct
    app <- if (is_auc) r$AUC else r[[m]]
    
    data.frame(
      Marker  = if (m == "AUC") r$Marker else "",
      Measure = metric_labels[[m]],
      Apparent = f(app),
      Optimism = if (is_auc) sprintf("%+.3f", r[[paste0(m, "_optimism")]])
      else sprintf("%+.1f", 100 * r[[paste0(m, "_optimism")]]),
      `Optimism-corrected` = f(r[[paste0(m, "_corrected")]]),
      `Out-of-bag, mean (95% interval)` = paste0(
        f(r[[paste0(m, "_oob")]]), " (",
        f(r[[paste0(m, "_oob_lo")]]), "–",
        f(r[[paste0(m, "_oob_hi")]]), ")"
      ),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  }))
}))


# ----------------------------------------------------------
# 7. Display and save
# ----------------------------------------------------------

cat("\nTable 1. Diagnostic performance of each marker (full sample)\n")
print(table1, row.names = FALSE)

cat("\nTable 2. Bootstrap internal validation\n")
print(table2, row.names = FALSE)

cat("\nBootstrap replicates valid/failed per marker:\n")
print(s[, c("Marker", "Boot_valid", "Boot_failed", "Mean_n_oob")],
      row.names = FALSE)

write.csv(table1, "ROC_AllMarkers_Table1_Performance.csv",
          row.names = FALSE, fileEncoding = "UTF-8")

write.csv(table2, "ROC_AllMarkers_Table2_Validation.csv",
          row.names = FALSE, fileEncoding = "UTF-8")


# ==========================================================
# 8. Publication ROC figure
# ==========================================================

roc_list <- lapply(results, `[[`, "roc")

labels <- c(
  GM           = "GM",
  WM           = "WM",
  WMH          = "WMH",
  AD_Signature = "AD signature",
  HOC_avg_norm = "HOC"
)

cols <- c(                      # colour-blind safe (Okabe-Ito)
  HOC_avg_norm = "#CC3399",
  AD_Signature = "#0072B2",
  WMH          = "#E69F00",
  GM           = "#000000",
  WM           = "#009E73"
)

aucs <- sapply(roc_list, function(r) as.numeric(auc(r)))
ord  <- names(sort(aucs, decreasing = TRUE))

legend_text <- sprintf("%s (AUC = %.3f)", labels[ord], aucs[ord])

draw_roc_figure <- function() {
  
  op <- par(mar = c(4.5, 4.8, 1, 1), las = 1, mgp = c(3, 0.7, 0),
            tcl = -0.3, family = "sans")
  on.exit(par(op))
  
  plot(NA, xlim = c(0, 1), ylim = c(0, 1), xaxs = "i", yaxs = "i",
       xlab = "1 \u2013 Specificity", ylab = "Sensitivity",
       axes = FALSE, cex.lab = 1.25)
  
  ticks <- seq(0, 1, by = 0.2)
  axis(1, at = ticks, labels = sprintf("%.1f", ticks), cex.axis = 1.1)
  axis(2, at = ticks, labels = sprintf("%.1f", ticks), cex.axis = 1.1)
  box(bty = "l")
  
  abline(a = 0, b = 1, lty = 2, col = "grey60")
  
  for (m in rev(ord)) {
    r <- roc_list[[m]]
    lines(1 - r$specificities, r$sensitivities,
          col = cols[[m]], lwd = 2.3)
  }
  
  legend("bottomright", legend = legend_text, col = cols[ord],
         lwd = 2.3, seg.len = 2, cex = 1.05, y.intersp = 1.2,
         bty = "n", inset = c(0.02, 0.03))
}

draw_roc_figure()

tiff(paste0(file_prefix, "_Figure.tiff"), width = 6.5, height = 6.5,
     units = "in", res = 600, compression = "lzw")
draw_roc_figure()
dev.off()

png(paste0(file_prefix, "_Figure.png"), width = 6.5, height = 6.5,
    units = "in", res = 300)
draw_roc_figure()
dev.off()


# ----------------------------------------------------------
# 9. ROC curve points (upload this if you want the figure
#    rendered here)
# ----------------------------------------------------------

roc_points <- do.call(rbind, lapply(names(roc_list), function(m) {
  r <- roc_list[[m]]
  data.frame(Marker = m, FPR = 1 - r$specificities,
             Sensitivity = r$sensitivities, AUC = as.numeric(auc(r)))
}))

write.csv(roc_points, paste0(file_prefix, "_curve_points.csv"),
          row.names = FALSE)


# ==========================================================
# UNIVARIATE ROC ANALYSIS: ALL MARKERS
# NL vs MCI + Demented
# Youden cutoff + stratified bootstrap internal validation
# ==========================================================

library(dplyr)
library(pROC)


# ----------------------------------------------------------
# 1. Settings
# ----------------------------------------------------------

data_in     <- ROC_Data2                 # dataset
outcome_col <- "Cognitive_Status_New"    # outcome column
NEG         <- "NL"                      # negative (control) level
POS         <- "MCI_Demented"            # positive (case) level
POS_label   <- "MCI/Dementia"            # label used in tables
file_prefix <- "ROC_NL_vs_MCIDem"        # prefix for saved files

# Direction for each marker (prespecified):
#   ">"  lower values indicate MCI/Dementia  (marker <= cutoff -> case)
#   "<"  higher values indicate MCI/Dementia (marker >= cutoff -> case)

markers <- c(
  GM           = ">",
  WM           = ">",
  WMH          = "<",
  AD_Signature = ">",
  HOC_avg_norm = ">"
)

B    <- 1000   # bootstrap replicates per marker
seed <- 123


# ----------------------------------------------------------
# 2. Helper functions
# ----------------------------------------------------------

run_roc <- function(x, status, direction) {
  roc(
    response  = status,
    predictor = x,
    levels    = c(NEG, POS),
    direction = direction,
    quiet     = TRUE
  )
}

# Youden-optimal cutoff; keeps the first if several tie
get_youden <- function(roc_obj) {
  best <- coords(
    roc_obj,
    x = "best",
    best.method = "youden",
    ret = "threshold",
    transpose = FALSE
  )
  list(
    cutoff = as.numeric(best$threshold[1]),
    n_ties = nrow(best)
  )
}

# Classify as case according to the marker's direction
predict_case <- function(x, cutoff, direction) {
  if (direction == ">") x <= cutoff else x >= cutoff
}

# Performance of a fixed cutoff (NA when a denominator is 0)
perf_at_cutoff <- function(x, status, cutoff, direction) {
  
  pred_pos <- predict_case(x, cutoff, direction)
  true_pos <- status == POS
  
  TP <- sum(pred_pos & true_pos)
  FN <- sum(!pred_pos & true_pos)
  TN <- sum(!pred_pos & !true_pos)
  FP <- sum(pred_pos & !true_pos)
  
  safe_div <- function(a, b) if (b > 0) a / b else NA_real_
  
  c(
    sens = safe_div(TP, TP + FN),
    spec = safe_div(TN, TN + FP),
    PPV  = safe_div(TP, TP + FP),
    NPV  = safe_div(TN, TN + FN),
    TP = TP, FN = FN, TN = TN, FP = FP
  )
}

metric_keys <- c("sens", "spec", "PPV", "NPV")

out_names <- c(
  "AUC_boot", "cutoff",
  "sens_boot", "spec_boot", "PPV_boot", "NPV_boot",
  "sens_test", "spec_test", "PPV_test", "NPV_test",
  "AUC_oob",
  "sens_oob",  "spec_oob",  "PPV_oob",  "NPV_oob",
  "n_oob"
)


# ----------------------------------------------------------
# 3. One stratified bootstrap replicate
# ----------------------------------------------------------

boot_one <- function(x, status, direction) {
  
  idx_neg <- which(status == NEG)
  idx_pos <- which(status == POS)
  
  boot_idx <- c(
    idx_neg[sample.int(length(idx_neg), replace = TRUE)],
    idx_pos[sample.int(length(idx_pos), replace = TRUE)]
  )
  
  oob_idx <- setdiff(seq_along(x), boot_idx)
  
  tryCatch({
    
    xb <- x[boot_idx]
    sb <- status[boot_idx]
    
    r_boot   <- run_roc(xb, sb, direction)
    auc_boot <- as.numeric(auc(r_boot))
    cut_boot <- get_youden(r_boot)$cutoff
    
    p_boot <- perf_at_cutoff(xb, sb, cut_boot, direction)
    p_test <- perf_at_cutoff(x, status, cut_boot, direction)
    
    xo <- x[oob_idx]
    so <- status[oob_idx]
    
    p_oob <- perf_at_cutoff(xo, so, cut_boot, direction)
    
    auc_oob <- if (all(c(NEG, POS) %in% so)) {
      as.numeric(auc(run_roc(xo, so, direction)))
    } else {
      NA_real_
    }
    
    out <- c(
      auc_boot, cut_boot,
      p_boot[metric_keys],
      p_test[metric_keys],
      auc_oob,
      p_oob[metric_keys],
      length(oob_idx)
    )
    names(out) <- out_names
    out
    
  }, error = function(e) {
    setNames(rep(NA_real_, length(out_names)), out_names)
  })
}


# ----------------------------------------------------------
# 4. Full analysis for one marker
# ----------------------------------------------------------

analyze_marker <- function(data, marker, direction, B, seed) {
  
  if (!marker %in% names(data)) {
    stop("Column '", marker, "' not found in the data.")
  }
  if (!outcome_col %in% names(data)) {
    stop("Outcome column '", outcome_col, "' not found in the data.")
  }
  
  # Analysis sample (complete cases for this marker)
  d <- data %>%
    filter(
      .data[[outcome_col]] %in% c(NEG, POS),
      !is.na(.data[[marker]])
    ) %>%
    mutate(
      .status = factor(.data[[outcome_col]], levels = c(NEG, POS))
    )
  
  x      <- d[[marker]]
  status <- d$.status
  
  if (!is.numeric(x)) stop(marker, " must be numeric.")
  
  N_neg <- sum(status == NEG)
  N_pos <- sum(status == POS)
  
  if (N_neg == 0 || N_pos == 0) {
    stop(marker, ": need participants in both groups. Check that ",
         outcome_col, " uses the levels '", NEG, "' and '", POS, "'.")
  }
  
  # Full-sample ROC, AUC and DeLong CI
  roc_full <- run_roc(x, status, direction)
  auc_full <- as.numeric(auc(roc_full))
  ci_full  <- as.numeric(ci.auc(roc_full, conf.level = 0.95))
  
  # Youden cutoff and apparent performance
  yd <- get_youden(roc_full)
  
  if (yd$n_ties > 1) {
    cat("  Note:", yd$n_ties,
        "thresholds tie on Youden; using the first.\n")
  }
  
  p_full <- perf_at_cutoff(x, status, yd$cutoff, direction)
  
  # Bootstrap
  set.seed(seed)
  boot <- as.data.frame(t(replicate(B, boot_one(x, status, direction))))
  
  valid   <- !is.na(boot$AUC_boot)
  boot_ok <- boot[valid, ]
  
  # Optimism (Harrell) and corrected estimates
  apparent <- c(AUC = auc_full, p_full[metric_keys])
  
  optimism <- c(
    AUC = mean(boot_ok$AUC_boot - auc_full),
    sapply(metric_keys, function(m)
      mean(boot_ok[[paste0(m, "_boot")]] - boot_ok[[paste0(m, "_test")]],
           na.rm = TRUE))
  )
  
  corrected <- apparent - optimism
  
  # Out-of-bag summaries
  oob_keys <- c(AUC = "AUC_oob",
                setNames(paste0(metric_keys, "_oob"), metric_keys))
  
  oob_mean <- sapply(oob_keys, function(v) mean(boot_ok[[v]], na.rm = TRUE))
  oob_lo   <- sapply(oob_keys, function(v)
    quantile(boot_ok[[v]], 0.025, na.rm = TRUE, names = FALSE))
  oob_hi   <- sapply(oob_keys, function(v)
    quantile(boot_ok[[v]], 0.975, na.rm = TRUE, names = FALSE))
  
  # Cutoff stability
  cut_ci <- quantile(boot_ok$cutoff, c(0.025, 0.975),
                     na.rm = TRUE, names = FALSE)
  
  # One row of numeric results
  summary_row <- data.frame(
    Marker    = marker,
    Direction = direction,
    N         = length(x),
    N_neg     = N_neg,
    N_pos     = N_pos,
    AUC       = auc_full,
    AUC_lo    = ci_full[1],
    AUC_hi    = ci_full[3],
    Cutoff    = yd$cutoff,
    Cutoff_lo = cut_ci[1],
    Cutoff_hi = cut_ci[2],
    TP = p_full[["TP"]], FP = p_full[["FP"]],
    FN = p_full[["FN"]], TN = p_full[["TN"]],
    stringsAsFactors = FALSE
  )
  
  for (m in c("AUC", metric_keys)) {
    if (m != "AUC") summary_row[[m]] <- apparent[[m]]
    summary_row[[paste0(m, "_optimism")]]  <- optimism[[m]]
    summary_row[[paste0(m, "_corrected")]] <- corrected[[m]]
    summary_row[[paste0(m, "_oob")]]       <- oob_mean[[m]]
    summary_row[[paste0(m, "_oob_lo")]]    <- oob_lo[[m]]
    summary_row[[paste0(m, "_oob_hi")]]    <- oob_hi[[m]]
  }
  
  summary_row$Boot_valid  <- sum(valid)
  summary_row$Boot_failed <- sum(!valid)
  summary_row$Mean_n_oob  <- mean(boot_ok$n_oob)
  
  list(summary = summary_row, roc = roc_full, boot = boot)
}


# ----------------------------------------------------------
# 5. Check outcome levels, then run all markers
# ----------------------------------------------------------

cat("Outcome levels in", outcome_col, ":\n")
print(table(data_in[[outcome_col]], useNA = "ifany"))

results <- list()

for (m in names(markers)) {
  cat("\nRunning", m, "(direction", markers[[m]], ") ...\n")
  results[[m]] <- analyze_marker(data_in, m, markers[[m]], B, seed)
  cat("  AUC =", round(results[[m]]$summary$AUC, 3),
      "| cutoff =", signif(results[[m]]$summary$Cutoff, 5), "\n")
}

summary_all <- bind_rows(lapply(results, `[[`, "summary"))

write.csv(summary_all, paste0(file_prefix, "_Numeric.csv"), row.names = FALSE)


# ==========================================================
# 6. Publication tables
# ==========================================================

fmt_cut <- function(x) {
  sapply(x, function(v) {
    if (is.na(v)) return("")
    if (abs(v) >= 100) sprintf("%.2f", v)
    else if (abs(v) >= 1) sprintf("%.3f", v)
    else sprintf("%.4f", v)
  })
}

fmt_pct   <- function(p) ifelse(is.na(p), "", sprintf("%.1f", 100 * p))
fmt_pct_n <- function(p, n, N) sprintf("%.1f (%d/%d)", 100 * p, n, N)

s <- summary_all


# Table 1: apparent (full-sample) performance

table1 <- data.frame(
  Marker = s$Marker,
  
  `Case indicated by` = ifelse(s$Direction == ">",
                               "Lower values", "Higher values"),
  
  `N (NL/MCI-Dementia)` = sprintf("%d (%d/%d)", s$N, s$N_neg, s$N_pos),
  
  `AUC (95% CI)` = sprintf("%.3f (%.3f–%.3f)", s$AUC, s$AUC_lo, s$AUC_hi),
  
  `Cutoff (95% bootstrap interval)` = paste0(
    ifelse(s$Direction == ">", "≤ ", "≥ "),
    fmt_cut(s$Cutoff), " (", fmt_cut(s$Cutoff_lo), "–",
    fmt_cut(s$Cutoff_hi), ")"
  ),
  
  `Sensitivity, % (n/N)` = fmt_pct_n(s$sens, s$TP, s$TP + s$FN),
  `Specificity, % (n/N)` = fmt_pct_n(s$spec, s$TN, s$TN + s$FP),
  `PPV, % (n/N)`         = fmt_pct_n(s$PPV,  s$TP, s$TP + s$FP),
  `NPV, % (n/N)`         = fmt_pct_n(s$NPV,  s$TN, s$TN + s$FN),
  
  check.names = FALSE,
  stringsAsFactors = FALSE
)


# Table 2: internal validation (one block per marker)

metric_labels <- c(AUC = "AUC", sens = "Sensitivity, %",
                   spec = "Specificity, %", PPV = "PPV, %", NPV = "NPV, %")

table2 <- bind_rows(lapply(seq_len(nrow(s)), function(i) {
  
  r <- s[i, ]
  
  bind_rows(lapply(names(metric_labels), function(m) {
    
    is_auc <- m == "AUC"
    f   <- if (is_auc) function(v) sprintf("%.3f", v) else fmt_pct
    app <- if (is_auc) r$AUC else r[[m]]
    
    data.frame(
      Marker  = if (is_auc) r$Marker else "",
      Measure = metric_labels[[m]],
      Apparent = f(app),
      Optimism = if (is_auc) sprintf("%+.3f", r[[paste0(m, "_optimism")]])
      else sprintf("%+.1f", 100 * r[[paste0(m, "_optimism")]]),
      `Optimism-corrected` = f(r[[paste0(m, "_corrected")]]),
      `Out-of-bag, mean (95% interval)` = paste0(
        f(r[[paste0(m, "_oob")]]), " (",
        f(r[[paste0(m, "_oob_lo")]]), "–",
        f(r[[paste0(m, "_oob_hi")]]), ")"
      ),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  }))
}))


# ----------------------------------------------------------
# 7. Display and save tables
# ----------------------------------------------------------

cat("\nTable 1. Diagnostic performance (NL vs MCI/Dementia)\n")
print(table1, row.names = FALSE)

cat("\nTable 2. Bootstrap internal validation\n")
print(table2, row.names = FALSE)

cat("\nBootstrap replicates valid/failed per marker:\n")
print(s[, c("Marker", "Boot_valid", "Boot_failed", "Mean_n_oob")],
      row.names = FALSE)

write.csv(table1, paste0(file_prefix, "_Table1_Performance.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")

write.csv(table2, paste0(file_prefix, "_Table2_Validation.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")


# ==========================================================
# 8. Publication ROC figure
# ==========================================================

roc_list <- lapply(results, `[[`, "roc")

labels <- c(
  GM           = "GM",
  WM           = "WM",
  WMH          = "WMH",
  AD_Signature = "AD signature",
  HOC_avg_norm = "HOC"
)

cols <- c(                      # colour-blind safe (Okabe-Ito)
  HOC_avg_norm = "#CC3399",
  AD_Signature = "#0072B2",
  WMH          = "#E69F00",
  GM           = "#000000",
  WM           = "#009E73"
)

aucs <- sapply(roc_list, function(r) as.numeric(auc(r)))
ord  <- names(sort(aucs, decreasing = TRUE))

legend_text <- sprintf("%s (AUC = %.3f)", labels[ord], aucs[ord])

draw_roc_figure <- function() {
  
  op <- par(mar = c(4.5, 4.8, 1, 1), las = 1, mgp = c(3, 0.7, 0),
            tcl = -0.3, family = "sans")
  on.exit(par(op))
  
  plot(NA, xlim = c(0, 1), ylim = c(0, 1), xaxs = "i", yaxs = "i",
       xlab = "1 \u2013 Specificity", ylab = "Sensitivity",
       axes = FALSE, cex.lab = 1.25)
  
  ticks <- seq(0, 1, by = 0.2)
  axis(1, at = ticks, labels = sprintf("%.1f", ticks), cex.axis = 1.1)
  axis(2, at = ticks, labels = sprintf("%.1f", ticks), cex.axis = 1.1)
  box(bty = "l")
  
  abline(a = 0, b = 1, lty = 2, col = "grey60")
  
  for (m in rev(ord)) {
    r <- roc_list[[m]]
    lines(1 - r$specificities, r$sensitivities,
          col = cols[[m]], lwd = 2.3)
  }
  
  legend("bottomright", legend = legend_text, col = cols[ord],
         lwd = 2.3, seg.len = 2, cex = 1.05, y.intersp = 1.2,
         bty = "n", inset = c(0.02, 0.03))
}

draw_roc_figure()

tiff(paste0(file_prefix, "_Figure.tiff"), width = 6.5, height = 6.5,
     units = "in", res = 600, compression = "lzw")
draw_roc_figure()
dev.off()

png(paste0(file_prefix, "_Figure.png"), width = 6.5, height = 6.5,
    units = "in", res = 300)
draw_roc_figure()
dev.off()


# ----------------------------------------------------------
# 9. ROC curve points (upload this if you want the figure
#    rendered here)
# ----------------------------------------------------------

roc_points <- do.call(rbind, lapply(names(roc_list), function(m) {
  r <- roc_list[[m]]
  data.frame(Marker = m, FPR = 1 - r$specificities,
             Sensitivity = r$sensitivities, AUC = as.numeric(auc(r)))
}))

write.csv(roc_points, paste0(file_prefix, "_curve_points.csv"),
          row.names = FALSE)


# ==========================================================
# UNIVARIATE ROC ANALYSIS: ALL MARKERS
# MCI vs Demented
# Youden cutoff + stratified bootstrap internal validation
# ==========================================================

library(dplyr)
library(pROC)


# ----------------------------------------------------------
# 1. Settings
# ----------------------------------------------------------

data_in     <- ROC_Data3                 # dataset
outcome_col <- "Cognitive_Status"        # outcome column
NEG         <- "MCI"                     # negative (reference) level
POS         <- "Demented"                # positive (case) level
NEG_label   <- "MCI"                     # labels used in tables
POS_label   <- "Dementia"
file_prefix <- "ROC_MCI_vs_Dem"          # prefix for saved files

# Direction for each marker (prespecified):
#   ">"  lower values indicate Dementia  (marker <= cutoff -> Dementia)
#   "<"  higher values indicate Dementia (marker >= cutoff -> Dementia)

markers <- c(
  GM           = ">",
  WM           = ">",
  WMH          = "<",
  AD_Signature = ">",
  HOC_avg_norm = ">"
)

B    <- 1000   # bootstrap replicates per marker
seed <- 123


# ----------------------------------------------------------
# 2. Helper functions
# ----------------------------------------------------------

run_roc <- function(x, status, direction) {
  roc(
    response  = status,
    predictor = x,
    levels    = c(NEG, POS),
    direction = direction,
    quiet     = TRUE
  )
}

# Youden-optimal cutoff; keeps the first if several tie
get_youden <- function(roc_obj) {
  best <- coords(
    roc_obj,
    x = "best",
    best.method = "youden",
    ret = "threshold",
    transpose = FALSE
  )
  list(
    cutoff = as.numeric(best$threshold[1]),
    n_ties = nrow(best)
  )
}

# Classify as case according to the marker's direction
predict_case <- function(x, cutoff, direction) {
  if (direction == ">") x <= cutoff else x >= cutoff
}

# Performance of a fixed cutoff (NA when a denominator is 0)
perf_at_cutoff <- function(x, status, cutoff, direction) {
  
  pred_pos <- predict_case(x, cutoff, direction)
  true_pos <- status == POS
  
  TP <- sum(pred_pos & true_pos)
  FN <- sum(!pred_pos & true_pos)
  TN <- sum(!pred_pos & !true_pos)
  FP <- sum(pred_pos & !true_pos)
  
  safe_div <- function(a, b) if (b > 0) a / b else NA_real_
  
  c(
    sens = safe_div(TP, TP + FN),
    spec = safe_div(TN, TN + FP),
    PPV  = safe_div(TP, TP + FP),
    NPV  = safe_div(TN, TN + FN),
    TP = TP, FN = FN, TN = TN, FP = FP
  )
}

metric_keys <- c("sens", "spec", "PPV", "NPV")

out_names <- c(
  "AUC_boot", "cutoff",
  "sens_boot", "spec_boot", "PPV_boot", "NPV_boot",
  "sens_test", "spec_test", "PPV_test", "NPV_test",
  "AUC_oob",
  "sens_oob",  "spec_oob",  "PPV_oob",  "NPV_oob",
  "n_oob"
)


# ----------------------------------------------------------
# 3. One stratified bootstrap replicate
# ----------------------------------------------------------

boot_one <- function(x, status, direction) {
  
  idx_neg <- which(status == NEG)
  idx_pos <- which(status == POS)
  
  boot_idx <- c(
    idx_neg[sample.int(length(idx_neg), replace = TRUE)],
    idx_pos[sample.int(length(idx_pos), replace = TRUE)]
  )
  
  oob_idx <- setdiff(seq_along(x), boot_idx)
  
  tryCatch({
    
    xb <- x[boot_idx]
    sb <- status[boot_idx]
    
    r_boot   <- run_roc(xb, sb, direction)
    auc_boot <- as.numeric(auc(r_boot))
    cut_boot <- get_youden(r_boot)$cutoff
    
    p_boot <- perf_at_cutoff(xb, sb, cut_boot, direction)
    p_test <- perf_at_cutoff(x, status, cut_boot, direction)
    
    xo <- x[oob_idx]
    so <- status[oob_idx]
    
    p_oob <- perf_at_cutoff(xo, so, cut_boot, direction)
    
    auc_oob <- if (all(c(NEG, POS) %in% so)) {
      as.numeric(auc(run_roc(xo, so, direction)))
    } else {
      NA_real_
    }
    
    out <- c(
      auc_boot, cut_boot,
      p_boot[metric_keys],
      p_test[metric_keys],
      auc_oob,
      p_oob[metric_keys],
      length(oob_idx)
    )
    names(out) <- out_names
    out
    
  }, error = function(e) {
    setNames(rep(NA_real_, length(out_names)), out_names)
  })
}


# ----------------------------------------------------------
# 4. Full analysis for one marker
# ----------------------------------------------------------

analyze_marker <- function(data, marker, direction, B, seed) {
  
  if (!marker %in% names(data)) {
    stop("Column '", marker, "' not found in the data.")
  }
  if (!outcome_col %in% names(data)) {
    stop("Outcome column '", outcome_col, "' not found in the data.")
  }
  
  # Analysis sample (complete cases for this marker)
  d <- data %>%
    filter(
      .data[[outcome_col]] %in% c(NEG, POS),
      !is.na(.data[[marker]])
    ) %>%
    mutate(
      .status = factor(.data[[outcome_col]], levels = c(NEG, POS))
    )
  
  x      <- d[[marker]]
  status <- d$.status
  
  if (!is.numeric(x)) stop(marker, " must be numeric.")
  
  N_neg <- sum(status == NEG)
  N_pos <- sum(status == POS)
  
  if (N_neg == 0 || N_pos == 0) {
    stop(marker, ": need participants in both groups. Check that ",
         outcome_col, " uses the levels '", NEG, "' and '", POS, "'.")
  }
  
  # Full-sample ROC, AUC and DeLong CI
  roc_full <- run_roc(x, status, direction)
  auc_full <- as.numeric(auc(roc_full))
  ci_full  <- as.numeric(ci.auc(roc_full, conf.level = 0.95))
  
  # Youden cutoff and apparent performance
  yd <- get_youden(roc_full)
  
  if (yd$n_ties > 1) {
    cat("  Note:", yd$n_ties,
        "thresholds tie on Youden; using the first.\n")
  }
  
  p_full <- perf_at_cutoff(x, status, yd$cutoff, direction)
  
  # Bootstrap
  set.seed(seed)
  boot <- as.data.frame(t(replicate(B, boot_one(x, status, direction))))
  
  valid   <- !is.na(boot$AUC_boot)
  boot_ok <- boot[valid, ]
  
  # Optimism (Harrell) and corrected estimates
  apparent <- c(AUC = auc_full, p_full[metric_keys])
  
  optimism <- c(
    AUC = mean(boot_ok$AUC_boot - auc_full),
    sapply(metric_keys, function(m)
      mean(boot_ok[[paste0(m, "_boot")]] - boot_ok[[paste0(m, "_test")]],
           na.rm = TRUE))
  )
  
  corrected <- apparent - optimism
  
  # Out-of-bag summaries
  oob_keys <- c(AUC = "AUC_oob",
                setNames(paste0(metric_keys, "_oob"), metric_keys))
  
  oob_mean <- sapply(oob_keys, function(v) mean(boot_ok[[v]], na.rm = TRUE))
  oob_lo   <- sapply(oob_keys, function(v)
    quantile(boot_ok[[v]], 0.025, na.rm = TRUE, names = FALSE))
  oob_hi   <- sapply(oob_keys, function(v)
    quantile(boot_ok[[v]], 0.975, na.rm = TRUE, names = FALSE))
  
  # Cutoff stability
  cut_ci <- quantile(boot_ok$cutoff, c(0.025, 0.975),
                     na.rm = TRUE, names = FALSE)
  
  # One row of numeric results
  summary_row <- data.frame(
    Marker    = marker,
    Direction = direction,
    N         = length(x),
    N_neg     = N_neg,
    N_pos     = N_pos,
    AUC       = auc_full,
    AUC_lo    = ci_full[1],
    AUC_hi    = ci_full[3],
    Cutoff    = yd$cutoff,
    Cutoff_lo = cut_ci[1],
    Cutoff_hi = cut_ci[2],
    TP = p_full[["TP"]], FP = p_full[["FP"]],
    FN = p_full[["FN"]], TN = p_full[["TN"]],
    stringsAsFactors = FALSE
  )
  
  for (m in c("AUC", metric_keys)) {
    if (m != "AUC") summary_row[[m]] <- apparent[[m]]
    summary_row[[paste0(m, "_optimism")]]  <- optimism[[m]]
    summary_row[[paste0(m, "_corrected")]] <- corrected[[m]]
    summary_row[[paste0(m, "_oob")]]       <- oob_mean[[m]]
    summary_row[[paste0(m, "_oob_lo")]]    <- oob_lo[[m]]
    summary_row[[paste0(m, "_oob_hi")]]    <- oob_hi[[m]]
  }
  
  summary_row$Boot_valid  <- sum(valid)
  summary_row$Boot_failed <- sum(!valid)
  summary_row$Mean_n_oob  <- mean(boot_ok$n_oob)
  
  list(summary = summary_row, roc = roc_full, boot = boot)
}


# ----------------------------------------------------------
# 5. Check outcome levels, then run all markers
# ----------------------------------------------------------

cat("Outcome levels in", outcome_col, ":\n")
print(table(data_in[[outcome_col]], useNA = "ifany"))

results <- list()

for (m in names(markers)) {
  cat("\nRunning", m, "(direction", markers[[m]], ") ...\n")
  results[[m]] <- analyze_marker(data_in, m, markers[[m]], B, seed)
  cat("  AUC =", round(results[[m]]$summary$AUC, 3),
      "| cutoff =", signif(results[[m]]$summary$Cutoff, 5), "\n")
}

summary_all <- bind_rows(lapply(results, `[[`, "summary"))

write.csv(summary_all, paste0(file_prefix, "_Numeric.csv"), row.names = FALSE)


# ==========================================================
# 6. Publication tables
# ==========================================================

fmt_cut <- function(x) {
  sapply(x, function(v) {
    if (is.na(v)) return("")
    if (abs(v) >= 100) sprintf("%.2f", v)
    else if (abs(v) >= 1) sprintf("%.3f", v)
    else sprintf("%.4f", v)
  })
}

fmt_pct   <- function(p) ifelse(is.na(p), "", sprintf("%.1f", 100 * p))
fmt_pct_n <- function(p, n, N) sprintf("%.1f (%d/%d)", 100 * p, n, N)

s <- summary_all


# Table 1: apparent (full-sample) performance

table1 <- data.frame(
  Marker = s$Marker,
  
  `Case indicated by` = ifelse(s$Direction == ">",
                               "Lower values", "Higher values"),
  
  N = sprintf("%d (%d/%d)", s$N, s$N_neg, s$N_pos),
  
  `AUC (95% CI)` = sprintf("%.3f (%.3f–%.3f)", s$AUC, s$AUC_lo, s$AUC_hi),
  
  `Cutoff (95% bootstrap interval)` = paste0(
    ifelse(s$Direction == ">", "≤ ", "≥ "),
    fmt_cut(s$Cutoff), " (", fmt_cut(s$Cutoff_lo), "–",
    fmt_cut(s$Cutoff_hi), ")"
  ),
  
  `Sensitivity, % (n/N)` = fmt_pct_n(s$sens, s$TP, s$TP + s$FN),
  `Specificity, % (n/N)` = fmt_pct_n(s$spec, s$TN, s$TN + s$FP),
  `PPV, % (n/N)`         = fmt_pct_n(s$PPV,  s$TP, s$TP + s$FP),
  `NPV, % (n/N)`         = fmt_pct_n(s$NPV,  s$TN, s$TN + s$FN),
  
  check.names = FALSE,
  stringsAsFactors = FALSE
)

names(table1)[names(table1) == "N"] <-
  paste0("N (", NEG_label, "/", POS_label, ")")


# Table 2: internal validation (one block per marker)

metric_labels <- c(AUC = "AUC", sens = "Sensitivity, %",
                   spec = "Specificity, %", PPV = "PPV, %", NPV = "NPV, %")

table2 <- bind_rows(lapply(seq_len(nrow(s)), function(i) {
  
  r <- s[i, ]
  
  bind_rows(lapply(names(metric_labels), function(m) {
    
    is_auc <- m == "AUC"
    f   <- if (is_auc) function(v) sprintf("%.3f", v) else fmt_pct
    app <- if (is_auc) r$AUC else r[[m]]
    
    data.frame(
      Marker  = if (is_auc) r$Marker else "",
      Measure = metric_labels[[m]],
      Apparent = f(app),
      Optimism = if (is_auc) sprintf("%+.3f", r[[paste0(m, "_optimism")]])
      else sprintf("%+.1f", 100 * r[[paste0(m, "_optimism")]]),
      `Optimism-corrected` = f(r[[paste0(m, "_corrected")]]),
      `Out-of-bag, mean (95% interval)` = paste0(
        f(r[[paste0(m, "_oob")]]), " (",
        f(r[[paste0(m, "_oob_lo")]]), "–",
        f(r[[paste0(m, "_oob_hi")]]), ")"
      ),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  }))
}))


# ----------------------------------------------------------
# 7. Display and save tables
# ----------------------------------------------------------

cat("\nTable 1. Diagnostic performance (", NEG_label, " vs ", POS_label, ")\n",
    sep = "")
print(table1, row.names = FALSE)

cat("\nTable 2. Bootstrap internal validation\n")
print(table2, row.names = FALSE)

cat("\nBootstrap replicates valid/failed per marker:\n")
print(s[, c("Marker", "Boot_valid", "Boot_failed", "Mean_n_oob")],
      row.names = FALSE)

write.csv(table1, paste0(file_prefix, "_Table1_Performance.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")

write.csv(table2, paste0(file_prefix, "_Table2_Validation.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")


# ==========================================================
# 8. Publication ROC figure
# ==========================================================

roc_list <- lapply(results, `[[`, "roc")

labels <- c(
  GM           = "GM",
  WM           = "WM",
  WMH          = "WMH",
  AD_Signature = "AD signature",
  HOC_avg_norm = "HOC"
)

cols <- c(                      # colour-blind safe (Okabe-Ito)
  HOC_avg_norm = "#CC3399",
  AD_Signature = "#0072B2",
  WMH          = "#E69F00",
  GM           = "#000000",
  WM           = "#009E73"
)

aucs <- sapply(roc_list, function(r) as.numeric(auc(r)))
ord  <- names(sort(aucs, decreasing = TRUE))

legend_text <- sprintf("%s (AUC = %.3f)", labels[ord], aucs[ord])

draw_roc_figure <- function() {
  
  op <- par(mar = c(4.5, 4.8, 1, 1), las = 1, mgp = c(3, 0.7, 0),
            tcl = -0.3, family = "sans")
  on.exit(par(op))
  
  plot(NA, xlim = c(0, 1), ylim = c(0, 1), xaxs = "i", yaxs = "i",
       xlab = "1 \u2013 Specificity", ylab = "Sensitivity",
       axes = FALSE, cex.lab = 1.25)
  
  ticks <- seq(0, 1, by = 0.2)
  axis(1, at = ticks, labels = sprintf("%.1f", ticks), cex.axis = 1.1)
  axis(2, at = ticks, labels = sprintf("%.1f", ticks), cex.axis = 1.1)
  box(bty = "l")
  
  abline(a = 0, b = 1, lty = 2, col = "grey60")
  
  for (m in rev(ord)) {
    r <- roc_list[[m]]
    lines(1 - r$specificities, r$sensitivities,
          col = cols[[m]], lwd = 2.3)
  }
  
  legend("bottomright", legend = legend_text, col = cols[ord],
         lwd = 2.3, seg.len = 2, cex = 1.05, y.intersp = 1.2,
         bty = "n", inset = c(0.02, 0.03))
}

draw_roc_figure()

tiff(paste0(file_prefix, "_Figure.tiff"), width = 6.5, height = 6.5,
     units = "in", res = 600, compression = "lzw")
draw_roc_figure()
dev.off()

png(paste0(file_prefix, "_Figure.png"), width = 6.5, height = 6.5,
    units = "in", res = 300)
draw_roc_figure()
dev.off()


# ----------------------------------------------------------
# 9. ROC curve points (upload this if you want the figure
#    rendered here)
# ----------------------------------------------------------

roc_points <- do.call(rbind, lapply(names(roc_list), function(m) {
  r <- roc_list[[m]]
  data.frame(Marker = m, FPR = 1 - r$specificities,
             Sensitivity = r$sensitivities, AUC = as.numeric(auc(r)))
}))

write.csv(roc_points, paste0(file_prefix, "_curve_points.csv"),
          row.names = FALSE)


#################
# Summary Table for Original Data
#################

# ----------------------------------------------------------
# 1. Settings
# ----------------------------------------------------------

data_in    <- ROC_Data1
group_col  <- "Cognitive_Status"

# Preferred group order (any other levels found are added after these)
group_order <- c("NL", "Impaired", "MCI", "Demented")

biomarkers <- c("GM", "WM", "WMH", "AD_Signature", "HOC_avg_norm", "TICV")


# ----------------------------------------------------------
# 2. Checks
# ----------------------------------------------------------

missing_cols <- setdiff(c(group_col, biomarkers), names(data_in))
if (length(missing_cols) > 0) {
  stop("Not found in the data: ", paste(missing_cols, collapse = ", "))
}

cat("Groups in", group_col, ":\n")
print(table(data_in[[group_col]], useNA = "ifany"))

found  <- unique(na.omit(as.character(data_in[[group_col]])))
levels_used <- c(intersect(group_order, found), setdiff(found, group_order))


# ----------------------------------------------------------
# 3. Long format (one row per participant x biomarker)
# ----------------------------------------------------------

long <- data_in %>%
  filter(!is.na(.data[[group_col]])) %>%
  mutate(Group = as.character(.data[[group_col]])) %>%
  select(Group, all_of(biomarkers)) %>%
  pivot_longer(-Group, names_to = "Biomarker", values_to = "Value") %>%
  filter(!is.na(Value))


# ----------------------------------------------------------
# 4. Summary statistics
# ----------------------------------------------------------

summarise_values <- function(df) {
  df %>%
    summarise(
      n      = n(),
      Mean   = mean(Value),
      SD     = sd(Value),
      Median = median(Value),
      Q1     = quantile(Value, 0.25, names = FALSE),
      Q3     = quantile(Value, 0.75, names = FALSE),
      IQR    = Q3 - Q1,
      Min    = min(Value),
      Max    = max(Value),
      .groups = "drop"
    )
}

by_group <- long %>% group_by(Biomarker, Group) %>% summarise_values()
overall  <- long %>% group_by(Biomarker) %>% summarise_values() %>%
  mutate(Group = "Overall")

summary_stats <- bind_rows(by_group, overall) %>%
  mutate(
    Biomarker = factor(Biomarker, levels = biomarkers),
    Group     = factor(Group, levels = c(levels_used, "Overall"))
  ) %>%
  arrange(Biomarker, Group)


# ----------------------------------------------------------
# 5. Rounded version for reporting
#    (4 decimals for small-scale markers, 2 for volumes)
# ----------------------------------------------------------

stat_cols <- c("Mean", "SD", "Median", "Q1", "Q3", "IQR", "Min", "Max")

summary_rounded <- summary_stats %>%
  group_by(Biomarker) %>%
  mutate(across(all_of(stat_cols),
                ~ round(.x, ifelse(abs(median(Median)) < 10, 4, 2)))) %>%
  ungroup()

print(as.data.frame(summary_rounded), row.names = FALSE)


# ----------------------------------------------------------
# 6. Save
# ----------------------------------------------------------

write.csv(summary_stats,   "Biomarker_Summary_Stats_full.csv",    row.names = FALSE)
write.csv(summary_rounded, "Biomarker_Summary_Stats_rounded.csv", row.names = FALSE)
