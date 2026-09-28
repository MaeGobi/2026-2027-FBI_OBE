# Pipeline Analysis General Population

library(here)
library(dplyr)
library(rstatix)
library(ggplot2)
library(ggforce)
library(patchwork)
library(factoextra)
library(psych)
library(MASS)
library(stargazer)
library(pscl)
library(sjPlot)

##################################################################################################################
# Charge and visualize dataset
##################################################################################################################
# Repository definition for R project. All files called from this repo.
here()

df_raw <- read.csv(file = here("data", "cohorte_LMM_OBE.csv"), 
                   header = TRUE, sep = ";", dec = ",", na.strings = c("NaN", "Na", "#NUL!", " "))
head(df_raw)
colnames(df_raw)
nrow(df_raw)

inclus <- df_raw$Ball_speed=="slow"
df <- df_raw[inclus,]
nrow(df)

df$Migraine <- factor(df$Migraine, levels=c("-0.5", "0.5"))
levels(df$MIGRAINE)

df$OBE <- factor(df$OBE, levels=c("-0.5", "0.5"))
levels(df$OBE)
summary(df$OBE)

df$Sex <- factor(df$Sex, levels=c("-0.5", "0.5"))
levels(df$Sex)


##################################################################################################################
# Preprocessing
##################################################################################################################
# NaN inspection
##################################################################################################################

# Define interest columns
cols_int <- c("drift", "Ball_speed", "Age_centered", "Sex", "Migraine", "OBE", "CDS", "logCDS", "Anxiety", "Depression", "HADS",
              "LogHADS", "video_game_habit", "EQ", "cognitiveempathy", "emotionalreactivity", "socialskill", "logEQ")

# Count number of missing/empty values for each interest column
colSums(is.na(df[cols_int]) | df[cols_int]=="" |is.null(df[cols_int]))

# Display row number of missing/empty values for each variable
missing <- sapply(df[cols_int], function(x) which(is.na(x) | x == "" | is.null(x)) + 1)
missing

##################################################################################################################
# Identify aberrant values (> total score
##################################################################################################################
# For CDS total score
av <- sum(df$CDS > 290, na.rm = TRUE)
cat ("CDS total : Aberrant values (>290):", av, "\n")


# For HADS A and D
anxdep <- c("Anxiety", "Depression")
for (col in anxdep) {
  av <- sum(df[[col]]  > 21, na.rm = TRUE)
  cat (col, ": Aberrant values (>21):", av, "\n")
}


##################################################################################################################
# Visualization of variables distribution
##################################################################################################################
num_cols <- c("drift", "Age", "CDS", "logCDS", "Anxiety", "Depression", "HADS",
              "video_game_habit", "EQ")

df[num_cols] <- lapply(df[num_cols], function(x) as.numeric(as.character(x)))

summary(df[num_cols])

####### Plot numerical variables distribution #######

#####Classic distribution plots (histogram + density) ######
distrib_plot_list <- lapply(num_cols, function(col) {
  p <- ggplot(df, aes(x= .data[[col]])) +
    geom_histogram(aes(y=after_stat(density)), bins=30, fill="purple4", color="white") +
    geom_density(fill="grey", alpha = 0.5) +
    geom_vline(xintercept = mean(df[[col]], na.rm=TRUE), linetype="dashed", color="turquoise", linewidth=1) +
    geom_vline(xintercept = median(df[[col]], na.rm=TRUE), linetype="dotdash", color="red", linewidth=1) +
    theme_minimal()
})
names(distrib_plot_list) <- num_cols

# Combine all plots
wrap_plots(distrib_plot_list, ncol=6)

# Save figure
ggsave(here("figures", "numeric_distribution_plots_V2.png"),
       wrap_plots(distrib_plot_list, ncol=3),
       width = 30, height = 20, dpi = 300)


###### Visualize (boxplots + violin plots_ Alternative) #####
box_violin2 <- lapply(num_cols, function(col) {
  p <- ggplot(df, aes(x =" ", y= .data[[col]])) +
    geom_violin(fill="lightsteelblue2", alpha=0.7) +
    geom_boxplot(outliers=FALSE, width = 0.1, fill="cornflowerblue") +
    labs(x=NULL, y=col) +
    theme_light()
  return(p)
})
names(box_violin2) <- num_cols

# Combine all plots
wrap_plots(box_violin2, ncol=6)


# Save figure
ggsave(here("figures", "box_violin_plots_V2.png"),
       wrap_plots(box_violin2, ncol=6),
       width = 30, height = 20, dpi = 300)

##################################################################################################################
# Outliers Identification
##################################################################################################################
# Count and identify values of outliers
for (col in num_cols) {
  out_values <- boxplot.stats(df[[col]])$out
  cat("\n=== Colonne:", col, "===\n")
  cat("Nombre d'outliers:", length(out_values), "\n")
  if (length(out_values) > 0) {
    cat("Valeurs:", paste(out_values, collapse = ", "), "\n")
  }
}




########## Plot categorical variables ###############
cat_cols <- c("Sex", "Migraine", "OBE")

df[cat_cols] <- lapply(df[cat_cols], factor)

# Description categorical variables
sum_cat <- lapply(df[cat_cols], function(x) as.data.frame(table(x)))
sum_cat

# Barplot of categorical variables distribution
cat_dist_plots <- lapply(cat_cols, function(col) {
  p<- ggplot(df %>% filter(!is.na(.data[[col]]), !.data[[col]] %in% c("", "NaN", "#NUL!")), aes(x = .data[[col]])) +
    geom_bar(fill="darkblue") +
    geom_text(stat="count", aes(label=after_stat(count)), vjust=-0.5, size = 3) +
    scale_y_continuous(expand=expansion(mult=c(0, 0.15))) +
    theme_light() +
    theme(legend.text=element_text(size=6), 
          plot.title=element_text(size=8),
          axis.text.x=element_text(size=8),
          axis.text.y=element_text(size=8))
  return(p)
})
names(cat_dist_plots) <- cat_cols

wrap_plots(cat_dist_plots, ncol=3)

ggsave(here("figures", "categ_distribution_plots_V2.png"),
       wrap_plots(cat_dist_plots, ncol=3),
       width = 30, height = 20, dpi = 300)








###################################################################################################################
# Linear Regression Models #
###################################################################################################################
# Drift
###################################################################################################################
# Description of Drift
summary(df$drift)
describe(df$drift)
shapiro.test(df$drift)

##################################################################################################################
# Modèle 0 #
#################################################################################################################
Model_0 <- lm(drift~1, na.action = na.exclude, data=df) 
summary(Model_0)

# Vérification des prérequis
# 1. Indépendance des résidus : pas de structure particulière dans la distribution des résidus
plot(Model_0, 1)
# Test de durbin-watson
library(lmtest)
dwtest(Model_0)
# Test de Breusch Godfrey
bgtest(Model_0)

# 2. Homoscédasticité : test de Harrison-McCabe
hmctest(Model_0)

# 3. Normalité
rstandard(Model_0)
hist(Model_0$residuals) # Histogramme des résidus standardisés
plot(Model_0, 2) # QQplot

shapiro.test(rstandard(Model_0))

library(fBasics)
dagoTest(rstandard(Model_0))

# 4. Détection des valeurs atypiques et aberrantes
# résidu std > 2 = atypique; > 3 = aberrant
plot(rstandard(Model_0))
abline(h = c(-2, 2), col="red", lty=2)
abline(h = c(-3, 3), col="forestgreen", lty=2)

# 5. Dipersion of CDS total distribution
chi2 <- sum(residuals(Model_0, "pearson")^2)
chi2 / df.residual(Model_0)
1 - pchisq(chi2, df = df.residual(Model_0))

##################################################################################################################
# Modèle 1 #
#################################################################################################################
Model_1 <- lm(drift~ OBE, na.action = na.exclude, data=df)
summary(Model_1)

# Vérification des prérequis
# 1. Indépendance des résidus : pas de structure particulière dans la distribution des résidus
plot(Model_1, 1)
# Test de durbin-watson
dwtest(Model_1)
# Test de Breusch Godfrey
bgtest(Model_1)

# 2. Homoscédasticité : test de Harrison-McCabe
hmctest(Model_1)

# 3. Normalité
rstandard(Model_1)
hist(Model_1$residuals) # Histogramme des résidus standardisés
plot(Model_1, 2) # QQplot

shapiro.test(rstandard(Model_1))

dagoTest(rstandard(Model_1))

# 4. Détection des valeurs atypiques et aberrantes
# résidu std > 2 = atypique; > 3 = aberrant
plot(rstandard(Model_1))
abline(h = c(-2, 2), col="red", lty=2)
abline(h = c(-3, 3), col="forestgreen", lty=2)

# 5. Dipersion of CDS total distribution
chi2 <- sum(residuals(Model_1, "pearson")^2)
chi2 / df.residual(Model_1)
1 - pchisq(chi2, df = df.residual(Model_1))


##################################################################################################################
# Modèle 2 #
#################################################################################################################
Model_2 <- lm(drift~ OBE+CDS, na.action = na.exclude, data=df)
summary(Model_2)

# Vérification des prérequis
# 1. Indépendance des résidus : pas de structure particulière dans la distribution des résidus
plot(Model_2, 1)
# Test de durbin-watson)
dwtest(Model_2)
# Test de Breusch Godfrey
bgtest(Model_2)

# 2. Homoscédasticité : test de Harrison-McCabe
hmctest(Model_2)

# 3. Normalité
rstandard(Model_2)
hist(Model_2$residuals) # Histogramme des résidus standardisés
plot(Model_2, 2) # QQplot

shapiro.test(rstandard(Model_2))

dagoTest(rstandard(Model_2))

# 4. Détection des valeurs atypiques et aberrantes
# résidu std > 2 = atypique; > 3 = aberrant
plot(rstandard(Model_2))
abline(h = c(-2, 2), col="red", lty=2)
abline(h = c(-3, 3), col="forestgreen", lty=2)

# 5. Multicollinéarité
library(car)
vif(Model_2)



##################################################################################################################
# Modèle 3 #
#################################################################################################################
Model_3 <- lm(drift~ OBE+CDS+Age, na.action = na.exclude, data=df)
summary(Model_3)

# Vérification des prérequis
# 1. Indépendance des résidus : pas de structure particulière dans la distribution des résidus
plot(Model_3, 1)
# Test de durbin-watson
dwtest(Model_3)
# Test de Breusch Godfrey
bgtest(Model_3)

# 2. Homoscédasticité : test de Harrison-McCabe
hmctest(Model_3)

# 3. Normalité
rstandard(Model_3)
hist(Model_3$residuals) # Histogramme des résidus standardisés
plot(Model_3, 2) # QQplot

shapiro.test(rstandard(Model_3))

dagoTest(rstandard(Model_3))

# 4. Détection des valeurs atypiques et aberrantes
# résidu std > 2 = atypique; > 3 = aberrant
plot(rstandard(Model_3))
abline(h = c(-2, 2), col="red", lty=2)
abline(h = c(-3, 3), col="forestgreen", lty=2)

# 5. Multicollinéarité
vif(Model_3)


##################################################################################################################
# Modèle 4 #
#################################################################################################################
Model_4 <- lm(drift~ OBE+CDS+Age+Sex, na.action = na.exclude, data=df)
summary(Model_4)

# Vérification des prérequis
# 1. Indépendance des résidus : pas de structure particulière dans la distribution des résidus
plot(Model_4, 1)
# Test de durbin-watson
library(lmtest)
dwtest(Model_4)
# Test de Breusch Godfrey
library(lmtest)
bgtest(Model_4)

# 2. Homoscédasticité : test de Harrison-McCabe
library(lmtest)
hmctest(Model_4)

# 3. Normalité
rstandard(Model_4)
hist(Model_4$residuals) # Histogramme des résidus standardisés
plot(Model_4, 2) # QQplot

shapiro.test(rstandard(Model_4))

library(fBasics)
dagoTest(rstandard(Model_4))

# 4. Détection des valeurs atypiques et aberrantes
# résidu std > 2 = atypique; > 3 = aberrant
plot(rstandard(Model_4))
abline(h = c(-2, 2), col="red", lty=2)
abline(h = c(-3, 3), col="forestgreen", lty=2)

# 5. Multicollinéarité
library(car)
vif(Model_4)

##################################################################################################################
# Modèle 5 #
##################################################################################################################
Model_5 <- lm(drift~ OBE+CDS+Age+Sex+Anxiety, na.action = na.exclude, data=df)
summary(Model_5)

# Vérification des prérequis
# 1. Indépendance des résidus : pas de structure particulière dans la distribution des résidus
plot(Model_5, 1)
# Test de durbin-watson
dwtest(Model_5)
# Test de Breusch Godfrey
bgtest(Model_5)

# 2. Homoscédasticité : test de Harrison-McCabe
hmctest(Model_5)

# 3. Normalité
rstandard(Model_5)
hist(Model_5$residuals) # Histogramme des résidus standardisés
plot(Model_5, 2) # QQplot

shapiro.test(rstandard(Model_5))

dagoTest(rstandard(Model_5))

# 4. Détection des valeurs atypiques et aberrantes
# résidu std > 2 = atypique; > 3 = aberrant
plot(rstandard(Model_5))
abline(h = c(-2, 2), col="red", lty=2)
abline(h = c(-3, 3), col="forestgreen", lty=2)

# 5. Multicollinéarité
vif(Model_5)


##################################################################################################################
# Modèle 6 #
##################################################################################################################
Model_6 <- lm(drift~ OBE+CDS+Age+Sex+Anxiety+Depression, na.action = na.exclude, data=df)
summary(Model_6)

# Vérification des prérequis
# 1. Indépendance des résidus : pas de structure particulière dans la distribution des résidus
plot(Model_6, 1)
# Test de durbin-watson
dwtest(Model_6)
# Test de Breusch Godfrey
bgtest(Model_6)

# 2. Homoscédasticité : test de Harrison-McCabe
hmctest(Model_6)

# 3. Normalité
rstandard(Model_6)
hist(Model_6$residuals) # Histogramme des résidus standardisés
plot(Model_6, 2) # QQplot

shapiro.test(rstandard(Model_6))

dagoTest(rstandard(Model_6))

# 4. Détection des valeurs atypiques et aberrantes
# résidu std > 2 = atypique; > 3 = aberrant
plot(rstandard(Model_6))
abline(h = c(-2, 2), col="red", lty=2)
abline(h = c(-3, 3), col="forestgreen", lty=2)

# 5. Multicollinéarité
vif(Model_6)


##################################################################################################################
# Modèle 7 #
#################################################################################################################
Model_7 <- lm(drift~ OBE+CDS+Age+Sex+Anxiety+Depression+Migraine, na.action = na.exclude, data=df)
summary(Model_7)

# Vérification des prérequis
# 1. Indépendance des résidus : pas de structure particulière dans la distribution des résidus
plot(Model_7, 1)
# Test de durbin-watson
dwtest(Model_7)
# Test de Breusch Godfrey
bgtest(Model_7)

# 2. Homoscédasticité : test de Harrison-McCabe
hmctest(Model_7)

# 3. Normalité
rstandard(Model_7)
hist(Model_7$residuals) # Histogramme des résidus standardisés
plot(Model_7, 2) # QQplot

shapiro.test(rstandard(Model_7))

dagoTest(rstandard(Model_7))

# 4. Détection des valeurs atypiques et aberrantes
# résidu std > 2 = atypique; > 3 = aberrant
plot(rstandard(Model_7))
abline(h = c(-2, 2), col="red", lty=2)
abline(h = c(-3, 3), col="forestgreen", lty=2)

# 5. Multicollinéarité
vif(Model_7)


##################################################################################################################
# Models Comparison #
#################################################################################################################
anova(Model_0, Model_1, Model_2, Model_3, Model_4, Model_5, Model_6, Model_7)


model_comparison <- data.frame(
  Model = c("Model_0", "Model_1", "Model_2", "Model_3", "Model_4", "Model_5", "Model_6", "Model_7"),
  Adjusted_R2 = c(summary(Model_0)$adj.r.squared,
                  summary(Model_1)$adj.r.squared,
                  summary(Model_2)$adj.r.squared,
                  summary(Model_3)$adj.r.squared,
                  summary(Model_4)$adj.r.squared,
                  summary(Model_5)$adj.r.squared,
                  summary(Model_6)$adj.r.squared,
                  summary(Model_7)$adj.r.squared),
  AIC = c(AIC(Model_0),
          AIC(Model_1),
          AIC(Model_2),
          AIC(Model_3),
          AIC(Model_4),
          AIC(Model_5),
          AIC(Model_6),
          AIC(Model_7)),
  BIC = c(BIC(Model_0),
          BIC(Model_1),
          BIC(Model_2),
          BIC(Model_3),
          BIC(Model_4),
          BIC(Model_5),
          BIC(Model_6),
          BIC(Model_7))
)
print(model_comparison)


# Global summary table of all models (exportation, article ready)
tab_model(Model_1, Model_2, Model_3, Model_4, Model_5, Model_6, Model_7,
          show.aic = TRUE,
          show.zeroinf = TRUE,
          show.intercept = FALSE,
          transform = "exp",
          dv.labels = c("Model 1", "Model 2", "Model 3", "Model 4", "Model 5", "Model_6", "Model_7"),
          pred.labels = c("OBE0.5" = "OBE",
                          "CDS" = "Total CDS score",
                          "Age" = "Age",
                          "Sex0.5" = "Sex (Woman)",
                          "Anxiety" = "Anxiety (HADS-A)",
                          "Depression" = "Depression (HADS-D)",
                          "Migraine0.5" = "Migraine (Yes)"),
          file = here("Figures", "tableau_modeles_regression_V2.doc"))




#################################################################################################################
# Simple Mediation  Model Drift
#################################################################################################################
library(lavaan)

# Mediation model definition
med_CDS_tot <- 
  ' # direct effect
             drift ~ c*OBE
           # mediators
             CDS ~ a*OBE + Age
             drift ~ b*CDS + Age
           # indirect effects
             ab := a*b
           # total effect
             total := c + (a*b)
         '
#### Fitting of mediation model
fit_med <- sem(med_CDS_tot, data=df, estimator="MLR")
summary(fit_med, standardized=TRUE, fit.measures=TRUE, ci=TRUE)

library(officer)
library(flextable)
#### Export results as table
# Extraction of standardized estimate
tab_SEM <- standardizedsolution(fit_med, type = "std.all", ci = TRUE)

# Filter standardized loadings (std.all)
loadings_std <- tab_SEM %>%
  mutate(across(c(est.std, se, ci.lower, ci.upper, z, pvalue), ~round(., 3)))
print(loadings_std)

# Export to Word
ft <- flextable(loadings_std)
doc <- read_docx()
doc <- body_add_flextable(doc, value = ft)
print(doc, target = here("Figures", "FBI_OBE_Mediation_loadings_std_V2.docx"))