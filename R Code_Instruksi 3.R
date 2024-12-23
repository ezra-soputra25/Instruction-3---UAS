#mengimport data pef
library(readr)
pef <- read_csv("https://raw.githubusercontent.com/dwi-agustian/biostat/refs/heads/main/pefc2.csv")

#Mengidentifikasi outlier berdasarkan kriteria dan visualisasi grafik boxplot
boxplot(pef$age, pef$height, pef$pef, pef$sc01_14_14, pef$sc02_14_14, pef$sc03_14_14,
        main="Boxplot pef",
        col=("lightblue"),
        names = c("Age", "Height", "PEF","SC01", "SC02", "SC03")
        )
boxplot(pef$age,
        main="boxplot Pef$Age")
boxplot(pef$height,
        main="boxplot Pef$Height")
boxplot(pef$pef,
        main="boxplot Pef$pef")
boxplot(pef$sc01_14_14, pef$sc02_14_14, pef$sc03_14_14,
        main="boxplot Pef",
        names = c("SC01", "SC02", "SC03"))

#menentukan batas atas dan bawah grafik boxplot
Q1_age <- quantile(pef$age,0.25)
Q1_height <- quantile(pef_ageo$height, 0.25)
Q1_SC01 <- quantile (pef_heighto$sc01_14_14, 0.25)
Q1_SC03 <- quantile (pef_sco$sc03_14_14, 0.25)

Q3_age <- quantile(pef$age, 0.75)
Q3_height <- quantile (pef_ageo$height, 0.75)
Q3_SC01 <- quantile (pef_heighto$sc01_14_14, 0.75)
Q3_SC03 <- quantile (pef_sco$sc03_14_14, 0.70)


IQR_age <- Q3_age - Q1_age
IQR_height <- Q3_height - Q1_height
IQR_SC01 <- Q3_SC01 - Q1_SC01
IQR_SC03 <- Q3_SC03 - Q1_SC03

lb_age <- Q1_age - 1.5 * IQR_age
ub_age <- Q3_age + 1.5 * IQR_age
lb_height <- Q1_height - 1.5 * IQR_height
ub_height <- Q3_height + 1.5 * IQR_height
lb_SC01 <- Q1_SC01 - 1.5 * IQR_SC01
ub_SC01 <- Q3_SC01 + 1.5 * IQR_SC01
lb_SC03 <- Q1_SC03 - 1.5 * IQR_SC03
ub_SC03 <- Q3_SC03 + 1.5 * IQR_SC03

cat("Batas bawah:",lb_age,"\n")
cat("Batas bawah:",lb_height,"\n")
cat("Batas bawah:", lb_SC01,"\n")
cat("Batas bawah:", lb_SC03,"\n")
cat("Batas atas:",ub_age,"\n")
cat("Batas atas:", ub_height,"\n")
cat("Batas atas:", ub_SC01,"\n")
cat("Batas atas:", ub_SC03,"\n")

outliers_age <- pef$age < lb_age | pef$age > ub_age
outliers_height <- pef_ageo$height < lb_height | pef_ageo$height > ub_height
outliers_SC01 <- pef_heighto$sc01_14_14 < lb_SC01 | pef_heighto$sc01_14_14 > ub_SC01
outliers_SC03 <- pef_sco$sc03_14_14 < lb_SC03 | pef_sco$sc03_14_14 > ub_SC03
ol_age <- pef[outliers_age,]
ol_height <- pef_ageo[outliers_height,]
ol_SC01 <- pef_heighto[outliers_SC01,]
ol_SC03 <- pef_sco[outliers_SC03,]
cat("Data outlier:", "\n")
print(ol_age)
print(ol_height)
print(ol_SC01)
print(ol_SC03)

#menghapus outliers  age dari data set
pef_ageo <- pef[!outliers_age,]
pef_heighto <- pef_ageo[!outliers_height,]
pef_sco <- pef_heighto[!outliers_SC01,]
pef_sco1 <- pef_sco[!outliers_SC03,]
head(pef_ageo)
head(pef_heighto)

boxplot(pef_sco1$age, pef_sco1$height, pef_sco1$sc01_14_14, pef_sco1$sc02_14_14, pef_sco1$sc03_14_14,
        main="Boxplot pef",
        col=("lightblue"),
        names = c("Age", "Height", "SC01","SC02","SC03")
)


#Uji normalitas data
hist(pef$pef)             #data sebelum outlier disingkirkan
hist(pef_sco1$pef)        #sesudah outlier disingkirkan

ks.test(pef$pef, "pnorm", mean(pef$pef), sd(pef$pef))
ks.test(pef_sco1$pef, "pnorm", mean(pef_sco1$pef), sd(pef_sco1$pef))

#Grafik QQ line untuk membandingkan visualisasi nilai PEF
library(ggplot2)
#Sebelum outliers disingkirkan
df <- data.frame(pef = pef$pef)
ggplot(df, aes(sample = pef)) +
  stat_qq(color = "blue", size = 2) +
  stat_qq_line(color = "red", linetype = "dashed") +
  labs(title = "QQ Plot of PEF Data sebelum", x = "Theoretical Quantiles", y = "Sample Quantiles") +
  theme_minimal()

#sesudah outliers disingkirkan
df1 <- data.frame(pef = pef_sco1$pef)
ggplot(df1, aes(sample = pef)) +
  stat_qq(color = "blue", size = 2) +
  stat_qq_line(color = "red", linetype = "dashed") +
  labs(title = "QQ Plot of PEF Data sesudah", x = "Theoretical Quantiles", y = "Sample Quantiles") +
  theme_minimal()

#membuat scatter plot dengan regressi linear dan LOESS
#PEF dan Height
# Data frame untuk ggplot
df_plot <- data.frame(height = pef_sco1$height, PEF = pef_sco1$pef)
# Scatter plot
ggplot(df_plot, aes(x = height, y = PEF)) +
  geom_point(color = "blue", size = 2) +  # Titik scatter
  geom_smooth(method = "lm", color = "red", se = FALSE, linetype = "dashed") +  # Garis linear
  geom_smooth(method = "loess", color = "green", se = FALSE) +  # Garis LOESS
  labs(title = "Scatter Plot: PEF vs Height",
       x = "Height",
       y = "PEF") +
  theme_minimal()

#PEF dan Age
df_plot1 <- data.frame(age = pef_sco1$age, PEF = pef_sco1$pef)
# Scatter plot
ggplot(df_plot1, aes(x = age, y = PEF)) +
  geom_point(color = "blue", size = 2) +  # Titik scatter
  geom_smooth(method = "lm", color = "red", se = FALSE, linetype = "dashed") +  # Garis linear
  geom_smooth(method = "loess", color = "green", se = FALSE) +  # Garis LOESS
  labs(title = "Scatter Plot: PEF vs Age",
       x = "Age",
       y = "PEF") +
  theme_minimal()
