library(ggplot2)

# Loading datasets
EPI_data <- read.csv("C:\\Users\\barry\\Downloads\\epi2024results06022024.csv")
attach(EPI_data)

#EPI Summary
tf <- is.na(EPI.new)
E <- EPI.new[!tf]            
summary(EPI.new) 

#FITTING A DISTRIBUTION EPI NEW
qqnorm(EPI.new); qqline(EPI.new)
x <- seq(20., 80., 1.0)
qqplot(qnorm(ppoints(200)), x)
qqline(x)
qqplot(qnorm(ppoints(200)),EPI.new)
qqline(EPI.new)
plot(ecdf(EPI.new), do.points=FALSE)
plot(ecdf(rnorm(1000, 45, 10)), do.points=FALSE) 
lines(ecdf(EPI.new))

#FITTING A DISTRIBUTION BDH NEW AND MKP NEW
BDH.new
tf <- is.na(BDH.new)
E <- BDH.new[!tf]            
summary(BDH.new) 
qqnorm(BDH.new); qqline(EPI.new)
x <- seq(20., 80., 1.0)
qqplot(qnorm(ppoints(200)), x)
qqline(x)
qqplot(qnorm(ppoints(200)),BDH.new)
qqline(BDH.new)
plot(ecdf(BDH.new), do.points = FALSE)
lines(ecdf(rnorm(1000, 45, 10)))
MKP.new
tf <- is.na(MKP.new)
E <- MKP.new[!tf]            
summary(MKP.new) 
plot(ecdf(MKP.new), do.points = FALSE)
lines(ecdf(rnorm(1000, 45, 10)))

#Comparison
boxplot(EPI.old, EPI.new, names=c("EPI.old","EPI.new"))
boxplot(BDH.old, BDH.new, names=c("BDH.old","BDH.new"))
boxplot(MKP.old, MKP.new, names=c("MKP.old","MKP.new"))

#PART 2
# read data
populations_2023 <- read.csv("C:/Users/barry/Downloads/countries_populations_2023.csv")
attach(populations_2023)
View(populations_2023)
# drop countries not in epi results
populations <- populations_2023[-which(!populations_2023$Country %in% EPI_data$country),]
# sort populations by country
populations <- populations[order(populations$Country),]
# drop countries not in populations
EPI_data.sub <- EPI_data[-which(!EPI_data$country %in% populations$Country),]
# sort epi results by country
EPI_data.sub <- EPI_data.sub[order(EPI_data.sub$country),]
# only keep necessary columns
EPI_data.sub <- EPI_data.sub[,c("country","EPI.old","EPI.new")]
# convert population to numeric
EPI_data.sub$population <- as.numeric(populations$Population)
# compute population log base 10
EPI_data.sub$population_log <- log10(EPI_data.sub$population)

#LINEAR MODEL
attach(EPI_data.sub)
# Remove rows with missing values in MKP.new and population_log

lin.mod.epinew <- lm(EPI.new~population_log,EPI_data.sub)
plot(EPI.new~population_log)
abline(lin.mod.epinew)
summary(lin.mod.epinew)
plot(lin.mod.epinew)
ggplot(epi.results.sub, aes(x = population_log, y = EPI.new)) +
  geom_point() +
  stat_smooth(method = "lm")
ggplot(lin.mod.epinew, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  labs(title='Residual vs. Fitted Values Plot', x='Fitted Values', y='Residuals')

#OTHER VARIABLES
attach(EPI_data.sub)

lin.mod.mkp <- lm(MKP.new ~ population_log, EPI_data.sub)
plot(MKP.new ~ population_log)
abline(lin.mod.mkp)
summary(lin.mod.mkp)
plot(lin.mod.mkp)

ggplot(EPI_data.sub, aes(x = population_log, y = MKP.new)) +
  geom_point() +
  stat_smooth(method = "lm")

ggplot(lin.mod.mkp, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  labs(title = 'Residual vs. Fitted Values Plot', x = 'Fitted Values', y = 'Residuals')

lin.mod.bdh <- lm(BDH.new ~ population_log, EPI_data.sub)
plot(BDH.new ~ population_log)
abline(lin.mod.bdh)
summary(lin.mod.bdh)
plot(lin.mod.bdh)

ggplot(EPI_data.sub, aes(x = population_log, y = BDH.new)) +
  geom_point() +
  stat_smooth(method = "lm")

ggplot(lin.mod.bdh, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  labs(title = 'Residual vs. Fitted Values Plot', x = 'Fitted Values', y = 'Residuals')


