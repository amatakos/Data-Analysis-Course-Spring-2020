#################################################################################################################################
#################################################################################################################################
setwd("D:\\Alex\\DataAnalysis\\ergasia1")

## Using the dataset "Arrests for Marijuana Possession"
## Info on the dataset here: https://vincentarelbundock.github.io/Rdatasets/doc/carData/Arrests.html

library('foreign')
arrests <- read.csv("Arrests.csv")

str(arrests)
head(arrests)

## Ερώτημα 1+2
## Πίνακας συχνοτήτων και σχετικών συχνοτήτων και ραβδόγραμμα για τη μεταβλητή released

released.freq <- table(arrests$released)
released.freq
released.rel.freq <- prop.table(released.freq)
released.rel.freq
released.bar <-
  barplot(
    height = t(released.freq), 
    beside = TRUE,
    horiz = FALSE, 
    col = rainbow(length(levels(arrests$released))),
    main = "Released / Imprisoned arrestees Distribution (15777)", 
    xlab = "Released after arrest", 
    ylab = "Number of arrestees", 
    ylim = c(0, 5000),
    cex.names = 0.8,
    legend.text = levels(arrests$released),
    args.legend = list(x="top"))  

etiketes <- paste(t(released.freq), paste0(round(t(released.rel.freq),2)*100, "%)"), sep = " (")
text( 
  x = released.bar, 
  y = t(released.freq), 
  labels = etiketes,
  pos = 3)

## Πίνακας συχνοτήτων και σχετικών συχνοτήτων για τη μεταβλητή colour

colour.freq <- table(arrests$colour)
colour.freq
colour.rel.freq <- prop.table(colour.freq)
colour.rel.freq

## Πίνακας συχνοτήτων και σχετικών συχνοτήτων για τη μεταβλητή sex

sex.freq <- table(arrests$sex)
sex.freq
sex.rel.freq <- prop.table(sex.freq)
sex.rel.freq

## Πίνακας συχνοτήτων και σχετικών συχνοτήτων για τη μεταβλητή employed

employed.freq <- table(arrests$employed)
employed.freq
employed.rel.freq <- prop.table(employed.freq)
employed.rel.freq

## Πίνακας συχνοτήτων και σχετικών συχνοτήτων για τη μεταβλητή citizen

citizen.freq <- table(arrests$citizen)
citizen.freq
citizen.rel.freq <- prop.table(citizen.freq)
citizen.rel.freq


## Ερώτημα 3
 

## Cross-tabulation matrix της μεταβλητής released ως προς το φύλο (μεταβλητή sex)
## Επέλεξα αυτές τις μεταβλητές για να εξετάσω αν υπάρχει κάποιο bias στη μεταχείριση των κρατούμενων 
## λόγω φύλου

released_sex.freq <- table(arrests$released,arrests$sex)
released_sex.freq

## Ομαδοποιημένο ραβδόγραμμα της μεταβλητής released ως προς το φύλο (μεταβλητή sex)

released_sex.bar<-
  barplot(
    height = t(released_sex.freq), 
    beside = TRUE,
    horiz = FALSE, 
    col = rainbow(length(levels(arrests$sex))),
    main = "Arrestee released status - Gender Distribution 15777", 
    xlab = "Released after arrest", 
    ylab = "Number of arrestees", 
    ylim = c(0,4000),
    cex.names = 0.8,
    legend.text = levels(arrests$sex),
    args.legend = list(x="top"))

## Chi-squared contingency table test 

## Η0: Οι δύο μεταβλητές είναι ανεξάρτητες
## Η1: Οι δύο μεταβλητές είναι εξαρτημένες

chitest = chisq.test(arrests$released, arrests$sex)
chitest

## Με τιμή p-value = 0.1 και α = 0.05 δεν μπορούμε να απορρίψουμε την υπόθεση 
## ότι είναι ανεξάρτητες, αλλά κατά πάσα πιθανότητα οι δύο μεταβλητές  
## φαίνεται να είναι εξαρτημένες.

## Οπτικοποίηση του αποτελέσματος του ελέγχου με gginference

#install.packages('gginference')
library('gginference')

ggchisqtest(chitest)



## Ερώτημα 4


## Υπολογισμός των στατιστικών για τις ποσοτικές μεταβλητές age και checks

## Μέση Τιμή 
mean(arrests$age)
mean(arrests$checks)

## Διάμεσος (Median)
median(arrests$age)
median(arrests$checks)

## Τυπική απόκλιση
sd(arrests$age)
sd(arrests$checks)

## Διακύμανση/διασπορά
var(arrests$age)
var(arrests$checks)

## Λοξότητα
library(DescriptiveStats.OBeu)

ds.skewness(arrests$age)
ds.skewness(arrests$checks)

## Κύρτωση
ds.kurtosis(arrests$age)
ds.kurtosis(arrests$checks)

## Εύρος
range(arrests$age)
range(arrests$checks)

## Ενδοτεταρτημοριακό εύρος
IQR(arrests$age)
IQR(arrests$checks)

## Θηκόγραμμα για τις δύο μεταβλητές

boxplot(arrests$age,
        data = arrests,
        main = "Age of arrestee 15777",
        col = "red")

boxplot(arrests$age,
        data = arrests,
        main = "Number of previous police checks 15777",
        col = "lightblue")

## Scatter plot για τη μεταβλητή age
## Οι παρατηρήσεις συγκεντρώνονται στις μικρότερες ηλικίες και να αραιώνουν
## όσο ανεβαίνουμε σε μεγαλύτερες ηλικίες. Φαίνεται να ακολουθούν κανονική κατανομή
## με λοξότητα προς τις μεγαλύτερες ηλικίες 

plot(arrests$age)


## Ερώτημα 5

## Μετασχηματισμός των δεδομένων

sub1 <- subset(arrests, colour == 'White')
sub2 <- subset(arrests, colour == 'Black')
age_white <- sub1$age
age_black <- sub2$age

## Υπολογισμός των στατιστικών ανά κατηγορία που προκύπτει

## Μέση Τιμή 
mean(age_white)
mean(age_black)

## Διάμεσος (Median)
median(age_white)
median(age_black)

## Τυπική απόκλιση
sd(age_white)
sd(age_black)

## Διασπορά
var(age_white)
var(age_black)

## Λοξότητα
library(DescriptiveStats.OBeu)

ds.skewness(age_white)
ds.skewness(age_black)

## Κύρτωση
ds.kurtosis(age_white)
ds.kurtosis(age_black)

## Εύρος
range(age_white)
range(age_black)

## Ενδοτεταρτημοριακό εύρος
IQR(age_white)
IQR(age_black)

## Συγκριτικό θηκόγραμμα της μεταβλητής age ως προς τις κατηγορίες της μεταβλητής colour

boxplot(formula = age ~ colour,
        data = arrests,
        main = "Arrestee age / skin colour 15777",
        xlab = "Skin colour",
        ylab = "Arrestee age",
        col = rainbow(2))

## Η κατανομή ακολουθεί κανονική κατανομή με λοξότητα στραμμένη προς τα δεξιά (μεγαλύτερες ηλικίες)


## Ερώτημα 6


## Θα χρησιμοποιήσουμε πάλι τις μεταβλητές age, colour. Εφόσον το πλήθος του δείγματος ειναι αρκετά 
## μεγάλο(5200) και έχουμε ήδη δει ότι η μεταβλητή age ακολουθεί κανονική κατανομή μπορούμε να χρησι-
## μοποιήσουμε t-test για τη σύγκριση των μέσων τιμών για τις κατηγοριες White/Black. 

age_colour_test = t.test(
  formula = age ~ colour,
  data = arrests, 
  alternative = "two.sided",
  paired = FALSE,
  var.equal = FALSE, 
  conf.level = 0.95)
age_colour_test

## Άρα συμπεραίνουμε ότι οι μέσες τιμές είναι κατά πάσα πιθανότητα ίσες


## Ερώτημα 7


plot( 
  x = arrests$age, 
  y = arrests$checks,
  xlab = "Age of arrestee",
  ylab = "Number of previous police checks of arrestee")

## Linear Regression

age_checks_lm = lm(
  formula = age ~ checks,
  data = arrests)
age_checks_lm 

abline( 
  a = age_checks_lm, 
  col = "red",
  lwd = 2)

## Το δείγμα είναι πολύ μεγάλο και δεν υπάρχει κάποιο σαφές συμπέρασμα

## Συντελεστής συσχέτισης του Pearson

library('ggpubr')
cor(arrests$age, arrests$checks, method = 'pearson')
cor.test(arrests$age, arrests$checks, method = 'pearson')

## Οι δύο μεταβλητές μάλλον δεν είναι συσχετισμένες