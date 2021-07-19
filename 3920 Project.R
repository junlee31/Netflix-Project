##  Set working directory
setwd("---")

##  Load packages
##  Uncomment below lines if not already installed!
##  install.packages("fastDummies")
##  install.packages("FNN")
##  install.packages("caret")
##  install.packages("leaps")
##  install.packages("randomForest")
##  instal..packages("tidyverse")

library("fastDummies")
library("FNN")
library("caret")
library("leaps")
library("randomForest")
library("tidyverse")

########################
##     Data Import    ##
##     and Cleanup    ##
########################

##  Load external data
imdbData = read.csv("IMDb movies.csv", encoding="UTF-8")
oscarsData = read.csv("the_oscar_award.csv", encoding="UTF-8")
starmeterData = read.csv("Top 100 Stars of 2020.csv", encoding="UTF-8")

##  Extract relevant variables
imdbDataVars = c("imdb_title_id", "original_title", "year", "genre", "duration", "country", "language", "director", "writer", "actors", "production_company", "avg_vote")
oscarsDataVars = c("category", "name", "winner")
starmeterDataVars = c("Name")

imdbData = imdbData[,imdbDataVars]
oscarsData = oscarsData[,oscarsDataVars]
starmeterData = starmeterData[,starmeterDataVars]

##  Recode blanks to NA
imdbData[imdbData == ""] = NA
oscarsData[oscarsData == ""] = NA

##  Omit movies where year is less than 1969
imdbData$year = as.numeric(imdbData$year) ##  Introduces NAs for improperly entered observations, warning message is okay :)
imdbData = imdbData[which(imdbData$year > 1969),]

##  Omit movies where duration is more than four hours
imdbData$duration = as.numeric(imdbData$duration) ##  Introduces NAs for improperly entered observations, warning message is okay :)
imdbData = imdbData[which(imdbData$duration < 240),]

##  Preserve only primary country and language
imdbData$country = gsub(",.*", "", imdbData$country)
imdbData$language = gsub(",.*", "", imdbData$language)

##  Omit movies where country no longer exists
imdbData = imdbData[which(imdbData$country != "Czechoslovakia" & imdbData$country != "East Germany" & imdbData$country != "Federal Republic of Yugoslavia" & imdbData$country != "Netherlands Antilles" & imdbData$country != "North Vietnam" & imdbData$country != "Serbia and Montenegro" & imdbData$country != "Soviet Union" & imdbData$country != "West Germany" & imdbData$country != "Yugoslavia"),]

##  Replace missing language with most common in country
imdbData = imdbData[!is.na(imdbData$country),]  ##  Omit movies where country is NA

missingLangCountry = imdbData[which(is.na(imdbData$language)),]; missingLangCountry$country = as.factor(missingLangCountry$country); levels(missingLangCountry$country)  ##  Find countries where language is NA
imdbData$language = ifelse(is.na(imdbData$language), imdbData$country, imdbData$language) ##  Replace NA in language with value of country

imdbData$language = as.character(imdbData$language)
imdbData$language[imdbData$language == "Argentina"] = "Spanish"
imdbData$language[imdbData$language == "Australia"] = "English"
imdbData$language[imdbData$language == "Austria"] = "German"
imdbData$language[imdbData$language == "Belgium"] = "Dutch"
imdbData$language[imdbData$language == "Bosnia and Herzegovina"] = "Bosnian"
imdbData$language[imdbData$language == "Brazil"] = "Portuguese"
imdbData$language[imdbData$language == "Bulgaria"] = "Bulgarian"
imdbData$language[imdbData$language == "Canada"] = "English"
imdbData$language[imdbData$language == "Chile"] = "Spanish"
imdbData$language[imdbData$language == "China"] = "Chinese"
imdbData$language[imdbData$language == "Colombia"] = "Spanish"
imdbData$language[imdbData$language == "Croatia"] = "Croatian"
imdbData$language[imdbData$language == "Czech Republic"] = "Czech"
imdbData$language[imdbData$language == "Denmark"] = "Danish"
imdbData$language[imdbData$language == "Egypt"] = "Arabic"
imdbData$language[imdbData$language == "Finland"] = "Finnish"
imdbData$language[imdbData$language == "France"] = "French"
imdbData$language[imdbData$language == "Germany"] = "German"
imdbData$language[imdbData$language == "Greece"] = "Greek"
imdbData$language[imdbData$language == "Hong Kong"] = "Cantonese"
imdbData$language[imdbData$language == "Hungary"] = "Hungarian"
imdbData$language[imdbData$language == "India"] = "English"
imdbData$language[imdbData$language == "Indonesia"] = "Indonesian"
imdbData$language[imdbData$language == "Iran"] = "Persian"
imdbData$language[imdbData$language == "Ireland"] = "English"
imdbData$language[imdbData$language == "Italy"] = "Italian"
imdbData$language[imdbData$language == "Jamaica"] = "English"
imdbData$language[imdbData$language == "Japan"] = "Japanese"
imdbData$language[imdbData$language == "Kazakhstan"] = "Kazakh"
imdbData$language[imdbData$language == "Kyrgyzstan"] = "Kirghiz"
imdbData$language[imdbData$language == "Lithuania"] = "Lithuanian"
imdbData$language[imdbData$language == "Malta"] = "Maltese"
imdbData$language[imdbData$language == "Mexico"] = "Spanish"
imdbData$language[imdbData$language == "Mongolia"] = "Mongolian"
imdbData$language[imdbData$language == "Netherlands"] = "Dutch"
imdbData$language[imdbData$language == "New Zealand"] = "English"
imdbData$language[imdbData$language == "Philippines"] = "Filipino"
imdbData$language[imdbData$language == "Russia"] = "Russian"
imdbData$language[imdbData$language == "Singapore"] = "English"
imdbData$language[imdbData$language == "Slovenia"] = "Slovenian"
imdbData$language[imdbData$language == "South Africa"] = "English"
imdbData$language[imdbData$language == "South Korea"] = "Korean"
imdbData$language[imdbData$language == "Spain"] = "Spanish"
imdbData$language[imdbData$language == "Sri Lanka"] = "Sinhalese"
imdbData$language[imdbData$language == "Sweden"] = "Swedish"
imdbData$language[imdbData$language == "Switzerland"] = "Swiss German"
imdbData$language[imdbData$language == "Taiwan"] = "Chinese"
imdbData$language[imdbData$language == "Thailand"] = "Thai"
imdbData$language[imdbData$language == "Turkey"] = "Turkish"
imdbData$language[imdbData$language == "UK"] = "English"
imdbData$language[imdbData$language == "USA"] = "English"
imdbData$language[imdbData$language == "Vietnam"] = "Tagalog"
imdbData$language = as.factor(imdbData$language)

##  Separate genre into primary and secondary
imdbData$genre_1 = gsub(",.*", "", imdbData$genre)
imdbData$genre_2 = gsub("^(.*?, .*?), .*", "\\1", imdbData$genre); imdbData$genre_2 = gsub(".*, ", "", imdbData$genre_2)  ##  If only one genre, genre_2 will equal genre_1

##  Separate directors into primary and secondary
imdbData$director_1 = gsub(",.*", "", imdbData$director)
imdbData$director_2 = gsub("^(.*?, .*?), .*", "\\1", imdbData$director); imdbData$director_2 = gsub(".*, ", "", imdbData$director_2)  ##  If only one director, director_2 will equal director_1

##  Separate writers into primary and secondary
imdbData$writer_1 = gsub(",.*", "", imdbData$writer)
imdbData$writer_2 = gsub("^(.*?, .*?), .*", "\\1", imdbData$writer); imdbData$writer_2 = gsub(".*, ", "", imdbData$writer_2)  ##  If only one writer, writer_2 will equal writer_1

##  Separate actors into primary and secondary
imdbData$actor_1 = gsub(",.*", "", imdbData$actors)
imdbData$actor_2 = gsub("^(.*?, .*?), .*", "\\1", imdbData$actors); imdbData$actor_2 = gsub(".*, ", "", imdbData$actor_2)  ##  If only one actor, actor_2 will equal actor_1

##  Keep relevant Oscars categories
oscarsData$category = as.factor(oscarsData$category)
oscarsData$category = gsub(" .*", "", oscarsData$category)
oscarsData = oscarsData[which(oscarsData$category == "ACTOR" | oscarsData$category == "ACTRESS" | oscarsData$category == "DIRECTING" | oscarsData$category == "WRITING"),]

##  Clean up Oscars data
oscarsData$name = gsub(",.*", "", oscarsData$name); oscarsData$name = gsub("and .*", "", oscarsData$name); oscarsData$name = gsub("&.*", "", oscarsData$name); oscarsData$name = gsub(";.*", "", oscarsData$name)  ##  Keep only primary name
oscarsData$name = gsub(".*by ", "", oscarsData$name); ##  Remove "Written by," "Story by," etc.
oscarsActors = oscarsData[which(oscarsData$category == "ACTOR" | oscarsData$category == "ACTRESS"), c("name")]
oscarsDirectors = oscarsData[which(oscarsData$category == "DIRECTING"), c("name")]
oscarsWriters = oscarsData[which(oscarsData$category == "WRITING"), c("name")]
oscarsWinners = oscarsData[which(oscarsData$winner == "True"), c("name")]

##  Append Oscars data
imdbData$oscar_actor = "None"; imdbData$oscar_director = "None"; imdbData$oscar_writer = "None" ##  Initialize variables
imdbData$oscar_actor[imdbData$actor_1 %in% oscarsActors | imdbData$actor_2 %in% oscarsActors] = "Nominee" ##  Find acting nominees
imdbData$oscar_actor[imdbData$actor_1 %in% oscarsWinners | imdbData$actor_2 %in% oscarsWinners] = "Winner"  ##  Find acting winners
imdbData$oscar_director[imdbData$actor_1 %in% oscarsDirectors | imdbData$actor_2 %in% oscarsDirectors] = "Nominee" ##  Find directing nominees
imdbData$oscar_director[imdbData$actor_1 %in% oscarsWinners | imdbData$actor_2 %in% oscarsWinners] = "Winner"  ##  Find directing winners
imdbData$oscar_writer[imdbData$actor_1 %in% oscarsWriters | imdbData$actor_2 %in% oscarsWriters] = "Nominee" ##  Find writing nominees
imdbData$oscar_writer[imdbData$actor_1 %in% oscarsWinners | imdbData$actor_2 %in% oscarsWinners] = "Winner"  ##  Find writing winners

##  Append STARmeter data
imdbData$top_100 = imdbData$actor_1 %in% starmeterData | imdbData$actor_2 %in% starmeterData | imdbData$director_1 %in% starmeterData | imdbData$director_2 %in% starmeterData | imdbData$writer_1 %in% starmeterData | imdbData$writer_2 %in% starmeterData

##  Trim and reorder columns
imdbData = imdbData[, c("original_title", "genre_1", "genre_2", "duration", "country", "language", "director_1", "director_2", "writer_1", "writer_2", "actor_1", "actor_2", "oscar_actor", "oscar_director", "oscar_writer", "production_company", "top_100", "avg_vote")]

##  Clean environment of objects that are no longer useful
rm(list = setdiff(ls(), c("imdbData", "oscarsData")))

##  Classify variables
imdbData$genre_1 = as.factor(imdbData$genre_1)
imdbData$genre_2 = as.factor(imdbData$genre_2)
imdbData$country = as.factor(imdbData$country)
imdbData$language = as.factor(imdbData$language)
imdbData$director_1 = as.factor(imdbData$director_1)
imdbData$director_2 = as.factor(imdbData$director_2)
imdbData$writer_1 = as.factor(imdbData$writer_1)
imdbData$writer_2 = as.factor(imdbData$writer_2)
imdbData$actor_1 = as.factor(imdbData$actor_1)
imdbData$actor_2 = as.factor(imdbData$actor_2)
imdbData$oscar_actor = as.factor(imdbData$oscar_actor)
imdbData$oscar_director = as.factor(imdbData$oscar_director)
imdbData$oscar_writer = as.factor(imdbData$oscar_writer)
imdbData$top_100 = as.factor(imdbData$top_100)
imdbData$production_company = as.factor(imdbData$production_company)

##  Omit NA values
imdbData = na.omit(imdbData)

########################
##     Exploratory    ##
##    Data Analysis   ##
########################

##  Display data structure & descriptive statistics
str(imdbData); summary(imdbData)

##  Plot distribution of ratings on histogram
hist(imdbData$avg_vote, breaks = 10, xlim = c(1, 10), xlab = "Average Vote", ylab = "Count", main = "Average Vote Distribution", col = "#EA2200")

##  Plot rating by duration
plot(imdbData$avg_vote~imdbData$duration, xlim = c(45, 200), ylim = c(1, 10), xlab = "Duration (Minutes)", ylab = "Average Vote", main = "Average Vote by Duration", pch = 16, col = "#EA220025"); abline(lm(imdbData$avg_vote ~ imdbData$duration), lwd = 2)

##  Plot distribution of country on pie chart
pie(c(20970, 2385, 4793, 4019, 2480, 4245, 24732), c("USA", "Canada", "India", "UK", "Japan", "France", "Other"), main = "Country Distribution", col = c("#EA2200", "#EA220040", "#EA220080", "#EA220060", "#EA220050", "#EA220070", "#ACACAC"))

##  Clean environment of objects that are no longer useful
rm(list = setdiff(ls(), c("imdbData", "oscarsData")))

########################
##         KNN        ##
##     Regression     ##
########################

##  Initialize dataframe
imdbDataKNN = imdbData

##  Create dummy variables
imdbDataKNN = dummy_cols(imdbDataKNN, select_columns = c("genre_1", "genre_2", "country", "language", "oscar_actor", "oscar_director", "oscar_writer"))
imdbDataKNN = imdbDataKNN[, -c(1, 2, 3, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16)] ##  Remove original columns after dummies have been created

##  Classify variables
imdbDataKNN$top_100 = as.integer(as.logical(imdbDataKNN$top_100))

##  Normalize numeric variable
imdbDataKNN$duration = scale(imdbDataKNN$duration)

##  80-20 split
set.seed(3920)
buildingIndex = sample(seq_len(nrow(imdbDataKNN)), size = floor(0.8 * nrow(imdbDataKNN)))

##  Split building and evaluation
knnBuildingSet = imdbDataKNN[buildingIndex, ]
knnEvaluationSet = imdbDataKNN[-buildingIndex, ]

##  Take 10% random sample of data
imdbDataSampled = knnBuildingSet[sample(nrow(knnBuildingSet), 6362), ]

##  Extract y from dataframe
knnY = as.data.frame(imdbDataSampled[, 3])
imdbDataSampled = imdbDataSampled[, -c(3)]

##  80-20 split
trainingIndex = sample(seq_len(nrow(imdbDataSampled)), size = floor(0.8 * nrow(imdbDataSampled)))

##  Split training and test x
knnTrainingSet = imdbDataSampled[trainingIndex, ]
knnTestSet = imdbDataSampled[-trainingIndex, ]

##  Split training and test y
knnTrainingY = knnY[trainingIndex, ]
knnTestY = knnY[-trainingIndex, ]

##  Find optimum K
fold = seq(1, 50, 2)  ##  {K | 1 <= K <= 50 and 2 !/ K}

knnMSE = 0  ##  Initialize knnMSE

for (K in fold) {
  knnRegressionModel = knn.reg(knnTrainingSet, knnTestSet, knnTrainingY, K)  ##  Build KNN regression models
  knnMSE[K] = mean((knnTestY - knnRegressionModel$pred) ^ 2)  ##  Calculate MSE at each value of K
}

minMSE = min(na.omit(knnMSE)) ##  Find minimum MSE

##  Plot MSE at values of K
par(pty = "m"); plot(knnMSE, ylim = c(1, 2), xlab = "K", ylab = "Mean Squared Error", main = "Mean Squared Error at K", sub = "for K = 25, MSE = 1.170668", pch = 16, col = "#EA2200"); text(knnMSE, labels = round(knnMSE, digits = 3), cex = .75, pos = 3); abline(h = minMSE, lty = 3, lwd = 2)

##  Evaluate optimum model on unseen data
imdbDataSampled = knnEvaluationSet[sample(nrow(knnEvaluationSet), 6362), ]

##  Extract y from dataframe
knnY = as.data.frame(imdbDataSampled[, 3])
imdbDataSampled = imdbDataSampled[, -c(3)]

##  80-20 split
trainingIndex = sample(seq_len(nrow(imdbDataSampled)), size = floor(0.8 * nrow(imdbDataSampled)))

##  Split training and test x
knnTrainingSet = imdbDataSampled[trainingIndex, ]
knnTestSet = imdbDataSampled[-trainingIndex, ]

##  Split training and test y
knnTrainingY = knnY[trainingIndex, ]
knnTestY = knnY[-trainingIndex, ]

##  Build model at optimum K
K = 25
knnRegressionModel = knn.reg(knnTrainingSet, knnTestSet, knnTrainingY, K) ##  Build model at optimum K

par(pty = "s"); plot(knnTestY, knnRegressionModel$pred, xlim = c(1, 10), ylim = c(1, 10), xlab = "Actual Average Vote", ylab = "Predicted Average Vote", main = "Predicted vs Actual Average Vote", pch = 16, col = "#EA220075"); abline(a = 0, b = 1, lty = 3, lwd = 2) ##  Plot predicted vs actual results

knnMSE = mean((knnTestY - knnRegressionModel$pred) ^ 2)  ##  Calculate MSE
knnMAE = MAE(knnTestY, knnRegressionModel$pred)  ##  Calculate MAE
knnRMSE = sqrt(knnMSE) ## Calculate RMSE
knnSI = knnRMSE / mean(knnY[,1])  ##  Calculate scatter index
cat("At K =", K, "MSE =", knnMSE, "MAE =", knnMAE, "RMSE =", knnRMSE, "SI =", knnSI) ##  Print Metrics

##  Clean environment of objects that are no longer useful
rm(list = setdiff(ls(), c("imdbData", "knnRegressionModel")))

########################
##         KNN        ##
##   Classification   ##
########################

##  Initialize dataframe
imdbDataKNN = imdbData

##  Convert y to categorical variable
knnCutoff = mean(imdbDataKNN$avg_vote)
imdbDataKNN$greenlight = "No"
imdbDataKNN$greenlight[imdbDataKNN$avg_vote > knnCutoff] = "Yes"

##  Create dummy variables
imdbDataKNN = dummy_cols(imdbDataKNN, select_columns = c("genre_1", "genre_2", "country", "language", "oscar_actor", "oscar_director", "oscar_writer"))
imdbDataKNN = imdbDataKNN[, -c(1, 2, 3, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 18)] ##  Remove original columns after dummies have been created

##  Classify variables
imdbDataKNN$top_100 = as.integer(as.logical(imdbDataKNN$top_100))

##  Normalize numeric variable
imdbDataKNN$duration = scale(imdbDataKNN$duration)

##  80-20 split
set.seed(3920)
buildingIndex = sample(seq_len(nrow(imdbDataKNN)), size = floor(0.8 * nrow(imdbDataKNN)))

##  Split building and evaluation
knnBuildingSet = imdbDataKNN[buildingIndex, ]
knnEvaluationSet = imdbDataKNN[-buildingIndex, ]

##  Take 10% random sample of data
imdbDataSampled = knnBuildingSet[sample(nrow(knnBuildingSet), 6362), ]

##  Extract y from dataframe
knnY = as.data.frame(imdbDataSampled[, 3])
imdbDataSampled = imdbDataSampled[, -c(3)]

##  80-20 split
trainingIndex = sample(seq_len(nrow(imdbDataSampled)), size = floor(0.8 * nrow(imdbDataSampled)))

##  Split training and test x
knnTrainingSet = imdbDataSampled[trainingIndex, ]
knnTestSet = imdbDataSampled[-trainingIndex, ]

##  Split training and test y
knnTrainingY = knnY[trainingIndex, ]
knnTestY = knnY[-trainingIndex, ]

##  Find optimum K
fold = seq(1, 50, 2)  ##  {K | 1 <= K <= 50 and 2 !/ K}
knnAccuracy = fold
knnSensitivity = fold
knnPrecision = fold

knnAccuracy = 0  ##  Initialize knnAccuracy

for (K in fold) {
  knnClassificationModel = knn(knnTrainingSet, knnTestSet, knnTrainingY, K)  ##  Build KNN regression models
  knnAccuracy[K] = mean(knnClassificationModel == knnTestY)  ##  Calculate accuracy at each value of K
}

maxAccuracy = max(na.omit(knnAccuracy)) ##  Find max accuracy

##  Plot accuracy at values of K
par(pty = "m"); plot(knnAccuracy, ylim = c(.6, .8), xlab = "K", ylab = "Accuracy", main = "Accuracy at K", sub = "for K = 15, Accuracy = .7172035", pch = 16, col = "#EA2200"); text(knnAccuracy, labels = round(knnAccuracy, digits = 3), cex = .75, pos = 3); abline(h = maxAccuracy, lty = 3, lwd = 2)

##  Evaluate optimum model on unseen data
imdbDataSampled = knnEvaluationSet[sample(nrow(knnEvaluationSet), 6362), ]

##  Extract y from dataframe
knnY = as.data.frame(imdbDataSampled[, 3])
imdbDataSampled = imdbDataSampled[, -c(3)]

##  80-20 split
trainingIndex = sample(seq_len(nrow(imdbDataSampled)), size = floor(0.8 * nrow(imdbDataSampled)))

##  Split training and test x
knnTrainingSet = imdbDataSampled[trainingIndex, ]
knnTestSet = imdbDataSampled[-trainingIndex, ]

##  Split training and test y
knnTrainingY = knnY[trainingIndex, ]
knnTestY = knnY[-trainingIndex, ]

##  Build model at optimum K
K = 15
knnClassificationModel = knn(knnTrainingSet, knnTestSet, knnTrainingY, K) ##  Build model at optimum K

confusionMatrix = table(knnTestY, knnClassificationModel)

knnAccuracy = (confusionMatrix[1] + confusionMatrix[4]) / (confusionMatrix[1] + confusionMatrix[2] + confusionMatrix[3] + confusionMatrix[4])
knnSensitivity = confusionMatrix[4] / (confusionMatrix[4] + confusionMatrix[2])
knnPrecision = confusionMatrix[4] / (confusionMatrix[4] + confusionMatrix[3])

confusionMatrix; cat("At K =", K, "Accuracy =", knnAccuracy, "Sensitivity =", knnSensitivity, "Precision =", knnPrecision) ##  Print Metrics

##  Clean environment of objects that are no longer useful
rm(list = setdiff(ls(), c("imdbData", "knnRegressionModel", "knnClassificationModel")))

########################
##   Data Wrangling   ##
##      Continued     ##
########################

##  Convert countries to continents
europeCountry = c("Albania", "Andorra", "Armenia", "Austria", "Azerbaijan", "Belarus", "Belgium", "Bosnia and Herzegovina", "Bulgaria", "Croatia", "Cyprus", "Czech Republic", "Denmark", "Estonia", "Finland", "France", "Georgia", "Germany", "Greece", "Hungary", "Iceland", "Ireland", "Italy", "Kazakhstan", "Kosovo", "Latvia", "Liechtenstein", "Lithuania", "Luxembourg", "Malta", "Moldova", "Monaco", "Montenegro", "Netherlands", "Republic of North Macedonia", "Norway", "Poland", "Portugal", "Romania", "Russia", "San Marino", "Serbia", "Slovakia", "Slovenia", "Spain", "Sweden", "Switzerland", "Turkey", "Ukraine", "UK", "Vatican City")

northAmericaCountry = c("USA", "Canada", "Mexico", "Greenland", "Puetro Rico")

southAmericaCountry = c("Argentina", "Bolivia", "Brazil", "Chile", "Colombia", "Ecuador", "Guyana", "Paraguay", "Peru", "Suriname", "Uruguay", "Venezuela")

asiaCountry = c("Afghanistan", "Armenia", "Azerbaijan", "Bahrain", "Bangladesh", "Bhutan", "Brunei", "Burma", "Cambodia", "China", "East Timor", "Georgia", "Hong Kong", "India", "Indonesia", "Iran", "Iraq", "Israel", "Japan", "Jordan", "Kazakhstan", "Kuwait", "Kyrgyzstan", "Laos", "Lebanon", "Malaysia", "Mongolia", "Nepal", "North Korea", "Oman", "Pakistan","Palestine","Papua New Guinea", "Philippines", "Qatar", "Saudi Arabia", "Singapore", "South Korea", "Sri Lanka", "Syria", "Taiwan", "Tajikistan", "Thailand", "Turkey", "Turkmenistan", "United Arab Emirates", "Uzbekistan", "Vietnam", "Yemen")

oceaniaCountry = c("Australia", "Fiji", "Kiribati", "Marshall Islands", "Micronesia", "Nauru", "New Zealand", "Palau", "Papua New Guinea", "Samoa", "Solomon Islands", "Tonga", "Tuvalu", "Vanuatu")

africaCountry = c("Algeria", "Angola", "Benin", "Botswana", "Burkina Faso", "Burundi", "Cameroon", "Cape Verde", "Central African Republic", "Chad", "Comoros", "The Democratic Republic Of Congo", "Ivory Coast", "Djibouti", "Egypt", "Equatorial Guinea", "Eritrea", "Ethiopia", "Gabon", "Gambia", "Ghana", "Guinea", "Guinea-Bissau", "Kenya", "Lesotho", "Liberia", "Libya", "Madagascar", "Malawi", "Mali", "Mauritania", "Mauritius", "Morocco", "Mozambique", "Namibia", "Niger", "Nigeria", "Rwanda", "Sao Tome and Principe", "Senegal", "Seychelles", "Sierra Leone", "Somalia", "South Africa", "Sudan", "Swaziland", "Tanzania", "Togo", "Tunisia", "Uganda", "Zambia", "Zimbabwe")

##  Create continent variable
imdbData$continent = NA
imdbData$continent[imdbData$country %in% africaCountry] = "Africa"
imdbData$continent[imdbData$country %in% asiaCountry] = "Asia"
imdbData$continent[imdbData$country %in% europeCountry] = "Europe"
imdbData$continent[imdbData$country %in% northAmericaCountry] = "North America"
imdbData$continent[imdbData$country %in% oceaniaCountry] = "Oceania"
imdbData$continent[imdbData$country %in% southAmericaCountry] = "South America"

##  Omit NA values
imdbData = na.omit(imdbData)

##  Recode non-English language to "Other"
imdbData$language = as.character(imdbData$language)
imdbData$language[which(imdbData$language != "English")] = "Other"

##  Clean environment of objects that are no longer useful
rm(list = setdiff(ls(), c("imdbData", "knnRegressionModel", "knnClassificationModel")))

########################
##       Linear       ##
##     Regression     ##
########################

imdbDataLr = imdbData 
logisticRCutoff = mean(imdbDataLr$avg_vote)
imdbDataLr$greenlight = "0"
imdbDataLr$greenlight[imdbDataLr$avg_vote > logisticRCutoff] = "1"
imdbDataLr$greenlight = as.integer(imdbDataLr$greenlight)

set.seed(3920)
imdbDataSample = imdbDataLr[sample(nrow(imdbDataLr),5000),]
set.seed(999)
trainingIndex = sample(seq_len(nrow(imdbDataSample)), size = floor(0.8 * nrow(imdbDataSample)))
trainData = imdbDataSample[trainingIndex,]
testData = imdbDataSample[-trainingIndex,]

model = lm(avg_vote~genre_1+genre_2+duration+continent+language+oscar_actor+oscar_director+top_100+oscar_writer, data = trainData)
summary(model)
par(mfrow=c(2,2))
plot(model)


model_2=lm(avg_vote~genre_1+genre_2+duration+language+continent+oscar_actor, data = trainData)
summary(model_2)
par(mfrow=c(2,2))
confint(model)
plot(model_2)

########################
##      Logistic      ##
##     Regression     ##
########################

model_1 = glm(greenlight~genre_1+genre_2+duration+continent+language+oscar_actor+oscar_director+top_100+oscar_writer,data=trainData,family=binomial)
summary(model_1)
plot(model_1)
par(mfrow=c(2,2))


pred.prob = predict(model_1,testData,type="response")
pred.prob[1:10]

pred.class = pred.prob
pred.class[pred.prob>0.5] = "Yes"
pred.class[!pred.prob>0.5] = "No"
pred.class[1:10]

c.matrix = table(actual = testData$greenlight, pred.class)
c.matrix

########################
##       Forward      ##
##      Selection     ##
########################

## New Data
data_FS = imdbData[,c("genre_1","genre_2","duration","language","oscar_actor","oscar_director","oscar_writer","top_100","avg_vote","continent")]

## Model
model_fwd = regsubsets(avg_vote ~ ., data = data_FS, nvmax = NULL, method= "forward")
summary(model_fwd)
plot(model_fwd,scale = "adjr2", main = "Forward Selection: AdjR2")

summary(model_fwd)$which[which.max(summary(model_fwd)$adjr2),]
model_fwd_summary = summary(model_fwd) # Store summary output
which.max(model_fwd_summary$adjr2)
summary(model_fwd)$which[47,]


## Constructing best model using Forward Selection
data_FS = dummy_cols(data_FS, select_columns = c("genre_1", "genre_2", "oscar_actor", "oscar_director", "oscar_writer", "continent"))
best_model_fwd = lm(avg_vote ~ genre_1_Adult + genre_1_Adventure + genre_1_Animation + genre_1_Biography + genre_1_Comedy + genre_1_Crime + genre_1_Documentary + genre_1_Drama + genre_1_Family + genre_1_Fantasy + genre_1_History + genre_1_Horror + genre_1_Music + genre_1_Musical + genre_1_Mystery + genre_1_Romance + `genre_1_Sci-Fi` + genre_1_Sport + genre_1_Thriller + genre_1_War + genre_1_Western + genre_2_Adventure + genre_2_Animation + genre_2_Biography + genre_2_Comedy + genre_2_Crime + genre_2_Drama + genre_2_Family + genre_2_Fantasy + genre_2_History + genre_2_Horror + genre_2_Music + genre_2_Musical + genre_2_Mystery + genre_2_News + `genre_2_Reality-TV` + genre_2_Romance + `genre_2_Sci-Fi` + genre_2_Sport + genre_2_Thriller + genre_2_War + genre_2_Western + duration + language + oscar_actor_None + oscar_actor_Winner + oscar_director_None + oscar_director_Winner + oscar_writer_None + oscar_writer_Winner + top_100 + continent_Asia + continent_Europe + `continent_North America` + continent_Oceania + `continent_South America`, data=data_FS)
summary(best_model_fwd)

########################
##       Random       ##
##       Forest       ##
########################

str(imdbData)

# adjust any data as proper type
imdbData$continent = as.factor(imdbData$continent)
imdbData$language = as.factor(imdbData$language)

# extract needed vars
data_rf = imdbData
data_rf_clean = data.frame(imdbData$genre_1,imdbData$genre_2,imdbData$duration,imdbData$language,
                           imdbData$oscar_actor,imdbData$oscar_director,imdbData$oscar_writer,
                           imdbData$top_100,imdbData$continent,imdbData$avg_vote)

str(data_rf_clean)
mean(data_rf_clean$imdbData.avg_vote)

train.index = sample(1:nrow(data_rf_clean),nrow(data_rf_clean)*0.8)
train = data_rf_clean[train.index,]
test = data_rf_clean[-train.index,]

train$imdbData.avg_vote = ifelse(train$imdbData.avg_vote >= 5.780367,'pass','fail')
test$imdbData.avg_vote = ifelse(test$imdbData.avg_vote >= 5.780367,'pass','fail')
summary(as.factor(train$imdbData.avg_vote))

# model creation
cl.model = randomForest(as.factor(imdbData.avg_vote)~.,data=train,importance=TRUE)
cl.model

# predictions run
pred.cl = predict(cl.model,test)
c.matrix = table(test$imdbData.avg_vote,pred.cl); c.matrix

# var of importance 
varImpPlot(cl.model,main="Variable Importance Plots")

##  Clean environment of objects that are no longer useful
rm(list = setdiff(ls(), c("imdbData", "knnRegressionModel", "knnClassificationModel", "best_model_fwd", "cl.model")))
