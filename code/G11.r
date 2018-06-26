#Reading the data
data = read.csv("Questionnaire.csv")
gpa = data$G.P.A.value

#removing the non numerical GPA values
to_remove = c("Don't know","We don't have a such G.P.A. value","Dont know","Current","no GPA",
              "No","Not given","Not yet given")

index = c()

for (i in 1:length(gpa)){
  
  if (is.element(gpa[i],to_remove)){
    index = c(index,i)
  }
}

cleaned_data = data[-index, ]

#Seperating the the original dataframe in to 2 datframes of sportsmen and non-sportsmen

df_sports = subset(cleaned_data, cleaned_data$sports=='Yes')
df_non_sports = subset(cleaned_data, cleaned_data$sports=='No')

#getting the GPA value of sportsmen and non-sportsmen
gpa_sports = c()
for (i in df_sports$G.P.A.value) {
  i = as.double(i)
  gpa_sports = c(gpa_sports,i)
  
}

gpa_non_sports = c()
for (i in df_non_sports$G.P.A.value) {
  i = as.double(i)
  gpa_non_sports = c(gpa_non_sports,i)
  
}

# T test for GPA difference of sportsmen and non-sportsmen
t.test(gpa_non_sports,gpa_sports,alternative = 'greater')

####################################################################
#seperating into gender
df_sports_male = subset(df_sports, df_sports$Gender=='Male')
df_sports_female = subset(df_sports, df_sports$Gender=='Female')
df_non_sports_male = subset(df_non_sports, df_non_sports$Gender=='Male')
df_non_sports_female = subset(df_non_sports, df_non_sports$Gender=='Female')

#test for male students
gpa_sports_male = c()
for (i in df_sports_male$G.P.A.value) {
  i = as.double(i)
  gpa_sports_male = c(gpa_sports_male,i)
  
}

gpa_non_sports_male = c()
for (i in df_non_sports_male$G.P.A.value) {
  i = as.double(i)
  gpa_non_sports_male = c(gpa_non_sports_male,i)
  
}

t.test(gpa_non_sports_male,gpa_sports_male,alternative = 'greater')


#test for female students
gpa_sports_female = c()
for (i in df_sports_female$G.P.A.value) {
  i = as.double(i)
  gpa_sports_female = c(gpa_sports_female,i)
  
}

gpa_non_sports_female = c()
for (i in df_non_sports_female$G.P.A.value) {
  i = as.double(i)
  gpa_non_sports_female = c(gpa_non_sports_female,i)
  
}

t.test(gpa_non_sports_female,gpa_sports_female,alternative = 'greater')

###############################################################################
df_sports_Colombo = subset(df_sports, df_sports$University=='University of Colombo')
df_sports_Mora = subset(df_sports, df_sports$University=='University of Moratuwa')
df_sports_Japura = subset(df_sports, df_sports$University=='University of Sri Jayawardenapura')
df_sports_Pera = subset(df_sports, df_sports$University=='University of Peradeniya')

df_non_sports_Colombo = subset(df_non_sports, df_non_sports$University=='University of Colombo')
df_non_sports_Mora = subset(df_non_sports, df_non_sports$University=='University of Moratuwa')
df_non_sports_Japura = subset(df_non_sports, df_non_sports$University=='University of Sri Jayawardenapura')
df_non_sports_Pera = subset(df_non_sports, df_non_sports$University=='University of Peradeniya')

#Test for UOC students
gpa_sports_Colombo= c()
for (i in df_sports_Colombo$G.P.A.value) {
  i = as.double(i)
  gpa_sports_Colombo = c(gpa_sports_Colombo,i)
  
}

gpa_non_sports_Colombo = c()
for (i in df_non_sports_Colombo$G.P.A.value) {
  i = as.double(i)
  gpa_non_sports_Colombo = c(gpa_non_sports_Colombo,i)
  
}

t.test(gpa_non_sports_Colombo,gpa_sports_Colombo,alternative = 'greater')


#Test for UOM students
gpa_sports_Mora= c()
for (i in df_sports_Mora$G.P.A.value) {
  i = as.double(i)
  gpa_sports_Mora = c(gpa_sports_Mora,i)
  
}

gpa_non_sports_Mora = c()
for (i in df_non_sports_Mora$G.P.A.value) {
  i = as.double(i)
  gpa_non_sports_Mora = c(gpa_non_sports_Mora,i)
  
}

t.test(gpa_non_sports_Mora,gpa_sports_Mora,alternative = 'greater')

#Test for UOP students
gpa_sports_Pera= c()
for (i in df_sports_Pera$G.P.A.value) {
  i = as.double(i)
  gpa_sports_Pera = c(gpa_sports_Pera,i)
  
}

gpa_non_sports_Pera = c()
for (i in df_non_sports_Pera$G.P.A.value) {
  i = as.double(i)
  gpa_non_sports_Pera = c(gpa_non_sports_Pera,i)
  
}

t.test(gpa_non_sports_Pera,gpa_sports_Pera,alternative = 'greater')


#Test for USJP students
gpa_sports_Japura = c()
for (i in df_sports_Japura$G.P.A.value) {
  i = as.double(i)
  gpa_sports_Japura = c(gpa_sports_Japura,i)
  
}

gpa_non_sports_Japura = c()
for (i in df_non_sports_Japura$G.P.A.value) {
  i = as.double(i)
  gpa_non_sports_Japura = c(gpa_non_sports_Japura,i)
  
}

t.test(gpa_non_sports_Japura,gpa_sports_Japura,alternative = 'greater')











