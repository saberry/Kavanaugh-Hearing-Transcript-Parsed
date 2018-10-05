###############
### Genders ###
###############

genderData = data.frame(name = c("BLUMENTHAL", "BOOKER", "BROMWICH",
                                 "COONS", "CORNYN", "CRAPO", "CRUZ",
                                 "DURBIN", "FEINSTEIN", "FLAKE",
                                 "FORD", "GRAHAM", "GRASSLEY",
                                 "HARRIS", "HATCH", "HIRONO",
                                 "KAVANAUGH", "KENNEDY", "KLOBUCHAR",
                                 "LEAHY", "LEE", "MITCHELL",
                                 "SASSE", "TILLIS", "WHITEHOUSE"), 
                        gender = c("male", "male", "male",
                                   "male", "male", "male", "male",
                                   "male", "female", "male",
                                   "female", "male", "male",
                                   "female", "male", "female",
                                   "male", "male", "female",
                                   "male", "male", "female",
                                   "male", "male", "male"))


write.csv(genderData, "data/genderData.csv", row.names = FALSE)
