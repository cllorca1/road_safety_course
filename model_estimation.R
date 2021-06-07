pacman::p_load(dplyr, ggplot2, MASS)


my_data = read.csv("regression/dataset.csv")


lrm = lm(Crashes ~ AADT + L, data = my_data)
summary(lrm)




poisson_model = glm(Crashes ~ AADT + L, data = my_data, family = poisson(link = "log"))
summary(poisson_model)


poisson_model_2 = glm(Crashes ~ log(AADT) + log(L), data = my_data, family = poisson(link = "log"))
summary(poisson_model_2)



nb_model = glm.nb(Crashes ~ log(AADT) + log(L), data = my_data)
summary(nb_model)
