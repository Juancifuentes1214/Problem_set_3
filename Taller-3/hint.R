
## librerias
require(pacman)
p_load(tidyverse,rio,stargazer,coefplot)

## datos
df = import("input/data_regresiones.rds")

## modelos
modelo_1 = lm(price ~ dist_cbd + as.factor(property_type) , data= df)
modelo_2 = lm(price ~ dist_cbd + as.factor(property_type) + rooms , data= df)
modelo_3 = lm(price ~ dist_cbd + as.factor(property_type) + rooms + bathrooms, data= df)

## visualizacion
coefplot(model = modelo_3) + theme_test()

## 
ggsave(filename = "output/plot_regresiones.png")
stargazer(modelo_1,modelo_2,modelo_3,
          type = "text", 
          out = "output/resultados_regresiones.xlsx")

