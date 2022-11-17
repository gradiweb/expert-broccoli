# Libraries Install
install.packages("sqldf")
install.packages("plotrix")

# Libraries Instance
library("dplyr")
library("sqldf")
library("janitor")
library("plotrix")
library("RColorBrewer")
library("psych") 

# Read CSV file
businessDataFrame <- read.csv(file = './One_thousand business_largest of_Colombia.csv')
# Rename Columns
colnames(businessDataFrame) <- c("Numero","Nit","RazonSocial","Supervisor","Region","Departamento","Ciudad","CIIU","Macrosector","IngresosOperacionales_2018","GananciaPerdida_2018","TotalActivos_2018","TotalPasivos_2018","TotalPatrimonio_2018","IngresosOperacionales_2017","GananciaPerdida_2017","TotalActivos_2017","TotalPasivos_2017","TotalPatrimonio_2017","GrupoNIIF")

# Add new columns to allow works better in database
businessDataFrame$NivelDeIngresos <- with(businessDataFrame, ifelse(IngresosOperacionales_2018 > 1001508387, 1,
                                                                 ifelse(IngresosOperacionales_2018 > 334184383, 2,
                                                                      ifelse(IngresosOperacionales_2018 > 230046500, 3,
                                                                             ifelse(IngresosOperacionales_2018 > 150258106, 4, 5)))))

businessDataFrame$NivelDeActivos <- with(businessDataFrame, ifelse(TotalActivos_2018 > 1001095239, 1,
                                                                    ifelse(TotalActivos_2018 > 300132265, 2,
                                                                           ifelse(TotalActivos_2018 > 212879841, 3,
                                                                                  ifelse(TotalActivos_2018 > 60003090, 4, 5)))))
# 1. ¿Cuál es el macrosector con más ingresos operacionales 2018?
profit_2018 <- sqldf('SELECT Macrosector,IngresosOperacionales_2018  
      FROM businessDataFrame')

mainMacrosectorProfit_2018 <- profit_2018 %>%                                     
  arrange(desc(IngresosOperacionales_2018)) %>%
  slice(1:1)

#mainMacrosectorProfit_2018


# 2. ¿Cuál es el macrosector con más ingresos operacionales 2017?
profit_2017 <- sqldf('SELECT Macrosector,IngresosOperacionales_2017  
      FROM businessDataFrame')

mainMacrosectorProfit_2017 <- profit_2017 %>%                                     
  arrange(desc(IngresosOperacionales_2017)) %>%
  slice(1:1)

#mainMacrosectorProfit_2017

# 3. ¿Cuál es el top de las 10 empresas con más ingresos operacionales en 2018?

bussinessProfit_2018 <- sqldf('SELECT RazonSocial,IngresosOperacionales_2018  
      FROM businessDataFrame')

topTenBusiness_2018 <- bussinessProfit_2018 %>%                                     
  arrange(desc(IngresosOperacionales_2018)) %>%
  slice(1:10)

#topTenBusiness_2018

# 4. Diagrama de pie / sectores - Top 10 empresas con más ingresos operacionales 2018 - %

profit <- c(topTenBusiness_2018$IngresosOperacionales_2018)
labels <- c(topTenBusiness_2018$RazonSocial)

pieProfitpercent_2018 <- round(100 * profit / sum(profit), 1)

pie3D(profit, labels = pieProfitpercent_2018,
      radius = 0.75,
      main = "Top 10 empresas - Ingresos operacionales 2018", 
      col= hcl.colors(length(profit), "Spectral"),
      labelcol = "black",
      labelcex = 0.55)
      legend("topleft", c(labels),
       cex = 0.3, fill = rainbow(length(profit)))


# 5. ¿Ingresos operacionales por región - 2018?

regionsProfit_2018 <- sqldf('SELECT Region,IngresosOperacionales_2018  
        FROM businessDataFrame')

profitTablePerRegion_2018 <- regionsProfit_2018 %>% group_by(Region) %>% 
        summarise(Profit = sum(IngresosOperacionales_2018),
            .groups = 'drop')
#profitTablePerRegion_2018

# 6 . Barplot ingresos operacionales por región

barplot(profitTablePerRegion_2018$Profit, 
           main = "Ingresos operacionales por región",
           col=c("#ffc502","#ed1f38","#2d143b","#22a1f2","#fa6720","#5457ff","#3ffe87"),
           ylab="Total Ingresos",
           xlab="Región",
           font.axis=1.2,
           font.main=2,
           legend.text = c(profitTablePerRegion_2018$Region))

# 7- ¿Cuál fue el promedio de ingresos operacionales para cada Macrosector?
attach(businessDataFrame)
tapply(IngresosOperacionales_2018, Macrosector, mean,na.rm = TRUE)

# 8 - ¿Cuál es el porcentaje que aporta cada macrosector a los ingresos operacionales en cada Departamento y región?

round(prop.table(table(Macrosector, Departamento),2),2) 
round(prop.table(table(Macrosector, Region),2),2)


# 9 ¿Qué relación existe entre empresas que tienen activos altos y sus Ingresos operacionales?

plot(NivelDeIngresos, NivelDeActivos, main="Activos VS Ingresos",
     xlab="Nivel de ingresos ", ylab="Nivel de Activos ", pch=20)
abline(lm(NivelDeActivos~NivelDeIngresos), col="red") # regression line (y~x)
lines(lowess(NivelDeActivos,NivelDeIngresos), col="blue") # lowess line (x,y)

sunflowerplot(businessDataFrame$NivelDeIngresos,businessDataFrame$NivelDeActivos, 
              col = rgb(0, 0, 0, alpha = 0.05),
              main = "Activos VS Ingresos")

matriz<-select(businessDataFrame,NivelDeIngresos, NivelDeActivos)
pairs.panels(a, pch=3,main="Matriz de Dispersión, Histograma y Correlación")

# 10 ¿Análisis nivel de ingresos 2018?
summary(NivelDeIngresos)

# 10 ¿Análisis nivel de activos 2018?
summary(NivelDeActivos)

# 11 ¿Cuál es el nivel de ingresos por macrosector?
boxplot(NivelDeIngresos~Macrosector, 
        border=c("#01d564", "#ed1c24"),
        main = "Nivel de ingresos por macrosector",
        cex.lab=1,
        cex.axis=0.4,
        xlab = "Macrosector",
        ylab = "Nivel de ingresos")
text(c(1:nlevels(Region)), labels=paste("n = ",table(Region),sep=":"))
