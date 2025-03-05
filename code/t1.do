* Do file taller 1 econometría avanzada


* * * * * * * * * * * 
* * *  punto 2  * * * 
* * * * * * * * * * * 

* Cargar base de datos
use "$data_dir/researchers.dta", clear

* Añadir etiquetas
label var author_citations "Número total de citas"
label var trending_topic "Trending Topic"


* Punto 7
set seed 12 // Para replicabilidad

local sizes 50 100 500 1000 5000 10000 
*Se omite el valor de 50.000 porque la muestra tiene 10.381 observaciones
local n_sizes = wordcount("`sizes'") + 1  // Incluye muestra completa

* Crear matriz para almacenar resultados
matrix results = J(`n_sizes', 2, .) // Matriz vacía con 2 columnas (log_n, beta_hat)

local i = 1
foreach n of local sizes {
    preserve
    sample `n', count // Toma `n' observaciones
    reg author_citations trending_topic
    matrix results[`i',1] = log(`n') // Guardar log(n)
    matrix results[`i',2] = _b[trending_topic] // Guardar coeficiente estimado
    restore
    local i = `i' + 1
}

di "Regresión con la muestra completa"
reg author_citations trending_topic //, noconstant
matrix results[`i',1] = log(_N)  // Log del tamaño total
matrix results[`i',2] = _b[trending_topic]  // Coeficiente con muestra completa


matrix list results
* Convertir matriz en variables sin modificar base original
matrix colnames results = log_n beta_hat

* Graficar resultados
clear
svmat results, names(col)  // Convierte la matriz en variables
rename log_n log_n
rename beta_hat beta_hat

twoway (scatter beta_hat log_n, msymbol(O) mcolor(blue)) ///
       (line beta_hat log_n, lcolor(red) lwidth(medium)) ///
       , yline(4, lcolor(black) lpattern(dash)) /// Línea horizontal en y = 4
    xlabel(, grid) ylabel(0(1) 8, grid) /// Etiquetas en el eje Y de 0 a 10 en pasos de 1
    title("Tendencia del estimador MCO con el tamaño de muestra") ///
    xtitle("Log del tamaño de la muestra") ytitle("Estimador MCO (β̂)") ///
    legend(order(1 "Estimaciones" 2 "Tendencia")) 


// Save the graph
graph export "$figures_dir/graficosininter.png", replace

* 9c
reg author_citations trending_topic 
outreg2 using "$tables_dir/resultadosp1.tex", ctitle("") replace nor2 label ///
	addtext("Observaciones", `e(N)', "Controles","No") noobs nonotes ///
	addstat("R cuadrado ajustado", e(r2_a)) ///
	addnote("Errores estandar en parentesis. * p<0.01, ** p<0.05, * p<0.1")
	
//	
// 	addstat("Controles", "No", "R2 ajustado", e(r2_a)) ///
//     ctitle("","Trending Topic")
//   

* 9d 
capture bysort article (article): gen count = _N
capture gen collaborative = (count != 1)


reg author_citations trending_topic c.trending_topic#c.collaborative
outreg2 using "$tables_dir/resultadosp1.tex", append ctitle("") nor2 nonotes label ///
	addtext("Observaciones", `e(N)', "Controles","No") noobs ///
	addstat("R cuadrado ajustado", e(r2_a)) ///
	addnote("Errores estandar en parentesis. * p<0.01, ** p<0.05, * p<0.1")
	
	
	
	rename(c.trending_topic#c.collaborative, "Trending Topic × Colaborativo")

reg author_citations c.trending_topic if collaborative == 0

*Obtener estimadores consistentes: La estrategia es que 
global mydir "/Users/HP/Desktop/Econometría avanzada 2025-1/talleres/t1"

preserve  
    bysort article (article): gen count = _N  // Cuenta cuántas veces aparece cada article
    keep if count == 1  // Conserva solo los artículos únicos
    drop count  // Elimina la variable auxiliar
	reg author_citations trending_topic
	outreg2 using "$mydir/resultadosp1.txt", replace

	
	

	
    list  // Muestra los datos filtrados
restore  // Vuelve a la base original
	
* * * * * * * * * * * 
* * *  punto 3  * * * 
* * * * * * * * * * * 

*1 una funcion a trozos, di = 1 si tal di= 2 si tal,,, la funcion de resultados potenciales seria Yig= 
 
*2 los supuestos de la complementaria del 28 de ferebro

*3  
use "C:\Users\HP\Desktop\Econometría avanzada 2025-1\talleres\t1\01.Treatment_PK.dta", clear 

tabulate Treatment


*4

label list 

*cambiar la categoria base
fvset base 4 Treatment
reghdfe agJune i.Treatment, absorb(District)

** Diferencia de medias conjunta para number of publicy bodies
reghdfe NActiveCCs i.Treatment, absorb(District)
 
** Diferencia de medias conjunta para physical assets
reghdfe bShareA091415 i.Treatment, absorb(District) 

* Diferencia de medias para Bachelors
reghdfe Bachelors i.Treatment, absorb(District) 

* Diferencia de medias para dMale
reghdfe dMale i.Treatment, absorb(District) 

* Diferencia de medias para age
reghdfe age i.Treatment, absorb(District) 


* ids en cada cluster
gen clus=floor(runiform() * 2) + 1
reghdfe age i.Treatment, cluster(clus) 
reghdfe age i.Treatment, absorb(clus) 

egen obs_per_cluster = count(clus), by(clus)
tabulate clus, summarize(obs_per_cluster)



* Mostrar los coeficientes de los efectos fijos de cada distrito
predict district_effects, u
tabulate District, summarize(district_effects)


*a porque dice que no hay evidencia estadistica de que las medias de los grupos sean distintos. permite dar evidencia sobre la correcta aleatorización del tratamiento debido a que uno de los objetivos de la aleatorización en experimentos controlados es garantizar que, antes del tratamiento, los grupos sean lo más similares posible en cuanto a características observables. En otras palabras, la aleatorización busca que las diferencias entre los grupos de tratamiento y el grupo de control sean, en promedio, pequeñas en términos de variables relevantes (como la edad, el género, o las características específicas del contexto, en este caso, "age"). Si después de la aleatorización se encuentran diferencias estadísticamente significativas entre los grupos en términos de estas variables, esto podría sugerir que la aleatorización no fue exitosa y que los grupos no son comparables antes del tratamiento. En este caso, la aleatorización podría estar sesgada o no estar bien ejecutada. la clave es que estamos en baseline y estas son caracteristicas observadas a priori

*b no necesariamente, puedo tener algo aleatorio pero que este sesgado hacia la derecha, o por ejemplo que la probabilidad de exito sea 0.99 y de fracaso 0.1, etc, entonces no necesariamente

*c si, yo creo


*5 yi= tau1 d1 + tau 2 d2+ tau 3 d3+ efecto fijo distrito + tau + controles+ error


*6
use "C:\Users\HP\Desktop\Econometría avanzada 2025-1\talleres\t1\02.Results_PK.dta" , clear
gen newitem_lquantity=NewItemID*lQuantity

* toca declarar que es un panel de datos: pregunta horario??
sort NewItemID CostCenterCode
*
reghdfe lUnitPrice IncentivesY2 AutonomyY2 BothY2, absorb(CostCenterCode) 
reghdfe lUnitPrice IncentivesY2 AutonomyY2 BothY2 i.NewItemID#c.lQuantity Year2 [aweight=ExpInCtrl], absorb(NewItemID CostCenterCode) cluster(CostCenterCode)
*7

*8
*9
*10
*11
*12

*supuesto de identificacion de RCTS


















	
	
