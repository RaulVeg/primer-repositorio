# Estructuras de seleccion #####
# Jose Raul Martinez Vega #

# Comparar estatura ####
yo <- 172 #creo un vecor con mi estatura
idolo <- 184 #vector con la estatura del idolo
if(yo > idolo) {
  print("eres mas alto")
} # trate de comparar si yo era mas alto que mi idolo y si lo era se imprimiria el texto
# no imprime nad aya que no soy mas alto 

#Me gusta el mole y el pozole ####
Mole="SI" #genere etas condiciones 
Pozole="SI"
if(Mole=="SI")
  if( Pozole=="SI"){
    print("me gusta el mole y el pozole")
  } #corriendo el codigo se cumplieron las condiones y me imprime el texto

# me gusta el mole o el pozole ####
Mole="SI"
Pozole="NO"
if(Mole=="SI")
  if(Pozole=="NO"){
    print("Me gusta el mole")
  }

# En que estacion del año naci ####
Me<- 8 #aqui puse el mes en el que naci
if( Me >= 7 | Me <= 9) { #aqui puse los meses que estan en tal estaciones
  print("naciste en verano") 
} else if(Me >= 1|Me <= 3) {
  print("Naciste en invierno")
} else if(Me >= 4| Me <= 6) {
  print("Naciste en primavera")
} else if(Me >= 10| Me <= 12) {
  print("Naciste en otoño")
} # al final me arroja que naci en verano

cumple <- 144
if(cumple>=81 & cumple<=171 ){
  print("Naciste en Primavera")
} else if(cumple>=172 & cumple<=264 ){
  print("Naciste en Verano")
} else if(cumple>=265 & cumple<=355 ){
  print("Naciste en Otoño")
} else{
  print("Naciste en invierno ")
}


dia <- readline(prompt = "Dime que dia naciste: ")
dia <- as.numeric(dia)
mes <- readline(prompt = "Dime que mes naciste (1:12):")
mes <- as.numeric(mes)
mes

# Excentar curso ####
EP <- 40 #aqui esta el porcentaje de los parciales
EQ <- 18 #aqui el de examenes quincenales
extra <-20 #aqui lo extra como tareas,expos donde se obtuvo en todo 10
if (EP+EQ+extra >= 80){ #se hace lasuma de los datos
  print("vas a apasar este curso") #si esa suma daba mas de o era igual 80%, aprobado
} else{
  print("Reprobaste, looser") #si daba menos entonces era un reprobado
} 