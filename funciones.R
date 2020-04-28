# La función renombrar cambia los nombres cortos de variable a etiquetas largas. 
#Está diseñanda para funcionar sin argumentos, no lleva nada entre paréntesis.
# input: un data frame, ouput un data frame

renombrar <- function(x) {
  viejos = diccionario$nombre[match(names(x), diccionario$nombre)]
  nuevos = diccionario$etiqueta[match(names(x), diccionario$nombre)]
  viejos = viejos[!is.na(viejos)]
  nuevos = nuevos[!is.na(nuevos)]
  rename_at(x, vars(viejos), ~nuevos)
}


# Usa expresiones regulares para reducir detectar cadenas de caracteres y asignar un partido/órbita política 
# input un vector, ouput un vector

reducir_partidos <- function(x) {
  variable = tolower(x)
  partido = case_when(
    str_detect(variable, "pro|cambiemos|ucr|integración|recrear|cc") ~ "cambiemos", 
    str_detect(variable, "unidad|frente|fpv|sur|consenso|izquierda") ~ "peronismo", 
    TRUE ~ "otro o sin datos (residual)"
  )
  partido
}