/*****************************************************************************
 * ARCHIVO DE CONFIGURACIÓN STATA
 * Proyecto: taller1-ECA
 * Fecha de creación: 1 marzo 2025
 *****************************************************************************/

/* Detección del sistema operativo y configuración de rutas */
if regexm(c(os), "Mac") {
    // Configuración para Mac (Gustavo Castillo)
    global root_dir "/Users/upar/Downloads/taller1-ECA"
    global user_name "upar"
}
else if regexm(c(os), "Windows") {
    // Configuración para Windows (Ana María Patrón)
    global root_dir "C:/Users/username/Documents/GitHub/taller1-ECA"
    global user_name "coauteur"
}
else {
    di as error "Sistema operativo no soportado."
    exit 198
}

/* Definición de los subdirectorios del proyecto */
global config_dir     "$root_dir/config"
global data_dir       "$root_dir/data"
global data_raw_dir   "$data_dir/raw"
global data_clean_dir "$data_dir/clean"
global output_dir     "$root_dir/output"
global results_dir    "$output_dir/results"
global figures_dir    "$output_dir/figures"
global tables_dir     "$output_dir/tables"
global logs_dir       "$output_dir/logs"
global dofiles_dir    "$root_dir/code"

/* Creación de directorios si no existen */
foreach dir in data_dir data_raw_dir data_clean_dir output_dir results_dir figures_dir tables_dir logs_dir {
    capture mkdir "${`dir'}"
}

/* Configuración del directorio de trabajo */
cd "$root_dir"
pwd

/* Parámetros generales */
set more off
set varabbrev off
set linesize 100
set maxvar 10000

/* Configuración para los registros */
global date_tag = string(date(c(current_date), "DMY"), "%tdYY.NN.DD")
global time_tag = subinstr(c(current_time), ":", ".", .)
global datetime_tag = "${date_tag}_${time_tag}"

/* Mensaje de confirmación */
di "Configuración cargada exitosamente para $user_name"
di "Directorio raíz: $root_dir"