#' Encuesta Nacional de Hogares versión anual - Metodología ACTUALIZADA
#'
#' Consulta las bases de datos de la ENAHO de forma directa
#' @param consulta_enaho Consulta a ENAHO de cada año en metodología actualizada
#' @return Base del módulo de interés de la ENAHO
#' @examples
#' temp1 <- consulta_enaho(periodo = 2020, codigo_modulo = 1, base = "enaho01-100-2022")
#' @export
consulta_enaho <- function(periodo,
                           codigo_modulo,
                           base,
                           guardar = FALSE,
                           ruta = "",
                           codificacion = NULL) {
  # Generamos dos objetos temporales: un archivo y una carpeta
  temp <- tempfile() ; tempdir <- tempdir()

  # Genera una matriz con el número identificador de versiones por cada año
  versiones <- matrix(
    c(
      2022, 784,
      2021, 759,
      2020, 739,
      2019, 691,
      2018, 638,
      2017, 605,
      2016, 548,
      2015, 504,
      2014, 441,
      2013, 407,
      2012, 323,
      2011, 290,
      2010, 260,
      2009, 238,
      2008, 209,
      2007, 194,
      2006, 183,
      2005, 150,
      2004, 120),
    byrow = T,
    ncol = 2)

  # Extrae el código de la encuesta con la matriz versiones
  codigo_encuesta <- versiones[versiones[,1] == periodo,2]
  ruta_base <- "https://proyectos.inei.gob.pe/iinei/srienaho/descarga/SPSS/" # La ruta de microdatos INEI
  modulo <- glue("-Modulo{codigo_modulo}.zip")
  url <- glue("{ruta_base}{codigo_encuesta}{modulo}")
  #https://proyectos.inei.gob.pe/iinei/srienaho/descarga/SPSS/548-Modulo64.zip

  # Descargamos el archivo
  utils::download.file(url,temp, timeout=120)

  # Listamos los archivos descargados y seleccionamos la base elegida
  archivos <- utils::unzip(temp,list = T)
  archivos <- archivos[stringr::str_detect(archivos$Name, paste0(base,"\\.")) == TRUE,]

  # Elegimos entre guardar los archivos o pasarlos directamente a un objeto
  if(guardar == TRUE) {
    utils::unzip(temp, files = archivos$Name, exdir = paste(getwd(), "/", ruta, sep = ""))
    print(paste("Archivos descargados en: ", getwd(), "/", ruta, sep = ""))
  }
  else {
    data <- haven::read_sav(
      utils::unzip(
        temp,
        files = archivos$Name[grepl(".sav|.SAV",archivos$Name)],
        exdir = tempdir
      ),
      encoding = codificacion
    )
    nombres <- toupper(colnames(data))
    colnames(data) <- nombres
    data
  }
}

#' Encuesta Nacional de Programas Presupuestales
#'
#' Consulta directa a la ENAPRES del INEI
#' @param consulta_enapres Consulta a módulo de interés de la ENAPRES según año
#' @return Módulo de la ENAPRES consultado
#' @examples
#' temp1 <- consulta_enapres(periodo = 2019, codigo_modulo = 1492, base = "CAP_300_URBANO_RURAL_5")
#' @export
consulta_enapres <- function(periodo, codigo_modulo, base, guardar = F, ruta = "", codificacion = NULL) {
  temp <- tempfile() ; tempdir <- tempdir();   # Generamos dos objetos temporales: un archivo y una carpeta
  versiones <- matrix(    # Genera una matriz con el número identificador de versiones por cada año
    c(
      2022, 785,
      2021, 761,
      2020, 736,
      2019, 684,
      2018, 626,
      2017, 596,
      2016, 544,
      2015, 495,
      2014, 438,
      2013, 406,
      2012, 325,
      2011, 293,
      2010, 266),
    byrow = T, ncol = 2);

  codigo_encuesta <- versiones[versiones[,1] == periodo,2] ;   # Extrae el código de la encuesta con la matriz versiones
  modulo <- glue("-Modulo{codigo_modulo}.zip") ;
  ruta_base <- "https://proyectos.inei.gob.pe/iinei/srienaho/descarga/SPSS/" ;
  url <- glue::glue("{ruta_base}{codigo_encuesta}{modulo}") ;

  utils::download.file(url,temp, timeout=120) ; # Descargamos

  archivos <- utils::unzip(temp,list = T) ; # Listamos los archivos
  archivos <- archivos[stringr::str_detect(archivos$Name, paste0(base,"\\.")) == TRUE,] ; # Seleccionamos la base

  if(guardar == TRUE) {
    utils::unzip(temp, files = archivos$Name, exdir = paste(getwd(), "/", ruta, sep = "")) ;
    print(paste("Archivos descargados en: ", getwd(), "/", ruta, sep = "")) ;
  }
  else {
    data <- haven::read_sav( utils::unzip(temp, files = archivos$Name[grepl(".sav|.SAV",archivos$Name)], exdir = tempdir), encoding = codificacion) ;
    nombres <- toupper(colnames(data)) ;
    colnames(data) <- nombres ;
    return(data) ;
  };
}
