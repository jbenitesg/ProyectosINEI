library(roxygen2); # Read in the roxygen2 R package
roxygenise();      # Builds the help files

# Función para trabajar la ENAHO
consulta_enapres <- function(periodo,
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
    byrow = T,
    ncol = 2)

  # Extrae el código de la encuesta con la matriz versiones
  codigo_encuesta <- versiones[versiones[,1] == periodo,2]
  modulo <- glue("-Modulo{codigo_modulo}.zip")
  ruta_base <- "https://proyectos.inei.gob.pe/iinei/srienaho/descarga/SPSS/" # La ruta de microdatos INEI
  modulo <- glue::glue("-Modulo{id_modulo +}.zip")
  url <- glue::glue("{ruta_base}{codigo_encuesta}{modulo}")
  #https://proyectos.inei.gob.pe/iinei/srienaho/descarga/SPSS/785-Modulo1727.zip

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
