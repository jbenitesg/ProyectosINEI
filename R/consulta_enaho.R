# Función para trabajar la ENAHO
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
