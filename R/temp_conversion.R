#' Encuesta Nacional de Hogares versión anual - Metodología ACTUALIZADA
#'
#' Consulta las bases de datos de la ENAHO de forma directa
#' @param periodo Año de consulta
#' @param codigo_modulo ID del módulo (Dos dígitos para la ENAHO)
#' @param base Nombre de la base a consultar
#' @return Base del módulo de interés de la ENAHO
#' @examples
#' temp1 <- consulta_enaho(periodo = 2020, codigo_modulo = 01, base = "enaho01-100-2022")
#' @export
consulta_enaho <- function(periodo,
                           codigo_modulo,
                           base,
                           guardar = FALSE,
                           ruta = "",
                           codificacion = NULL) {
  # Generamos dos objetos temporales: un archivo y una carpeta
  temp <- tempfile()
  tempdir <- tempdir()

  # Genera una matriz con el número identificador de versiones por cada año
  versiones <- matrix(
    c(
      2022, 784,
      2021, 759,
      2020, 737,
      2019, 687,
      2018, 634,
      2017, 603,
      2016, 546,
      2015, 498,
      2014, 440,
      2013, 404,
      2012, 324,
      2011, 291,
      2010, 279,
      2009, 285,
      2008, 284,
      2007, 283,
      2006, 282,
      2005, 281,
      2004, 280
    ),
    byrow = T,
    ncol = 2
  )

  # Extrae el código de la encuesta con la matriz versiones
  codigo_encuesta <- versiones[versiones[, 1] == periodo, 2]
  ruta_base <- "https://proyectos.inei.gob.pe/iinei/srienaho/descarga/STATA/" # La ruta de microdatos INEI
  modulo <- glue("-Modulo{codigo_modulo}.zip")
  url <- glue("{ruta_base}{codigo_encuesta}{modulo}")
  # https://proyectos.inei.gob.pe/iinei/srienaho/descarga/SPSS/548-Modulo64.zip

  # Descargamos el archivo
  utils::download.file(url, temp, timeout = 120)

  # Listamos los archivos descargados y seleccionamos la base elegida
  archivos <- utils::unzip(temp, list = T)
  archivos <- archivos[stringr::str_detect(archivos$Name, paste0(base, "\\.")) == TRUE, ]

  # Elegimos entre guardar los archivos o pasarlos directamente a un objeto
  if (guardar == TRUE) {
    utils::unzip(temp, files = archivos$Name, exdir = paste(getwd(), "/", ruta, sep = ""))
    print(paste("Archivos descargados en: ", getwd(), "/", ruta, sep = ""))
  } else {
    data <- haven::read_dta(
      utils::unzip(
        temp,
        files = archivos$Name[grepl(".dta|.DTA", archivos$Name)],
        exdir = tempdir
      ),
      encoding = codificacion
    )
    nombres <- tolower(colnames(data))
    colnames(data) <- nombres
    data
  }
}



#' Encuesta Demográfica y de Salud Familiar - ENDES
#'
#' Consulta las bases de datos de la ENDES de forma directa
#' @param periodo Año de consulta
#' @param codigo_modulo ID del módulo (Dos dígitos para la ENDES)
#' @param base Nombre de la base a consultar
#' @return Base del módulo de interés de la ENDES
#' @examples
#' temp1 <- consulta_endes(periodo = 2004, codigo_modulo = 64, base = "RECH0")
#' @export
consulta_endes <- function(periodo,
                           codigo_modulo,
                           base,
                           guardar = FALSE,
                           ruta = "",
                           codificacion = NULL) {
  # Generamos dos objetos temporales: un archivo y una carpeta
  temp <- tempfile()
  tempdir <- tempdir()

  # Genera una matriz con el número identificador de versiones por cada año
  versiones <- matrix(
    c(
      2022, 786,
      2021, 760,
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
      2004, 120
    ),
    byrow = T,
    ncol = 2
  )

  # Extrae el código de la encuesta con la matriz versiones
  codigo_encuesta <- versiones[versiones[, 1] == periodo, 2]
  ruta_base <- "https://proyectos.inei.gob.pe/iinei/srienaho/descarga/SPSS/" # La ruta de microdatos INEI
  modulo <- glue("-Modulo{codigo_modulo}.zip")
  url <- glue("{ruta_base}{codigo_encuesta}{modulo}")
  # https://proyectos.inei.gob.pe/iinei/srienaho/descarga/SPSS/548-Modulo64.zip

  # Descargamos el archivo
  utils::download.file(url, temp, timeout = 120)

  # Listamos los archivos descargados y seleccionamos la base elegida
  archivos <- utils::unzip(temp, list = T)
  archivos <- archivos[stringr::str_detect(archivos$Name, paste0(base, "\\.")) == TRUE, ]

  # Elegimos entre guardar los archivos o pasarlos directamente a un objeto
  if (guardar == TRUE) {
    utils::unzip(temp, files = archivos$Name, exdir = paste(getwd(), "/", ruta, sep = ""))
    print(paste("Archivos descargados en: ", getwd(), "/", ruta, sep = ""))
  } else {
    data <- haven::read_sav(
      utils::unzip(
        temp,
        files = archivos$Name[grepl(".sav|.SAV", archivos$Name)],
        exdir = tempdir
      ),
      encoding = codificacion
    )
    nombres <- tolower(colnames(data))
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
  temp <- tempfile()
  tempdir <- tempdir() # Generamos dos objetos temporales: un archivo y una carpeta
  versiones <- matrix( # Genera una matriz con el número identificador de versiones por cada año
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
      2010, 266
    ),
    byrow = T, ncol = 2
  )

  codigo_encuesta <- versiones[versiones[, 1] == periodo, 2]  # Extrae el código de la encuesta con la matriz versiones
  modulo <- glue("-Modulo{codigo_modulo}.zip")
  ruta_base <- "https://proyectos.inei.gob.pe/iinei/srienaho/descarga/SPSS/"
  url <- glue::glue("{ruta_base}{codigo_encuesta}{modulo}")

  utils::download.file(url, temp, timeout = 120)  # Descargamos

  archivos <- utils::unzip(temp, list = T)  # Listamos los archivos
  archivos <- archivos[stringr::str_detect(archivos$Name, paste0(base, "\\.")) == TRUE, ]  # Seleccionamos la base

  if (guardar == TRUE) {
    utils::unzip(temp, files = archivos$Name, exdir = paste(getwd(), "/", ruta, sep = ""))
    print(paste("Archivos descargados en: ", getwd(), "/", ruta, sep = ""))
  } else {
    data <- haven::read_sav(utils::unzip(temp, files = archivos$Name[grepl(".sav|.SAV", archivos$Name)], exdir = tempdir), encoding = codificacion)
    nombres <- tolower(colnames(data))
    colnames(data) <- nombres
    return(data)
  }
}

#' Media ponederada
#'
#' @export
media_ponderada <- function(data, variable, groups, peso, total = T) {
  # data: El dataframe que contiene los datos
  # variable: El nombre de la variable a estimar (puede ser un vector de nombres para varias variables)
  # groups: El nombre de las variables de agrupación (puede ser un vector de nombres para varios grupos)
  # peso: El nombre de la variable de peso a utilizar en la media ponderada

  # Convertir las variables de agrupación y estimación a símbolos para usar en dplyr
  group_cols <- rlang::syms(groups)
  var_cols <- rlang::syms(variable)
  peso_col <- rlang::sym(peso)

  if (total == T) {
    # Crear una lista para almacenar los resultados de cada nivel de agrupación
    resultados <- list()

    # Calcular la media ponderada para cada nivel de agrupación
    for (i in seq_along(group_cols)) {
      current_group <- group_cols[1:i]
      media_ponderada <- data %>%
        dplyr::group_by(!!!current_group) %>%
        dplyr::summarise_at(vars(!!!var_cols), ~ collapse::fmean(.x, w = eval(peso_col)))

      # Agregar el nivel de agrupación como una columna nueva con etiquetas
      # Solo para el primer nivel de agrupación, los demás ya están incluidos en el data frame
      if (i < length(group_cols)) {
        if (i == 1) {
          j <- 2
          while (j <= length(group_cols)) {
            col_name <- paste(rlang::get_expr(group_cols[j]))
            media_ponderada[[col_name]] <- haven::labelled(x = 0, labels = c("Total" = 0))
            j <- j + 1
          }
        }
        lead <- i + 1
        lead_group <- group_cols[lead]
        col_name <- paste(rlang::get_expr(lead_group), collapse = "")
        media_ponderada[[col_name]] <- haven::labelled(x = 0, labels = c("Total" = 0))
      }

      resultados[[paste(rlang::get_expr(current_group), collapse = "_")]] <- media_ponderada
    }

    # Combinar los resultados en un único dataframe
    consolidado <- do.call(dplyr::bind_rows, resultados) %>%
      dplyr::arrange(!!!group_cols)
  } else {
    consolidado <- data %>%
      plyr::group_by(!!!group_cols) %>%
      plyr::summarise_at(vars(!!!var_cols), ~ collapse::fmean(.x, w = eval(peso_col)))
  }

  return(consolidado)
}
#' Suma ponderada
#'
#' @export
suma_ponderada <- function(data, variable, groups, peso, total = T) {
  # data: El dataframe que contiene los datos
  # variable: El nombre de la variable a estimar (puede ser un vector de nombres para varias variables)
  # groups: El nombre de las variables de agrupación (puede ser un vector de nombres para varios grupos)
  # peso: El nombre de la variable de peso a utilizar en la media ponderada

  # Convertir las variables de agrupación y estimación a símbolos para usar en dplyr
  group_cols <- rlang::syms(groups)
  var_cols <- rlang::syms(variable)
  peso_col <- rlang::sym(peso)

  if (total == T) {
    # Crear una lista para almacenar los resultados de cada nivel de agrupación
    resultados <- list()

    # Calcular la media ponderada para cada nivel de agrupación
    for (i in seq_along(group_cols)) {
      current_group <- group_cols[1:i]
      suma_ponderada <- data %>%
        dplyr::group_by(!!!current_group) %>%
        dplyr::summarise_at(vars(!!!var_cols), ~ collapse::fsum(.x, w = eval(peso_col)))

      # Agregar el nivel de agrupación como una columna nueva con etiquetas
      # Solo para el primer nivel de agrupación, los demás ya están incluidos en el data frame
      if (i < length(group_cols)) {
        if (i == 1) {
          j <- 2
          while (j <= length(group_cols)) {
            col_name <- paste(rlang::get_expr(group_cols[j]))
            suma_ponderada[[col_name]] <- haven::labelled(x = 0, labels = c("Total" = 0))
            j <- j + 1
          }
        }
        lead <- i + 1
        lead_group <- group_cols[lead]
        col_name <- paste(rlang::get_expr(lead_group), collapse = "")
        suma_ponderada[[col_name]] <- haven::labelled(x = 0, labels = c("Total" = 0))
      }

      resultados[[paste(rlang::get_expr(current_group), collapse = "_")]] <- suma_ponderada
    }

    # Combinar los resultados en un único dataframe
    consolidado <- do.call(dplyr::bind_rows, resultados) %>%
      dplyr::arrange(!!!group_cols)
  } else {
    consolidado <- data %>%
      plyr::group_by(!!!group_cols) %>%
      dplyr::summarise_at(vars(!!!var_cols), ~ collapse::fsum(.x, w = eval(peso_col)))
  }

  return(consolidado)
}
#' Tablas en formato INEI
#'
#' @export
inei_tabla <- function(wb, sheet, data, cuadro, titulo, subtitulo, fuente = NULL, nota = NULL, formato = "0%") {
  # Creamos la hoja
  openxlsx::addWorksheet(wb, sheetName = sheet)
  # Seteo inicial
  frow <- 4
  fcol <- 2

  # Número de cuadro
  openxlsx::writeData(wb, sheet,
    x = cuadro,
    startCol = fcol, startRow = frow
  )
  openxlsx::mergeCells(wb, sheet,
    cols = c(fcol:(ncol(data) + 1)),
    rows = (frow)
  )
  openxlsx::addStyle(wb, sheet,
    style = createStyle(
      halign = "center",
      textDecoration = "bold"
    ),
    cols = (fcol:(ncol(data) + 1)),
    rows = (frow)
  )
  # Título
  openxlsx::writeData(wb, sheet,
    x = toupper(titulo),
    startCol = 2,
    startRow = (frow + 1)
  )
  openxlsx::mergeCells(wb, sheet,
    cols = c(fcol:(ncol(data) + 1)),
    rows = (frow + 1)
  )
  openxlsx::addStyle(wb, sheet,
    style = createStyle(
      halign = "center",
      wrapText = T
    ),
    cols = (fcol:(ncol(data) + 1)),
    rows = (frow + 1)
  )
  # Subtítulo
  openxlsx::writeData(wb, sheet,
    x = str_to_sentence(subtitulo),
    startCol = 2,
    startRow = (frow + 2)
  )
  openxlsx::mergeCells(wb, sheet,
    cols = c(fcol:(ncol(data) + 1)),
    rows = (frow + 2)
  )

  openxlsx::addStyle(wb, sheet,
    style = createStyle(
      halign = "center"
    ),
    cols = (fcol:(ncol(data) + 1)),
    rows = (frow + 2)
  )
  # Data
  fcol_l <- fcol + 1
  fcol_u <- ncol(data) + 1
  frow_l <- frow + 4
  frow_u <- nrow(data) + frow_l

  openxlsx::writeData(
    wb, sheet,
    data,
    startCol = fcol,
    startRow = frow_l
  )

  for (i in c(frow_l:frow_u)) {
    openxlsx::addStyle(wb, sheet,
      style = createStyle(
        numFmt = formato,
        halign = "center"
      ),
      cols = (fcol_l:fcol_u),
      rows = (i)
    )
  }

  # Formato a encabezados
  openxlsx::addStyle(wb, sheet,
    style = createStyle(
      borderStyle = "medium",
      border = "TopBottom",
      halign = "center"
    ),
    cols = fcol:(ncol(data) + 1),
    rows = (frow + 4)
  )
  # Añadimos notas y fuente
  if (is.null(fuente) == F) {
    if (is.null(nota) == T) {
      openxlsx::writeData(wb, sheet,
        x = fuente,
        startCol = fcol,
        startRow = frow_u + 2
      )

      openxlsx::mergeCells(wb, sheet,
        cols = c(fcol:(ncol(data) + 1)),
        rows = (frow_u + 2)
      )
      openxlsx::addStyle(wb, sheet,
        style = createStyle(
          textDecoration = "bold",
          wrapText = T
        ),
        cols = c(fcol:(ncol(data) + 1)),
        rows = (frow_u + 2)
      )
    } else {
      lnota <- length(nota)
      # Nota
      openxlsx::writeData(wb, sheet,
        x = "Nota:",
        startCol = fcol,
        startRow = frow_u + 2
      )

      openxlsx::mergeCells(wb, sheet,
        cols = c(fcol:(ncol(data) + 1)),
        rows = (frow_u + 2)
      )
      openxlsx::addStyle(wb, sheet,
        style = createStyle(wrapText = T),
        cols = c(fcol:(ncol(data) + 1)),
        rows = (frow_u + 2)
      )
      for (cap in 1:lnota) {
        # Nota
        openxlsx::writeData(wb, sheet,
          x = nota[cap],
          startCol = fcol,
          startRow = frow_u + 2 + cap
        )

        openxlsx::mergeCells(wb, sheet,
          cols = c(fcol:(ncol(data) + 1)),
          rows = (frow_u + 2 + cap)
        )
        openxlsx::addStyle(wb, sheet,
          style = createStyle(wrapText = T),
          cols = c(fcol:(ncol(data) + 1)),
          rows = (frow_u + 2 + cap)
        )
      }

      # Fuente
      openxlsx::writeData(wb, sheet,
        x = fuente,
        startCol = fcol,
        startRow = frow_u + 3 + lnota
      )

      openxlsx::mergeCells(wb, sheet,
        cols = c(fcol:(ncol(data) + 1)),
        rows = (frow_u + 3 + lnota)
      )
      openxlsx::addStyle(wb, sheet,
        style = createStyle(
          textDecoration = "bold",
          wrapText = T
        ),
        cols = c(fcol:(ncol(data) + 1)),
        rows = (frow_u + 3 + lnota)
      )
    }
  }

  openxlsx::addStyle(wb, sheet,
    style = createStyle(
      borderStyle = "medium",
      border = "top"
    ),
    cols = fcol:(ncol(data) + 1),
    rows = (frow_u + 1)
  )

  openxlsx::setColWidths(wb, sheet,
    widths = "auto",
    cols = c((fcol + 1):(ncol(data) + 1))
  )

  openxlsx::setRowHeights(wb, sheet,
    heights = 25,
    rows = frow + 1
  )
}
