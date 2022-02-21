#' Title
#'
#' @return
#' @export
#'
#' @examples
fetch_data <- function(sheetIndex) {

  data_path <- Sys.getenv("DATA_PATH")
  data_file <- Sys.getenv("DATA_FILE")

  complete_path <- paste(data_path, data_file, sep = "/")


  # load data.table
  dt <- read_xlsx(
    path = complete_path,
    sheet = sheetIndex
  ) %>% as.data.table()

  return(dt)

}

#' Title
#'
#' @param dt (data.table) with fetch_data format
#'
#' @return
#' @export
#'
#' @examples
process <- function(dt) {

  # drop rows with all columns as NA's
  dt <- dt[rowSums(is.na(dt)) != length(dt)]

  # fill nas
  # numerical columns
  num_cols <- names(dt)[sapply(dt, class) %>% grepl(pattern = "num|int")]
  lapply(
    num_cols,
    function(col) set(dt, which(is.na(dt[[col]])), col, 0)
  )

  # categorical columns
  cat_cols <- names(dt)[sapply(dt, class) %>% grepl(pattern = "char")]
  lapply(
    cat_cols,
    function(col) set(dt, which(is.na(dt[[col]])), col, "indefinido")
  )

  # melt numerical columns
  dt <- melt(dt, id.vars = cat_cols, value.name = "Suma")

  # create new vars
  dt[, `:=` (
    Curso = ifelse(grepl("1", variable), 1, 2),
    Editorial = ifelse(
      grepl("SM", variable),
      "SM",
      ifelse(
        grepl("Santillana", variable),
        "Santillana",
        "VV"
      )
    )
  )]

  # drop auxiliary columns
  dt[, variable := NULL]

  # Transform column names
  setnames(
    dt,
    old = names(dt),
    new = sapply(
      names(dt),
      function(col_name) {
        return(gsub(pattern = ".", replacement = " ", x = col_name, fixed = T))
      }
    )
  )

  # get name of table to study
  tbl_name = names(dt)[
    ! grepl(pattern = "^Distribuc|^Suma|^Curso|^Editorial", x = names(dt))
  ]

  # create tbl_name column
  dt[, tbl_name := tbl_name]

  # change col name in data.table
  setnames(dt, old = tbl_name, new = "Clases observadas")

  # set column order
  setcolorder(
  	dt,
  	c("tbl_name", "Clases observadas",
  	  names(dt)[grepl("^Distribuc", names(dt))][1],
  	 "Curso", "Editorial",  "Suma")
  )

  # factorize all vars except Suma
  vars_to_factor <- names(dt) %>% setdiff("Suma")
  dt[, (vars_to_factor) := lapply(.SD, factor), .SDcols = vars_to_factor]

  return(dt)

}

#' Title
#'
#' @param dt
#' @param path
#' @param file
#'
#' @return
#' @export
#'
#' @examples
save_dt <- function(dt, path, file) {

  # create dir if does not exist
  dir.create(path, recursive = T, showWarnings = F)

  # save data.table as Rds
  saveRDS(dt, file = paste(path, file, sep = "/"))

}



#' Title
#'
#' @param dt_list
#' @param all_cursos
#' @param all_editoriales
#' @param input
#'
#' @return
#' @export
#'
#' @examples
reactive_filter <- function(
  input, dt_list, all_cursos, all_editoriales
) {

  dt_filtered <- dt_list[[input$tbl]]

  if ("tbl_name" %in% names(dt_filtered)) {
  	dt_filtered[, tbl_name := NULL]
  }

  if (! all_cursos) {
    dt_filtered <- dt_filtered[Curso == input$curso] %>%
    .[, Curso := NULL]
  }

  if (! all_editoriales) {
    dt_filtered <- dt_filtered[Editorial == input$editorial] %>%
    .[, Editorial := NULL]
  }

  return(dt_filtered)

}

#' Title
#'
#' @param dt_plot
#' @param agg_curso
#' @param agg_editorial
#'
#' @return
#' @export
#'
#' @examples
reactive_bar_chart <- function(dt_plot, agg_curso = F, agg_editorial = F) {

	if (! agg_curso) {

		dt_plot <- dt_plot[
			,
			.(Suma = sum(Suma)),
			by = setdiff(names(dt_plot), c("Curso", "Suma"))
		]

	}

	if (! agg_editorial) {

		dt_plot <- dt_plot[
			,
			.(Suma = sum(Suma)),
			  by = setdiff(names(dt_plot), c("Editorial", "Suma"))
		]

	}

	plot <- dt_plot %>% ggplot(
		aes(
			x = `Clases observadas`,
			y = Suma,
			fill = `Distribución de personajes en la imagen`
		)
	) + geom_col()

	if (agg_curso) {

		if (agg_editorial) {
			plot <- plot + facet_wrap(~ Curso + Editorial)
		} else {
			plot <- plot + facet_wrap(~ Curso)
		}

	} else if (agg_editorial) {
		plot <- plot + facet_wrap(~ Editorial)
	}

	plot <- plot +
		theme(axis.text.x = element_text(angle = 65, vjust = 0.6)) +
		theme(legend.text = element_text(size = 6)) +
		labs(x = "", y = "Total observaciones")

	return(plot)

}

#' Title
#'
#' @param plot
#'
#' @return
#' @export
#'
#' @examples
custom_ggplotly <- function(plot) {

	plotly <- ggplotly(plot) %>%
		layout(
			showlegend = TRUE,
			legend = list(
				y = 0.5,
				font = list(size = 8),
				title = list(font = list(size = 12)),
				tracegroupgap = 6
			)
		)

	return(plotly)
}



















