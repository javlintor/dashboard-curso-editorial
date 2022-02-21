library(shiny)
library(data.table)
library(ggplot2)
library(plotly)
source("R/utils.R")

load("data/dt.rda")

# tables
tbl_names <- dt$tbl_name %>% unique
names(tbl_names) <- tbl_names
dt_list <- split(dt, by = "tbl_name")

# cursos
cursos <- dt$Curso %>% unique
cursos <- c("Todos", cursos)

# editoriales
editoriales <- dt$Editorial %>% unique
editoriales <- c("Todos", editoriales)


# ------------------------------------------------------------------------------
# ------------------------         UI         ----------------------------------
# ------------------------------------------------------------------------------

# Define UI for app
ui <- fluidPage(

	title = "Natalia Fdez",
	lang = "es",
	# css stylesheet
	includeCSS("./www/styles.css"),


	# App title ----

	div(
		h1("Proyecto de Investigación Educativa"),
		h3("Natalia Fernández"),
		class = "title-container"
	),


	# Sidebar layout with input and output definitions ----
	sidebarLayout(


		# Sidebar panel for inputs ----
		sidebarPanel(
			width = 3,
			# Input: Select the random distribution type ----
			radioButtons(
				"tbl",
				"Seleccione una tabla:",
				tbl_names
			)
		),

		# Main panel for displaying outputs ----
		mainPanel(

			width = 9,

			# create some tabs
			tabsetPanel(

				tabPanel(
					"Gráfico",
					# Multiple selectors for aggregation level
					fluidRow(
						column(
							2,
							checkboxInput(
								inputId = "agg_curso",
								label = "Desagrega por curso"
							)
						),
						column(
							2,
							checkboxInput(
								inputId = "agg_editorial",
								label = "Desagrega por Editorial"
							)
						),
						column(
							2,
							# download image button
							downloadButton('downloadPlot', 'Download Plot')
						),
						class = "selector-container"
					),

					# Insert plot
					plotlyOutput(
						outputId = "plotly",
						height = "700px"
					)
				),

				tabPanel(
					"Tabla",
					#selectors
					fluidRow(
						column(
							6,
							# cursos selector
							selectInput(
								inputId = "curso",
								label = "Curso",
								choices = cursos

							)
						),
						column(
							6,
							# editorial selector
							selectInput(
								inputId = "editorial",
								label = "Editorial",
								choices = editoriales

							)
						),
						class = "selector-container"
					),
					# insert table
					dataTableOutput(outputId = "table")
				)
			)

		)

	)
)


# ------------------------------------------------------------------------------
# ------------------------       SERVER       ----------------------------------
# ------------------------------------------------------------------------------



# Define server logic for random distribution app ----
server <- function(input, output) {

	# all options
	all_cursos <- reactive(input$curso == "Todos")
	all_editoriales <- reactive(input$editorial == "Todos")

	# render table
	dt_reactive <- reactive(
		reactive_filter(
			input, dt_list, all_cursos(), all_editoriales()
		)
	)

	# create table output
	output$table <- renderDataTable(
		dt_reactive(),
		options = list(pageLength = 10, lengthMenu = c(5, 10, 20))
	)

	# render plot
	plotly_reactive <- reactive(
		reactive_bar_chart(
			dt_plot = dt_reactive(),
			agg_curso = input$agg_curso,
			agg_editorial = input$agg_editorial
		) %>% custom_ggplotly()
	)

	# render plot output
	output$plotly <- renderPlotly(
		plotly_reactive()
	)

	output$downloadPlot <- downloadHandler(
		filename = "Shinyplot.png",
		contentType = "image/png",
		content = function(file) {
			png(file)
			reactive_bar_chart(
				dt_plot = dt_reactive(),
				agg_curso = input$agg_curso,
				agg_editorial = input$agg_editorial
			) %>% print()
			dev.off()
		})

}

shinyApp(ui = ui, server = server)
