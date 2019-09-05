

shinyUI(fluidPage(
      useShinyjs(),
      inlineCSS("loading.css"),
      tags$head(tags$style("
                  #container * {display: inline;}")),
      div(class = "topbar", style="height: 6px;
                                  background: transparent linear-gradient(to right, #ec77ab 0%, #7873f5 100%) repeat scroll 0% 0%;
                                  margin-left: -15px;
                                  margin-right: -15px;"),
      a(img(src="log-20190722.png",style="margin-left: 30px;margin-top:30px"),href="http://bioinfo.vanderbilt.edu/database/Immu-Mela/"),
      tags$style(HTML("
        .tabbable > .nav {margin-left: 300px; margin-right: 300px;
                        border-color: transparent;
                        margin-bottom: 30px}
       .tabbable > .nav > li {width:auto; }
       .tabbable > .nav > li > a {
                                  font-family: 'Segoe UI', Helvetica, Arial, sans-serif;
                                  font-size: 30px;
                                  border-color: transparent;
                                  color: black}
       .tabbable > .nav > li[class=active] > a {background-color: transparent; 
                                                border-color: transparent;
                                                font-size: 30px;
                                                border-image: linear-gradient(to right, #ec77ab 0%, #7873f5 100%) 1 1 stretch;
                                                border-image-width: 0 0 2 0;
                                                color: #663399}
       .tabbable > .nav > li:hover {background-color: transparent; 
                                   border-color: transparent;
                                   font-size: 30px; 
                                   border-image: linear-gradient(to right, #ec77ab 0%, #7873f5 100%) 1 1 stretch;
                                   border-image-width: 0 0 1 0;
                                   color: #663399}
       .dropdown-menu > li >a {
                                  font-family: 'Segoe UI', Helvetica, Arial, sans-serif;
                                  font-size: 25px;
                                  border-color: transparent;
                                  color: black}
       .tabbable > .nav > li.active > a, 
       .dropdown-menu > li.active > a{color: inherit; 
                                      background-color: inherit;
                                      font-size: 30px;
                                      border-image: linear-gradient(to right, #ec77ab 0%, #7873f5 100%) 1 1 stretch;
                                      border-image-width: 0 0 2 0;}
       .dropdown-menu > li > a:hover {color: inherit;
                                      background-color: inherit;
                                      
                                      text-decoration: underline;
                                      border-image: linear-gradient(to right, #ec77ab 0%, #7873f5 100%) 1 1 stretch;
                                      border-image-width: 0 0 2 0;}
                                      
       
       .tabbable > .nav >.dropdown-menu > .active > a, 
       .tabbable > .nav >.dropdown-menu > .active > a:focus, 
       .tabbable > .nav >.dropdown-menu > .active > a:hover {color: inherit;font-size: 25px;
                                           background-color: inherit;
                                           text-decoration: underline;border-image: linear-gradient(to right, #ec77ab 0%, #7873f5 100%) 1 1 stretch;
                                           border-image-width: 0 0 2 0;}
       div.intro-divider {
                    height: 6px;
                    background: transparent linear-gradient(to right, #ec77ab 0%, #7873f5 100%) repeat scroll 0% 0%;
                    margin-top: 20px;
                    margin-bottom: 20px;
                    margin-left: 275px;
                    margin-right: 275px;}
        div.intro-divider-sub {
                   height: 5px;
                    background: transparent linear-gradient(to right, #DCDCDC 0%, #DCDCDC 100%) repeat scroll 0% 0%;
                    margin-top: 20px;
                    margin-bottom: 20px;
                    margin-left: 15px;
                    margin-right: 15px;}
       .col-sm-8 > .tabbable > .nav {margin-left: 0px; 
                                    font-size: 10px;
                                    margin-bottom: 20px;
                                    border-color: transparent;}
       .col-sm-8 > .tabbable > .nav  > li > a {font-size: 25px;}
        div.container {display: inline;}
        .selectize-dropdown [data-value=ALL] { color: red }
    ")),
  div(id="loading-content",h2("Loading..."),style="text-align:center"),
  hidden(div(id="app-content",tabsetPanel(
    tabPanel ("HOME",
              
              div(img(src='background3.jpg'),style="background: url('background3.jpg');
			                                     background-repeat: repeat;
                                          background-size: auto;
                                          margin-left: -15px;
                                          margin-right: -15px;
                                          margin-top: 20px;
                                          margin-bottom: 20px;
                                          height: inherit;"),
              p ("Immu-Mela is an open-access resource for interactive exploration of a variety of known and novel 
      genomic signatures predictive of immunotherapy response across multiple datasets in melanoma.The current version of 
         the Immu-Mela contains 1008 samples derived from both bulk sequencing and single-cell RNA sequencing. 
         The association between each or integrative genomic profiling and clinical outcome can be queried, 
         downloaded and visualized across multiple datasets to explore the consistency of signatures. 
         Each genomic profiling provides multiple types of features, including the known or novel signatures 
         predictive of immunotherapy response.",  
                 style="font-size:20px;
                 margin-left: 275px;
                 margin-right: 275px;"),
              p("-- Genetic profiling supports tumor mutational burden, and gene mutation, and mutational signatures.",
                style="font-size:20px;
                 margin-left: 275px;
                 margin-right: 275px;"),
              p("-- Transcriptomic profiling includes gene/gene sets expression, gene expression relations, and immune cell components.",
                style="font-size:20px;
                 margin-left: 275px;
                 margin-right: 275px;"),
              p("-- Integration provides combination analysis between features.",
                style="font-size:20px;
                 margin-left: 275px;
                 margin-right: 275px;"),
              p("-- Single-cell supports specific gene expression signals and specific cell populations.",
                style="font-size:20px;
                 margin-left: 275px;
                 margin-right: 275px;"),
              p("-- Users' data provides uploading function for users' own data (unrestricted to any cancer types).",
                style="font-size:20px;
                 margin-left: 275px;
                 margin-right: 275px;"),
              div(class="intro-divider"),
              fluidRow(column(width=3,plotlyOutput("pieplot1")),
                       column(width=3,plotlyOutput("pieplot2")),
                       column(width=3,plotlyOutput("pieplot3")),
                       column(width=3,plotlyOutput("pieplot4")),
                       style="margin-left:275px;margin-right:275px;margin-top:-10px") ,
              fluidRow(column(width=3,h4("Ten data sources",style="text-align:center")),
                       column(width=3,h4("Two types of drug",style="text-align:center")),
                       column(width=3,h4("Pre- and On- treatment",style="text-align:center")),
                       column(width=3,h4("Immunotherapy response",style="text-align:center")),
                       style="margin-left:275px;margin-right:275px;margin-top:-10px"),
              div(class="intro-divider"),
              
              div(DT::dataTableOutput("start_table"),style="margin-left:275px;margin-right:275px;margin-top:10px;margin-bottom:10px"),
                      div(class = "topbar", style="height: 6px;
                                  background: transparent linear-gradient(to right, #ec77ab 0%, #7873f5 100%) repeat scroll 0% 0%;
                                  margin-left: -15px;
                                  margin-right: -15px;
                                  margin-bottom:20px"),
              p("@Vanderbilt University Medical Center",
                style="color:darkgrey;
                margin-down:-1px;
                text-align:center"),
              p("Center for Quantitative Sciences",
                style="color:darkgrey;
                margin-down:-1px;
                text-align:center"),
              
              p("497A Preston Research Building | Nashville, TN 37232 | 615-322-6618",
                style="color:darkgrey;
                margin-down:1px;
                text-align:center")),
    navbarMenu("Genetic",
               tabPanel ("Mutation",
                         h4("Mutation",style="font-size:25px;
		                                  text-align: center;
		                                  margin-top: 20px;
                                      color: #663399"),
                         div(p("Exploring the association of mutations to immunotherapy response, overall survival and progression free survival.",
                               style="font-size:20px;
                  font-weight:bold;
                  margin-left: 500px;
                  margin-right: 500px;
                  text-align: center;
                  position:absolute;
                  color:white"),
                             div(img(src='background4.jpg'),style="background: url('background4.jpg');
			                                     background-repeat: repeat;
                                          background-size: auto;
                                          margin-left: -15px;
                                          margin-right: -15px;
                                          margin-top: 20px;
                                          margin-bottom: 20px;
                                          height: inherit;"),style="position:relative"),
                         
                         #div(class = "intro-divider-sub"),
                         sidebarPanel(h4("Selection",style="color: #663399;font-size:25px"),
                                      uiOutput("data_source_mutation"),
                                      uiOutput("treatment_mutation"),
                                      uiOutput("description_mutation"),
                                      selectizeInput("gene_mutation",
                                                     "Insert a gene(s), for example: BRAF KRAS",
                                                     choices=NULL,
                                                     multiple=TRUE),
                                      #                       uiOutput("gene_mutation"),
                                      #                       textInput("gene_mutation",
                                      #                                 "Insert a gene or gene set, for example: BRAF,KRAS",
                                      #                                 "BRAF,KRAS"),
                                      actionButton("do_mutation","Go",style="background-color: lightblue")),
                         #                       uiOutput("slider_mutation")),
                         conditionalPanel(condition="input.do_mutation",
                                          mainPanel(tabsetPanel(
                                            tabPanel("Immunotherapy response",
                                                     #                     wellPanel (radioButtons("mutation_plots_result","Groups: ",
                                                     #                                             c("Progression free survival status","Overall survival status"))),
                                                     wellPanel(div(id="container",
                                                                   h4("Number of selected samples: "),
                                                                   div(textOutput("mutation_plots_n_samples_NA"),style="font-family: 'Lobster', cursive; color: #ad1d28; font-size: 20px"),
                                                                   style="margin-bottom:30px"),
                                                               #                               div(plotlyOutput("mutation_plots_plots_bar",height="auto")),
                                                               #                               div(plotlyOutput("mutation_plots_plots",height="auto"))
                                                               uiOutput("mutation_plots_plots",height = "auto")
                                                     ),
                                                     wellPanel (div(id="container",
                                                                    h4("Data format:   "),
                                                                    radioButtons("mutation_plots_type","",
                                                                                 choices = c("Excel (CSV)","Text (TSV)", "JSON"),
                                                                                 inline = TRUE),
                                                                    style="margin-bottom:30px"),
                                                                div(downloadButton("mutation_plots_download", "Download"),style="margin-bottom:30px"),
                                                                div(DT::dataTableOutput("mutation_plots_table"),style="background-color: lightblue;overflow-y: scroll;overflow-x: scroll"))),
                                            tabPanel("Overall survival",
                                                     
                                                     wellPanel(div(id="container",
                                                                   h4("Number of selected samples:"),
                                                                   div(textOutput("mutation_survival_n_samples_NA"),style="font-family: 'Lobster', cursive; color: #ad1d28; font-size: 20px"),
                                                                   style="margin-bottom:30px"),
                                                               uiOutput("mutation_survival_plots",height = "auto")),
                                                     wellPanel(div(id="container",
                                                                   h4("Data format:   "),
                                                                   radioButtons("mutation_survival_type","",
                                                                                choices = c("Excel (CSV)","Text (TSV)", "JSON"),
                                                                                inline = TRUE),
                                                                   style="margin-bottom:30px"),
                                                               div(downloadButton("mutation_survival_download", "Download"),style="margin-bottom:30px"),
                                                               div(DT::dataTableOutput("mutation_survival_table"),style="background-color: lightblue;overflow-y: scroll;overflow-x: scroll"))),#### ??????????????????,?????????????????????????????????
                                            tabPanel("Progression free survival",
                                                     
                                                     wellPanel(div(id="container",
                                                                   h4("Number of selected samples:"),
                                                                   div(textOutput("mutation_PFS_n_samples_NA"),style="font-family: 'Lobster', cursive; color: #ad1d28; font-size: 20px"),
                                                                   style="margin-bottom:30px"),
                                                               uiOutput("mutation_PFS_plots",height = "auto")),
                                                     wellPanel(div(id="container",
                                                                   h4("Data format:   "),
                                                                   radioButtons("mutation_PFS_type","",
                                                                                choices = c("Excel (CSV)","Text (TSV)", "JSON"),
                                                                                inline = TRUE),
                                                                   style="margin-bottom:30px"),
                                                               div(downloadButton("mutation_PFS_download", "Download"),style="margin-bottom:30px"),
                                                               div(DT::dataTableOutput("mutation_PFS_table"),style="background-color: lightblue;overflow-y: scroll;overflow-x: scroll")))#### ??????????????????,?????????????????????????????????          
                                          )))),
               
               tabPanel ("Mutational loads/burden",
                         h4("Mutational loads/burden",style="font-size:25px;
		                                  text-align: center;
		                                  margin-top: 20px;color: #663399"),
                         div(p("Exploring the association of tumor mutational burden (TMB) to immunotherapy response, overall survival and progression free survival.",
                               style="font-size:20px;
		            font-weight:bold;
                 margin-left: 500px;
                 margin-right: 500px;
		            text-align: center;
		            position:absolute;
                  color:white"),
                             div(img(src='background4.jpg'),style="background: url('background4.jpg');
			                                     background-repeat: repeat;
                                          background-size: auto;
                                          margin-left: -15px;
                                          margin-right: -15px;
                                          margin-top: 20px;
                                          margin-bottom: 20px;
                                          height: inherit;"),style="position:relative"),
                         #div(class = "intro-divider-sub"),
                         sidebarPanel(h4("Selection",style="color: #663399;font-size: 25px"),
                                      uiOutput("data_source_mutation_loads"),
                                      uiOutput("treatment_mutation_loads"),
                                      uiOutput("description_mutation_loads"),
                                      actionButton("do_mutation_loads","Go",style="background-color: lightblue")),
                         #		                       uiOutput("slider_mutation_loads")),
                         conditionalPanel(condition="input.do_mutation_loads",
                                          mainPanel(tabsetPanel(
                                            tabPanel("Immunotherapy response",
                                                     #		                     wellPanel (radioButtons("mutation_loads_plots_result","Groups: ",
                                                     #		                                             c("Progression free survival status","Overall survival status"))),
                                                     wellPanel (div(id="container",
                                                                    h4("Number of selected samples: "), 
                                                                    div(textOutput("mutation_loads_plots_n_samples_NA"),style="font-family: 'Lobster', cursive; color: #ad1d28; font-size: 20px"),
                                                                    style="margin-bottom:30px"),
                                                                uiOutput("mutation_loads_plots_plots")
                                                                #		                                plotlyOutput("mutation_loads_plots_plots",height="auto")
                                                     ),
                                                     wellPanel (div(id="container",
                                                                    h4("Data format:   "),
                                                                    radioButtons("mutation_loads_plots_type","",
                                                                                 choices = c("Excel (CSV)","Text (TSV)", "JSON"),
                                                                                 inline = TRUE),
                                                                    style="margin-bottom:30px"),
                                                                div(downloadButton("mutation_loads_plots_download", "Download"),style="margin-bottom:30px"),
                                                                div(DT::dataTableOutput("mutation_loads_plots_table"),style="background-color: lightblue;overflow-y: scroll;overflow-x: scroll"))),#### ??????????????????,?????????????????????????????????
                                            tabPanel("Overall survival",
                                                     wellPanel(uiOutput("slider_mutation_loads_OS")),
                                                     wellPanel(div(id="container",
                                                                   h4("Number of selected samples:"),
                                                                   div(textOutput("mutation_loads_survival_n_samples_NA"),style="font-family: 'Lobster', cursive; color: #ad1d28; font-size: 20px"),
                                                                   style="margin-bottom:30px"),
                                                               uiOutput("mutation_loads_survival_plots")),
                                                     wellPanel(div(id="container",
                                                                   h4("Data format:   "),
                                                                   radioButtons("mutation_loads_survival_type","",
                                                                                choices = c("Excel (CSV)","Text (TSV)", "JSON"),
                                                                                inline = TRUE),
                                                                   style="margin-bottom:30px"),
                                                               div(downloadButton("mutation_loads_survival_download", "Download"),style="margin-bottom:30px"),
                                                               div(DT::dataTableOutput("mutation_loads_survival_table"),style="background-color: lightblue;overflow-y: scroll;overflow-x: scroll"))),
                                            tabPanel("Progression free survival",
                                                     wellPanel(uiOutput("slider_mutation_loads_PFS")),
                                                     wellPanel(div(id="container",
                                                                   h4("Number of selected samples:"),
                                                                   div(textOutput("mutation_loads_PFS_n_samples_NA"),style="font-family: 'Lobster', cursive; color: #ad1d28; font-size: 20px"),
                                                                   style="margin-bottom:30px"),
                                                               uiOutput("mutation_loads_PFS_plots")),
                                                     wellPanel(div(id="container",
                                                                   h4("Data format:   "),
                                                                   radioButtons("mutation_loads_PFS_type","",
                                                                                choices = c("Excel (CSV)","Text (TSV)", "JSON"),
                                                                                inline = TRUE),
                                                                   style="margin-bottom:30px"),
                                                               div(downloadButton("mutation_loads_PFS_download", "Download"),style="margin-bottom:30px"),
                                                               div(DT::dataTableOutput("mutation_loads_PFS_table"),style="background-color: lightblue;overflow-y: scroll;overflow-x: scroll")))#### ??????????????????,?????????????????????????????????
                                          )))),
               tabPanel ("Mutational signatures",
                         h4("Mutational signatures",style="font-size:25px;
		                                  text-align: center;
		                                  margin-top: 20px;
                                      color: #663399"),
                         div(p("Exploring the association of mutational signatures to immunotherapy response, overall survival and progression free survival.",
                               style="font-size:20px;
                  font-weight:bold;
                  margin-left: 500px;
                  margin-right: 500px;
                  text-align: center;
                  position:absolute;
                  color:white"),
                             div(img(src='background4.jpg'),style="background: url('background4.jpg');
			                                     background-repeat: repeat;
                                          background-size: auto;
                                          margin-left: -15px;
                                          margin-right: -15px;
                                          margin-top: 20px;
                                          margin-bottom: 20px;
                                          height: inherit;"),style="position:relative"),
                         #div(class = "intro-divider-sub"),
                         sidebarPanel(h4("Selection",style="color: #663399;font-size: 25px"),
                                      uiOutput("data_source_mutation_signature"),
                                      uiOutput("treatment_mutation_signature"),
                                      uiOutput("description_mutation_signature"),
                                      uiOutput("signature_mutation_signature"),
                                      actionButton("do_mutation_signature","Go",style="background-color: lightblue")),
                         #                       uiOutput("slider_mutation_signature")),
                         conditionalPanel(condition="input.do_mutation_signature",
                                          mainPanel(tabsetPanel(
                                            tabPanel("Immunotherapy response",
                                                     #                     wellPanel (radioButtons("mutation_signature_plots_result","Groups: ",
                                                     #                                             c("Progression free survival status","Overall survival status"))),
                                                     wellPanel (div(id="container",
                                                                    h4("Number of selected samples: "), 
                                                                    div(textOutput("mutation_signature_plots_n_samples_NA"),style="font-family: 'Lobster', cursive; color: #ad1d28; font-size: 20px"),
                                                                    style="margin-bottom:30px"),
                                                                uiOutput("mutation_signature_plots_plots")),
                                                     wellPanel (div(id="container",
                                                                    h4("Data format:   "),
                                                                    radioButtons("mutation_signature_plots_type","",
                                                                                 choices = c("Excel (CSV)","Text (TSV)", "JSON"),
                                                                                 inline = TRUE),
                                                                    style="margin-bottom:30px"),
                                                                div(downloadButton("mutation_signature_plots_download", "Download"),style="margin-bottom:30px"),
                                                                div(DT::dataTableOutput("mutation_signature_plots_table"),style="background-color: lightblue;overflow-y: scroll;overflow-x: scroll"))),#### ??????????????????,?????????????????????????????????
                                            tabPanel("Overall survival",
                                                     wellPanel(uiOutput("slider_mutation_signature_OS")),
                                                     wellPanel(div(id="container",
                                                                   h4("Number of samples selected:"),
                                                                   div(textOutput("mutation_signature_survival_n_samples_NA"),style="font-family: 'Lobster', cursive; color: #ad1d28; font-size: 20px"),
                                                                   style="margin-bottom:30px"),
                                                               uiOutput("mutation_signature_survival_plots")),
                                                     wellPanel(div(id="container",
                                                                   h4("Data format:   "),
                                                                   radioButtons("mutation_signature_survival_type","",
                                                                                choices = c("Excel (CSV)","Text (TSV)", "JSON"),
                                                                                inline = TRUE),
                                                                   style="margin-bottom:30px"),
                                                               div(downloadButton("mutation_signature_survival_download", "Download"),style="margin-bottom:30px"),
                                                               div(DT::dataTableOutput("mutation_signature_survival_table"),style="background-color: lightblue;overflow-y: scroll;overflow-x: scroll"))),
                                            tabPanel("Progression free survival",
                                                     wellPanel(uiOutput("slider_mutation_signature_PFS")),
                                                     wellPanel(div(id="container",
                                                                   h4("Number of samples selected:"),
                                                                   div(textOutput("mutation_signature_PFS_n_samples_NA"),style="font-family: 'Lobster', cursive; color: #ad1d28; font-size: 20px"),
                                                                   style="margin-bottom:30px"),
                                                               uiOutput("mutation_signature_PFS_plots")),
                                                     wellPanel(div(id="container",
                                                                   h4("Data format:   "),
                                                                   radioButtons("mutation_signature_PFS_type","",
                                                                                choices = c("Excel (CSV)","Text (TSV)", "JSON"),
                                                                                inline = TRUE),
                                                                   style="margin-bottom:30px"),
                                                               div(downloadButton("mutation_signature_PFS_download", "Download"),style="margin-bottom:30px"),
                                                               div(DT::dataTableOutput("mutation_signature_PFS_table"),style="background-color: lightblue;overflow-y: scroll;overflow-x: scroll")))#### ??????????????????,?????????????????????????????????
                                          ))))),
    navbarMenu("Transcriptomic",
               tabPanel ("Expression",
                         h4("Gene expression",style="font-size:25px;
		                                  text-align: center;
		                                  margin-top: 40px;color: #663399"),
                         div(p("Exploring the association of gene expression to immunotherapy response, overall survival and progression free survival.",
                               style="font-size:20px;
                  font-weight:bold;
                  margin-left: 500px;
                  margin-right: 500px;
                  text-align: center;
                  position:absolute;
                  color:white"),
                             div(img(src='background4.jpg'),style="background: url('background4.jpg');
			                                     background-repeat: repeat;
                                          background-size: auto;
                                          margin-left: -15px;
                                          margin-right: -15px;
                                          margin-top: 20px;
                                          margin-bottom: 20px;
                                          height: inherit;"),style="position:relative"),
                         #div(class = "intro-divider-sub"),
                         sidebarPanel(h4("Selection",style="color: #663399;font-size: 25px"),
                                      uiOutput("data_source_expression"),
                                      uiOutput("treatment_expression"),
                                      uiOutput("description_expression"),
                                      selectizeInput("gene_expression","Insert a gene, for example: CTLA4",choices=NULL),
                                      #		                       textInput("gene_expression",
                                      #		                                Insert a gene, for example: PUM2",
                                      #		                                "PUM2"),
                                      actionButton("do_expression","Go",style="background-color: lightblue")),
                         #		                       uiOutput("slider_expression")),
                         conditionalPanel(condition="input.do_expression",
                                          mainPanel(tabsetPanel(
                                            tabPanel("Immunotherapy response",
                                                     #                                wellPanel (radioButtons("expression_plots_result","Groups: ",
                                                     #                                                        c("Progression free survival status","Overall survival status"))),
                                                     wellPanel (div(id="container",
                                                                    h4("Number of selected samples: "), 
                                                                    div(textOutput("expression_plots_n_samples_NA"),style="font-family: 'Lobster', cursive; color: #ad1d28; font-size: 20px"),
                                                                    style="margin-bottom:30px"),
                                                                uiOutput("expression_plots_plots")),
                                                     wellPanel (div(id="container",
                                                                    h4("Data format:   "),
                                                                    radioButtons("expression_plots_type","",
                                                                                 choices = c("Excel (CSV)","Text (TSV)", "JSON"),
                                                                                 inline = TRUE),
                                                                    style="margin-bottom:30px"),
                                                                div(downloadButton("expression_plots_download", "Download"),style="margin-bottom:30px"),
                                                                div(DT::dataTableOutput("expression_plots_table"),style="background-color: lightblue;overflow-y: scroll;overflow-x: scroll"))),#### ??????????????????,?????????????????????????????????
                                            tabPanel("Overall survival",
                                                     wellPanel(uiOutput("slider_expression_OS")),
                                                     wellPanel(div(id="container",
                                                                   h4("Number of samples selected:"),
                                                                   div(textOutput("expression_survival_n_samples_NA"),style="font-family: 'Lobster', cursive; color: #ad1d28; font-size: 20px"),
                                                                   style="margin-bottom:30px"),
                                                               uiOutput("expression_survival_plots")),
                                                     wellPanel(div(id="container",
                                                                   h4("Data format:   "),
                                                                   radioButtons("expression_survival_type","",
                                                                                choices = c("Excel (CSV)","Text (TSV)", "JSON"),
                                                                                inline = TRUE),
                                                                   style="margin-bottom:30px"),
                                                               div(downloadButton("expression_survival_download", "Download"),style="margin-bottom:30px"),
                                                               div(DT::dataTableOutput("expression_survival_table"),style="background-color: lightblue;overflow-y: scroll;overflow-x: scroll"))),#### ??????????????????,?????????????????????????????????
                                            tabPanel("Progression free survival",
                                                     wellPanel(uiOutput("slider_expression_PFS")),
                                                     wellPanel(div(id="container",
                                                                   h4("Number of samples selected:"),
                                                                   div(textOutput("expression_PFS_n_samples_NA"),style="font-family: 'Lobster', cursive; color: #ad1d28; font-size: 20px"),
                                                                   style="margin-bottom:30px"),
                                                               uiOutput("expression_PFS_plots",height = "auto")),
                                                     wellPanel(div(id="container",
                                                                   h4("Data format:   "),
                                                                   radioButtons("expression_PFS_type","",
                                                                                choices = c("Excel (CSV)","Text (TSV)", "JSON"),
                                                                                inline = TRUE),
                                                                   style="margin-bottom:30px"),
                                                               div(downloadButton("expression_PFS_download", "Download"),style="margin-bottom:30px"),
                                                               div(DT::dataTableOutput("expression_PFS_table"),style="background-color: lightblue;overflow-y: scroll;overflow-x: scroll")))#### ??????????????????,?????????????????????????????????
                                          )))),
               tabPanel ("Expression sum",
                         h4("Gene expression sum",style="font-size:25px;
                                                     text-align: center;
                                                     margin-top: 20px;color: #663399"),
                         div(p("Exploring the association of genes expression sum to immunotherapy response, overall survival and progression free survival.",
                               style="font-size:20px;
                  font-weight:bold;
                  margin-left: 500px;
                  margin-right: 500px;
                  text-align: center;
                  position:absolute;
                  color:white"),
                             div(img(src='background4.jpg'),style="background: url('background4.jpg');
			                                     background-repeat: repeat;
                                          background-size: auto;
                                          margin-left: -15px;
                                          margin-right: -15px;
                                          margin-top: 20px;
                                          margin-bottom: 20px;
                                          height: inherit;"),style="position:relative"),
                         #div(class = "intro-divider-sub"),
                         sidebarPanel(h4("Selection",style="color: #663399;font-size: 25px"),
                                      uiOutput("data_source_expression_sum"),
                                      uiOutput("treatment_expression_sum"),
                                      uiOutput("description_expression_sum"),
                                      selectizeInput("gene_expression_sum","Insert a gene set, for example: CTLA4 PDCD1",choices=NULL,multiple=TRUE),
                                      #                       textInput("gene_expression_sum",
                                      #                                 "Insert a gene, for example: PUM2,DPM1",
                                      #                                 "PUM2,DPM1"),
                                      actionButton("do_expression_sum","Go",style="background-color: lightblue")),
                         #                       uiOutput("slider_expression_sum")),
                         conditionalPanel(condition="input.do_expression_sum",
                                          mainPanel(tabsetPanel(
                                            tabPanel("Immunotherapy response",
                                                     #                                wellPanel (radioButtons("expression_sum_plots_result","Groups: ",
                                                     #                                                        c("Progression free survival status","Overall survival status"))),
                                                     wellPanel (div(id="container",
                                                                    h4("Number of selected samples: "), 
                                                                    div(textOutput("expression_sum_plots_n_samples_NA"),style="font-family: 'Lobster', cursive; color: #ad1d28; font-size: 20px"),
                                                                    style="margin-bottom:30px"),
                                                                uiOutput("expression_sum_plots_plots")),
                                                     wellPanel (div(id="container",
                                                                    h4("Data format:   "),
                                                                    radioButtons("expression_sum_plots_type","",
                                                                                 choices = c("Excel (CSV)","Text (TSV)", "JSON"),
                                                                                 inline = TRUE),
                                                                    style="margin-bottom:30px"),
                                                                div(downloadButton("expression_sum_plots_download", "Download"),style="margin-bottom:30px"),
                                                                div(DT::dataTableOutput("expression_sum_plots_table"),style="background-color: lightblue;overflow-y: scroll;overflow-x: scroll"))),#### ??????????????????,?????????????????????????????????
                                            tabPanel("Overall survival",
                                                     wellPanel(uiOutput("slider_expression_sum_OS")),
                                                     wellPanel(div(id="container",
                                                                   h4("Number of samples selected:"),
                                                                   div(textOutput("expression_sum_survival_n_samples_NA"),style="font-family: 'Lobster', cursive; color: #ad1d28; font-size: 20px"),
                                                                   style="margin-bottom:30px"),
                                                               uiOutput("expression_sum_survival_plots")),
                                                     wellPanel(div(id="container",
                                                                   h4("Data format:   "),
                                                                   radioButtons("expression_sum_survival_type","",
                                                                                choices = c("Excel (CSV)","Text (TSV)", "JSON"),
                                                                                inline = TRUE),
                                                                   style="margin-bottom:30px"),
                                                               div(downloadButton("expression_sum_survival_download", "Download"),style="margin-bottom:30px"),
                                                               div(DT::dataTableOutput("expression_sum_survival_table"),style="background-color: lightblue;overflow-y: scroll;overflow-x: scroll"))),#### ??????????????????,?????????????????????????????????
                                            tabPanel("Progression free survival",
                                                     wellPanel(uiOutput("slider_expression_sum_PFS")),
                                                     wellPanel(div(id="container",
                                                                   h4("Number of samples selected:"),
                                                                   div(textOutput("expression_sum_PFS_n_samples_NA"),style="font-family: 'Lobster', cursive; color: #ad1d28; font-size: 20px"),
                                                                   style="margin-bottom:30px"),
                                                               uiOutput("expression_sum_PFS_plots")),
                                                     wellPanel(div(id="container",
                                                                   h4("Data format:   "),
                                                                   radioButtons("expression_sum_PFS_type","",
                                                                                choices = c("Excel (CSV)","Text (TSV)", "JSON"),
                                                                                inline = TRUE),
                                                                   style="margin-bottom:30px"),
                                                               div(downloadButton("expression_sum_PFS_download", "Download"),style="margin-bottom:30px"),
                                                               div(DT::dataTableOutput("expression_sum_PFS_table"),style="background-color: lightblue;overflow-y: scroll;overflow-x: scroll")))#### ??????????????????,?????????????????????????????????
                                          )))),
               tabPanel ("Expression relation pairs",
                         h4("Gene expression relations",style="font-size:25px;
                        text-align: center;
                        margin-top: 20px;color: #663399"),
                         div(p("Exploring the association of gene expression relation pairs to immunotherapy response, overall survival and progression free survival. ",
                               style="font-size:20px;
                  font-weight:bold;
                  margin-left: 500px;
                  margin-right: 500px;
                  text-align: center;
                  position:absolute;
                  color:white"),
                             div(img(src='background4.jpg'),style="background: url('background4.jpg');
			                                     background-repeat: repeat;
                                          background-size: auto;
                                          margin-left: -15px;
                                          margin-right: -15px;
                                          margin-top: 20px;
                                          margin-bottom: 20px;
                                          height: inherit;"),style="position:relative"),
                         #div(class = "intro-divider-sub"),
                         sidebarPanel(h4("Selection",style="color: #663399;font-size: 25px"),
                                      uiOutput("data_source_expression_pairs"),
                                      uiOutput("treatment_expression_pairs"),
                                      uiOutput("description_expression_pairs"),
                                      actionButton("add_expression_pairs","Add gene pairs"),
                                      p(strong("Please click 'Add gene pairs' buttom to insert more gene pairs.")),
                                      p("For example:"),
                                      p("Gene pairs #4: CD28 CD86"),
                                      p("Gene pairs #5: CD27 PDCD1"),
                                      #                                  p("Gene pairs #3: CD86 TNFRSF4"),
                                      #                                  p("Gene pairs #4: CD86 CD200"),
                                      #                                  p("Gene pairs #5: CTLA4 TNFRSF4"),
                                      #                                  p("Gene pairs #6: PDCD1 TNFRSF4"),
                                      #                                  p("Gene pairs #7: CD80 TNFSF9"),
                                      #                                  p("Gene pairs #8: CD86 HAVCR2"),
                                      #                                  p("Gene pairs #9: CD28 CD86"),
                                      #                                  p("Gene pairs #10: CD27 PDCD1"),
                                      
                                      p("For more gene relation pairs which were proved related with immunotheray response, please refer to",a(href="https://www.nature.com/articles/s41591-018-0157-9/","Auslander, et al.")),
                                      p("Gene expression relations, are defined as a sum score for pairwise
                                                    comparison of gene expressions."),
                                      p(withMathJax("$$F=\\sum F_{i,j}, F_{i,j}=\\binom{1, exp\\left ( i \\right ) > exp\\left ( j \\right )}{0, otherwise}$$")),
                                      p("Where F is the sum score, F(i,j) is the score derived by comparing expression between gene 
                                      i and j. For example, if there are three gene pairs (PD-L1 and CD40, CD28 and CD276, CD86 and CD200),
                                      and expression of PD-L1 < CD40, CD28 > CD276, and CD86 > CD200, F would be 2 (0+1+1).  "),
                                      selectizeInput("gene_expression_pairs_1","Gene pairs #1",choices=NULL,multiple=TRUE,selected = NULL,options=list(maxItems=2)),
                                      selectizeInput("gene_expression_pairs_2","Gene pairs #2",choices=NULL,multiple=TRUE,selected = NULL,options=list(maxItems=2)),
                                      selectizeInput("gene_expression_pairs_3","Gene pairs #3",choices=NULL,multiple=TRUE,selected = NULL,options=list(maxItems=2)),
                                      uiOutput("gene_expression_pairs"),
                                      #                       textInput("gene_expression_pairs",
                                      #                                 "Insert gene pairs, for example: PUM2:DPM1",
                                      #                                 "PUM2:DPM1"),
                                      actionButton("do_expression_pairs","Go",style="background-color: lightblue")),
                         #                       uiOutput("slider_expression_sum")),
                         conditionalPanel(condition="input.do_expression_pairs",
                                          mainPanel(tabsetPanel(
                                            tabPanel("Immunotherapy response",
                                                     #                                wellPanel (radioButtons("expression_pairs_plots_result","Groups: ",
                                                     #                                                        c("Progression free survival status","Overall survival status"))),
                                                     wellPanel (div(id="container",
                                                                    h4("Number of selected samples: "), 
                                                                    div(textOutput("expression_pairs_plots_n_samples_NA"),style="font-family: 'Lobster', cursive; color: #ad1d28; font-size: 20px"),
                                                                    style="margin-bottom:30px"),
                                                                uiOutput("expression_pairs_plots_plots",height="auto")),
                                                     wellPanel (div(id="container",
                                                                    h4("Data format:   "),
                                                                    radioButtons("expression_pairs_plots_type","",
                                                                                 choices = c("Excel (CSV)","Text (TSV)", "JSON"),
                                                                                 inline = TRUE),
                                                                    style="margin-bottom:30px"),
                                                                div(downloadButton("expression_pairs_plots_download", "Download"),style="margin-bottom:30px"),
                                                                div(DT::dataTableOutput("expression_pairs_plots_table"),style="background-color: lightblue;overflow-y: scroll;overflow-x: scroll"))),#### ??????????????????,?????????????????????????????????
                                            tabPanel("Overall survival",
                                                     wellPanel(uiOutput("slider_expression_pairs_OS")),
                                                     wellPanel(div(id="container",
                                                                   h4("Number of samples selected:"),
                                                                   div(textOutput("expression_pairs_survival_n_samples_NA"),style="font-family: 'Lobster', cursive; color: #ad1d28; font-size: 20px"),
                                                                   style="margin-bottom:30px"),
                                                               uiOutput("expression_pairs_survival_plots",height = "auto")),
                                                     wellPanel(div(id="container",
                                                                   h4("Data format:   "),
                                                                   radioButtons("expression_pairs_survival_type","",
                                                                                choices = c("Excel (CSV)","Text (TSV)", "JSON"),
                                                                                inline = TRUE),
                                                                   style="margin-bottom:30px"),
                                                               div(downloadButton("expression_pairs_survival_download", "Download"),style="margin-bottom:30px"),
                                                               div(DT::dataTableOutput("expression_pairs_survival_table"),style="background-color: lightblue;overflow-y: scroll;overflow-x: scroll"))),#### ??????????????????,?????????????????????????????????
                                            tabPanel("Progression free survival",
                                                     wellPanel(uiOutput("slider_expression_pairs_PFS")),
                                                     wellPanel(div(id="container",
                                                                   h4("Number of samples selected:"),
                                                                   div(textOutput("expression_pairs_PFS_n_samples_NA"),style="font-family: 'Lobster', cursive; color: #ad1d28; font-size: 20px"),
                                                                   style="margin-bottom:30px"),
                                                               uiOutput("expression_pairs_PFS_plots",height = "auto")),
                                                     wellPanel(div(id="container",
                                                                   h4("Data format:   "),
                                                                   radioButtons("expression_pairs_PFS_type","",
                                                                                choices = c("Excel (CSV)","Text (TSV)", "JSON"),
                                                                                inline = TRUE),
                                                                   style="margin-bottom:30px"),
                                                               div(downloadButton("expression_pairs_PFS_download", "Download"),style="margin-bottom:30px"),
                                                               div(DT::dataTableOutput("expression_pairs_PFS_table"),style="background-color: lightblue;overflow-y: scroll;overflow-x: scroll")))#### ??????????????????,?????????????????????????????????
                                          )))),
               tabPanel ("Immune cell components",
                         h4("Immune cell components",style="font-size:25px;
		                                  text-align: center;
		                                  margin-top: 20px;color: #663399"),
                         div(p("Exploring the association of immune cell proportion to immunotherapy response, overall survival and progression free survival.",
                               style="font-size:20px;
                  font-weight:bold;
                  margin-left: 500px;
                  margin-right: 500px;
                  text-align: center;
                  position:absolute;
                  color:white"),
                             div(img(src='background4.jpg'),style="background: url('background4.jpg');
			                                     background-repeat: repeat;
                                          background-size: auto;
                                          margin-left: -15px;
                                          margin-right: -15px;
                                          margin-top: 20px;
                                          margin-bottom: 20px;
                                          height: inherit;"),style="position:relative"),
                         #div(class = "intro-divider-sub"),
                         sidebarPanel(h4("Selection",style="color: #663399;font-size:25px"),
                                      uiOutput("data_source_cibersort"),
                                      uiOutput("treatment_cibersort"),
                                      uiOutput("description_cibersort"),
                                      uiOutput("immucell_cibersort"),
                                      actionButton("do_cibersort","Go",style="background-color: lightblue")),
                         #		                       uiOutput("slider_cibersort")),
                         conditionalPanel(condition="input.do_cibersort",
                                          mainPanel(tabsetPanel(
                                            tabPanel("Immunotherapy response",
                                                     #                                wellPanel (radioButtons("cibersort_plots_result","Groups: ",
                                                     #                                                        c("Progression free survival status","Overall survival status"))),
                                                     wellPanel(div(id="container",
                                                                   h4("Number of selected samples: "),
                                                                   div(textOutput("cibersort_plots_n_samples_NA"),style="font-family: 'Lobster', cursive; color: #ad1d28; font-size: 20px"),
                                                                   style="margin-bottom:30px"),
                                                               uiOutput("cibersort_plots_plots",height="auto")),
                                                     wellPanel (div(id="container",
                                                                    h4("Data format:   "),
                                                                    radioButtons("cibersort_plots_type","",
                                                                                 choices = c("Excel (CSV)","Text (TSV)", "JSON"),
                                                                                 inline = TRUE),
                                                                    style="margin-bottom:30px"),
                                                                div(downloadButton("cibersort_plots_download", "Download"),style="margin-bottom:30px"),
                                                                div(DT::dataTableOutput("cibersort_plots_table"),style="background-color: lightblue;overflow-y: scroll;overflow-x: scroll"))),
                                            tabPanel("Overall survival",
                                                     wellPanel(uiOutput("slider_cibersort_OS")),
                                                     wellPanel(div(id="container",
                                                                   h4("Number of samples selected:"),
                                                                   div(textOutput("cibersort_survival_n_samples_NA"),style="font-family: 'Lobster', cursive; color: #ad1d28; font-size: 20px"),
                                                                   style="margin-bottom:30px"),
                                                               uiOutput("cibersort_survival_plots",height = "auto")),
                                                     wellPanel(div(id="container",
                                                                   h4("Data format:   "),
                                                                   radioButtons("cibersort_survival_type","",
                                                                                choices = c("Excel (CSV)","Text (TSV)", "JSON"),
                                                                                inline = TRUE),
                                                                   style="margin-bottom:30px"),
                                                               div(downloadButton("cibersort_survival_download", "Download"),style="margin-bottom:30px"),
                                                               div(DT::dataTableOutput("cibersort_survival_table"),style="background-color: lightblue;overflow-y: scroll;overflow-x: scroll"))),#### ??????????????????,?????????????????????????????????
                                            tabPanel("Progression free survival",
                                                     wellPanel(uiOutput("slider_cibersort_PFS")),
                                                     wellPanel(div(id="container",
                                                                   h4("Number of samples selected:"),
                                                                   div(textOutput("cibersort_PFS_n_samples_NA"),style="font-family: 'Lobster', cursive; color: #ad1d28; font-size: 20px"),
                                                                   style="margin-bottom:30px"),
                                                               uiOutput("cibersort_PFS_plots",height = "auto")),
                                                     wellPanel(div(id="container",
                                                                   h4("Data format:   "),
                                                                   radioButtons("cibersort_PFS_type","",
                                                                                choices = c("Excel (CSV)","Text (TSV)", "JSON"),
                                                                                inline = TRUE),
                                                                   style="margin-bottom:30px"),
                                                               div(downloadButton("cibersort_PFS_download", "Download"),style="margin-bottom:30px"),
                                                               div(DT::dataTableOutput("cibersort_PFS_table"),style="background-color: lightblue;overflow-y: scroll;overflow-x: scroll")))#### ??????????????????,?????????????????????????????????          
                                          ))))),
    tabPanel ("Integration",
              h4("Integrative analysis",style="font-size:25px;
		                                  text-align: center;
		                                  margin-top: 20px;color: #663399"),
              div(p("Exploring the association of any two features, one as the X variable and the other as the Y variable, to immunotherapy response, overall survival and progression free survival. ",
                    style="font-size:20px;
                  font-weight:bold;
                  margin-left: 500px;
                  margin-right: 500px;
                  text-align: center;
                  position:absolute;
                  color:white"),
                  div(img(src='background4.jpg'),style="background: url('background4.jpg');
			                                     background-repeat: repeat;
                                          background-size: auto;
                                          margin-left: -15px;
                                          margin-right: -15px;
                                          margin-top: 20px;
                                          margin-bottom: 20px;
                                          height: inherit;"),style="position:relative"),
              #div(class = "intro-divider-sub"),
              sidebarPanel(
                wellPanel(
                  h4("X variable",style="color: #663399;font-size: 25px"),
                  selectInput("xaxis_multi_dimensions",
                              "Features: ",
                              choices = c("expression","expression sum","expression relation pairs","immune cell components","mutation","mutational loads","mutational signatures"),
                              multiple = FALSE,
                              selected = "mutational loads"),
                  conditionalPanel(condition="input.xaxis_multi_dimensions!='expression relation pairs'",
                                   uiOutput("xaxis_gene_multi_dimensions")),
                  conditionalPanel(condition="input.xaxis_multi_dimensions=='expression relation pairs'",
                                   actionButton("xaxis_add_multi_dimensions","Add gene pairs"),
                                   uiOutput("xaxis_gene_multi_dimensions_x"))),
                wellPanel(
                  h4("Y variable",style="color: #663399;font-size: 25px"),
                  selectInput("yaxis_multi_dimensions",
                              "Features: ",
                              choices = c("expression","expression sum","expression relation pairs","immune cell components","mutation","mutational loads","mutational signatures"),
                              multiple = FALSE,
                              selected = "immune cell components"),
                  conditionalPanel(condition="input.yaxis_multi_dimensions!='expression relation pairs'",
                                   uiOutput("yaxis_gene_multi_dimensions")),
                  conditionalPanel(condition="input.yaxis_multi_dimensions=='expression relation pairs'",
                                   actionButton("yaxis_add_multi_dimensions","Add gene pairs"),
                                   uiOutput("yaxis_gene_multi_dimensions_y"))),
                wellPanel (
                  h4("Selection",style="color: #663399;font-size: 25px"),
                  uiOutput("data_source_multi_dimensions"),
                  uiOutput("treatment_multi_dimensions"),
                  uiOutput("description_multi_dimensions"),
                  actionButton("do_multi_dimensions","Go",style="background-color: lightblue"))
              ),
              conditionalPanel(condition="input.do_multi_dimensions",
                               mainPanel(tabsetPanel(
                                 tabPanel("Immunotherapy response",
                                          #                     wellPanel (radioButtons("multi_dimensions_plots_result","Groups: ",
                                          #                                             c("Progression free survival status","Overall survival status"))),
                                          wellPanel (uiOutput("slider_multi_dimensions_x"),
                                                     uiOutput("slider_multi_dimensions_y")),
                                          wellPanel (div(id="container",
                                                         h4("Number of selected samples: "), 
                                                         div(textOutput("multi_dimensions_plots_n_samples_NA"),style="font-family: 'Lobster', cursive; color: #ad1d28; font-size: 20px"),
                                                         style="margin-bottom:30px"),
                                                     uiOutput("multi_dimensions_plots_plots")),
                                          wellPanel (div(id="container",
                                                         h4("Data format:   "),
                                                         radioButtons("multi_dimensions_plots_type","",
                                                                      choices = c("Excel (CSV)","Text (TSV)", "JSON"),
                                                                      inline = TRUE),
                                                         style="margin-bottom:30px"),
                                                     div(downloadButton("multi_dimensions_plots_download", "Download"),style="margin-bottom:30px"),
                                                     div(DT::dataTableOutput("multi_dimensions_plots_table"),style="background-color: lightblue;overflow-y: scroll;overflow-x: scroll"))),
                                 tabPanel("Overall survival",
                                          wellPanel (uiOutput("slider_multi_dimensions_survival_x"),
                                                     uiOutput("slider_multi_dimensions_survival_y")),
                                          wellPanel (div(id="container",
                                                         h4("Number of selected samples: "), 
                                                         div(textOutput("multi_dimensions_survival_n_samples_NA"),style="font-family: 'Lobster', cursive; color: #ad1d28; font-size: 20px"),
                                                         style="margin-bottom:30px"),
                                                     uiOutput("multi_dimensions_survival_plots")),
                                          wellPanel (div(id="container",
                                                         h4("Data format:   "),
                                                         radioButtons("multi_dimensions_survival_type","",
                                                                      choices = c("Excel (CSV)","Text (TSV)", "JSON"),
                                                                      inline = TRUE),
                                                         style="margin-bottom:30px"),
                                                     div(downloadButton("multi_dimensions_survival_download", "Download"),style="margin-bottom:30px"),
                                                     div(DT::dataTableOutput("multi_dimensions_survival_table"),style="background-color: lightblue;overflow-y: scroll;overflow-x: scroll"))),
                                 tabPanel("Progression free survival",
                                          wellPanel (uiOutput("slider_multi_dimensions_PFS_x"),
                                                     uiOutput("slider_multi_dimensions_PFS_y")),
                                          wellPanel (div(id="container",
                                                         h4("Number of selected samples: "), 
                                                         div(textOutput("multi_dimensions_PFS_n_samples_NA"),style="font-family: 'Lobster', cursive; color: #ad1d28; font-size: 20px"),
                                                         style="margin-bottom:30px"),
                                                     uiOutput("multi_dimensions_PFS_plots")),
                                          wellPanel (div(id="container",
                                                         h4("Data format:   "),
                                                         radioButtons("multi_dimensions_PFS_type","",
                                                                      choices = c("Excel (CSV)","Text (TSV)", "JSON"),
                                                                      inline = TRUE),
                                                         style="margin-bottom:30px"),
                                                     div(downloadButton("multi_dimensions_PFS_download", "Download"),style="margin-bottom:30px"),
                                                     div(DT::dataTableOutput("multi_dimensions_PFS_table"),style="background-color: lightblue;overflow-y: scroll;overflow-x: scroll")))
                                 
                                 #### ??????????????????,?????????????????????????????????
                               )))),
    navbarMenu("Single-cell",
               tabPanel("Gene expression",
                        h4("Gene expression in single-cell",style="font-size:25px;
		                                  text-align: center;
		                                  margin-top: 20px;color: #663399"),
                        div(p("Exploring the association of gene expression signal to immunotherapy response.",
                              style="font-size:20px;
                  font-weight:bold;
                  margin-left: 500px;
                  margin-right: 500px;
                  text-align: center;
                  position:absolute;
                  color:white"),
                            div(img(src='background4.jpg'),style="background: url('background4.jpg');
			                                     background-repeat: repeat;
                                          background-size: auto;
                                          margin-left: -15px;
                                          margin-right: -15px;
                                          margin-top: 20px;
                                          margin-bottom: 20px;
                                          height: inherit;"),style="position:relative"),
                        #div(class = "intro-divider-sub"),
                        sidebarPanel(h4("Selection",style="color: #663399;font-size:25px"),
                                     uiOutput("data_source_single_cell_gene"),
                                     uiOutput("treatment_single_cell_gene"),
                                     uiOutput("description_single_cell_gene"),
                                     selectizeInput("gene_single_cell_gene","Insert a gene, for example: CD19",choices=NULL),
                                     actionButton("do_single_cell_gene","Go",style="background-color: lightblue")),
                        #		                       uiOutput("slider_cibersort")),
                        conditionalPanel(condition="input.do_single_cell_gene",
                                         mainPanel(tabsetPanel(
                                           tabPanel("Immunotherapy response",
                                                    ### violin
                                                    wellPanel(div(id="container",
                                                                  h4("Number of selected samples: "),
                                                                  div(textOutput("single_cell_gene_plots_n_samples_NA"),style="font-family: 'Lobster', cursive; color: #ad1d28; font-size: 20px"),
                                                                  style="margin-bottom:30px"),
                                                              div(id="container",
                                                                  h4("Number of selected cells: "),
                                                                  div(textOutput("single_cell_gene_plots_n_cells_NA"),style="font-family: 'Lobster', cursive; color: #ad1d28; font-size: 20px"),
                                                                  style="margin-bottom:30px"),
                                                              #                              uiOutput("single_cell_gene_plots_violin",height="auto"),
                                                              plotOutput("single_cell_gene_plots_violin"), #violin
                                                              div(id="container",
                                                                  h4("Highlight cells which have high "),
                                                                  div(textOutput("single_cell_gene_plots_tSNE_gene"),style="font-family: 'Lobster', cursive; color: #ad1d28; font-size: 20px"),
                                                                  h4 (" expression"),
                                                                  style="margin-bottom:30px;margin-top:60px"),
                                                              #                              uiOutput("single_cell_gene_plots_tSNE",height="auto")),
                                                              #                              plotlyOutput("single_cell_gene_plots_tSNE",height="auto")), #tSNE
                                                              plotOutput("single_cell_gene_plots_tSNE")), #tSNE
                                                    wellPanel (div(id="container",
                                                                   h4("Data format:   "),
                                                                   radioButtons("single_cell_gene_plots_tSNE_type","",
                                                                                choices = c("Excel (CSV)","Text (TSV)", "JSON"),
                                                                                inline = TRUE),
                                                                   style="margin-bottom:30px"),
                                                               div(downloadButton("single_cell_gene_plots_tSNE_download", "Download"),style="margin-bottom:30px"),
                                                               div(DT::dataTableOutput("single_cell_gene_plots_tSNE_table"),style="background-color: lightblue;overflow-y: scroll;overflow-x: scroll"))
                                           ))))),
               tabPanel("Cell populations",
                        h4("Cell populations",style="font-size:25px;
		                                  text-align: center;
		                                  margin-top: 20px;color: #663399"),
                        div(p("Exploring the association of cell population frequencies to immunotherapy response, overall survival and progression free survival.",
                              style="font-size:20px;
                  font-weight:bold;
                  margin-left: 500px;
                  margin-right: 500px;
                  text-align: center;
                  position:absolute;
                  color:white"),
                            div(img(src='background4.jpg'),style="background: url('background4.jpg');
			                                     background-repeat: repeat;
                                          background-size: auto;
                                          margin-left: -15px;
                                          margin-right: -15px;
                                          margin-top: 20px;
                                          margin-bottom: 20px;
                                          height: inherit;"),style="position:relative"),
                        #div(class = "intro-divider-sub"),
                        sidebarPanel(h4("Selection",style="color: #663399;font-size:25px"),
                                     uiOutput("data_source_single_cell_cell"),
                                     uiOutput("treatment_single_cell_cell"),
                                     uiOutput("description_single_cell_cell"),
                                     selectizeInput("cluster_single_cell_cell","Insert a cluster, for example: cluster 0",
                                                    multiple=TRUE, choices=NULL),
                                     actionButton("do_single_cell_cell","Go",style="background-color: lightblue")),
                        conditionalPanel(condition="input.do_single_cell_cell",
                                         mainPanel(tabsetPanel(
                                           tabPanel("Immunotherapy response",
                                                    ### boxplot
                                                    wellPanel(div(id="container",
                                                                  h4("Number of selected samples: "),
                                                                  div(textOutput("single_cell_cell_plots_n_samples_NA"),style="font-family: 'Lobster', cursive; color: #ad1d28; font-size: 20px"),
                                                                  style="margin-bottom:30px"),
                                                              div(id="container",
                                                                  h4("Number of selected cells: "),
                                                                  div(textOutput("single_cell_cell_plots_n_cells_NA"),style="font-family: 'Lobster', cursive; color: #ad1d28; font-size: 20px"),
                                                                  style="margin-bottom:30px"),
                                                              plotlyOutput("single_cell_cell_plots_boxplot",height="auto"), #boxplot
                                                              div(id="container",
                                                                  h4("Highlight cluster "),
                                                                  div(textOutput("single_cell_cell_plots_tSNE_cluster"),style="font-family: 'Lobster', cursive; color: #ad1d28; font-size: 20px"),
                                                                  style="margin-bottom:30px;margin-top:60px"),
                                                              plotOutput("single_cell_cell_plots_tSNE",height=500),#tSNE
                                                              div(id="container",
                                                                  h4("Top 10 marker genes of cluster "),
                                                                  div(textOutput("single_cell_cell_plots_heatmap_cluster"),style="font-family: 'Lobster', cursive; color: #ad1d28; font-size: 20px"),
                                                                  style="margin-bottom:30px;margin-top:60px"), 
                                                              plotOutput("single_cell_cell_plots_heatmap",height=500)),
                                                    wellPanel (div(id="container",
                                                                   h4("Cells information of selected cluster "),
                                                                   div(textOutput("single_cell_cell_plots_table_1"),style="font-family: 'Lobster', cursive; color: #ad1d28; font-size: 20px"),
                                                                   style="margin-bottom:30px"),
                                                               div(id="container",
                                                                   h4("Data format:   "),
                                                                   radioButtons("single_cell_cell_plots_tSNE_type","",
                                                                                choices = c("Excel (CSV)","Text (TSV)", "JSON"),
                                                                                inline = TRUE),
                                                                   style="margin-bottom:30px"),
                                                               div(downloadButton("single_cell_cell_plots_tSNE_download", "Download"),style="margin-bottom:30px"),
                                                               div(DT::dataTableOutput("single_cell_cell_plots_tSNE_table"),style="background-color: lightblue;overflow-y: scroll;overflow-x: scroll")),
                                                    wellPanel (div(id="container",
                                                                   h4("All marker genes of selected cluster "),
                                                                   div(textOutput("single_cell_cell_plots_table_2"),style="font-family: 'Lobster', cursive; color: #ad1d28; font-size: 20px"),
                                                                   style="margin-bottom:30px"),
                                                               div(id="container",
                                                                   h4("Data format:   "),
                                                                   radioButtons("single_cell_cell_plots_heatmap_type","",
                                                                                choices = c("Excel (CSV)","Text (TSV)", "JSON"),
                                                                                inline = TRUE),
                                                                   style="margin-bottom:30px"),
                                                               div(downloadButton("single_cell_cell_plots_heatmap_download", "Download"),style="margin-bottom:30px"),
                                                               div(DT::dataTableOutput("single_cell_cell_plots_heatmap_table"),style="background-color: lightblue;overflow-y: scroll;overflow-x: scroll"))),
                                           tabPanel("Overall survival",
                                                    wellPanel(uiOutput("slider_single_cell_cell_OS")),
                                                    wellPanel(div(id="container",
                                                                  h4("Number of samples selected:"),
                                                                  div(textOutput("single_cell_cell_survival_n_samples_NA"),style="font-family: 'Lobster', cursive; color: #ad1d28; font-size: 20px"),
                                                                  style="margin-bottom:30px"),
                                                              plotlyOutput("single_cell_cell_survival_plots",height="auto")),
                                                    wellPanel(div(id="container",
                                                                  h4("Data format:   "),
                                                                  radioButtons("single_cell_cell_survival_type","",
                                                                               choices = c("Excel (CSV)","Text (TSV)", "JSON"),
                                                                               inline = TRUE),
                                                                  style="margin-bottom:30px"),
                                                              div(downloadButton("single_cell_cell_survival_download", "Download"),style="margin-bottom:30px"),
                                                              div(DT::dataTableOutput("single_cell_cell_survival_table"),style="background-color: lightblue;overflow-y: scroll;overflow-x: scroll"))),
                                           tabPanel("Progression free survival",
                                                    wellPanel(uiOutput("slider_single_cell_cell_PFS")),
                                                    wellPanel(div(id="container",
                                                                  h4("Number of samples selected:"),
                                                                  div(textOutput("single_cell_cell_PFS_n_samples_NA"),style="font-family: 'Lobster', cursive; color: #ad1d28; font-size: 20px"),
                                                                  style="margin-bottom:30px"),
                                                              plotlyOutput("single_cell_cell_PFS_plots",height = "auto")),
                                                    wellPanel(div(id="container",
                                                                  h4("Data format:   "),
                                                                  radioButtons("single_cell_cell_PFS_type","",
                                                                               choices = c("Excel (CSV)","Text (TSV)", "JSON"),
                                                                               inline = TRUE),
                                                                  style="margin-bottom:30px"),
                                                              div(downloadButton("single_cell_cell_PFS_download", "Download"),style="margin-bottom:30px"),
                                                              div(DT::dataTableOutput("single_cell_cell_PFS_table"),style="background-color: lightblue;overflow-y: scroll;overflow-x: scroll")))
                                         ))))),
    tabPanel("Users' data",
             h4("Users' data",style="font-size:25px;
		                                  text-align: center;
		                                  margin-top: 20px;color: #663399"),
             div(p("This tab enables users to upload your own datasets, 
                   which are not restricted to melanoma but can be from any cancer types. 
                   The uploaded data can be analysed independently or co-analysed with existing datasets to 
                   identify or validate signatures of interest.",
                   style="font-size:20px;
                  font-weight:bold;
                  margin-left: 500px;
                  margin-right: 500px;
                  text-align: center;
                  position:absolute;
                  color:white"),
                 div(img(src='background4.jpg'),style="background: url('background4.jpg');
			                                     background-repeat: repeat;
                                          background-size: auto;
                                          margin-left: -15px;
                                          margin-right: -15px;
                                          margin-top: 20px;
                                          margin-bottom: 20px;
                                          height: inherit;"),style="position:relative"),
             #div(class = "intro-divider-sub"),
             
             fluidRow(column(4,h4("Upload clinical data",style="color: #663399;font-size: 20px"),
                             fileInput("file_clinical","Choose a file to upload:",
                                       accept = c(
                                         'text/csv',
                                         'text/comma-separated-values',
                                         'text/tab-separated-values',
                                         'text/plain',
                                         '.csv',
                                         '.tsv')),
                             #                         tags$hr(),
                             #                         radioButtons("header_clinical","Header",c(Yes=TRUE,No=FALSE),TRUE),
                             #                         radioButtons("sep_clinical","Separator",c(Comma=",",Semicolon=";",Tab="\t"),"\t"),
                             #                         actionButton("do_user_data_clinical","Combine",style="background-color: lightblue"),
                             tags$hr(),
                             p("Example file for clinical data: ",a(href="Example_clinical_data.txt","Example_clinical_data.txt")),
                             p("The clinical data must contain 10 columns which are 'PatientID','data_source','treatment','description','group','RECIST','dead','OS','PFS',and 'Age'"),
                             p("Please keep your column names same with above columns' names. If no data available please fill in NA."),
                             style="background-color: #DCDCDC;margin-right:20px"),
                      column (7,h4("Check for uploaded clinical data",style="color: #663399;font-size: 20px"),
                              radioButtons("display_clinical","Display",
                                           choices = c("Top 10 rows","All"),
                                           inline = TRUE),
                              div(tableOutput("user_clinical_table"),style="background-color: lightblue;overflow-y: scroll;overflow-x: scroll"),
                              style="background-color: #DCDCDC"),
                      style="margin-left:25px;margin-right:25px"
             ),
             div(class = "intro-divider-sub"), 
             fluidRow(column(4,h4("Upload transcriptome data",style="color: #663399;font-size: 20px"),
                             fileInput("file_transcriptome","Choose a file to upload:",
                                       accept = c(
                                         'text/csv',
                                         'text/comma-separated-values',
                                         'text/tab-separated-values',
                                         'text/plain',
                                         '.csv',
                                         '.tsv')),
                             #                            tags$hr(),
                             #                            radioButtons("header_transcriptome","Header",c(Yes="TRUE",No="FALSE"),"TRUE"),
                             #                            radioButtons("sep_transcriptome","Separator",c(Comma=",",Semicolon=";",Tab="\t"),"\t"),
                             #                            actionButton("do_user_data_transcriptome","Combine",style="background-color: lightblue"),
                             tags$hr(),
                             p("Example file for transcriptome data: ",a(href="Example_transcriptome_data.txt","Example_transcriptome_data.txt")),
                             p("The transcriptome data must contain m rows which denote samples and n columns which denote genes."),
                             style="background-color: #DCDCDC;margin-right:20px"),
                      column (7,h4("Check for uploaded transcriptome data",style="color: #663399;font-size: 20px"),
                              radioButtons("display_transcriptome","Display",
                                           choices = c("Top 10 rows and columns","All"),
                                           inline = TRUE),
                              div(tableOutput("user_transcriptome_table"),style="background-color: lightblue;overflow-y: scroll;overflow-x: scroll"),
                              style="background-color: #DCDCDC"),
                      style="margin-left:25px;margin-right:25px"
             ),
             div(class = "intro-divider-sub"),
             fluidRow(column(4,h4("Upload mutation data",style="color: #663399;font-size: 20px"),
                             fileInput("file_mutation","Choose a file to upload:",
                                       accept = c(
                                         'text/csv',
                                         'text/comma-separated-values',
                                         'text/tab-separated-values',
                                         'text/plain',
                                         '.csv',
                                         '.tsv')),
                             #                              tags$hr(),
                             #                              radioButtons("header_mutation","Header",c(Yes="TRUE",No="FALSE"),"TRUE"),
                             #                              radioButtons("sep_mutation","Separator",c(Comma=",",Semicolon=";",Tab="\t"),"\t"),
                             
                             tags$hr(),
                             p("Example file for mutation data: ",a(href="Example_mutation_data.txt","Example_mutation_data.txt")),
                             p("The mutation data must contain 7 columns which are 'PatientID','data_source','sample','gene','variant_class','Protein_Change',and 'Chromosome'"),
                             p("Please keep your column names same with above columns' names. If no data available please fill in NA."),
                             
                             style="background-color: #DCDCDC;margin-right:20px"),
                      column(7,h4("Check for uploaded mutation data",style="color: #663399;font-size: 20px"), 
                             radioButtons("display_mutation","Display",
                                          choices = c("Top 10 rows","All"),
                                          inline = TRUE),
                             div(tableOutput("user_mutation_table"),style="background-color: lightblue;overflow-y: scroll;overflow-x: scroll"),
                             style="background-color: #DCDCDC"),
                      style="margin-left:25px;margin-right:25px"
             ),
             div(class = "intro-divider-sub"),
             actionButton("do_user_data","Combine my data to Immu-Mela",style="background-color: lightblue; margin-left:25px"),
             div(progressBar(id = "pb2",value = 0,total = 100,title = "",display_pct = TRUE),style="margin-left:25px;margin-right:25px"),
             p(strong("This may take a while"),style="margin-left:25px;margin-right:25px;font-family: 'Segoe UI', Helvetica, Arial, sans-serif"),
             div(class = "intro-divider-sub")
    ),
    tabPanel("HELP",
             div(img(src='background3.jpg'),style="background: url('background3.jpg');
			                                     background-repeat: repeat;
                                          background-size: auto;
                                          margin-left: -15px;
                                          margin-right: -15px;
                                          margin-top: 20px;
                                          margin-bottom: 20px;
                                          height: inherit;"),
             #h4("Users' data",style="font-size:25px;
             #                              text-align: center;
             #                              margin-top: 20px"),
             # p("the lazy fox jumped over the quick brown dog. the lazy fox jumped over the quick brown dog. the lazy fox jumped over the quick brown dog.",
             #   style="font-size:20px;
             #         margin-left: 275px;
             #         margin-right: 275px;"),
             # div(class = "intro-divider-sub"),
             h4("Contact",style="margin-left:275px;color: #663399;font-size:25px"),
             p("Immu-Mela is a web-based portal to explore the relationships between multidimensional genomic data 
		       and immune checkpoint blockade (ICB) therapy response. It is freely available for non-commercial use. It is hosted and maintained by Center for
		       Quantitative Sciences at Vanderbilt University Medical Center, Nashville.", 
               style="font-size:20px;
                 margin-left: 300px;
                 margin-right: 275px;"),
             p("If you have questions, comments, or find any bugs with Immu-Mela, please contact",
               style="font-size:20px;
                 margin-left: 300px;
                 margin-right: 275px;"),
             div(id="container",
                 p("Dr. Qi Liu <",style=""),
                 p("qi.liu@vumc.org",style="color:blue"),
                 p(">",style=""),
                 style="font-size:20px;margin-left: 300px;margin-right: 275px;"),
             div(id="container",
                 p("Dr. Jing Yang <",style=""),
                 p("jing.yang@vumc.org",style="color:blue"),
                 p(">",style=""),
                 style="font-size:20px;margin-left: 300px;margin-right: 275px;"),
             h4("Citation",style="margin-left:275px;color: #663399;font-size:25px"),
             p("Immu-Mela portal: an open platform for exploring immunotherapy-related multidimensional genomic 
		      profiles in melanoma ",
               style="font-size:20px;
                 margin-left: 300px;
                 margin-right: 275px;"),
             h4("Tutorial",style="margin-left:275px;color: #663399;font-size:25px"),
             #		    strong("Introduction",style="margin-left:300px;color: black;font-size:20px"),
             p("The core functions of Immu-Mela are retrieval and visualization. 4-step user-friendly web interfaces 
		      are enough to make all the queried data available. First of all, pick an interested genomic profiling 
		      and select a feature accordingly. Then a new web page will be created, so that datasets, agents 
		      (anti-PD-1, anti-CTLA-4, or both) and treatment time 
		      (pre- or on-treatment) should be determined on this page. Before the end, a signature, 
		      for example mutations of genes, BRAF, NRAS and NF1, should be inserted. Finally submit the query. 
		      Based on the users' selection, Immu-Mela automatically generates three reports, each in a 
		      separate tab. First of all, for immunotherapy response, Immu-Mela summarizes samples statistics and creates a graph which 
		      represents difference of queried signature between responders and nonresponders in each dataset. 
		      The consistency of queried signature across selected datasets can be discovered easily. 
		      And a downloadable table includes clinical information with annotations is displayed. 
		      Other two reports, overall survival and progression survival, are exhibited in a respective tab.  
		      Immu-Mela allows users to assign cutoffs to separate the selected samples into two (high or low) 
		      groups through interactive sliders.  In the same way, the consistency 
		      of survival analyses can also be explored across datasets easily. To query at the Integrative level, 
               users first select any two features from genetic and/or transcriptomic profiling, 
               one as the X variable and the other as the Y variable. Immu-Mela will then split 
               samples into four groups by two cutoffs set by the interactive sliders, X-high/Y-high, 
               X-high/Y-low, X-low/Y-high and X-low/Y-low. Finally, Immu-Mela will investigate their 
               relationship with immunotherapy response, overall survival and progression-free survival. 
               Immu-Mela provides uploading function to analyse users' own datasets, which are unrestricted 
               to any cancer types. Users can upload clinical data, transcriptome and/or mutation data, 
               and then merge the data into the database by clicking the button 'Combine data to Immu-Mela'. 
               After the process is complete, the data will appear in the list of Data source as 'User', 
               which can be selected or co-selected with other existing datasets for the further analysis. ",
               style="font-size:20px;
                 margin-left: 300px;
                 margin-right: 275px;"),
             p("Step 1. Pick a feature",
               style="font-size:20px;
                 margin-left: 300px;
                 margin-right: 275px;"),
             p("Step 2. Fill out Selection",
               style="font-size:20px;
                 margin-left: 300px;
                 margin-right: 275px;"),
             p("Step 3. Click Go",
               style="font-size:20px;
                 margin-left: 300px;
                 margin-right: 275px;"),
             p("Step 4. Browse results",
               style="font-size:20px;
                 margin-left: 300px;
                 margin-right: 275px;"),
             div(img(src='help_fig1.jpg'),style="
			                                     text-align: center;
                                          margin-top: 20px;
                                          margin-bottom: 20px
                                          "),
             div(strong("Figure 1. Usage of Immu-Mela"),style="text-align: center;font-size:15px"),
             h4("Reference",style="margin-left:275px;color: #663399;font-size:25px"),
             div(id="container",
                 p("[",a(href="https://www.ncbi.nlm.nih.gov/pubmed/23945592","1"), "]", "Alexandrov, L.B., Nik-Zainal, S., Wedge, D.C., Aparicio, S.A., Behjati, S., Biankin, A.V., Bignell, 
              G.R., Bolli, N., Borg, A., Borresen-Dale, A.L. et al. (2013) 
              Signatures of mutational processes in human cancer.",style="font-size:20px;"),
                 strong("Nature,",style="font-size:20px;"), 
                 p("500, 415-421.",style="font-size:20px;"),
                 style="font-size:20px;
                 margin-left: 300px;
                 margin-right: 275px;"),
             div(id="container",
                 p("[",a(href="https://www.ncbi.nlm.nih.gov/pubmed/26899170","2"), "]", "Rosenthal, R., McGranahan, N., 
              Herrero, J., Taylor, B.S. and Swanton, C. (2016) DeconstructSigs: delineating mutational processes 
              in single tumors distinguishes DNA repair deficiencies and patterns of carcinoma evolution.",style="font-size:20px;"),
                 strong("Genome Biol,",style="font-size:20px;"), 
                 p("17, 31.",style="font-size:20px;"),
                 style="font-size:20px;
                 margin-left: 300px;
                 margin-right: 275px;"),
             div(id="container",
                 p("[",a(href="https://www.ncbi.nlm.nih.gov/pubmed/30127394","3"), "]", "Auslander, N., Zhang, 
              G., Lee, J.S., Frederick, D.T., Miao, B., Moll, T., Tian, T., Wei, Z., Madan, S., Sullivan, R.J. et al. 
              (2018) Robust prediction of response to immune checkpoint blockade therapy in metastatic melanoma.",style="font-size:20px;"),
                 strong("Nat Med,",style="font-size:20px;"), 
                 p("24, 1545-1549.",style="font-size:20px;"),
                 style="font-size:20px;
                 margin-left: 300px;
                 margin-right: 275px;"),
             div(id="container",
                 p("[",a(href="https://www.ncbi.nlm.nih.gov/pubmed/25822800","4"), "]", "Newman, A.M., Liu, C.L., 
              Green, M.R., Gentles, A.J., Feng, W., Xu, Y., Hoang, C.D., Diehn, M. and Alizadeh, A.A. (2015) 
              Robust enumeration of cell subsets from tissue expression profiles. ",style="font-size:20px;"),
                 strong("Nat Methods,",style="font-size:20px;"), 
                 p("12, 453-457.",style="font-size:20px;"),
                 style="font-size:20px;
                 margin-left: 300px;
                 margin-right: 275px;"),
             div(id="container",
                 p("[",a(href="https://www.ncbi.nlm.nih.gov/pubmed/29608179","5"), "]", "Butler, A., Hoffman, P.,
              Smibert, P., Papalexi, E. and Satija, R. (2018) Integrating single-cell transcriptomic data 
              across different conditions, technologies, and species. ",style="font-size:20px;"),
                 strong("Nat Biotechnol,",style="font-size:20px;"), 
                 p("36, 411-420.",style="font-size:20px;"),
                 style="font-size:20px;
                 margin-left: 300px;
                 margin-right: 275px;"),
             div(id="container",
                 p("[",a(href="https://www.ncbi.nlm.nih.gov/pubmed/27288499","6"), "]", "Skidmore, Z.L., Wagner, 
              A.H., Lesurf, R., Campbell, K.M., Kunisaki, J., Griffith, O.L. and Griffith, M. (2016) GenVisR: 
              Genomic Visualizations in R.",style="font-size:20px;"),
                 strong("Bioinformatics,",style="font-size:20px;"), 
                 p("32, 3012-3014.",style="font-size:20px;"),
                 style="font-size:20px;
                 margin-left: 300px;
                 margin-right: 275px;"),
             
             
             
             
             
             div(class = "topbar", style="height: 6px;
                                  background: transparent linear-gradient(to right, #ec77ab 0%, #7873f5 100%) repeat scroll 0% 0%;
                                  margin-left: -15px;
                                  margin-right: -15px;
                                  margin-bottom:20px"),
             p("@Vanderbilt University Medical Center",style="color:darkgrey;margin-down:-1px;text-align:center"),
             p("Center for Quantitative Sciences",style="color:darkgrey;margin-down:-1px;text-align:center"),
             p("497A Preston Research Building | Nashville, TN 37232 | 615-322-6618",style="color:darkgrey;margin-down:-1px;text-align:center"))
    
    
    

  )))  
))
