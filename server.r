"%ni%" <- Negate("%in%")
start.table <- readRDS("start.table.rds")
#load("pbmc.RData")

#input <- list(data_source_cibersort=c("Van Allen","Hugo"),treatment_cibersort=c("anti-CTLA-4","anti-PD-1","anti-PD-1+anti-CTLA-4"),description_cibersort=c("Pre","on"),immucell_cibersort="T.cells.CD8",slider_cibersort_OS=0.5)
#input <- list(data_source_expression=c("Auslander","Chen","Hugo","Prat","Riaz","Van Allen"),treatment_expression=c("anti-CTLA-4","anti-PD-1","anti-PD-1+anti-CTLA-4"),description_expression=c("Pre","on"),gene_expression="CTLA4",slider_expression_OS=0.5)
#input <- list(data_source_expression_sum=c("Van Allen","Prat"),treatment_expression_sum=c("anti-CTLA-4","anti-PD-1","anti-PD-1+anti-CTLA-4"),description_expression_sum=c("Pre","on"),gene_expression_sum=c("CTLA4","PDCD1"),slider_expression_sum_OS=0.5)
#input <- list(gene_expression_pairs_1=c("CD28","CD276"),gene_expression_pairs_2=c("PDCD1","TNFRSF4"),gene_expression_pairs_3=c("CTLA4","TNFRSF4"),data_source_expression_pairs=c("Hugo","Van Allen"),treatment_expression_pairs=c("anti-CTLA-4","anti-PD-1"),description_expression_pairs=c("Pre","On"),slider_expression_pairs_OS=1,slider_expression_pairs_PFS=1)

#input <- list(data_source_mutation_signature=c("Van Allen","Hugo"),treatment_mutation_signature=c("anti-CTLA-4","anti-PD-1","anti-PD-1+anti-CTLA-4"),description_mutation_signature=c("Pre","On"),signature_mutation_signature="Signature.12",slider_mutation_signature_PFS=0.5)
#input <- list(data_source_mutation="Snyder",treatment_mutation="anti-CTLA-4",description_mutation="Pre",gene_mutation="BRAF")
#input <- list(data_source_mutation_loads=c("Snyder","Van Allen"),treatment_mutation_loads=c("anti-PD-1","anti-CTLA-4","anti-PD-1+anti-CTLA-4"),description_mutation_loads=c("Pre","On"),slider_mutation_loads_OS=0.5,slider_mutation_loads_PFS=0.5)
#input <- list(mutation_plots_result="Overall survival status",data_source_mutation=c("Hugo","Van Allen","Riaz","Snyder"),treatment_mutation=c("anti-PD-1","anti-CTLA-4","anti-PD-1+anti-CTLA-4"),description_mutation=c("Pre","On"),gene_mutation=c("BRAF","KRAS","HRAS","NRAS","NF1"))
#input <- list(data_source_multi_dimensions="Robert",treatment_multi_dimensions=c("anti-CTLA-4","anti-PD-1"),description_multi_dimensions=c("Pre","On"), yaxis_multi_dimensions="mutation load",xaxis_multi_dimensions="mutation load",yaxis_gene_multi_dimensions="Mutation.Load",xaxis_gene_multi_dimensions="Mutation.Load")
#input <- list(data_source_multi_dimensions=c("Riaz","Van Allen"),treatment_multi_dimensions=c("anti-CTLA-4","anti-PD-1","anti-PD-1+anti-CTLA-4"),description_multi_dimensions=c("Pre","On"), xaxis_multi_dimensions="expression",yaxis_multi_dimensions="mutation",xaxis_gene_multi_dimensions="CD19",yaxis_gene_multi_dimensions="BRAF",slider_multi_dimensions_survival_x=0.5,slider_multi_dimensions_survival_y=0.5,do_multi_dimensions=TRUE)

#input <- list(data_source_single_cell_gene="Krieg_panel_3",treatment_single_cell_gene="anti-PD-1",description_single_cell_gene=c("Pre","On"),gene_single_cell_gene="CD19")
#input <- list(data_source_single_cell_cell="Moshe",treatment_single_cell_cell=c("anti-PD-1","anti-CTLA-4","anti-PD-1+anti-CTLA-4"),description_single_cell_cell=c("Pre","On"),cluster_single_cell_cell=4,slider_single_cell_cell_OS=0.5,slider_single_cell_cell_PFS=0.5)

#input <- list(file_clinical=list(datapath="H:/YJ/VUMC-Project/ImmuPortal/code/shiny example/www/Example_clinical_data.txt"),file_transcriptome=list(datapath="H:/YJ/VUMC-Project/ImmuPortal/code/shiny example/www/Example_transcriptome_data.txt"),file_mutation=list(datapath="H:/YJ/VUMC-Project/ImmuPortal/code/shiny example/www/Example_mutation_data.txt"),do_user_data=TRUE)
#input <- list(file_clinical=NULL,file_transcriptome=NULL,file_mutation=NULL,sep_clinical="\t",sep_transcriptome=",",sep_mutation="\t")
#sig_matrix<-"H:/YJ/VUMC-Project/ImmuPortal/code/shiny example/www/LM22.txt"
#mixture_file<-"H:/YJ/VUMC-Project/ImmuPortal/code/shiny example/www/Example_transcriptome_data.txt"


shinyServer(function(input, output, session) {
  ###############################
  #### Start
  ##############################
  
  output$pieplot1 <- renderPlotly({
    
    a <- data.frame(table(database.test.process()$data_source))
    colnames(a) <- c("data_source","number")
    a[,"data_source"] <- as.character(a[,"data_source"])
    a[a[,"data_source"] %in% c("Krieg_panel_1","Krieg_panel_2","Krieg_panel_3"),"data_source"] <- "Krieg_panel_1\nKrieg_panel_2\nKrieg_panel_3"
    a <- aggregate(.~data_source, data=a,sum)
    a <- a[order(a$number,decreasing = T),]
    p1 <- plot_ly(a,labels=~data_source,values=~number,type="pie",
                  textposition='inside',textinfo='label',
                  insidetextfont = list(color = '#FFFFFF',size=20,family="sans-serif"), hoverinfo = 'text',
                  text = ~paste('Sample number:', number),showlegend = FALSE) %>%
      layout(title="test",margin=list(l = 0,r = 0,b = 0,t = 0, pad = 0, autoexpand = TRUE),
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)) %>%
      config(displayModeBar = F)
    p1}) 
  output$pieplot2 <- renderPlotly({
    a <- data.frame(table(database.test.process()$treatment))
    colnames(a) <- c("treatment","number")
    a <- a[order(a$number,decreasing = T),]
    p1 <- plot_ly(a,labels=~treatment,values=~number,type="pie",
                  textposition='inside',textinfo='label',
                  insidetextfont = list(color = '#FFFFFF',size=20,family="sans-serif"), hoverinfo = 'text',
                  text = ~paste('Sample number:', number),showlegend = FALSE) %>%
      layout(margin=list(l = 0,r = 0,b = 0,t = 0, pad = 0, autoexpand = TRUE),
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)) %>%
      config(displayModeBar = F)
    p1}) 
  output$pieplot3 <- renderPlotly({
    a <- data.frame(table(database.test.process()$description))
    colnames(a) <- c("description","number")
    a <- a[order(a$number,decreasing = T),]
    p1 <- plot_ly(a,labels=~description,values=~number,type="pie",
                  textposition='inside',textinfo='label',
                  insidetextfont = list(color = '#FFFFFF',size=20,family="sans-serif"), hoverinfo = 'text',
                  text = ~paste('Sample number:', number),showlegend = FALSE) %>%
      layout(margin=list(l = 0,r = 0,b = 0,t = 0, pad = 0, autoexpand = TRUE),
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)) %>%
      config(displayModeBar = F)
    p1}) 
  output$pieplot4 <- renderPlotly({
    a <- database.test.process()
    a[is.na(a[,"PFS.status"]),"PFS.status"] <- "Unknown"
    a <- data.frame(table(a$PFS.status))
    colnames(a) <- c("response","number")
    a <- a[order(a$number,decreasing = T),]
    p1 <- plot_ly(a,labels=~response,values=~number,type="pie",
                  textposition='inside',textinfo='label',
                  insidetextfont = list(color = '#FFFFFF',size=20,family="sans-serif"), hoverinfo = 'text',
                  text = ~paste('Sample number:', number),showlegend = FALSE) %>%
      layout(margin=list(l = 0,r = 0,b = 0,t = 0, pad = 0, autoexpand = TRUE),
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)) %>%
      config(displayModeBar = F)
    p1}) 
  
  output$start_table <- DT::renderDataTable({
    start.table$datasource <- paste0("<a href='", start.table$reference, "' target='_blank'>", start.table$datasource, "</a>")
    colnames(start.table)[1] <- "data source"
    DT::datatable(start.table[,1:13],options = list(searching = FALSE,paging = FALSE,info=FALSE),rownames=FALSE,escape = FALSE)
    
  })
  ############################################
  #### Start Users' data
  ############################################
  user_clinical <- function () {
    if (is.null(input$file_clinical)) {
      return(NULL)
    } else {
      df_clinical <- fread(input$file_clinical$datapath,data.table = FALSE)
      df_clinical
    }
  }
  user_transcriptome <- function() {
    if (is.null(input$file_transcriptome)) {
      return(NULL)
    } else {
      df_transcriptome <- fread(input$file_transcriptome$datapath,data.table = FALSE)
      df_transcriptome
    }
  }
  user_mutation <- function () {
    if (is.null(input$file_mutation)) {
      return(NULL)
    } else {
      
      df_mutation <- fread(input$file_mutation$datapath,data.table = FALSE)
      df_mutation
    }
  }
  
  database.test.process <- reactive({
    try.input <- try(input)
    if (try.input=="try-error") {
      database.test
    } else {
      #    if (input$do_user_data!=0) {
      if (!is.null(input$do_user_data)) {
        if (!is.null(input$file_clinical) | !is.null(input$file_transcriptome) | !is.null(input$file_mutation)) {
          
          user_sample <- na.omit(unique(c(as.character(user_clinical()[,1]),as.character(user_transcriptome()[,1]),as.character(unique(user_mutation()[,1])))))
          cal_clinical <- eventReactive (input$do_user_data, {
            if (is.null(input$file_clinical)) {
              data.frame(PatientID=user_sample,data_source=NA,treatment=NA,description=NA,PFS.status=NA,RECIST=NA,
                         OS.status=NA,OS=NA,PFS=NA,Age=NA) 
            } else {
              user_clinical()
            }
          })
          cal_transcriptome <- eventReactive (input$do_user_data, {
            if (is.null(input$file_transcriptome)) {
              data.frame(PatientID=user_sample) 
            } else {
              user_transcriptome()
            }
          })
          cal_cibersort <- eventReactive (input$do_user_data, {
            if (is.null(input$file_transcriptome)) {
              data.frame(PatientID=user_sample,B.cells.naive=NA,B.cells.memory=NA,Plasma.cells=NA,T.cells.CD8=NA,
                         T.cells.CD4.naive=NA,T.cells.CD4.memory.resting=NA,T.cells.CD4.memory.activated=NA,
                         T.cells.follicular.helper=NA,T.cells.regulatory..Tregs.=NA,T.cells.gamma.delta=NA,NK.cells.resting=NA,
                         NK.cells.activated=NA,Monocytes=NA,Macrophages.M0=NA,Macrophages.M1=NA,Macrophages.M2=NA,
                         Dendritic.cells.resting=NA,Dendritic.cells.activated=NA,Mast.cells.resting=NA,Mast.cells.activated=NA,
                         Eosinophils=NA,Neutrophils=NA)
            } else {
              
              #sig_matrix<-"H:/YJ/VUMC-Project/ImmuPortal/code/shiny example/www/LM22.txt"
              sig_matrix<-readRDS("LM22.rds")
              a<-user_transcriptome()
              b<-t(a)
              colnames(b)<-b[1,]
              b<-b[-1,]
              c<-apply(b,2,as.numeric)
              na.test <-  function (x) {
                w <- apply(x, 2, function(x)all(is.na(x)))
                if (any(w)) {
                  x<-x[,-which(w)]
                  #    stop(paste("All NA in columns", paste(which(w), collapse=", ")))
                }
                x
              }
              d<-na.test(c)
              rownames(d)<-rownames(b)
              e<-CIBERSORT(sig_matrix, d, perm=0, QN=FALSE, absolute=FALSE, abs_method='sig.score')
              data.frame(PatientID=rownames(e),e[,1:22])
            }
          })
          cal_mutation_loads <- eventReactive (input$do_user_data, {
            if (is.null(input$file_mutation)) {
              data.frame(PatientID=user_sample,Mutation.Load=NA)
            } else {
              df_mutation <- user_mutation()
              df_mutation_loads <- as.data.frame(table(df_mutation[,1]))
              colnames(df_mutation_loads) <- c("PatientID","Mutation.Load")
              df_mutation_loads
            }
          })
          cal_mutation_signature <- eventReactive (input$do_user_data,{
            if (is.null(input$file_mutation)) {
              data.frame(PatientID=user_sample,Signature.1=NA,Signature.2=NA,Signature.3=NA,Signature.4=NA,Signature.5=NA,
                         Signature.6=NA,Signature.7=NA,Signature.8=NA,Signature.9=NA,Signature.10=NA,Signature.11=NA,
                         Signature.12=NA,Signature.13=NA,Signature.14=NA,Signature.15=NA,Signature.16=NA,Signature.17=NA,
                         Signature.18=NA,Signature.19=NA,Signature.20=NA,Signature.21=NA,Signature.22=NA,Signature.23=NA,
                         Signature.24=NA,Signature.25=NA,Signature.26=NA,Signature.27=NA,Signature.28=NA,Signature.29=NA,
                         Signature.30=NA,unknown=NA)
            } else {
              df_mutation <- user_mutation()
              library(deconstructSigs)
              if (length(grep("chr",df_mutation[,"Chromosome"]))==0) {
                df_mutation[,"Chromosome"] <- paste0("chr",df_mutation[,"Chromosome"]) 
              } 
              chr <- c("chr1","chr2","chr3","chr4","chr5","chr6","chr7","chr8","chr9",
                       "chr10","chr11","chr12","chr13","chr14","chr15","chr16","chr17","chr18",
                       "chr19","chr20","chr21","chr22","chrX","chrY")
              df_mutation <- df_mutation[df_mutation[,"Chromosome"] %in% chr,]
              df_mutation.sigs.input <- mut.to.sigs.input(mut.ref = df_mutation,
                                                          sample.id = "PatientID", 
                                                          chr = "Chromosome", 
                                                          pos = "pos", 
                                                          ref = "ref", 
                                                          alt = "alt")
              df_mutation.signature <- NA
              for (i in rownames(df_mutation.sigs.input)){
                df_mutation.sample.single = whichSignatures(tumor.ref = df_mutation.sigs.input, 
                                                            signatures.ref = signatures.cosmic, 
                                                            sample.id = i, 
                                                            contexts.needed = TRUE,
                                                            tri.counts.method = 'default')
                df_mutation.signature <- rbind(df_mutation.signature,cbind(df_mutation.sample.single$weight,unknown=df_mutation.sample.single$unknown))
              }
              df_mutation.signature <- df_mutation.signature[-1,]
              df_mutation.signature
            }
          })
          user_combine <- function() {
            a<-merge(cal_clinical(),cal_transcriptome(),by="PatientID",all=T)
            b<-merge(cal_cibersort(),cal_mutation_loads(),by="PatientID",all=T)
            c<-merge(a,b,by="PatientID",all=T)
            d<-merge(c,cal_mutation_signature(),by.x="PatientID",by.y="row.names",all=T)
            d<-d[!is.na(d[,"PatientID"]) & !is.na(d[,"data_source"]),]
          }
          library(plyr)
          data.test.adduser <- rbind.fill(database.test,user_combine())
          data.test.adduser <- data.test.adduser[!is.na(data.test.adduser[,"PatientID"]) & !is.na(data.test.adduser[,"data_source"]),]
        } else {
          database.test
        }
      } else {
        database.test
      } 
    }
  })
  all.mutations.process <- reactive({
    if (!is.null(input$file_clinical) | !is.null(input$file_transcriptome) | !is.null(input$file_mutation)) {
      if (!is.null(user_mutation())) {
        a <- user_mutation()
        colnames(a)[1] <- "sample"
        b<-data.frame(a[,1:5],PatientID=a[,1],data_source="User")
        rbind.fill(all.mutations,b)
      } else {
        all.mutations
      }
    } else {
      all.mutations
    }
  })
  
  
  all.cols <- isolate(database.test.process())
  a.colnames<-colnames(all.cols)[11:ncol(all.cols)]
  
  ###########################################  
  #### mutation
  ###########################################
  output$data_source_mutation <- renderUI({
    all.data.source <- as.character(unique(database.test.process()[,"data_source"]))
    user.data.source <- all.data.source[!(all.data.source %in% c("Robert","Snyder","Moshe","Auslander","Chen","Hugo","Prat","Riaz","Van Allen","Krieg_panel_1","Krieg_panel_2","Krieg_panel_3"))]
    selectInput("data_source_mutation",
                "Data source: ",
                choices = c("Hugo","Riaz","Snyder","Van Allen",user.data.source),
                multiple = TRUE,
                selected = c("Hugo","Van Allen"))})
  output$treatment_mutation <- renderUI ({
    selectInput("treatment_mutation",
                "Treatment: ",
                choices = as.character(unique(database.test.process()[,"treatment"])),
                multiple = TRUE,
                selected = c("anti-CTLA-4","anti-PD-1","anti-PD-1+anti-CTLA-4"))})
  output$description_mutation <- renderUI ({
    selectInput("description_mutation",
                "Pre- or On-treatment: ",
                choices = as.character(unique(database.test.process()[,"description"])),
                multiple = TRUE,
                selected = c("Pre","On"))})
  updateSelectizeInput(session,"gene_mutation",
                       choices=a.colnames,selected=c("BRAF","KRAS","HRAS","NRAS","NF1"),
                       server=TRUE)#})
  
  selecteddatabase_mutation <- eventReactive(input$do_mutation,{
    data_source1 <- input$data_source_mutation
    treatment1 <- input$treatment_mutation
    description1 <- input$description_mutation
    genes1 <- c("PatientID","data_source","treatment","description","PFS.status","RECIST","OS.status","OS","PFS","Age")
    a<-data.table(database.test.process())
    m<-a[data_source %in% data_source1 & treatment %in% treatment1 & description %in% description1]
    m<-m[,colnames(m) %in% genes1,with=FALSE]
    as.data.frame(m)
  })
  ############
  #### mutation plot
  ############
  observeEvent(input$do_mutation,{
    all.mutations <- all.mutations.process()
    
  output$mutation_plots_n_samples_NA <- renderText({
    input$do_mutation ##keep these two rows
    isolate({ ##keep these two rows
      nrow(selecteddatabase_mutation())})})
  
  output$mutation_plots_plots <- renderUI({
    input$do_mutation
    isolate({
      input_gene_mutation <- input$gene_mutation
      selected.database <- selecteddatabase_mutation()
      
      selected.mutations <- all.mutations[all.mutations[,"PatientID"] %in% selected.database[,"PatientID"],]
      selected.mutations.1 <- selected.mutations[selected.mutations[,"gene"] %in% input_gene_mutation,]
      if (nrow(selected.database)==0 | nrow(selected.mutations.1)==0) {
        tagList(plotlyOutput("mutation_plots_plots0",height="auto"))
      } else {
        if (length(input$data_source_mutation)==1) {
          tagList(plotlyOutput("mutation_plots_plots1",height="auto"),tags$h1(" "))
        } else if (length(input$data_source_mutation)==2) {
          tagList(plotlyOutput("mutation_plots_plots1",height="auto"),tags$h1(" "),
                  plotlyOutput("mutation_plots_plots2",height="auto"),tags$h1(" "))
        } else if (length(input$data_source_mutation)==3) {
          tagList(plotlyOutput("mutation_plots_plots1",height="auto"),tags$h1(" "),
                  plotlyOutput("mutation_plots_plots2",height="auto"),tags$h1(" "),
                  plotlyOutput("mutation_plots_plots3",height="auto"),tags$h1(" "))
        } else if (length(input$data_source_mutation)==4) {
          tagList(plotlyOutput("mutation_plots_plots1",height="auto"),tags$h1(" "),
                  plotlyOutput("mutation_plots_plots2",height="auto"),tags$h1(" "),
                  plotlyOutput("mutation_plots_plots3",height="auto"),tags$h1(" "),
                  plotlyOutput("mutation_plots_plots4",height="auto"),tags$h1(" "))
        } else if (length(input$data_source_mutation)==5) {
          tagList(plotlyOutput("mutation_plots_plots1",height="auto"),tags$h1(" "),
                  plotlyOutput("mutation_plots_plots2",height="auto"),tags$h1(" "),
                  plotlyOutput("mutation_plots_plots3",height="auto"),tags$h1(" "),
                  plotlyOutput("mutation_plots_plots4",height="auto"),tags$h1(" "),
                  plotlyOutput("mutation_plots_plots5",height="auto"),tags$h1(" "))
        }
      }
    })
  })
  observeEvent(input$do_mutation,{
    input$do_mutation
    isolate({
      input_gene_mutation <- input$gene_mutation
      selected.database <- selecteddatabase_mutation()
      selected.mutations <- all.mutations[all.mutations[,"PatientID"] %in% selected.database[,"PatientID"],]
      selected.mutations.1 <- selected.mutations[selected.mutations[,"gene"] %in% input_gene_mutation,]
      if (nrow(selected.database)==0 | nrow(selected.mutations.1)==0) {
        output[["mutation_plots_plots0"]] <- renderPlotly({
          plot_ly () %>% 
            layout(title = paste(input$data_source_mutation,collapse = ","),font=list(size=15,family="Arial")) %>%
            add_annotations(x=2,y=2,text="no data available under this selection",showarrow=F,font=list(size=16,color="purple")) 
        })
      } else {
        selected.database <- data.frame(selected.database,Groups=selected.database$PFS.status)
        splitted_list <- selected.database %>% as.matrix %>% as.data.frame %>% split(.$data_source)
        splitted_list <- splitted_list[input$data_source_mutation]
        #      splitted_list_mutation <- selected.mutations.1 %>% as.matrix %>% as.data.frame %>% split(.$data_source)
        splitted_list_mutation <- selected.mutations %>% as.matrix %>% as.data.frame %>% split(.$data_source)
        splitted_list_mutation <- splitted_list_mutation[input$data_source_mutation]
        for (k in 1:length(input$data_source_mutation)) {
          local({
            my_i <- k
            mutation_plots_plots_name <- paste0("mutation_plots_plots", my_i)
            d<-splitted_list[[my_i]]
            d.mutations <- splitted_list_mutation[[my_i]]
            output[[mutation_plots_plots_name]] <- renderPlotly({
              
#              isolate({
                if (is.na(names(splitted_list)[my_i]) | is.na(names(splitted_list_mutation)[my_i])) {
                  plot_ly () %>% 
                    layout(title = input$data_source_mutation[[my_i]],font=list(size=15,family="Arial")) %>%
                    add_annotations(x=2,y=2,text="no data available under this selection",showarrow=F,font=list(size=16,color="purple")) 
                } else {
                  if (nrow(d[!is.na(d[,"PFS.status"]),])==0 | nrow(d.mutations)==0) {
                    plot_ly () %>% 
                      layout(title = unique(d$data_source),font=list(size=15,family="Arial")) %>%
                      add_annotations(x=2,y=2,text="no immunotherapy response under this selection",showarrow=F,font=list(size=16,color="purple")) 
                  } else {
                    withProgress(message = 'Making plot', value = 0, {
                    d <- d[!is.na(d[,"PFS.status"]),]
                    d<-data.frame(d,IND="no mutation")
                    d[,"IND"]<-as.character(d[,"IND"])
                    for (j in 1:length(input_gene_mutation)) {
                      d[d[,"PatientID"] %in% d.mutations[d.mutations[,"gene"]==input_gene_mutation[j],"PatientID"],"IND"]<- paste0("mutation in ",input_gene_mutation[j])
                    }
                    try.test <- try(p.v <- fisher.test(unlist(table(d[,c("PFS.status","IND")])))$p.value)
                    if (class(try.test)=="try-error") {
                      p.v <- "NA"
                    } else {
                      p.v <- fisher.test(unlist(table(d[,c("PFS.status","IND")])))$p.value
                      if (p.v < 0.001) {
                        p.v <- "< 0.001"
                      } else {
                        p.v<-round(p.v,3)
                      } 
                    }
                    ind <- sort(unique(d[,"Groups"]))
                    d.mutations.res <- d.mutations[d.mutations[,"PatientID"] %in% d[(d[,"Groups"]==ind[2] & !is.na(d[,"Groups"])),"PatientID"] ,]
                    d.mutations.nonres <- d.mutations[d.mutations[,"PatientID"] %in% d[(d[,"Groups"]==ind[1] & !is.na(d[,"Groups"])),"PatientID"] ,]
                    
                    d.mutations.res<-d.mutations.res[,-1]
                    d.mutations.res<-d.mutations.res[,c(5,1:4,6)]
                    colnames(d.mutations.res)[1] <- "sample"
                    if (nrow(d.mutations.res[d.mutations.res[,"gene"] %in% input_gene_mutation,])==0) {
                      p3 <- plot_ly () %>% 
                        add_annotations(x=2,y=2,text="no mutations under this selection",showarrow=F,font=list(size=16,color="purple")) 
                    } else {
                      p1<-#ggplotly(
                        waterfall(d.mutations.res, fileType = "Custom",
                                  variant_class_order=sort(as.character(unique(all.mutations[,3]))), ### here all.mutation is important, if not, different data source will have different color enve in the same mutation type
                                  plotGenes=unlist(str_split(input_gene_mutation,",")),
                                  mainXlabel = TRUE,my_x_label=ind[2],
                                  plotMutBurden = FALSE,returnp="p1",
                                  mainPalette=c('#4f00A8', '#A80100', '#CF5A59', '#A80079', '#BC2D94',
                                                '#CF59AE', '#000000', '#006666', '#00A8A8', '#009933',
                                                '#ace7b9', '#cdf0d5', '#59CF74', '#002AA8', '#5977CF',
                                                '#F37812', '#F2B079', '#888811', '#FDF31C', '#8C8C8C',
                                                '#CD5C5C', '#E9967A', '#FF0000', '#FF7F50', '#FFD700',
                                                '#FF8C00', '#FFFFE0', '#FFEFD5', '#FFDAB9', '#EEE8AA',
                                                '#7CFC00', '#228B22', '#ADFF2F', '#8FBC8F', '#2E8B57',
                                                '#00FFFF', '#AFEEEE', '#00CED1', '#008080', '#B0C4DE'))#,
                      #height =  480 )
                      p2<-#ggplotly(
                        waterfall(d.mutations.res, fileType = "Custom",
                                  variant_class_order=sort(as.character(unique(all.mutations[,3]))),
                                  plotGenes=unlist(str_split(input_gene_mutation,",")),
                                  mainXlabel = TRUE,my_x_label=ind[2],
                                  plotMutBurden = FALSE,returnp="p2",
                                  mainPalette=c('#4f00A8', '#A80100', '#CF5A59', '#A80079', '#BC2D94',
                                                '#CF59AE', '#000000', '#006666', '#00A8A8', '#009933',
                                                '#ace7b9', '#cdf0d5', '#59CF74', '#002AA8', '#5977CF',
                                                '#F37812', '#F2B079', '#888811', '#FDF31C', '#8C8C8C',
                                                '#CD5C5C', '#E9967A', '#FF0000', '#FF7F50', '#FFD700',
                                                '#FF8C00', '#FFFFE0', '#FFEFD5', '#FFDAB9', '#EEE8AA',
                                                '#7CFC00', '#228B22', '#ADFF2F', '#8FBC8F', '#2E8B57',
                                                '#00FFFF', '#AFEEEE', '#00CED1', '#008080', '#B0C4DE'))#,
                      #height =  480 )
                      #                 tooltip = c("x","y","text"),legendgroup=~variant_class)
                      p3 <- subplot(style(p2,showlegend=T),style(p1,showlegend=T),margin=0.03,widths = c(1/5,4/5),titleX=TRUE,titleY=TRUE)
                    }
                    d.mutations.nonres<-d.mutations.nonres[,-1]
                    d.mutations.nonres<-d.mutations.nonres[,c(5,1:4,6)]
                    colnames(d.mutations.nonres)[1] <- "sample"
                    if (nrow(d.mutations.nonres[d.mutations.nonres[,"gene"] %in% input_gene_mutation,])==0) {
                      q3 <- plot_ly () %>% 
                        add_annotations(x=2,y=2,text="no mutations under this selection",showarrow=F,font=list(size=16,color="purple")) 
                    } else {
                      q1<-#ggplotly(
                        waterfall(d.mutations.nonres, fileType = "Custom",
                                  variant_class_order=sort(as.character(unique(all.mutations[,3]))),
                                  plotGenes=unlist(str_split(input_gene_mutation,",")),
                                  mainXlabel = TRUE,my_x_label=ind[1],
                                  plotMutBurden = FALSE,returnp="p1",
                                  mainPalette=c('#4f00A8', '#A80100', '#CF5A59', '#A80079', '#BC2D94',
                                                '#CF59AE', '#000000', '#006666', '#00A8A8', '#009933',
                                                '#ace7b9', '#cdf0d5', '#59CF74', '#002AA8', '#5977CF',
                                                '#F37812', '#F2B079', '#888811', '#FDF31C', '#8C8C8C',
                                                '#CD5C5C', '#E9967A', '#FF0000', '#FF7F50', '#FFD700',
                                                '#FF8C00', '#FFFFE0', '#FFEFD5', '#FFDAB9', '#EEE8AA',
                                                '#7CFC00', '#228B22', '#ADFF2F', '#8FBC8F', '#2E8B57',
                                                '#00FFFF', '#AFEEEE', '#00CED1', '#008080', '#B0C4DE'))#,
                      #height =  480 )
                      q2<-#ggplotly(
                        waterfall(d.mutations.nonres, fileType = "Custom",
                                  variant_class_order=sort(as.character(unique(all.mutations[,3]))),
                                  plotGenes=unlist(str_split(input_gene_mutation,",")),
                                  mainXlabel = TRUE,my_x_label=ind[1],
                                  plotMutBurden = FALSE,returnp="p2",
                                  mainPalette=c('#4f00A8', '#A80100', '#CF5A59', '#A80079', '#BC2D94',
                                                '#CF59AE', '#000000', '#006666', '#00A8A8', '#009933',
                                                '#ace7b9', '#cdf0d5', '#59CF74', '#002AA8', '#5977CF',
                                                '#F37812', '#F2B079', '#888811', '#FDF31C', '#8C8C8C',
                                                '#CD5C5C', '#E9967A', '#FF0000', '#FF7F50', '#FFD700',
                                                '#FF8C00', '#FFFFE0', '#FFEFD5', '#FFDAB9', '#EEE8AA',
                                                '#7CFC00', '#228B22', '#ADFF2F', '#8FBC8F', '#2E8B57',
                                                '#00FFFF', '#AFEEEE', '#00CED1', '#008080', '#B0C4DE'))#,
                      #height =  480 )
                      #                     tooltip = c("x","y","text"),legendgroup=~variant_class)
                      q3 <- subplot(style(q2,showlegend=F),style(q1,showlegend=F),margin=0.03,widths = c(1/5,4/5),titleX=TRUE,titleY=TRUE)
                    }
                    if (nrow(d.mutations.res)==0 | nrow(d.mutations.nonres)==0) {
                      lable_x <- NULL
                      side_x <- "bottom"
                    } else {
                      lable_x <- paste0("p-value = ",p.v)
                      side_x <- "top"                
                    }
                    incProgress(100, detail = "Doing part %")
                    subplot(style(p3,showlegend=F),style(q3,showlegend=F),nrows=2,margin=0.1,titleX=TRUE,titleY=TRUE) %>% 
                      layout(title = unique(d$data_source), 
                             titlefont=list(size=20,color="black",family= "Arial"),
                             xaxis=list(title=lable_x,side=side_x))
                  })
                    } 
                  
                }
#              })
            })
          })
        }
      }
    })
  })
  
  
  # output$mutation_plots_plots_bar <- renderPlotly({
  #   selected.database <- mutation_survival_download_data()
  #   selected.database.plot <- as.data.frame(unlist(table(selected.database[,c("PFS.status","IND")])))
  #   colnames(selected.database.plot)[2] <- "Frequency"
  #   try.test <- try(p.v <- fisher.test(unlist(table(selected.database[,c("PFS.status","IND")])))$p.value)
  #   if (class(try.test)=="try-error") {
  #     p.v <- "NA"
  #   } else {
  #     p.v <- fisher.test(unlist(table(selected.database[,c("PFS.status","IND")])))$p.value
  #     if (p.v < 0.001) {
  #       p.v <- "< 0.001"
  #     } else {
  #       p.v<-round(p.v,3)
  #     } 
  #   }
  #   
  #   ggplotly(ggplot(selected.database.plot, aes(fill=Frequency, y=Freq, x=PFS.status)) + ggtitle (input$data_source_mutation) + 
  #              geom_bar( stat="identity", position="fill") + ylab("Mutation frequency") + xlab("Immunotherapy response") + 
  #              annotate("text",x=1.5,y=1.2,label=paste0("Fisher's exact test p-value: ",p.v),size=4))
  # })
  
  output$mutation_plots_download <- downloadHandler(
    filename = function() {
      if (input$mutation_plots_type == "Excel (CSV)") {
        "newfile.csv"
      } else if (input$mutation_plots_type == "Text (TSV)") {
        "newfile.txt"
      } else if (input$mutation_plots_type == "JSON") {
        "newfile.json"
      }
    },
    content = function(file) {
      if (input$mutation_plots_type == "Excel (CSV)") {
        write.csv (mutation_plots_download_data(),file,row.names = FALSE)
      } else if (input$mutation_plots_type == "Text (TSV)") {
        write.table (mutation_plots_download_data(),file,quote=F, row.names = FALSE, sep="\t")
      } else if (input$mutation_plots_type == "JSON") {
        write(toJSON(mutation_plots_download_data()),file)
      }
    }
  )
  mutation_plots_download_data <- function() {
    input_gene_mutation <- input$gene_mutation
    selected.database<-selecteddatabase_mutation()
    if (nrow(selected.database)==0) {
      selected.database
    } else {
      selected.mutations <- all.mutations[all.mutations[,"PatientID"] %in% selected.database[,"PatientID"],]
      #    gene1 <- unlist(str_split(input_gene_mutation,","))
      gene1 <- input_gene_mutation
      selected.mutations.1 <- selected.mutations[selected.mutations[,"gene"] %in% gene1,] ## samples carried mutation
      a <- unique(selected.mutations[,c(6,7)])
      selected.mutations.2 <- a[!(a[,1] %in% unique(selected.mutations.1[,6])),] ## samples no mutation
      #    selected.mutations.2 <- unique(selected.mutations.2[,c(6,7)])
      selected.mutations.2 <- data.frame(gene=paste0("no mutation in ",paste(input_gene_mutation,collapse = ",")),variant_class="no mutation",Protein_Change="no mutation",Chromosome="no mutation",selected.mutations.2)
      selected.mutations.res <- merge(selected.database,rbind(selected.mutations.1[,-1],selected.mutations.2),by="PatientID")
      selected.mutations.res <- selected.mutations.res[,-ncol(selected.mutations.res)]
      colnames(selected.mutations.res)[2] <- "data_source"
      #    selected.mutations.1 <- selected.mutations.1[,c(6,7,2,3,4,5)]
      b<- selected.database[!(selected.database[,"PatientID"] %in% selected.mutations.res[,"PatientID"]),] ## samples no genetic data
      if (nrow(b)!=0) {
        b<- data.frame(b,gene=NA,variant_class=NA,Protein_Change=NA,Chromosome=NA)
      }
      selected.mutations.res<-rbind(selected.mutations.res,b)
      selected.mutations.res[,c(1:7,10:ncol(selected.mutations.res))]
      unique(selected.mutations.res)
    }
  }
  output$mutation_plots_table <- DT::renderDataTable({
    input$do_mutation
    isolate({
      mutation_plots_download_data()
    })
  })     
  #########
  #### mutation survival
  #########
  output$mutation_survival_n_samples_NA <- renderText({
    input$do_mutation
    isolate({
      nrow(selecteddatabase_mutation())
    })
  })
  output$mutation_survival_plots <- renderUI({
    input$do_mutation
    isolate({
      input_gene_mutation <- input$gene_mutation
      selected.database <- selecteddatabase_mutation()
      selected.mutations <- all.mutations[all.mutations[,"PatientID"] %in% selected.database[,"PatientID"],]
      selected.mutations.1 <- selected.mutations[selected.mutations[,"gene"] %in% input_gene_mutation,]
      if (nrow(selected.database)==0 | nrow(selected.mutations.1)==0) {
        tagList(plotlyOutput("mutation_survival_plots0",height="auto"))
      } else {
        if (length(input$data_source_mutation)==1) {
          tagList(plotlyOutput("mutation_survival_plots1",height="auto"),tags$h1(" "))
        } else if (length(input$data_source_mutation)==2) {
          tagList(plotlyOutput("mutation_survival_plots1",height="auto"),tags$h1(" "),
                  plotlyOutput("mutation_survival_plots2",height="auto"),tags$h1(" "))
        } else if (length(input$data_source_mutation)==3) {
          tagList(plotlyOutput("mutation_survival_plots1",height="auto"),tags$h1(" "),
                  plotlyOutput("mutation_survival_plots2",height="auto"),tags$h1(" "),
                  plotlyOutput("mutation_survival_plots3",height="auto"),tags$h1(" "))
        } else if (length(input$data_source_mutation)==4) {
          tagList(plotlyOutput("mutation_survival_plots1",height="auto"),tags$h1(" "),
                  plotlyOutput("mutation_survival_plots2",height="auto"),tags$h1(" "),
                  plotlyOutput("mutation_survival_plots3",height="auto"),tags$h1(" "),
                  plotlyOutput("mutation_survival_plots4",height="auto"),tags$h1(" "))
        } else if (length(input$data_source_mutation)==5) {
          tagList(plotlyOutput("mutation_survival_plots1",height="auto"),tags$h1(" "),
                  plotlyOutput("mutation_survival_plots2",height="auto"),tags$h1(" "),
                  plotlyOutput("mutation_survival_plots3",height="auto"),tags$h1(" "),
                  plotlyOutput("mutation_survival_plots4",height="auto"),tags$h1(" "),
                  plotlyOutput("mutation_survival_plots5",height="auto"),tags$h1(" "))
        }
      }
    })
  })
  observeEvent(input$do_mutation,{
    input$do_mutation
    isolate({
      input_gene_mutation <- input$gene_mutation
      selected.database <- selecteddatabase_mutation()
      selected.mutations <- all.mutations[all.mutations[,"PatientID"] %in% selected.database[,"PatientID"],]
      selected.mutations.1 <- selected.mutations[selected.mutations[,"gene"] %in% input_gene_mutation,]
      if (nrow(selected.database)==0 | nrow(selected.mutations.1)==0) {
        output[["mutation_survival_plots0"]] <- renderPlotly({
          plot_ly () %>% 
            layout(title = paste(input$data_source_mutation,collapse = ","),font=list(size=15,family="Arial")) %>%
            add_annotations(x=2,y=2,text="no data available under this selection",showarrow=F,font=list(size=16,color="purple")) 
        })
      } else {
        selected.database <- data.frame(selected.database,Groups=selected.database$PFS.status)
        splitted_list <- selected.database %>% as.matrix %>% as.data.frame %>% split(.$data_source)
        splitted_list <- splitted_list[input$data_source_mutation]
        splitted_list_mutation <- selected.mutations.1 %>% as.matrix %>% as.data.frame %>% split(.$data_source)
        splitted_list_mutation <- splitted_list_mutation[input$data_source_mutation]
        for (k in 1:length(input$data_source_mutation)) {
          local({
            my_i <- k
            mutation_survival_plots_name <- paste0("mutation_survival_plots", my_i)
            d<-splitted_list[[my_i]]
            d.mutations <- splitted_list_mutation[[my_i]]
            output[[mutation_survival_plots_name]] <- renderPlotly({
              
#              isolate({
                if (is.na(names(splitted_list)[my_i]) | is.na(names(splitted_list_mutation)[my_i])) {
                  plot_ly () %>% 
                    layout(title = input$data_source_mutation[[my_i]],font=list(size=15,family="Arial")) %>%
                    add_annotations(x=2,y=2,text="no data available under this selection",showarrow=F,font=list(size=16,color="purple")) 
                } else {
                  if (nrow(d[!is.na(d[,"OS.status"]) & !is.na(d[,"OS"]),])==0 | nrow(d.mutations)==0) {
                    plot_ly () %>% 
                      layout(title = unique(d$data_source),font=list(size=15,family="Arial")) %>%
                      add_annotations(x=2,y=2,text="no immunotherapy response under this selection",showarrow=F,font=list(size=16,color="purple")) 
                  } else {
                    d <- d[!is.na(d[,"OS.status"]) & !is.na(d[,"OS"]),]
                    d<-data.frame(d,dead=NA)
                    d[d[,"OS.status"]=="living" & !is.na(d[,"OS.status"]),"dead"] <- 0
                    d[d[,"OS.status"]=="deceased" & !is.na(d[,"OS.status"]),"dead"] <- 1
                    d <- d[d[,"dead"]==1 | d[,"dead"]==0,]
                    
                    d<-data.frame(d[,1:6],unfactor(d[,c("dead","OS","PFS","Age")]))
                    
                    d<-data.frame(d,IND=paste0("samples without mutation (", 
                                               nrow(d[!(d[,"PatientID"] %in% d.mutations[,"PatientID"]),]),")"))
                    d[,"IND"]<-as.character(d[,"IND"])
                    d[d[,"PatientID"] %in% d.mutations[,"PatientID"],"IND"]<- paste0("samples with mutation (", 
                                                                                     nrow(d[d[,"PatientID"] %in% d.mutations[,"PatientID"],]),")")
                    d<-d[order(d[,"IND"]),]
                    
                    fit.result <- survfit(Surv(OS,dead)~IND,data=d)
                    plot.result <- ggsurv(fit.result,surv.col = c("lightgreen","plum"))
                    diff.result <- survdiff(Surv(OS,dead)~IND,data=d)
                    p.value= 1 - pchisq(diff.result$chisq, length(diff.result$n) - 1)
                    plot.result.ann <- plot.result + theme(legend.position = "right", legend.title = element_blank()) + 
                      ggplot2::geom_text(aes(label = sprintf("p-value = %0.2g",
                                                             pchisq(diff.result$chisq,df=1,lower.tail = F)),x=200,y=0.05))
                    ggplotly(plot.result.ann,legendgroup=~IND,height = 480 ) %>%
                      layout(title=unique(d$data_source),titlefont=list(size=20,color="black",family= "Arial"),
                             xaxis=list(title="Days",titlefont=list(size=20,color="black",family= "Aril")),
                             yaxis=list(title="Overall survival proportion",titlefont=list(size=20,color="black",family= "Aril")))
                    
                  } 
                  
                }
#              })
            })
          })
        }
      }
    })
  }) 
  
  output$mutation_survival_download <- downloadHandler(
    filename = function() {
      if (input$mutation_survival_type == "Excel (CSV)") {
        "newfile.csv"
      } else if (input$mutation_survival_type == "Text (TSV)") {
        "newfile.txt"
      } else if (input$mutation_survival_type == "JSON") {
        "newfile.json"
      }
    },
    content = function(file) {
      if (input$mutation_survival_type == "Excel (CSV)") {
        write.csv (mutation_survival_download_data(),file,row.names = FALSE)
      } else if (input$mutation_survival_type == "Text (TSV)") {
        write.table (mutation_survival_download_data(),file,quote=F, row.names = FALSE, sep="\t")
      } else if (input$mutation_survival_type == "JSON") {
        write(toJSON(mutation_survival_download_data()),file)
      }
    }
  )
  mutation_survival_download_data <- function() {
    input_gene_mutation <- input$gene_mutation
    selected.database<-selecteddatabase_mutation()
    if (nrow(selected.database)==0) {
      selected.database
    } else {
      selected.mutations <- all.mutations[all.mutations[,"PatientID"] %in% selected.database[,"PatientID"],]
      #    gene1 <- unlist(str_split(input$gene_mutation,","))
      gene1 <- input_gene_mutation
      selected.mutations.1 <- selected.mutations[selected.mutations[,"gene"] %in% gene1,]
      d<-selected.database
      d<-data.frame(d,IND="no mutation")
      d[,"IND"]<-as.character(d[,"IND"])
      for (i in 1:length(gene1)) {
        d[d[,"PatientID"] %in% selected.mutations.1[selected.mutations.1[,"gene"]==gene1[i],"PatientID"],"IND"]<- paste0("mutation in ",gene1[i])
      }
      d[,c(1:8,10:11)] 
    }
  }
  output$mutation_survival_table <- DT::renderDataTable({
    input$do_mutation
    isolate({
      mutation_survival_download_data()
    })
  })
  ##############
  #### mutation PFS
  ##############
  output$mutation_PFS_n_samples_NA <- renderText({
    input$do_mutation
    isolate({
      nrow(selecteddatabase_mutation())
    })
  })
  
  output$mutation_PFS_plots <- renderUI({
    input$do_mutation
    isolate({
      input_gene_mutation <- input$gene_mutation
      selected.database <- selecteddatabase_mutation()
      selected.mutations <- all.mutations[all.mutations[,"PatientID"] %in% selected.database[,"PatientID"],]
      selected.mutations.1 <- selected.mutations[selected.mutations[,"gene"] %in% input_gene_mutation,]
      if (nrow(selected.database)==0 | nrow(selected.mutations.1)==0) {
        tagList(plotlyOutput("mutation_PFS_plots0",height="auto"))
      } else {
        if (length(input$data_source_mutation)==1) {
          tagList(plotlyOutput("mutation_PFS_plots1",height="auto"),tags$h1(" "))
        } else if (length(input$data_source_mutation)==2) {
          tagList(plotlyOutput("mutation_PFS_plots1",height="auto"),tags$h1(" "),
                  plotlyOutput("mutation_PFS_plots2",height="auto"),tags$h1(" "))
        } else if (length(input$data_source_mutation)==3) {
          tagList(plotlyOutput("mutation_PFS_plots1",height="auto"),tags$h1(" "),
                  plotlyOutput("mutation_PFS_plots2",height="auto"),tags$h1(" "),
                  plotlyOutput("mutation_PFS_plots3",height="auto"),tags$h1(" "))
        } else if (length(input$data_source_mutation)==4) {
          tagList(plotlyOutput("mutation_PFS_plots1",height="auto"),tags$h1(" "),
                  plotlyOutput("mutation_PFS_plots2",height="auto"),tags$h1(" "),
                  plotlyOutput("mutation_PFS_plots3",height="auto"),tags$h1(" "),
                  plotlyOutput("mutation_PFS_plots4",height="auto"),tags$h1(" "))
        } else if (length(input$data_source_mutation)==5) {
          tagList(plotlyOutput("mutation_PFS_plots1",height="auto"),tags$h1(" "),
                  plotlyOutput("mutation_PFS_plots2",height="auto"),tags$h1(" "),
                  plotlyOutput("mutation_PFS_plots3",height="auto"),tags$h1(" "),
                  plotlyOutput("mutation_PFS_plots4",height="auto"),tags$h1(" "),
                  plotlyOutput("mutation_PFS_plots5",height="auto"),tags$h1(" "))
        }
      }
    })
  })
  observeEvent(input$do_mutation,{
    input$do_mutation
    isolate({
      input_gene_mutation <- input$gene_mutation
      selected.database <- selecteddatabase_mutation()
      selected.mutations <- all.mutations[all.mutations[,"PatientID"] %in% selected.database[,"PatientID"],]
      selected.mutations.1 <- selected.mutations[selected.mutations[,"gene"] %in% input_gene_mutation,]
      if (nrow(selected.database)==0 | nrow(selected.mutations.1)==0) {
        output[["mutation_PFS_plots0"]] <- renderPlotly({
          plot_ly () %>% 
            layout(title = paste(input$data_source_mutation,collapse = ","),font=list(size=15,family="Arial")) %>%
            add_annotations(x=2,y=2,text="no data available under this selection",showarrow=F,font=list(size=16,color="purple")) 
        })
      } else {
        selected.database <- data.frame(selected.database,Groups=selected.database$PFS.status)
        splitted_list <- selected.database %>% as.matrix %>% as.data.frame %>% split(.$data_source)
        splitted_list <- splitted_list[input$data_source_mutation]
        splitted_list_mutation <- selected.mutations.1 %>% as.matrix %>% as.data.frame %>% split(.$data_source)
        splitted_list_mutation <- splitted_list_mutation[input$data_source_mutation]
        for (k in 1:length(input$data_source_mutation)) {
          local({
            my_i <- k
            mutation_PFS_plots_name <- paste0("mutation_PFS_plots", my_i)
            d<-splitted_list[[my_i]]
            d.mutations <- splitted_list_mutation[[my_i]]
            output[[mutation_PFS_plots_name]] <- renderPlotly({
              
#              isolate({
                if (is.na(names(splitted_list)[my_i]) | is.na(names(splitted_list_mutation)[my_i])) {
                  plot_ly () %>% 
                    layout(title = input$data_source_mutation[[my_i]],font=list(size=15,family="Arial")) %>%
                    add_annotations(x=2,y=2,text="no data available under this selection",showarrow=F,font=list(size=16,color="purple")) 
                } else {
                  if (nrow(d[!is.na(d[,"PFS.status"]) & !is.na(d[,"PFS"]),])==0 | nrow(d.mutations)==0) {
                    plot_ly () %>% 
                      layout(title = unique(d$data_source),font=list(size=15,family="Arial")) %>%
                      add_annotations(x=2,y=2,text="no immunotherapy response under this selection",showarrow=F,font=list(size=16,color="purple")) 
                  } else {
                    d <- d[!is.na(d[,"PFS.status"]) & !is.na(d[,"PFS"]),]
                    d<-data.frame(d[,1:6],unfactor(d[,c("OS.status","OS","PFS","Age")]))
                    
                    d<-data.frame(d,IND=paste0("samples without mutation (", 
                                               nrow(d[!(d[,"PatientID"] %in% d.mutations[,"PatientID"]),]),")"))
                    d[,"IND"]<-as.character(d[,"IND"])
                    d[d[,"PatientID"] %in% d.mutations[,"PatientID"],"IND"]<- paste0("samples with mutation (", 
                                                                                     nrow(d[d[,"PatientID"] %in% d.mutations[,"PatientID"],]),")")
                    d <- data.frame(d,group.IND=0)
                    d[d[,"PFS.status"]=="nonresponse","group.IND"] <- 1
                    d<-d[order(d[,"IND"]),]
                    
                    fit.result <- survfit(Surv(PFS,group.IND)~IND,data=d)
                    plot.result <- ggsurv(fit.result,surv.col = c("lightgreen","plum"))
                    diff.result <- survdiff(Surv(PFS,group.IND)~IND,data=d)
                    p.value= 1 - pchisq(diff.result$chisq, length(diff.result$n) - 1)
                    plot.result.ann <- plot.result + theme(legend.position = "right", legend.title = element_blank()) + 
                      ggplot2::geom_text(aes(label = sprintf("p-value = %0.2g",
                                                             pchisq(diff.result$chisq,df=1,lower.tail = F)),x=200,y=0.05))
                    ggplotly(plot.result.ann,legendgroup=~IND,height = 480 ) %>%
                      layout(title=unique(d$data_source),titlefont=list(size=20,color="black",family= "Arial"),
                             xaxis=list(title="Days",titlefont=list(size=20,color="black",family= "Aril")),
                             yaxis=list(title="Preogression free survival proportion",titlefont=list(size=20,color="black",family= "Aril")))
                    
                  } 
                  
                }
#              })
            })
          })
        }
      }
    })
  }) 
  
  
  
  
  output$mutation_PFS_download <- downloadHandler(
    filename = function() {
      if (input$mutation_PFS_type == "Excel (CSV)") {
        "newfile.csv" 
      } else if (input$mutation_PFS_type == "Text (TSV)") {
        "newfile.txt"
      } else if (input$mutation_PFS_type == "JSON") {
        "newfile.json"
      }
    },
    content = function(file) {
      if (input$mutation_PFS_type == "Excel (CSV)") {
        write.csv (mutation_PFS_download_data(),file,row.names = FALSE)
      } else if (input$mutation_PFS_type == "Text (TSV)") {
        write.table (mutation_PFS_download_data(),file,quote=F, row.names = FALSE, sep="\t")
      } else if (input$mutation_PFS_type == "JSON") {
        write(toJSON(mutation_PFS_download_data()),file)
      }
    }
  )
  mutation_PFS_download_data <- function(){
    selected.database<-selecteddatabase_mutation()
    input_gene_mutation <- input$gene_mutation
#    selected.database<-selecteddatabase_mutation()
    if (nrow(selected.database)==0) {
      selected.database
    } else {
      selected.mutations <- all.mutations[all.mutations[,"PatientID"] %in% selected.database[,"PatientID"],]
      #    gene1 <- unlist(str_split(input$gene_mutation,","))
      gene1 <- input_gene_mutation
      selected.mutations.1 <- selected.mutations[selected.mutations[,"gene"] %in% gene1,]
      d<-selected.database
      d<-data.frame(d,IND="no mutation")
      d[,"IND"]<-as.character(d[,"IND"])
      for (i in 1:length(gene1)) {
        d[d[,"PatientID"] %in% selected.mutations.1[selected.mutations.1[,"gene"]==gene1[i],"PatientID"],"IND"]<- paste0("mutation in ",gene1[i])
      }
      d <- data.frame(d,group.IND=0)
      d[d[,"PFS.status"]=="nonresponse","group.IND"] <- 1
      
      d[,c(1:6,9:12)]
    }
#    })
#    })
  }
  output$mutation_PFS_table <- DT::renderDataTable({
    input$do_mutation
    isolate({
      mutation_PFS_download_data()
    })
  })
  })
  
  ###########################################
  #### mutation_loads
  ###########################################
  output$data_source_mutation_loads <- renderUI({
    all.data.source <- as.character(unique(database.test.process()[,"data_source"]))
    user.data.source <- all.data.source[!(all.data.source %in% c("Robert","Snyder","Moshe","Auslander","Chen","Hugo","Prat","Riaz","Van Allen","Krieg_panel_1","Krieg_panel_2","Krieg_panel_3"))]
    selectInput("data_source_mutation_loads",
                "Data source: ",
                choices = c("Hugo","Riaz","Snyder","Van Allen","Robert",user.data.source),
                multiple = TRUE,
                selected = c("Van Allen","Snyder"))})
  output$treatment_mutation_loads <- renderUI ({
    selectInput("treatment_mutation_loads",
                "Treatment: ",
                #                choices = as.character(unique(filter_input_interactive(filter_input=input$data_source_mutation_loads)[,"treatment"])),
                choices = as.character(unique(database.test.process()[,"treatment"])),
                multiple = TRUE,
                selected = "anti-CTLA-4")})
  output$description_mutation_loads <- renderUI ({
    selectInput("description_mutation_loads",
                "Pre- or On-treatment: ",
                #                choices = as.character(unique(filter_input_interactive(filter_input=c(input$data_source_mutation_loads,input$treatment_mutation_loads))[,"description"])),
                choices = as.character(unique(database.test.process()[,"description"])),
                multiple = TRUE,
                selected = c("Pre","On"))})
  output$slider_mutation_loads_OS <- renderUI({
    sliderInput("slider_mutation_loads_OS",
                "Seperate samples with gene mutation loads cutoff (%): ",
                min=0,max=1,value=0.5)  })
  output$slider_mutation_loads_PFS <- renderUI({
    sliderInput("slider_mutation_loads_PFS",
                "Seperate samples with gene mutation loads cutoff (%): ",
                min=0,max=1,value=0.5)  })
  
  selecteddatabase_mutation_loads <- eventReactive(input$do_mutation_loads,{
    data_source1 <- input$data_source_mutation_loads

    treatment1 <- input$treatment_mutation_loads
    description1 <- input$description_mutation_loads
    genes1 <- c("PatientID","data_source","treatment","description","PFS.status","RECIST","OS.status","OS","PFS","Age","Mutation.Load")
    a<-data.table(database.test.process())
    m<-a[data_source %in% data_source1 & treatment %in% treatment1 & description %in% description1]
    m<-m[,colnames(m) %in% genes1,with=FALSE]
    m<-m[,Mutation.Load:=as.numeric(Mutation.Load)]
    as.data.frame(m)
  })
  ##########
  #### mutation loads plots
  ##########
  output$mutation_loads_plots_n_samples_NA <- renderText({
    input$do_mutation_loads
    isolate({
      nrow(selecteddatabase_mutation_loads())
    })
  })
  
  output$mutation_loads_plots_plots <- renderUI({
    input$do_mutation_loads
    isolate({
      selected.database <- selecteddatabase_mutation_loads()
      if (nrow(selected.database)==0 | nrow(selected.database[!is.na(selected.database[,11]),])==0) {
        tagList(plotlyOutput("mutation_loads_plots_plots0",height="auto"))
      } else {
        if (length(input$data_source_mutation_loads)==1) {
          tagList(plotlyOutput("mutation_loads_plots_plots1",height="auto"),tags$h1(" "))
        } else if (length(input$data_source_mutation_loads)==2) {
          tagList(plotlyOutput("mutation_loads_plots_plots1",height="auto"),tags$h1(" "),
                  plotlyOutput("mutation_loads_plots_plots2",height="auto"),tags$h1(" "))
        } else if (length(input$data_source_mutation_loads)==3) {
          tagList(plotlyOutput("mutation_loads_plots_plots1",height="auto"),tags$h1(" "),
                  plotlyOutput("mutation_loads_plots_plots2",height="auto"),tags$h1(" "),
                  plotlyOutput("mutation_loads_plots_plots3",height="auto"),tags$h1(" "))
        } else if (length(input$data_source_mutation_loads)==4) {
          tagList(plotlyOutput("mutation_loads_plots_plots1",height="auto"),tags$h1(" "),
                  plotlyOutput("mutation_loads_plots_plots2",height="auto"),tags$h1(" "),
                  plotlyOutput("mutation_loads_plots_plots3",height="auto"),tags$h1(" "),
                  plotlyOutput("mutation_loads_plots_plots4",height="auto"),tags$h1(" "))
        } else if (length(input$data_source_mutation_loads)==5) {
          tagList(plotlyOutput("mutation_loads_plots_plots1",height="auto"),tags$h1(" "),
                  plotlyOutput("mutation_loads_plots_plots2",height="auto"),tags$h1(" "),
                  plotlyOutput("mutation_loads_plots_plots3",height="auto"),tags$h1(" "),
                  plotlyOutput("mutation_loads_plots_plots4",height="auto"),tags$h1(" "),
                  plotlyOutput("mutation_loads_plots_plots5",height="auto"),tags$h1(" "))
        } else if (length(input$data_source_mutation_loads)==6) {
          tagList(plotlyOutput("mutation_loads_plots_plots1",height="auto"),tags$h1(" "),
                  plotlyOutput("mutation_loads_plots_plots2",height="auto"),tags$h1(" "),
                  plotlyOutput("mutation_loads_plots_plots3",height="auto"),tags$h1(" "),
                  plotlyOutput("mutation_loads_plots_plots4",height="auto"),tags$h1(" "),
                  plotlyOutput("mutation_loads_plots_plots5",height="auto"),tags$h1(" "),
                  plotlyOutput("mutation_loads_plots_plots6",height="auto"),tags$h1(" "))
        } 
      }
    })
  })
  
  observeEvent(input$do_mutation_loads,{
    input$do_mutation_loads
    isolate({
      selected.database <- selecteddatabase_mutation_loads()
      if (nrow(selected.database)==0 | nrow(selected.database[!is.na(selected.database[,11]),])==0) {
        output[["mutation_loads_plots_plots0"]] <- renderPlotly({
          plot_ly () %>% 
            layout(title = paste(input$data_source_mutation_loads,collapse = ","),font=list(size=15,family="Arial")) %>%
            add_annotations(x=2,y=2,text="no data available under this selection",showarrow=F,font=list(size=16,color="purple")) 
        })
      } else {
        selected.database <- data.frame(selected.database,Groups=selected.database$PFS.status)
        splitted_list <- selected.database %>% as.matrix %>% as.data.frame %>% split(.$data_source)
        splitted_list <- splitted_list[input$data_source_mutation_loads]
        #      if (!is.null(length(splitted_list))) {
        for (k in 1:length(input$data_source_mutation_loads)) {
          local({
            my_i <- k
            mutation_loads_plots_plots_name <- paste0("mutation_loads_plots_plots", my_i)
            
            #          if (nrow(d)==0| nrow(d[!is.na(d[,11]) & !is.na(d[,5]),])==0) {
            #            output[[mutation_loads_plots_plots_name]] <- renderPlotly({
            #              plot_ly () %>% add_annotations(x=2,y=2,text="no data available",showarrow=F,font=list(size=16,color="purple")) 
            #            })
            #          } else {
            d<-splitted_list[[my_i]]
            output[[mutation_loads_plots_plots_name]] <- renderPlotly({
#              isolate({
              if (is.na(names(splitted_list)[my_i])) {
                plot_ly () %>% 
                  layout(title = input$data_source_mutation_loads[[my_i]],font=list(size=15,family="Arial")) %>%
                  add_annotations(x=2,y=2,text="no data available under this selection",showarrow=F,font=list(size=16,color="purple")) 
              } else {
                #d <- splitted_list[[my_i]]
                if (nrow(d[!is.na(d[,"PFS.status"]),])==0) {
                  plot_ly () %>% 
                    layout(title = unique(d$data_source),font=list(size=15,family="Arial")) %>%
                    add_annotations(x=2,y=2,text="no immunotherapy response under this selection",showarrow=F,font=list(size=16,color="purple")) 
                } else {
                  #d<-splitted_list[[my_i]]
                  d[,"Groups"] <- as.character(d[,"Groups"])
                  d[d[,"Groups"]=="response" & !is.na(d[,"Groups"]),"Groups"] <- paste("response (n=",nrow(d[d[,"Groups"]=="response",]),")")
                  d[d[,"Groups"]=="nonresponse" & !is.na(d[,"Groups"]),"Groups"] <- paste("nonresponse (n=",nrow(d[d[,"Groups"]=="nonresponse",]),")")
                  ind <- unique(d[,"Groups"])
                  try.test <- try(p.v <- wilcox.test(as.numeric(d[d[,"Groups"]==ind[1],"Mutation.Load"]),as.numeric(d[d[,"Groups"]==ind[2],"Mutation.Load"]))$p.value)
                  if (class(try.test)=="try-error") {
                    p.v <- "NA"
                  } else {
                    p.v <- wilcox.test(as.numeric(d[d[,"Groups"]==ind[1],"Mutation.Load"]),as.numeric(d[d[,"Groups"]==ind[2],"Mutation.Load"]))$p.value
                    if (p.v < 0.001) {
                      p.v <- "< 0.001"
                    } else {
                      p.v<-round(p.v,3)
                    } 
                  }
                  
                  if (unique(d$data_source)=="Robert") {
                    lable_y <- "Number of mutations/megabase"
                    text_y <- "no immunotherapy outcome for this data"
                    color_y <- "purple"
                    size_y <- 16
                  } else {
                    lable_y <- "Number of mutations"
                    text_y <- paste0("p-value = ",p.v)
                    color_y <- "black"
                    size_y <- 15
                  }
                  p<-plot_ly(data=d,x=~Groups,
                             y=~Mutation.Load,
                             type="box",
                             boxpoints="all",
                             jitter=0.3,
                             pointpos=-1.8,
                             #mode="markders",
                             color= ~PFS.status, showlegend=TRUE,
                             colors=c("plum","lightgreen"),
                             #hoveron="points+boxes",
                             legendgroup=~PFS.status,
                             text= ~paste0("Sample ID: ", PatientID,
                                           "<br>Mutation loads: ",Mutation.Load,
                                           "<br>Treatment: ",treatment,
                                           "<br>Description: ",description)) %>%
                    layout(title = unique(d$data_source),titlefont=list(size=20,color="black",family= "Arial"),
                           height = 480 ,
                           xaxis=list(title="Immunotherapy response",titlefont=list(size=20,color="black",family= "Aril black"),
                                      showticklabels=TRUE,tickcolor="#444",linecolor = "#444"),
                           yaxis=list(title=lable_y,
                                      titlefont=list(size=20,color="black",family= "Aril black"),
                                      showticklabels=TRUE,tickmode="auto",nticks=4,tickcolor="#444",linecolor = "#444")) %>% 
                    add_annotations(x=0.5,
                                    y=d[which.max(d[,"Mutation.Load"]),"Mutation.Load"],
                                    text=text_y,   
                                    font=list(size=size_y,color=color_y),
                                    xref = paste0("x", 1),  # add this
                                    yref = paste0("y", 1),  # add this
                                    showarrow = FALSE,
                                    ax = 20,
                                    ay = -40) 
                } 
                
              }
#            })
            })
            #          }
          })
        }
      }
    })
  })
  
  
  
  output$mutation_loads_plots_download <- downloadHandler(
    filename = function() {
      if (input$mutation_loads_plots_type == "Excel (CSV)") {
        "newfile.csv"
      } else if (input$mutation_loads_plots_type == "Text (TSV)") {
        "newfile.txt"
      } else if (input$mutation_loads_plots_type == "JSON") {
        "newfile.json"
      }
    },
    content = function(file) {
      if (input$mutation_loads_plots_type == "Excel (CSV)") {
        write.csv (mutation_loads_plots_download_data(),file,row.names = FALSE)
      } else if (input$mutation_loads_plots_type == "Text (TSV)") {
        write.table (mutation_loads_plots_download_data(),file,quote=F, row.names = FALSE, sep="\t")
      } else if (input$mutation_loads_plots_type == "JSON") {
        write(toJSON(mutation_loads_plots_download_data()),file)
      }
    }
  )  
  mutation_loads_plots_download_data <- function(){
    m<-selecteddatabase_mutation_loads()
    m<-m[,c(1:7,10:11)]
    m
  }
  output$mutation_loads_plots_table <- DT::renderDataTable({
    input$do_mutation_loads
    isolate({
      mutation_loads_plots_download_data()
    })
  })    
  #############
  #### mutation loads survival
  #############
  output$mutation_loads_survival_n_samples_NA <- renderText({
    input$do_mutation_loads
    isolate({
      nrow(selecteddatabase_mutation_loads())
    })
  })
  
  output$mutation_loads_survival_plots <- renderUI({
    input$do_mutation_loads
    isolate({
      selected.database <- selecteddatabase_mutation_loads()
      if (nrow(selected.database)==0 | nrow(selected.database[!is.na(selected.database[,11]),])==0) {
        tagList(plotlyOutput("mutation_loads_survival_plots0",height="auto"))
      } else {
        if (length(input$data_source_mutation_loads)==1) {
          tagList(plotlyOutput("mutation_loads_survival_plots1",height="auto"),tags$h1(" "))
        } else if (length(input$data_source_mutation_loads)==2) {
          tagList(plotlyOutput("mutation_loads_survival_plots1",height="auto"),tags$h1(" "),
                  plotlyOutput("mutation_loads_survival_plots2",height="auto"),tags$h1(" "))
        } else if (length(input$data_source_mutation_loads)==3) {
          tagList(plotlyOutput("mutation_loads_survival_plots1",height="auto"),tags$h1(" "),
                  plotlyOutput("mutation_loads_survival_plots2",height="auto"),tags$h1(" "),
                  plotlyOutput("mutation_loads_survival_plots3",height="auto"),tags$h1(" "))
        } else if (length(input$data_source_mutation_loads)==4) {
          tagList(plotlyOutput("mutation_loads_survival_plots1",height="auto"),tags$h1(" "),
                  plotlyOutput("mutation_loads_survival_plots2",height="auto"),tags$h1(" "),
                  plotlyOutput("mutation_loads_survival_plots3",height="auto"),tags$h1(" "),
                  plotlyOutput("mutation_loads_survival_plots4",height="auto"),tags$h1(" "))
        } else if (length(input$data_source_mutation_loads)==5) {
          tagList(plotlyOutput("mutation_loads_survival_plots1",height="auto"),tags$h1(" "),
                  plotlyOutput("mutation_loads_survival_plots2",height="auto"),tags$h1(" "),
                  plotlyOutput("mutation_loads_survival_plots3",height="auto"),tags$h1(" "),
                  plotlyOutput("mutation_loads_survival_plots4",height="auto"),tags$h1(" "),
                  plotlyOutput("mutation_loads_survival_plots5",height="auto"),tags$h1(" "))
        } else if (length(input$data_source_mutation_loads)==6) {
          tagList(plotlyOutput("mutation_loads_survival_plots1",height="auto"),tags$h1(" "),
                  plotlyOutput("mutation_loads_survival_plots2",height="auto"),tags$h1(" "),
                  plotlyOutput("mutation_loads_survival_plots3",height="auto"),tags$h1(" "),
                  plotlyOutput("mutation_loads_survival_plots4",height="auto"),tags$h1(" "),
                  plotlyOutput("mutation_loads_survival_plots5",height="auto"),tags$h1(" "),
                  plotlyOutput("mutation_loads_survival_plots6",height="auto"),tags$h1(" "))
        } 
      }
    })
  })
  
  observeEvent(input$do_mutation_loads,{
    input$do_mutation_loads
    isolate({
      selected.database <- selecteddatabase_mutation_loads()
      if (nrow(selected.database)==0 | nrow(selected.database[!is.na(selected.database[,11]),])==0) {
        output[["mutation_loads_survival_plots0"]] <- renderPlotly({
          plot_ly () %>% 
            layout(title = paste(input$data_source_mutation_loads,collapse = ","),font=list(size=15,family="Arial")) %>%
            add_annotations(x=2,y=2,text="no data available under this selection",showarrow=F,font=list(size=16,color="purple")) 
        })
      } else {
        #    selected.database[selected.database[,"OS.status"]=="living" & !is.na(selected.database[,"OS.status"]),"dead"] <- 0
        #    selected.database[selected.database[,"OS.status"]=="deceased" & !is.na(selected.database[,"OS.status"]),"dead"] <- 1
        #    selected.database <- selected.database[!is.na(selected.database[,"Mutation.Load"]) & !is.na(selected.database[,"dead"]),]
        #    selected.database <- selected.database[selected.database[,"dead"]==1 | selected.database[,"dead"]==0,]
        
        splitted_list <- selected.database %>% as.matrix %>% as.data.frame %>% split(.$data_source)
        splitted_list <- splitted_list[input$data_source_mutation_loads]
        for (k in 1:length(input$data_source_mutation_loads)) {
          local({
            my_i <- k
            mutation_loads_survival_plots_name <- paste0("mutation_loads_survival_plots", my_i)
            d<-splitted_list[[my_i]]
            output[[mutation_loads_survival_plots_name]] <- renderPlotly({
#              isolate({
              if (is.na(names(splitted_list)[my_i])) {
                plot_ly () %>% 
                  layout(title = input$data_source_mutation_loads[[my_i]],font=list(size=15,family="Arial")) %>%
                  add_annotations(x=2,y=2,text="no data available under this selection",showarrow=F,font=list(size=16,color="purple")) 
              } else {
                
                if (nrow(d[!is.na(d[,"OS.status"]),])==0 | nrow(d[!is.na(d[,"OS"]),])==0) {
                  plot_ly () %>% 
                    layout(title = unique(d$data_source),font=list(size=15,family="Arial")) %>%
                    add_annotations(x=2,y=2,text="no overall survival outcome under this selection",showarrow=F,font=list(size=16,color="purple")) 
                } else {
                  d <- d[!is.na(d[,"OS.status"]) & !is.na(d[,"OS"]) & !is.na(d[,"Mutation.Load"]),]
                  d<-data.frame(d,dead=NA)
                  d[d[,"OS.status"]=="living" & !is.na(d[,"OS.status"]),"dead"] <- 0
                  d[d[,"OS.status"]=="deceased" & !is.na(d[,"OS.status"]),"dead"] <- 1
                  d <- d[!is.na(d[,"Mutation.Load"]) & !is.na(d[,"dead"]),]
                  d <- d[d[,"dead"]==1 | d[,"dead"]==0,]
                  
                  if (unique(d$data_source)=="Robert") {
                    lable_y <- "TMB"
                  } else {
                    lable_y <- "number of mutations"
                  }
                  d<-data.frame(d[,1:6],unfactor(d[,c("dead","OS","PFS","Age","Mutation.Load")]))
                  d<-data.frame(d,IND=paste0("samples with bottom ",
                                             input$slider_mutation_loads_OS*100, 
                                             "% ",
                                             lable_y,
                                             " (",
                                             nrow(d[!is.na(d[,"Mutation.Load"]) & d[,"Mutation.Load"]<= quantile(na.omit(d[,"Mutation.Load"]),input$slider_mutation_loads_OS),]),
                                             ")"))
                  d[,"IND"]<-as.character(d[,"IND"])
                  d[!is.na(d[,"Mutation.Load"]) & d[,"Mutation.Load"]> quantile(na.omit(d[,"Mutation.Load"]),input$slider_mutation_loads_OS),"IND"]<- 
                    paste0("samples with top ",
                           (1-input$slider_mutation_loads_OS)*100,
                           "% ",
                           lable_y,
                           " (",
                           nrow(d[d[,"Mutation.Load"]> quantile(d[,"Mutation.Load"],input$slider_mutation_loads_OS),])
                           ,")")
                  d[,"dead"] <- as.numeric(d[,"dead"]) ### I don't know why I need to as.numeric since expression section doesn't need this
                  d<-d[order(d[,"IND"]),]
                  fit.result <- survfit(Surv(OS,dead)~IND,data=d)
                  plot.result <- ggsurv(fit.result,surv.col = c("plum","lightgreen"))
                  diff.result <- survdiff(Surv(OS,dead)~IND,data=d)
                  p.value= 1 - pchisq(diff.result$chisq, length(diff.result$n) - 1)
                  plot.result.ann <- plot.result + theme(legend.position = "right", legend.title = element_blank()) + 
                    ggplot2::geom_text(aes(label = sprintf("p-value = %0.2g",
                                                           pchisq(diff.result$chisq,df=1,lower.tail = F)),x=200,y=0.05))
                  p1<-ggplotly(plot.result.ann,legendgroup=~IND,height = 480 ) %>%
                    layout(title = unique(d$data_source),titlefont=list(size=20,color="black",family= "Arial"),
                           xaxis=list(title="Days",titlefont=list(size=20,color="black",family= "Aril black")),
                           yaxis=list(title="Overall survival proportion",titlefont=list(size=20,color="black",family= "Aril black"))) #%>%
                  p1
                }
              }
#            })
            })
          })
        }
      }
    })
  })
  
  output$mutation_loads_survival_download <- downloadHandler(
    filename = function() {
      if (input$mutation_loads_survival_type == "Excel (CSV)") {
        "newfile.csv"
      } else if (input$mutation_loads_survival_type == "Text (TSV)") {
        "newfile.txt"
      } else if (input$mutation_loads_survival_type == "JSON") {
        "newfile.json"
      }
    },
    content = function(file) {
      if (input$mutation_loads_survival_type == "Excel (CSV)") {
        write.csv (mutation_loads_survival_download_data(),file,row.names = FALSE)
      } else if (input$mutation_loads_survival_type == "Text (TSV)") {
        write.table (mutation_loads_survival_download_data(),file,quote=F, row.names = FALSE, sep="\t")
      } else if (input$mutation_loads_survival_type == "JSON") {
        write(toJSON(mutation_loads_survival_download_data()),file)
      }
    }
  )
  mutation_loads_survival_download_data <- function() {
    m<-selecteddatabase_mutation_loads()
    if (nrow(m)>0 & ncol(m)>10) {
      m <- data.frame(m,IND=NA)
      m1 <- m %>% split(.$data_source) %>% 
        lapply(function(d) {#d[d[,"UserGene"]>=median(na.omit(d[,"UserGene"])),"IND"] <- paste("top 50% ", input$gene_expression," expression",sep="")
          d[!is.na(d[,"Mutation.Load"]) & d[,"Mutation.Load"]<= quantile(na.omit(d[,"Mutation.Load"]),input$slider_mutation_loads_OS),"IND"] <- 
            paste0("samples with bottom ",
                   input$slider_mutation_loads_OS*100,
                   "% mutation loads or TMB")
          d[!is.na(d[,"Mutation.Load"]) & d[,"Mutation.Load"]> quantile(na.omit(d[,"Mutation.Load"]),input$slider_mutation_loads_OS),"IND"] <- 
            paste0("samples with top ",
                   (1-input$slider_mutation_loads_OS)*100,
                   "% mutation loads or TMB")
          
          d})
      m2<-ldply(m1,data.frame)
      m2<-m2[,-1]
      m2
      #m2[,c(1:8,10:12)]
    } else {
      m2<-m
      m2
      #m2[,c(1:8,10:11)]
    }
  }
  
  output$mutation_loads_survival_table <- DT::renderDataTable({
#    input$do_mutation_loads
#    isolate({
      mutation_loads_survival_download_data()
#    })
  })
  #############
  #### mutation loads PFS
  #############
  output$mutation_loads_PFS_plots <- renderUI({
    input$do_mutation_loads
    isolate({
      selected.database <- selecteddatabase_mutation_loads()
      if (nrow(selected.database)==0 | nrow(selected.database[!is.na(selected.database[,11]),])==0) {
        tagList(plotlyOutput("mutation_loads_PFS_plots0",height="auto"))
      } else {
        if (length(input$data_source_mutation_loads)==1) {
          tagList(plotlyOutput("mutation_loads_PFS_plots1",height="auto"),tags$h1(" "))
        } else if (length(input$data_source_mutation_loads)==2) {
          tagList(plotlyOutput("mutation_loads_PFS_plots1",height="auto"),tags$h1(" "),
                  plotlyOutput("mutation_loads_PFS_plots2",height="auto"),tags$h1(" "))
        } else if (length(input$data_source_mutation_loads)==3) {
          tagList(plotlyOutput("mutation_loads_PFS_plots1",height="auto"),tags$h1(" "),
                  plotlyOutput("mutation_loads_PFS_plots2",height="auto"),tags$h1(" "),
                  plotlyOutput("mutation_loads_PFS_plots3",height="auto"),tags$h1(" "))
        } else if (length(input$data_source_mutation_loads)==4) {
          tagList(plotlyOutput("mutation_loads_PFS_plots1",height="auto"),tags$h1(" "),
                  plotlyOutput("mutation_loads_PFS_plots2",height="auto"),tags$h1(" "),
                  plotlyOutput("mutation_loads_PFS_plots3",height="auto"),tags$h1(" "),
                  plotlyOutput("mutation_loads_PFS_plots4",height="auto"),tags$h1(" "))
        } else if (length(input$data_source_mutation_loads)==5) {
          tagList(plotlyOutput("mutation_loads_PFS_plots1",height="auto"),tags$h1(" "),
                  plotlyOutput("mutation_loads_PFS_plots2",height="auto"),tags$h1(" "),
                  plotlyOutput("mutation_loads_PFS_plots3",height="auto"),tags$h1(" "),
                  plotlyOutput("mutation_loads_PFS_plots4",height="auto"),tags$h1(" "),
                  plotlyOutput("mutation_loads_PFS_plots5",height="auto"),tags$h1(" "))
        } else if (length(input$data_source_mutation_loads)==6) {
          tagList(plotlyOutput("mutation_loads_PFS_plots1",height="auto"),tags$h1(" "),
                  plotlyOutput("mutation_loads_PFS_plots2",height="auto"),tags$h1(" "),
                  plotlyOutput("mutation_loads_PFS_plots3",height="auto"),tags$h1(" "),
                  plotlyOutput("mutation_loads_PFS_plots4",height="auto"),tags$h1(" "),
                  plotlyOutput("mutation_loads_PFS_plots5",height="auto"),tags$h1(" "),
                  plotlyOutput("mutation_loads_PFS_plots6",height="auto"),tags$h1(" "))
        } 
      }
    })
  })
  observeEvent(input$do_mutation_loads,{
    input$do_mutation_loads
    isolate({
      selected.database <- selecteddatabase_mutation_loads()
      if (nrow(selected.database)==0 | nrow(selected.database[!is.na(selected.database[,11]),])==0) {
        output[["mutation_loads_PFS_plots0"]] <- renderPlotly({
          plot_ly () %>% 
            layout(title = paste(input$data_source_mutation_loads,collapse = ","),font=list(size=15,family="Arial")) %>%
            add_annotations(x=2,y=2,text="no data available under this selection",showarrow=F,font=list(size=16,color="purple")) 
        })
      } else {
        splitted_list <- selected.database %>% as.matrix %>% as.data.frame %>% split(.$data_source)
        splitted_list <- splitted_list[input$data_source_mutation_loads]
        for (k in 1:length(input$data_source_mutation_loads)) {
          local({
            my_i <- k
            mutation_loads_PFS_plots_name <- paste0("mutation_loads_PFS_plots", my_i)
            d<-splitted_list[[my_i]]
            output[[mutation_loads_PFS_plots_name]] <- renderPlotly({
              if (is.na(names(splitted_list)[my_i])) {
                plot_ly () %>% 
                  layout(title = input$data_source_mutation_loads[[my_i]],font=list(size=15,family="Arial")) %>%
                  add_annotations(x=2,y=2,text="no data available under this selection",showarrow=F,font=list(size=16,color="purple")) 
              } else {
                if (nrow(d[!is.na(d[,"PFS.status"]),])==0 | nrow(d[!is.na(d[,"PFS"]),])==0) {
                  plot_ly () %>% 
                    layout(title = unique(d$data_source),font=list(size=15,family="Arial")) %>%
                    add_annotations(x=2,y=2,text="no PFS outcome under this selection",showarrow=F,font=list(size=16,color="purple")) 
                } else {
                  d <- d[!is.na(d[,"Mutation.Load"]) & !is.na(d[,"PFS"]),]
                  
                  if (unique(d$data_source)=="Robert") {
                    lable_y <- "TMB"
                  } else {
                    lable_y <- "number of mutations"
                  }
                  d<-data.frame(d[,1:6],unfactor(d[,c("OS.status","OS","PFS","Age","Mutation.Load")]))
                  d<-data.frame(d,IND=paste0("samples with bottom ",input$slider_mutation_loads_PFS*100, "% ",lable_y,
                                             " (",
                                             nrow(d[d[,"Mutation.Load"]<= quantile(d[,"Mutation.Load"],input$slider_mutation_loads_PFS),]),
                                             ")"))
                  d[,"IND"]<-as.character(d[,"IND"])
                  d[d[,"Mutation.Load"]> quantile(d[,"Mutation.Load"],input$slider_mutation_loads_PFS),"IND"]<- 
                    paste0("samples with top ",(1-input$slider_mutation_loads_PFS)*100,"% ",lable_y," (",
                           nrow(d[d[,"Mutation.Load"]> quantile(d[,"Mutation.Load"],input$slider_mutation_loads_PFS),])
                           ,")")
                  d <- data.frame(d,group.IND=0)
                  d[d[,"PFS.status"]=="nonresponse","group.IND"] <- 1
                  #        d[,"dead"] <- as.numeric(d[,"dead"]) ### I don't know why I need to as.numeric since expression section doesn't need this
                  d<-d[order(d[,"IND"]),]
                  fit.result <- survfit(Surv(PFS,group.IND)~IND,data=d)
                  plot.result <- ggsurv(fit.result,surv.col = c("plum","lightgreen"))
                  diff.result <- survdiff(Surv(PFS,group.IND)~IND,data=d)
                  p.value= 1 - pchisq(diff.result$chisq, length(diff.result$n) - 1)
                  plot.result.ann <- plot.result + theme(legend.position = "right", legend.title = element_blank()) + 
                    ggplot2::geom_text(aes(label = sprintf("p-value = %0.2g",
                                                           pchisq(diff.result$chisq,df=1,lower.tail = F)),x=200,y=0.05))
                  p1<-ggplotly(plot.result.ann,legendgroup=~IND, height = 480  ) %>%
                    layout(title = unique(d$data_source),titlefont=list(size=20,color="black",family= "Arial"),
                           xaxis=list(title="Days",titlefont=list(size=20,color="black",family= "Aril")),
                           yaxis=list(title="Progression free survival proportion",titlefont=list(size=20,color="black",family= "Aril"))) 
                  p1
                }
              }
            })
          })
        }
      }
    })
  })
  
  output$mutation_loads_PFS_n_samples_NA <- renderText({
    input$do_mutation_loads
    isolate({
      nrow(selecteddatabase_mutation_loads())
    })
  })
  output$mutation_loads_PFS_download <- downloadHandler(
    filename = function() {
      if (input$mutation_loads_PFS_type == "Excel (CSV)") {
        "newfile.csv"
      } else if (input$mutation_loads_PFS_type == "Text (TSV)") {
        "newfile.txt"
      } else if (input$mutation_loads_PFS_type == "JSON") {
        "newfile.json"
      }
    },
    content = function(file) {
      if (input$mutation_loads_PFS_type == "Excel (CSV)") {
        write.csv (mutation_loads_PFS_download_data(),file,row.names = FALSE)
      } else if (input$mutation_loads_PFS_type == "Text (TSV)") {
        write.table (mutation_loads_PFS_download_data(),file,quote=F, row.names = FALSE, sep="\t")
      } else if (input$mutation_loads_PFS_type == "JSON") {
        write(toJSON(mutation_loads_PFS_download_data()),file)
      }
    }
  )
  mutation_loads_PFS_download_data <- function() {

    selected.database<-selecteddatabase_mutation_loads()
    if (nrow(selected.database)>0 & ncol(selected.database)>10) {
      selected.database <- data.frame(selected.database,IND=NA)
      
      m1 <- selected.database %>% split(.$data_source) %>% 
        lapply(function(d) {
          d[!is.na(d[,"Mutation.Load"]) & d[,"Mutation.Load"] <= quantile(na.omit(d[,"Mutation.Load"]),input$slider_mutation_loads_PFS),"IND"] <- paste0("samples with bottom ",input$slider_mutation_loads_PFS*100,"% mutation loads or TMB")
          d[!is.na(d[,"Mutation.Load"]) & d[,"Mutation.Load"] > quantile(na.omit(d[,"Mutation.Load"]),input$slider_mutation_loads_PFS),"IND"] <- paste0("samples with top ",(1-input$slider_mutation_loads_PFS)*100,"% mutation loads or TMB")
          d
        })
      m2 <- ldply(m1,data.frame)
      m2[,-c(1,7:8)]
#      m2[,c(1:6,9:12)]
    } else {
      selected.database[,-c(7:8)]
    }

  }
    output$mutation_loads_PFS_table <- DT::renderDataTable ({
#      input$do_mutation_loads
#      isolate({
        mutation_loads_PFS_download_data()
#      })
    })
    ###########################################
    #### mutational signatures
    ###########################################
    output$data_source_mutation_signature <- renderUI({
      all.data.source <- as.character(unique(database.test.process()[,"data_source"]))
      user.data.source <- all.data.source[!(all.data.source %in% c("Robert","Snyder","Moshe","Auslander","Chen","Hugo","Prat","Riaz","Van Allen","Krieg_panel_1","Krieg_panel_2","Krieg_panel_3"))]
      selectInput("data_source_mutation_signature",
                  "Data source: ",
                  choices = c("Hugo","Riaz","Snyder","Van Allen",user.data.source),
                  multiple = TRUE,
                  selected = c("Van Allen","Riaz"))})
    output$treatment_mutation_signature <- renderUI ({
      selectInput("treatment_mutation_signature",
                  "Treatment: ",
                  #                choices = as.character(unique(filter_input_interactive(filter_input=input$data_source_mutation_signature)[,"treatment"])),
                  choices = as.character(unique(database.test.process()[,"treatment"])),
                  multiple = TRUE,
                  selected = c("anti-CTLA-4","anti-PD-1","anti-PD-1+anti-CTLA-4"))})
    output$description_mutation_signature <- renderUI ({
      selectInput("description_mutation_signature",
                  "Pre- or On-treatment: ",
                  #                choices = as.character(unique(filter_input_interactive(filter_input=c(input$data_source_mutation_signature,input$treatment_mutation_signature))[,"description"])),
                  choices = as.character(unique(database.test.process()[,"description"])),
                  multiple = TRUE,
                  selected = c("Pre","On"))})
    output$signature_mutation_signature <- renderUI ({
      selectInput("signature_mutation_signature",
                  "mutational signature type: ",
                  choices = sort(c("Signature.1 (age correlated)","Signature.2","Signature.3 (BRCA1 and BRCA2 associated)","Signature.4 (tobacco associated)","Signature.5","Signature.6 (DNA mismatch repair associated)","Signature.7 (ultraviolet associated)","Signature.8","Signature.9","Signature.10","Signature.11","Signature.12","Signature.13","Signature.14 (high tumor mutation burden associated)","Signature.15 (DNA mismatch repair associated)","Signature.16","Signature.17","Signature.18","Signature.19","Signature.20 (DNA mismatch repair associated)","Signature.21","Signature.22","Signature.23","Signature.24","Signature.25 (Hodgkin's cell lines only)","Signature.26 (DNA mismatch repair associated)","Signature.27","Signature.28","Signature.29","Signature.30","unknown")),
                  selected = "Signature.1 (age correlated)")})
    output$slider_mutation_signature_OS <- renderUI({
      sliderInput("slider_mutation_signature_OS",
                  "Seperate samples with mutational signature cutoff (%): ",
                  min=0,max=1,value=0.5)  })
    output$slider_mutation_signature_PFS <- renderUI({
      sliderInput("slider_mutation_signature_PFS",
                  "Seperate samples with mutational signature cutoff (%): ",
                  min=0,max=1,value=0.5)  })
    
    selecteddatabase_mutation_signature <- eventReactive(input$do_mutation_signature,{
      # Due to dplyr issue #318, we need temp variables for input values
      #    if (input$data_source_mutation_signature == "ALL") {
      #      data_source1 <- c("Hugo","Riaz","Snyder","Van Allen")
      #    } else {
      data_source1 <- input$data_source_mutation_signature
      #    }
      
      treatment1 <- input$treatment_mutation_signature
      description1 <- input$description_mutation_signature
      #    if (input$signature_mutation_signature==""){
      #      genes1 <- c("PatientID","data_source","treatment","description","group","RECIST","dead","OS","PFS","Age","Signature.1 (age correlated)")
      #    } else {
      genes1 <- c("PatientID","data_source","treatment","description","PFS.status","RECIST","OS.status","OS","PFS","Age",str_split(input$signature_mutation_signature," ",2)[[1]][1])
      #    }
      
      # Apply filters
      #    m <- database.test.process() %>% 
      #      filter(
      #        data_source %in% data_source1,
      #        treatment %in% treatment1,
      #        description %in% description1
      #      ) %>%
      #      select(genes1)
      a<-data.table(database.test.process())
      m<-a[data_source %in% data_source1 & treatment %in% treatment1 & description %in% description1]
      m<-m[,colnames(m) %in% genes1,with=FALSE]
      colnames(m)[11] <- "Signature"
      as.data.frame(m)
    })
    ##########
    #### mutational signature plots
    ##########
    output$mutation_signature_plots_n_samples_NA <- renderText({
      input$do_mutation_signature
      isolate({
        nrow(selecteddatabase_mutation_signature())
      })
    })
    output$mutation_signature_plots_plots <- renderUI({
      input$do_mutation_signature
      isolate({
        input_signature_mutation_signature <- input$signature_mutation_signature
        selected.database <- selecteddatabase_mutation_signature()
        if (nrow(selected.database)==0 | nrow(selected.database[!is.na(selected.database[,11]),])==0) {
          tagList(plotlyOutput("mutation_signature_plots_plots0",height="auto"))
        } else {
          if (length(input$data_source_mutation_signature)==1) {
            tagList(plotlyOutput("mutation_signature_plots_plots1",height="auto"),tags$h1("  "))
          } else if (length(input$data_source_mutation_signature)==2) {
            tagList(plotlyOutput("mutation_signature_plots_plots1",height="auto"),tags$h1("  "),
                    plotlyOutput("mutation_signature_plots_plots2",height="auto"),tags$h1("  "))
          } else if (length(input$data_source_mutation_signature)==3) {
            tagList(plotlyOutput("mutation_signature_plots_plots1",height="auto"),tags$h1("  "),
                    plotlyOutput("mutation_signature_plots_plots2",height="auto"),tags$h1("  "),
                    plotlyOutput("mutation_signature_plots_plots3",height="auto"),tags$h1("  "))
          } else if (length(input$data_source_mutation_signature)==4) {
            tagList(plotlyOutput("mutation_signature_plots_plots1",height="auto"),tags$h1("  "),
                    plotlyOutput("mutation_signature_plots_plots2",height="auto"),tags$h1("  "),
                    plotlyOutput("mutation_signature_plots_plots3",height="auto"),tags$h1("  "),
                    plotlyOutput("mutation_signature_plots_plots4",height="auto"),tags$h1("  "))
          } else if (length(input$data_source_mutation_signature)==5) {
            tagList(plotlyOutput("mutation_signature_plots_plots1",height="auto"),tags$h1("  "),
                    plotlyOutput("mutation_signature_plots_plots2",height="auto"),tags$h1("  "),
                    plotlyOutput("mutation_signature_plots_plots3",height="auto"),tags$h1("  "),
                    plotlyOutput("mutation_signature_plots_plots4",height="auto"),tags$h1("  "),
                    plotlyOutput("mutation_signature_plots_plots5",height="auto"),tags$h1("  "))
          } 
        }
      })
    })
    observeEvent(input$do_mutation_signature,{
      input$do_mutation_signature
      isolate({
        input_signature_mutation_signature <- input$signature_mutation_signature
        selected.database <- selecteddatabase_mutation_signature()
        if (nrow(selected.database)==0 | nrow(selected.database[!is.na(selected.database[,11]),])==0) {
          output[["mutation_signature_plots_plots0"]] <- renderPlotly({
            plot_ly () %>% 
              layout(title = paste(input$data_source_mutation_signature,collapse = ","),font=list(size=15,family="Arial")) %>%
              add_annotations(x=2,y=2,text="no data available under this selection",showarrow=F,font=list(size=16,color="purple")) 
          })
        } else {
          selected.database <- data.frame(selected.database,Groups=selected.database$PFS.status)
          splitted_list <- selected.database %>% as.matrix %>% as.data.frame %>% split(.$data_source)
          splitted_list <- splitted_list[input$data_source_mutation_signature]
          for (k in 1:length(input$data_source_mutation_signature)) {
            local({
              my_i <- k
              mutation_signature_plots_plots_name <- paste0("mutation_signature_plots_plots", my_i)
              d<-splitted_list[[my_i]]
              output[[mutation_signature_plots_plots_name]] <- renderPlotly({
                if (is.na(names(splitted_list)[my_i])) {
                  plot_ly () %>% 
                    layout(title = input$data_source_mutation_signature[[my_i]],font=list(size=15,family="Arial")) %>%
                    add_annotations(x=2,y=2,text="no data available under this selection",showarrow=F,font=list(size=16,color="purple")) 
                } else {
                  #d <- splitted_list[[my_i]]
                  if (nrow(d[!is.na(d[,"PFS.status"]),])==0) {
                    plot_ly () %>% 
                      layout(title = unique(d$data_source),font=list(size=15,family="Arial")) %>%
                      add_annotations(x=2,y=2,text="no immunotherapy response under this selection",showarrow=F,font=list(size=16,color="purple")) 
                  } else {
                    d[,"Groups"] <- as.character(d[,"Groups"])
                    d[d[,"Groups"]=="response" & !is.na(d[,"Groups"]),"Groups"] <- paste("response (n=",nrow(d[d[,"Groups"]=="response",]),")")
                    d[d[,"Groups"]=="nonresponse" & !is.na(d[,"Groups"]),"Groups"] <- paste("nonresponse (n=",nrow(d[d[,"Groups"]=="nonresponse",]),")")
                    ind <- unique(d[,"Groups"])
                    try.test <- try(p.v <- wilcox.test(as.numeric(d[d[,"Groups"]==ind[1],"Signature"]),as.numeric(d[d[,"Groups"]==ind[2],"Signature"]))$p.value)
                    if (class(try.test)=="try-error" ) {
                      p.v <- "NA"
                    } else {
                      p.v <- wilcox.test(as.numeric(d[d[,"Groups"]==ind[1],"Signature"]),as.numeric(d[d[,"Groups"]==ind[2],"Signature"]))$p.value
                      if (is.na(p.v)) {
                        p.v <- "NA"
                      } else {
                        if (p.v < 0.001) {
                          p.v <- "< 0.001"
                        } else {
                          p.v<-round(p.v,3)
                        } 
                      }
                    }
                    plot_ly(data=d,x=~Groups,
                            y=~Signature,
                            type="box",
                            boxpoints="all",
                            jitter=0.3,
                            pointpos=-1.8,
                            #mode="markders",
                            color= ~PFS.status,
                            colors=c("plum","lightgreen"),
                            #hoveron="points+boxes",
                            legendgroup=~PFS.status,
                            showlegend=TRUE,
                            text= ~paste0("Sample ID: ", PatientID,
                                          "<br>Signature: ",str_split(input$signature_mutation_signature," ",2)[[1]][1]," :",Signature,
                                          "<br>Treatment: ",treatment,
                                          "<br>Description: ",description)) %>%
                      layout(title = unique(d$data_source),titlefont=list(size=20,color="black",family= "Arial"),
                             height = 480,
                             xaxis=list(title="Immunotherapy response",titlefont=list(size=20,color="black",family= "Aril"),
                                        showticklabels=TRUE,tickcolor="#444",linecolor = "#444"),
                             yaxis=list(title=paste(str_split(input_signature_mutation_signature," ",2)[[1]][1], " weight",sep=""),
                                        titlefont=list(size=20,color="black",family= "Aril"),
                                        showticklabels=TRUE,tickmode="auto",nticks=4,tickcolor="#444",linecolor = "#444"),
                             annotations=list(x=0.5,y=max(na.omit(as.numeric(d[,"Signature"]))),text=paste0("p-value = ",p.v),xref="x",yref="y",
                                              showarrow=FALSE))
                  } 
                  
                }
              })
            })
          }
        }
      })
    })
    
    
    output$mutation_signature_plots_download <- downloadHandler(
      filename = function() {
        if (input$mutation_signature_plots_type == "Excel (CSV)") {
          "newfile.csv"
        } else if (input$mutation_signature_plots_type == "Text (TSV)") {
          "newfile.txt"
        } else if (input$mutation_signature_plots_type == "JSON") {
          "newfile.json"
        }
      },
      content = function(file) {
        if (input$mutation_signature_plots_type == "Excel (CSV)") {
          write.csv (mutation_signature_plots_download_data(),file,row.names = FALSE)
        } else if (input$mutation_signature_plots_type == "Text (TSV)") {
          write.table (mutation_signature_plots_download_data(),file,quote=F, row.names = FALSE, sep="\t")
        } else if (input$mutation_signature_plots_type == "JSON") {
          write(toJSON(mutation_signature_plots_download_data()),file)
        }
      }
    ) 
    mutation_signature_plots_download_data <- function() {
      input_signature_mutation_signature <- input$signature_mutation_signature
      m<-selecteddatabase_mutation_signature()
      if (str_split(input$signature_mutation_signature," ",2)[[1]][1]==""){
        colnames(m)[11] <- "Signature.1"
      } else {
        colnames(m)[11] <- str_split(input_signature_mutation_signature," ",2)[[1]][1]
      }
      m<-m[,c(1:7,10:11)]
      m
    }
    output$mutation_signature_plots_table <- DT::renderDataTable({
      input$do_mutation_signature
      isolate({
        mutation_signature_plots_download_data()
      })
    })    
    #############
    #### mutational signature survival
    #############
    output$mutation_signature_survival_n_samples_NA <- renderText({
      input$do_mutation_signature
      isolate({
        nrow(selecteddatabase_mutation_signature())
      })
    })
    
    output$mutation_signature_survival_plots <- renderUI({
      input$do_mutation_signature
      isolate({
      input_signature_mutation_signature <- input$signature_mutation_signature
      selected.database <- selecteddatabase_mutation_signature()
      if (nrow(selected.database)==0 | nrow(selected.database[!is.na(selected.database[,11]),])==0) {
        tagList(plotlyOutput("mutation_signature_survival_plots0",height="auto"))
      } else {
        if (length(input$data_source_mutation_signature)==1) {
          tagList(plotlyOutput("mutation_signature_survival_plots1",height="auto"),tags$h1(" "))
        } else if (length(input$data_source_mutation_signature)==2) {
          tagList(plotlyOutput("mutation_signature_survival_plots1",height="auto"),tags$h1(" "),
                  plotlyOutput("mutation_signature_survival_plots2",height="auto"),tags$h1(" "))
        } else if (length(input$data_source_mutation_signature)==3) {
          tagList(plotlyOutput("mutation_signature_survival_plots1",height="auto"),tags$h1(" "),
                  plotlyOutput("mutation_signature_survival_plots2",height="auto"),tags$h1(" "),
                  plotlyOutput("mutation_signature_survival_plots3",height="auto"),tags$h1(" "))
        } else if (length(input$data_source_mutation_signature)==4) {
          tagList(plotlyOutput("mutation_signature_survival_plots1",height="auto"),tags$h1(" "),
                  plotlyOutput("mutation_signature_survival_plots2",height="auto"),tags$h1(" "),
                  plotlyOutput("mutation_signature_survival_plots3",height="auto"),tags$h1(" "),
                  plotlyOutput("mutation_signature_survival_plots4",height="auto"),tags$h1(" "))
        } else if (length(input$data_source_mutation_signature)==5) {
          tagList(plotlyOutput("mutation_signature_survival_plots1",height="auto"),tags$h1(" "),
                  plotlyOutput("mutation_signature_survival_plots2",height="auto"),tags$h1(" "),
                  plotlyOutput("mutation_signature_survival_plots3",height="auto"),tags$h1(" "),
                  plotlyOutput("mutation_signature_survival_plots4",height="auto"),tags$h1(" "),
                  plotlyOutput("mutation_signature_survival_plots5",height="auto"),tags$h1(" "))
        } 
      }
    })
  })
    
    
    observeEvent(input$do_mutation_signature,{
      input$do_mutation_signature
      isolate({
        input_signature_mutation_signature <- input$signature_mutation_signature
        selected.database <- selecteddatabase_mutation_signature()
        if (nrow(selected.database)==0 | nrow(selected.database[!is.na(selected.database[,11]),])==0) {
          output[["mutation_signature_survival_plots0"]] <- renderPlotly({
            plot_ly () %>% 
              layout(title = paste(input$data_source_mutation_signature,collapse = ","),font=list(size=15,family="Arial")) %>%
              add_annotations(x=2,y=2,text="no data available under this selection",showarrow=F,font=list(size=16,color="purple")) 
          })
        } else {
          #    selected.database[selected.database[,"OS.status"]=="living" & !is.na(selected.database[,"OS.status"]),"dead"] <- 0
          #    selected.database[selected.database[,"OS.status"]=="deceased" & !is.na(selected.database[,"OS.status"]),"dead"] <- 1
          #    selected.database <- selected.database[!is.na(selected.database[,"Signature"]) & !is.na(selected.database[,"dead"]),]
          #    selected.database <- selected.database[selected.database[,"dead"]==1 | selected.database[,"dead"]==0,]
          
          splitted_list <- selected.database %>% as.matrix %>% as.data.frame %>% split(.$data_source)
          splitted_list <- splitted_list[input$data_source_mutation_signature]
          for (k in 1:length(input$data_source_mutation_signature)) {
            local({
              my_i <- k
              mutation_signature_survival_plots_name <- paste0("mutation_signature_survival_plots", my_i)
              d<-splitted_list[[my_i]]
              d[,"Signature"] <- as.numeric(as.character(d[,"Signature"]))
              output[[mutation_signature_survival_plots_name]] <- renderPlotly({
                if (is.na(names(splitted_list)[my_i])) {
                  plot_ly () %>% 
                    layout(title = input$data_source_mutation_signature[[my_i]],font=list(size=15,family="Arial")) %>%
                    add_annotations(x=2,y=2,text="no data available under this selection",showarrow=F,font=list(size=16,color="purple")) 
                } else {
                  
                  if (nrow(d[!is.na(d[,"OS.status"]),])==0 | nrow(d[!is.na(d[,"OS"]),])==0 | nrow(d[!is.na(d[,"Signature"]),])==0 | sum(d[!is.na(d[,"Signature"]),"Signature"])==0) {
                    plot_ly () %>% 
                      layout(title = unique(d$data_source),font=list(size=15,family="Arial")) %>%
                      add_annotations(x=2,y=2,text="no overall survival outcome under this selection",showarrow=F,font=list(size=16,color="purple")) 
                  } else {
                    
                    d[d[,"OS.status"]=="living" & !is.na(d[,"OS.status"]),"dead"] <- 0
                    d[d[,"OS.status"]=="deceased" & !is.na(d[,"OS.status"]),"dead"] <- 1
                    d <- d[!is.na(d[,"Signature"]) & !is.na(d[,"dead"]),]
                    d <- d[d[,"dead"]==1 | d[,"dead"]==0,]
                    
                    d<-data.frame(d[,1:6],unfactor(d[,c("dead","OS","PFS","Age","Signature")]))
                    d<-data.frame(d,IND=paste0("samples with bottom ",
                                               input$slider_mutation_signature_OS*100, 
                                               "% ",
                                               paste(str_split(input_signature_mutation_signature," ",2)[[1]][1], " weight",sep=""),
                                               " (",
                                               nrow(d[d[,"Signature"]<= quantile(d[,"Signature"],input$slider_mutation_signature_OS),]),
                                               ")"))
                    d[,"IND"]<-as.character(d[,"IND"])
                    d[d[,"Signature"]> quantile(d[,"Signature"],input$slider_mutation_signature_OS),"IND"]<- 
                      paste0("samples with top ",
                             (1-input$slider_mutation_signature_OS)*100,
                             "% ",
                             paste(str_split(input_signature_mutation_signature," ",2)[[1]][1], " weight",sep=""),
                             " (",
                             nrow(d[d[,"Signature"]> quantile(d[,"Signature"],input$slider_mutation_signature_OS),])
                             ,")")
                    d[,"dead"] <- as.numeric(d[,"dead"]) ### I don't know why I need to as.numeric since expression section doesn't need this
                    d<-d[order(d[,"IND"]),]
                    fit.result <- survfit(Surv(OS,dead)~IND,data=d)
                    plot.result <- ggsurv(fit.result,surv.col = c("plum","lightgreen"))
                    diff.result <- survdiff(Surv(OS,dead)~IND,data=d)
                    p.value= 1 - pchisq(diff.result$chisq, length(diff.result$n) - 1)
                    plot.result.ann <- plot.result + theme(legend.position = "right", legend.title = element_blank()) + 
                      ggplot2::geom_text(aes(label = sprintf("p-value = %0.2g",
                                                             pchisq(diff.result$chisq,df=1,lower.tail = F)),x=200,y=0.05))
                    p1<-ggplotly(plot.result.ann,legendgroup=~IND,height = 480 ) %>%
                      layout(title = unique(d$data_source),titlefont=list(size=20,color="black",family= "Arial"),
                             xaxis=list(title="Days",titlefont=list(size=20,color="black",family= "Aril black")),
                             yaxis=list(title="Overall survival proportion",titlefont=list(size=20,color="black",family= "Aril black"))) #%>%
                    p1
                  }
                }
              })
            })
          }
        }
      })
    })
    
    
    output$mutation_signature_survival_download <- downloadHandler(
      filename = function() {
        if (input$mutation_signature_survival_type == "Excel (CSV)") {
          "newfile.csv"
        } else if (input$mutation_signature_survival_type == "Text (TSV)") {
          "newfile.txt"
        } else if (input$mutation_signature_survival_type == "JSON") {
          "newfile.json"
        }
      },
      content = function(file) {
        if (input$mutation_signature_survival_type == "Excel (CSV)") {
          write.csv (mutation_signature_survival_download_data(),file,row.names = FALSE)
        } else if (input$mutation_signature_survival_type == "Text (TSV)") {
          write.table (mutation_signature_survival_download_data(),file,quote=F, row.names = FALSE, sep="\t")
        } else if (input$mutation_signature_survival_type == "JSON") {
          write(toJSON(mutation_signature_survival_download_data()),file)
        }
      }
    )
    mutation_signature_survival_download_data <- function() {
      input_signature_mutation_signature <- input$signature_mutation_signature
      m<-selecteddatabase_mutation_signature()
      #    m <- m[!is.na(m[,"UserGene"]),]
      if (nrow(m)>0) {
        m<-data.frame(m,IND=NA)
        m<-m[!is.na(m[,"Signature"]),]
        m1 <- m %>% split(.$data_source) %>% 
          lapply(function(d) {
            d[!is.na(d[,"Signature"]) & d[,"Signature"]> quantile(d[,"Signature"],input$slider_mutation_signature_OS),"IND"] <- paste0("samples with top ",
                                                                                                                                       (1-input$slider_mutation_signature_OS)*100,
                                                                                                                                       "% ",
                                                                                                                                       paste(str_split(input_signature_mutation_signature," ",2)[[1]][1], " weight",sep=""))
            d[!is.na(d[,"Signature"]) & d[,"Signature"]<=quantile(d[,"Signature"],input$slider_mutation_signature_OS),"IND"] <- paste0("samples with bottom ",
                                                                                                                                       input$slider_mutation_signature_OS*100,
                                                                                                                                       "% ",
                                                                                                                                       paste(str_split(input_signature_mutation_signature," ",2)[[1]][1], " weight",sep=""))
            d })
        
        m2 <- ldply(m1,data.frame)
        m2 <- m2[,-1]
        #      m2<-ldply(m1,data.frame)
        #      m2<-m2[,-1]
        if (str_split(input_signature_mutation_signature," ",2)[[1]][1]==""){
          #colnames(m2)[11] <- "Signature.1"
          m2<-m2[,c(1:8,10:12)]
          m2
        } else {
          colnames(m2)[11] <- str_split(input_signature_mutation_signature," ",2)[[1]][1]
          m2<-m2[,c(1:8,10:12)]
          m2
        }
        
      } else {
        #    m <- data.frame(m,IND=NA) #remove "IND" column, because "Error in [.data.frame: undefined columns selected"
        m2<-m
        if (str_split(input_signature_mutation_signature," ",2)[[1]][1]==""){
          #colnames(m2)[11] <- "Signature.1"
          m2<-m2[,c(1:8,10:11)] # can't select 12 clumn
          m2
        } else {
          colnames(m2)[11] <- str_split(input_signature_mutation_signature," ",2)[[1]][1]
          m2<-m2[,c(1:8,10:11)] # can't select 12 clumn
          m2
        }
        
      } 
    }
    output$mutation_signature_survival_table <- DT::renderDataTable({
#      input$do_mutation_signature
#      isolate({
        mutation_signature_survival_download_data()
#      })
    })
    #############
    #### mutational signature PFS
    #############
    
    output$mutation_signature_PFS_plots <- renderUI({
      input$do_mutation_signature
      isolate({
        input_signature_mutation_signature <- input$signature_mutation_signature
        selected.database <- selecteddatabase_mutation_signature()
        if (nrow(selected.database)==0 | nrow(selected.database[!is.na(selected.database[,11]),])==0) {
          tagList(plotlyOutput("mutation_signature_PFS_plots0",height="auto"))
        } else {
          if (length(input$data_source_mutation_signature)==1) {
            tagList(plotlyOutput("mutation_signature_PFS_plots1",height="auto"),tags$h1(" "))
          } else if (length(input$data_source_mutation_signature)==2) {
            tagList(plotlyOutput("mutation_signature_PFS_plots1",height="auto"),tags$h1(" "),
                    plotlyOutput("mutation_signature_PFS_plots2",height="auto"),tags$h1(" "))
          } else if (length(input$data_source_mutation_signature)==3) {
            tagList(plotlyOutput("mutation_signature_PFS_plots1",height="auto"),tags$h1(" "),
                    plotlyOutput("mutation_signature_PFS_plots2",height="auto"),tags$h1(" "),
                    plotlyOutput("mutation_signature_PFS_plots3",height="auto"),tags$h1(" "))
          } else if (length(input$data_source_mutation_signature)==4) {
            tagList(plotlyOutput("mutation_signature_PFS_plots1",height="auto"),tags$h1(" "),
                    plotlyOutput("mutation_signature_PFS_plots2",height="auto"),tags$h1(" "),
                    plotlyOutput("mutation_signature_PFS_plots3",height="auto"),tags$h1(" "),
                    plotlyOutput("mutation_signature_PFS_plots4",height="auto"),tags$h1(" "))
          } else if (length(input$data_source_mutation_signature)==5) {
            tagList(plotlyOutput("mutation_signature_PFS_plots1",height="auto"),tags$h1(" "),
                    plotlyOutput("mutation_signature_PFS_plots2",height="auto"),tags$h1(" "),
                    plotlyOutput("mutation_signature_PFS_plots3",height="auto"),tags$h1(" "),
                    plotlyOutput("mutation_signature_PFS_plots4",height="auto"),tags$h1(" "),
                    plotlyOutput("mutation_signature_PFS_plots5",height="auto"),tags$h1(" "))
          } 
        }
      })
    })
    
    
    observeEvent(input$do_mutation_signature,{
      input$do_mutation_signature
      isolate({
        input_signature_mutation_signature <- input$signature_mutation_signature
        selected.database <- selecteddatabase_mutation_signature()
        if (nrow(selected.database)==0 | nrow(selected.database[!is.na(selected.database[,11]),])==0) {
          output[["mutation_signature_PFS_plots0"]] <- renderPlotly({
            plot_ly () %>% 
              layout(title = paste(input$data_source_mutation_signature,collapse = ","),font=list(size=15,family="Arial")) %>%
              add_annotations(x=2,y=2,text="no data available under this selection",showarrow=F,font=list(size=16,color="purple")) 
          })
        } else {
          splitted_list <- selected.database %>% as.matrix %>% as.data.frame %>% split(.$data_source)
          splitted_list <- splitted_list[input$data_source_mutation_signature]
          for (k in 1:length(input$data_source_mutation_signature)) {
            local({
              my_i <- k
              mutation_signature_PFS_plots_name <- paste0("mutation_signature_PFS_plots", my_i)
              d<-splitted_list[[my_i]]
              d[,"Signature"] <- as.numeric(as.character(d[,"Signature"]))
              output[[mutation_signature_PFS_plots_name]] <- renderPlotly({
                if (is.na(names(splitted_list)[my_i])) {
                  plot_ly () %>% 
                    layout(title = input$data_source_mutation_signature[[my_i]],font=list(size=15,family="Arial")) %>%
                    add_annotations(x=2,y=2,text="no data available under this selection",showarrow=F,font=list(size=16,color="purple")) 
                } else {
                  
                  if (nrow(d[!is.na(d[,"PFS.status"]),])==0 | nrow(d[!is.na(d[,"PFS"]),])==0 | nrow(d[!is.na(d[,"Signature"]),])==0 | sum(d[!is.na(d[,"Signature"]),"Signature"])==0) {
                    plot_ly () %>% 
                      layout(title = unique(d$data_source),font=list(size=15,family="Arial")) %>%
                      add_annotations(x=2,y=2,text="no PFS outcome under this selection",showarrow=F,font=list(size=16,color="purple")) 
                  } else {
                    d <- d[!is.na(d[,"Signature"]) & !is.na(d[,"PFS"]) & !is.na(d[,"PFS.status"]),]
                    
                    d<-data.frame(d[,1:6],unfactor(d[,c("OS.status","OS","PFS","Age","Signature")]))
                    d<-data.frame(d,IND=paste0("samples with bottom ",
                                               input$slider_mutation_signature_PFS*100, 
                                               "% ",
                                               paste(str_split(input_signature_mutation_signature," ",2)[[1]][1], " weight",sep=""),
                                               " (",
                                               nrow(d[d[,"Signature"]<= quantile(d[,"Signature"],input$slider_mutation_signature_PFS),]),
                                               ")"))
                    d[,"IND"]<-as.character(d[,"IND"])
                    d[d[,"Signature"]> quantile(d[,"Signature"],input$slider_mutation_signature_PFS),"IND"]<- 
                      paste0("samples with top ",
                             (1-input$slider_mutation_signature_PFS)*100,
                             "% ",
                             paste(str_split(input_signature_mutation_signature," ",2)[[1]][1], " weight",sep=""),
                             " (",
                             nrow(d[d[,"Signature"]> quantile(d[,"Signature"],input$slider_mutation_signature_PFS),])
                             ,")")
                    d <- data.frame(d,group.IND=0)
                    d[d[,"PFS.status"]=="nonresponse","group.IND"] <- 1
                    
                    d<-d[order(d[,"IND"]),]
                    fit.result <- survfit(Surv(PFS,group.IND)~IND,data=d)
                    plot.result <- ggsurv(fit.result,surv.col = c("plum","lightgreen"))
                    diff.result <- survdiff(Surv(PFS,group.IND)~IND,data=d)
                    p.value= 1 - pchisq(diff.result$chisq, length(diff.result$n) - 1)
                    plot.result.ann <- plot.result + theme(legend.position = "right", legend.title = element_blank()) +  
                      ggplot2::geom_text(aes(label = sprintf("p-value = %0.2g",
                                                             pchisq(diff.result$chisq,df=1,lower.tail = F)),x=200,y=0.05))
                    p1<-ggplotly(plot.result.ann,legendgroup=~IND,height = 480 ) %>%
                      layout(title = unique(d$data_source),titlefont=list(size=20,color="black",family= "Arial"),
                             xaxis=list(title="Days",titlefont=list(size=20,color="black",family= "Aril black")),
                             yaxis=list(title="Progression free survival proportion",titlefont=list(size=20,color="black",family= "Aril black"))) #%>%
                    p1
                  }
                }
              })
            })
          }
        }
      })
    })
    
    output$mutation_signature_PFS_n_samples_NA <- renderText({
      nrow(selecteddatabase_mutation_signature())
    })
    output$mutation_signature_PFS_download <- downloadHandler(
      filename = function() {
        if (input$mutation_signature_PFS_type == "Excel (CSV)") {
          "newfile.csv"
        } else if (input$mutation_signature_PFS_type == "Text (TSV)") {
          "newfile.txt"
        } else if (input$mutation_signature_PFS_type == "JSON") {
          "newfile.json"
        }
      },
      content = function(file) {
        if (input$mutation_signature_PFS_type == "Excel (CSV)") {
          write.csv (mutation_signature_PFS_download_data(),file,row.names = FALSE)
        } else if (input$mutation_signature_PFS_type == "Text (TSV)") {
          write.table (mutation_signature_PFS_download_data(),file,quote=F, row.names = FALSE, sep="\t")
        } else if (input$mutation_signature_PFS_type == "JSON") {
          write(toJSON(mutation_signature_PFS_download_data()),file)
        }
      }
    )
    mutation_signature_PFS_download_data <- function(){
      input_signature_mutation_signature <- input$signature_mutation_signature
      m<-selecteddatabase_mutation_signature()
      #    m <- m[!is.na(m[,"UserGene"]),]
      if (nrow(m)>0) {
        m<-data.frame(m,IND=NA)
        m<-m[!is.na(m[,"Signature"]),]
        m1 <- m %>% split(.$data_source) %>% 
          lapply(function(d) {
            d[!is.na(d[,"Signature"]) & d[,"Signature"]> quantile(d[,"Signature"],input$slider_mutation_signature_PFS),"IND"] <- paste0("samples with top ",
                                                                                                                                       (1-input$slider_mutation_signature_PFS)*100,
                                                                                                                                       "% ",
                                                                                                                                       paste(str_split(input_signature_mutation_signature," ",2)[[1]][1], " weight",sep=""))
            d[!is.na(d[,"Signature"]) & d[,"Signature"]<=quantile(d[,"Signature"],input$slider_mutation_signature_PFS),"IND"] <- paste0("samples with bottom ",
                                                                                                                                       input$slider_mutation_signature_PFS*100,
                                                                                                                                       "% ",
                                                                                                                                       paste(str_split(input_signature_mutation_signature," ",2)[[1]][1], " weight",sep=""))
            d })
        m2 <- ldply(m1,data.frame)
        m2 <- m2[,-1]    
        if (str_split(input_signature_mutation_signature," ",2)[[1]][1]==""){
          m2<-m2[,c(1:6,9:12)]
          m2
        } else {
          colnames(m2)[11] <- str_split(input_signature_mutation_signature," ",2)[[1]][1]
          m2<-m2[,c(1:6,9:12)]
          m2
        }
        
      } else {
        #    m <- data.frame(m,IND=NA) #remove "IND" column, because "Error in [.data.frame: undefined columns selected"
        m2<-m
        if (str_split(input_signature_mutation_signature," ",2)[[1]][1]==""){
          m2<-m2[,c(1:6,9:11)] # can't select 12 clumn
          m2
        } else {
          colnames(m2)[11] <- str_split(input_signature_mutation_signature," ",2)[[1]][1]
          m2<-m2[,c(1:6,9:11)] # can't select 12 clumn
          m2
        }
        
      }
    }
    output$mutation_signature_PFS_table <- DT::renderDataTable({
#      input$do_mutation_signature
#      isolate({
        mutation_signature_PFS_download_data()
#      })
    })
    ###########################################
    #### expression
    ###########################################
    output$data_source_expression <- renderUI({
      all.data.source <- as.character(unique(database.test.process()[,"data_source"]))
      user.data.source <- all.data.source[!(all.data.source %in% c("Robert","Snyder","Moshe","Auslander","Chen","Hugo","Prat","Riaz","Van Allen","Krieg_panel_1","Krieg_panel_2","Krieg_panel_3"))]
      selectInput("data_source_expression",
                  "Data source: ",
                  #                label=p(p("Data source: "),
                  #                        p(strong("PLEASE NOTE: ",style="color: gray"), 
                  #                          "multi-selection of data source will cause",
                  #                          style="color:gray")),
                  #                choices = as.character(unique(database.test.process()[,"data_source"])),
                  choices= c("Auslander","Chen","Hugo","Prat","Riaz","Van Allen",user.data.source),
                  multiple = TRUE,
                  selected=c("Van Allen","Riza","Chen"))})
    output$treatment_expression <- renderUI ({
      selectInput("treatment_expression",
                  "Treatment: ",
                  #                choices=as.character(unique(filter_input_interactive(filter_input=input$data_source_expression)[,"treatment"])),
                  choices = as.character(unique(database.test.process()[,"treatment"])),
                  multiple = TRUE,
                  selected=c("anti-CTLA-4","anti-PD-1","anti-PD-1+anti-CTLA-4"))})
    output$description_expression <- renderUI ({
      selectInput("description_expression",
                  "Pre- or On-treatment: ",
                  #                choices=as.character(unique(filter_input_interactive(filter_input=c(input$data_source_expression,input$treatment_expression))[,"description"])),
                  choices = as.character(unique(database.test.process()[,"description"])),
                  multiple = TRUE,selected=c("Pre","On"))})
    #  output$gene_expression <- renderUI({
    #  observeEvent(input$do_expression,{
    updateSelectizeInput(session,"gene_expression",
                         choices=a.colnames,selected="CTLA4",
                         server=TRUE)
    #    })
    output$slider_expression_OS <- renderUI({
      sliderInput("slider_expression_OS",
                  "Seperate samples with gene expression cutoff (%): ",
                  min=0,max=1,value=0.5)  })
    output$slider_expression_PFS <- renderUI({
      sliderInput("slider_expression_PFS",
                  "Seperate samples with gene expression cutoff (%): ",
                  min=0,max=1,value=0.5)  })
    selecteddatabase_expression <- eventReactive(input$do_expression,{
      
      #    validate( 
      #      need (input$data_source_expression != "","Please select a data source!")
      #      )
      # Due to dplyr issue #318, we need temp variables for input values
      #    if (input$data_source_expression=="ALL") {
      #      data_source1 <- c("Auslander","Chen","Hugo","Prat","Riaz","Van Allen")
      #    } else {
      data_source1 <- input$data_source_expression
      #    }
      treatment1 <- input$treatment_expression
      description1 <- input$description_expression
      if (input$gene_expression==""){
        genes1 <- c("PatientID","data_source","treatment","description","PFS.status","RECIST","OS.status","OS","PFS","Age")
      } else {
        genes1 <- c("PatientID","data_source","treatment","description","PFS.status","RECIST","OS.status","OS","PFS","Age",input$gene_expression)
      }
      
      # Apply filters
      #    m <- isolate(database.test.process()) %>% 
      #      filter(
      #        data_source %in% data_source1,
      #        treatment %in% treatment1,
      #        description %in% description1
      #      ) %>%
      #      select(genes1)
      a<-data.table(database.test.process())
      m<-a[data_source %in% data_source1 & treatment %in% treatment1 & description %in% description1]
      m<-m[,colnames(m) %in% genes1,with=FALSE]
      if (ncol(m)==11) {
        colnames(m)[11] <- "UserGene"
      } 
      as.data.frame(m)
      
    })
    ################
    #### expression plots
    ################
    output$expression_plots_n_samples_NA <- renderText({
      input$do_expression
      isolate({
        nrow(selecteddatabase_expression())
        
      })
    })
    output$expression_plots_plots <- renderUI({
      input$do_expression
      isolate({
        selected.database <- selecteddatabase_expression()
        if (nrow(selected.database)==0 | ncol(selected.database)<11) {
          tagList(plotlyOutput("expression_plots_plots0",height="auto"))
        } else {
          if (length(input$data_source_expression)==1) {
            tagList(plotlyOutput("expression_plots_plots1",height="auto"),tags$h1(" "))
          } else if (length(input$data_source_expression)==2) {
            tagList(plotlyOutput("expression_plots_plots1",height="auto"),tags$h1(" "),
                    plotlyOutput("expression_plots_plots2",height="auto"),tags$h1(" "))
          } else if (length(input$data_source_expression)==3) {
            tagList(plotlyOutput("expression_plots_plots1",height="auto"),tags$h1(" "),
                    plotlyOutput("expression_plots_plots2",height="auto"),tags$h1(" "),
                    plotlyOutput("expression_plots_plots3",height="auto"),tags$h1(" "))
          } else if (length(input$data_source_expression)==4) {
            tagList(plotlyOutput("expression_plots_plots1",height="auto"),tags$h1(" "),
                    plotlyOutput("expression_plots_plots2",height="auto"),tags$h1(" "),
                    plotlyOutput("expression_plots_plots3",height="auto"),tags$h1(" "),
                    plotlyOutput("expression_plots_plots4",height="auto"),tags$h1(" "))
          } else if (length(input$data_source_expression)==5) {
            tagList(plotlyOutput("expression_plots_plots1",height="auto"),tags$h1(" "),
                    plotlyOutput("expression_plots_plots2",height="auto"),tags$h1(" "),
                    plotlyOutput("expression_plots_plots3",height="auto"),tags$h1(" "),
                    plotlyOutput("expression_plots_plots4",height="auto"),tags$h1(" "),
                    plotlyOutput("expression_plots_plots5",height="auto"),tags$h1(" "))
          } else if (length(input$data_source_expression)==6) {
            tagList(plotlyOutput("expression_plots_plots1",height="auto"),tags$h1(" "),
                    plotlyOutput("expression_plots_plots2",height="auto"),tags$h1(" "),
                    plotlyOutput("expression_plots_plots3",height="auto"),tags$h1(" "),
                    plotlyOutput("expression_plots_plots4",height="auto"),tags$h1(" "),
                    plotlyOutput("expression_plots_plots5",height="auto"),tags$h1(" "),
                    plotlyOutput("expression_plots_plots6",height="auto"),tags$h1(" "))
          } else if (length(input$data_source_expression)==7) {
            tagList(plotlyOutput("expression_plots_plots1",height="auto"),tags$h1(" "),
                    plotlyOutput("expression_plots_plots2",height="auto"),tags$h1(" "),
                    plotlyOutput("expression_plots_plots3",height="auto"),tags$h1(" "),
                    plotlyOutput("expression_plots_plots4",height="auto"),tags$h1(" "),
                    plotlyOutput("expression_plots_plots5",height="auto"),tags$h1(" "),
                    plotlyOutput("expression_plots_plots6",height="auto"),tags$h1(" "),
                    plotlyOutput("expression_plots_plots7",height="auto"),tags$h1(" "))
          } 
        }
      })
    })
    
    observeEvent(input$do_expression,{
      input$do_expression
      isolate({
        
        input_gene_expression <- input$gene_expression
        
        selected.database <- selecteddatabase_expression()
        if (nrow(selected.database)==0 | ncol(selected.database)<11) {
          output[["expression_plots_plots0"]] <- renderPlotly({
            plot_ly () %>% 
              layout(title = paste(input$data_source_expression,collapse = ","),font=list(size=15,family="Arial")) %>%
              add_annotations(x=2,y=2,text="no data available under this selection",showarrow=F,font=list(size=16,color="purple")) 
          })
        } else {
          selected.database <- data.frame(selected.database,Groups=selected.database$PFS.status)
          splitted_list <- selected.database %>% as.matrix %>% as.data.frame %>% split(.$data_source)
          splitted_list <- splitted_list[input$data_source_expression]
          for (k in 1:length(input$data_source_expression)) {
            local({
              my_i <- k
              expression_plots_plots_name <- paste0("expression_plots_plots", my_i)
              d<-splitted_list[[my_i]]
              
              output[[expression_plots_plots_name]] <- renderPlotly({
                if (is.na(names(splitted_list)[my_i])) {
                  plot_ly () %>% 
                    layout(title = input$data_source_expression[[my_i]],font=list(size=15,family="Arial")) %>%
                    add_annotations(x=2,y=2,text="no data available under this selection",showarrow=F,font=list(size=16,color="purple")) 
                } else {
                  if (nrow(d[!is.na(d[,"PFS.status"]) & !is.na(d[,11]),])==0 ) {
                    plot_ly () %>% 
                      layout(title = unique(d$data_source),font=list(size=15,family="Arial")) %>%
                      add_annotations(x=2,y=2,text="no immunotherapy response under this selection",showarrow=F,font=list(size=16,color="purple")) 
                  } else {
                    d <- d[!is.na(d[,"PFS.status"]) & !is.na(d[,11]),]
                    d[,"Groups"] <- as.character(d[,"Groups"])
                    d[d[,"Groups"]=="response" & !is.na(d[,"Groups"]),"Groups"] <- paste("response (n=",nrow(d[d[,"Groups"]=="response",]),")")
                    d[d[,"Groups"]=="nonresponse" & !is.na(d[,"Groups"]),"Groups"] <- paste("nonresponse (n=",nrow(d[d[,"Groups"]=="nonresponse",]),")")
                    ind <- unique(d[,"Groups"])
                    if (unique(d$data_source) == "Van Allen" | unique(d$data_source) == "Riaz" | unique(d$data_source) == "Auslander") {
                      lable_y <- " (RSEM)"
                    } else if (unique(d$data_source) == "Chen" | unique(d$data_source) == "Prat") {
                      lable_y <- " (NanoString signal)"
                    } else if (unique(d$data_source) == "Hugo") {
                      lable_y <- " (FPKM)"
                    } else {
                      lable_y <- NULL
                    }
                    try.test <- try(p.v <- wilcox.test(as.numeric(d[d[,"Groups"]==ind[1],"UserGene"]),as.numeric(d[d[,"Groups"]==ind[2],"UserGene"]))$p.value)
                    if (class(try.test)=="try-error") {
                      p.v <- "NA"
                    } else {
                      p.v <- wilcox.test(as.numeric(d[d[,"Groups"]==ind[1],"UserGene"]),as.numeric(d[d[,"Groups"]==ind[2],"UserGene"]))$p.value
                      if (p.v < 0.001) {
                        p.v <- "< 0.001"
                      } else {
                        p.v<-round(p.v,3)
                      } 
                    }
                    p<-plot_ly(data=d,x= ~Groups,
                               y= ~as.numeric(as.character(UserGene)), 
                               type="box",
                               boxpoints="all",
                               jitter=0.3,
                               pointpos=-1.8,
                               #mode="markders",
                               color= ~PFS.status,
                               colors=c("plum","lightgreen"),
                               #hoveron="points+boxes",
                               legendgroup=~PFS.status, showlegend=TRUE,
                               text= ~paste0("Sample ID: ", PatientID,
                                             "<br>Expression: ",input_gene_expression," :",UserGene,
                                             "<br>Treatment: ",treatment,
                                             "<br>Description: ",description)
                               
                    )  %>%
                      layout(title = unique(d$data_source),titlefont=list(size=20,color="black",family= "Arial"),
                             height = 480,
                             xaxis=list(title="Immunotherapy response",
                                        titlefont=list(size=20,color="black",family= "Aril"),
                                        showticklabels=TRUE,tickcolor="#444",linecolor = "#444"),
                             yaxis=list(title=paste(input_gene_expression, " expression value",lable_y,sep=""),
                                        titlefont=list(size=20,color="black",family= "Aril"),
                                        showticklabels=TRUE,tickmode="auto",tickcolor="#444",linecolor = "#444"
                             )) %>% 
                      add_annotations(x = 0.5,
                                      y = d[which.max(d[,"UserGene"]),"UserGene"],
                                      text = paste0("p-value = ",p.v),
                                      xref = paste0("x", 1),  # add this
                                      yref = paste0("y", 1),  # add this
                                      showarrow = FALSE,
                                      ax = 20,
                                      ay = -40)
                    p
                  } 
                  
                }
              })
              
              
            })
          }
        }
      }) 
    })
    expression_plots_download_data <- function() {
      input_gene_expression <- input$gene_expression
      m<-selecteddatabase_expression()
      if (input$gene_expression==""){
        m<-m[,c(1:7,10)]
        m
      } else {
        colnames(m)[11] <- input_gene_expression
        m<-m[,c(1:7,10:11)]
        m
      }
      
    }
    
    output$expression_plots_download <- downloadHandler(
      filename = function() {
        if (input$expression_plots_type == "Excel (CSV)") {
          "newfile.csv"
        } else if (input$expression_plots_type == "Text (TSV)") {
          "newfile.txt"
        } else if (input$expression_plots_type == "JSON") {
          "newfile.json"
        }
      },
      content = function(file) {
        if (input$expression_plots_type == "Excel (CSV)") {
          write.csv (expression_plots_download_data(),file,row.names = FALSE)
        } else if (input$expression_plots_type == "Text (TSV)") {
          write.table (expression_plots_download_data(),file,quote=F, row.names = FALSE, sep="\t")
        } else if (input$expression_plots_type == "JSON") {
          write(toJSON(expression_plots_download_data()),file)
        }
      }
    )
    output$expression_plots_table <- DT::renderDataTable({
      input$do_expression
      isolate({
        expression_plots_download_data()
      })
    })
    
    ################
    #### expression survival
    ################
    output$expression_survival_n_samples_NA <- renderText({
      input$do_expression
      isolate({
        nrow(selecteddatabase_expression())
      })
    })
    
    output$expression_survival_plots <- renderUI({
      input$do_expression
      isolate({
        selected.database <- selecteddatabase_expression()
        if (nrow(selected.database)==0 | ncol(selected.database)<11) {
          tagList(plotlyOutput("expression_survival_plots0",height="auto"))
        } else {
          if (length(input$data_source_expression)==1) {
            tagList(plotlyOutput("expression_survival_plots1",height="auto"),tags$h1(" "))
          } else if (length(input$data_source_expression)==2) {
            tagList(plotlyOutput("expression_survival_plots1",height="auto"),tags$h1(" "),
                    plotlyOutput("expression_survival_plots2",height="auto"),tags$h1(" "))
          } else if (length(input$data_source_expression)==3) {
            tagList(plotlyOutput("expression_survival_plots1",height="auto"),tags$h1(" "),
                    plotlyOutput("expression_survival_plots2",height="auto"),tags$h1(" "),
                    plotlyOutput("expression_survival_plots3",height="auto"),tags$h1(" "))
          } else if (length(input$data_source_expression)==4) {
            tagList(plotlyOutput("expression_survival_plots1",height="auto"),tags$h1(" "),
                    plotlyOutput("expression_survival_plots2",height="auto"),tags$h1(" "),
                    plotlyOutput("expression_survival_plots3",height="auto"),tags$h1(" "),
                    plotlyOutput("expression_survival_plots4",height="auto"),tags$h1(" "))
          } else if (length(input$data_source_expression)==5) {
            tagList(plotlyOutput("expression_survival_plots1",height="auto"),tags$h1(" "),
                    plotlyOutput("expression_survival_plots2",height="auto"),tags$h1(" "),
                    plotlyOutput("expression_survival_plots3",height="auto"),tags$h1(" "),
                    plotlyOutput("expression_survival_plots4",height="auto"),tags$h1(" "),
                    plotlyOutput("expression_survival_plots5",height="auto"),tags$h1(" "))
          } else if (length(input$data_source_expression)==6) {
            tagList(plotlyOutput("expression_survival_plots1",height="auto"),tags$h1(" "),
                    plotlyOutput("expression_survival_plots2",height="auto"),tags$h1(" "),
                    plotlyOutput("expression_survival_plots3",height="auto"),tags$h1(" "),
                    plotlyOutput("expression_survival_plots4",height="auto"),tags$h1(" "),
                    plotlyOutput("expression_survival_plots5",height="auto"),tags$h1(" "),
                    plotlyOutput("expression_survival_plots6",height="auto"),tags$h1(" "))
          } else if (length(input$data_source_expression)==7) {
            tagList(plotlyOutput("expression_survival_plots1",height="auto"),tags$h1(" "),
                    plotlyOutput("expression_survival_plots2",height="auto"),tags$h1(" "),
                    plotlyOutput("expression_survival_plots3",height="auto"),tags$h1(" "),
                    plotlyOutput("expression_survival_plots4",height="auto"),tags$h1(" "),
                    plotlyOutput("expression_survival_plots5",height="auto"),tags$h1(" "),
                    plotlyOutput("expression_survival_plots6",height="auto"),tags$h1(" "),
                    plotlyOutput("expression_survival_plots7",height="auto"),tags$h1(" "))
          } 
        }
      })
    })
    
    observeEvent(input$do_expression,{
      input$do_expression
      isolate({
        input_gene_expression <- input$gene_expression
        selected.database <- selecteddatabase_expression()
        if (nrow(selected.database)==0 | ncol(selected.database)<11) {
          output[["expression_survival_plots0"]] <- renderPlotly({
            plot_ly () %>% 
              layout(title = paste(input$data_source_expression,collapse = ","),font=list(size=15,family="Arial")) %>%
              add_annotations(x=2,y=2,text="no data available under this selection",showarrow=F,font=list(size=16,color="purple")) 
          })
        } else {
          splitted_list <- selected.database %>% as.matrix %>% as.data.frame %>% split(.$data_source)
          splitted_list <- splitted_list[input$data_source_expression]
          for (k in 1:length(input$data_source_expression)) {
            local({
              my_i <- k
              expression_survival_plots_name <- paste0("expression_survival_plots", my_i)
              d<-splitted_list[[my_i]]
              output[[expression_survival_plots_name]] <- renderPlotly({
                if (is.na(names(splitted_list)[my_i])) {
                  plot_ly () %>% 
                    layout(title = input$data_source_expression[[my_i]],font=list(size=15,family="Arial")) %>%
                    add_annotations(x=2,y=2,text="no data available under this selection",showarrow=F,font=list(size=16,color="purple")) 
                } else {
                  if (nrow(d[!is.na(d[,"OS.status"]) & !is.na(d[,"OS"]) & !is.na(d[,11]),])==0 ) {
                    plot_ly () %>% 
                      layout(title = unique(d$data_source),font=list(size=15,family="Arial")) %>%
                      add_annotations(x=2,y=2,text="no overall survival outcome under this selection",showarrow=F,font=list(size=16,color="purple")) 
                  } else {
                    d <- d[!is.na(d[,"OS.status"]) & !is.na(d[,"OS"]) & !is.na(d[,11]),]
                    d<-data.frame(d,dead=NA)
                    d[d[,"OS.status"]=="living" & !is.na(d[,"OS.status"]),"dead"] <- 0
                    d[d[,"OS.status"]=="deceased" & !is.na(d[,"OS.status"]),"dead"] <- 1
                    d <- d[!is.na(d[,"UserGene"]) & !is.na(d[,"dead"]),]
                    d <- d[d[,"dead"]==1 | d[,"dead"]==0,]
                    d<-data.frame(d[,1:6],unfactor(d[,c("dead","OS","PFS","Age","UserGene")]))
                    d<-data.frame(d,IND=paste("samples with bottom ",input$slider_expression_OS*100,"% ", 
                                              input_gene_expression," expression (",
                                              nrow(d[d[,"UserGene"]<= quantile(d[,"UserGene"],input$slider_expression_OS),]),")",
                                              sep=""))
                    d[,"IND"]<-as.character(d[,"IND"])
                    d[d[,"UserGene"]> quantile(d[,"UserGene"],input$slider_expression_OS),"IND"]<- 
                      paste("samples with top ",(1-input$slider_expression_OS)*100,"% ", input_gene_expression," expression (",
                            nrow(d[d[,"UserGene"]> quantile(d[,"UserGene"],input$slider_expression_OS),]),")",
                            sep="")
                    d<-d[order(d[,"IND"]),]
                    d[,"dead"] <- as.numeric(d[,"dead"])
                    fit.result <- survfit(Surv(OS,dead)~IND,data=d)
                    plot.result <- ggsurv(fit.result,surv.col = c("lightgreen","plum"))
                    diff.result <- survdiff(Surv(OS,dead)~IND,data=d) 
                    p.value= 1 - pchisq(diff.result$chisq, length(diff.result$n) - 1)
                    plot.result.ann <- plot.result + theme(legend.position = "right", legend.title = element_blank()) + 
                      ggplot2::geom_text(aes(label = sprintf("p-value = %0.2g",
                                                             pchisq(diff.result$chisq,df=1,lower.tail = F)),x=200,y=0.05)) +
                      ggplot2::guides(color=FALSE,linetype=FALSE)
                    p1<-ggplotly(plot.result.ann,legendgroup=~IND,
                                 height = 480 ) %>%
                      layout(title = unique(d$data_source),titlefont=list(size=20,color="black",family= "Arial"),
                             xaxis=list(title="Days",titlefont=list(size=20,color="black",family= "Aril black")),
                             yaxis=list(title="Overall survival proportion",titlefont=list(size=20,color="black",family= "Aril black")))
                    p1
                  }
                }
              })
            })
          }
        }
      })
    })
    
    output$expression_survival_download <- downloadHandler(
      filename = function() {
        if (input$expression_survival_type == "Excel (CSV)") {
          "newfile.csv"
        } else if (input$expression_survival_type == "Text (TSV)") {
          "newfile.txt"
        } else if (input$expression_survival_type == "JSON") {
          "newfile.json"
        }
      },
      content = function(file) {
        if (input$expression_survival_type == "Excel (CSV)") {
          write.csv (expression_survival_download_data(),file,row.names = FALSE)
        } else if (input$expression_survival_type == "Text (TSV)") {
          write.table (expression_survival_download_data(),file,quote=F, row.names = FALSE, sep="\t")
        } else if (input$expression_survival_type == "JSON") {
          write(toJSON(expression_survival_download_data()),file)
        }
      }
    )
    expression_survival_download_data <- function() {
      input_gene_expression <- input$gene_expression
      m<-selecteddatabase_expression()
      #    m <- m[!is.na(m[,"UserGene"]),]
      if (nrow(m)>0 & ncol(m)>10) {
        m <- data.frame(m,IND=NA)
        m1 <- m %>% split(.$data_source) %>% 
          lapply(function(d) {#d[d[,"UserGene"]>=median(na.omit(d[,"UserGene"])),"IND"] <- paste("top 50% ", input$gene_expression," expression",sep="")
            #d[d[,"UserGene"]<median(na.omit(d[,"UserGene"])),"IND"] <- paste("bottom 50% ", input$gene_expression," expression",sep="")
            #d
            d[!is.na(d[,"UserGene"]) & d[,"UserGene"]>=quantile(na.omit(d[,"UserGene"]),input$slider_expression_OS),"IND"] <- paste("top ",(1-input$slider_expression_OS)*100,"% ", input$gene_expression," expression",sep="")
            d[!is.na(d[,"UserGene"]) & d[,"UserGene"]<quantile(na.omit(d[,"UserGene"]),input$slider_expression_OS),"IND"] <- paste("bottom ",input$slider_expression_OS*100,"% ", input$gene_expression," expression",sep="")
            d})
        m2<-ldply(m1,data.frame)
        m2<-m2[,-1]
        #    m[!is.na(m[,"UserGene"]) & m[,"UserGene"]>=median(na.omit(m[,"UserGene"])),"IND"] <- paste("top 50% ", input$gene_expression," expression",sep="")
        #    m[!is.na(m[,"UserGene"]) & m[,"UserGene"]<median(na.omit(m[,"UserGene"])),"IND"] <- paste("bottom 50% ", input$gene_expression," expression",sep="")
        if (input_gene_expression==""){
          m2[,c(1:8,10:11)]
        } else {
          colnames(m2)[11] <- input_gene_expression
          m2[,c(1:8,10:12)]
        }
        #m2<-m2[,c(1:8,10:12)]
        #m2
      } else {
        #    m <- data.frame(m,IND=NA) #remove "IND" column, because "Error in [.data.frame: undefined columns selected"
        m2<-m
        if (input_gene_expression==""){
          #colnames(m2)[11] <- "PUM2"
          m2[,c(1:8,10)]
        } else {
          colnames(m2)[11] <- input_gene_expression
          m2[,c(1:8,10:11)]
        }
        #m2<-m2[,c(1:8,10:11)] # can't select 12 clumn
        #m2
      }
    }
    output$expression_survival_table <- DT::renderDataTable({
#      input$do_expression
#      isolate({
        expression_survival_download_data()
#      })
    })
    
    ###############
    #### expression PFS
    ###############
    output$expression_PFS_n_samples_NA <- renderText({
      input$do_expression
      isolate({
        nrow(selecteddatabase_expression())
      })
    })
    
    output$expression_PFS_plots <- renderUI({
      input$do_expression
      isolate({
        selected.database <- selecteddatabase_expression()
        if (nrow(selected.database)==0 | ncol(selected.database)<11) {
          tagList(plotlyOutput("expression_PFS_plots0",height="auto"))
        } else {
          if (length(input$data_source_expression)==1) {
            tagList(plotlyOutput("expression_PFS_plots1",height="auto"),tags$h1(" "))
          } else if (length(input$data_source_expression)==2) {
            tagList(plotlyOutput("expression_PFS_plots1",height="auto"),tags$h1(" "),
                    plotlyOutput("expression_PFS_plots2",height="auto"),tags$h1(" "))
          } else if (length(input$data_source_expression)==3) {
            tagList(plotlyOutput("expression_PFS_plots1",height="auto"),tags$h1(" "),
                    plotlyOutput("expression_PFS_plots2",height="auto"),tags$h1(" "),
                    plotlyOutput("expression_PFS_plots3",height="auto"),tags$h1(" "))
          } else if (length(input$data_source_expression)==4) {
            tagList(plotlyOutput("expression_PFS_plots1",height="auto"),tags$h1(" "),
                    plotlyOutput("expression_PFS_plots2",height="auto"),tags$h1(" "),
                    plotlyOutput("expression_PFS_plots3",height="auto"),tags$h1(" "),
                    plotlyOutput("expression_PFS_plots4",height="auto"),tags$h1(" "))
          } else if (length(input$data_source_expression)==5) {
            tagList(plotlyOutput("expression_PFS_plots1",height="auto"),tags$h1(" "),
                    plotlyOutput("expression_PFS_plots2",height="auto"),tags$h1(" "),
                    plotlyOutput("expression_PFS_plots3",height="auto"),tags$h1(" "),
                    plotlyOutput("expression_PFS_plots4",height="auto"),tags$h1(" "),
                    plotlyOutput("expression_PFS_plots5",height="auto"),tags$h1(" "))
          } else if (length(input$data_source_expression)==6) {
            tagList(plotlyOutput("expression_PFS_plots1",height="auto"),tags$h1(" "),
                    plotlyOutput("expression_PFS_plots2",height="auto"),tags$h1(" "),
                    plotlyOutput("expression_PFS_plots3",height="auto"),tags$h1(" "),
                    plotlyOutput("expression_PFS_plots4",height="auto"),tags$h1(" "),
                    plotlyOutput("expression_PFS_plots5",height="auto"),tags$h1(" "),
                    plotlyOutput("expression_PFS_plots6",height="auto"),tags$h1(" "))
          } else if (length(input$data_source_expression)==7) {
            tagList(plotlyOutput("expression_PFS_plots1",height="auto"),tags$h1(" "),
                    plotlyOutput("expression_PFS_plots2",height="auto"),tags$h1(" "),
                    plotlyOutput("expression_PFS_plots3",height="auto"),tags$h1(" "),
                    plotlyOutput("expression_PFS_plots4",height="auto"),tags$h1(" "),
                    plotlyOutput("expression_PFS_plots5",height="auto"),tags$h1(" "),
                    plotlyOutput("expression_PFS_plots6",height="auto"),tags$h1(" "),
                    plotlyOutput("expression_PFS_plots7",height="auto"),tags$h1(" "))
          }
        }
      })
    }) 
    
    observeEvent(input$do_expression,{
      input$do_expression
      isolate({
        input_gene_expression <- input$gene_expression
        selected.database <- selecteddatabase_expression()
        if (nrow(selected.database)==0 | ncol(selected.database)<11) {
          output[["expression_PFS_plots0"]] <- renderPlotly({
            plot_ly () %>% 
              layout(title = paste(input$data_source_expression,collapse = ","),font=list(size=15,family="Arial")) %>%
              add_annotations(x=2,y=2,text="no data available under this selection",showarrow=F,font=list(size=16,color="purple")) 
          })
        } else {
          splitted_list <- selected.database %>% as.matrix %>% as.data.frame %>% split(.$data_source)
          splitted_list <- splitted_list[input$data_source_expression]
          for (k in 1:length(input$data_source_expression)) {
            local({
              my_i <- k
              expression_PFS_plots_name <- paste0("expression_PFS_plots", my_i)
              d<-splitted_list[[my_i]]
              output[[expression_PFS_plots_name]] <- renderPlotly({
                if (is.na(names(splitted_list)[my_i])) {
                  plot_ly () %>% 
                    layout(title = input$data_source_expression[[my_i]],font=list(size=15,family="Arial")) %>%
                    add_annotations(x=2,y=2,text="no data available under this selection",showarrow=F,font=list(size=16,color="purple")) 
                } else {
                  if (nrow(d[!is.na(d[,"PFS.status"]) & !is.na(d[,"PFS"]) & !is.na(d[,11]),])==0 ) {
                    plot_ly () %>% 
                      layout(title = unique(d$data_source),font=list(size=15,family="Arial")) %>%
                      add_annotations(x=2,y=2,text="no PFS outcome under this selection",showarrow=F,font=list(size=16,color="purple")) 
                  } else {
                    d <- d[!is.na(d[,"PFS.status"]) & !is.na(d[,"PFS"]) & !is.na(d[,11]),]
                    d<-data.frame(d[,1:6],unfactor(d[,c("OS.status","OS","PFS","Age","UserGene")]))
                    d<-data.frame(d,IND=paste("samples with bottom ",input$slider_expression_PFS*100,"% ", 
                                              input_gene_expression," expression (",
                                              nrow(d[d[,"UserGene"]<= quantile(d[,"UserGene"],input$slider_expression_PFS),]),")",
                                              sep=""))
                    d[,"IND"]<-as.character(d[,"IND"])
                    d[d[,"UserGene"]> quantile(d[,"UserGene"],input$slider_expression_PFS),"IND"]<- 
                      paste("samples with top ",(1-input$slider_expression_PFS)*100,"% ", input_gene_expression," expression (",
                            nrow(d[d[,"UserGene"]> quantile(d[,"UserGene"],input$slider_expression_PFS),]),")",
                            sep="")
                    d <- data.frame(d,group.IND=0)
                    d[d[,"PFS.status"]=="nonresponse","group.IND"] <- 1
                    d<-d[order(d[,"IND"]),]
                    fit.result <- survfit(Surv(PFS,group.IND)~IND,data=d)
                    plot.result <- ggsurv(fit.result,surv.col = c("lightgreen","plum"))
                    diff.result <- survdiff(Surv(PFS,group.IND)~IND,data=d) 
                    p.value= 1 - pchisq(diff.result$chisq, length(diff.result$n) - 1)
                    plot.result.ann <- plot.result + theme(legend.position = "right", legend.title = element_blank()) + 
                      ggplot2::geom_text(aes(label = sprintf("p-value = %0.2g",
                                                             pchisq(diff.result$chisq,df=1,lower.tail = F)),x=200,y=0.05)) +
                      ggplot2::guides(color=FALSE,linetype=FALSE)
                    p1<-ggplotly(plot.result.ann,legendgroup=~IND,
                                 height = 480 ) %>%
                      layout(title = unique(d$data_source),titlefont=list(size=20,color="black",family= "Arial"),
                             xaxis=list(title="Days",titlefont=list(size=20,color="black",family= "Aril black")),
                             yaxis=list(title="Progression free survival proportion",titlefont=list(size=20,color="black",family= "Aril black")))
                    p1
                  }
                }
              })
            })
          }
        }
      })
    })
    
    output$expression_PFS_download <- downloadHandler(
      filename = function() {
        if (input$expression_PFS_type == "Excel (CSV)") {
          "newfile.csv"
        } else if (input$expression_PFS_type == "Text (TSV)") {
          "newfile.txt"
        } else if (input$expression_PFS_type == "JSON") {
          "newfile.json"
        }
      },
      content = function(file) {
        if (input$expression_PFS_type == "Excel (CSV)") {
          write.csv (expression_PFS_download_data(),file,row.names = FALSE)
        } else if (input$expression_PFS_type == "Text (TSV)") {
          write.table (expression_PFS_download_data(),file,quote=F, row.names = FALSE, sep="\t")
        } else if (input$expression_PFS_type == "JSON") {
          write(toJSON(expression_PFS_download_data()),file)
        }
      }
    )
    expression_PFS_download_data <- function() {
      input_gene_expression <- input$gene_expression
      m<-selecteddatabase_expression()
      #    m <- m[!is.na(m[,"UserGene"]),]
      if (nrow(m)>0 & ncol(m)>10) {
        m <- data.frame(m,IND=NA)
        m1 <- m %>% split(.$data_source) %>% 
          lapply(function(d) {#d[d[,"UserGene"]>=median(na.omit(d[,"UserGene"])),"IND"] <- paste("top 50% ", input$gene_expression," expression",sep="")
            #d[d[,"UserGene"]<median(na.omit(d[,"UserGene"])),"IND"] <- paste("bottom 50% ", input$gene_expression," expression",sep="")
            #d
            d[!is.na(d[,"UserGene"]) & d[,"UserGene"]>=quantile(na.omit(d[,"UserGene"]),input$slider_expression_PFS),"IND"] <- paste("top ",(1-input$slider_expression_PFS)*100,"% ", input_gene_expression," expression",sep="")
            d[!is.na(d[,"UserGene"]) & d[,"UserGene"]<quantile(na.omit(d[,"UserGene"]),input$slider_expression_PFS),"IND"] <- paste("bottom ",input$slider_expression_PFS*100,"% ", input_gene_expression," expression",sep="")
            d})
        m2<-ldply(m1,data.frame)
        m2<-m2[,-1]
        #    m[!is.na(m[,"UserGene"]) & m[,"UserGene"]>=median(na.omit(m[,"UserGene"])),"IND"] <- paste("top 50% ", input$gene_expression," expression",sep="")
        #    m[!is.na(m[,"UserGene"]) & m[,"UserGene"]<median(na.omit(m[,"UserGene"])),"IND"] <- paste("bottom 50% ", input$gene_expression," expression",sep="")
        if (input_gene_expression==""){
          #colnames(m2)[11] <- "PUM2"
          m2[,c(1:6,9:11)]
        } else {
          colnames(m2)[11] <- input_gene_expression
          m2[,c(1:6,9:12)]
        }
        #m2<-m2[,c(1:6,9:12)]
        #m2
      } else {
        #      m <- data.frame(m,IND=NA) #remove "IND" column, because "Error in [.data.frame: undefined columns selected"
        m2<-m
        if (input_gene_expression==""){
          #colnames(m2)[11] <- "PUM2"
          m2[,c(1:6,9:10)]
        } else {
          colnames(m2)[11] <- input_gene_expression
          m2[,c(1:6,9:11)]
        }
        #m2<-m2[,c(1:6,9:11)] #can not select 12 column
        #m2
      }
    }
    output$expression_PFS_table <- DT::renderDataTable({
#      input$do_expression
#      isolate({
        expression_PFS_download_data()
#      })
    })
    ###########################################
    #### expression sum
    ###########################################
    output$data_source_expression_sum <- renderUI({
      all.data.source <- as.character(unique(database.test.process()[,"data_source"]))
      user.data.source <- all.data.source[!(all.data.source %in% c("Robert","Snyder","Moshe","Auslander","Chen","Hugo","Prat","Riaz","Van Allen","Krieg_panel_1","Krieg_panel_2","Krieg_panel_3"))]
      selectInput("data_source_expression_sum",
                  "Data source: ",
                  choices = c("Auslander","Chen","Hugo","Prat","Riaz","Van Allen",user.data.source),
                  multiple = TRUE,
                  selected = c("Van Allen","Riaz","Chen"))})
    output$treatment_expression_sum <- renderUI ({
      selectInput("treatment_expression_sum",
                  "Treatment: ",
                  #              choices = as.character(unique(filter_input_interactive(filter_input=input$data_source_expression_sum)[,"treatment"])),
                  choices = as.character(unique(database.test.process()[,"treatment"])),
                  multiple = TRUE,
                  selected = c("anti-PD-1","anti-CTLA-4","anti-PD-1+anti-CTLA-4"))})
    output$description_expression_sum <- renderUI ({
      selectInput("description_expression_sum",
                  "Pre- or On-treatment: ",
                  #              choices = as.character(unique(filter_input_interactive(filter_input=c(input$data_source_expression_sum,input$treatment_expression_sum))[,"description"])),
                  choices = as.character(unique(database.test.process()[,"description"])),
                  multiple = TRUE,
                  selected = c("Pre","On"))})
    #  output$gene_expression_sum <- renderUI({
    updateSelectizeInput(session,"gene_expression_sum",
                         choices=a.colnames,selected=c("CD4","CD6","CD8A","CD8B"),
                         server=TRUE)
    #    })
    output$slider_expression_sum_OS <- renderUI({
      sliderInput("slider_expression_sum_OS",
                  "Seperate samples with gene expression sum cutoff (%): ",
                  min=0,max=1,value=0.5)  })
    output$slider_expression_sum_PFS <- renderUI({
      sliderInput("slider_expression_sum_PFS",
                  "Seperate samples with gene expression sum cutoff (%): ",
                  min=0,max=1,value=0.5)  })
    selecteddatabase_expression_sum <- eventReactive(input$do_expression_sum,{
      # Due to dplyr issue #318, we need temp variables for input values
      #    if (input$data_source_expression_sum=="ALL") {
      #      data_source1 <- c("Auslander","Chen","Hugo","Prat","Riaz","Van Allen")
      #    } else {
      data_source1 <- input$data_source_expression_sum
      #    }
      
      treatment1 <- input$treatment_expression_sum
      description1 <- input$description_expression_sum
      if (is.null(input$gene_expression_sum)){
        genes1 <- c("PatientID","data_source","treatment","description","PFS.status","RECIST","OS.status","OS","PFS","Age")
      } else {
        #    genessum <- unlist(str_split(input$gene_expression_sum,","))
        genessum <- input$gene_expression_sum
        genes1 <- c("PatientID","data_source","treatment","description","PFS.status","RECIST","OS.status","OS","PFS","Age",genessum)
      }
      
      
      # Apply filters
      #  m <- database.test.process() %>% 
      #    filter(
      #      data_source %in% data_source1,
      #      treatment %in% treatment1,
      #      description %in% description1
      #    ) %>%
      #    select(genes1)
      a<-data.table(database.test.process())
      m<-a[data_source %in% data_source1 & treatment %in% treatment1 & description %in% description1]
      m<-m[,colnames(m) %in% genes1,with=FALSE]
      as.data.frame(m)
    })
    
    #############
    #### expression sum plot
    #############
    output$expression_sum_plots_n_samples_NA <- renderText({
      input$do_expression_sum
      isolate({
        nrow(selecteddatabase_expression_sum())
      })
    })
    
    output$expression_sum_plots_plots <- renderUI({
      input$do_expression_sum
      isolate({
        input_gene_expression_sum <- input$gene_expression_sum
        selected.database <- selecteddatabase_expression_sum()
        if (nrow(selected.database)==0 | ncol(selected.database)<11) {
          tagList(plotlyOutput("expression_sum_plots_plots0",height="auto"))
        } else {
          if (length(input$data_source_expression_sum)==1) {
            tagList(plotlyOutput("expression_sum_plots_plots1",height="auto"),tags$h1(" "))
          } else if (length(input$data_source_expression_sum)==2) {
            tagList(plotlyOutput("expression_sum_plots_plots1",height="auto"),tags$h1(" "),
                    plotlyOutput("expression_sum_plots_plots2",height="auto"),tags$h1(" "))
          } else if (length(input$data_source_expression_sum)==3) {
            tagList(plotlyOutput("expression_sum_plots_plots1",height="auto"),tags$h1(" "),
                    plotlyOutput("expression_sum_plots_plots2",height="auto"),tags$h1(" "),
                    plotlyOutput("expression_sum_plots_plots3",height="auto"),tags$h1(" "))
          } else if (length(input$data_source_expression_sum)==4) {
            tagList(plotlyOutput("expression_sum_plots_plots1",height="auto"),tags$h1(" "),
                    plotlyOutput("expression_sum_plots_plots2",height="auto"),tags$h1(" "),
                    plotlyOutput("expression_sum_plots_plots3",height="auto"),tags$h1(" "),
                    plotlyOutput("expression_sum_plots_plots4",height="auto"),tags$h1(" "))
          } else if (length(input$data_source_expression_sum)==5) {
            tagList(plotlyOutput("expression_sum_plots_plots1",height="auto"),tags$h1(" "),
                    plotlyOutput("expression_sum_plots_plots2",height="auto"),tags$h1(" "),
                    plotlyOutput("expression_sum_plots_plots3",height="auto"),tags$h1(" "),
                    plotlyOutput("expression_sum_plots_plots4",height="auto"),tags$h1(" "),
                    plotlyOutput("expression_sum_plots_plots5",height="auto"),tags$h1(" "))
          } else if (length(input$data_source_expression_sum)==6) {
            tagList(plotlyOutput("expression_sum_plots_plots1",height="auto"),tags$h1(" "),
                    plotlyOutput("expression_sum_plots_plots2",height="auto"),tags$h1(" "),
                    plotlyOutput("expression_sum_plots_plots3",height="auto"),tags$h1(" "),
                    plotlyOutput("expression_sum_plots_plots4",height="auto"),tags$h1(" "),
                    plotlyOutput("expression_sum_plots_plots5",height="auto"),tags$h1(" "),
                    plotlyOutput("expression_sum_plots_plots6",height="auto"),tags$h1(" "))
          } else if (length(input$data_source_expression_sum)==7) {
            tagList(plotlyOutput("expression_sum_plots_plots1",height="auto"),tags$h1(" "),
                    plotlyOutput("expression_sum_plots_plots2",height="auto"),tags$h1(" "),
                    plotlyOutput("expression_sum_plots_plots3",height="auto"),tags$h1(" "),
                    plotlyOutput("expression_sum_plots_plots4",height="auto"),tags$h1(" "),
                    plotlyOutput("expression_sum_plots_plots5",height="auto"),tags$h1(" "),
                    plotlyOutput("expression_sum_plots_plots6",height="auto"),tags$h1(" "),
                    plotlyOutput("expression_sum_plots_plots7",height="auto"),tags$h1(" "))
          } 
        }
      })
    })
    
    observeEvent(input$do_expression_sum,{
      input$do_expression_sum
      isolate({
        input_gene_expression_sum <- input$gene_expression_sum
        selected.database <- selecteddatabase_expression_sum()
        if (nrow(selected.database)==0 | ncol(selected.database)<11) {
          output[["expression_sum_plots_plots0"]] <- renderPlotly({
            plot_ly () %>% 
              layout(title = paste(input$data_source_expression_sum,collapse = ","),font=list(size=15,family="Arial")) %>%
              add_annotations(x=2,y=2,text="no data available under this selection",showarrow=F,font=list(size=16,color="purple")) 
          })
        } else {
          selected.database <- data.frame(selected.database,Groups=selected.database$PFS.status)
          splitted_list <- selected.database %>% as.matrix %>% as.data.frame %>% split(.$data_source)
          splitted_list <- splitted_list[input$data_source_expression_sum]
          for (k in 1:length(input$data_source_expression_sum)) {
            local({
              my_i <- k
              expression_sum_plots_plots_name <- paste0("expression_sum_plots_plots", my_i)
              d<-splitted_list[[my_i]]
              output[[expression_sum_plots_plots_name]] <- renderPlotly({
                if (is.na(names(splitted_list)[my_i])) {
                  plot_ly () %>% 
                    layout(title = input$data_source_expression_sum[[my_i]],font=list(size=15,family="Arial")) %>%
                    add_annotations(x=2,y=2,text="no data available under this selection",showarrow=F,font=list(size=16,color="purple")) 
                } else {
                  #d[,11:ncol(d)] <- unfactor(d[,11:ncol(d)])
                  d <- data.frame(d,sum=apply(d[,11:ncol(d)],1,function(x) sum(na.omit(as.numeric(x)))))
                  d[d[,"sum"]==0,"sum"]<-NA
                  if (nrow(d[!is.na(d[,"PFS.status"]) & !is.na(d[,"sum"]),])==0 ) {
                    plot_ly () %>% 
                      layout(title = unique(d$data_source),font=list(size=15,family="Arial")) %>%
                      add_annotations(x=2,y=2,text="no immunotherapy response under this selection",showarrow=F,font=list(size=16,color="purple")) 
                  } else {
                    d <- d[!is.na(d[,"PFS.status"]) & !is.na(d[,"sum"]),]
                    d[,"Groups"] <- as.character(d[,"Groups"])
                    d[d[,"Groups"]=="response" & !is.na(d[,"Groups"]),"Groups"] <- paste("response (n=",nrow(d[d[,"Groups"]=="response",]),")")
                    d[d[,"Groups"]=="nonresponse" & !is.na(d[,"Groups"]),"Groups"] <- paste("nonresponse (n=",nrow(d[d[,"Groups"]=="nonresponse",]),")")
                    ind <- unique(d[,"Groups"])
                    if (unique(d$data_source) == "Van Allen" | unique(d$data_source) == "Riaz" | unique(d$data_source) == "Auslander") {
                      lable_y <- " (RSEM)"
                    } else if (unique(d$data_source) == "Chen" | unique(d$data_source) == "Prat") {
                      lable_y <- " (NanoString signal)"
                    } else if (unique(d$data_source) == "Hugo") {
                      lable_y <- " (FPKM)"
                    }
                    try.test <- try(p.v <- wilcox.test(as.numeric(d[d[,"Groups"]==ind[1],"sum"]),as.numeric(d[d[,"Groups"]==ind[2],"sum"]))$p.value)
                    if (class(try.test)=="try-error") {
                      p.v <- "NA"
                    } else {
                      p.v <- wilcox.test(as.numeric(d[d[,"Groups"]==ind[1],"sum"]),as.numeric(d[d[,"Groups"]==ind[2],"sum"]))$p.value
                      if (p.v < 0.001) {
                        p.v <- "< 0.001"
                      } else {
                        p.v<-round(p.v,3)
                      } 
                    }
                    p<-plot_ly(data=d,x= ~Groups,
                               y= ~as.numeric(as.character(sum)), 
                               type="box",
                               boxpoints="all",
                               jitter=0.3,
                               pointpos=-1.8,
                               #mode="markders",
                               color= ~PFS.status,
                               colors=c("plum","lightgreen"),
                               #hoveron="points+boxes",
                               legendgroup=~PFS.status,showlegend=TRUE,
                               text= ~paste0("Sample ID: ", PatientID,
                                             "<br>Expression: ",sum,
                                             "<br>Treatment: ",treatment,
                                             "<br>Description: ",description)
                               
                    )  %>%
                      layout(title = unique(d$data_source),titlefont=list(size=20,color="black",family= "Arial"),
                             height = 480,
                             xaxis=list(title="Immunotherapy response",titlefont=list(size=20,color="black",family= "Aril"),
                                        showticklabels=TRUE,tickcolor="#444",linecolor = "#444"),
                             yaxis=list(title=paste0("Sum of selected expression",
                                                     lable_y),
                                        titlefont=list(size=20,color="black",family= "Aril"),
                                        showticklabels=TRUE,tickmode="auto",tickcolor="#444",linecolor = "#444"
                             )) %>% 
                      add_annotations(x = 0.5,
                                      y = d[which.max(d[,"sum"]),"sum"],
                                      text = paste0("p-value = ",p.v),
                                      xref = paste0("x", 1),  # add this
                                      yref = paste0("y", 1),  # add this
                                      showarrow = FALSE,
                                      ax = 20,
                                      ay = -40)
                    p
                  } 
                  
                }
              })
            })
          }
        }
      })
    })
    
    
    
    output$expression_sum_plots_download <- downloadHandler(
      filename = function() {
        if (input$expression_sum_plots_type == "Excel (CSV)") {
          "newfile.csv"
        } else if (input$expression_sum_plots_type == "Text (TSV)") {
          "newfile.txt"
        } else if (input$expression_sum_plots_type == "JSON") {
          "newfile.json"
        }
      },
      content = function(file) {
        if (input$expression_sum_plots_type == "Excel (CSV)") {
          write.csv (expression_sum_plots_download_data(),file,row.names = FALSE)
        } else if (input$expression_sum_plots_type == "Text (TSV)") {
          write.table (expression_sum_plots_download_data(),file,quote=F, row.names = FALSE, sep="\t")
        } else if (input$expression_sum_plots_type == "JSON") {
          write(toJSON(expression_sum_plots_download_data()),file)
        }
      }
    )
    expression_sum_plots_download_data <- function() {
      m<-selecteddatabase_expression_sum()
      if (nrow(m)>0 & ncol(m)>10) {
        m <- data.frame(m,sum=apply(m[,11:ncol(m)],1,function(x) sum(na.omit(as.numeric(x)))))
        m[m[,"sum"]==0,"sum"]<-NA
        m<-m[,-c(8:9)]
        m
      } else {
        m<-m[,-c(8:9)]
        m
      }
    }
    output$expression_sum_plots_table <- DT::renderDataTable({
      input$do_expression_sum
      isolate({
        expression_sum_plots_download_data()
      })
    })
    
    ############
    #### expression sum survival
    ############
    output$expression_sum_survival_n_samples_NA <- renderText({
      input$do_expression_sum
      isolate({
        nrow(selecteddatabase_expression_sum())
      })
    })
    
    output$expression_sum_survival_plots <- renderUI({
      input$do_expression_sum
      isolate({
        input_gene_expression_sum <- input$gene_expression_sum
        selected.database <- selecteddatabase_expression_sum()
        if (nrow(selected.database)==0 | ncol(selected.database)<11) {
          tagList(plotlyOutput("expression_sum_survival_plots0",height="auto"))
        } else {
          if (length(input$data_source_expression_sum)==1) {
            tagList(plotlyOutput("expression_sum_survival_plots1",height="auto"),tags$h1(" "))
          } else if (length(input$data_source_expression_sum)==2) {
            tagList(plotlyOutput("expression_sum_survival_plots1",height="auto"),tags$h1(" "),
                    plotlyOutput("expression_sum_survival_plots2",height="auto"),tags$h1(" "))
          } else if (length(input$data_source_expression_sum)==3) {
            tagList(plotlyOutput("expression_sum_survival_plots1",height="auto"),tags$h1(" "),
                    plotlyOutput("expression_sum_survival_plots2",height="auto"),tags$h1(" "),
                    plotlyOutput("expression_sum_survival_plots3",height="auto"),tags$h1(" "))
          } else if (length(input$data_source_expression_sum)==4) {
            tagList(plotlyOutput("expression_sum_survival_plots1",height="auto"),tags$h1(" "),
                    plotlyOutput("expression_sum_survival_plots2",height="auto"),tags$h1(" "),
                    plotlyOutput("expression_sum_survival_plots3",height="auto"),tags$h1(" "),
                    plotlyOutput("expression_sum_survival_plots4",height="auto"),tags$h1(" "))
          } else if (length(input$data_source_expression_sum)==5) {
            tagList(plotlyOutput("expression_sum_survival_plots1",height="auto"),tags$h1(" "),
                    plotlyOutput("expression_sum_survival_plots2",height="auto"),tags$h1(" "),
                    plotlyOutput("expression_sum_survival_plots3",height="auto"),tags$h1(" "),
                    plotlyOutput("expression_sum_survival_plots4",height="auto"),tags$h1(" "),
                    plotlyOutput("expression_sum_survival_plots5",height="auto"),tags$h1(" "))
          } else if (length(input$data_source_expression_sum)==6) {
            tagList(plotlyOutput("expression_sum_survival_plots1",height="auto"),tags$h1(" "),
                    plotlyOutput("expression_sum_survival_plots2",height="auto"),tags$h1(" "),
                    plotlyOutput("expression_sum_survival_plots3",height="auto"),tags$h1(" "),
                    plotlyOutput("expression_sum_survival_plots4",height="auto"),tags$h1(" "),
                    plotlyOutput("expression_sum_survival_plots5",height="auto"),tags$h1(" "),
                    plotlyOutput("expression_sum_survival_plots6",height="auto"),tags$h1(" "))
          } else if (length(input$data_source_expression_sum)==7) {
            tagList(plotlyOutput("expression_sum_survival_plots1",height="auto"),tags$h1(" "),
                    plotlyOutput("expression_sum_survival_plots2",height="auto"),tags$h1(" "),
                    plotlyOutput("expression_sum_survival_plots3",height="auto"),tags$h1(" "),
                    plotlyOutput("expression_sum_survival_plots4",height="auto"),tags$h1(" "),
                    plotlyOutput("expression_sum_survival_plots5",height="auto"),tags$h1(" "),
                    plotlyOutput("expression_sum_survival_plots6",height="auto"),tags$h1(" "),
                    plotlyOutput("expression_sum_survival_plots7",height="auto"),tags$h1(" "))
          } 
        }
      })
    })
    observeEvent(input$do_expression_sum,{
      input$do_expression_sum
      isolate({
        input_gene_expression_sum <- input$gene_expression_sum
        selected.database <- selecteddatabase_expression_sum()
        if (nrow(selected.database)==0 | ncol(selected.database)<11) {
          output[["expression_sum_survival_plots0"]] <- renderPlotly({
            plot_ly () %>% 
              layout(title = paste(input$data_source_expression_sum,collapse = ","),font=list(size=15,family="Arial")) %>%
              add_annotations(x=2,y=2,text="no data available under this selection",showarrow=F,font=list(size=16,color="purple")) 
          })
        } else {
          splitted_list <- selected.database %>% as.matrix %>% as.data.frame %>% split(.$data_source)
          splitted_list <- splitted_list[input$data_source_expression_sum]
          for (k in 1:length(input$data_source_expression_sum)) {
            local({
              my_i <- k
              expression_sum_survival_plots_name <- paste0("expression_sum_survival_plots", my_i)
              d<-splitted_list[[my_i]]
              output[[expression_sum_survival_plots_name]] <- renderPlotly({
                if (is.na(names(splitted_list)[my_i])) {
                  plot_ly () %>% 
                    layout(title = input$data_source_expression_sum[[my_i]],font=list(size=15,family="Arial")) %>%
                    add_annotations(x=2,y=2,text="no data available under this selection",showarrow=F,font=list(size=16,color="purple")) 
                } else {
                  d <- data.frame(d,sum=apply(d[,11:ncol(d)],1,function(x) sum(na.omit(as.numeric(x)))))
                  d[d[,"sum"]==0,"sum"]<-NA
                  if (nrow(d[!is.na(d[,"OS.status"]) & !is.na(d[,"OS"]) & !is.na(d[,"sum"]),])==0 ) {
                    plot_ly () %>% 
                      layout(title = unique(d$data_source),font=list(size=15,family="Arial")) %>%
                      add_annotations(x=2,y=2,text="no overall survival outcome under this selection",showarrow=F,font=list(size=16,color="purple")) 
                  } else {
                    d <- d[!is.na(d[,"OS.status"]) & !is.na(d[,"OS"]) & !is.na(d[,"sum"]),]
                    d<-data.frame(d,dead=NA)
                    d[d[,"OS.status"]=="living" & !is.na(d[,"OS.status"]),"dead"] <- 0
                    d[d[,"OS.status"]=="deceased" & !is.na(d[,"OS.status"]),"dead"] <- 1
                    d <- d[!is.na(d[,"sum"]) & !is.na(d[,"dead"]),]
                    d <- d[d[,"dead"]==1 | d[,"dead"]==0,]
                    d<-data.frame(d[,1:6],unfactor(d[,c("dead","OS","PFS","Age","sum")]))
                    d<-data.frame(d,IND=paste0("samples with bottom ",input$slider_expression_sum_OS*100,"% expression sum (",
                                               nrow(d[d[,"sum"]<= quantile(d[,"sum"],input$slider_expression_sum_OS),]),")"))
                    d[,"IND"]<-as.character(d[,"IND"])
                    d[d[,"sum"]>=quantile(d[,"sum"],input$slider_expression_sum_OS),"IND"] <- 
                      paste0("samples with top ",(1-input$slider_expression_sum_OS)*100,"% expression sum (",
                             nrow(d[d[,"sum"]> quantile(d[,"sum"],input$slider_expression_sum_OS),]),")")
                    d<-d[order(d[,"IND"]),]
                    d[,"dead"] <- as.numeric(d[,"dead"])
                    
                    fit.result <- survfit(Surv(OS,dead)~IND,data=d)
                    plot.result <- ggsurv(fit.result,surv.col = c("lightgreen","plum"))
                    diff.result <- survdiff(Surv(OS,dead)~IND,data=d)
                    p.value= 1 - pchisq(diff.result$chisq, length(diff.result$n) - 1)
                    plot.result.ann <- plot.result + theme(legend.position = "right", legend.title = element_blank()) + 
                      ggplot2::geom_text(aes(label = sprintf("p-value = %0.2g",
                                                             pchisq(diff.result$chisq,df=1,lower.tail = F)),x=200,y=0.05))
                    p1<-ggplotly(plot.result.ann,legendgroup=~IND,
                                 height = 480 ) %>%
                      layout(title=unique(d$data_source),titlefont=list(size=20,color="black",family= "Arial"),
                             xaxis=list(title="Days",titlefont=list(size=20,color="black",family= "Aril")),
                             yaxis=list(title="Overall survival proportion",titlefont=list(size=20,color="black",family= "Aril")))
                    p1
                  }
                }
              })
            })
          }
        }
      })
    })
    
    
    output$expression_sum_survival_download <- downloadHandler(
      filename = function() {
        if (input$expression_sum_survival_type == "Excel (CSV)") {
          "newfile.csv"
        } else if (input$expression_sum_survival_type == "Text (TSV)") {
          "newfile.txt"
        } else if (input$expression_sum_survival_type == "JSON") {
          "newfile.json"
        }
      },
      content = function(file) {
        if (input$expression_sum_survival_type == "Excel (CSV)") {
          write.csv (expression_sum_survival_download_data(),file,row.names = FALSE)
        } else if (input$expression_sum_survival_type == "Text (TSV)") {
          write.table (expression_sum_survival_download_data(),file,quote=F, row.names = FALSE, sep="\t")
        } else if (input$expression_sum_survival_type == "JSON") {
          write(toJSON(expression_sum_survival_download_data()),file)
        }
      }
    )
    expression_sum_survival_download_data <- function() {
      input_gene_expression_sum <- input$gene_expression_sum
      m<-selecteddatabase_expression_sum()
      if (nrow(m)>0 & ncol(m)>10) {
        m <- data.frame(m,sum=apply(m[,11:ncol(m)],1,function(x) sum(na.omit(x))))
        m[m[,"sum"]==0,"sum"]<-NA
        m<-as.data.frame(m,IND=NA)
        m1 <- m %>% split(.$data_source) %>% 
          lapply(function(d) {
            d[!is.na(d[,"sum"]) & d[,"sum"]>=quantile(na.omit(d[,"sum"]),input$slider_expression_sum_OS),"IND"] <- paste0("top ",(1-input$slider_expression_sum_OS)*100,"% expression sum")
            d[!is.na(d[,"sum"]) & d[,"sum"]<quantile(na.omit(d[,"sum"]),input$slider_expression_sum_OS),"IND"] <- paste0("bottom ",input$slider_expression_sum_OS*100,"% expression sum")
            d})
        m2<-ldply(m1,data.frame)
        m2<-m2[,-1]
        m2[,-9]
      } else {
        m[,-9]
      }
    }
    output$expression_sum_survival_table <- DT::renderDataTable({
#      input$do_expression_sum
#      isolate({
        expression_sum_survival_download_data()
#      })
    })
    
    ##########
    #### expression sum PFS
    ##########
    output$expression_sum_PFS_n_samples_NA <- renderText({
      input$do_expression_sum
      isolate({
        nrow(selecteddatabase_expression_sum())
      })
    })
    output$expression_sum_PFS_plots <- renderUI({
      input$do_expression_sum
      isolate({
        selected.database <- selecteddatabase_expression_sum()
        if (nrow(selected.database)==0 | ncol(selected.database)<11) {
          tagList(plotlyOutput("expression_sum_PFS_plots0",height="auto"))
        } else {
          if (length(input$data_source_expression_sum)==1) {
            tagList(plotlyOutput("expression_sum_PFS_plots1",height="auto"),tags$h1(" "))
          } else if (length(input$data_source_expression_sum)==2) {
            tagList(plotlyOutput("expression_sum_PFS_plots1",height="auto"),tags$h1(" "),
                    plotlyOutput("expression_sum_PFS_plots2",height="auto"),tags$h1(" "))
          } else if (length(input$data_source_expression_sum)==3) {
            tagList(plotlyOutput("expression_sum_PFS_plots1",height="auto"),tags$h1(" "),
                    plotlyOutput("expression_sum_PFS_plots2",height="auto"),tags$h1(" "),
                    plotlyOutput("expression_sum_PFS_plots3",height="auto"),tags$h1(" "))
          } else if (length(input$data_source_expression_sum)==4) {
            tagList(plotlyOutput("expression_sum_PFS_plots1",height="auto"),tags$h1(" "),
                    plotlyOutput("expression_sum_PFS_plots2",height="auto"),tags$h1(" "),
                    plotlyOutput("expression_sum_PFS_plots3",height="auto"),tags$h1(" "),
                    plotlyOutput("expression_sum_PFS_plots4",height="auto"),tags$h1(" "))
          } else if (length(input$data_source_expression_sum)==5) {
            tagList(plotlyOutput("expression_sum_PFS_plots1",height="auto"),tags$h1(" "),
                    plotlyOutput("expression_sum_PFS_plots2",height="auto"),tags$h1(" "),
                    plotlyOutput("expression_sum_PFS_plots3",height="auto"),tags$h1(" "),
                    plotlyOutput("expression_sum_PFS_plots4",height="auto"),tags$h1(" "),
                    plotlyOutput("expression_sum_PFS_plots5",height="auto"),tags$h1(" "))
          } else if (length(input$data_source_expression_sum)==6) {
            tagList(plotlyOutput("expression_sum_PFS_plots1",height="auto"),tags$h1(" "),
                    plotlyOutput("expression_sum_PFS_plots2",height="auto"),tags$h1(" "),
                    plotlyOutput("expression_sum_PFS_plots3",height="auto"),tags$h1(" "),
                    plotlyOutput("expression_sum_PFS_plots4",height="auto"),tags$h1(" "),
                    plotlyOutput("expression_sum_PFS_plots5",height="auto"),tags$h1(" "),
                    plotlyOutput("expression_sum_PFS_plots6",height="auto"),tags$h1(" "))
          } else if (length(input$data_source_expression_sum)==7) {
            tagList(plotlyOutput("expression_sum_PFS_plots1",height="auto"),tags$h1(" "),
                    plotlyOutput("expression_sum_PFS_plots2",height="auto"),tags$h1(" "),
                    plotlyOutput("expression_sum_PFS_plots3",height="auto"),tags$h1(" "),
                    plotlyOutput("expression_sum_PFS_plots4",height="auto"),tags$h1(" "),
                    plotlyOutput("expression_sum_PFS_plots5",height="auto"),tags$h1(" "),
                    plotlyOutput("expression_sum_PFS_plots6",height="auto"),tags$h1(" "),
                    plotlyOutput("expression_sum_PFS_plots7",height="auto"),tags$h1(" "))
          } 
        }
      })
    })
    observeEvent(input$do_expression_sum,{
      input$do_expression_sum
      isolate({
        input_gene_expression_sum <- input$gene_expression_sum
        selected.database <- selecteddatabase_expression_sum()
        if (nrow(selected.database)==0 | ncol(selected.database)<11) {
          output[["expression_sum_PFS_plots0"]] <- renderPlotly({
            plot_ly () %>% 
              layout(title = paste(input$data_source_expression_sum,collapse = ","),font=list(size=15,family="Arial")) %>%
              add_annotations(x=2,y=2,text="no data available under this selection",showarrow=F,font=list(size=16,color="purple")) 
          })
        } else {
          splitted_list <- selected.database %>% as.matrix %>% as.data.frame %>% split(.$data_source)
          splitted_list <- splitted_list[input$data_source_expression_sum]
          for (k in 1:length(input$data_source_expression_sum)) {
            local({
              my_i <- k
              expression_sum_PFS_plots_name <- paste0("expression_sum_PFS_plots", my_i)
              d<-splitted_list[[my_i]]
              output[[expression_sum_PFS_plots_name]] <- renderPlotly({
                if (is.na(names(splitted_list)[my_i])) {
                  plot_ly () %>% 
                    layout(title = input$data_source_expression_sum[[my_i]],font=list(size=15,family="Arial")) %>%
                    add_annotations(x=2,y=2,text="no data available under this selection",showarrow=F,font=list(size=16,color="purple")) 
                } else {
                  d <- data.frame(d,sum=apply(d[,11:ncol(d)],1,function(x) sum(na.omit(as.numeric(x)))))
                  d[d[,"sum"]==0,"sum"]<-NA
                  if (nrow(d[!is.na(d[,"PFS.status"]) & !is.na(d[,"PFS"]) & !is.na(d[,"sum"]),])==0 ) {
                    plot_ly () %>% 
                      layout(title = unique(d$data_source),font=list(size=15,family="Arial")) %>%
                      add_annotations(x=2,y=2,text="no PFS outcome under this selection",showarrow=F,font=list(size=16,color="purple")) 
                  } else {
                    d <- d[!is.na(d[,"PFS.status"]) & !is.na(d[,"PFS"]) & !is.na(d[,"sum"]),]
                    d<-data.frame(d[,1:6],unfactor(d[,c("OS.status","OS","PFS","Age","sum")]))
                    d<-data.frame(d,IND=paste("samples with bottom ",input$slider_expression_sum_PFS*100,"% ", 
                                              "expression sum (",
                                              nrow(d[d[,"sum"]<= quantile(d[,"sum"],input$slider_expression_sum_PFS),]),")",
                                              sep=""))
                    d[,"IND"]<-as.character(d[,"IND"])
                    d[d[,"sum"]> quantile(d[,"sum"],input$slider_expression_sum_PFS),"IND"]<- 
                      paste("samples with top ",(1-input$slider_expression_sum_PFS)*100,"% ", "expression sum (",
                            nrow(d[d[,"sum"]> quantile(d[,"sum"],input$slider_expression_sum_PFS),]),")",
                            sep="")
                    d <- data.frame(d,group.IND=0)
                    d[d[,"PFS.status"]=="nonresponse","group.IND"] <- 1
                    d<-d[order(d[,"IND"]),]
                    fit.result <- survfit(Surv(PFS,group.IND)~IND,data=d)
                    plot.result <- ggsurv(fit.result,surv.col = c("lightgreen","plum"))
                    diff.result <- survdiff(Surv(PFS,group.IND)~IND,data=d)
                    p.value= 1 - pchisq(diff.result$chisq, length(diff.result$n) - 1)
                    plot.result.ann <- plot.result + theme(legend.position = "right", legend.title = element_blank()) + 
                      ggplot2::geom_text(aes(label = sprintf("p-value = %0.2g",
                                                             pchisq(diff.result$chisq,df=1,lower.tail = F)),
                                             x=200,y=0.05))
                    p1 <- ggplotly(plot.result.ann,legendgroup=~IND,
                                   height = 480 ) %>%
                      layout(title=unique(d$data_source),titlefont=list(size=20,color="black",family= "Arial"),
                             xaxis=list(title="Days",titlefont=list(size=20,color="black",family= "Aril")),
                             yaxis=list(title="Progression free survival proportion",titlefont=list(size=20,color="black",family= "Aril")))
                    p1
                  }
                }
              })
            })
          }
        }
      })
    })
    
    output$expression_sum_PFS_download <- downloadHandler(
      filename = function() {
        if (input$expression_sum_PFS_type == "Excel (CSV)") {
          "newfile.csv"
        } else if (input$expression_sum_PFS_type == "Text (TSV)") {
          "newfile.txt"
        } else if (input$expression_sum_PFS_type == "JSON") {
          "newfile.json"
        }
      },
      content = function(file) {
        if (input$expression_sum_PFS_type == "Excel (CSV)") {
          write.csv (expression_sum_PFS_download_data(),file,row.names = FALSE)
        } else if (input$expression_sum_PFS_type == "Text (TSV)") {
          write.table (expression_sum_PFS_download_data(),file,quote=F, row.names = FALSE, sep="\t")
        } else if (input$expression_sum_PFS_type == "JSON") {
          write(toJSON(expression_sum_PFS_download_data()),file)
        }
      }
    )
    expression_sum_PFS_download_data <- function() {
      m<-selecteddatabase_expression_sum()
      if (nrow(m)>0 & ncol(m)>10) {
        m <- data.frame(m,sum=apply(m[,11:ncol(m)],1,function(x) sum(na.omit(x))))
        m[m[,"sum"]==0,"sum"]<-NA
        m<-as.data.frame(m,IND=NA)
        m1 <- m %>% split(.$data_source) %>% 
          lapply(function(d) {
            d[!is.na(d[,"sum"]) & d[,"sum"]>=quantile(na.omit(d[,"sum"]),input$slider_expression_sum_PFS),"IND"] <- paste0("top ",(1-input$slider_expression_sum_PFS)*100,"% expression sum")
            d[!is.na(d[,"sum"]) & d[,"sum"]<quantile(na.omit(d[,"sum"]),input$slider_expression_sum_PFS),"IND"] <- paste0("bottom ",input$slider_expression_sum_PFS*100,"% expression sum")
            d})
        m2<-ldply(m1,data.frame)
        m2<-m2[,-1]
        m2[,-c(7:8)]
      } else {
        m[,-c(7:8)]
      }
      
    }
    output$expression_sum_PFS_table <- DT::renderDataTable({
#      input$do_expression_sum
#      isolate({
        expression_sum_PFS_download_data() 
#      })
    })    
    ###############################################
    #### expression relation pairs
    ###############################################
    output$data_source_expression_pairs <- renderUI({
      all.data.source <- as.character(unique(database.test.process()[,"data_source"]))
      user.data.source <- all.data.source[!(all.data.source %in% c("Robert","Snyder","Moshe","Auslander","Chen","Hugo","Prat","Riaz","Van Allen","Krieg_panel_1","Krieg_panel_2","Krieg_panel_3"))]
      selectInput("data_source_expression_pairs",
                  "Data source: ",
                  choices = c("Auslander","Chen","Hugo","Prat","Riaz","Van Allen",user.data.source),
                  multiple = TRUE,
                  selected = c("Van Allen","Riaz"))})
    output$treatment_expression_pairs <- renderUI ({
      selectInput("treatment_expression_pairs",
                  "Treatment: ",
                  choices = as.character(unique(database.test.process()[,"treatment"])),
                  multiple = TRUE,
                  selected = c("anti-CTLA-4","anti-PD-1","anti-PD-1+anti-CTLA-4"))})
    output$description_expression_pairs <- renderUI ({
      selectInput("description_expression_pairs",
                  "Pre- or On-treatment: ",
                  choices = as.character(unique(database.test.process()[,"description"])),
                  multiple = TRUE,
                  selected = c("Pre","On"))})
    updateSelectizeInput(session,"gene_expression_pairs_1",choices=a.colnames,selected = c("CD28","CD276"),server=TRUE)
    updateSelectizeInput(session,"gene_expression_pairs_2",choices=a.colnames,selected = c("PDCD1","TNFRSF4"),server=TRUE)
    updateSelectizeInput(session,"gene_expression_pairs_3",choices=a.colnames,selected = c("CTLA4","TNFRSF4"),server=TRUE)
    
    ids<<-c(1,2,3)
    #  ids <- NULL
    observeEvent(input$add_expression_pairs,{
      #    a<-database.test.process()
      #    print(ids)
      
      if (is.null(ids)) {
        ids<<-1
      } else {
        ids <<- c(ids,max(ids)+1)
      }
      output$gene_expression_pairs <- renderUI({
        tagList(
          lapply(4:length(ids),function(i) {
            #          insertUI(selector="#add_expression_pairs",where="afterEnd",ui=selectizeInput(paste("gene_expression_pairs_",ids[i],sep=""),sprintf("Gene pairs #%d",ids[i]),multiple=TRUE,choices=NULL,selected = c("CTLA4","PDCD1"),options=list(maxItems=2)))
            selectizeInput(paste("gene_expression_pairs_",ids[i],sep=""),sprintf("Gene pairs #%d",ids[i]),choices=NULL,multiple=TRUE,selected = NULL,options=list(maxItems=2))
            #          updateSelectizeInput(session,paste("gene_expression_pairs_",ids[i],sep=""),choices=a.colnames,selected = c("CTLA4","PDCD1"),server=TRUE)
          })
          #        lapply(1:length(ids),function(i) {
          #          updateSelectizeInput(session,paste("gene_expression_pairs_",ids[i],sep=""),choices=a.colnames,selected = c("CTLA4","PDCD1"),server=TRUE)
          #        })
          
        )
      })
      lapply(4:length(ids),function(i) {
        #          insertUI(selector="#add_expression_pairs",where="afterEnd",ui=selectizeInput(paste("gene_expression_pairs_",ids[i],sep=""),sprintf("Gene pairs #%d",ids[i]),multiple=TRUE,choices=NULL,selected = c("CTLA4","PDCD1"),options=list(maxItems=2)))
        updateSelectizeInput(session,paste("gene_expression_pairs_",ids[i],sep=""),choices=a.colnames,selected=c("CTLA4","PDCD1"),server=TRUE)
      })
      
    })
    #  observeEvent(input$remove_expression_pairs,{
    #    removeUI(selector = paste("div:has(> #gene_expression_pairs_",ids[length(ids)],")",sep=""),immediate = TRUE)
    #    ids<-ids[-length(ids)]
    #  })
    length_ids <- eventReactive(input$do_expression_pairs,{
      length(ids)
    })
    output$slider_expression_pairs_OS <- renderUI({ 
      sliderInput("slider_expression_pairs_OS",
                  "Seperate samples with sum of gene relation pairs: ",
                  min=0,max=length_ids(),value=0) })
#                  min=min(na.omit(expression_pairs_plots_download_data()[,"sum"])),max=max(na.omit(expression_pairs_plots_download_data()[,"sum"])),value=mean(na.omit(expression_pairs_plots_download_data()[,"sum"])))  })
    output$slider_expression_pairs_PFS <- renderUI({
      sliderInput("slider_expression_pairs_PFS",
                  "Seperate samples with sum of gene relation pairs: ",
                  min=0,max=length_ids(),value=0) })
#                  min=min(na.omit(expression_pairs_plots_download_data()[,"sum"])),max=max(na.omit(expression_pairs_plots_download_data()[,"sum"])),value=mean(na.omit(expression_pairs_plots_download_data()[,"sum"])))  })
    
    selecteddatabase_expression_pairs <- eventReactive(input$do_expression_pairs,{
      # Due to dplyr issue #318, we need temp variables for input values
      #    if (input$data_source_expression_pairs=="ALL") {
      #      data_source1 <- c("Auslander","Chen","Hugo","Prat","Riaz","Van Allen")
      #    } else {
      data_source1 <- input$data_source_expression_pairs
      #    }
      treatment1 <- input$treatment_expression_pairs
      description1 <- input$description_expression_pairs
      if (is.null(ids)) {
        genes1 <- c("PatientID","data_source","treatment","description","PFS.status","RECIST","OS.status","OS","PFS","Age")
      } else {
        genessum<-list()
        #      for (i in 1:length(ids)) {
        txt_ids <- sapply(1:length(ids), function(i) paste("gene_expression_pairs_",ids[i],sep=""))
        # 			txt_ids[i] <- paste("gene_expression_pairs_",ids[i],sep="")
        #      }
        for (i in 1:length(txt_ids)) {
          genessum[[i]] <- input[[ txt_ids[i] ]] 
        }
        genes1 <- c("PatientID","data_source","treatment","description","PFS.status","RECIST","OS.status","OS","PFS","Age",unlist(genessum))
      }
      a<-data.table(database.test.process())
      m<-a[data_source %in% data_source1 & treatment %in% treatment1 & description %in% description1]
      m<-m[,colnames(m) %in% genes1,with=FALSE]
      as.data.frame(m)
    })
    
    
    #############
    #### expression pairs plot
    #############
    output$expression_pairs_plots_n_samples_NA <- renderText({
      input$do_expression_pairs
      isolate({
        nrow(selecteddatabase_expression_pairs())})})
    output$expression_pairs_plots_plots <- renderUI({
      input$do_expression_pairs
      isolate({
        selected.database <- selecteddatabase_expression_pairs()
        if (nrow(selected.database)==0 | ncol(selected.database)<11) {
          tagList(plotlyOutput("expression_pairs_plots_plots0",height="auto"))
        } else {
          if (length(input$data_source_expression_pairs)==1) {
            tagList(plotlyOutput("expression_pairs_plots_plots1",height="auto"),tags$h1(" "))
          } else if (length(input$data_source_expression_pairs)==2) {
            tagList(plotlyOutput("expression_pairs_plots_plots1",height="auto"),tags$h1(" "),
                    plotlyOutput("expression_pairs_plots_plots2",height="auto"),tags$h1(" "))
          } else if (length(input$data_source_expression_pairs)==3) {
            tagList(plotlyOutput("expression_pairs_plots_plots1",height="auto"),tags$h1(" "),
                    plotlyOutput("expression_pairs_plots_plots2",height="auto"),tags$h1(" "),
                    plotlyOutput("expression_pairs_plots_plots3",height="auto"),tags$h1(" "))
          } else if (length(input$data_source_expression_pairs)==4) {
            tagList(plotlyOutput("expression_pairs_plots_plots1",height="auto"),tags$h1(" "),
                    plotlyOutput("expression_pairs_plots_plots2",height="auto"),tags$h1(" "),
                    plotlyOutput("expression_pairs_plots_plots3",height="auto"),tags$h1(" "),
                    plotlyOutput("expression_pairs_plots_plots4",height="auto"),tags$h1(" "))
          } else if (length(input$data_source_expression_pairs)==5) {
            tagList(plotlyOutput("expression_pairs_plots_plots1",height="auto"),tags$h1(" "),
                    plotlyOutput("expression_pairs_plots_plots2",height="auto"),tags$h1(" "),
                    plotlyOutput("expression_pairs_plots_plots3",height="auto"),tags$h1(" "),
                    plotlyOutput("expression_pairs_plots_plots4",height="auto"),tags$h1(" "),
                    plotlyOutput("expression_pairs_plots_plots5",height="auto"),tags$h1(" "))
          } else if (length(input$data_source_expression_pairs)==6) {
            tagList(plotlyOutput("expression_pairs_plots_plots1",height="auto"),tags$h1(" "),
                    plotlyOutput("expression_pairs_plots_plots2",height="auto"),tags$h1(" "),
                    plotlyOutput("expression_pairs_plots_plots3",height="auto"),tags$h1(" "),
                    plotlyOutput("expression_pairs_plots_plots4",height="auto"),tags$h1(" "),
                    plotlyOutput("expression_pairs_plots_plots5",height="auto"),tags$h1(" "),
                    plotlyOutput("expression_pairs_plots_plots6",height="auto"),tags$h1(" "))
          } else if (length(input$data_source_expression_pairs)==7) {
            tagList(plotlyOutput("expression_pairs_plots_plots1",height="auto"),tags$h1(" "),
                    plotlyOutput("expression_pairs_plots_plots2",height="auto"),tags$h1(" "),
                    plotlyOutput("expression_pairs_plots_plots3",height="auto"),tags$h1(" "),
                    plotlyOutput("expression_pairs_plots_plots4",height="auto"),tags$h1(" "),
                    plotlyOutput("expression_pairs_plots_plots5",height="auto"),tags$h1(" "),
                    plotlyOutput("expression_pairs_plots_plots6",height="auto"),tags$h1(" "),
                    plotlyOutput("expression_pairs_plots_plots7",height="auto"),tags$h1(" "))
          } 
        }
      })
    })
#    isolate({
    observeEvent(input$do_expression_pairs,{
      input$do_expression_pairs
      isolate({
        selected.database <- selecteddatabase_expression_pairs()
        if (nrow(selected.database)==0 | ncol(selected.database)<11) {
          output[["expression_pairs_plots_plots0"]] <- renderPlotly({
            plot_ly () %>% 
              layout(title = paste(input$data_source_expression_pairs,collapse = ","),font=list(size=15,family="Arial")) %>%
              add_annotations(x=2,y=2,text="no data available under this selection",showarrow=F,font=list(size=16,color="purple")) 
          })
        } else {
          
          #      selected.database <- data.frame(selected.database,Groups=selected.database$PFS.status)
          splitted_list <- selected.database %>% as.matrix %>% as.data.frame %>% split(.$data_source)
          splitted_list <- splitted_list[input$data_source_expression_pairs]
          for (k in 1:length(input$data_source_expression_pairs)) {
            local({
              my_i <- k
              expression_pairs_plots_plots_name <- paste0("expression_pairs_plots_plots", my_i)
              d<-splitted_list[[my_i]]
              output[[expression_pairs_plots_plots_name]] <- renderPlotly({
#                isolate({
                  if (is.na(names(splitted_list)[my_i])) {
                    plot_ly () %>% 
                      layout(title = input$data_source_expression_pairs[[my_i]],font=list(size=15,family="Arial")) %>%
                      add_annotations(x=2,y=2,text="no data available under this selection",showarrow=F,font=list(size=16,color="purple")) 
                  } else {
                    n <- ncol(d)
                    d[,11:ncol(d)] <- unfactor(d[,11:ncol(d)])
                    genessum<-list()
                    txt_ids <- sapply(1:length(ids), function(i) paste("gene_expression_pairs_",ids[i],sep=""))
                  isolate({
                    for (i in 1:length(txt_ids)) {
                      genessum[[i]] <- input[[ txt_ids[i] ]]
                      if (length(input[[ txt_ids[i] ]]) == 1) {
                        d <- data.frame(d,NA)
                      } else {
                        d <- data.frame(d,d[,input[[ txt_ids[i] ]][1]] > d[,input[[ txt_ids[i] ]][2]])
                      }
                    }
                  })
                    colnames(d)[(n+1):ncol(d)] <- unlist(lapply(genessum,function(x) paste(x,collapse=".")))
                    
                    if (nrow(d[!is.na(d[,"PFS.status"]),])==0 | ncol(d[,which(unlist(lapply(d, function(x)!all(is.na(x)))))])<=10) {
                      plot_ly () %>% 
                        layout(title = unique(d$data_source),font=list(size=15,family="Arial")) %>%
                        add_annotations(x=2,y=2,text="no immunotherapy response under this selection",showarrow=F,font=list(size=16,color="purple")) 
                    } else {
                      d <- d[!is.na(d[,"PFS.status"]) & ncol(d[,which(unlist(lapply(d, function(x)!all(is.na(x)))))])>10,]
                      cols <- sapply(d, is.logical)
                      for (j in (n+1):ncol(d)) {
                        d[,j] <- as.numeric(d[,j])
                      }
                      d <- data.frame(d,sum=NA)
                      for (i in 1:nrow(d)) {
                        d[i,"sum"] <- sum(na.omit(as.numeric(d[i,(n+1):(ncol(d)-1)])))
                      }
                      d <- data.frame(d,Groups=d$PFS.status)
                      ind <- unique(d[,"Groups"])
                      d <- d[!is.na(d[,"sum"]),]
                      d[,"sum"]<-paste0("number of positive gene pairs = ",d[,"sum"])
                      d[,"PFS.status"] <- unfactor(d[,"PFS.status"])
                      d[d[,"PFS.status"]=="response" & !is.na(d[,"PFS.status"]),"PFS.status"] <- paste("response (n=",nrow(d[d[,"PFS.status"]=="response",]),")")
                      d[d[,"PFS.status"]=="nonresponse" & !is.na(d[,"PFS.status"]),"PFS.status"] <- paste("nonresponse (n=",nrow(d[d[,"PFS.status"]=="nonresponse",]),")")
                      d.plot <- as.data.frame(unlist(table(d[,c("PFS.status","sum")])))
                      colnames(d.plot)[2] <- "Relation.pairs"
                      try.test <- try(p.v <- fisher.test(unlist(table(d[,c("PFS.status","sum")])))$p.value)
                      if (class(try.test)=="try-error") {
                        p.v <- "NA"
                      } else {
                        p.v <- fisher.test(unlist(table(d[,c("PFS.status","sum")])))$p.value
                        if (p.v < 0.001) {
                          p.v <- "< 0.001"
                        } else {
                          p.v<-round(p.v,3)
                        } 
                      }
                      ggplotly(ggplot(d.plot, aes(fill=Relation.pairs, y=Freq, x=PFS.status)) + ggtitle(as.character(unique(d$data_source))) + 
                                 geom_bar( stat="identity", position="fill") +ylab("Sum of gene relations frequency") + xlab("Immunotherapy response") + 
                                 annotate("text",x=1.5,y=1.2,label=paste0("p-value = ",p.v),size=4))
                    } 
                    
                  }
#                })
              })
            })
          }
          
        }
      })
    })
#  })
    
    
    output$expression_pairs_plots_download <- downloadHandler(
      filename = function() {
        if (input$expression_pairs_plots_type == "Excel (CSV)") {
          "newfile.csv"
        } else if (input$expression_pairs_plots_type == "Text (TSV)") {
          "newfile.txt"
        } else if (input$expression_pairs_plots_type == "JSON") {
          "newfile.json"
        }
      },
      content = function(file) {
        if (input$expression_pairs_plots_type == "Excel (CSV)") {
          write.csv (expression_pairs_plots_download_data(),file,row.names = FALSE)
        } else if (input$expression_pairs_plots_type == "Text (TSV)") {
          write.table (expression_pairs_plots_download_data(),file,quote=F, row.names = FALSE, sep="\t")
        } else if (input$expression_pairs_plots_type == "JSON") {
          write(toJSON(expression_pairs_plots_download_data()),file)
        }
      }
    )
    expression_pairs_plots_download_data <- function() {
      #  expression_pairs_plots_download_data <- function(input=input$gene_expression_pairs) {
      selected.database <- selecteddatabase_expression_pairs()
      #    n<-nrow(selected.database)
      if (nrow(selected.database)>0) {
        n <- ncol(selected.database)
        genessum<-list()
        #    for (i in 1:length(ids)) {
        txt_ids <- sapply(1:length(ids), function(i) paste("gene_expression_pairs_",ids[i],sep=""))
        #      txt_ids <- paste("gene_expression_pairs_",ids[i],sep="")
        #    }
        for (i in 1:length(txt_ids)) {
          genessum[[i]] <- input[[ txt_ids[i] ]]
          if (length(input[[ txt_ids[i] ]]) == 1) {
            selected.database <- data.frame(selected.database,NA)
          } else {
            selected.database <- data.frame(selected.database,selected.database[,input[[ txt_ids[i] ]][1]] > selected.database[,input[[ txt_ids[i] ]][2]])
          }
        }
        colnames(selected.database)[(n+1):ncol(selected.database)] <- unlist(lapply(genessum,function(x) paste(x,collapse=".")))
        
        
        #    a <- unlist(str_split(input,","))
        #    for (i in 1:length(a)) {
        #        b <- unlist(str_split(a[i],":"))
        #        selected.database <- data.frame(selected.database,selected.database[,b[1]] > selected.database[,b[2]])
        #      }
        #    colnames(selected.database)[(n+1):ncol(selected.database)] <- unlist(str_split(input,","))
        cols <- sapply(selected.database, is.logical)
        for (j in (n+1):ncol(selected.database)) {
          selected.database[,j] <- as.numeric(selected.database[,j])
        }
        selected.database <- data.frame(selected.database,sum=NA)
        for (i in 1:nrow(selected.database)) {
          selected.database[i,"sum"] <- sum(na.omit(as.numeric(selected.database[i,(n+1):(ncol(selected.database)-1)])))
        }
        selected.database
      }
    }
    output$expression_pairs_plots_table <- DT::renderDataTable({
      input$do_expression_pairs
      isolate({
        expression_pairs_plots_download_data()
      })
    })
    
    ############
    #### expression pairs survival
    ############
    output$expression_pairs_survival_n_samples_NA <- renderText({
      input$do_expression_pairs
      isolate({
        nrow(selecteddatabase_expression_pairs())})})
    output$expression_pairs_survival_plots <- renderUI({
      input$do_expression_pairs
      isolate({
        selected.database <- selecteddatabase_expression_pairs()
        if (nrow(selected.database)==0 | ncol(selected.database)<11) {
          tagList(plotlyOutput("expression_pairs_survival_plots0",height="auto"))
        } else {
          if (length(input$data_source_expression_pairs)==1) {
            tagList(plotlyOutput("expression_pairs_survival_plots1",height="auto"),tags$h1(" "))
          } else if (length(input$data_source_expression_pairs)==2) {
            tagList(plotlyOutput("expression_pairs_survival_plots1",height="auto"),tags$h1(" "),
                    plotlyOutput("expression_pairs_survival_plots2",height="auto"),tags$h1(" "))
          } else if (length(input$data_source_expression_pairs)==3) {
            tagList(plotlyOutput("expression_pairs_survival_plots1",height="auto"),tags$h1(" "),
                    plotlyOutput("expression_pairs_survival_plots2",height="auto"),tags$h1(" "),
                    plotlyOutput("expression_pairs_survival_plots3",height="auto"),tags$h1(" "))
          } else if (length(input$data_source_expression_pairs)==4) {
            tagList(plotlyOutput("expression_pairs_survival_plots1",height="auto"),tags$h1(" "),
                    plotlyOutput("expression_pairs_survival_plots2",height="auto"),tags$h1(" "),
                    plotlyOutput("expression_pairs_survival_plots3",height="auto"),tags$h1(" "),
                    plotlyOutput("expression_pairs_survival_plots4",height="auto"),tags$h1(" "))
          } else if (length(input$data_source_expression_pairs)==5) {
            tagList(plotlyOutput("expression_pairs_survival_plots1",height="auto"),tags$h1(" "),
                    plotlyOutput("expression_pairs_survival_plots2",height="auto"),tags$h1(" "),
                    plotlyOutput("expression_pairs_survival_plots3",height="auto"),tags$h1(" "),
                    plotlyOutput("expression_pairs_survival_plots4",height="auto"),tags$h1(" "),
                    plotlyOutput("expression_pairs_survival_plots5",height="auto"),tags$h1(" "))
          } else if (length(input$data_source_expression_pairs)==6) {
            tagList(plotlyOutput("expression_pairs_survival_plots1",height="auto"),tags$h1(" "),
                    plotlyOutput("expression_pairs_survival_plots2",height="auto"),tags$h1(" "),
                    plotlyOutput("expression_pairs_survival_plots3",height="auto"),tags$h1(" "),
                    plotlyOutput("expression_pairs_survival_plots4",height="auto"),tags$h1(" "),
                    plotlyOutput("expression_pairs_survival_plots5",height="auto"),tags$h1(" "),
                    plotlyOutput("expression_pairs_survival_plots6",height="auto"),tags$h1(" "))
          } else if (length(input$data_source_expression_pairs)==7) {
            tagList(plotlyOutput("expression_pairs_survival_plots1",height="auto"),tags$h1(" "),
                    plotlyOutput("expression_pairs_survival_plots2",height="auto"),tags$h1(" "),
                    plotlyOutput("expression_pairs_survival_plots3",height="auto"),tags$h1(" "),
                    plotlyOutput("expression_pairs_survival_plots4",height="auto"),tags$h1(" "),
                    plotlyOutput("expression_pairs_survival_plots5",height="auto"),tags$h1(" "),
                    plotlyOutput("expression_pairs_survival_plots6",height="auto"),tags$h1(" "),
                    plotlyOutput("expression_pairs_survival_plots7",height="auto"),tags$h1(" "))
          } 
        }
      })
    })
    observeEvent(input$do_expression_pairs,{
      input$do_expression_pairs
      isolate({
        selected.database <- selecteddatabase_expression_pairs()
        if (nrow(selected.database)==0 | ncol(selected.database)<11) {
          output[["expression_pairs_survival_plots0"]] <- renderPlotly({
            plot_ly () %>% 
              layout(title = paste(input$data_source_expression_pairs,collapse = ","),font=list(size=15,family="Arial")) %>%
              add_annotations(x=2,y=2,text="no data available under this selection",showarrow=F,font=list(size=16,color="purple")) 
          })
        } else {
          #      selected.database <- data.frame(selected.database,Groups=selected.database$PFS.status)
          splitted_list <- selected.database %>% as.matrix %>% as.data.frame %>% split(.$data_source)
          splitted_list <- splitted_list[input$data_source_expression_pairs]
          for (k in 1:length(input$data_source_expression_pairs)) {
            local({
              my_i <- k
              expression_pairs_survival_plots_name <- paste0("expression_pairs_survival_plots", my_i)
              d<-splitted_list[[my_i]]
              output[[expression_pairs_survival_plots_name]] <- renderPlotly({

                  if (is.na(names(splitted_list)[my_i])) {
                    plot_ly () %>% 
                      layout(title = input$data_source_expression_pairs[[my_i]],font=list(size=15,family="Arial")) %>%
                      add_annotations(x=2,y=2,text="no data available under this selection",showarrow=F,font=list(size=16,color="purple")) 
                  } else {
                    n <- ncol(d)
                    d[,11:ncol(d)] <- unfactor(d[,11:ncol(d)])
                    genessum<-list()
                    txt_ids <- sapply(1:length(ids), function(i) paste("gene_expression_pairs_",ids[i],sep=""))
                  isolate({
                    for (i in 1:length(txt_ids)) {
                      genessum[[i]] <- input[[ txt_ids[i] ]]
                      if (length(input[[ txt_ids[i] ]]) == 1) {
                        d <- data.frame(d,NA)
                      } else {
                        d <- data.frame(d,d[,input[[ txt_ids[i] ]][1]] > d[,input[[ txt_ids[i] ]][2]])
                      }
                    }
                  })
                    colnames(d)[(n+1):ncol(d)] <- unlist(lapply(genessum,function(x) paste(x,collapse=".")))
                    
                    if (nrow(d[!is.na(d[,"OS.status"]) & !is.na(d[,"OS"]),])==0 | ncol(d[,which(unlist(lapply(d, function(x)!all(is.na(x)))))])<=10) {
                      plot_ly () %>% 
                        layout(title = unique(d$data_source),font=list(size=15,family="Arial")) %>%
                        add_annotations(x=2,y=2,text="no overall survival data available under this selection",showarrow=F,font=list(size=16,color="purple")) 
                    } else {
                      d <- d[!is.na(d[,"OS.status"]) & !is.na(d[,"OS"]) & ncol(d[,which(unlist(lapply(d, function(x)!all(is.na(x)))))])>10,]
                      cols <- sapply(d, is.logical)
                      for (j in (n+1):ncol(d)) {
                        d[,j] <- as.numeric(d[,j])
                      }
                      d <- data.frame(d,sum=NA)
                      for (i in 1:nrow(d)) {
                        d[i,"sum"] <- sum(na.omit(as.numeric(d[i,(n+1):(ncol(d)-1)])))
                      }
                      d <- d[!is.na(d[,"sum"]),]
                      if (length(unique(d[,"sum"]))<2) {
                        plot_ly () %>% add_annotations(x=2,y=2,text="can't seperate samples because of same expression relations, please select more samples",showarrow=F,font=list(size=16,color="purple"))
                      } else {
                        d<-data.frame(d,dead=NA)
                        d[d[,"OS.status"]=="living" & !is.na(d[,"OS.status"]),"dead"] <- 0
                        d[d[,"OS.status"]=="deceased" & !is.na(d[,"OS.status"]),"dead"] <- 1
                        d <- d[!is.na(d[,"sum"]) & !is.na(d[,"dead"]),]
                        d <- d[d[,"dead"]==1 | d[,"dead"]==0,]
                        d<-data.frame(d[,1:6],unfactor(d[,c("dead","OS","PFS","Age","sum")]))
                        
                        
                        d<-data.frame(d,IND=paste0("samples equal to or lower than ",input$slider_expression_pairs_OS," pairs (",
                                                   nrow(d[d[,"sum"]<=input$slider_expression_pairs_OS,]),")"))
                        d[,"IND"]<-as.character(d[,"IND"])
                        d[d[,"sum"]>input$slider_expression_pairs_OS,"IND"] <- 
                          paste0("samples higher than ",input$slider_expression_pairs_OS," pairs (",
                                 nrow(d[d[,"sum"]>input$slider_expression_pairs_OS,]),")")
                        if (nrow(d[d[,"sum"]<=input$slider_expression_pairs_OS,])==0 | nrow(d[d[,"sum"]>input$slider_expression_pairs_OS,])==0) {
                          plot_ly () %>% add_annotations(x=2,y=2,text="can't seperate samples under this cutoff",showarrow=F,font=list(size=16,color="purple"))
                        } else {
                          d<-d[order(d[,"IND"]),]
                          #d[,"dead"] <- as.numeric(d[,"dead"])
                          
                          fit.result <- survfit(Surv(OS,dead)~IND,data=d)
                          plot.result <- ggsurv(fit.result,surv.col = c("lightgreen","plum"))
                          diff.result <- survdiff(Surv(OS,dead)~IND,data=d)
                          p.value= 1 - pchisq(diff.result$chisq, length(diff.result$n) - 1)
                          plot.result.ann <- plot.result + theme(legend.position = "right", legend.title = element_blank()) + 
                            ggplot2::geom_text(aes(label = sprintf(" p-value = %0.2g",
                                                                   pchisq(diff.result$chisq,df=1,lower.tail = F)),x=200,y=0.05))
                          ggplotly(plot.result.ann,legendgroup=~IND,height = 480 ) %>%
                            layout(title=unique(d$data_source),titlefont=list(size=20,color="black",family= "Arial"),
                                   xaxis=list(title="Days",titlefont=list(size=20,color="black",family= "Aril")),
                                   yaxis=list(title="Overall survival proportion",titlefont=list(size=20,color="black",family= "Aril")))
                        }
                      }
                    } 
                    
                  }
#                })
              })
            })
          }
        }
      })
    }) 
    
    output$expression_pairs_survival_download <- downloadHandler(
      filename = function() {
        if (input$expression_pairs_survival_type == "Excel (CSV)") {
          "newfile.csv"
        } else if (input$expression_pairs_survival_type == "Text (TSV)") {
          "newfile.txt"
        } else if (input$expression_pairs_survival_type == "JSON") {
          "newfile.json"
        }
      },
      content = function(file) {
        if (input$expression_pairs_survival_type == "Excel (CSV)") {
          write.csv (expression_pairs_survival_download_data(),file,row.names = FALSE)
        } else if (input$expression_pairs_survival_type == "Text (TSV)") {
          write.table (expression_pairs_survival_download_data(),file,quote=F, row.names = FALSE, sep="\t")
        } else if (input$expression_pairs_survival_type == "JSON") {
          write(toJSON(expression_pairs_survival_download_data()),file)
        }
      }
    )
    expression_pairs_survival_download_data <- function() {
      selected.database<-selecteddatabase_expression_pairs()
      #    n<-ncol(selected.database)
      if (nrow(selected.database)>0) {
        n <- ncol(selected.database)
        genessum<-list()
        #      for (i in 1:length(ids)) {
        txt_ids <- sapply(1:length(ids), function(i) paste("gene_expression_pairs_",ids[i],sep=""))
        #      }
      isolate({
        for (i in 1:length(txt_ids)) {
          genessum[[i]] <- input[[ txt_ids[i] ]]
          if (length(input[[ txt_ids[i] ]]) == 1) {
            selected.database <- data.frame(selected.database,NA)
          } else {
            selected.database <- data.frame(selected.database,selected.database[,input[[ txt_ids[i] ]][1]] > selected.database[,input[[ txt_ids[i] ]][2]])
          }
        }
      })
        colnames(selected.database)[(n+1):ncol(selected.database)] <- unlist(lapply(genessum,function(x) paste(x,collapse=".")))
        cols <- sapply(selected.database, is.logical)
        for (j in (n+1):ncol(selected.database)) {
          selected.database[,j] <- as.numeric(selected.database[,j])
        }
        selected.database <- data.frame(selected.database,sum=NA)
        for (i in 1:nrow(selected.database)) {
          selected.database[i,"sum"] <- sum(na.omit(as.numeric(selected.database[i,(n+1):(ncol(selected.database)-1)])))
        }
        selected.database<-data.frame(selected.database,IND=NA)
        #      selected.database[,"IND"]<-as.character(selected.database[,"IND"])
        m1 <- selected.database %>% split(.$data_source) %>% 
          lapply(function(d) {
            d[!is.na(d[,"sum"]) & d[,"sum"]>input$slider_expression_pairs_OS,"IND"] <- paste0("samples higher than ",input$slider_expression_pairs_OS," pairs")
            d[!is.na(d[,"sum"]) & d[,"sum"]<=input$slider_expression_pairs_OS,"IND"] <- paste0("samples equal to or lower than ",input$slider_expression_pairs_OS," pairs")
            d
          })
        m2 <- ldply(m1,data.frame)
        m2 <- m2[,-1]
        m2[,-c(5,9)]
        #
        
      } else {
        selected.database[,-c(5,9)]
      }
    }
    output$expression_pairs_survival_table <- DT::renderDataTable({
#      input$do_expression_pairs
#      isolate({
        expression_pairs_survival_download_data()
#      })
    })
    ############
    #### expression pairs PFS
    ############
    output$expression_pairs_PFS_n_samples_NA <- renderText({
      input$do_expression_pairs
      isolate({
        nrow(selecteddatabase_expression_pairs())})})
    output$expression_pairs_PFS_plots <- renderUI({
      input$do_expression_pairs
      isolate({
        selected.database <- selecteddatabase_expression_pairs()
        if (nrow(selected.database)==0 | ncol(selected.database)<11) {
          tagList(plotlyOutput("expression_pairs_PFS_plots0",height="auto"))
        } else {
          if (length(input$data_source_expression_pairs)==1) {
            tagList(plotlyOutput("expression_pairs_PFS_plots1",height="auto"),tags$h1(" "))
          } else if (length(input$data_source_expression_pairs)==2) {
            tagList(plotlyOutput("expression_pairs_PFS_plots1",height="auto"),tags$h1(" "),
                    plotlyOutput("expression_pairs_PFS_plots2",height="auto"),tags$h1(" "))
          } else if (length(input$data_source_expression_pairs)==3) {
            tagList(plotlyOutput("expression_pairs_PFS_plots1",height="auto"),tags$h1(" "),
                    plotlyOutput("expression_pairs_PFS_plots2",height="auto"),tags$h1(" "),
                    plotlyOutput("expression_pairs_PFS_plots3",height="auto"),tags$h1(" "))
          } else if (length(input$data_source_expression_pairs)==4) {
            tagList(plotlyOutput("expression_pairs_PFS_plots1",height="auto"),tags$h1(" "),
                    plotlyOutput("expression_pairs_PFS_plots2",height="auto"),tags$h1(" "),
                    plotlyOutput("expression_pairs_PFS_plots3",height="auto"),tags$h1(" "),
                    plotlyOutput("expression_pairs_PFS_plots4",height="auto"),tags$h1(" "))
          } else if (length(input$data_source_expression_pairs)==5) {
            tagList(plotlyOutput("expression_pairs_PFS_plots1",height="auto"),tags$h1(" "),
                    plotlyOutput("expression_pairs_PFS_plots2",height="auto"),tags$h1(" "),
                    plotlyOutput("expression_pairs_PFS_plots3",height="auto"),tags$h1(" "),
                    plotlyOutput("expression_pairs_PFS_plots4",height="auto"),tags$h1(" "),
                    plotlyOutput("expression_pairs_PFS_plots5",height="auto"),tags$h1(" "))
          } else if (length(input$data_source_expression_pairs)==6) {
            tagList(plotlyOutput("expression_pairs_PFS_plots1",height="auto"),tags$h1(" "),
                    plotlyOutput("expression_pairs_PFS_plots2",height="auto"),tags$h1(" "),
                    plotlyOutput("expression_pairs_PFS_plots3",height="auto"),tags$h1(" "),
                    plotlyOutput("expression_pairs_PFS_plots4",height="auto"),tags$h1(" "),
                    plotlyOutput("expression_pairs_PFS_plots5",height="auto"),tags$h1(" "),
                    plotlyOutput("expression_pairs_PFS_plots6",height="auto"),tags$h1(" "))
          } else if (length(input$data_source_expression_pairs)==7) {
            tagList(plotlyOutput("expression_pairs_PFS_plots1",height="auto"),tags$h1(" "),
                    plotlyOutput("expression_pairs_PFS_plots2",height="auto"),tags$h1(" "),
                    plotlyOutput("expression_pairs_PFS_plots3",height="auto"),tags$h1(" "),
                    plotlyOutput("expression_pairs_PFS_plots4",height="auto"),tags$h1(" "),
                    plotlyOutput("expression_pairs_PFS_plots5",height="auto"),tags$h1(" "),
                    plotlyOutput("expression_pairs_PFS_plots6",height="auto"),tags$h1(" "),
                    plotlyOutput("expression_pairs_PFS_plots7",height="auto"),tags$h1(" "))
          } 
        }
      })
    })
    observeEvent(input$do_expression_pairs,{
      input$do_expression_pairs
      isolate({
        selected.database <- selecteddatabase_expression_pairs()
        if (nrow(selected.database)==0 | ncol(selected.database)<11) {
          output[["expression_pairs_PFS_plots0"]] <- renderPlotly({
            plot_ly () %>% 
              layout(title = paste(input$data_source_expression_pairs,collapse = ","),font=list(size=15,family="Arial")) %>%
              add_annotations(x=2,y=2,text="no data available under this selection",showarrow=F,font=list(size=16,color="purple")) 
          })
        } else {
          #      selected.database <- data.frame(selected.database,Groups=selected.database$PFS.status)
          splitted_list <- selected.database %>% as.matrix %>% as.data.frame %>% split(.$data_source)
          splitted_list <- splitted_list[input$data_source_expression_pairs]
          for (k in 1:length(input$data_source_expression_pairs)) {
            local({
              my_i <- k
              expression_pairs_PFS_plots_name <- paste0("expression_pairs_PFS_plots", my_i)
              d<-splitted_list[[my_i]]
              output[[expression_pairs_PFS_plots_name]] <- renderPlotly({
#                isolate({
                  if (is.na(names(splitted_list)[my_i])) {
                    plot_ly () %>% 
                      layout(title = input$data_source_expression_pairs[[my_i]],font=list(size=15,family="Arial")) %>%
                      add_annotations(x=2,y=2,text="no data available under this selection",showarrow=F,font=list(size=16,color="purple")) 
                  } else {
                    n <- ncol(d)
                    d[,11:ncol(d)] <- unfactor(d[,11:ncol(d)])
                    genessum<-list()
                    txt_ids <- sapply(1:length(ids), function(i) paste("gene_expression_pairs_",ids[i],sep=""))
                  isolate({
                    for (i in 1:length(txt_ids)) {
                      genessum[[i]] <- input[[ txt_ids[i] ]]
                      if (length(input[[ txt_ids[i] ]]) == 1) {
                        d <- data.frame(d,NA)
                      } else {
                        d <- data.frame(d,d[,input[[ txt_ids[i] ]][1]] > d[,input[[ txt_ids[i] ]][2]])
                      }
                    }
                  })
                    colnames(d)[(n+1):ncol(d)] <- unlist(lapply(genessum,function(x) paste(x,collapse=".")))
                    
                    if (nrow(d[!is.na(d[,"PFS.status"]) & !is.na(d[,"PFS"]),])==0 | ncol(d[,which(unlist(lapply(d, function(x)!all(is.na(x)))))])<=10) {
                      plot_ly () %>% 
                        layout(title = unique(d$data_source),font=list(size=15,family="Arial")) %>%
                        add_annotations(x=2,y=2,text="no PFS data available under this selection",showarrow=F,font=list(size=16,color="purple")) 
                    } else {
                      d <- d[!is.na(d[,"PFS.status"]) & !is.na(d[,"PFS"]) & ncol(d[,which(unlist(lapply(d, function(x)!all(is.na(x)))))])>10,]
                      cols <- sapply(d, is.logical)
                      for (j in (n+1):ncol(d)) {
                        d[,j] <- as.numeric(d[,j])
                      }
                      d <- data.frame(d,sum=NA)
                      for (i in 1:nrow(d)) {
                        d[i,"sum"] <- sum(na.omit(as.numeric(d[i,(n+1):(ncol(d)-1)])))
                      }
                      d <- d[!is.na(d[,"sum"]),]
                      if (length(unique(d[,"sum"]))<2) {
                        plot_ly () %>% add_annotations(x=2,y=2,text="can't seperate samples because of same expression relations, please select more samples",showarrow=F,font=list(size=16,color="purple"))
                      } else {
                        d<-data.frame(d[,1:6],unfactor(d[,c("OS.status","OS","PFS","Age","sum")]))
                        d<-data.frame(d,IND=paste0("samples equal to or lower than ",input$slider_expression_pairs_PFS," pairs (",
                                                   nrow(d[d[,"sum"]<=input$slider_expression_pairs_PFS,]),")"))
                        d[,"IND"]<-as.character(d[,"IND"])
                        d[d[,"sum"]>input$slider_expression_pairs_PFS,"IND"] <- 
                          paste0("samples higher than ",input$slider_expression_pairs_PFS," pairs (",
                                 nrow(d[d[,"sum"]>input$slider_expression_pairs_PFS,]),")")
                        if (nrow(d[d[,"sum"]<=input$slider_expression_pairs_PFS,])==0 | nrow(d[d[,"sum"]>input$slider_expression_pairs_PFS,])==0) {
                          plot_ly () %>% add_annotations(x=2,y=2,text="can't seperate samples under this cutoff",showarrow=F,font=list(size=16,color="purple"))
                        } else {
                          d <- data.frame(d,group.IND=0)
                          d[d[,"PFS.status"]=="nonresponse","group.IND"] <- 1
                          d<-d[order(d[,"IND"]),]
                          fit.result <- survfit(Surv(PFS,group.IND)~IND,data=d)
                          plot.result <- ggsurv(fit.result,surv.col = c("lightgreen","plum"))
                          diff.result <- survdiff(Surv(PFS,group.IND)~IND,data=d)
                          p.value= 1 - pchisq(diff.result$chisq, length(diff.result$n) - 1)
                          plot.result.ann <- plot.result + theme(legend.position = "right", legend.title = element_blank()) + 
                            ggplot2::geom_text(aes(label = sprintf("p-value = %0.2g",
                                                                   pchisq(diff.result$chisq,df=1,lower.tail = F)),x=200,y=0.05))
                          ggplotly(plot.result.ann,legendgroup=~IND,height = 480 ) %>%
                            layout(title=unique(d$data_source),titlefont=list(size=20,color="black",family= "Arial"),
                                   xaxis=list(title="Days",titlefont=list(size=20,color="black",family= "Aril")),
                                   yaxis=list(title="Progression free survival proportion",titlefont=list(size=20,color="black",family= "Aril")))
                        }
                      }
                    } 
                    
                  }
#                })
              })
            })
          }
        }
      })
    }) 
    
    output$expression_pairs_PFS_download <- downloadHandler(
      filename = function() {
        if (input$expression_pairs_PFS_type == "Excel (CSV)") {
          "newfile.csv"
        } else if (input$expression_pairs_PFS_type == "Text (TSV)") {
          "newfile.txt"
        } else if (input$expression_pairs_PFS_type == "JSON") {
          "newfile.json"
        }
      },
      content = function(file) {
        if (input$expression_pairs_PFS_type == "Excel (CSV)") {
          write.csv (expression_pairs_PFS_download_data(),file,row.names = FALSE)
        } else if (input$expression_pairs_PFS_type == "Text (TSV)") {
          write.table (expression_pairs_PFS_download_data(),file,quote=F, row.names = FALSE, sep="\t")
        } else if (input$expression_pairs_PFS_type == "JSON") {
          write(toJSON(expression_pairs_PFS_download_data()),file)
        }
      }
    )
    expression_pairs_PFS_download_data <- function() {
      selected.database<-selecteddatabase_expression_pairs()
      #    n<-ncol(selected.database)
      if (nrow(selected.database)>0) {
        n <- ncol(selected.database)
        genessum<-list()
        #      for (i in 1:length(ids)) {
        txt_ids <- sapply(1:length(ids), function(i) paste("gene_expression_pairs_",ids[i],sep=""))
        #      }
      isolate({
        for (i in 1:length(txt_ids)) {
          genessum[[i]] <- input[[ txt_ids[i] ]]
          if (length(input[[ txt_ids[i] ]]) == 1) {
            selected.database <- data.frame(selected.database,NA)
          } else {
            selected.database <- data.frame(selected.database,selected.database[,input[[ txt_ids[i] ]][1]] > selected.database[,input[[ txt_ids[i] ]][2]])
          }
        }
      })
        colnames(selected.database)[(n+1):ncol(selected.database)] <- unlist(lapply(genessum,function(x) paste(x,collapse=".")))
        cols <- sapply(selected.database, is.logical)
        for (j in (n+1):ncol(selected.database)) {
          selected.database[,j] <- as.numeric(selected.database[,j])
        }
        selected.database <- data.frame(selected.database,sum=NA)
        for (i in 1:nrow(selected.database)) {
          selected.database[i,"sum"] <- sum(na.omit(as.numeric(selected.database[i,(n+1):(ncol(selected.database)-1)])))
        }
        selected.database<-data.frame(selected.database,IND=NA)
        m1 <- selected.database %>% split(.$data_source) %>% 
          lapply(function(d) {
            d[!is.na(d[,"sum"]) & d[,"sum"]>input$slider_expression_pairs_PFS,"IND"] <- paste0("samples higher than ",input$slider_expression_pairs_PFS," pairs")
            d[!is.na(d[,"sum"]) & d[,"sum"]<=input$slider_expression_pairs_PFS,"IND"] <- paste0("samples equal to or lower than ",input$slider_expression_pairs_PFS," pairs")
            d
          })
        m2 <- ldply(m1,data.frame)
        m2 <- m2[,-1]
        m2[,-c(7,8)]
        
      } else {
        selected.database[,-c(7,8)]
      }
    }
    output$expression_pairs_PFS_table <- DT::renderDataTable({
#      input$do_expression_pairs
#      isolate({
        expression_pairs_PFS_download_data()
#      })
    })
    ###############################################
    #### cibersort
    ###############################################
    output$data_source_cibersort <- renderUI({
      all.data.source <- as.character(unique(database.test.process()[,"data_source"]))
      user.data.source <- all.data.source[!(all.data.source %in% c("Robert","Snyder","Moshe","Auslander","Chen","Hugo","Prat","Riaz","Van Allen","Krieg_panel_1","Krieg_panel_2","Krieg_panel_3"))]
      selectInput("data_source_cibersort",
                  "Data source: ",
                  choices =  c("Auslander","Hugo","Riaz","Van Allen",user.data.source),
                  multiple = TRUE,
                  selected = c("Van Allen","Riaz"))})
    output$treatment_cibersort <- renderUI ({
      selectInput("treatment_cibersort",
                  "Treatment: ",
                  #                choices = as.character(unique(filter_input_interactive(filter_input=input$data_source_cibersort)[,"treatment"])),
                  choices = as.character(unique(database.test.process()[,"treatment"])),
                  multiple = TRUE,
                  selected = c("anti-CTLA-4","anti-PD-1","anti-PD-1+anti-CTLA-4"))})
    output$description_cibersort <- renderUI ({
      selectInput("description_cibersort",
                  "Pre- or On-treatment: ",
                  #                choices = as.character(unique(filter_input_interactive(filter_input=c(input$data_source_cibersort,input$treatment_cibersort))[,"description"])),
                  choices = as.character(unique(database.test.process()[,"description"])),
                  multiple = TRUE,
                  selected = c("Pre","On"))})
    output$immucell_cibersort <- renderUI ({
      selectInput("immucell_cibersort",
                  "Immune cell type: ",
                  choices = sort(c("B.cells.naive","B.cells.memory","Plasma.cells","T.cells.CD8","T.cells.CD4.naive","T.cells.CD4.memory.resting","T.cells.CD4.memory.activated","T.cells.follicular.helper","T.cells.regulatory..Tregs.","T.cells.gamma.delta","NK.cells.resting","NK.cells.activated","Monocytes","Macrophages.M0","Macrophages.M1","Macrophages.M2","Dendritic.cells.resting","Dendritic.cells.activated","Mast.cells.resting","Mast.cells.activated","Eosinophils","Neutrophils")),
                  selected = "Macrophages.M1")})
    output$slider_cibersort_OS <- renderUI({
      sliderInput("slider_cibersort_OS",
                  "Seperate samples with immune cell infiltration cutoff: ",
                  min=0,max=1,value=0.5)  })
    output$slider_cibersort_PFS <- renderUI({
      sliderInput("slider_cibersort_PFS",
                  "Seperate samples with immune cell infiltration cutoff: ",
                  min=0,max=1,value=0.5)  })
    
    selecteddatabase_cibersort <- eventReactive(input$do_cibersort,{
      # Due to dplyr issue #318, we need temp variables for input values
      #    if (input$data_source_cibersort == "ALL") {
      #      data_source1 <- c("Auslander","Hugo","Riaz","Van Allen")
      #    } else {
      data_source1 <- input$data_source_cibersort
      #    }
      treatment1 <- input$treatment_cibersort
      description1 <- input$description_cibersort
      if (input$immucell_cibersort==""){
        genes1 <- c("PatientID","data_source","treatment","description","PFS.status","RECIST","OS.status","OS","PFS","Age")
      } else {
        genes1 <- c("PatientID","data_source","treatment","description","PFS.status","RECIST","OS.status","OS","PFS","Age",input$immucell_cibersort)
      }
      
      # Apply filters
      #    m <- database.test.process() %>% 
      #      filter(
      #        data_source %in% data_source1,
      #        treatment %in% treatment1,
      #        description %in% description1
      #      ) %>%
      #      select(genes1)
      a<-data.table(database.test.process())
      m<-a[data_source %in% data_source1 & treatment %in% treatment1 & description %in% description1]
      m<-m[,colnames(m) %in% genes1,with=FALSE]
      if (ncol(m) == 11) {
        colnames(m)[11] <- "UserImmu"
        as.data.frame(m)
      } else {
        as.data.frame(m)
      }
    })
    ##########
    #### cibersort plots
    ##########
    output$cibersort_plots_n_samples_NA <- renderText({
      input$do_cibersort
      isolate({
        nrow(selecteddatabase_cibersort())
      })
    })
    output$cibersort_plots_plots <- renderUI({
      input$do_cibersort
      isolate({
        input_immucell_cibersort <- input$immucell_cibersort
        selected.database <- selecteddatabase_cibersort()
        if (nrow(selected.database)==0 | nrow(selected.database[!is.na(selected.database[,11]),])==0) {
          tagList(plotlyOutput("cibersort_plots_plots0",height="auto"))
        } else {
          if (length(input$data_source_cibersort)==1) {
            tagList(plotlyOutput("cibersort_plots_plots1",height="auto"),tags$h1("  "))
          } else if (length(input$data_source_cibersort)==2) {
            tagList(plotlyOutput("cibersort_plots_plots1",height="auto"),tags$h1("  "),
                    plotlyOutput("cibersort_plots_plots2",height="auto"),tags$h1("  "))
          } else if (length(input$data_source_cibersort)==3) {
            tagList(plotlyOutput("cibersort_plots_plots1",height="auto"),tags$h1("  "),
                    plotlyOutput("cibersort_plots_plots2",height="auto"),tags$h1("  "),
                    plotlyOutput("cibersort_plots_plots3",height="auto"),tags$h1("  "))
          } else if (length(input$data_source_cibersort)==4) {
            tagList(plotlyOutput("cibersort_plots_plots1",height="auto"),tags$h1("  "),
                    plotlyOutput("cibersort_plots_plots2",height="auto"),tags$h1("  "),
                    plotlyOutput("cibersort_plots_plots3",height="auto"),tags$h1("  "),
                    plotlyOutput("cibersort_plots_plots4",height="auto"),tags$h1("  "))
          } else if (length(input$data_source_cibersort)==5) {
            tagList(plotlyOutput("cibersort_plots_plots1",height="auto"),tags$h1("  "),
                    plotlyOutput("cibersort_plots_plots2",height="auto"),tags$h1("  "),
                    plotlyOutput("cibersort_plots_plots3",height="auto"),tags$h1("  "),
                    plotlyOutput("cibersort_plots_plots4",height="auto"),tags$h1("  "),
                    plotlyOutput("cibersort_plots_plots5",height="auto"),tags$h1("  "))
          } 
        }
      })
    })
    observeEvent(input$do_cibersort,{
      input$do_cibersort
      isolate({
        input_immucell_cibersort <- input$immucell_cibersort
        selected.database <- selecteddatabase_cibersort()
        if (nrow(selected.database)==0 | nrow(selected.database[!is.na(selected.database[,11]),])==0) {
          output[["cibersort_plots_plots0"]] <- renderPlotly({
            plot_ly () %>% 
              layout(title = paste(input$data_source_cibersort,collapse = ","),font=list(size=15,family="Arial")) %>%
              add_annotations(x=2,y=2,text="no data available under this selection",showarrow=F,font=list(size=16,color="purple")) 
          })
        } else {
          selected.database <- data.frame(selected.database,Groups=selected.database$PFS.status)
          splitted_list <- selected.database %>% as.matrix %>% as.data.frame %>% split(.$data_source)
          splitted_list <- splitted_list[input$data_source_cibersort]
          for (k in 1:length(input$data_source_cibersort)) {
            local({
              my_i <- k
              cibersort_plots_plots_name <- paste0("cibersort_plots_plots", my_i)
              d<-splitted_list[[my_i]]
              output[[cibersort_plots_plots_name]] <- renderPlotly({
                if (is.na(names(splitted_list)[my_i])) {
                  plot_ly () %>% 
                    layout(title = input$data_source_cibersort[[my_i]],font=list(size=15,family="Arial")) %>%
                    add_annotations(x=2,y=2,text="no data available under this selection",showarrow=F,font=list(size=16,color="purple")) 
                } else {
                  #d <- splitted_list[[my_i]]
                  if (nrow(d[!is.na(d[,"PFS.status"]) & !is.na(d[,11]),])==0) {
                    plot_ly () %>% 
                      layout(title = unique(d$data_source),font=list(size=15,family="Arial")) %>%
                      add_annotations(x=2,y=2,text="no immunotherapy response under this selection",showarrow=F,font=list(size=16,color="purple")) 
                  } else {
                    d[,"Groups"] <- as.character(d[,"Groups"])
                    d[d[,"Groups"]=="response" & !is.na(d[,"Groups"]),"Groups"] <- paste("response (n=",nrow(d[d[,"Groups"]=="response",]),")")
                    d[d[,"Groups"]=="nonresponse" & !is.na(d[,"Groups"]),"Groups"] <- paste("nonresponse (n=",nrow(d[d[,"Groups"]=="nonresponse",]),")")
                    ind <- unique(d[,"Groups"])
                    try.test <- try(p.v <- wilcox.test(as.numeric(d[d[,"Groups"]==ind[1],"UserImmu"]),as.numeric(d[d[,"Groups"]==ind[2],"UserImmu"]))$p.value)
                    if (class(try.test)=="try-error") {
                      p.v <- "NA"
                    } else {
                      p.v <- wilcox.test(as.numeric(d[d[,"Groups"]==ind[1],"UserImmu"]),as.numeric(d[d[,"Groups"]==ind[2],"UserImmu"]))$p.value
                      if (p.v < 0.001) {
                        p.v <- "< 0.001"
                      } else {
                        p.v<-round(p.v,3)
                      } 
                    }
                    plot_ly(data=d,x=~Groups,
                            y=~UserImmu,
                            type="box",
                            boxpoints="all",
                            jitter=0.3,
                            pointpos=-1.8,
                            #mode="markders",
                            color= ~PFS.status,
                            colors=c("plum","lightgreen"),
                            #hoveron="points+boxes",
                            legendgroup=~PFS.status,
                            showlegend=TRUE,
                            text= ~paste0("Sample ID: ", PatientID,
                                          "<br>Immune cell: ",input_immucell_cibersort," :",UserImmu,
                                          "<br>Treatment: ",treatment,
                                          "<br>Description: ",description)) %>%
                      layout(title = unique(d$data_source),titlefont=list(size=20,color="black",family= "Arial"),
                             height = 480,
                             xaxis=list(title="Immunotherapy response",titlefont=list(size=20,color="black",family= "Aril"),
                                        showticklabels=TRUE,tickcolor="#444",linecolor = "#444"),
                             yaxis=list(title=paste0(input_immucell_cibersort," component"),
                                        titlefont=list(size=20,color="black",family= "Aril"),
                                        showticklabels=TRUE,tickmode="auto",nticks=4,tickcolor="#444",linecolor = "#444"),
                             annotations=list(x=0.5,y=max(na.omit(as.numeric(d[,"UserImmu"]))),text=paste0("p-value = ",p.v),xref="x",yref="y",
                                              showarrow=FALSE))
                  } 
                  
                }
              })
            })
          }
        }
      })
    })
    
    output$cibersort_plots_download <- downloadHandler(
      filename = function() {
        if (input$cibersort_plots_type == "Excel (CSV)") {
          "newfile.csv"
        } else if (input$cibersort_plots_type == "Text (TSV)") {
          "newfile.txt"
        } else if (input$cibersort_plots_type == "JSON") {
          "newfile.json"
        }
      },
      content = function(file) {
        if (input$cibersort_plots_type == "Excel (CSV)") {
          write.csv (cibersort_plots_download_data(),file,row.names = FALSE)
        } else if (input$cibersort_plots_type == "Text (TSV)") {
          write.table (cibersort_plots_download_data(),file,quote=F, row.names = FALSE, sep="\t")
        } else if (input$cibersort_plots_type == "JSON") {
          write(toJSON(cibersort_plots_download_data()),file)
        }
      }
    )  
    cibersort_plots_download_data <- function() {
      input_immucell_cibersort <- input$immucell_cibersort
      m<-selecteddatabase_cibersort()
      if (input_immucell_cibersort==""){
        m<-m[,c(1:7,10)]
        m
      } else {
        colnames(m)[11] <- input_immucell_cibersort
        m<-m[,c(1:7,10:11)]
        m
      }
    }
    output$cibersort_plots_table <- DT::renderDataTable({
      input$do_cibersort
      isolate({
        cibersort_plots_download_data()
      })
    })    
    #############
    #### cibersort survival
    #############
    output$cibersort_survival_n_samples_NA <- renderText({
      input$do_cibersort
      isolate({
        nrow(selecteddatabase_cibersort())
      })
    })
    
    output$cibersort_survival_plots <- renderUI({
      input$do_cibersort
      isolate({
        selected.database <- selecteddatabase_cibersort()
        if (nrow(selected.database)==0 | nrow(selected.database[!is.na(selected.database[,11]),])==0) {
          tagList(plotlyOutput("cibersort_survival_plots0",height="auto"))
        } else {
          if (length(input$data_source_cibersort)==1) {
            tagList(plotlyOutput("cibersort_survival_plots1",height="auto"),tags$h1(" "))
          } else if (length(input$data_source_cibersort)==2) {
            tagList(plotlyOutput("cibersort_survival_plots1",height="auto"),tags$h1(" "),
                    plotlyOutput("cibersort_survival_plots2",height="auto"),tags$h1(" "))
          } else if (length(input$data_source_cibersort)==3) {
            tagList(plotlyOutput("cibersort_survival_plots1",height="auto"),tags$h1(" "),
                    plotlyOutput("cibersort_survival_plots2",height="auto"),tags$h1(" "),
                    plotlyOutput("cibersort_survival_plots3",height="auto"),tags$h1(" "))
          } else if (length(input$data_source_cibersort)==4) {
            tagList(plotlyOutput("cibersort_survival_plots1",height="auto"),tags$h1(" "),
                    plotlyOutput("cibersort_survival_plots2",height="auto"),tags$h1(" "),
                    plotlyOutput("cibersort_survival_plots3",height="auto"),tags$h1(" "),
                    plotlyOutput("cibersort_survival_plots4",height="auto"),tags$h1(" "))
          } else if (length(input$data_source_cibersort)==5) {
            tagList(plotlyOutput("cibersort_survival_plots1",height="auto"),tags$h1(" "),
                    plotlyOutput("cibersort_survival_plots2",height="auto"),tags$h1(" "),
                    plotlyOutput("cibersort_survival_plots3",height="auto"),tags$h1(" "),
                    plotlyOutput("cibersort_survival_plots4",height="auto"),tags$h1(" "),
                    plotlyOutput("cibersort_survival_plots5",height="auto"),tags$h1(" "))
          } 
        }
      })
    })
    
    
    observeEvent(input$do_cibersort,{
      input$do_cibersort
      isolate({
        input_immucell_cibersort <- input$immucell_cibersort
        selected.database <- selecteddatabase_cibersort()
        if (nrow(selected.database)==0 | nrow(selected.database[!is.na(selected.database[,11]),])==0) {
          output[["cibersort_survival_plots0"]] <- renderPlotly({
            plot_ly () %>% 
              layout(title = paste(input$data_source_cibersort,collapse = ","),font=list(size=15,family="Arial")) %>%
              add_annotations(x=2,y=2,text="no data available under this selection",showarrow=F,font=list(size=16,color="purple")) 
          })
        } else {
          splitted_list <- selected.database %>% as.matrix %>% as.data.frame %>% split(.$data_source)
          splitted_list <- splitted_list[input$data_source_cibersort]
          for (k in 1:length(input$data_source_cibersort)) {
            local({
              my_i <- k
              cibersort_survival_plots_name <- paste0("cibersort_survival_plots", my_i)
              d<-splitted_list[[my_i]]
              d[,"UserImmu"] <- as.numeric(as.character(d[,"UserImmu"]))
              output[[cibersort_survival_plots_name]] <- renderPlotly({
                if (is.na(names(splitted_list)[my_i])) {
                  plot_ly () %>% 
                    layout(title = input$data_source_cibersort[[my_i]],font=list(size=15,family="Arial")) %>%
                    add_annotations(x=2,y=2,text="no data available under this selection",showarrow=F,font=list(size=16,color="purple")) 
                } else {
                  
                  if (nrow(d[!is.na(d[,"OS.status"]),])==0 | nrow(d[!is.na(d[,"OS"]),])==0 | nrow(d[!is.na(d[,"UserImmu"]),])==0 | sum(d[!is.na(d[,"UserImmu"]),"UserImmu"])==0) {
                    plot_ly () %>% 
                      layout(title = unique(d$data_source),font=list(size=15,family="Arial")) %>%
                      add_annotations(x=2,y=2,text="no overall survival data available under this selection",showarrow=F,font=list(size=16,color="purple")) 
                  } else {
                    
                    d[d[,"OS.status"]=="living" & !is.na(d[,"OS.status"]),"dead"] <- 0
                    d[d[,"OS.status"]=="deceased" & !is.na(d[,"OS.status"]),"dead"] <- 1
                    d <- d[!is.na(d[,"UserImmu"]) & !is.na(d[,"dead"]),]
                    d <- d[d[,"dead"]==1 | d[,"dead"]==0,]
                    
                    d<-data.frame(d[,1:6],unfactor(d[,c("dead","OS","PFS","Age","UserImmu")]))
                    d<-data.frame(d,IND=paste0("samples with bottom ",
                                               input$slider_cibersort_OS*100, 
                                               "% ",
                                               input_immucell_cibersort, " weight",
                                               " (",
                                               nrow(d[d[,"UserImmu"]<= quantile(d[,"UserImmu"],input$slider_cibersort_OS),]),
                                               ")"))
                    d[,"IND"]<-as.character(d[,"IND"])
                    d[d[,"UserImmu"]> quantile(d[,"UserImmu"],input$slider_cibersort_OS),"IND"]<- 
                      paste0("samples with top ",
                             (1-input$slider_cibersort_OS)*100,
                             "% ",
                             input_immucell_cibersort, " weight",
                             " (",
                             nrow(d[d[,"UserImmu"]> quantile(d[,"UserImmu"],input$slider_cibersort_OS),])
                             ,")")
                    d[,"dead"] <- as.numeric(d[,"dead"]) ### I don't know why I need to as.numeric since expression section doesn't need this
                    d<-d[order(d[,"IND"]),]
                    fit.result <- survfit(Surv(OS,dead)~IND,data=d)
                    plot.result <- ggsurv(fit.result,surv.col = c("plum","lightgreen"))
                    diff.result <- survdiff(Surv(OS,dead)~IND,data=d)
                    p.value= 1 - pchisq(diff.result$chisq, length(diff.result$n) - 1)
                    plot.result.ann <- plot.result + theme(legend.position = "right", legend.title = element_blank()) + 
                      ggplot2::geom_text(aes(label = sprintf("p-value = %0.2g",
                                                             pchisq(diff.result$chisq,df=1,lower.tail = F)),x=200,y=0.05))
                    p1<-ggplotly(plot.result.ann,legendgroup=~IND,height = 480 ) %>%
                      layout(title = unique(d$data_source),titlefont=list(size=20,color="black",family= "Arial"),
                             xaxis=list(title="Days",titlefont=list(size=20,color="black",family= "Aril black")),
                             yaxis=list(title="Overall survival proportion",titlefont=list(size=20,color="black",family= "Aril black"))) #%>%
                    p1
                  }
                }
              })
            })
          }
        }
      })
    })
    
    output$cibersort_survival_download <- downloadHandler(
      filename = function() {
        if (input$cibersort_survival_type == "Excel (CSV)") {
          "newfile.csv"
        } else if (input$cibersort_survival_type == "Text (TSV)") {
          "newfile.txt"
        } else if (input$cibersort_survival_type == "JSON") {
          "newfile.json"
        }
      },
      content = function(file) {
        if (input$cibersort_survival_type == "Excel (CSV)") {
          write.csv (cibersort_survival_download_data(),file,row.names = FALSE)
        } else if (input$cibersort_survival_type == "Text (TSV)") {
          write.table (cibersort_survival_download_data(),file,quote=F, row.names = FALSE, sep="\t")
        } else if (input$cibersort_survival_type == "JSON") {
          write(toJSON(cibersort_survival_download_data()),file)
        }
      }
    )
    cibersort_survival_download_data<-function() {
      input_immucell_cibersort <- input$immucell_cibersort
      m<-selecteddatabase_cibersort()
      #    m <- m[!is.na(m[,"UserGene"]),]
      if (nrow(m)>0 ) {
        m<-data.frame(m,IND=NA)
        m<-m[!is.na(m[,"UserImmu"]),]
        m1 <- m %>% split(.$data_source) %>% 
          lapply(function(d) {
            d[!is.na(d[,"UserImmu"]) & d[,"UserImmu"]> quantile(d[,"UserImmu"],input$slider_cibersort_OS),"IND"] <- paste0("samples with top ",
                                                                                                                           (1-input$slider_cibersort_OS)*100,
                                                                                                                           "% ",
                                                                                                                           input$immucell_cibersort, " weight")
            d[!is.na(d[,"UserImmu"]) & d[,"UserImmu"]<=quantile(d[,"UserImmu"],input$slider_cibersort_OS),"IND"] <- paste0("samples with bottom ",
                                                                                                                           input$slider_cibersort_OS*100,
                                                                                                                           "% ",
                                                                                                                           input$immucell_cibersort, " weight")
            
            d })
        m2 <- ldply(m1,data.frame)
        m2 <- m2[,-1]
        if (input_immucell_cibersort==""){
          m2<-m2[,c(1:8,10:11)]
          m2
        } else {
          colnames(m2)[11] <- input_immucell_cibersort
          m2<-m2[,c(1:8,10:12)]
          m2
        }
        
      } else {
        #    m <- data.frame(m,IND=NA) #remove "IND" column, because "Error in [.data.frame: undefined columns selected"
        m2<-m
        if (input_immucell_cibersort==""){
          m2<-m2[,c(1:8,10)]
          m2
        } else {
          colnames(m2)[11] <- input_immucell_cibersort
          m2<-m2[,c(1:8,10:11)]
          m2
        }
      }
    }
    output$cibersort_survival_table <- DT::renderDataTable({
#      input$do_cibersort
#      isolate({
        cibersort_survival_download_data()
#      })
    })
    #############
    #### cibersort PFS
    #############
    output$cibersort_PFS_plots <- renderUI({
      input$do_cibersort
      isolate({
        selected.database <- selecteddatabase_cibersort()
        if (nrow(selected.database)==0 | nrow(selected.database[!is.na(selected.database[,11]),])==0) {
          tagList(plotlyOutput("cibersort_PFS_plots0",height="auto"))
        } else {
          if (length(input$data_source_cibersort)==1) {
            tagList(plotlyOutput("cibersort_PFS_plots1",height="auto"),tags$h1(" "))
          } else if (length(input$data_source_cibersort)==2) {
            tagList(plotlyOutput("cibersort_PFS_plots1",height="auto"),tags$h1(" "),
                    plotlyOutput("cibersort_PFS_plots2",height="auto"),tags$h1(" "))
          } else if (length(input$data_source_cibersort)==3) {
            tagList(plotlyOutput("cibersort_PFS_plots1",height="auto"),tags$h1(" "),
                    plotlyOutput("cibersort_PFS_plots2",height="auto"),tags$h1(" "),
                    plotlyOutput("cibersort_PFS_plots3",height="auto"),tags$h1(" "))
          } else if (length(input$data_source_cibersort)==4) {
            tagList(plotlyOutput("cibersort_PFS_plots1",height="auto"),tags$h1(" "),
                    plotlyOutput("cibersort_PFS_plots2",height="auto"),tags$h1(" "),
                    plotlyOutput("cibersort_PFS_plots3",height="auto"),tags$h1(" "),
                    plotlyOutput("cibersort_PFS_plots4",height="auto"),tags$h1(" "))
          } else if (length(input$data_source_cibersort)==5) {
            tagList(plotlyOutput("cibersort_PFS_plots1",height="auto"),tags$h1(" "),
                    plotlyOutput("cibersort_PFS_plots2",height="auto"),tags$h1(" "),
                    plotlyOutput("cibersort_PFS_plots3",height="auto"),tags$h1(" "),
                    plotlyOutput("cibersort_PFS_plots4",height="auto"),tags$h1(" "),
                    plotlyOutput("cibersort_PFS_plots5",height="auto"),tags$h1(" "))
          } 
        }
      })
    })
    
    
    observeEvent(input$do_cibersort,{
      input$do_cibersort
      isolate({
        input_immucell_cibersort <- input$immucell_cibersort
        selected.database <- selecteddatabase_cibersort()
        if (nrow(selected.database)==0 | nrow(selected.database[!is.na(selected.database[,11]),])==0) {
          output[["cibersort_PFS_plots0"]] <- renderPlotly({
            plot_ly () %>% 
              layout(title = paste(input$data_source_cibersort,collapse = ","),font=list(size=15,family="Arial")) %>%
              add_annotations(x=2,y=2,text="no data available under this selection",showarrow=F,font=list(size=16,color="purple")) 
          })
        } else {
          splitted_list <- selected.database %>% as.matrix %>% as.data.frame %>% split(.$data_source)
          splitted_list <- splitted_list[input$data_source_cibersort]
          for (k in 1:length(input$data_source_cibersort)) {
            local({
              my_i <- k
              cibersort_PFS_plots_name <- paste0("cibersort_PFS_plots", my_i)
              d<-splitted_list[[my_i]]
              d[,"UserImmu"] <- as.numeric(as.character(d[,"UserImmu"]))
              output[[cibersort_PFS_plots_name]] <- renderPlotly({
                if (is.na(names(splitted_list)[my_i])) {
                  plot_ly () %>% 
                    layout(title = input$data_source_cibersort[[my_i]],font=list(size=15,family="Arial")) %>%
                    add_annotations(x=2,y=2,text="no data available under this selection",showarrow=F,font=list(size=16,color="purple")) 
                } else {
                  
                  if (nrow(d[!is.na(d[,"PFS.status"]),])==0 | nrow(d[!is.na(d[,"PFS"]),])==0 | nrow(d[!is.na(d[,"UserImmu"]),])==0 | sum(d[!is.na(d[,"UserImmu"]),"UserImmu"])==0) {
                    plot_ly () %>% 
                      layout(title = unique(d$data_source),font=list(size=15,family="Arial")) %>%
                      add_annotations(x=2,y=2,text="no PFS outcome under this selection",showarrow=F,font=list(size=16,color="purple")) 
                  } else {
                    d <- d[!is.na(d[,"UserImmu"]) & !is.na(d[,"PFS"]) & !is.na(d[,"PFS.status"]),]
                    
                    d<-data.frame(d[,1:6],unfactor(d[,c("OS.status","OS","PFS","Age","UserImmu")]))
                    d<-data.frame(d,IND=paste0("samples with bottom ",
                                               input$slider_cibersort_PFS*100, 
                                               "% ",
                                               input_immucell_cibersort, " weight",
                                               " (",
                                               nrow(d[d[,"UserImmu"]<= quantile(d[,"UserImmu"],input$slider_cibersort_PFS),]),
                                               ")"))
                    d[,"IND"]<-as.character(d[,"IND"])
                    d[d[,"UserImmu"]> quantile(d[,"UserImmu"],input$slider_cibersort_PFS),"IND"]<- 
                      paste0("samples with top ",
                             (1-input$slider_cibersort_PFS)*100,
                             "% ",
                             input_immucell_cibersort," weight",
                             " (",
                             nrow(d[d[,"UserImmu"]> quantile(d[,"UserImmu"],input$slider_cibersort_PFS),])
                             ,")")
                    d <- data.frame(d,group.IND=0)
                    d[d[,"PFS.status"]=="nonresponse","group.IND"] <- 1
                    
                    d<-d[order(d[,"IND"]),]
                    fit.result <- survfit(Surv(PFS,group.IND)~IND,data=d)
                    plot.result <- ggsurv(fit.result,surv.col = c("plum","lightgreen"))
                    diff.result <- survdiff(Surv(PFS,group.IND)~IND,data=d)
                    p.value= 1 - pchisq(diff.result$chisq, length(diff.result$n) - 1)
                    plot.result.ann <- plot.result + theme(legend.position = "right", legend.title = element_blank()) +  
                      ggplot2::geom_text(aes(label = sprintf("p-value = %0.2g",
                                                             pchisq(diff.result$chisq,df=1,lower.tail = F)),x=200,y=0.05))
                    p1<-ggplotly(plot.result.ann,legendgroup=~IND,height = 480 ) %>%
                      layout(title = unique(d$data_source),titlefont=list(size=20,color="black",family= "Arial"),
                             xaxis=list(title="Days",titlefont=list(size=20,color="black",family= "Aril black")),
                             yaxis=list(title="Progression free survival proportion",titlefont=list(size=20,color="black",family= "Aril black"))) #%>%
                    p1
                  }
                }
              })
            })
          }
        }
      })
    })
    
    output$cibersort_PFS_n_samples_NA <- renderText({
      nrow(selecteddatabase_cibersort())
    })
    output$cibersort_PFS_download <- downloadHandler(
      filename = function() {
        if (input$cibersort_PFS_type == "Excel (CSV)") {
          "newfile.csv"
        } else if (input$cibersort_PFS_type == "Text (TSV)") {
          "newfile.txt"
        } else if (input$cibersort_PFS_type == "JSON") {
          "newfile.json"
        }
      },
      content = function(file) {
        if (input$cibersort_PFS_type == "Excel (CSV)") {
          write.csv (cibersort_PFS_download_data(),file,row.names = FALSE)
        } else if (input$cibersort_PFS_type == "Text (TSV)") {
          write.table (cibersort_PFS_download_data(),file,quote=F, row.names = FALSE, sep="\t")
        } else if (input$cibersort_PFS_type == "JSON") {
          write(toJSON(cibersort_PFS_download_data()),file)
        }
      }
    )
    cibersort_PFS_download_data <- function () {
      input_immucell_cibersort <- input$immucell_cibersort
      m<-selecteddatabase_cibersort()
      #    m <- m[!is.na(m[,"UserGene"]),]
      if (nrow(m)>0) {
        m<-as.data.frame(m,IND=NA)
        m<-m[!is.na(m[,"UserImmu"]),]
        m1 <- m %>% split(.$data_source) %>% 
          lapply(function(d) {
            d[!is.na(d[,"UserImmu"]) & d[,"UserImmu"]> quantile(d[,"UserImmu"],input$slider_cibersort_PFS),"IND"] <- paste0("samples with top ",
                                                                                                                           (1-input$slider_cibersort_PFS)*100,
                                                                                                                           "% ",
                                                                                                                           input$immucell_cibersort, " weight")
            d[!is.na(d[,"UserImmu"]) & d[,"UserImmu"]<=quantile(d[,"UserImmu"],input$slider_cibersort_PFS),"IND"] <- paste0("samples with bottom ",
                                                                                                                           input$slider_cibersort_PFS*100,
                                                                                                                           "% ",
                                                                                                                           input$immucell_cibersort, " weight")
            
            d })
        m2 <- ldply(m1,data.frame)
        m2 <- m2[,-1]
        
        #      m2<-ldply(m1,data.frame)
        #      m2<-m2[,-1]
        if (input_immucell_cibersort==""){
          m2<-m2[,c(1:6,9:11)]
          m2
        } else {
          colnames(m2)[11] <- input_immucell_cibersort
          m2<-m2[,c(1:6,9:12)]
          m2
        }
      } else {
        #    m <- data.frame(m,IND=NA) #remove "IND" column, because "Error in [.data.frame: undefined columns selected"
        m2<-m
        if (input_immucell_cibersort==""){
          m2<-m2[,c(1:6,9:10)]
          m2
        } else {
          colnames(m2)[11] <- input_immucell_cibersort
          m2<-m2[,c(1:6,9:11)]
          m2
        }
      }
    }
    output$cibersort_PFS_table <- DT::renderDataTable({
#      input$do_cibersort
#      isolate({
        cibersort_PFS_download_data()
#      })
    })
    ############################################
    #### multi-dimensions
    #############################################
    
    output$data_source_multi_dimensions <- renderUI({
      all.data.source <- as.character(unique(database.test.process()[,"data_source"]))
      user.data.source <- all.data.source[!(all.data.source %in% c("Robert","Snyder","Moshe","Auslander","Chen","Hugo","Prat","Riaz","Van Allen","Krieg_panel_1","Krieg_panel_2","Krieg_panel_3"))]
      selectInput("data_source_multi_dimensions",
                  "Data source: ",
                  choices = c("Robert","Snyder","Auslander","Chen","Hugo","Prat","Riaz","Van Allen",user.data.source),
                  multiple = TRUE,
                  selected = c("Van Allen","Riaz"))
    })
    output$treatment_multi_dimensions <- renderUI ({
      selectInput("treatment_multi_dimensions",
                  "Treatment: ",
                  #                choices = as.character(unique(filter_input_interactive(filter_input=input$data_source_multi_dimensions)[,"treatment"])),
                  choices = as.character(unique(database.test.process()[,"treatment"])),
                  multiple = TRUE,
                  selected = c("anti-PD-1","anti-CTLA-4","anti-PD-1+anti-CTLA-4"))})
    output$description_multi_dimensions <- renderUI ({
      selectInput("description_multi_dimensions",
                  "Pre- or On-treatment: ",
                  #                choices = as.character(unique(filter_input_interactive(filter_input=c(input$data_source_multi_dimensions,input$treatment_multi_dimensions))[,"description"])),
                  choices = as.character(unique(database.test.process()[,"description"])),
                  multiple = TRUE,
                  selected = c("Pre","On"))})
    
    observeEvent(input$xaxis_multi_dimensions,{
      output$xaxis_gene_multi_dimensions <- renderUI({
        if (input$xaxis_multi_dimensions=="expression" ) {
          selectizeInput("xaxis_gene_multi_dimensions",
                         "Insert a gene, for example: CTLA4",
                         choices=NULL)
        } else if (input$xaxis_multi_dimensions=="expression sum" ) {
          selectizeInput("xaxis_gene_multi_dimensions",
                         "Insert a gene set, for example: PDCD1 CTLA4",
                         choices=NULL,multiple=TRUE)
        } else if (input$xaxis_multi_dimensions=="mutation") {
          selectizeInput("xaxis_gene_multi_dimensions",
                         "Insert a gene(s), for example: BRAF KRAS",
                         choices=NULL,multiple=TRUE)
        } else if (input$xaxis_multi_dimensions=="immune cell components") {
          selectInput("xaxis_gene_multi_dimensions",
                      "Immune cell type: ",
                      choices = sort(c("B.cells.naive","B.cells.memory","Plasma.cells","T.cells.CD8","T.cells.CD4.naive","T.cells.CD4.memory.resting","T.cells.CD4.memory.activated","T.cells.follicular.helper","T.cells.regulatory..Tregs.","T.cells.gamma.delta","NK.cells.resting","NK.cells.activated","Monocytes","Macrophages.M0","Macrophages.M1","Macrophages.M2","Dendritic.cells.resting","Dendritic.cells.activated","Mast.cells.resting","Mast.cells.activated","Eosinophils","Neutrophils")),
                      selected = "T.cells.CD8")
        } else if (input$xaxis_multi_dimensions=="mutational loads") {
          selectInput("xaxis_gene_multi_dimensions",
                      "Mutational loads: ",
                      choices = "Mutation.Load",
                      selected = "Mutation.Load")
        } else if (input$xaxis_multi_dimensions=="mutational signatures") {
          selectInput("xaxis_gene_multi_dimensions",
                      "Mutational signature type: ",
                      choices = sort(c("Signature.1 (age correlated)","Signature.2","Signature.3 (BRCA1 and BRCA2 associated)","Signature.4 (tobacco associated)","Signature.5","Signature.6 (DNA mismatch repair associated)","Signature.7 (ultraviolet associated)","Signature.8","Signature.9","Signature.10","Signature.11","Signature.12","Signature.13","Signature.14 (high tumor mutation burden associated)","Signature.15 (DNA mismatch repair associated)","Signature.16","Signature.17","Signature.18","Signature.19","Signature.20 (DNA mismatch repair associated)","Signature.21","Signature.22","Signature.23","Signature.24","Signature.25 (Hodgkin's cell lines only)","Signature.26 (DNA mismatch repair associated)","Signature.27","Signature.28","Signature.29","Signature.30","unknown")),
                      selected = "Signature.1 (age correlated)")
        }
      })
      if (input$xaxis_multi_dimensions=="expression" ) {
        updateSelectizeInput(session,"xaxis_gene_multi_dimensions",
                             choices=a.colnames,
                             selected = "CTLA4",server=TRUE)
      } else if (input$xaxis_multi_dimensions=="expression sum" ) {
        updateSelectizeInput(session,"xaxis_gene_multi_dimensions",
                             choices=a.colnames,
                             selected = c("PDCD1","CTLA4"),
                             server=TRUE)
      } else if (input$xaxis_multi_dimensions=="mutation") {
        updateSelectizeInput(session,"xaxis_gene_multi_dimensions",
                             choices=a.colnames,
                             selected=c("BRAF","KRAS","HRAS","NRAS","NF1"),server=TRUE)
      } })
    xids<<-NULL
    observeEvent(input$xaxis_add_multi_dimensions,{
      
      if (is.null(xids)) {
        xids <<- 1
      } else {
        xids <<- c(xids,max(xids)+1)
      }
      output$xaxis_gene_multi_dimensions_x <- renderUI({
        tagList(
          lapply(1:length(xids),function(i) {
            #      insertUI(selector="#xaxis_add_multi_dimensions",where="afterEnd",ui=selectizeInput(paste("xaxis_gene_multi_dimensions_",xids[i],sep=""),sprintf("Gene pairs #%d",xids[i]),choices=a.colnames,multiple=TRUE,selected = c("CD2","CD5"),options=list(maxItems=2)))
            selectizeInput(paste("xaxis_gene_multi_dimensions_",xids[i],sep=""),sprintf("Gene pairs #%d",xids[i]),choices=NULL,multiple=TRUE,selected = c("CD2","CD5"),options=list(maxItems=2))
          })
        )
      })
      lapply(1:length(xids),function(i) {
        #      insertUI(selector="#xaxis_add_multi_dimensions",where="afterEnd",ui=selectizeInput(paste("xaxis_gene_multi_dimensions_",xids[i],sep=""),sprintf("Gene pairs #%d",xids[i]),choices=a.colnames,multiple=TRUE,selected = c("CD2","CD5"),options=list(maxItems=2)))
        updateSelectizeInput(session,paste("xaxis_gene_multi_dimensions_",xids[i],sep=""),choices=a.colnames,selected = c("CD2","CD5"),server=TRUE)
      })
      
    })
    
    observeEvent(input$yaxis_multi_dimensions,{
      output$yaxis_gene_multi_dimensions <- renderUI({
        if (input$yaxis_multi_dimensions=="expression" ) {
          selectizeInput("yaxis_gene_multi_dimensions",
                         "Insert a gene, for example: PDCD1",
                         choices=NULL)
        } else if (input$yaxis_multi_dimensions=="expression sum" ) {
          selectizeInput("yaxis_gene_multi_dimensions",
                         "Insert a gene set, for example: CD4,CD6 and CD8",
                         choices=NULL,multiple=TRUE)
        } else if (input$yaxis_multi_dimensions=="mutation") {
          selectizeInput("yaxis_gene_multi_dimensions",
                         "Insert a gene(s), for example: HRAS NRAS",
                         choices=NULL,multiple=TRUE)
        } else if (input$yaxis_multi_dimensions=="immune cell components") {
          selectInput("yaxis_gene_multi_dimensions",
                      "Immune cell type: ",
                      choices = sort(c("B.cells.naive","B.cells.memory","Plasma.cells","T.cells.CD8","T.cells.CD4.naive","T.cells.CD4.memory.resting","T.cells.CD4.memory.activated","T.cells.follicular.helper","T.cells.regulatory..Tregs.","T.cells.gamma.delta","NK.cells.resting","NK.cells.activated","Monocytes","Macrophages.M0","Macrophages.M1","Macrophages.M2","Dendritic.cells.resting","Dendritic.cells.activated","Mast.cells.resting","Mast.cells.activated","Eosinophils","Neutrophils")),
                      selected = "T.cells.CD8")
        } else if (input$yaxis_multi_dimensions=="mutational loads") {
          selectInput("yaxis_gene_multi_dimensions",
                      "Mutational loads: ",
                      choices = "Mutation.Load",
                      selected = "Mutation.Load")
        } else if (input$yaxis_multi_dimensions=="mutational signatures") {
          selectInput("yaxis_gene_multi_dimensions",
                      "Mutational signature type: ",
                      choices = sort(c("Signature.1 (age correlated)","Signature.2","Signature.3 (BRCA1 and BRCA2 associated)","Signature.4 (tobacco associated)","Signature.5","Signature.6 (DNA mismatch repair associated)","Signature.7 (ultraviolet associated)","Signature.8","Signature.9","Signature.10","Signature.11","Signature.12","Signature.13","Signature.14 (high tumor mutation burden associated)","Signature.15 (DNA mismatch repair associated)","Signature.16","Signature.17","Signature.18","Signature.19","Signature.20 (DNA mismatch repair associated)","Signature.21","Signature.22","Signature.23","Signature.24","Signature.25 (Hodgkin's cell lines only)","Signature.26 (DNA mismatch repair associated)","Signature.27","Signature.28","Signature.29","Signature.30","unknown")),
                      selected = "Signature.1 (age correlated)")
        }})
      if (input$yaxis_multi_dimensions=="expression" ) {
        updateSelectizeInput(session,"yaxis_gene_multi_dimensions",
                             choices=a.colnames,
                             selected = "PDCD1",server=TRUE)
      } else if (input$yaxis_multi_dimensions=="expression sum" ) {
        updateSelectizeInput(session,"yaxis_gene_multi_dimensions",
                             choices=a.colnames,
                             selected = c("CD4","CD6","CD8A","CD8B"),
                             server=TRUE)
      } else if (input$yaxis_multi_dimensions=="mutation") {
        updateSelectizeInput(session,"yaxis_gene_multi_dimensions",
                             choices=a.colnames,
                             selected=c("HRAS","NRAS"),server=TRUE)
      }
    })
    yids<<-NULL
    observeEvent(input$yaxis_add_multi_dimensions,{
      
      if (is.null(yids)) {
        yids <<- 1
      } else {
        yids <<- c(yids,max(yids)+1)
      }
      output$yaxis_gene_multi_dimensions_y <- renderUI ({
        tagList(
          lapply(1:length(yids),function(i) {
            #      insertUI(selector="#yaxis_add_multi_dimensions",where="afterEnd",ui=selectizeInput(paste("yaxis_gene_multi_dimensions_",yids[i],sep=""),sprintf("Gene pairs #%d",yids[i]),choices=a.colnames,multiple=TRUE,selected = c("CD4","CD6"),options=list(maxItems=2)))
            selectizeInput(paste("yaxis_gene_multi_dimensions_",yids[i],sep=""),sprintf("Gene pairs #%d",yids[i]),choices=NULL,multiple=TRUE,selected = c("CD4","CD6"),options=list(maxItems=2))
          })
        )
      })
      lapply(1:length(yids),function(i) {
        #      insertUI(selector="#yaxis_add_multi_dimensions",where="afterEnd",ui=selectizeInput(paste("yaxis_gene_multi_dimensions_",yids[i],sep=""),sprintf("Gene pairs #%d",yids[i]),choices=a.colnames,multiple=TRUE,selected = c("CD4","CD6"),options=list(maxItems=2)))
        updateSelectizeInput(session,paste("yaxis_gene_multi_dimensions_",yids[i],sep=""),choices=a.colnames,selected = c("CD4","CD6"),server=TRUE)
      })
      
    })
    
    
    
    selecteddatabase_multi_dimensions <- eventReactive(input$do_multi_dimensions,{
      
      multi_dimensions_expression_pairs <- function(data_source_multi_dimensions,
                                                    treatment_multi_dimensions,
                                                    description_multi_dimensions,
                                                    ids,x_or_y="xaxis_gene_multi_dimensions") {
        # Due to dplyr issue #318, we need temp variables for input values
        data_source1 <- data_source_multi_dimensions
        treatment1 <- treatment_multi_dimensions
        description1 <- description_multi_dimensions
        if (is.null(ids)) {
          genes1 <- c("PatientID","data_source","treatment","description","PFS.status","RECIST","OS.status","OS","PFS","Age")
        } else {
          genessum<-list()
          #        for (i in 1:length(ids)) {
          txt_ids <- sapply(1:length(ids), function(i) paste(x_or_y,ids[i],sep="_"))
          #        }
          for (i in 1:length(txt_ids)) {
            genessum[[i]] <- input[[ txt_ids[i] ]] 
          }
          genes1 <- c("PatientID","data_source","treatment","description","PFS.status","RECIST","OS.status","OS","PFS","Age",unlist(genessum))
        }
        a<-data.table(database.test.process())
        m<-a[data_source %in% data_source1 & treatment %in% treatment1 & description %in% description1]
        m<-m[,colnames(m) %in% genes1,with=FALSE]
        #    m<-m[,colnames(m) %in% genes1,with=FALSE]
        selected.database<-as.data.frame(m)
        
        if (nrow(selected.database)>0) {
          n <- ncol(selected.database)
          for (i in 1:length(txt_ids)) {
            if (length(input[[ txt_ids[i] ]])==1) {
              selected.database <- data.frame(selected.database,NA)
            } else {
              selected.database <- data.frame(selected.database,selected.database[,input[[ txt_ids[i] ]][1]] > selected.database[,input[[ txt_ids[i] ]][2]])
            }
          }
          colnames(selected.database)[(n+1):ncol(selected.database)] <- unlist(lapply(genessum,function(x) paste(x,collapse=".")))
          cols <- sapply(selected.database, is.logical)
          for (j in (n+1):ncol(selected.database)) {
            selected.database[,j] <- as.numeric(selected.database[,j])
          }
          selected.database <- data.frame(selected.database,sum=NA)
          for (i in 1:nrow(selected.database)) {
            selected.database[i,"sum"] <- sum(na.omit(as.numeric(selected.database[i,(n+1):(ncol(selected.database)-1)])))
          }
          selected.database
        }
      }
      # Due to dplyr issue #318, we need temp variables for input values
      #    if (input$data_source_multi_dimensions=="ALL") {
      #      data_source1 <- as.character(unique(database.test.process()[,"data_source"]))
      #    } else {
      data_source1 <- input$data_source_multi_dimensions
      #    }
      treatment1 <- input$treatment_multi_dimensions
      description1 <- input$description_multi_dimensions
      
      if (input$xaxis_multi_dimensions!="mutation" & input$yaxis_multi_dimensions!="mutation") {
        
        if (input$xaxis_multi_dimensions=="mutational signatures" ) {
          xgenessum <- str_split(input$xaxis_gene_multi_dimensions," ",2)[[1]][1]
        } else if (input$xaxis_multi_dimensions=="expression relation pairs") {
          if (is.null(xids)) {
            xgenessum<-NULL
          } else {
            xgenessum<-list()
            #          for (i in 1:length(xids)) {
            txt_xids <- sapply(1:length(xids), function(i) paste("xaxis_gene_multi_dimensions_",xids[i],sep=""))
            #          }
            for (i in 1:length(txt_xids)) {
              xgenessum[[i]] <- input[[ txt_xids[i] ]] 
            }}
        } else {
          xgenessum <- input$xaxis_gene_multi_dimensions
        }
        if (input$yaxis_multi_dimensions=="mutational signatures" ) {
          ygenessum <- str_split(input$yaxis_gene_multi_dimensions," ",2)[[1]][1]
        } else if  (input$yaxis_multi_dimensions=="expression relation pairs") {
          if (is.null(yids)) {
            ygenessum<-NULL
          } else {
            ygenessum<-list()
            #          for (i in 1:length(yids)) {
            txt_yids <- sapply(1:length(yids), function(i) paste("yaxis_gene_multi_dimensions_",yids[i],sep=""))
            #          }
            for (i in 1:length(txt_yids)) {
              ygenessum[[i]] <- input[[ txt_yids[i] ]] 
            }}
        } else {
          ygenessum <- input$yaxis_gene_multi_dimensions
        }
        genes1 <- c("PatientID","data_source","treatment","description","PFS.status","RECIST","OS.status","OS","PFS","Age",xgenessum,ygenessum)
        a<-data.table(database.test.process())
        m<-a[data_source %in% data_source1 & treatment %in% treatment1 & description %in% description1]
        m<-m[,colnames(m) %in% genes1,with=FALSE]
        m<-as.data.frame(m)
        if (input$xaxis_multi_dimensions=="expression relation pairs" & input$yaxis_multi_dimensions=="expression relation pairs") {
          a1 <- multi_dimensions_expression_pairs(data_source1,#input$data_source_multi_dimensions,
                                                  treatment1,#input$treatment_multi_dimensions,
                                                  description1,#input$description_multi_dimensions,
                                                  ids=xids,x_or_y="xaxis_gene_multi_dimensions")
          colnames(a1)[ncol(a1)] <- "sumx"
          a2 <- multi_dimensions_expression_pairs(data_source1,#input$data_source_multi_dimensions,
                                                  treatment1,#input$treatment_multi_dimensions,
                                                  description1,#input$description_multi_dimensions,
                                                  ids=yids,x_or_y="yaxis_gene_multi_dimensions")
          colnames(a2)[ncol(a2)] <- "sumy"
          a<- cbind(a1,a2)
          a<-a[,!duplicated(colnames(a))]
          as.data.frame(a)
        } else if (input$xaxis_multi_dimensions=="expression relation pairs" & input$yaxis_multi_dimensions!="expression relation pairs") {
          a1 <- multi_dimensions_expression_pairs(data_source1,#input$data_source_multi_dimensions,
                                                  treatment1,#input$treatment_multi_dimensions,
                                                  description1,#input$description_multi_dimensions,
                                                  ids=xids,x_or_y="xaxis_gene_multi_dimensions")
          colnames(a1)[ncol(a1)] <- "sumx"
          m<-data.frame(m,sumy=apply(as.matrix(m[,ygenessum]),1,function(x) sum(na.omit(x))))
          if (nrow(m)==0) {
            m<-as.data.frame(m)
          } else {
            m[apply(as.matrix(m[,ygenessum]),1,function(x) length(na.omit(x))==0),"sumy"]<-NA
            m<-as.data.frame(m)}
          a<- cbind(a1,m)
          a<-a[,!duplicated(colnames(a))]
          as.data.frame(a)
        } else if (input$xaxis_multi_dimensions!="expression relation pairs" & input$yaxis_multi_dimensions=="expression relation pairs") {
          a1 <- multi_dimensions_expression_pairs(data_source1,#input$data_source_multi_dimensions,
                                                  treatment1,#input$treatment_multi_dimensions,
                                                  description1,#input$description_multi_dimensions,
                                                  ids=yids,x_or_y="yaxis_gene_multi_dimensions")
          colnames(a1)[ncol(a1)] <- "sumy"
          m<-data.frame(m,sumx=apply(as.matrix(m[,xgenessum]),1,function(x) sum(na.omit(x))))
          if (nrow(m)==0) {
            m<-as.data.frame(m)
          } else {
            m[apply(as.matrix(m[,xgenessum]),1,function(x) length(na.omit(x))==0),"sumx"]<-NA
            m<-as.data.frame(m)}
          a<- cbind(a1,m)
          a<-a[,!duplicated(colnames(a))]
          as.data.frame(a)
        } else {
          m<-data.frame(m,sumx=apply(as.matrix(m[,xgenessum]),1,function(x) sum(na.omit(x))),sumy=apply(as.matrix(m[,ygenessum]),1,function(x) sum(na.omit(x))))
          if (nrow(m)==0) {
            as.data.frame(m)
          } else {
            m[apply(as.matrix(m[,xgenessum]),1,function(x) length(na.omit(x))==0),"sumx"]<-NA
            m[apply(as.matrix(m[,ygenessum]),1,function(x) length(na.omit(x))==0),"sumy"]<-NA
            as.data.frame(m)} }
      } else if (input$xaxis_multi_dimensions=="mutation" & input$yaxis_multi_dimensions!="mutation"){
        if (input$yaxis_multi_dimensions=="mutational signatures" ) {
          ygenessum <- str_split(input$yaxis_gene_multi_dimensions," ",2)[[1]][1]
        } else if  (input$yaxis_multi_dimensions=="expression relation pairs") {
          if (is.null(yids)) {
            ygenessum<-NULL
          } else {
            ygenessum<-list()
            #            for (i in 1:length(yids)) {
            txt_yids <- sapply(1:length(yids), function(i) paste("yaxis_gene_multi_dimensions_",yids[i],sep=""))
            #            }
            for (i in 1:length(txt_yids)) {
              ygenessum[[i]] <- input[[ txt_yids[i] ]] 
            }}
        } else {
          ygenessum <- input$yaxis_gene_multi_dimensions
        }
        genes1 <- c("PatientID","data_source","treatment","description","PFS.status","RECIST","OS.status","OS","PFS","Age",ygenessum)
        a<-data.table(database.test.process())
        m<-a[data_source %in% data_source1 & treatment %in% treatment1 & description %in% description1]
        m<-m[,colnames(m) %in% genes1,with=FALSE]
        m<-as.data.frame(m)
        if (input$yaxis_multi_dimensions=="expression relation pairs") {
          a1 <- multi_dimensions_expression_pairs(data_source1,#input$data_source_multi_dimensions,
                                                  treatment1,#input$treatment_multi_dimensions,
                                                  description1,#input$description_multi_dimensions,
                                                  ids=yids,x_or_y="yaxis_gene_multi_dimensions")
          colnames(a1)[ncol(a1)] <- "sumy"
          a1
        } else {
          m<-data.frame(m,sumy=apply(as.matrix(m[,ygenessum]),1,function(x) sum(na.omit(x))))
          if (nrow(m)==0) {
            as.data.frame(m)
          } else {
            m[apply(as.matrix(m[,ygenessum]),1,function(x) length(na.omit(x))==0),"sumy"]<-NA
            as.data.frame(m)}}
      } else if (input$xaxis_multi_dimensions!="mutation" & input$yaxis_multi_dimensions=="mutation"){
        if (input$xaxis_multi_dimensions=="mutational signatures" ) {
          xgenessum <- str_split(input$xaxis_gene_multi_dimensions," ",2)[[1]][1]
        } else if (input$xaxis_multi_dimensions=="expression relation pairs") {
          if (is.null(xids)) {
            xgenessum<-NULL
          } else {
            xgenessum<-list()
            #          for (i in 1:length(xids)) {
            txt_xids <- sapply(1:length(xids), function(i) paste("xaxis_gene_multi_dimensions_",xids[i],sep=""))
            #          }
            for (i in 1:length(txt_xids)) {
              xgenessum[[i]] <- input[[ txt_xids[i] ]] 
            }}
        } else {
          xgenessum <- input$xaxis_gene_multi_dimensions
        }
        genes1 <- c("PatientID","data_source","treatment","description","PFS.status","RECIST","OS.status","OS","PFS","Age",xgenessum)
        a<-data.table(database.test.process())
        m<-a[data_source %in% data_source1 & treatment %in% treatment1 & description %in% description1]
        m<-m[,colnames(m) %in% genes1,with=FALSE]
        m<-as.data.frame(m)
        if (input$xaxis_multi_dimensions=="expression relation pairs") {
          a1 <- multi_dimensions_expression_pairs(data_source1,#input$data_source_multi_dimensions,
                                                  treatment1,#input$treatment_multi_dimensions,
                                                  description1,#input$description_multi_dimensions,
                                                  ids=xids,x_or_y="xaxis_gene_multi_dimensions")
          colnames(a1)[ncol(a1)] <- "sumx"
          a1
        } else {
          m<-data.frame(m,sumx=apply(as.matrix(m[,xgenessum]),1,function(x) sum(na.omit(x))))
          if (nrow(m)==0) {
            as.data.frame(m)
          } else {
            m[apply(as.matrix(m[,xgenessum]),1,function(x) length(na.omit(x))==0),"sumx"]<-NA
            as.data.frame(m)}}
      } else if (input$xaxis_multi_dimensions=="mutation" & input$yaxis_multi_dimensions=="mutation") {
        genes1 <- c("PatientID","data_source","treatment","description","PFS.status","RECIST","OS.status","OS","PFS","Age")
        a<-data.table(database.test.process())
        m<-a[data_source %in% data_source1 & treatment %in% treatment1 & description %in% description1]
        m<-m[,colnames(m) %in% genes1,with=FALSE]
        as.data.frame(m)
      }
      
    })
    multi_dimensions_seperate_groups <- function (d.my.i, slider.x, slider.y) {
      if (input$xaxis_multi_dimensions=="mutation" & input$yaxis_multi_dimensions=="mutation") {
        ############# mutation mutation##########
        selected.mutations <- all.mutations[all.mutations[,"PatientID"] %in% d.my.i[,"PatientID"],]
        if (nrow(selected.mutations)==0) {
          d.my.i <- data.frame(d.my.i,IND="no data available")
          d.my.i
          # plot_ly () %>% 
          #   layout(title = unique(d.my.i$data_source),font=list(size=15,family="Arial")) %>%
          #   add_annotations(x=2,y=2,text="no mutation data available in this dataset",showarrow=F,font=list(size=16,color="purple"))
        } else {
          d.my.i <- data.frame(d.my.i,IND.x="X-low",IND.y="Y-low",IND="NA")
          d.my.i[,"IND.x"] <- as.character(d.my.i[,"IND.x"])
          d.my.i[,"IND.y"] <- as.character(d.my.i[,"IND.y"])
          d.my.i[,"IND"] <- as.character(d.my.i[,"IND"])
          d.my.i[d.my.i[,"PatientID"] %in% unique(selected.mutations[selected.mutations[,"gene"] %in% input$xaxis_gene_multi_dimensions,"PatientID"]),"IND.x"] <- "X-high"
          #                     d.my.i[d.my.i[,"PatientID"] %in% unique(selected.mutations[selected.mutations[,"gene"] %ni% input$xaxis_gene_multi_dimensions,"PatientID"]),"IND.x"] <- "X-low"
          d.my.i[d.my.i[,"PatientID"] %in% unique(selected.mutations[selected.mutations[,"gene"] %in% input$yaxis_gene_multi_dimensions,"PatientID"]),"IND.y"] <- "Y-high"
          #                     d.my.i[d.my.i[,"PatientID"] %in% unique(selected.mutations[selected.mutations[,"gene"] %ni% input$yaxis_gene_multi_dimensions,"PatientID"]),"IND.y"] <- "Y-low"
          d.my.i[,"IND"] <- paste(d.my.i[,"IND.x"],d.my.i[,"IND.y"],sep="/")
          d.my.i
        }
      } else if (input$xaxis_multi_dimensions=="mutation" & input$yaxis_multi_dimensions!="mutation") {
        ############## mutation non-mutation ################
        selected.mutations <- all.mutations[all.mutations[,"PatientID"] %in% d.my.i[,"PatientID"],]
        d.y <- d.my.i[!is.na(d.my.i[,"sumy"]),]
        if (nrow(selected.mutations)==0 & nrow(d.y)==0 ) {
          ## no mutation data, no d.my.i data ##
          d.my.i <- data.frame(d.my.i,IND="no data available")
          d.my.i
          # plot_ly () %>% 
          #   layout(title = unique(d.my.i$data_source),font=list(size=15,family="Arial")) %>%
          #   add_annotations(x=2,y=2,text="no data available in this dataset",showarrow=F,font=list(size=16,color="purple"))
          
        } else if (nrow(selected.mutations)==0 & nrow(d.y)!=0 ) {
          ## no mutation data, have d.my.i data ##
          d.my.i <- d.my.i[!is.na(d.my.i[,"sumy"]),]
          d.y.na <- d.my.i[is.na(d.my.i[,"sumy"]),]
          d.my.i <- data.frame(d.my.i,IND.x="NA",IND.y="Y-low",IND="NA")
          d.my.i[,"IND.x"] <- as.character(d.my.i[,"IND.x"])
          d.my.i[,"IND.y"] <- as.character(d.my.i[,"IND.y"])
          d.my.i[,"IND"] <- as.character(d.my.i[,"IND"])
          d.my.i[,"sumy"] <- unfactor(d.my.i[,"sumy"])
          if (input$yaxis_multi_dimensions=="expression relation pairs") {
            d.my.i[d.my.i[,"sumy"] > slider.y,"IND.y"] <- "Y-high"
          } else {
            d.my.i[d.my.i[,"sumy"] > quantile(d.my.i[,"sumy"],slider.y),"IND.y"] <- "Y-high"
          }
          d.my.i[,"IND"] <- paste(d.my.i[,"IND.x"],d.my.i[,"IND.y"],sep="/")
          rbind.fill(d.my.i,d.y.na)
        } else if (nrow(selected.mutations)!=0 & nrow(d.y)==0 ) {
          ## have mutation data, no d.my.i data ##
          d.my.i <- data.frame(d.my.i,IND.x="X-low",IND.y="NA",IND="NA")
          d.my.i[,"IND.x"] <- as.character(d.my.i[,"IND.x"])
          d.my.i[,"IND.y"] <- as.character(d.my.i[,"IND.y"])
          d.my.i[,"IND"] <- as.character(d.my.i[,"IND"])
          d.my.i[d.my.i[,"PatientID"] %in% unique(selected.mutations[selected.mutations[,"gene"] %in% input$xaxis_gene_multi_dimensions,"PatientID"]),"IND.x"] <- "X-high"
          d.my.i[,"IND"] <- paste(d.my.i[,"IND.x"],d.my.i[,"IND.y"],sep="/")
          d.my.i
        } else if (nrow(selected.mutations)!=0 & nrow(d.y)!=0) {
          d.my.i <- d.my.i[!is.na(d.my.i[,"sumy"]),]
          d.y.na <- d.my.i[is.na(d.my.i[,"sumy"]),]
          d.my.i <- data.frame(d.my.i,IND.x="X-low",IND.y="Y-low",IND="NA")
          d.my.i[,"IND.x"] <- as.character(d.my.i[,"IND.x"])
          d.my.i[,"IND.y"] <- as.character(d.my.i[,"IND.y"])
          d.my.i[,"IND"] <- as.character(d.my.i[,"IND"])
          d.my.i[,"sumy"] <- unfactor(d.my.i[,"sumy"])
          d.my.i[d.my.i[,"PatientID"] %in% unique(selected.mutations[selected.mutations[,"gene"] %in% input$xaxis_gene_multi_dimensions,"PatientID"]),"IND.x"] <- "X-high"
         if (input$yaxis_multi_dimensions=="expression relation pairs") {
            d.my.i[d.my.i[,"sumy"] > slider.y,"IND.y"] <- "Y-high"
          } else {
            d.my.i[d.my.i[,"sumy"] > quantile(d.my.i[,"sumy"],slider.y),"IND.y"] <- "Y-high"
          }
          d.my.i[,"IND"] <- paste(d.my.i[,"IND.x"],d.my.i[,"IND.y"],sep="/")
          rbind(d.my.i,d.y.na)
        }
        
        
      } else if (input$xaxis_multi_dimensions!="mutation" & input$yaxis_multi_dimensions=="mutation") {
        ###################### non mutation, mutation ####################################
        selected.mutations <- all.mutations[all.mutations[,"PatientID"] %in% d.my.i[,"PatientID"],]
        d.x <- d.my.i[!is.na(d.my.i[,"sumx"]),]
        if (nrow(d.x)==0 & nrow(selected.mutations)==0) {
          ## no d.my.i, and.my.i no mutation ##
          d.my.i <- data.frame(d.my.i,IND="no data available")
          d.my.i
          # plot_ly () %>% 
          #   layout(title = unique(d.my.i$data_source),font=list(size=15,family="Arial")) %>%
          #   add_annotations(x=2,y=2,text="no data available in this dataset",showarrow=F,font=list(size=16,color="purple"))
        } else if (nrow(d.x)!=0 & nrow(selected.mutations)==0) {
          ## have d.my.i, no mutation##
          d.my.i <- d.my.i[!is.na(d.my.i[,"sumx"]),]
          d.x.na <- d.my.i[is.na(d.my.i[,"sumx"]),]
          d.my.i <- data.frame(d.my.i,IND.x="X-low",IND.y="NA",IND="NA")
          d.my.i[,"IND.x"] <- as.character(d.my.i[,"IND.x"])
          d.my.i[,"IND.y"] <- as.character(d.my.i[,"IND.y"])
          d.my.i[,"IND"] <- as.character(d.my.i[,"IND"])
          d.my.i[,"sumx"] <- unfactor(d.my.i[,"sumx"])
          if (input$xaxis_multi_dimensions=="expression relation pairs") {
            d.my.i[d.my.i[,"sumx"] > slider.x,"IND.x"] <- "X-high"
          } else {
            d.my.i[d.my.i[,"sumx"] > quantile(d.my.i[,"sumx"],slider.x),"IND.x"] <- "X-high"
          }
          d.my.i[,"IND"] <- paste(d.my.i[,"IND.x"],d.my.i[,"IND.y"],sep="/")
          rbind.fill(d.my.i,d.x.na)
        } else if (nrow(d.x)==0 & nrow(selected.mutations)!=0) {
          ## no d.my.i, have mutation##
          d.my.i <- data.frame(d.my.i,IND.x="NA",IND.y="Y-low",IND="NA")
          d.my.i[,"IND.x"] <- as.character(d.my.i[,"IND.x"])
          d.my.i[,"IND.y"] <- as.character(d.my.i[,"IND.y"])
          d.my.i[,"IND"] <- as.character(d.my.i[,"IND"])
          d.my.i[d.my.i[,"PatientID"] %in% unique(selected.mutations[selected.mutations[,"gene"] %in% input$yaxis_gene_multi_dimensions,"PatientID"]),"IND.y"] <- "Y-high"
          d.my.i[,"IND"] <- paste(d.my.i[,"IND.x"],d.my.i[,"IND.y"],sep="/")
          d.my.i
        } else if (nrow(d.x)!=0 & nrow(selected.mutations)!=0) {
          ## have d.my.i, have mutation ##
          d.my.i <- d.my.i[!is.na(d.my.i[,"sumx"]),]
          d.x.na <- d.my.i[is.na(d.my.i[,"sumx"]),]
          d.my.i <- data.frame(d.my.i,IND.x="X-low",IND.y="Y-low",IND="NA")
          d.my.i[,"IND.x"] <- as.character(d.my.i[,"IND.x"])
          d.my.i[,"IND.y"] <- as.character(d.my.i[,"IND.y"])
          d.my.i[,"IND"] <- as.character(d.my.i[,"IND"])
          
          d.my.i[d.my.i[,"PatientID"] %in% unique(selected.mutations[selected.mutations[,"gene"] %in% input$yaxis_gene_multi_dimensions,"PatientID"]),"IND.y"] <- "Y-high"
          #d.x <- d.my.i[!is.na(d.my.i[,"sumx"]),]
          
          d.my.i[,"sumx"] <- unfactor(d.my.i[,"sumx"])
          if (input$xaxis_multi_dimensions=="expression relation pairs") {
            d.my.i[d.my.i[,"sumx"] > slider.x,"IND.x"] <- "X-high"
          } else {
            d.my.i[d.my.i[,"sumx"] > quantile(d.my.i[,"sumx"],slider.x),"IND.x"] <- "X-high"
          }
          d.my.i[,"IND"] <- paste(d.my.i[,"IND.x"],d.my.i[,"IND.y"],sep="/")
          rbind.fill(d.my.i,d.x.na)
        }
        
      } else if (input$xaxis_multi_dimensions!="mutation" & input$yaxis_multi_dimensions!="mutation") {
        ############## non mutation , non mutation ############
        d.x <- d.my.i[!is.na(d.my.i[,"sumx"]) ,]
        d.x.na <- d.my.i[is.na(d.my.i[,"sumx"]) ,]
        d.y <- d.my.i[!is.na(d.my.i[,"sumy"]) ,]
        d.y.na <- d.my.i[is.na(d.my.i[,"sumy"]) ,]
        if (nrow(d.x)==0 & nrow(d.y)==0) {
          ## no d.x, no d.y ##
          d.my.i <- data.frame(d.my.i,IND="no data available")
          #d.my.i
          # plot_ly () %>% 
          #   layout(title = unique(d.my.i$data_source),font=list(size=15,family="Arial")) %>%
          #   add_annotations(x=2,y=2,text="no data available in this dataset",showarrow=F,font=list(size=16,color="purple"))
        } else if (nrow(d.x)==0 & nrow(d.y)!=0) {
          ## no d.x, have d.y ##
          d.my.i <- d.my.i[!is.na(d.my.i[,"sumy"]) ,]
          d.my.i <- data.frame(d.my.i,IND.x="NA",IND.y="Y-low",IND="NA")
          d.my.i[,"IND.x"] <- as.character(d.my.i[,"IND.x"])
          d.my.i[,"IND.y"] <- as.character(d.my.i[,"IND.y"])
          d.my.i[,"IND"] <- as.character(d.my.i[,"IND"])
          d.my.i[,"sumy"] <- unfactor(d.my.i[,"sumy"])
          if (input$yaxis_multi_dimensions=="expression relation pairs") {
            d.my.i[d.my.i[,"sumy"] > slider.y,"IND.y"] <- "Y-high"
          } else {
            d.my.i[d.my.i[,"sumy"] > quantile(d.my.i[,"sumy"],slider.y),"IND.y"] <- "Y-high"
          }
          d.my.i[,"IND"] <- paste(d.my.i[,"IND.x"],d.my.i[,"IND.y"],sep="/")
          #d.my.i
        } else if (nrow(d.x)!=0 & nrow(d.y)==0) {
          ## no d.x, have d.y ##
          d.my.i <- d.my.i[!is.na(d.my.i[,"sumx"]) ,]
          d.my.i <- data.frame(d.my.i,IND.x="X-low",IND.y="NA",IND="NA")
          d.my.i[,"IND.x"] <- as.character(d.my.i[,"IND.x"])
          d.my.i[,"IND.y"] <- as.character(d.my.i[,"IND.y"])
          d.my.i[,"IND"] <- as.character(d.my.i[,"IND"])
          d.my.i[,"sumx"] <- unfactor(d.my.i[,"sumx"])
          if (input$xaxis_multi_dimensions=="expression relation pairs") {
            d.my.i[d.my.i[,"sumx"] > slider.x,"IND.x"] <- "X-high"
          } else {
            d.my.i[d.my.i[,"sumx"] > quantile(d.my.i[,"sumx"],slider.x),"IND.x"] <- "X-high"
          }
          d.my.i[,"IND"] <- paste(d.my.i[,"IND.x"],d.my.i[,"IND.y"],sep="/")
          #d.my.i
        } else if (nrow(d.x)!=0 & nrow(d.y)!=0) {
          ## have d.x, have d.y ##
          d.my.i <- d.my.i[!is.na(d.my.i[,"sumx"]) & !is.na(d.my.i[,"sumy"]),]
          d.my.i <- data.frame(d.my.i,IND.x="NA",IND.y="NA",IND="NA")
          d.my.i[,"IND.x"] <- as.character(d.my.i[,"IND.x"])
          d.my.i[,"IND.y"] <- as.character(d.my.i[,"IND.y"])
          d.my.i[,"IND"] <- as.character(d.my.i[,"IND"])
          d.my.i[,"sumx"] <- unfactor(d.my.i[,"sumx"])
          d.my.i[,"sumy"] <- unfactor(d.my.i[,"sumy"])
          if (input$xaxis_multi_dimensions=="expression relation pairs") {
            d.my.i[d.my.i[,"sumx"] > slider.x,"IND.x"] <- "X-high"
            d.my.i[d.my.i[,"sumx"] <= slider.x,"IND.x"] <- "X-low"
          } else {
            d.my.i[d.my.i[,"sumx"] > quantile(d.my.i[,"sumx"],slider.x),"IND.x"] <- "X-high"
            d.my.i[d.my.i[,"sumx"] <= quantile(d.my.i[,"sumx"],slider.x),"IND.x"] <- "X-low"
          }
          if (input$yaxis_multi_dimensions=="expression relation pairs") {
            d.my.i[d.my.i[,"sumy"] > slider.y,"IND.y"] <- "Y-high"
            d.my.i[d.my.i[,"sumy"] <= slider.y,"IND.y"] <- "Y-low"
          } else {
            d.my.i[d.my.i[,"sumy"] > quantile(d.my.i[,"sumy"],slider.y),"IND.y"] <- "Y-high"
            d.my.i[d.my.i[,"sumy"] <= quantile(d.my.i[,"sumy"],slider.y),"IND.y"] <- "Y-low"
          }
          d.my.i[,"IND"] <- paste(d.my.i[,"IND.x"],d.my.i[,"IND.y"],sep="/")
          #d.my.i
        }
        rbind.fill(d.my.i,rbind(d.x.na,d.y.na))
      }
    }
    ###############################
    #### multi-dimensions plots
    ###############################
    
    observeEvent(input$do_multi_dimensions,{
      if (input$xaxis_multi_dimensions=="mutation") {
        output$slider_multi_dimensions_x <- renderUI({
          sliderInput("slider_multi_dimensions_x",
                      "Seperate samples into X-low and X-high groups by setting a cutoff of X variable (0 denotes X-low samples without mutation; 1 denotes X-high samples with mutations): ",
                      min=0,max=1,value=0)  })
      } else if (input$xaxis_multi_dimensions=="expression relation pairs") {
        output$slider_multi_dimensions_x <- renderUI({
          sliderInput("slider_multi_dimensions_x",
                      "Seperate samples into X-low and X-high groups by setting a cutoff of X variable: ",
                      min=min(selecteddatabase_multi_dimensions()[,"sumx"]),max=max(selecteddatabase_multi_dimensions()[,"sumx"]),value=min(selecteddatabase_multi_dimensions()[,"sumx"]))  })
      } else {
        output$slider_multi_dimensions_x <- renderUI({
          sliderInput("slider_multi_dimensions_x",
                      "Seperate samples into X-low and X-high groups by setting a cutoff of X variable: ",
                      min=0,max=1,value=0.5)  })
      }
      if (input$yaxis_multi_dimensions=="mutation") {
        output$slider_multi_dimensions_y <- renderUI({
          sliderInput("slider_multi_dimensions_y",
                      "Seperate samples into Y-low and Y-high groups by setting a cutoff of Y variable (0 denotes X-low samples without mutation; 1 denotes X-high samples with mutations): ",
                      min=0,max=1,value=0)  })
      } else if (input$yaxis_multi_dimensions=="expression relation pairs") {
        output$slider_multi_dimensions_y <- renderUI({
          sliderInput("slider_multi_dimensions_y",
                      "Seperate samples into Y-low and Y-high groups by setting a cutoff of Y variable: ",
                      min=min(selecteddatabase_multi_dimensions()[,"sumy"]),max=max(selecteddatabase_multi_dimensions()[,"sumy"]),value=min(selecteddatabase_multi_dimensions()[,"sumy"]))  })
      } else {
        output$slider_multi_dimensions_y <- renderUI({
          sliderInput("slider_multi_dimensions_y",
                      "Seperate samples into Y-low and Y-high groups by setting a cutoff of Y variable: ",
                      min=0,max=1,value=0.5)  })
      }
    })
    

    output$multi_dimensions_plots_n_samples_NA <- renderText({
      input$do_multi_dimensions
      isolate({
        nrow(selecteddatabase_multi_dimensions())
      })
    })
    
    output$multi_dimensions_plots_plots <- renderUI({
      input$do_multi_dimensions
      isolate({
        selected.database <- selecteddatabase_multi_dimensions()
        if (nrow(selected.database)==0) {
          tagList(plotlyOutput("multi_dimensions_plots_plots0",height="auto"))
        } else {
          if (length(input$data_source_multi_dimensions)==1) {
            tagList(plotlyOutput("multi_dimensions_plots_plots1",height="auto"),tags$h1("  "))
          } else if (length(input$data_source_multi_dimensions)==2) {
            tagList(plotlyOutput("multi_dimensions_plots_plots1",height="auto"),tags$h1("  "),
                    plotlyOutput("multi_dimensions_plots_plots2",height="auto"),tags$h1("  "))
          } else if (length(input$data_source_multi_dimensions)==3) {
            tagList(plotlyOutput("multi_dimensions_plots_plots1",height="auto"),tags$h1("  "),
                    plotlyOutput("multi_dimensions_plots_plots2",height="auto"),tags$h1("  "),
                    plotlyOutput("multi_dimensions_plots_plots3",height="auto"),tags$h1("  "))
          } else if (length(input$data_source_multi_dimensions)==4) {
            tagList(plotlyOutput("multi_dimensions_plots_plots1",height="auto"),tags$h1("  "),
                    plotlyOutput("multi_dimensions_plots_plots2",height="auto"),tags$h1("  "),
                    plotlyOutput("multi_dimensions_plots_plots3",height="auto"),tags$h1("  "),
                    plotlyOutput("multi_dimensions_plots_plots4",height="auto"),tags$h1("  "))
          } else if (length(input$data_source_multi_dimensions)==5) {
            tagList(plotlyOutput("multi_dimensions_plots_plots1",height="auto"),tags$h1("  "),
                    plotlyOutput("multi_dimensions_plots_plots2",height="auto"),tags$h1("  "),
                    plotlyOutput("multi_dimensions_plots_plots3",height="auto"),tags$h1("  "),
                    plotlyOutput("multi_dimensions_plots_plots4",height="auto"),tags$h1("  "),
                    plotlyOutput("multi_dimensions_plots_plots5",height="auto"),tags$h1("  "))
          } else if (length(input$data_source_multi_dimensions)==6) {
            tagList(plotlyOutput("multi_dimensions_plots_plots1",height="auto"),tags$h1("  "),
                    plotlyOutput("multi_dimensions_plots_plots2",height="auto"),tags$h1("  "),
                    plotlyOutput("multi_dimensions_plots_plots3",height="auto"),tags$h1("  "),
                    plotlyOutput("multi_dimensions_plots_plots4",height="auto"),tags$h1("  "),
                    plotlyOutput("multi_dimensions_plots_plots5",height="auto"),tags$h1("  "),
                    plotlyOutput("multi_dimensions_plots_plots6",height="auto"),tags$h1("  "))
          } else if (length(input$data_source_multi_dimensions)==7) {
            tagList(plotlyOutput("multi_dimensions_plots_plots1",height="auto"),tags$h1("  "),
                    plotlyOutput("multi_dimensions_plots_plots2",height="auto"),tags$h1("  "),
                    plotlyOutput("multi_dimensions_plots_plots3",height="auto"),tags$h1("  "),
                    plotlyOutput("multi_dimensions_plots_plots4",height="auto"),tags$h1("  "),
                    plotlyOutput("multi_dimensions_plots_plots5",height="auto"),tags$h1("  "),
                    plotlyOutput("multi_dimensions_plots_plots6",height="auto"),tags$h1("  "),
                    plotlyOutput("multi_dimensions_plots_plots7",height="auto"),tags$h1("  "))
          } else if (length(input$data_source_multi_dimensions)==8) {
            tagList(plotlyOutput("multi_dimensions_plots_plots1",height="auto"),tags$h1("  "),
                    plotlyOutput("multi_dimensions_plots_plots2",height="auto"),tags$h1("  "),
                    plotlyOutput("multi_dimensions_plots_plots3",height="auto"),tags$h1("  "),
                    plotlyOutput("multi_dimensions_plots_plots4",height="auto"),tags$h1("  "),
                    plotlyOutput("multi_dimensions_plots_plots5",height="auto"),tags$h1("  "),
                    plotlyOutput("multi_dimensions_plots_plots6",height="auto"),tags$h1("  "),
                    plotlyOutput("multi_dimensions_plots_plots7",height="auto"),tags$h1("  "),
                    plotlyOutput("multi_dimensions_plots_plots8",height="auto"),tags$h1("  "))
          } else if (length(input$data_source_multi_dimensions)==9) {
            tagList(plotlyOutput("multi_dimensions_plots_plots1",height="auto"),tags$h1("  "),
                    plotlyOutput("multi_dimensions_plots_plots2",height="auto"),tags$h1("  "),
                    plotlyOutput("multi_dimensions_plots_plots3",height="auto"),tags$h1("  "),
                    plotlyOutput("multi_dimensions_plots_plots4",height="auto"),tags$h1("  "),
                    plotlyOutput("multi_dimensions_plots_plots5",height="auto"),tags$h1("  "),
                    plotlyOutput("multi_dimensions_plots_plots6",height="auto"),tags$h1("  "),
                    plotlyOutput("multi_dimensions_plots_plots7",height="auto"),tags$h1("  "),
                    plotlyOutput("multi_dimensions_plots_plots8",height="auto"),tags$h1("  "),
                    plotlyOutput("multi_dimensions_plots_plots9",height="auto"),tags$h1("  "))
          }  
        }
      })
    })
    
    isolate({
    observeEvent(input$do_multi_dimensions,{
      input$do_multi_dimensions
      isolate({
        input_data_source_multi_dimensions <- input$data_source_multi_dimensions
        selected.database <- selecteddatabase_multi_dimensions()
        #      if (nrow(selected.database)==0 | nrow(selected.database[is.na(selected.database[,"sumx"]) & is.na(selected.database[,"sumy"]),])==nrow(selected.database)) {
        if (nrow(selected.database)==0) {
          plot_ly () %>% 
            layout(title = paste(input$data_source_multi_dimensions,collapse = ","),font=list(size=15,family="Arial")) %>%
            add_annotations(x=2,y=2,text="no data available under this selection",showarrow=F,font=list(size=16,color="purple"))
        } else {
          selected.database <- data.frame(selected.database,Groups=selected.database$PFS.status)
          splitted_list <- selected.database %>% as.matrix %>% as.data.frame %>% split(.$data_source)
          splitted_list <- splitted_list[input$data_source_multi_dimensions]
          for (k in 1:length(input$data_source_multi_dimensions)) {
            local({
              my_i <- k
              multi_dimensions_plots_plots_name <- paste0("multi_dimensions_plots_plots", my_i)
              d<-splitted_list[[my_i]]
              output[[multi_dimensions_plots_plots_name]] <- renderPlotly({
                if (is.na(names(splitted_list)[my_i])) {
                  plot_ly () %>% 
                    layout(title = input$data_source_multi_dimensions[[my_i]],font=list(size=15,family="Arial")) %>%
                    add_annotations(x=2,y=2,text="no data available under this selection",showarrow=F,font=list(size=16,color="purple")) 
                } else {
                  #                if (nrow(d[!is.na(d[,"PFS.status"]) & (!is.na(d[,"sumx"]) | !is.na(d[,"sumy"])),])==0) {
                  if (nrow(d[!is.na(d[,"PFS.status"]),])==0) {
                    plot_ly () %>% 
                      layout(title = unique(d$data_source),font=list(size=15,family="Arial")) %>%
                      add_annotations(x=2,y=2,text="no immunotherapy response under this selection",showarrow=F,font=list(size=16,color="purple")) 
                  } else {
                    d <- multi_dimensions_seperate_groups(d.my.i=d, slider.x=input$slider_multi_dimensions_x, slider.y=input$slider_multi_dimensions_y)
                    
                    if (unique(d[,"IND"])=="no data available") {
                      plot_ly () %>%
                        layout(title = input$data_source_multi_dimensions[[my_i]],font=list(size=15,family="Arial")) %>%
                        add_annotations(x=2,y=2,text="no X/Y available in this dataset",showarrow=F,font=list(size=16,color="purple"))
                    } else {
                      d.plot <- as.data.frame(unlist(table(d[,c("PFS.status","IND")])))
                      colnames(d.plot)[2] <- "Groups"
                      d.plot[,"PFS.status"] <- as.character(d.plot[,"PFS.status"])
                      d.plot[,"Groups"] <- as.character(d.plot[,"Groups"])
                      d.plot[d.plot[,"PFS.status"]=="nonresponse","PFS.status"] <- paste(d.plot[d.plot[,"PFS.status"]=="nonresponse","PFS.status"]," (n=",sum(d.plot[d.plot[,"PFS.status"]=="nonresponse","Freq"]),")",sep="" )
                      d.plot[d.plot[,"PFS.status"]=="response","PFS.status"] <- paste(d.plot[d.plot[,"PFS.status"]=="response","PFS.status"]," (n=",sum(d.plot[d.plot[,"PFS.status"]=="response","Freq"]),")",sep="" )
                      d.plot[,"Groups"] <- paste(d.plot[,"Groups"]," group",sep="")
#                      d.number <- table(d.plot[,"Groups"])
#                      d[,"Groups"] <- paste(d.plot[,"Groups"]," (n=",d.number[d.plot[,"Groups"]],")",sep="")
                      #                  try.test <- try(p.v <- fisher.test(matrix(d.plot[,"Freq"],4,2,dimnames=list(c("X-high/Y-high","X-high/Y-low","X-low/Y-high","X-low/Y-low"),c("nonresponse","response"))))$p.value)
                      try.test <- try(p.v <- fisher.test(unlist(table(d[,c("PFS.status","IND")])))$p.value)
                      
                      if (class(try.test)=="try-error") {
                        p.v <- "NA"
                      } else {
                        #                    p.v <- fisher.test(matrix(d.plot[,"Freq"],4,2,dimnames=list(c("X-high/Y-high","X-high/Y-low","X-low/Y-high","X-low/Y-low"),c("nonresponse","response"))))$p.value
                        p.v <- fisher.test(unlist(table(d[,c("PFS.status","IND")])))$p.value
                        if (p.v < 0.001) {
                          p.v <- "< 0.001"
                        } else {
                          p.v<-round(p.v,3)
                        } 
                      }
                      ggplotly(ggplot(d.plot, aes(fill=Groups, y=Freq, x=PFS.status)) + ggtitle(as.character(unique(d$data_source))) + 
                                 geom_bar( stat="identity", position="fill") +ylab("Frequency of groups") + xlab("Immunotherapy response") + 
                                 annotate("text",x=1.5,y=1.2,label=paste0("p-value = ",p.v),size=4))
                    }
                  } 
                  
                }
              })
            })
            
          }
          
        }
      })
    })
    })
    
    output$multi_dimensions_plots_download <- downloadHandler(
      filename = function() {
        if (input$multi_dimensions_plots_type == "Excel (CSV)") {
          "newfile.csv"
        } else if (input$multi_dimensions_plots_type == "Text (TSV)") {
          "newfile.txt"
        } else if (input$multi_dimensions_plots_type == "JSON") {
          "newfile.json"
        }
      },
      content = function(file) {
        if (input$multi_dimensions_plots_type == "Excel (CSV)") {
          write.csv (multi_dimensions_plots_download_data(),file,row.names = FALSE)
        } else if (input$multi_dimensions_plots_type == "Text (TSV)") {
          write.table (multi_dimensions_plots_download_data(),file,quote=F, row.names = FALSE, sep="\t")
        } else if (input$multi_dimensions_plots_type == "JSON") {
          write(toJSON(multi_dimensions_plots_download_data()),file)
        }
      }
    )
    multi_dimensions_plots_download_data <- function() {
      selected.database <- selecteddatabase_multi_dimensions()
      
      m1 <- selected.database %>% as.matrix %>% as.data.frame %>% split(.$data_source) %>% 
        lapply(function(d) { 
          multi_dimensions_seperate_groups(d.my.i=d, slider.x=input$slider_multi_dimensions_x, slider.y=input$slider_multi_dimensions_y)
        })
      m2 <- ldply(m1,data.frame)
      m2 <- m2[,-1]
      m2[,-c(7:9)]
    }
    output$multi_dimensions_plots_table <- DT::renderDataTable({
#      input$do_multi_dimensions
#      isolate({
        multi_dimensions_plots_download_data()
#      })
    })  
    
    
    #####################################################
    #### multi dimensions survival
    #####################################################
    observeEvent(input$do_multi_dimensions,{
      if (input$xaxis_multi_dimensions=="mutation") {
        output$slider_multi_dimensions_survival_x <- renderUI({
          sliderInput("slider_multi_dimensions_survival_x",
                      "Seperate samples into X-low and X-high groups by setting a cutoff of X variable (0 denotes X-low samples without mutation; 1 denotes X-high samples with mutations): ",
                      min=0,max=1,value=0)  })
      } else if (input$xaxis_multi_dimensions=="expression relation pairs") {
        output$slider_multi_dimensions_survival_x <- renderUI({
          sliderInput("slider_multi_dimensions_survival_x",
                      "Seperate samples into X-low and X-high groups by setting a cutoff of X variable: ",
                      min=min(selecteddatabase_multi_dimensions()[,"sumx"]),max=max(selecteddatabase_multi_dimensions()[,"sumx"]),value=min(selecteddatabase_multi_dimensions()[,"sumx"]))  })
      } else {
        output$slider_multi_dimensions_survival_x <- renderUI({
          sliderInput("slider_multi_dimensions_survival_x",
                      "Seperate samples into X-low and X-high groups by setting a cutoff of X variable: ",
                      min=0,max=1,value=0.5)  })
      }
      if (input$yaxis_multi_dimensions=="mutation") {
        output$slider_multi_dimensions_survival_y <- renderUI({
          sliderInput("slider_multi_dimensions_survival_y",
                      "Seperate samples into Y-low and Y-high groups by setting a cutoff of Y variable (0 denotes X-low samples without mutation; 1 denotes X-high samples with mutations): ",
                      min=0,max=1,value=0)  })
      } else if (input$yaxis_multi_dimensions=="expression relation pairs") {
        output$slider_multi_dimensions_survival_y <- renderUI({
          sliderInput("slider_multi_dimensions_survival_y",
                      "Seperate samples into Y-low and Y-high groups by setting a cutoff of Y variable: ",
                      min=min(selecteddatabase_multi_dimensions()[,"sumy"]),max=max(selecteddatabase_multi_dimensions()[,"sumy"]),value=min(selecteddatabase_multi_dimensions()[,"sumy"]))  })
      } else {
        output$slider_multi_dimensions_survival_y <- renderUI({
          sliderInput("slider_multi_dimensions_survival_y",
                      "Seperate samples into Y-low and Y-high groups by setting a cutoff of Y variable: ",
                      min=0,max=1,value=0.5)  })
      }
    })
    
    
    output$multi_dimensions_survival_n_samples_NA <- renderText({
      input$do_multi_dimensions
      isolate({
        nrow(selecteddatabase_multi_dimensions())
      })
    })
    
    output$multi_dimensions_survival_plots <- renderUI({
      input$do_multi_dimensions
      isolate({
        selected.database <- selecteddatabase_multi_dimensions()
        if (nrow(selected.database)==0) {
          tagList(plotlyOutput("multi_dimensions_survival_plots0",height="auto"))
        } else {
          if (length(input$data_source_multi_dimensions)==1) {
            tagList(plotlyOutput("multi_dimensions_survival_plots1",height="auto"),tags$h1("  "))
          } else if (length(input$data_source_multi_dimensions)==2) {
            tagList(plotlyOutput("multi_dimensions_survival_plots1",height="auto"),tags$h1("  "),
                    plotlyOutput("multi_dimensions_survival_plots2",height="auto"),tags$h1("  "))
          } else if (length(input$data_source_multi_dimensions)==3) {
            tagList(plotlyOutput("multi_dimensions_survival_plots1",height="auto"),tags$h1("  "),
                    plotlyOutput("multi_dimensions_survival_plots2",height="auto"),tags$h1("  "),
                    plotlyOutput("multi_dimensions_survival_plots3",height="auto"),tags$h1("  "))
          } else if (length(input$data_source_multi_dimensions)==4) {
            tagList(plotlyOutput("multi_dimensions_survival_plots1",height="auto"),tags$h1("  "),
                    plotlyOutput("multi_dimensions_survival_plots2",height="auto"),tags$h1("  "),
                    plotlyOutput("multi_dimensions_survival_plots3",height="auto"),tags$h1("  "),
                    plotlyOutput("multi_dimensions_survival_plots4",height="auto"),tags$h1("  "))
          } else if (length(input$data_source_multi_dimensions)==5) {
            tagList(plotlyOutput("multi_dimensions_survival_plots1",height="auto"),tags$h1("  "),
                    plotlyOutput("multi_dimensions_survival_plots2",height="auto"),tags$h1("  "),
                    plotlyOutput("multi_dimensions_survival_plots3",height="auto"),tags$h1("  "),
                    plotlyOutput("multi_dimensions_survival_plots4",height="auto"),tags$h1("  "),
                    plotlyOutput("multi_dimensions_survival_plots5",height="auto"),tags$h1("  "))
          } else if (length(input$data_source_multi_dimensions)==6) {
            tagList(plotlyOutput("multi_dimensions_survival_plots1",height="auto"),tags$h1("  "),
                    plotlyOutput("multi_dimensions_survival_plots2",height="auto"),tags$h1("  "),
                    plotlyOutput("multi_dimensions_survival_plots3",height="auto"),tags$h1("  "),
                    plotlyOutput("multi_dimensions_survival_plots4",height="auto"),tags$h1("  "),
                    plotlyOutput("multi_dimensions_survival_plots5",height="auto"),tags$h1("  "),
                    plotlyOutput("multi_dimensions_survival_plots6",height="auto"),tags$h1("  "))
          } else if (length(input$data_source_multi_dimensions)==7) {
            tagList(plotlyOutput("multi_dimensions_survival_plots1",height="auto"),tags$h1("  "),
                    plotlyOutput("multi_dimensions_survival_plots2",height="auto"),tags$h1("  "),
                    plotlyOutput("multi_dimensions_survival_plots3",height="auto"),tags$h1("  "),
                    plotlyOutput("multi_dimensions_survival_plots4",height="auto"),tags$h1("  "),
                    plotlyOutput("multi_dimensions_survival_plots5",height="auto"),tags$h1("  "),
                    plotlyOutput("multi_dimensions_survival_plots6",height="auto"),tags$h1("  "),
                    plotlyOutput("multi_dimensions_survival_plots7",height="auto"),tags$h1("  "))
          } else if (length(input$data_source_multi_dimensions)==8) {
            tagList(plotlyOutput("multi_dimensions_survival_plots1",height="auto"),tags$h1("  "),
                    plotlyOutput("multi_dimensions_survival_plots2",height="auto"),tags$h1("  "),
                    plotlyOutput("multi_dimensions_survival_plots3",height="auto"),tags$h1("  "),
                    plotlyOutput("multi_dimensions_survival_plots4",height="auto"),tags$h1("  "),
                    plotlyOutput("multi_dimensions_survival_plots5",height="auto"),tags$h1("  "),
                    plotlyOutput("multi_dimensions_survival_plots6",height="auto"),tags$h1("  "),
                    plotlyOutput("multi_dimensions_survival_plots7",height="auto"),tags$h1("  "),
                    plotlyOutput("multi_dimensions_survival_plots8",height="auto"),tags$h1("  "))
          } else if (length(input$data_source_multi_dimensions)==9) {
            tagList(plotlyOutput("multi_dimensions_survival_plots1",height="auto"),tags$h1("  "),
                    plotlyOutput("multi_dimensions_survival_plots2",height="auto"),tags$h1("  "),
                    plotlyOutput("multi_dimensions_survival_plots3",height="auto"),tags$h1("  "),
                    plotlyOutput("multi_dimensions_survival_plots4",height="auto"),tags$h1("  "),
                    plotlyOutput("multi_dimensions_survival_plots5",height="auto"),tags$h1("  "),
                    plotlyOutput("multi_dimensions_survival_plots6",height="auto"),tags$h1("  "),
                    plotlyOutput("multi_dimensions_survival_plots7",height="auto"),tags$h1("  "),
                    plotlyOutput("multi_dimensions_survival_plots8",height="auto"),tags$h1("  "),
                    plotlyOutput("multi_dimensions_survival_plots9",height="auto"),tags$h1("  "))
          }  
        }
      })
    })
    
    isolate({
    observeEvent(input$do_multi_dimensions,{
      input$do_multi_dimensions
      isolate({
        input_data_source_multi_dimensions <- input$data_source_multi_dimensions
        selected.database <- selecteddatabase_multi_dimensions()
        #      if (nrow(selected.database)==0 | nrow(selected.database[is.na(selected.database[,"sumx"]) & is.na(selected.database[,"sumy"]),])==nrow(selected.database)) {
        if (nrow(selected.database)==0) {
          plot_ly () %>% 
            layout(title = paste(input$data_source_multi_dimensions,collapse = ","),font=list(size=15,family="Arial")) %>%
            add_annotations(x=2,y=2,text="no data available under this selection",showarrow=F,font=list(size=16,color="purple"))
        } else {
          selected.database <- data.frame(selected.database,Groups=selected.database$PFS.status)
          splitted_list <- selected.database %>% as.matrix %>% as.data.frame %>% split(.$data_source)
          splitted_list <- splitted_list[input$data_source_multi_dimensions]
          for (k in 1:length(input$data_source_multi_dimensions)) {
            local({
              my_i <- k
              multi_dimensions_survival_plots_name <- paste0("multi_dimensions_survival_plots", my_i)
              d<-splitted_list[[my_i]]
              output[[multi_dimensions_survival_plots_name]] <- renderPlotly({
                if (is.na(names(splitted_list)[my_i])) {
                  plot_ly () %>% 
                    layout(title = input$data_source_multi_dimensions[[my_i]],font=list(size=15,family="Arial")) %>%
                    add_annotations(x=2,y=2,text="no data available under this selection",showarrow=F,font=list(size=16,color="purple")) 
                } else {
                  #                if (nrow(d[!is.na(d[,"PFS.status"]) & (!is.na(d[,"sumx"]) | !is.na(d[,"sumy"])),])==0) {
                  if (nrow(d[!is.na(d[,"PFS.status"]),])==0) {
                    plot_ly () %>% 
                      layout(title = unique(d$data_source),font=list(size=15,family="Arial")) %>%
                      add_annotations(x=2,y=2,text="no immunotherapy response under this selection",showarrow=F,font=list(size=16,color="purple")) 
                  } else {
                    d <- multi_dimensions_seperate_groups(d.my.i=d, slider.x=input$slider_multi_dimensions_survival_x, slider.y=input$slider_multi_dimensions_survival_y)
                    
                    #                  d.model <- data.frame(PFS.status=c(rep("nonresponse",4),rep("response",4)),
                    #                                        IND=rep(c("X-high/Y-high","X-high/Y-low","X-low/Y-high","X-low/Y-low"),2),
                    #                                        Freq=rep(0,8))
                    if (unique(d[,"IND"])=="no data available") {
                      plot_ly () %>%
                        layout(title = input$data_source_multi_dimensions[[my_i]],font=list(size=15,family="Arial")) %>%
                        add_annotations(x=2,y=2,text="no X/Y available in this dataset",showarrow=F,font=list(size=16,color="purple"))
                    } else {
                      if (nrow(d[!is.na(d[,"OS.status"]),])==0 | nrow(d[!is.na(d[,"OS"]),])==0 ) {
                        plot_ly () %>% 
                          layout(title = unique(d$data_source),font=list(size=15,family="Arial")) %>%
                          add_annotations(x=2,y=2,text="no overall survival outcome under this selection",showarrow=F,font=list(size=16,color="purple")) 
                      } else {
                        d <- d[!is.na(d[,"OS.status"]) & !is.na(d[,"OS"]),]
                        d<-data.frame(d,dead=NA)
                        d[d[,"OS.status"]=="living" & !is.na(d[,"OS.status"]),"dead"] <- 0
                        d[d[,"OS.status"]=="deceased" & !is.na(d[,"OS.status"]),"dead"] <- 1
                        d <- d[!is.na(d[,"IND"]) & !is.na(d[,"dead"]),]
                        d <- d[d[,"dead"]==1 | d[,"dead"]==0,]
                        
                        d<-data.frame(d[,1:6],unfactor(d[,c("dead","OS","PFS","Age","IND")]))
                        d[,"dead"] <- as.numeric(d[,"dead"])
                        d<-d[order(d[,"IND"]),]
                        define.color <- c('#EE82EE', '#48D1CC', '#9ACD32', '#F08080', '#FF7F50',
                          '#CF59AE', '#3CB371', '#006666', '#FF1493')
                        names(define.color) <- c("X-low/Y-low","X-low/Y-high","X-high/Y-low","X-high/Y-high",
                                                 "NA/Y-low","NA/Y-high","X-low/NA","X-high/NA","NA/NA")
                        fit.result <- survfit(Surv(OS,dead)~IND,data=d)
                        plot.result <- ggsurv(fit.result,surv.col = define.color[na.omit(unique(as.character(d[,"IND"])))])
                        diff.result <- survdiff(Surv(OS,dead)~IND,data=d)
                        p.value= 1 - pchisq(diff.result$chisq, length(diff.result$n) - 1)
                        plot.result.ann <- plot.result + theme(legend.position = "right", legend.title = element_blank()) + 
                          ggplot2::geom_text(aes(label = sprintf("p-value = %0.2g",
                                                                 pchisq(diff.result$chisq,df=1,lower.tail = F)),x=200,y=0.05))
                        p1<-ggplotly(plot.result.ann,legendgroup=~IND,height = 480 ) %>%
                          layout(title = unique(d$data_source),titlefont=list(size=20,color="black",family= "Arial"),
                                 xaxis=list(title="Days",titlefont=list(size=20,color="black",family= "Aril black")),
                                 yaxis=list(title="Overall survival proportion",titlefont=list(size=20,color="black",family= "Aril black"))) #%>%
                        p1
                        
                      }
                    }
                  } 
                  
                }
              })
            })
            
          }
          
        }
      })
    })
    })
    
    output$multi_dimensions_survival_download <- downloadHandler(
      filename = function() {
        if (input$multi_dimensions_survival_type == "Excel (CSV)") {
          "newfile.csv"
        } else if (input$multi_dimensions_survival_type == "Text (TSV)") {
          "newfile.txt"
        } else if (input$multi_dimensions_survival_type == "JSON") {
          "newfile.json"
        }
      },
      content = function(file) {
        if (input$multi_dimensions_survival_type == "Excel (CSV)") {
          write.csv (multi_dimensions_survival_download_data(),file,row.names = FALSE)
        } else if (input$multi_dimensions_survival_type == "Text (TSV)") {
          write.table (multi_dimensions_survival_download_data(),file,quote=F, row.names = FALSE, sep="\t")
        } else if (input$multi_dimensions_survival_type == "JSON") {
          write(toJSON(multi_dimensions_survival_download_data()),file)
        }
      }
    )
    multi_dimensions_survival_download_data <- function() {
      selected.database <- selecteddatabase_multi_dimensions()
      
      m1 <- selected.database %>% as.matrix %>% as.data.frame %>% split(.$data_source) %>% 
        lapply(function(d) { 
          multi_dimensions_seperate_groups(d.my.i=d, slider.x=input$slider_multi_dimensions_survival_x, slider.y=input$slider_multi_dimensions_survival_y)
        })
      m2 <- ldply(m1,data.frame)
      m2 <- m2[,-1]
      m2[,-9]
    }
    output$multi_dimensions_survival_table <- DT::renderDataTable({
#      input$do_multi_dimensions
#      isolate({
        multi_dimensions_survival_download_data()
#      })
    })  
    ##########################################################
    #### multi dimensions PFS
    #########################################################
    observeEvent(input$do_multi_dimensions,{
      if (input$xaxis_multi_dimensions=="mutation") {
        output$slider_multi_dimensions_PFS_x <- renderUI({
          sliderInput("slider_multi_dimensions_PFS_x",
                      "Seperate samples into X-low and X-high groups by setting a cutoff of X variable (0 denotes X-low samples without mutation; 1 denotes X-high samples with mutations): ",
                      min=0,max=1,value=0)  })
      } else if (input$xaxis_multi_dimensions=="expression relation pairs") {
        output$slider_multi_dimensions_PFS_x <- renderUI({
          sliderInput("slider_multi_dimensions_PFS_x",
                      "Seperate samples into X-low and X-high groups by setting a cutoff of X variable: ",
                      min=min(selecteddatabase_multi_dimensions()[,"sumx"]),max=max(selecteddatabase_multi_dimensions()[,"sumx"]),value=min(selecteddatabase_multi_dimensions()[,"sumx"]))  })
      } else {
        output$slider_multi_dimensions_PFS_x <- renderUI({
          sliderInput("slider_multi_dimensions_PFS_x",
                      "Seperate samples into X-low and X-high groups by setting a cutoff of X variable: ",
                      min=0,max=1,value=0.5)  })
      }
      if (input$yaxis_multi_dimensions=="mutation") {
        output$slider_multi_dimensions_PFS_y <- renderUI({
          sliderInput("slider_multi_dimensions_PFS_y",
                      "Seperate samples into Y-low and Y-high groups by setting a cutoff of Y variable (0 denotes X-low samples without mutation; 1 denotes X-high samples with mutations): ",
                      min=0,max=1,value=0)  })
      } else if (input$yaxis_multi_dimensions=="expression relation pairs") {
        output$slider_multi_dimensions_PFS_y <- renderUI({
          sliderInput("slider_multi_dimensions_PFS_y",
                      "Seperate samples into Y-low and Y-high groups by setting a cutoff of Y variable: ",
                      min=min(selecteddatabase_multi_dimensions()[,"sumy"]),max=max(selecteddatabase_multi_dimensions()[,"sumy"]),value=min(selecteddatabase_multi_dimensions()[,"sumy"]))  })
      } else {
        output$slider_multi_dimensions_PFS_y <- renderUI({
          sliderInput("slider_multi_dimensions_PFS_y",
                      "Seperate samples into Y-low and Y-high groups by setting a cutoff of Y variable: ",
                      min=0,max=1,value=0.5)  })
      }
    })
    
    
    output$multi_dimensions_PFS_n_samples_NA <- renderText({
      input$do_multi_dimensions
      isolate({
        nrow(selecteddatabase_multi_dimensions())
      })
    })
    output$multi_dimensions_PFS_plots <- renderUI({
      input$do_multi_dimensions
      isolate({
        selected.database <- selecteddatabase_multi_dimensions()
        if (nrow(selected.database)==0) {
          tagList(plotlyOutput("multi_dimensions_PFS_plots0",height="auto"))
        } else {
          if (length(input$data_source_multi_dimensions)==1) {
            tagList(plotlyOutput("multi_dimensions_PFS_plots1",height="auto"),tags$h1("  "))
          } else if (length(input$data_source_multi_dimensions)==2) {
            tagList(plotlyOutput("multi_dimensions_PFS_plots1",height="auto"),tags$h1("  "),
                    plotlyOutput("multi_dimensions_PFS_plots2",height="auto"),tags$h1("  "))
          } else if (length(input$data_source_multi_dimensions)==3) {
            tagList(plotlyOutput("multi_dimensions_PFS_plots1",height="auto"),tags$h1("  "),
                    plotlyOutput("multi_dimensions_PFS_plots2",height="auto"),tags$h1("  "),
                    plotlyOutput("multi_dimensions_PFS_plots3",height="auto"),tags$h1("  "))
          } else if (length(input$data_source_multi_dimensions)==4) {
            tagList(plotlyOutput("multi_dimensions_PFS_plots1",height="auto"),tags$h1("  "),
                    plotlyOutput("multi_dimensions_PFS_plots2",height="auto"),tags$h1("  "),
                    plotlyOutput("multi_dimensions_PFS_plots3",height="auto"),tags$h1("  "),
                    plotlyOutput("multi_dimensions_PFS_plots4",height="auto"),tags$h1("  "))
          } else if (length(input$data_source_multi_dimensions)==5) {
            tagList(plotlyOutput("multi_dimensions_PFS_plots1",height="auto"),tags$h1("  "),
                    plotlyOutput("multi_dimensions_PFS_plots2",height="auto"),tags$h1("  "),
                    plotlyOutput("multi_dimensions_PFS_plots3",height="auto"),tags$h1("  "),
                    plotlyOutput("multi_dimensions_PFS_plots4",height="auto"),tags$h1("  "),
                    plotlyOutput("multi_dimensions_PFS_plots5",height="auto"),tags$h1("  "))
          } else if (length(input$data_source_multi_dimensions)==6) {
            tagList(plotlyOutput("multi_dimensions_PFS_plots1",height="auto"),tags$h1("  "),
                    plotlyOutput("multi_dimensions_PFS_plots2",height="auto"),tags$h1("  "),
                    plotlyOutput("multi_dimensions_PFS_plots3",height="auto"),tags$h1("  "),
                    plotlyOutput("multi_dimensions_PFS_plots4",height="auto"),tags$h1("  "),
                    plotlyOutput("multi_dimensions_PFS_plots5",height="auto"),tags$h1("  "),
                    plotlyOutput("multi_dimensions_PFS_plots6",height="auto"),tags$h1("  "))
          } else if (length(input$data_source_multi_dimensions)==7) {
            tagList(plotlyOutput("multi_dimensions_PFS_plots1",height="auto"),tags$h1("  "),
                    plotlyOutput("multi_dimensions_PFS_plots2",height="auto"),tags$h1("  "),
                    plotlyOutput("multi_dimensions_PFS_plots3",height="auto"),tags$h1("  "),
                    plotlyOutput("multi_dimensions_PFS_plots4",height="auto"),tags$h1("  "),
                    plotlyOutput("multi_dimensions_PFS_plots5",height="auto"),tags$h1("  "),
                    plotlyOutput("multi_dimensions_PFS_plots6",height="auto"),tags$h1("  "),
                    plotlyOutput("multi_dimensions_PFS_plots7",height="auto"),tags$h1("  "))
          } else if (length(input$data_source_multi_dimensions)==8) {
            tagList(plotlyOutput("multi_dimensions_PFS_plots1",height="auto"),tags$h1("  "),
                    plotlyOutput("multi_dimensions_PFS_plots2",height="auto"),tags$h1("  "),
                    plotlyOutput("multi_dimensions_PFS_plots3",height="auto"),tags$h1("  "),
                    plotlyOutput("multi_dimensions_PFS_plots4",height="auto"),tags$h1("  "),
                    plotlyOutput("multi_dimensions_PFS_plots5",height="auto"),tags$h1("  "),
                    plotlyOutput("multi_dimensions_PFS_plots6",height="auto"),tags$h1("  "),
                    plotlyOutput("multi_dimensions_PFS_plots7",height="auto"),tags$h1("  "),
                    plotlyOutput("multi_dimensions_PFS_plots8",height="auto"),tags$h1("  "))
          } else if (length(input$data_source_multi_dimensions)==9) {
            tagList(plotlyOutput("multi_dimensions_PFS_plots1",height="auto"),tags$h1("  "),
                    plotlyOutput("multi_dimensions_PFS_plots2",height="auto"),tags$h1("  "),
                    plotlyOutput("multi_dimensions_PFS_plots3",height="auto"),tags$h1("  "),
                    plotlyOutput("multi_dimensions_PFS_plots4",height="auto"),tags$h1("  "),
                    plotlyOutput("multi_dimensions_PFS_plots5",height="auto"),tags$h1("  "),
                    plotlyOutput("multi_dimensions_PFS_plots6",height="auto"),tags$h1("  "),
                    plotlyOutput("multi_dimensions_PFS_plots7",height="auto"),tags$h1("  "),
                    plotlyOutput("multi_dimensions_PFS_plots8",height="auto"),tags$h1("  "),
                    plotlyOutput("multi_dimensions_PFS_plots9",height="auto"),tags$h1("  "))
          }  
        }
      })
    })
    
    isolate({
    observeEvent(input$do_multi_dimensions,{
      input$do_multi_dimensions
      isolate({
        input_data_source_multi_dimensions <- input$data_source_multi_dimensions
        selected.database <- selecteddatabase_multi_dimensions()
        #      if (nrow(selected.database)==0 | nrow(selected.database[is.na(selected.database[,"sumx"]) & is.na(selected.database[,"sumy"]),])==nrow(selected.database)) {
        if (nrow(selected.database)==0) {
          plot_ly () %>% 
            layout(title = paste(input$data_source_multi_dimensions,collapse = ","),font=list(size=15,family="Arial")) %>%
            add_annotations(x=2,y=2,text="no data available under this selection",showarrow=F,font=list(size=16,color="purple"))
        } else {
          selected.database <- data.frame(selected.database,Groups=selected.database$PFS.status)
          splitted_list <- selected.database %>% as.matrix %>% as.data.frame %>% split(.$data_source)
          splitted_list <- splitted_list[input$data_source_multi_dimensions]
          for (k in 1:length(input$data_source_multi_dimensions)) {
            local({
              my_i <- k
              multi_dimensions_PFS_plots_name <- paste0("multi_dimensions_PFS_plots", my_i)
              d<-splitted_list[[my_i]]
              output[[multi_dimensions_PFS_plots_name]] <- renderPlotly({
                if (is.na(names(splitted_list)[my_i])) {
                  plot_ly () %>% 
                    layout(title = input$data_source_multi_dimensions[[my_i]],font=list(size=15,family="Arial")) %>%
                    add_annotations(x=2,y=2,text="no data available under this selection",showarrow=F,font=list(size=16,color="purple")) 
                } else {
                  #                if (nrow(d[!is.na(d[,"PFS.status"]) & (!is.na(d[,"sumx"]) | !is.na(d[,"sumy"])),])==0) {
                  if (nrow(d[!is.na(d[,"PFS.status"]),])==0) {
                    plot_ly () %>% 
                      layout(title = unique(d$data_source),font=list(size=15,family="Arial")) %>%
                      add_annotations(x=2,y=2,text="no immunotherapy response under this selection",showarrow=F,font=list(size=16,color="purple")) 
                  } else {
                    d <- multi_dimensions_seperate_groups(d.my.i=d, slider.x=input$slider_multi_dimensions_PFS_x, slider.y=input$slider_multi_dimensions_PFS_y)
                    
                    if (unique(d[,"IND"])=="no data available") {
                      plot_ly () %>%
                        layout(title = input$data_source_multi_dimensions[[my_i]],font=list(size=15,family="Arial")) %>%
                        add_annotations(x=2,y=2,text="no X/Y available in this dataset",showarrow=F,font=list(size=16,color="purple"))
                    } else {
                      if (nrow(d[!is.na(d[,"PFS.status"]),])==0 | nrow(d[!is.na(d[,"PFS"]),])==0 ) {
                        plot_ly () %>% 
                          layout(title = unique(d$data_source),font=list(size=15,family="Arial")) %>%
                          add_annotations(x=2,y=2,text="no PFS outcome under this selection",showarrow=F,font=list(size=16,color="purple")) 
                      } else {
                        d <- d[!is.na(d[,"PFS.status"]) & !is.na(d[,"PFS"]),]
                        d<-data.frame(d[,1:6],unfactor(d[,c("OS.status","OS","PFS","Age","IND")]))
                        d <- data.frame(d,group.IND=0)
                        d[d[,"PFS.status"]=="nonresponse","group.IND"] <- 1
                        d<-d[order(d[,"IND"]),]
                        define.color <- c('#EE82EE', '#48D1CC', '#9ACD32', '#F08080', '#FF7F50',
                                          '#CF59AE', '#3CB371', '#006666', '#FF1493')
                        names(define.color) <- c("X-low/Y-low","X-low/Y-high","X-high/Y-low","X-high/Y-high",
                                                 "NA/Y-low","NA/Y-high","X-low/NA","X-high/NA","NA/NA")
                        fit.result <- survfit(Surv(PFS,group.IND)~IND,data=d)
                        plot.result <- ggsurv(fit.result,surv.col = define.color[na.omit(unique(as.character(d[,"IND"])))])
                        diff.result <- survdiff(Surv(PFS,group.IND)~IND,data=d)
                        p.value= 1 - pchisq(diff.result$chisq, length(diff.result$n) - 1)
                        plot.result.ann <- plot.result + theme(legend.position = "right", legend.title = element_blank()) + 
                          ggplot2::geom_text(aes(label = sprintf("p-value = %0.2g",
                                                                 pchisq(diff.result$chisq,df=1,lower.tail = F)),x=200,y=0.05))
                        p1<-ggplotly(plot.result.ann,legendgroup=~IND,height = 480 ) %>%
                          layout(title = unique(d$data_source),titlefont=list(size=20,color="black",family= "Arial"),
                                 xaxis=list(title="Days",titlefont=list(size=20,color="black",family= "Aril black")),
                                 yaxis=list(title="Progression survival proportion",titlefont=list(size=20,color="black",family= "Aril black"))) #%>%
                        p1
                        
                      }
                    }
                  } 
                  
                }
              })
            })
            
          }
          
        }
      })
    })
    })
    
    output$multi_dimensions_PFS_download <- downloadHandler(
      filename = function() {
        if (input$multi_dimensions_PFS_type == "Excel (CSV)") {
          "newfile.csv"
        } else if (input$multi_dimensions_PFS_type == "Text (TSV)") {
          "newfile.txt"
        } else if (input$multi_dimensions_PFS_type == "JSON") {
          "newfile.json"
        }
      },
      content = function(file) {
        if (input$multi_dimensions_PFS_type == "Excel (CSV)") {
          write.csv (multi_dimensions_PFS_download_data(),file,row.names = FALSE)
        } else if (input$multi_dimensions_PFS_type == "Text (TSV)") {
          write.table (multi_dimensions_PFS_download_data(),file,quote=F, row.names = FALSE, sep="\t")
        } else if (input$multi_dimensions_PFS_type == "JSON") {
          write(toJSON(multi_dimensions_PFS_download_data()),file)
        }
      }
    )
    multi_dimensions_PFS_download_data <- function() {
      selected.database <- selecteddatabase_multi_dimensions()
      
      m1 <- selected.database %>% as.matrix %>% as.data.frame %>% split(.$data_source) %>% 
        lapply(function(d) { 
          multi_dimensions_seperate_groups(d.my.i=d, slider.x=input$slider_multi_dimensions_PFS_x, slider.y=input$slider_multi_dimensions_PFS_y)
        })
      m2 <- ldply(m1,data.frame)
      m2 <- m2[,-1]
      m2[,-c(7:8)]
    }
    output$multi_dimensions_PFS_table <- DT::renderDataTable({
#      input$do_multi_dimensions
#      isolate({
        multi_dimensions_PFS_download_data()
#      })
    })  
    ######################################################
    #### single cell gene expression
    ######################################################

    output$data_source_single_cell_gene <- renderUI({
      selectInput("data_source_single_cell_gene",
                  "Data source: ",
                  choices= c("Moshe","Krieg_panel_1","Krieg_panel_2","Krieg_panel_3"),
                  multiple = FALSE,
                  selected="Moshe")})
    output$treatment_single_cell_gene <- renderUI ({
      selectInput("treatment_single_cell_gene",
                  "Treatment: ",
                  #                choices=as.character(unique(filter_input_interactive(filter_input=input$data_source_expression)[,"treatment"])),
                  choices = as.character(unique(database.test.process()[,"treatment"])),
                  multiple = TRUE,
                  selected=c("anti-PD-1","anti-CTLA-4","anti-PD-1+anti-CTLA-4"))})
    output$description_single_cell_gene <- renderUI ({
      selectInput("description_single_cell_gene",
                  "Pre- or On-treatment: ",
                  #                choices=as.character(unique(filter_input_interactive(filter_input=c(input$data_source_expression,input$treatment_expression))[,"description"])),
                  choices = as.character(unique(database.test.process()[,"description"])),
                  multiple = TRUE,selected=c("Pre","On"))})
    # updateSelectizeInput(session,"gene_single_cell_gene",
    #                      choices=unique(c(Moshe_genes,Krieg_panel_1,Krieg_panel_2,Krieg_panel_3)), #rownames(pbmc@assays$RNA@counts),
    #                      selected="CD19",
    #                      server=TRUE)
    
    observeEvent(input$data_source_single_cell_gene,{
      if (input$data_source_single_cell_gene=="Krieg_panel_1") {
        updateSelectizeInput(session,"gene_single_cell_gene",
                             choices=Krieg_panel_1_genes, #rownames(Krieg_panel_1@assays$RNA@counts),
                             selected="CTLA4",
                             server=TRUE)
      } else if (input$data_source_single_cell_gene=="Krieg_panel_2") {
        updateSelectizeInput(session,"gene_single_cell_gene",
                             choices=Krieg_panel_2_genes, #rownames(Krieg_panel_2@assays$RNA@counts),
                             selected="CTLA4",
                             server=TRUE)
      } else if (input$data_source_single_cell_gene=="Krieg_panel_3") {
        updateSelectizeInput(session,"gene_single_cell_gene",
                             choices=Krieg_panel_3_genes, #rownames(Krieg_panel_3@assays$RNA@counts),
                             selected="CD16",
                             server=TRUE)
      } else {
        updateSelectizeInput(session,"gene_single_cell_gene",
                             choices=Moshe_genes, #rownames(pbmc@assays$RNA@counts),
                             selected="CD19",
                             server=TRUE)
      }
    })
    
    
    
    
    observeEvent(input$do_single_cell_gene,{
      if (input$data_source_single_cell_gene=="Krieg_panel_2") {
        #    seurat_Krieg_panel_1 <- readRDS("Krieg_panel_1.rds")
        seurat_Krieg_panel_2 <- readRDS("Krieg_panel_2.rds")
        #    seurat_Krieg_panel_3 <- readRDS("Krieg_panel_3.rds")
      } else {
        #   seurat_Krieg_panel_1 <- NULL
        seurat_Krieg_panel_2 <- NULL
        #   seurat_Krieg_panel_3 <- NULL
      }
      seurat_data_source <- switch (input$data_source_single_cell_gene,
                                    "Moshe"=seurat_Moshe,
                                    "Krieg_panel_1"=seurat_Krieg_panel_1,
                                    "Krieg_panel_2"=seurat_Krieg_panel_2,
                                    "Krieg_panel_3"=seurat_Krieg_panel_3)
      
      #    seurat_data_source <- readRDS(paste(input$data_sourcee,".rds",sep=""))
      selecteddatabase_single_cell_gene <- eventReactive(input$do_single_cell_gene,{
        # Due to dplyr issue #318, we need temp variables for input values
        data_source1 <- input$data_source_single_cell_gene
        treatment1 <- input$treatment_single_cell_gene
        description1 <- input$description_single_cell_gene
        a<-data.table(each.cell)
        m<-a[data_source %in% data_source1 & treatment %in% treatment1 & description %in% description1]
        #    m<-m[,colnames(m) %in% genes1,with=FALSE]
        #    colnames(m)[11] <- "UserGene"
        as.data.frame(m)
      })
      
      selected.database <- selecteddatabase_single_cell_gene()
      #################
      #### single cell gene expression plot
      ###################
      output$single_cell_gene_plots_n_samples_NA <- renderText({
        input$do_single_cell_gene
        isolate({
#          selected.database <- selecteddatabase_single_cell_gene()
          length(unique(selected.database[,"PatientID"])) 
        })
      })
      output$single_cell_gene_plots_n_cells_NA <- renderText({
        input$do_single_cell_gene
        isolate({
#          selected.database <- selecteddatabase_single_cell_gene()
          length(unique(selected.database[,"cells"]))
          #    .libPaths()
          #    as.character(sessionInfo())
        })
      })
      
      
      output$single_cell_gene_plots_violin <- renderPlot({
        input$do_single_cell_gene
        isolate({
          input_gene_single_cell_gene <- input$gene_single_cell_gene
          if (nrow(selected.database)==0) {
            plot_ly () %>% 
              layout(title = input$data_source_single_cell_gene,font=list(size=15,family="Arial")) %>%
              add_annotations(x=2,y=2,text="no data available",showarrow=F,font=list(size=16,color="purple"))
          } else {
            if (input_gene_single_cell_gene=="") {
              plot_ly () %>% 
                layout(title = input$data_source_single_cell_gene,font=list(size=15,family="Arial")) %>%
                add_annotations(x=2,y=2,text="no data available",showarrow=F,font=list(size=16,color="purple"))
            } else {
              d <- selected.database
              d <- data.frame(d,Groups=d$PFS.status)
              d[,"Groups"] <- as.character(d[,"Groups"])
              d[d[,"Groups"]=="response" & !is.na(d[,"Groups"]),"Groups"] <- paste("response (n=",nrow(d[d[,"Groups"]=="response",]),")")
              d[d[,"Groups"]=="nonresponse" & !is.na(d[,"Groups"]),"Groups"] <- paste("nonresponse (n=",nrow(d[d[,"Groups"]=="nonresponse",]),")")
              #        seurat_data_source <- readRDS(paste(input$data_source_single_cell_gene,".rds",sep=""))
              d <- merge(d,as.data.frame(seurat_data_source@assays$RNA@data[input_gene_single_cell_gene,]),by.x="cells",by.y="row.names",all.x=T)
              colnames(d)[ncol(d)] <- "UserGene"
              if (nrow(d[!is.na(d[,"PFS.status"]) & !is.na(d[,"UserGene"]),])==0 ) {
                plot_ly () %>% 
                  layout(title = unique(d$data_source),font=list(size=15,family="Arial")) %>%
                  add_annotations(x=2,y=2,text="no immunotherapy response under this selection",showarrow=F,font=list(size=16,color="purple")) 
              } else {
                d <- d[!is.na(d[,"PFS.status"]) & !is.na(d[,"UserGene"]),]
                ind <- sort(unique(d[,"Groups"]))
                try.test <- try(p.v <- wilcox.test(as.numeric(d[d[,"Groups"]==ind[1],"UserGene"]),as.numeric(d[d[,"Groups"]==ind[2],"UserGene"]))$p.value)
                if (class(try.test)=="try-error") {
                  p.v <- "NA"
                } else {
                  p.v <- wilcox.test(as.numeric(d[d[,"Groups"]==ind[1],"UserGene"]),as.numeric(d[d[,"Groups"]==ind[2],"UserGene"]))$p.value
                  if (p.v < 0.001) {
                    p.v <- "< 0.001"
                  } else {
                    p.v<-round(p.v,3)
                  } 
                }
                if (input$data_source_single_cell_gene=="Moshe") {
                  label_y <- paste(input_gene_single_cell_gene, " expression value (TPM)",sep="")
                } else {
                  label_y <- paste(input_gene_single_cell_gene, " expression value (CyTOF)",sep="")
                }
                if (nrow(d) > 10000) {
                  d <- d[sample(rownames(d),10000),]
                } 
                ggplot(d,aes(x=Groups,y=UserGene,fill=Groups))+scale_fill_manual(values=c("#DDA0DD", "#90EE90"))+
                  geom_violin()+geom_boxplot(width=0.05,fill="white")+ggtitle(as.character(unique(d$data_source))) +
                  ylab(label_y) + xlab("Immunotherapy response") + 
                  theme(plot.title = element_text(size=20,family = "Arial"),axis.text=element_text(size=12),axis.title = element_text(size=17))+
                  annotate("text",x=1.5,y=max(d[,"UserGene"]),label=paste0("p-value: ",p.v),size=6)
                # d <- d[1:200000,]
                # plot_ly(data=d,
                #         x=~Groups,
                #         y=~UserGene,
                #         split=~PFS.status,
                #         type="violin",
                #         color=~PFS.status,
                #         colors=c("plum","lightgreen"),
                #         legendgroup=~PFS.status, showlegend=TRUE,
                #         box=list(visible=T),
                #         meanline=list(visible=T),
                #         text= ~paste0("Sample ID: ", PatientID,
                #                       "<br>Cell ID: ", cells,
                #                       "<br>Expression: ",input$gene_single_cell," :",UserGene,
                #                       "<br>Treatment: ",treatment,
                #                       "<br>Description: ",description)) %>%
                #   layout(title = paste("<br><b>",input$data_source_single_cell_gene,"<b>",collapse = ""),titlefont=list(size=20,color="black",family= "Arial"),
                #          height=480,
                #          xaxis=list(title="Immunotherapy response",titlefont=list(size=20,color="black",family= "Aril"),
                #                     showticklabels=TRUE,tickcolor="#444",linecolor = "#444"),
                #          yaxis=list(title=label_y,
                #                     titlefont=list(size=20,color="black",family= "Aril"),
                #                     showticklabels=TRUE,tickmode="auto",tickcolor="#444",linecolor = "#444"),
                #          annotations=list(x=0.5,y=max(d[,"UserGene"]),text=paste0("p-value = ",p.v),xref="x",yref="y",showarrow=FALSE))
              }
            }
          }
        })
      })
      
      output$single_cell_gene_plots_tSNE <- renderPlot({
        input$do_single_cell_gene
        isolate({
          input_gene_single_cell_gene <- input$gene_single_cell_gene
          if (nrow(selected.database)==0 | input$gene_single_cell_gene=="") {
            plot_ly () %>% 
              layout(title = input$data_source_single_cell_gene,font=list(size=15,family="Arial")) %>%
              add_annotations(x=2,y=2,text="no data available",showarrow=F,font=list(size=16,color="purple"))
          } else {
            selected.database <- merge(selected.database,as.data.frame(seurat_data_source@assays$RNA@data[input$gene_single_cell_gene,]),by.x="cells",by.y="row.names",all.x=T)
            if (ncol(selected.database)<12) {
              plot_ly () %>% 
                layout(title = input$data_source_single_cell_gene,font=list(size=15,family="Arial")) %>%
                add_annotations(x=2,y=2,text="no data available",showarrow=F,font=list(size=16,color="purple"))
            } else {
              colnames(selected.database)[ncol(selected.database)] <- input$gene_single_cell_gene
              if (nrow(selected.database[!is.na(selected.database[,"PFS.status"]) & !is.na(selected.database[,12]),])==0 ) {
                plot_ly () %>% 
                  layout(title = input$data_source_single_cell_gene,font=list(size=15,family="Arial")) %>%
                  add_annotations(x=2,y=2,text="no immunotherapy response under this selection",showarrow=F,font=list(size=16,color="purple"))
              } else {
                selected.database <- selected.database[!is.na(selected.database[,"PFS.status"]) & !is.na(selected.database[,12]),]
                
                selected.database<-merge(selected.database,seurat_data_source@active.ident,by.x="cells",by.y="row.names",all=T)
                selected.database <- selected.database[!is.na(selected.database[,"PFS.status"]),]
                colnames(selected.database)[ncol(selected.database)] <- "cluster"
                selected.database.response <- selected.database[selected.database[,"PFS.status"]=="response",]
                selected.database.nonresponse <- selected.database[selected.database[,"PFS.status"]=="nonresponse",]
                withProgress(message = 'Making plot', value = 0, {
                  
                  cells.non <- as.character(unique(selected.database.nonresponse[,"cells"]))
                  if (length(cells.non) >10000) {
                    cells.non <- cells.non[sample(cells.non,10000) %in% rownames(seurat_data_source@reductions$tsne)]
                  } else {
                    cells.non <- cells.non[cells.non %in% rownames(seurat_data_source@reductions$tsne)]
                  }
                  p1 <- #ggplotly(
                    #                FeaturePlot(pbmc_small,features="CD79A",cells=rownames(pbmc_small@reductions$tsne)[1:10])
                    #                FeaturePlot(object = pbmc_small, features = 'CD79A',cells=colnames(pbmc_small@assays$RNA@counts)[1:10])
                    FeaturePlot(object = seurat_data_source, features = input$gene_single_cell_gene,
                                cells=cells.non,
                                cols=c("grey","purple")) #,height = 480) %>% 
                  #                                layout(xaxis=list(title="tSNE_1\nnonresponse",titlefont=list(size=23,color="black",family= "Aril")),
                  #                                       yaxis=list(title="tSNE_2",titlefont=list(size=23,color="black",family= "Aril")))
                  p1 <- p1+ggtitle("")+xlab("tSNE_1\nnonresponse")+theme(legend.position = "none")
                  cells.res <- as.character(unique(selected.database.response[,"cells"]))
                  if (length(cells.res) >10000) {
                    cells.res <- cells.res[sample(cells.res,10000) %in% rownames(seurat_data_source@reductions$tsne)]
                  } else {
                    cells.res <- cells.res[cells.res %in% rownames(seurat_data_source@reductions$tsne)]
                  }
                  p2 <- #ggplotly(
                    #                FeaturePlot(pbmc_small,features="CD79A",cells=rownames(pbmc_small@reductions$tsne)[1:10])
                    #                FeaturePlot(object = pbmc_small, features = 'CD79A',cells=colnames(pbmc_small@assays$RNA@counts)[11:20])
                    FeaturePlot(object = seurat_data_source, features = input$gene_single_cell_gene,
                                cells=cells.res,
                                cols=c("grey","purple")) #,height = 480) %>% 
                  #                                layout(xaxis=list(title="tSNE_1\nresponse",titlefont=list(size=23,color="black",family= "Aril")),
                  #                                       yaxis=list(title="tSNE_2",titlefont=list(size=23,color="black",family= "Aril")))
                  p2 <- p2 + ggtitle("")+ xlab("tSNE_1\nresponse")
                  
                  incProgress(100, detail = "Doing part %")
                  #              subplot(style(p1,showlegend=FALSE),style(p2,showlegend=FALSE),nrows=1,margin=0.05,titleX=TRUE,titleY=TRUE)
                  grid.arrange(p1,p2,ncol=2,top=textGrob(input$gene_single_cell_gene,gp=gpar(fontsize=20,font=3)))
                })
              }
            }
          } 
        })
      })
      output$single_cell_gene_plots_violin_download <- downloadHandler(
        filename = function() {
          if (input$single_cell_gene_plots_violin_type == "Excel (CSV)") {
            "newfile.csv" 
          } else if (input$single_cell_gene_plots_violin_type == "Text (TSV)") {
            "newfile.txt"
          } else if (input$single_cell_gene_plots_violin_type == "JSON") {
            "newfile.json"
          }
        },
        content = function(file) {
          if (input$single_cell_gene_plots_violin_type == "Excel (CSV)") {
            write.csv (single_cell_gene_plots_violin_download_data(),file,row.names = FALSE)
          } else if (input$single_cell_gene_plots_violin_type == "Text (TSV)") {
            write.table (single_cell_gene_plots_violin_download_data(),file,quote=F, row.names = FALSE, sep="\t")
          } else if (input$single_cell_gene_plots_violin_type == "JSON") {
            write(toJSON(single_cell_gene_plots_violin_download_data()),file)
          }
        }
      )                 
      single_cell_gene_plots_violin_download_data <- function() {
        m <- selected.database
        if (nrow(m)>0) {
          if (input_gene_single_cell_gene=="") {
            m
          } else {
            #        seurat_data_source <- readRDS(paste(input$data_source_single_cell_gene,".rds",sep=""))
            m <- merge(selected.database,as.data.frame(seurat_data_source@assays$RNA@data[input$gene_single_cell_gene,]),by.x="cells",by.y="row.names",all=T)
            if (length(input$gene_single_cell_gene)==1) {
              colnames(m)[ncol(m)] <- input_gene_single_cell_gene
              m
            } else {
              m
            }
          }
        } else {
          m
        }
        
      }
      output$single_cell_gene_plots_violin_table <- DT::renderDataTable({
        input$do_single_cell_gene
        isolate(single_cell_gene_plots_violin_download_data())
      }) 
      
      output$single_cell_gene_plots_tSNE_gene <- renderText({
        input$do_single_cell_gene
          isolate(input$gene_single_cell_gene)
        
      })
      output$single_cell_gene_plots_tSNE_download <- downloadHandler(
        filename = function() {
          if (input$single_cell_gene_plots_tSNE_type == "Excel (CSV)") {
            "newfile.csv" 
          } else if (input$single_cell_gene_plots_tSNE_type == "Text (TSV)") {
            "newfile.txt"
          } else if (input$single_cell_gene_plots_tSNE_type == "JSON") {
            "newfile.json"
          }
        },
        content = function(file) {
          if (input$single_cell_gene_plots_tSNE_type == "Excel (CSV)") {
            write.csv (single_cell_gene_plots_tSNE_download_data(),file,row.names = FALSE)
          } else if (input$single_cell_gene_plots_tSNE_type == "Text (TSV)") {
            write.table (single_cell_gene_plots_tSNE_download_data(),file,quote=F, row.names = FALSE, sep="\t")
          } else if (input$single_cell_gene_plots_tSNE_type == "JSON") {
            write(toJSON(single_cell_gene_plots_tSNE_download_data()),file)
          }
        }
      )                 
      single_cell_gene_plots_tSNE_download_data <- function() {
        m <- selected.database
        if (nrow(m)==0 | input$gene_single_cell_gene=="") {
          m
        } else {
          #      seurat_data_source <- readRDS(paste(input$data_source_single_cell_gene,".rds",sep=""))
          m <- merge(m,as.data.frame(seurat_data_source@assays$RNA@data[input$gene_single_cell_gene,]),by.x="cells",by.y="row.names",all=T)
          colnames(m)[ncol(m)] <- input$gene_single_cell_gene
          a <- merge(seurat_data_source@reductions$tsne@cell.embeddings,seurat_data_source@active.ident,by="row.names")
          m <- merge(m,a,by.x="cells",by.y="Row.names",all=T)
          colnames(m)[ncol(m)] <- "cluster"
          
          m
        }
        
      }
      output$single_cell_gene_plots_tSNE_table <- DT::renderDataTable({
        input$do_single_cell_gene
        isolate(single_cell_gene_plots_tSNE_download_data())
      }) 
      
    })  
    ######################################################
    #### single cell cell population
    ######################################################
    
    output$data_source_single_cell_cell <- renderUI({
      selectInput("data_source_single_cell_cell",
                  "Data source: ",
                  choices= c("Moshe","Krieg_panel_1","Krieg_panel_2","Krieg_panel_3"),
                  multiple = FALSE,
                  selected="Moshe")})
    output$treatment_single_cell_cell <- renderUI ({
      selectInput("treatment_single_cell_cell",
                  "Treatment: ",
                  #                choices=as.character(unique(filter_input_interactive(filter_input=input$data_source_expression)[,"treatment"])),
                  choices = as.character(unique(database.test.process()[,"treatment"])),
                  multiple = TRUE,
                  selected=c("anti-PD-1","anti-CTLA-4","anti-PD-1+anti-CTLA-4"))})
    output$description_single_cell_cell <- renderUI ({
      selectInput("description_single_cell_cell",
                  "Pre- or On-treatment: ",
                  #                choices=as.character(unique(filter_input_interactive(filter_input=c(input$data_source_expression,input$treatment_expression))[,"description"])),
                  choices = as.character(unique(database.test.process()[,"description"])),
                  multiple = TRUE,selected=c("Pre","On"))})
    # updateSelectizeInput(session,"cluster_single_cell_cell",
    #                      choices=sort(unique(pbmc@active.ident)),selected=4,
    #                      server=TRUE)
    observeEvent(input$data_source_single_cell_cell,{
      if (input$data_source_single_cell_cell=="Krieg_panel_1") {
        updateSelectizeInput(session,"cluster_single_cell_cell",
                             choices=c(0,1:17), #rownames(Krieg_panel_1@assays$RNA@counts),
                             selected=0,
                             server=TRUE)
      } else if (input$data_source_single_cell_cell=="Krieg_panel_2") {
        updateSelectizeInput(session,"cluster_single_cell_cell",
                             choices=c(0,1:14), #rownames(Krieg_panel_2@assays$RNA@counts),
                             selected=0,
                             server=TRUE)
      } else if (input$data_source_single_cell_cell=="Krieg_panel_3") {
        updateSelectizeInput(session,"cluster_single_cell_cell",
                             choices=c(0,1:11), #rownames(Krieg_panel_3@assays$RNA@counts),
                             selected=0,
                             server=TRUE)
      } else {
        updateSelectizeInput(session,"cluster_single_cell_cell",
                             choices=c(0,1:11), #rownames(pbmc@assays$RNA@counts),
                             selected=4,
                             server=TRUE)
      }
    })
    output$slider_single_cell_cell_OS <- renderUI({
      sliderInput("slider_single_cell_cell_OS",
                  "Seperate samples with frequency cutoff (%): ",
                  min=0, max=1, value=0.5)  })
    output$slider_single_cell_cell_PFS <- renderUI({
      sliderInput("slider_single_cell_cell_PFS",
                  "Seperate samples with frequency cutoff (%): ",
                  min=0,max=1,value=0.5)  })
    
    # selecteddatabase_single_cell_cell <- eventReactive(input$do_single_cell_cell,{
    #   if (input$data_source_single_cell_cell=="Krieg_panel_2") {
    #     #   seurat_Krieg_panel_1 <- readRDS("Krieg_panel_1.rds")
    #     seurat_Krieg_panel_2 <- readRDS("Krieg_panel_2.rds")
    #     #   seurat_Krieg_panel_3 <- readRDS("Krieg_panel_3.rds")
    #   } else {
    #     #   seurat_Krieg_panel_1 <- NULL
    #     seurat_Krieg_panel_2 <- NULL
    #     #   seurat_Krieg_panel_3 <- NULL
    #   }
    #   seurat_data_source <- switch (input$data_source_single_cell_cell,
    #                                 "Moshe"=seurat_Moshe,
    #                                 "Krieg_panel_1"=seurat_Krieg_panel_1,
    #                                 "Krieg_panel_2"=seurat_Krieg_panel_2,
    #                                 "Krieg_panel_3"=seurat_Krieg_panel_3)
    #   
    #   # Due to dplyr issue #318, we need temp variables for input values
    #   data_source1 <- input$data_source_single_cell_cell
    #   treatment1 <- input$treatment_single_cell_cell
    #   description1 <- input$description_single_cell_cell
    #   a<-data.table(each.cell)
    #   a<-merge(a,data.frame(cells=names(seurat_data_source@active.ident),cluster=seurat_data_source@active.ident),by="cells",all.x=T)
    #   m<-a[data_source %in% data_source1 & treatment %in% treatment1 & description %in% description1]
    #   as.data.frame(m)
    #   
    # })

    
    
    
     observeEvent(input$do_single_cell_cell,{
       
       if (input$data_source_single_cell_cell=="Krieg_panel_2") {
      #   seurat_Krieg_panel_1 <- readRDS("Krieg_panel_1.rds")
         seurat_Krieg_panel_2 <- readRDS("Krieg_panel_2.rds")
      #   seurat_Krieg_panel_3 <- readRDS("Krieg_panel_3.rds")
       } else {
      #   seurat_Krieg_panel_1 <- NULL
         seurat_Krieg_panel_2 <- NULL
      #   seurat_Krieg_panel_3 <- NULL
       }
    
       seurat_data_source <- switch (input$data_source_single_cell_cell,
                                     "Moshe"=seurat_Moshe,
                                     "Krieg_panel_1"=seurat_Krieg_panel_1,
                                     "Krieg_panel_2"=seurat_Krieg_panel_2,
                                     "Krieg_panel_3"=seurat_Krieg_panel_3)
       
       markers_data_source <- switch (input$data_source_single_cell_cell,
                                   "Moshe"=marker_Moshe,
                                   "Krieg_panel_1"=marker_Krieg_panel_1,
                                   "Krieg_panel_2"=marker_Krieg_panel_2,
                                   "Krieg_panel_3"=marker_Krieg_panel_3)
       selecteddatabase_single_cell_cell <- eventReactive(input$do_single_cell_cell,{

         # Due to dplyr issue #318, we need temp variables for input values
         data_source1 <- input$data_source_single_cell_cell
         treatment1 <- input$treatment_single_cell_cell
         description1 <- input$description_single_cell_cell
         a<-data.table(each.cell)
         a<-merge(a,data.frame(cells=names(seurat_data_source@active.ident),cluster=seurat_data_source@active.ident),by="cells",all.x=T)
         m<-a[data_source %in% data_source1 & treatment %in% treatment1 & description %in% description1]
         as.data.frame(m)
         
       })
       selected.database <- selecteddatabase_single_cell_cell()
       #################
       #### single cell cell population plot
       ###################
       
    output$single_cell_cell_plots_n_samples_NA <- renderText({
      input$do_single_cell_cell
      isolate({
#        selected.database <- selecteddatabase_single_cell_cell()
        length(unique(selected.database[,"PatientID"])) 
      })
      
    })
    output$single_cell_cell_plots_n_cells_NA <- renderText({
      input$do_single_cell_cell
      isolate({
#        selected.database <- selecteddatabase_single_cell_cell()
        length(unique(selected.database[,"cells"])) 
      })
    })
    
    output$single_cell_cell_plots_boxplot <- renderPlotly({
      #    selecteddatabase_single_cell_cell() :
      #  PatientID      cells data_source treatment description PFS.status RECIST OS.status  OS PFS Age cluster
      #  1 Moshe_Post_P1 A10_P1_M15       Moshe anti-PD-1          On   response     NA    living 822  NA  49       1
      #  2 Moshe_Post_P1 A10_P4_M15       Moshe anti-PD-1          On   response     NA    living 822  NA  49       2
      #  3 Moshe_Post_P1 A10_P5_M15       Moshe anti-PD-1          On   response     NA    living 822  NA  49       1
      input$do_single_cell_cell
      isolate({
#        selected.database <- selecteddatabase_single_cell_cell()
        
        if (nrow(selected.database)==0 | length(input$cluster_single_cell_cell)==0) {
          plot_ly () %>% add_annotations(x=2,y=2,text="no data available",showarrow=F,font=list(size=16,color="purple"))
        } else {
          withProgress(message = 'Making plot', value = 0, {
            index.nonres <- selected.database[selected.database[,"PFS.status"]=="nonresponse",c("PatientID","cluster")]
            index.nonres[,1] <- as.character(index.nonres[,1])
            b.nonres<-table(index.nonres[,c("PatientID","cluster")])
            index.res <- selected.database[selected.database[,"PFS.status"]=="response",c("PatientID","cluster")]
            index.res[,1] <- as.character(index.res[,1])
            b.res<-table(index.res[,c("PatientID","cluster")])
            b<-rbind(data.frame(b.nonres,PFS.status="nonresponse"),data.frame(b.res,PFS.status="response"))
            b<-b %>% filter(cluster %in% input$cluster_single_cell_cell)
            b <- aggregate(Freq ~ PatientID+PFS.status,data=b,sum)
            b <- data.frame(b,cluster=paste(input$cluster_single_cell_cell,collapse = "+"))
            selected.database<-merge(as.data.frame(b),as.data.frame(table(selected.database[,"PatientID"])),by.x="PatientID",by.y="Var1")
            selected.database<-data.frame(selected.database,Freq=apply(selected.database,1,function(x) as.numeric(x[3])/as.numeric(x[5])*100))
            selected.database <- data.frame(selected.database,Groups=selected.database$PFS.status)
            #    selected.database <- selected.database %>% filter(cluster %in% input$cluster_single_cell_cell)
            
            d <- selected.database
            d[,"Groups"] <- as.character(d[,"Groups"])
            d[d[,"Groups"]=="response" & !is.na(d[,"Groups"]),"Groups"] <- paste("response (n=",nrow(d[d[,"Groups"]=="response",]),")")
            d[d[,"Groups"]=="nonresponse" & !is.na(d[,"Groups"]),"Groups"] <- paste("nonresponse (n=",nrow(d[d[,"Groups"]=="nonresponse",]),")")
            ind <- unique(d[,"Groups"])
            try.test <- try(p.v <- wilcox.test(as.numeric(d[d[,"Groups"]==ind[1],"Freq"]),as.numeric(d[d[,"Groups"]==ind[2],"Freq"]))$p.value)
            if (class(try.test)=="try-error") {
              p.v <- "NA"
            } else {
              p.v <- wilcox.test(as.numeric(d[d[,"Groups"]==ind[1],"Freq"]),as.numeric(d[d[,"Groups"]==ind[2],"Freq"]))$p.value
              if (p.v < 0.001) {
                p.v <- "< 0.001"
              } else {
                p.v<-round(p.v,3)
              } 
            }
            incProgress(100, detail = "Doing part %")
            plot_ly(data=d,x=~Groups,
                    y=~Freq,
                    type="box",
                    boxpoints="all",
                    jitter=0.3,
                    pointpos=-1.8,
                    #mode="markders",
                    color= ~PFS.status,
                    colors=c("plum","lightgreen"),
                    #hoveron="points+boxes",
                    legendgroup=~PFS.status, showlegend=TRUE,
                    text= ~paste0("Sample ID: ", PatientID,
                                  "<br>Cluster: ",cluster,
                                  "<br>Frequency: ",Freq)) %>%
              layout(title = paste("<br><b>",input$data_source_single_cell_cell,"<b>",collapse = ""),titlefont=list(size=20,color="black",family= "Arial"),
                     height=480,
                     xaxis=list(title="Immunotherapy response",titlefont=list(size=20,color="black",family= "Aril"),
                                showticklabels=TRUE,tickcolor="#444",linecolor = "#444"),
                     yaxis=list(title=paste0("Frequency (%) of cluster ",paste(input$cluster_single_cell_cell,collapse = ",")),
                                titlefont=list(size=20,color="black",family= "Aril"),
                                showticklabels=TRUE,tickmode="auto",tickcolor="#444",linecolor = "#444"
                     )) %>% 
              add_annotations(x=0.5,y=d[which.max(d[,"Freq"]),"Freq"],
                              text = paste0("p-value = ",p.v),
                              #                    xref = x1,  # add this
                              #                    yref = y1,  # add this
                              showarrow = FALSE,
                              ax = 20,
                              ay = -40)
          })
        }
      })
    })
    
    output$single_cell_cell_plots_tSNE_cluster <- renderText({
      input$do_single_cell_cell 
      isolate({
        paste(input$cluster_single_cell_cell,collapse=",")
      })
    })
    output$single_cell_cell_plots_table_1 <- renderText({
      input$do_single_cell_cell 
      isolate({
        paste(input$cluster_single_cell_cell,collapse=",")
      })
    })
    output$single_cell_cell_plots_table_2 <- renderText({
      input$do_single_cell_cell 
      isolate({
        paste(input$cluster_single_cell_cell,collapse=",")
      })
    })
    output$single_cell_cell_plots_tSNE <- renderPlot({ 
      input$do_single_cell_cell 
      isolate({
        withProgress(message = 'Making plot', value = 0, {
#          selected.database <- selecteddatabase_single_cell_cell()
          color.grey <- rep("Grey",20)
          mainPalette=c('#4f00A8', '#CF59AE', '#ace7b9', '#F37812', '#CD5C5C', 
                        '#FF8C00', '#7CFC00', '#00FFFF',  '#A80100',  '#000000',  '#cdf0d5',  
                        '#F2B079',  '#E9967A',  '#FFFFE0',  '#228B22',  '#AFEEEE','#CF5A59',  
                        '#006666',  '#59CF74',  '#888811',  '#FF0000',  '#FFEFD5',  '#ADFF2F',  
                        '#00CED1',  '#A80079',  '#00A8A8',  '#002AA8',  '#FDF31C',  '#FF7F50',  
                        '#FFDAB9',  '#8FBC8F',  '#008080',  '#BC2D94',   '#009933',   '#5977CF',  
                        '#8C8C8C',   '#FFD700',   '#EEE8AA',   '#2E8B57',   '#B0C4DE')   
          for (i in 1:length(input$cluster_single_cell_cell)) { color.grey[as.numeric(input$cluster_single_cell_cell[i])+1]<-mainPalette[as.numeric(input$cluster_single_cell_cell[i])+1]}
          color.character<-color.grey
          #        color.character <- c(rep("Grey",as.numeric(input$cluster_single_cell_cell)),
          #                             rep("purple",1),
          #                             rep("Grey",length(unique(pbmc@active.ident))-as.numeric(input$cluster_single_cell_cell)))
          #    p1 <- ggplotly(TSNEPlot(pbmc,colors.use = color.character,cells.use = as.character(selected.database[selected.database[,"PFS.status"]=="nonresponse","cells"])),
          #                   height=480) %>% layout(xaxis=list(title="\n\ntSNE_1\nnonresponse",
          #                                                     range=c(min(pbmc@dr$tsne@cell.embeddings[,1]),max(pbmc@dr$tsne@cell.embeddings[,1]))),
          #                                          yaxis=list(range=c(min(pbmc@dr$tsne@cell.embeddings[,2]),max(pbmc@dr$tsne@cell.embeddings[,2]))))
          #    
          #    p2 <- ggplotly(TSNEPlot(pbmc,colors.use = color.character,cells.use = as.character(selected.database[selected.database[,"PFS.status"]=="response","cells"])),
          #                   height=480) %>% layout(xaxis=list(title="\n\ntSNE_1\nresponse",
          #                                                     range=c(min(pbmc@dr$tsne@cell.embeddings[,1]),max(pbmc@dr$tsne@cell.embeddings[,1]))),
          #                                          yaxis=list(range=c(min(pbmc@dr$tsne@cell.embeddings[,2]),max(pbmc@dr$tsne@cell.embeddings[,2]))))
          cells.non <- as.character(selected.database[selected.database[,"PFS.status"]=="nonresponse","cells"])
#          if (length(cells.non) >10000) {
#            cells.non <- cells.non[sample(cells.non,10000) %in% rownames(seurat_data_source@reductions$tsne)]
#          } else {
            cells.non <- cells.non[cells.non %in% rownames(seurat_data_source@reductions$tsne)]
#          }
          p1 <- #ggplotly(
            DimPlot(seurat_data_source,cols = color.character,cells = cells.non)
          #      ,height=500) %>% layout(xaxis=list(title="tSNE_1\nnonresponse",titlefont=list(size=23,color="black",family= "Aril"),
          #                                                     range=c(min(seurat_data_source@reductions$tsne@cell.embeddings[,1]),max(seurat_data_source@reductions$tsne@cell.embeddings[,1]))),
          #                                          yaxis=list(title="tSNE_2",titlefont=list(size=23,color="black",family= "Aril"),
          #                                                     range=c(min(seurat_data_source@reductions$tsne@cell.embeddings[,2]),max(seurat_data_source@reductions$tsne@cell.embeddings[,2]))))
          p1 <- p1 + ggtitle("")+xlab("tSNE_1\nnonresponse")+theme(legend.position = "none")
          cells.res <- as.character(selected.database[selected.database[,"PFS.status"]=="response","cells"])
#          if (length(cells.res) >10000) {
#            cells.res <- cells.res[sample(cells.res,10000) %in% rownames(seurat_data_source@reductions$tsne)]
#          } else {
            cells.res <- cells.res[cells.res %in% rownames(seurat_data_source@reductions$tsne)]
#          }
          p2 <- #ggplotly(
            DimPlot(seurat_data_source,cols = color.character,cells = cells.res)
          #      ,height=500) %>% layout(xaxis=list(title="tSNE_1\nresponse",titlefont=list(size=23,color="black",family= "Aril"),
          #                                                     range=c(min(seurat_data_source@reductions$tsne@cell.embeddings[,1]),max(seurat_data_source@reductions$tsne@cell.embeddings[,1]))),
          #                                          yaxis=list(title="tSNE_2",titlefont=list(size=23,color="black",family= "Aril"),
          #                                                     range=c(min(seurat_data_source@reductions$tsne@cell.embeddings[,2]),max(seurat_data_source@reductions$tsne@cell.embeddings[,2]))))
          #    p3 <- plot_ly() %>% layout(xaxis=list(title = "",
          #                                          zeroline = FALSE,
          #                                          showline = FALSE,
          #                                          showticklabels = FALSE,
          #                                          showgrid = FALSE),
          #                               yaxis=list(title = "",
          #                                          zeroline = FALSE,
          #                                          showline = FALSE,
          #                                          showticklabels = FALSE,
          #                                          showgrid = FALSE)) ### an empty chart
          p2 <- p2 + ggtitle("")+ xlab("tSNE_1\nresponse")
          incProgress(100, detail = "Doing part %")
          #    subplot(p3,subplot(style(p1,showlegend=FALSE),style(p2,showlegend=TRUE),margin=0.05,titleX=TRUE,titleY=TRUE),nrows=2,heights=c(1/10,9/10),titleX=TRUE,titleY=TRUE) %>% 
          #      layout(title = paste("<br><b>Cluster ",paste(input$cluster_single_cell_cell,collapse=","),"<b>",sep=""), 
          #             titlefont=list(size=20,color="black",family= "Arial"))
          grid.arrange(p1,p2,ncol=2,top=textGrob(paste("Cluster ",paste(input$cluster_single_cell_cell,collapse=","),sep=""),gp=gpar(fontsize=20,font=3)))
        }) 
      })
    })
    
    output$single_cell_cell_plots_tSNE_download <- downloadHandler(
      filename = function() {
        if (input$single_cell_cell_plots_tSNE_type == "Excel (CSV)") {
          "newfile.csv" 
        } else if (input$single_cell_cell_plots_tSNE_type == "Text (TSV)") {
          "newfile.txt"
        } else if (input$single_cell_cell_plots_tSNE_type == "JSON") {
          "newfile.json"
        }
      },
      content = function(file) {
        if (input$single_cell_cell_plots_tSNE_type == "Excel (CSV)") {
          write.csv (single_cell_cell_plots_tSNE_download_data(),file,row.names = FALSE)
        } else if (input$single_cell_cell_plots_tSNE_type == "Text (TSV)") {
          write.table (single_cell_cell_plots_tSNE_download_data(),file,quote=F, row.names = FALSE, sep="\t")
        } else if (input$single_cell_cell_plots_tSNE_type == "JSON") {
          write(toJSON(single_cell_cell_plots_tSNE_download_data()),file)
        }
      }
    )   
    single_cell_cell_plots_tSNE_download_data <- function() {
#      selected.database <- selecteddatabase_single_cell_cell()
      index.nonres <- selected.database[selected.database[,"PFS.status"]=="nonresponse",c("PatientID","cluster")]
      index.nonres[,1] <- as.character(index.nonres[,1])
      b.nonres<-table(index.nonres[,c("PatientID","cluster")])
      index.res <- selected.database[selected.database[,"PFS.status"]=="response",c("PatientID","cluster")]
      index.res[,1] <- as.character(index.res[,1])
      b.res<-table(index.res[,c("PatientID","cluster")])
      b<-rbind(data.frame(b.nonres,PFS.status="nonresponse"),data.frame(b.res,PFS.status="response"))
      selected.database<-merge(as.data.frame(b),as.data.frame(table(selected.database[,"PatientID"])),by.x="PatientID",by.y="Var1")
      colnames(selected.database)[c(3,5)] <- c("cells of cluster of patient","cells of patient")
      selected.database<-data.frame(selected.database,Freq=apply(selected.database,1,function(x) as.numeric(x[3])/as.numeric(x[5])*100))
      
      if (input$cluster_single_cell_cell=="") {
        selected.database <- selected.database %>% filter(cluster==0)
      } else {
        selected.database <- selected.database %>% filter(cluster %in% input$cluster_single_cell_cell)
      }
      selected.database <- merge(selected.database, database.test[database.test[,"data_source"]==input$data_source_single_cell_cell,1:10],by="PatientID",all.x=T)
      selected.database
    }
    output$single_cell_cell_plots_tSNE_table <- DT::renderDataTable({
      input$do_single_cell_cell 
      isolate({
        single_cell_cell_plots_tSNE_download_data()
      })
    })
    
    output$single_cell_cell_plots_heatmap_cluster <- renderText({
      input$do_single_cell_cell 
      isolate({
        paste(input$cluster_single_cell_cell,collapse=",")
      })
    })
    
    output$single_cell_cell_plots_heatmap <- renderPlot({
      input$do_single_cell_cell 
      isolate({
        withProgress(message = 'Making plot', value = 0, {
#          selected.database <- selecteddatabase_single_cell_cell()
          top10 <- markers_data_source %>% group_by(cluster) %>% top_n(10, wt=avg_logFC)
          top10 <- as.data.frame(top10)
          a<-top10[top10[,"cluster"] %in% input$cluster_single_cell_cell,]
          #        b <- selected.database[!is.na(selected.database[,"cluster"]),]
          incProgress(100, detail = "Doing part %")
          #        DoHeatmap(object = pbmc, genes.use = top10$gene, slim.col.label = TRUE, remove.key = TRUE,cex.row = 3, use.scaled = FALSE)
          #        DoHeatmap(object = pbmc, features = a$gene, cells = as.character(b[b[,"cluster"] %in% input$cluster_single_cell_cell,"cells"]), slot="counts")
          if (length(seurat_data_source@active.ident)>25000) {
            DoHeatmap(object = seurat_data_source, features = a$gene, cells = sample(names(seurat_data_source@active.ident),25000), slot = "data")
          } else {
            DoHeatmap(object = seurat_data_source, features = a$gene, slot = "data")
          }
          
          
        })
      })
    })
    
    output$single_cell_cell_plots_heatmap_download <- downloadHandler(
      filename = function() {
        if (input$single_cell_cell_plots_heatmap_type == "Excel (CSV)") {
          "newfile.csv" 
        } else if (input$single_cell_cell_plots_heatmap_type == "Text (TSV)") {
          "newfile.txt"
        } else if (input$single_cell_cell_plots_heatmap_type == "JSON") {
          "newfile.json"
        }
      },
      content = function(file) {
        if (input$single_cell_cell_plots_heatmap_type == "Excel (CSV)") {
          write.csv (single_cell_cell_plots_heatmap_download_data(),file,row.names = FALSE)
        } else if (input$single_cell_cell_plots_heatmap_type == "Text (TSV)") {
          write.table (single_cell_cell_plots_heatmap_download_data(),file,quote=F, row.names = FALSE, sep="\t")
        } else if (input$single_cell_cell_plots_heatmap_type == "JSON") {
          write(toJSON(single_cell_cell_plots_heatmap_download_data()),file)
        }
      }
    )   
    single_cell_cell_plots_heatmap_download_data <- function() {
      markers_data_source[markers_data_source[,"cluster"] %in% input$cluster_single_cell_cell,]
    }
    output$single_cell_cell_plots_heatmap_table <- DT::renderDataTable({
      input$do_single_cell_cell 
      isolate({
        single_cell_cell_plots_heatmap_download_data()
      })
    })
    
    #########################################
    #### single cell cell survival
    #########################################
    
    output$single_cell_cell_survival_plots <- renderPlotly({
      #    selecteddatabase_single_cell_cell() :
      #  PatientID      cells data_source treatment description PFS.status RECIST OS.status  OS PFS Age cluster
      #  1 Moshe_Post_P1 A10_P1_M15       Moshe anti-PD-1          On   response     NA    living 822  NA  49       1
      #  2 Moshe_Post_P1 A10_P4_M15       Moshe anti-PD-1          On   response     NA    living 822  NA  49       2
      #  3 Moshe_Post_P1 A10_P5_M15       Moshe anti-PD-1          On   response     NA    living 822  NA  49       1
#      input$do_single_cell_cell
#      isolate({
#        selected.database <- selecteddatabase_single_cell_cell()
        if (nrow(selected.database)==0 | length(input$cluster_single_cell_cell)==0 | nrow(selected.database[!is.na(selected.database[,"OS"]),])==0) {
          plot_ly () %>% add_annotations(x=2,y=2,text="no data available",showarrow=F,font=list(size=16,color="purple"))
        } else {
          withProgress(message = 'Making plot', value = 0, {
            index.nonres <- selected.database[selected.database[,"PFS.status"]=="nonresponse",c("PatientID","cluster")]
            index.nonres[,1] <- as.character(index.nonres[,1])
            b.nonres<-table(index.nonres[,c("PatientID","cluster")])
            index.res <- selected.database[selected.database[,"PFS.status"]=="response",c("PatientID","cluster")]
            index.res[,1] <- as.character(index.res[,1])
            b.res<-table(index.res[,c("PatientID","cluster")])
            b<-rbind(data.frame(b.nonres,PFS.status="nonresponse"),data.frame(b.res,PFS.status="response"))
            selected.database<-merge(as.data.frame(b),as.data.frame(table(selected.database[,"PatientID"])),by.x="PatientID",by.y="Var1")
            colnames(selected.database)[c(3,5)] <- c("cells of cluster of patient","cells of patient")
            selected.database<-data.frame(selected.database,Freq=apply(selected.database,1,function(x) as.numeric(x[3])/as.numeric(x[5])*100))
            
            if (input$cluster_single_cell_cell=="") {
              selected.database <- selected.database %>% filter(cluster==0)
            } else {
              selected.database <- selected.database %>% filter(cluster %in% input$cluster_single_cell_cell)
            }
            selected.database <- merge(database.test[database.test[,"data_source"]==input$data_source_single_cell_cell,1:10],selected.database[,c("PatientID","cluster","Freq")],by="PatientID",all.x=T)
            d <- selected.database
            d <- d[!is.na(d[,"OS.status"]) & !is.na(d[,"OS"]) & !is.na(d[,"Freq"]),]
            d<-data.frame(d,dead=NA)
            d[d[,"OS.status"]=="living" & !is.na(d[,"OS.status"]),"dead"] <- 0
            d[d[,"OS.status"]=="deceased" & !is.na(d[,"OS.status"]),"dead"] <- 1
            d <- d[d[,"dead"]==1 | d[,"dead"]==0,]
            
            d<-data.frame(d[,1:6],unfactor(d[,c("dead","OS","PFS","Age","Freq")]))
            d<-data.frame(d,IND=paste0("samples with bottom ",
                                       input$slider_single_cell_cell_OS*100, 
                                       "% frequency (",
                                       nrow(d[d[,"Freq"]<= quantile(na.omit(d[,"Freq"]),input$slider_single_cell_cell_OS),]),
                                       ")"))
            d[,"IND"]<-as.character(d[,"IND"])
            d[d[,"Freq"]> quantile(na.omit(d[,"Freq"]),input$slider_single_cell_cell_OS),"IND"]<- 
              paste0("samples with top ",
                     (1-input$slider_single_cell_cell_OS)*100,
                     "% frequency (",
                     nrow(d[d[,"Freq"]> quantile(na.omit(d[,"Freq"]),input$slider_single_cell_cell_OS),])
                     ,")")
            d[,"dead"] <- as.numeric(d[,"dead"]) ### I don't know why I need to as.numeric since expression section doesn't need this
            
            d<-d[order(d[,"IND"]),]
            fit.result <- survfit(Surv(OS,dead)~IND,data=d)
            plot.result <- ggsurv(fit.result,surv.col = c("plum","lightgreen"))
            diff.result <- survdiff(Surv(OS,dead)~IND,data=d)
            p.value= 1 - pchisq(diff.result$chisq, length(diff.result$n) - 1)
            plot.result.ann <- plot.result + theme(legend.position = "right", legend.title = element_blank()) + 
              ggplot2::geom_text(aes(label = sprintf("p-value = %0.2g",
                                                     pchisq(diff.result$chisq,df=1,lower.tail = F)),x=200,y=0.05))
            p1<-ggplotly(plot.result.ann,legendgroup=~IND,height = 480 ) %>%
               layout(title = unique(d$data_source),titlefont=list(size=20,color="black",family= "Arial"),
                      xaxis=list(title="Days",titlefont=list(size=20,color="black",family= "Aril black")),
                      yaxis=list(title="Overall survival proportion",titlefont=list(size=20,color="black",family= "Aril black"))) #%>%
            incProgress(100, detail = "Doing part %") 
            p1
            
          })
        }
#      })
    })
    output$single_cell_cell_survival_n_samples_NA <- renderText({
      input$do_single_cell_cell
      isolate({
      nrow(selected.database)
      })
    })
    output$single_cell_cell_survival_download <- downloadHandler(
      filename = function() {
        if (input$single_cell_cell_survival_type == "Excel (CSV)") {
          "newfile.csv"
        } else if (input$single_cell_cell_survival_type == "Text (TSV)") {
          "newfile.txt"
        } else if (input$single_cell_cell_survival_type == "JSON") {
          "newfile.json"
        }
      },
      content = function(file) {
        if (input$single_cell_cell_survival_type == "Excel (CSV)") {
          write.csv (single_cell_cell_OS_download_data(),file,row.names = FALSE)
        } else if (input$single_cell_cell_survival_type == "Text (TSV)") {
          write.table (single_cell_cell_OS_download_data(),file,quote=F, row.names = FALSE, sep="\t")
        } else if (input$single_cell_cell_survival_type == "JSON") {
          write(toJSON(single_cell_cell_OS_download_data()),file)
        }
      }
    )
    
    
    single_cell_cell_OS_download_data <- function() {
#      selected.database <- selecteddatabase_single_cell_cell()
      if (nrow(selected.database)==0 | length(input$cluster_single_cell_cell)==0 | nrow(selected.database[!is.na(selected.database[,"OS"]),])==0) {
        selected.database
      } else {
      index.nonres <- selected.database[selected.database[,"PFS.status"]=="nonresponse",c("PatientID","cluster")]
      index.nonres[,1] <- as.character(index.nonres[,1])
      b.nonres<-table(index.nonres[,c("PatientID","cluster")])
      index.res <- selected.database[selected.database[,"PFS.status"]=="response",c("PatientID","cluster")]
      index.res[,1] <- as.character(index.res[,1])
      b.res<-table(index.res[,c("PatientID","cluster")])
      b<-rbind(data.frame(b.nonres,PFS.status="nonresponse"),data.frame(b.res,PFS.status="response"))
      selected.database<-merge(as.data.frame(b),as.data.frame(table(selected.database[,"PatientID"])),by.x="PatientID",by.y="Var1")
      colnames(selected.database)[c(3,5)] <- c("cells of cluster of patient","cells of patient")
      selected.database<-data.frame(selected.database,Freq=apply(selected.database,1,function(x) as.numeric(x[3])/as.numeric(x[5])*100))
      
      if (input$cluster_single_cell_cell=="") {
        selected.database <- selected.database %>% filter(cluster==0)
      } else {
        selected.database <- selected.database %>% filter(cluster %in% input$cluster_single_cell_cell)
      }
      selected.database <- merge(database.test[database.test[,"data_source"]==input$data_source_single_cell_cell,1:10],selected.database[,c("PatientID","cluster","Freq")],by="PatientID",all.x=T)
      d <- selected.database
      d <- d[!is.na(d[,"OS.status"]) & !is.na(d[,"OS"]) & !is.na(d[,"Freq"]),]
      d<-data.frame(d,dead=NA)
      d[d[,"OS.status"]=="living" & !is.na(d[,"OS.status"]),"dead"] <- 0
      d[d[,"OS.status"]=="deceased" & !is.na(d[,"OS.status"]),"dead"] <- 1
      d <- d[d[,"dead"]==1 | d[,"dead"]==0,]
      
      d<-data.frame(d[,1:6],unfactor(d[,c("dead","OS","PFS","Age","Freq")]))
      d<-data.frame(d,IND=paste0("samples with bottom ",
                                 input$slider_single_cell_cell_OS*100, 
                                 "% frequency (",
                                 nrow(d[d[,"Freq"]<= quantile(d[,"Freq"],input$slider_single_cell_cell_OS),]),
                                 ")"))
      d[,"IND"]<-as.character(d[,"IND"])
      d[!is.na(d[,"Freq"]) & d[,"Freq"]> quantile(na.omit(d[,"Freq"]),input$slider_single_cell_cell_OS),"IND"]<-
        paste0("samples with top ",
               (1-input$slider_single_cell_cell_OS)*100,
               "% frequency (",
               nrow(d[d[,"Freq"]> quantile(d[,"Freq"],input$slider_single_cell_cell_OS),])
               ,")")
      d[,"dead"] <- as.numeric(d[,"dead"])
      d
      }
    }
    output$single_cell_cell_survival_table <- DT::renderDataTable({
#      input$do_single_cell_cell 
#      isolate({
        single_cell_cell_OS_download_data()
#      })
    })
    #################################################
    #### single cell cell PFS
    #################################################
    output$single_cell_cell_PFS_plots <- renderPlotly({
      #    selecteddatabase_single_cell_cell() :
      #  PatientID      cells data_source treatment description PFS.status RECIST OS.status  OS PFS Age cluster
      #  1 Moshe_Post_P1 A10_P1_M15       Moshe anti-PD-1          On   response     NA    living 822  NA  49       1
      #  2 Moshe_Post_P1 A10_P4_M15       Moshe anti-PD-1          On   response     NA    living 822  NA  49       2
      #  3 Moshe_Post_P1 A10_P5_M15       Moshe anti-PD-1          On   response     NA    living 822  NA  49       1
      #      input$do_single_cell_cell
      #      isolate({
      #        selected.database <- selecteddatabase_single_cell_cell()
#      selected.database <- selecteddatabase_single_cell_cell()
      if (nrow(selected.database)==0 | length(input$cluster_single_cell_cell)==0 | nrow(selected.database[!is.na(selected.database[,"PFS"]),])==0) {
        plot_ly () %>% add_annotations(x=2,y=2,text="no data available",showarrow=F,font=list(size=16,color="purple"))
      } else {
        withProgress(message = 'Making plot', value = 0, {
          index.nonres <- selected.database[selected.database[,"PFS.status"]=="nonresponse",c("PatientID","cluster")]
          index.nonres[,1] <- as.character(index.nonres[,1])
          b.nonres<-table(index.nonres[,c("PatientID","cluster")])
          index.res <- selected.database[selected.database[,"PFS.status"]=="response",c("PatientID","cluster")]
          index.res[,1] <- as.character(index.res[,1])
          b.res<-table(index.res[,c("PatientID","cluster")])
          b<-rbind(data.frame(b.nonres,PFS.status="nonresponse"),data.frame(b.res,PFS.status="response"))
          selected.database<-merge(as.data.frame(b),as.data.frame(table(selected.database[,"PatientID"])),by.x="PatientID",by.y="Var1")
          colnames(selected.database)[c(3,5)] <- c("cells of cluster of patient","cells of patient")
          selected.database<-data.frame(selected.database,Freq=apply(selected.database,1,function(x) as.numeric(x[3])/as.numeric(x[5])*100))
          
          if (input$cluster_single_cell_cell=="") {
            selected.database <- selected.database %>% filter(cluster==0)
          } else {
            selected.database <- selected.database %>% filter(cluster %in% input$cluster_single_cell_cell)
          }
          selected.database <- merge(database.test[database.test[,"data_source"]==input$data_source_single_cell_cell,1:10],selected.database[,c("PatientID","cluster","Freq")],by="PatientID",all.x=T)
          d <- selected.database
          d <- d[!is.na(d[,"PFS.status"]) & !is.na(d[,"PFS"]) & !is.na(d[,"Freq"]),]
          d<-data.frame(d[,1:6],unfactor(d[,c("OS.status","OS","PFS","Age","Freq")]))
          d<-data.frame(d,IND=paste0("samples with bottom ",
                                     input$slider_single_cell_cell_PFS*100, 
                                     "% frequency (",
                                     nrow(d[d[,"Freq"]<= quantile(na.omit(d[,"Freq"]),input$slider_single_cell_cell_PFS),]),
                                     ")"))
          d[,"IND"]<-as.character(d[,"IND"])
          d[d[,"Freq"]> quantile(na.omit(d[,"Freq"]),input$slider_single_cell_cell_PFS),"IND"]<- 
            paste0("samples with top ",
                   (1-input$slider_single_cell_cell_PFS)*100,
                   "% frequency (",
                   nrow(d[d[,"Freq"]> quantile(na.omit(d[,"Freq"]),input$slider_single_cell_cell_PFS),])
                   ,")")
          d <- data.frame(d,group.IND=0)
          d[d[,"PFS.status"]=="nonresponse","group.IND"] <- 1
          d<-d[order(d[,"IND"]),]
          fit.result <- survfit(Surv(PFS,group.IND)~IND,data=d)
          plot.result <- ggsurv(fit.result,surv.col = c("plum","lightgreen"))
          diff.result <- survdiff(Surv(PFS,group.IND)~IND,data=d)
          p.value= 1 - pchisq(diff.result$chisq, length(diff.result$n) - 1)
          plot.result.ann <- plot.result + theme(legend.position = "right", legend.title = element_blank()) + 
            ggplot2::geom_text(aes(label = sprintf("p-value = %0.2g",
                                                   pchisq(diff.result$chisq,df=1,lower.tail = F)),x=200,y=0.05))
          p1<-ggplotly(plot.result.ann,legendgroup=~IND,height = 480 ) %>%
            layout(title = unique(d$data_source),titlefont=list(size=20,color="black",family= "Arial"),
                   xaxis=list(title="Days",titlefont=list(size=20,color="black",family= "Aril black")),
                   yaxis=list(title="Overall survival proportion",titlefont=list(size=20,color="black",family= "Aril black"))) #%>%
          incProgress(100, detail = "Doing part %")
          p1
          
        })
      }
    
      #      })
    })

    output$single_cell_cell_PFS_n_samples_NA <- renderText({
      input$do_single_cell_cell
      isolate({
        nrow(selected.database)
      })
    })
    output$single_cell_cell_PFS_download <- downloadHandler(
      filename = function() {
        if (input$single_cell_cell_PFS_type == "Excel (CSV)") {
          "newfile.csv"
        } else if (input$single_cell_cell_PFS_type == "Text (TSV)") {
          "newfile.txt"
        } else if (input$single_cell_cell_PFS_type == "JSON") {
          "newfile.json"
        }
      },
      content = function(file) {
        if (input$single_cell_cell_PFS_type == "Excel (CSV)") {
          write.csv (single_cell_cell_PFS_download_data(),file,row.names = FALSE)
        } else if (input$single_cell_cell_PFS_type == "Text (TSV)") {
          write.table (single_cell_cell_PFS_download_data(),file,quote=F, row.names = FALSE, sep="\t")
        } else if (input$single_cell_cell_PFS_type == "JSON") {
          write(toJSON(single_cell_cell_PFS_download_data()),file)
        }
      }
    )
    
    
    single_cell_cell_PFS_download_data <- function() {
#      selected.database <- selecteddatabase_single_cell_cell()
      if (nrow(selected.database)==0 | length(input$cluster_single_cell_cell)==0 | nrow(selected.database[!is.na(selected.database[,"PFS"]),])==0) {
        selected.database
      } else {
      index.nonres <- selected.database[selected.database[,"PFS.status"]=="nonresponse",c("PatientID","cluster")]
      index.nonres[,1] <- as.character(index.nonres[,1])
      b.nonres<-table(index.nonres[,c("PatientID","cluster")])
      index.res <- selected.database[selected.database[,"PFS.status"]=="response",c("PatientID","cluster")]
      index.res[,1] <- as.character(index.res[,1])
      b.res<-table(index.res[,c("PatientID","cluster")])
      b<-rbind(data.frame(b.nonres,PFS.status="nonresponse"),data.frame(b.res,PFS.status="response"))
      selected.database<-merge(as.data.frame(b),as.data.frame(table(selected.database[,"PatientID"])),by.x="PatientID",by.y="Var1")
      colnames(selected.database)[c(3,5)] <- c("cells of cluster of patient","cells of patient")
      selected.database<-data.frame(selected.database,Freq=apply(selected.database,1,function(x) as.numeric(x[3])/as.numeric(x[5])*100))
      
      if (input$cluster_single_cell_cell=="") {
        selected.database <- selected.database %>% filter(cluster==0)
      } else {
        selected.database <- selected.database %>% filter(cluster %in% input$cluster_single_cell_cell)
      }
      selected.database <- merge(database.test[database.test[,"data_source"]==input$data_source_single_cell_cell,1:10],selected.database[,c("PatientID","cluster","Freq")],by="PatientID",all.x=T)
      d <- selected.database
      d <- d[!is.na(d[,"PFS.status"]) & !is.na(d[,"PFS"]) & !is.na(d[,"Freq"]),]
      d<-data.frame(d[,1:6],unfactor(d[,c("OS.status","OS","PFS","Age","Freq")]))
      d<-data.frame(d,IND=paste0("samples with bottom ",
                                 input$slider_single_cell_cell_PFS*100, 
                                 "% frequency (",
                                 nrow(d[d[,"Freq"]<= quantile(na.omit(d[,"Freq"]),input$slider_single_cell_cell_PFS),]),
                                 ")"))
      d[,"IND"]<-as.character(d[,"IND"])
      d[d[,"Freq"]> quantile(na.omit(d[,"Freq"]),input$slider_single_cell_cell_PFS),"IND"]<- 
        paste0("samples with top ",
               (1-input$slider_single_cell_cell_PFS)*100,
               "% frequency (",
               nrow(d[d[,"Freq"]> quantile(na.omit(d[,"Freq"]),input$slider_single_cell_cell_PFS),])
               ,")")
      d
      }
    }
    output$single_cell_cell_PFS_table <- DT::renderDataTable({
      #      input$do_single_cell_cell 
      #      isolate({
      single_cell_cell_PFS_download_data()
      #      })
    })  
     })    
     #######################################################
     #### users' data
     #######################################################
     output$user_clinical_table <- renderTable({
       if (is.null(input$file_clinical)) {
         return(NULL)
       } else {
         
         df_clinical <- user_clinical()
         if (input$display_clinical=="All") {
           df_clinical  
         } else {
           if (nrow(df_clinical)>=10) {
             df_clinical[1:10,]
           } else {
             df_clinical
           }
         }
       }
     })
     output$user_transcriptome_table <- renderTable({
       if (is.null(input$file_transcriptome)) {
         return(NULL)
       } else {
         
         df_transcriptome <- user_transcriptome()
         if (input$display_transcriptome=="All") {
           df_transcriptome  
         } else {
           if (ncol(df_transcriptome)>=10) {
             df_transcriptome[1:10,1:10]
           } else {
             df_transcriptome[1:10,]
           }
         }
       }
     })
     output$user_mutation_table <- renderTable({
       if (is.null(input$file_mutation)) {
         return(NULL)
       } else {
         
         df_mutation <- user_mutation()
         if (input$display_mutation=="All") {
           df_mutation  
         } else {
           if (nrow(df_mutation)>=10) {
             df_mutation[1:10,]
           } else {
             df_mutation
           }
         }
       }
     })
     observeEvent(input$do_user_data, {
       for (i in 1:100) {
         updateProgressBar(
           session = session,
           id = "pb2",
           value = i, total = 100,
           title = paste("Process", trunc(i/10))
         )
         database.test.process()
         all.mutations.process()
       }
     })
     
     
  
  shinyjs::hide(id = "loading-content", anim = TRUE, animType = "fade")    
  shinyjs::show("app-content")
  
})