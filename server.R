

#**************************************
shinyServer(function(input, output, session) {
  v<-reactiveValues(check1=F,tempRes=NULL,allData=NULL)
  
  
  # output$tbl<-renderTable({
  allData<-list(read_excel("test.xlsx", sheet = "final"))
  
  
  cn<-names(allData[[1]])
  cmax<-0
  tempRes<-list()

  
  for (i in 8:(length(cn)-2)){
    co<-allData[[1]][cn[i]]
    
    
    allCo<-unlist(lapply(co,function(x) unlist(strsplit(x,";"))))
    
    allCo<-unlist(lapply(allCo,function(x) gsub("\r\r","",x)))
    
    
    tempTable<-as.data.frame(sort(table(allCo),decreasing = T))
    cmax<-max(cmax,nrow(tempTable))
    
    tempRes<-c(tempRes,list(tempTable))
    
    
  }
  
  i<-5
  co<-allData[[1]][cn[i]]
  allCo<-unlist(co)
  tempTable<-as.data.frame(sort(table(allCo),decreasing = T))
  cmax<-max(cmax,nrow(tempTable))
  tempRes<-c(tempRes,list(tempTable))
  
  
  
  dff<-as.data.frame(matrix("",cmax,length(tempRes)*2))
  
  
  
  for (i in 1:length(tempRes)){
    tempt<-tempRes[[i]]
    dff[1:nrow(tempt),(i*2-1)]<-as.character(tempt$allCo)
    dff[1:nrow(tempt),(i*2)]<-tempt$Freq
  }
  
  # v$allData<-allData
  # v$tempRes<-tempRes
  
  # dff
  # 
  # })
  # 
  
  
  # observeEvent(input$tabSwitch, {
  #   
  #   
  #   updateSelectInput("svar1","variable 1",c("Year",cn[8:length(cn)]),selected=cn[11])
  #   updateSelectInput("svar2","variable 2",c("Year",cn[8:length(cn)]),selected=cn[12])
  #   updateSelectInput("svar3","variable 3",c("Year",cn[8:length(cn)]),selected=cn[13])
  #   
  # })
  output$var1<-renderUI({
    
    selectInput("svar1","variable 3",c("Year",cn[8:length(cn)]),selected = cn[11])
    
  })
  output$var2<-renderUI({
    selectInput("svar2","variable 2",c("Year",cn[8:length(cn)]),selected = cn[12])
    
  })
  output$var3<-renderUI({
    selectInput("svar3","variable 1",c("Year",cn[8:length(cn)]),selected = cn[13])
    
  })
  
  output$var1_2<-renderUI({
    selectInput("svar1_2","variable 1",c("Year",cn[8:length(cn)]),selected = cn[11])
    
  })
  output$var2_2<-renderUI({
    selectInput("svar2_2","variable 2",c("Year",cn[8:length(cn)]),selected = cn[12])
    
  })
  
  
  
  output$var1_3<-renderUI({
    selectInput("svar1_3","variable 1",c(cn[8:length(cn)]),selected = cn[11])
    
  })
  
  output$var1_4<-renderUI({
    selectInput("svar1_4","variable 1",c(cn[8:length(cn)]),selected = cn[11])
    
  })
  
  output$var1_5<-renderUI({
    selectInput("svar1_5","variable 1",c(cn[8:length(cn)]),selected = cn[11])
    
  })
  
  
  output$plt2_holder<-renderUI({
    if (input$chord_plot=="chord")
      return(plotOutput("plt2"))
    else
     return(plotlyOutput("plt2_2"))
  })
  
  
  
  
  output$plt1 <- renderSankeyNetwork({
    
    
    if (is.null(input$svar1)) return()
    if (is.null(input$svar2)) return()
    if (is.null(input$svar3)) return()
    # if (is.null(input$thres11)) return()
    # if (is.null(input$thres13)) return()
    if (is.null(input$thres21)) return()
    if (is.null(input$thres23)) return()
    
    nvar1<-which(cn%in%input$svar1)-7
    nvar2<-which(cn%in%input$svar2)-7
    nvar3<-which(cn%in%input$svar3)-7
    
    if (!((nvar1!=nvar2)&(nvar1!=nvar3)&(nvar2!=nvar3)))  return()
    
    pair1<-quantAnal(nvar1,nvar2,allData,tempRes,cn,co)
    pair2<-quantAnal(nvar2,nvar3,allData,tempRes,cn,co)
    
    
    '=========================================================================='
    
    keep1<-apply(pair1,1,function(x) sum(x)>=input$thres22_2)
    keep2<-apply(pair1,2,function(x) sum(x)>=input$thres21)
    removedCat<-rownames(pair1)[!keep1]
    
    pair1<-pair1[keep1,keep2]
    pair2<-pair2[,!(colnames(pair2)%in%removedCat)]
    
    keep1<-apply(pair2,1,function(x) sum(x)>=input$thres23)
    keep2<-apply(pair2,2,function(x) sum(x)>=input$thres22_1)
    removedCat<-colnames(pair2)[!keep2]
    
    pair2<-pair2[keep1,keep2]
    pair1<-pair1[!(rownames(pair1)%in%removedCat),]
    
    
    '----------------------------------------------------------------------------'
    
    
    
    # keep1<-apply(pair1,1,function(x) max(x)>=input$thres12_2)
    # keep2<-apply(pair1,2,function(x) max(x)>=input$thres11)
    # removedCat<-rownames(pair1)[!keep1]
    # 
    # pair1<-pair1[keep1,keep2]
    # pair2<-pair2[,!(colnames(pair2)%in%removedCat)]
    # 
    # keep1<-apply(pair2,1,function(x) max(x)>=input$thres13)
    # keep2<-apply(pair2,2,function(x) max(x)>=input$thres12_1)
    # removedCat<-colnames(pair2)[!keep2]
    # 
    # pair2<-pair2[keep1,keep2]
    # pair1<-pair1[!(rownames(pair1)%in%removedCat),]
    
    
    
    # pair1<-pair1[,apply(pair1,2,function(x) max(x)>=input$thres11)]
    # pair2<-pair2[apply(pair2,1,function(x) max(x)>=input$thres13),]
    pair1[pair1 %in% (0:(input$thres11-1))] <- 0
    
    
    pair2[pair2 %in% (0:(input$thres13-1))] <- 0
    
    
    
    
    if ((is.null(dim(pair1)))|(is.null(dim(pair2)))) return()
    if (min(dim(pair1))<2) return()
    if (min(dim(pair2))<2) return()
    
    # print(pair1)
    
    links<-quantAnal2(pair1,pair2)
    
    
    
    
    links<-links[!(links$value==0),]
    
    
    # for (i in nrow(links):1){
    # 
    #   if (links$target[i]%in%colnames(pair1)){
    #     if (links$value[i]<input$thres11){
    #       links<-links[-i,]
    #     }
    # 
    #   }
    #   if (links$source[i]%in%rownames(pair2)){
    #     if (links$value[i]<input$thres13){
    #       links<-links[-i,]
    #     }
    # 
    #   }
    # }
    
    nod<-unique(c(as.character(links$source),as.character(links$target)))
    
    
    
    
    # nodes <- data.frame(
    #   name=c(as.character(links$source), 
    #          as.character(links$target)) %>% unique()
    # )
    nodes <- data.frame(
      name=unique(c(as.character(links$source), 
                    as.character(links$target)))
    )
    # browser()
    
    
    
    
    nodes$group<-as.factor(substring(nod, 1, 10))
    
    # With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
    links$IDsource <- match(links$source, nodes$name)-1 
    links$IDtarget <- match(links$target, nodes$name)-1
    
    # Make the Network
    
    p <- sankeyNetwork(Links = links, Nodes = nodes,
                       Source = "IDsource", Target = "IDtarget",fontFamily = "Calibri",
                       Value = "value", NodeID = "name", NodeGroup="group",
                       sinksRight=FALSE,fontSize = 20)
    p
    
  })
  
  
  
  output$plt2 <- renderPlot({
    
    if (is.null(input$svar1_2)) return()
    if (is.null(input$svar2_2)) return()
    if (is.null(input$thres1_2)) return()
    if (input$chord_plot!="chord") return()

    

    
    if (input$svar1_2=="Year")
      nvar1<-1
    else
      nvar1<-which(cn%in%input$svar1_2)-7

    if (input$svar2_2=="Year")
      nvar2<-1
    else
      nvar2<-which(cn%in%input$svar2_2)-7
    # nvar1<-which(cn%in%input$svar1_2)-6
    # nvar2<-which(cn%in%input$svar2_2)-6
    if ((nvar1==nvar2))  return()
    
    fSize<-input$fontSize2
    
    
    mat<-quantAnal(nvar1,nvar2,allData,tempRes,cn,co)
    
    circos.clear()
    nm = unique(unlist(dimnames(mat)))
    
    
    mat<-mat[!(rownames(mat)%in%"NA"),!(colnames(mat)%in%"NA")]
    
    mat<-mat[apply(mat,1,function(x) sum(x)>=input$thres2_2_2),apply(mat,2,function(x) sum(x)>=input$thres1_2_2)]
    # mat<-mat[apply(mat,1,function(x) max(x)>=input$thres2_2),apply(mat,2,function(x) max(x)>=input$thres1_2)]
    
    mat[mat %in% (0:(input$thres1_2-1))] <- 0
    


      saveComb(mat)
      
      group = structure(gsub("\\d", "", nm), names = nm)
      
      group<-structure(c(rep("A",nrow(mat)),rep("B",ncol(mat))),names=c(rownames(mat),colnames(mat)))
      
      
      # grid.col = structure(c(rep(2, 5), rep(3, 5), rep(4, 5)),
      #                      names = c(paste0("A", 1:5), paste0("B", 1:5), paste0("C", 1:5)))
      # 
      # chordDiagram(mat, group = group, grid.col = grid.col,fontsize(4))
      # browser()
      # grid.col <- setNames(rainbow(length(unlist(dimnames(mat)))), union(rownames(mat), colnames(mat)))
      grid.col <- setNames(rainbow(length(group)), names(group))
      
      # now, the image with rotated labels
      circos.par(start.degree = 90, clock.wise = FALSE)
      chordDiagram(mat, annotationTrack = "grid", preAllocateTracks = 1, grid.col = grid.col,fontsize(fSize))
      circos.trackPlotRegion(track.index = 1, panel.fun = function(x, y) {
        xlim = get.cell.meta.data("xlim")
        ylim = get.cell.meta.data("ylim")
        sector.name = get.cell.meta.data("sector.index")
        circos.text(mean(xlim), ylim[1] + .1, sector.name, facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.8))
        circos.axis(h = "top", labels.cex = 1,major.tick.length=2,major.tick = TRUE,minor.ticks=4, sector.index = sector.name, track.index = 2)
      }, bg.border = NA)
    

  }, height = 1000, width = 1000)
  
  
  
  output$plt2_2 <- renderPlotly({
    
    if (is.null(input$svar1_2)) return()
    if (is.null(input$svar2_2)) return()
    if (is.null(input$thres1_2)) return()
    if (input$chord_plot!="heatmap") return()
    
    
    if (input$svar1_2=="Year")
      nvar1<-1
    else
      nvar1<-which(cn%in%input$svar1_2)-7
    
    if (input$svar2_2=="Year")
      nvar2<-1
    else
      nvar2<-which(cn%in%input$svar2_2)-7
    # nvar1<-which(cn%in%input$svar1_2)-6
    # nvar2<-which(cn%in%input$svar2_2)-6
    if ((nvar1==nvar2))  return()
    
    fSize<-input$fontSize2
    mat<-quantAnal(nvar1,nvar2,allData,tempRes,cn,co)
    
    circos.clear()
    nm = unique(unlist(dimnames(mat)))
    
    
    mat<-mat[!(rownames(mat)%in%"NA"),!(colnames(mat)%in%"NA")]
    
    mat<-mat[apply(mat,1,function(x) sum(x)>=input$thres2_2_2),apply(mat,2,function(x) sum(x)>=input$thres1_2_2)]
    # mat<-mat[apply(mat,1,function(x) max(x)>=input$thres2_2),apply(mat,2,function(x) max(x)>=input$thres1_2)]
    
    mat[mat %in% (0:(input$thres1_2-1))] <- 0
    mat<-mat[apply(mat,1,function(x) sum(x)>=input$thres2_2_2),apply(mat,2,function(x) sum(x)>=input$thres1_2_2)]


      return(heatmaply(mat,dendrogram='none',fontsize_row=fSize,fontsize_col=fSize,
                      # colors = colorRampPalette(brewer.pal(3, "RdBu"))(256))
             colors = coolwarm(128))
             %>% layout(width=1200,height=800,xaxis=list(tickfont = list(family = "Calibri")),yaxis=list(tickfont = list(family = "Calibri")))
             )
    
  })
  
  
  output$plt3 <- renderGvis({
    if (is.null(input$svar1_3)) return()
    if (is.null(input$thres1_3)) return()
    
    if (input$svar1_3=="Year")
      nvar1<-1
    else
      nvar1<-which(cn%in%input$svar1_3)-7
    if (length(tempRes)<nvar1) return()
    

    df<-data.frame(as.character(tempRes[[nvar1]]$allCo),tempRes[[nvar1]]$Freq)
    df<-df[!df[,1]%in%"NA",]
    df<-df[!df[, 2]<input$thres1_3, ]
    
    colnames(df)<-c("test","frequency")

    if (input$bar_plot=="Pie")
      return(gvisPieChart(df,options=list(seriesType="bars",
                        vAxis=paste0("{slantedText:'true',showTextEvery:10,textStyle:{fontName:'calibri', fontSize:",input$fontSize3,"}}"),
                        bar="{groupWidth:'66%'}", width=780, height=900)))
    else if (input$bar_plot=="Bar")
    {
      browser()
      return(gvisBarChart(df,
                          options=list(seriesType="bars", legend="top",
                                       vAxis=paste0("{slantedText:'true',showTextEvery:10,textStyle:{fontName:'calibri',fontSize:",input$fontSize3,"}}"),
                                       bar="{groupWidth:'50%'}", width=880, height=min(1200,nrow(df)*55)),
                          chartid="thincolumns"))
    }
    else if (input$bar_plot=="Column")
      return(gvisColumnChart(df,
                          options=list(seriesType="bars", legend="top",
                                       vAxis=paste0("{slantedText:'true',showTextEvery:10,textStyle:{fontName:'calibri',fontSize:",input$fontSize3,"}}"),
                                       bar="{groupWidth:'66%'}", width=780, height=min(1200,nrow(df)*55)),
                          chartid="thincolumns"))
    
  })
  
  
  
  output$dt3 <- renderDataTable({
    if (is.null(input$svar1_3)) return()
    
    if (input$svar1_3=="Year")
      nvar1<-1
    else
      nvar1<-which(cn%in%input$svar1_3)-6
    if (length(tempRes)<nvar1) return()
    
    df<-data.frame(as.character(tempRes[[nvar1]]$allCo),tempRes[[nvar1]]$Freq)
    df<-df[!df[,1]%in%"NA",]
    
    colnames(df)<-c("Name","Frequency")
    
    df
    
  })
  
  
  

  
  
  output$plt5 <- renderGvis({
    if (is.null(input$svar1_5)) return()
    if (is.null(input$thres1_5)) return()
    

    nvar2<-which(cn%in%input$svar1_5)-7
    mat<-quantAnalTime(-2,nvar2,allData,tempRes,cn,co)
    

    df<-t(mat)
    df<-df[,!colnames(df)%in%"NA"]
    df<-df[, apply(df,2,sum)>=input$thres1_5]
    
    df<-cbind(as.numeric(rownames(df)),df)
    colnames(df)[1]<-"year"
    df<-as.data.frame(df)
    df$year<-as.character(df$year)
    
    
  return(gvisColumnChart(df, xvar="year", yvar=colnames(df[-1]),
         options=list(seriesType="bars",
                      hAxis.textStyle="{fontSize:20}",
                      bar="{groupWidth:'66%'}", width=800, height=600)))
         
    
  })
  
  
  
  # output$plt4 <- renderPlot({
  #   
  #   if (is.null(input$svar1_4)) return()
  #   
  #   
  #   df<-vennMakeDF(as.matrix(allData[[1]][input$svar1_4]))
  #   
  #   df<-df[!names(df)%in%"NA"]
  # 
  #   df<-df[names(sort(unlist(lapply(df,length)),decreasing = T))[1:3]]
  #   set.seed(10)
  #   colors<-rainbow(length(df))
  #   venn.diagram(
  #     x = df,
  #     category.names = names(df),filename = "test.png",
  #     imagetype="png", 
  #     scaled = TRUE,
  #     col = "black",
  #     fill = colors ,
  #     cat.col = colors,
  #     cat.cex = 0.8,
  #     margin = 0.15)
  #   # Only plot the Venn diagram if the input values are legit (the input values
  #   # are all available and the intersection area isn't greater than either individual set). 
  #   # if(as.numeric(req(input$adobeGoogle)) <= as.numeric(req(input$adobe)) &&
  #   #    as.numeric(req(input$adobeGoogle)) <= as.numeric(req(input$google))){
  #   #   
  #   #   draw.pairwise.venn(area1 = as.numeric(input$google),
  #   #                      area2 = as.numeric(input$adobe),
  #   #                      cross.area = as.numeric(input$adobeGoogle),
  #   #                      category = c("Google Analytics","Adobe Analytics"),
  #   #                      fill = c("#F29B05","#A1D490"),
  #   #                      ext.text = TRUE,
  #   #                      ext.percent = c(0.1,0.1,0.1),
  #   #                      ext.length = 0.6,
  #   #                      label.col = rep("gray10",3),
  #   #                      lwd = 0,
  #   #                      cex = 2,
  #   #                      fontface = rep("bold",3),
  #   #                      fontfamily = rep("sans",3), 
  #   #                      cat.cex = 1.5,
  #   #                      cat.fontface = rep("plain",2),
  #   #                      cat.fontfamily = rep("sans",2),
  #   #                      cat.pos = c(0, 0),
  #   #                      print.mode = c("percent","raw")
  #   #   )
  #   # }
  # })
  
  output$plt4_2 <- renderImage({
    
    if (is.null(input$svar1_4)) return()

    
    df<-vennMakeDF(as.matrix(allData[[1]][input$svar1_4]))
    
    df<-df[!names(df)%in%"NA"]
    
    df<-df[names(sort(unlist(lapply(df,length)),decreasing = T))[1:3]]
    set.seed(10)
    colors<-rainbow(length(df))
    
    futile.logger::flog.threshold(futile.logger::ERROR, name = "VennDiagramLogger")
    venn.diagram(
      x = df,
      category.names = names(df),filename = "images/test.png",
      imagetype="png", 
      scaled = TRUE,
      col = "black",
      fill = colors ,
      cat.col = colors,
      cat.cex = 0.8,
      margin = 0.15)
    
    return(list(
      src = "images/test.png",
      contentType = "image/png",
      alt = "Face",
      width = 600,
      height = 600
    ))
    
  }, deleteFile = FALSE)
  
  
  
  
  # 
  # output$plotholder <- renderUI({
  #   # if (!is.null(input$svar1))
  #   #   return (sankeyNetworkOutput('plt1'))
  #   # else if (!is.null(input$svar1_1))
  #   #   return (plotOutput('plt2'))
  #   return (plotOutput('plt2'))
  # })
  
  
  
  
})


vennMakeDF<-function(dat)
{
  allOptions<-unique(unlist(strsplit(dat,";")))
  allOptions<-allOptions[!allOptions%in%"NA"]
  
  empty_list <- vector(mode = "list", length = length(allOptions))
  names(empty_list)<-allOptions
  
  
  # df<-as.data.frame(matrix(0,length(allOptions),length(allOptions)))
  # colnames(df)<-allOptions
  # rownames(df)<-allOptions
  # 
  # for (i in 1:length(dat)){
  #     tempDat<-unlist(strsplit(dat[i],";"))
  #     ind<-which(colnames(df)%in%tempDat)
  #     for (j in ind){
  #         for (k in ind){
  #           df[j,k]<-df[j,k]+1
  #         }
  #     }
  #     
  # }
  # 
  # df
  for (i in 1:length(dat)){
    tempDat<-unlist(strsplit(dat[i],";"))
    for (t in tempDat){
      if (is.null(empty_list[t][[1]]))
        empty_list[t][[1]]<-i
      else
        empty_list[t][[1]]<-c(empty_list[t][[1]],i)
    }
    
  }
  
  
  
  empty_list
  
}





quantAnal<-function(n1,n2,allData,tempRes,cn,co)
{
  
  year<-unique(unlist(allData[[1]]['Year']))
  
  if (n1==-2)
  {var1<-year}
  else
    var1<-as.character(tempRes[[n1]]$allCo)
  
  
  var2<-as.character(tempRes[[n2]]$allCo)
  
  
  
  tempDF<-as.data.frame(matrix(0,length(var1),length(var2)))
  colnames(tempDF)<-var2
  rownames(tempDF)<-var1
  
  co1<-allData[[1]][cn[7+n1]]
  co2<-allData[[1]][cn[7+n2]]
  for (i in 1:length(co[[1]])){
    for (j in 1:length(var1)){
      for (k in 1:length(var2)){
        if (length(grep(var1[j],co1[[1]][i]))>0)
          if (length(grep(var2[k],co2[[1]][i]))>0)
            tempDF[j,k]<-tempDF[j,k]+1
      }
    }
  }
  
  return(t(tempDF))
}



quantAnal2<-function(var1,var2)
{
  
  tt<-var1
  
  tt<-tt[!(rownames(tt)%in%"NA"),!(colnames(tt)%in%"NA")]
  
  rnames<-row.names(tt)
  cnames<-colnames(tt)
  
  j<-1
  crData<-NULL
  for (i in 1:nrow(tt)){
    for (j in 1:ncol(tt)){
      crData<-rbind(crData,c(rnames[i],cnames[j],tt[i,j]))
    }
  }
  
  links<-crData
  
  tt<-var2
  
  tt<-tt[!(rownames(tt)%in%"NA"),!(colnames(tt)%in%"NA")]
  
  rnames<-row.names(tt)
  cnames<-colnames(tt)
  
  j<-1
  crData<-NULL
  for (i in 1:nrow(tt)){
    for (j in 1:ncol(tt)){
      crData<-rbind(crData,c(rnames[i],cnames[j],tt[i,j]))
    }
  }
  
  links<-rbind(links,crData)
  
  links<-as.data.frame(links)
  colnames(links)<-c("source","target","value")
  return(links)
  
}

quantAnalTime<-function(n1,n2,allData,tempRes,cn,co)
{
  
  year<-unique(unlist(allData[[1]]['Year']))
  var1<-year
  
  
  var2<-as.character(tempRes[[n2]]$allCo)
  
  
  
  tempDF<-as.data.frame(matrix(0,length(var1),length(var2)))
  colnames(tempDF)<-var2
  rownames(tempDF)<-var1
  
  co1<-allData[[1]][cn[7+n1]]
  co2<-allData[[1]][cn[7+n2]]
  for (i in 1:length(co[[1]])){
    for (j in 1:length(var1)){
      for (k in 1:length(var2)){
        if (length(grep(var1[j],co1[[1]][i]))>0)
          if (length(grep(var2[k],co2[[1]][i]))>0)
            tempDF[j,k]<-tempDF[j,k]+1
      }
    }
  }
  
  return(t(tempDF))
}
# 
# 
# makeSank<-function(links){
#   
#   links<-links[!(links$value==0),]
#   
#   nod<-unique(c(links$source,links$target))
#   
#   
#   
#   # From these flows we need to create a node data frame: it lists every entities involved in the flow
#   nodes <- data.frame(
#     name=c(as.character(links$source), 
#            as.character(links$target)) %>% unique()
#   )
#   
#   
#   
#   
#   nodes$group<-as.factor(substring(nod, 1, 10))
#   
#   my_color <- 'd3.scaleOrdinal() .domain([ "C01" ,"L04" ,"L01" ,"L01" ,"A04" ,"G04" ,"J01" ,"L04" ,"M05" ,"N05" ,"N06" ,"N06" ,"P02" ,"R05" ,"L04" ,"C01" ,"L01" ,"N06" ,"N06" ,"L01"
# "A04" ,"G04" ,"J01" ,"M05" ,"N05" ,"P02" ,"R05" ,"L04" ,"C01" ,"L01" ,"N06" ,"A04" ,"G04" ,"J01" ,"M05" ,"N05" ,"P02" ,"R05"]) .range(["#69b3a2", "steelblue"])'
#   
#   # With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
#   links$IDsource <- match(links$source, nodes$name)-1 
#   links$IDtarget <- match(links$target, nodes$name)-1
#   
#   
#   
#   
#   
#   # Make the Network
#   
#   
#   p <- sankeyNetwork(Links = links, Nodes = nodes,
#                      Source = "IDsource", Target = "IDtarget",
#                      Value = "value", NodeID = "name", NodeGroup="group",
#                      sinksRight=FALSE,fontSize = 20)
# 
#   p
# 
#   
# }
# 
# 
# makeCirc<-function(mat,title){
#   circos.clear()
#   nm = unique(unlist(dimnames(mat)))
#   
#   
#   mat<-mat[!(rownames(mat)%in%"NA"),!(colnames(mat)%in%"NA")]
#   
#   group = structure(gsub("\\d", "", nm), names = nm)
#   
#   group<-structure(c(rep("A",nrow(mat)),rep("B",ncol(mat))),names=c(rownames(mat),colnames(mat)))
#   
#   
#   grid.col = structure(c(rep(2, 5), rep(3, 5), rep(4, 5)),
#                        names = c(paste0("A", 1:5), paste0("B", 1:5), paste0("C", 1:5)))
#   chordDiagram(mat, group = group, grid.col = grid.col,fontsize(4))
# 
# }


# ppp<-lapply(dat,function(x) unlist(strsplit(x,";")))



saveComb<-function(df){
  
  
  temp<-apply(df[1:5,],1,function(x)  paste(names(sort(x,decreasing=T)), sort(x,decreasing=T)))
  
  write.table(temp,'test.csv',sep=",",row.names = F)
}





