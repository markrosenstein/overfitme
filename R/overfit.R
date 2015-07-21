#library(shiny)
#library(ggplot2)

#' Setup LDA simulated data
#'
#' @return list of data for LDA simulation
doLDA <- function() {
  set.seed(717)
  lda.tr.dat<-matrix(rnorm(20*14, 0, 1), nrow=20)
  lda.tr.dat<-data.frame(class=c(rep("c1", 10), rep("c2", 10)),
                         sig=c(rnorm(10, 2, 1), rnorm(10, 0, 1)), lda.tr.dat)
  lda.tst.dat<-matrix(rnorm(20*14, 0, 1), nrow=20)
  lda.tst.dat<-data.frame(class=c(rep("c1", 10), rep("c2", 10)),
                          sig=c(rnorm(10, 2, 1), rnorm(10, 0, 1)),
                          lda.tst.dat)
  colnames(lda.tst.dat)<-stringr::str_replace(colnames(lda.tst.dat),
                                              "X", "noise")
  colnames(lda.tr.dat) <- stringr::str_replace(colnames(lda.tr.dat),
                                               "X", "noise")
  
  ldas<-list()
  coefs<-list()
  tr.preds<-list()
  tst.preds<-list()
  tr.c.mat<-list()
  tst.c.mat<-list()

  for (i in 1:14) {
    tr.lda <- MASS::lda(class~., data=lda.tr.dat[,1:(i+2)])

    ##predict train and get cm
    tr.p <- predict(tr.lda, lda.tr.dat)
    tr.preds[[i]] <- data.frame(class=lda.tr.dat$class, tr.p$x)
    tr.c.mat[[i]] <- table(lda.tr.dat$class, tr.p$class)

    ##predict test and get cm
    tst.p <- predict(tr.lda, lda.tst.dat)
    tst.preds[[i]] <- data.frame(class=lda.tst.dat$class, tst.p$x)
    tst.c.mat[[i]] <- table(lda.tst.dat$class, tst.p$class)

   
    ## Get scalings
    ts.coef <- data.frame(g=rownames(tr.lda$scaling),
                          tr.lda$scaling)
    ts.coef$ymin <- ifelse(ts.coef$LD1<0, ts.coef$LD1, 0)
    ts.coef$ymax <- ifelse(ts.coef$LD1>0, ts.coef$LD1, 0)
    ts.coef <- dplyr::arrange(ts.coef, abs(ts.coef$LD1))
    ts.coef$g <- factor(ts.coef$g, levels=ts.coef$g)
    coefs[[i]] <- ts.coef
  }
  list(ldas=ldas, coefs=coefs, tr.preds=tr.preds, tst.preds=tst.preds,
       tr.c.mat=tr.c.mat, tst.c.mat=tst.c.mat)
}

#' Plot LDA predictions
#'
#' @param tr.preds Training predictions data.frame
#' @param tst.preds Test predictions data.frame
#' @param xmin ignored. Future work.
#' @param xmax ignored. Future work.
#' @return ggplot object
#' @import ggplot2
plotLDAPreds <- function(tr.preds, tst.preds, xmin, xmax) {
  set.seed(17)
  ggplot(tr.preds, aes(LD1, class, ymin=0, ymax=1, xmin=-13, xmax=9.5)) +
    geom_point(shape=1, size=4) +
    geom_point(data=tst.preds, col="red", shape=2, size=4, 
               position=position_jitter(height=.2)) +
    geom_vline(xintercept = 0, colour="grey",
               linetype = "longdash") +
    ## to get legend
    geom_point(data=data.frame(x=c(-14,-14), y=c(0,0),
                               type=factor(c("train", "test"),
                                           levels=c("train", "test"))),
               aes(x, y, color=type, shape=type)) +
    scale_colour_manual(values = c("black", "red")) +
    scale_shape_manual(values=c(1, 2)) +
    theme_bw(base_size=20)
}

#' Plot LDA coefficients
#'
#' @param scalings Data.frame of LDA coefficients
#' @return ggplot object
#' @import ggplot2
plotLDAScalings <- function(scalings) {
  ggplot(scalings) + geom_linerange(aes(x=g, ymin=ymin, ymax=ymax, xmin=0,
                                        xmax=16)) +
    geom_hline(yintercept=0, color="grey80") +
    coord_flip() +
    theme_bw(base_size=20)
}

instructions <- "There are 20 train (black circles) and 20 test (red triangles) observations. The response is a categorical variable with two classes, 'c1' and 'c2'. There is one feature that is related to the response, and you choose the number of additional 'noise' features that are unrelated to the response. LDA is performed on the set features to classify the observations."

#' Describe a shiny UI
#'
#' @return A shiny UI description
#' @import shiny
makeUI <- function() {
  fluidPage(
    titlePanel('LDA: Overfit Me'),
    sidebarLayout(
      sidebarPanel(
        p(instructions), p(),
        sliderInput("noise", "Number of Noise Features:",
                    min = 1,
                    max = 14,
                    value = 1,
                    round = T),
        h4("Training Set Confusion Matrix"),
        tableOutput('cm.tr'),
        h4("Test Set Confusion Matrix"),
        tableOutput('cm.tst'),
        p()
      ),
      mainPanel(
        h3("Train and Test Predictions"),
        plotOutput('preds'),
        h3("LDA Coefficients"),
        plotOutput('coefs')
      )
    )
  )
}

#' Shiny server function
#'
#' @param input Something passed in by shiny
#' @param output Something used by shiny
#' @return None This runs until cows come home
server <- function(input, output) {

  ldaInfo <- doLDA()
  p.min<-min(sapply(c(ldaInfo$tr.pred, ldaInfo$tst.pred),
                      function(x) min(x$LD1)))
  p.max<-max(sapply(c(ldaInfo$tr.pred, ldaInfo$tst.pred),
                      function(x) max(x$LD1)))

  output$preds <- renderPlot({
    wh <- input$noise
    plotLDAPreds(ldaInfo$tr.pred[[wh]], ldaInfo$tst.pred[[wh]],
                 xmin=p.min, xmax=p.max)
  })

  output$cm.tr <- renderTable({
    wh <- input$noise
    ldaInfo$tr.c.mat[[wh]]
  })

  output$cm.tst <- renderTable({
    wh <- input$noise
    ldaInfo$tst.c.mat[[wh]]
  })

  output$coefs <- renderPlot({
    wh <- input$noise
    plotLDAScalings(ldaInfo$coefs[[wh]])
  })

}

#' Run an example of overfitting using LDA
#'
#' Based an example from Di Cook, Module 2 of SSBID 2015.
#'
#' @return None This function starts a shiny example.
#' @export
overfitLDA <- function () {
  ui <- makeUI()
  runApp(shinyApp(ui, server))
}
