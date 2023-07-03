#' Safety riskmatrix function
#'
#' This function can be used to plot an interactive risk matrix for risk assessment.
#' riskmatrix() plots a 5x5 matrix with Severity of Outcome as the x-axis
#' and Probability of Occurrence as the y-axis. The matrix is separated into a
#' grid with each block representing a certain risk category. Blocks are
#' color-coded according to risk level. Red represents the highest risk and
#' green represents the lowest risk. When hovered over, each block and hazard
#' will display a text box explaining the risk parameters.
#'
#' @param risk A dataframe describing the failure conditions. Column names should
#' be: c("ID", "Title", "Risk", "Type","Severity.Name","Severity", "Probability")
#'
#' @return An interactive risk matrix called "risk_plot"
#'  \item{risk_plot}{A risk matrix plot visualizing the risk of the hazard dataframe}
#'
#' @usage
#' riskmatrix(risk)
#'
#' @examples
#' risk <- risk_demo_data
#' riskmatrix(risk)
#'
#' @export
riskmatrix <- function(risk) {
  # define global variables
  ID <- NA
  Probability <- NA
  Severity <- NA
  Type <- NA

  # Create a heatmap background for the risk matrix
  # Set a score in order to calculate the risk level possibilities
  Likelihood <- rep(c(1:5),5)
  Consequence <- rep(c(1:5),each=5)
  Likelihood_score <- rep(c(1,2,4,6,12),5)
  Consequence_score <- rep(c(1,2,4,6,12),each=5)
  df <- data.frame(Likelihood,Consequence)
  risk_score <- Consequence_score * Likelihood_score
  Risk <- dplyr::case_when(risk_score >= 0 & risk_score < 6 ~ 1,
                           risk_score >= 6 & risk_score < 12 ~ 2,
                           risk_score >= 12 & risk_score < 32  ~ 3,
                           risk_score >= 32 ~ 4)
  df2 <- dplyr::mutate(df, risk_score, Risk)

  # Plot the provided risk data
  risk_p<- suppressWarnings(ggplot2::ggplot(df2,ggplot2::aes(x =Consequence, y =Likelihood, fill=Risk))+
    ggplot2::geom_tile()+
    ggplot2::scale_fill_gradientn(colours = c("red", "orange","#EEEE00","#008000"),guide="none")+
    ggplot2::scale_x_continuous(name= "Severity of Outcome",breaks = 0:5, expand = c(0, 0), position = "top")+
    ggplot2::scale_y_continuous(trans = "reverse", name = "Probability of Occurrence",breaks = 0:5, expand = c(0, 0))+
    ggplot2::theme_bw()+
    ggplot2::geom_hline(yintercept = seq(1.5,5.5), color = "white")+
    ggplot2::geom_vline(xintercept = seq(1.5,5.5), color = "white")+
    ggplot2::ggtitle("Interactive Risk Matrix")+
    ggplot2::theme(legend.position="bottom")+
    ggplot2::guides(color=ggplot2::guide_legend(title="Hazard Type"))+
    ggplot2::geom_jitter(data = risk,
                         inherit.aes = FALSE, width= 0.3 ,height = 0.3,
                         ggplot2::aes(label = ID,
                                      y = Probability,
                                      x = Severity,
                                      col = Type))+
    ggplot2::scale_color_manual(values = c("#9400D3","#009fdf","#aaaaaa")
    ))

    plot <- plotly::ggplotly(risk_p)

    return(risk_plot = plot)

}
