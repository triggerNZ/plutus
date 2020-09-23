{-# LANGUAGE GADTs #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import Data.Int (Int32)
import qualified Language.R as R
import Language.R (R)
import Language.R.QQ
import Language.Marlowe.ACTUS.Analysis(sampleCashflows)
import Data.Time.Calendar(showGregorian)
import Language.Marlowe.ACTUS.Definitions.BusinessEvents(RiskFactors(..))
import Language.Marlowe.ACTUS.Definitions.Schedule(CashFlow(..))
import Data.Aeson(decode)
import Data.String (IsString (fromString))

get_dates :: String -> R s [String]
get_dates terms = return $ case (decode $ fromString terms) of
    Just terms' -> 
      let 
        cfs = sampleCashflows (\_ -> RiskFactors 1.0 1.0 1.0 0.0) terms'
        date = showGregorian <$> cashCalculationDay <$> cfs
        event = show <$> cashEvent <$> cfs
      in (\(d, e) -> d ++ " " ++ e) <$> (zip date event)
    Nothing -> []

get_cfs :: String -> Double -> R s [Double]
get_cfs terms rrmo = return $ case (decode $ fromString terms) of
    Just terms' -> amount <$> sampleCashflows (\_ -> RiskFactors 1.0 rrmo 1.0 0.0) terms'
    Nothing -> []

r_shiny :: R s Int32
r_shiny = do
    R.dynSEXP <$> [r| 
library(shiny)
library(shinyjs)
library(plotly)
library(purrr)

jscode <- "
shinyjs.init = function() {
  window.addEventListener(\"message\", receiveMessage, false);
  
  function receiveMessage(event) {
    Shiny.onInputChange(\"contract\", event.data); 
  }
}"

ui <- fluidPage(
  useShinyjs(),
  extendShinyjs(text = jscode, functions = c()),  
  headerPanel(''),
  mainPanel(plotlyOutput('plot')),
  sliderInput("ipnr", 
              label = "Interest rate resets:",
              min = 0, max = 1, value = c(0))

)

server <- function(input, output, session) { 
    observeEvent(input$contract, {
      session$userData$contract = input$contract
    })
    
    toListen <- reactive({
      list(input$contract,input$ipnr)
    })
    
    observeEvent(toListen(), {
      if (!is.null(session$userData$contract) && !is.null(input$ipnr)) {
        tryCatch ({
          x <- get_dates_hs(toString(input$contract))
          y <- get_cfs_hs(toString(input$contract), as.double(input$ipnr))
          text <- y %>% 
            map(function(x) if (x > 0) paste("+", toString(x), sep = "") else toString(x))

          message(input$ipnr)
          message(x)
          message(y)

          data <- data.frame(x=factor(x,levels=x),text,y)
          
          fig <- plot_ly(
            data, name = "20", type = "waterfall",
            x = ~x, textposition = "outside", y= ~y, text =~text,
            connector = list(line = list(color= "rgb(63, 63, 63)"))) 
          fig <- fig %>% layout(title = "Cashflows",
                                xaxis = list(title = ""),
                                yaxis = list(title = ""),
                                autosize = TRUE)
          
          fig
        })
        output$plot <- renderPlotly(fig)
      }
    })
}

# Create Shiny app ----
message("Starting Marlowe Shiny app: ")
runApp(shinyApp(ui = ui, server = server), port = 8081)
1
    |]

main :: IO ()
main = R.withEmbeddedR R.defaultConfig $ do
    code <- R.runRegion $ r_shiny
    print code