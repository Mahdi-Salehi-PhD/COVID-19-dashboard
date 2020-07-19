#Mahdi Salehi et al. (2020)
#salehi2sms@gmail.com
#Note: Before running the app, be sure that you are connected to the internet.

options(rsconnect.max.bundle.size = 8.192e+9)
library(shiny)
library(DT)
library(shinydashboardPlus)
source("Modules/CountryName_wrangling.R")
source("Modules/DataDownload.R")
source("Modules/Continent.R")
source("Modules/MainBody_Page.R")
source("Modules/compute_proportion.R")
source("Modules/compute_cum_ranks.R")
source("Modules/filter.R")
source("Modules/barCharts.R")
source("Modules/MainBody.R")
source("Modules/Country_property.R")
source("Modules/Logistic_Growth_Model_with_ggplot2.R")
source("Modules/timeSeriesPlots.R")
source("Modules/timeSeriesPlotPage.R")
source("Modules/leafMap_data_computation.R")
source("Modules/leafletMap.R")
source("Modules/leafletMap_Page.R")
source("Modules/weight_matrices.R")
source("Modules/Moran.R")
source("Modules/Moran_Page.R")
source("Modules/Moran_Main_Page.R")
source("Modules/Logistic_Growth_Model_Page.R")

ui <- dashboardPage(
  title = "COVID-19 Dashboard",
  dashboardHeaderPlus(titleWidth = 220, title = span(img(src = "Corona2.png", width = 50)), disable = FALSE),
  {
    ## Sidebar  #----
    dashboardSidebar(
      width = 220,
      sidebarMenu(
        id = "tabs",
        menuItem("Home Page", tabName = "homepage"),
        menuItem("Our Team", tabName = "OurTeam"),
        menuItem("Data Set", tabName = "table"),
        menuItem(
          "Demographic",
          menuItem("Overview", tabName = "mainPage_Over"),
          menuItem(
            "Bar Charts",
            menuSubItem("Country", tabName = "mainPage_country"),
            menuSubItem("Continent", tabName = "mainPage_continent")
          )
        ),
        menuItem(
          "Time Series Visualization",
          menuItem("Overview", tabName = "tim_over"),
          menuItem(
            "Time Series Plots",
            menuSubItem("Country", tabName = "tim_Country"),
            menuSubItem("Continent", tabName = "tim_Continent")
          )
        ),
        menuItem(
          "Dynamic growth models",
          menuSubItem("Overview", tabName = "DGM_over"),
          menuSubItem("Country", tabName = "reg_Country"),
          menuSubItem("Continent", tabName = "reg_Continent")
        ),
        menuItem(
          "Spatial Analysis",
          menuItem("Overview", tabName = "map_over"),
          menuSubItem("Map", tabName = "map_ord_country"),
          menuSubItem("Moran's Index", tabName = "moran")
        ),
        br(),
        br(),
        hr(),
        tags$span(style = "font-size:14px; align:right; color:yellow;align:top", textOutput("txtOnline"))
      )
    )
  },
  dashboardBody({
    # Body #---------------------------------------------------
    tabItems(
      ## Home Page ####
      tabItem(
        "homepage",
        tags$img(src = "background2.jpg", style = "position: absolute; opacity: 0.3"),
        column(
          width = 12,

          HTML(
            "<center><h1 style = 'color:red;font-weight: bold;font-size: 40px'>COVID-19 Dashboard</h1></center>"
          ), hr(),

          HTML("
                 <p style = 'font-size:15px;font-weight:bold'>
                 Welcome to our COVID-19 dashboard. We hope you will find it informative and useful!
                 <br>
                 The goal of this web-based app is to give you the ability to play around with some of the data related to the COVID-19 pandemic currently sweeping the globe.
                 </p>
                  <p>
                   <span style= 'font-weight:bold;font-size:18px'>
                            <div style= 'font-size: 16px;color:green;font-weight:bold'>  What exactly does this app do? </div>
                            </span>
                   </p>
                 <p style = 'font-size:15px;font-weight:bold'>
This app provides a dashboard based on COVID-19 data as collected by the WHO and the CDC in the US.
                          It has similar features as other such dashboards; to visualise the massive amounts of data that is 
                          mostly being recorded in real time (at least daily). The power of this app is two-fold: producing downloadable plots
                          of COVID-19 counts (infected, recovered, deceased) which the user can sort by country/continent; but also in an 
                        interactive setting, where users can shiftdays since first infection etc. on a scale and see how the data changed over time.<br><hr>
                    <a style = 'font-size:16px;color:red;font-weight:bold'> New features added:</a><br>
                    <a style = 'font-size:16px;color:purple;font-weight:bold'>
                    - Gompertz growth model<br>
                    - Logistic growth model<br>
                    - Global Moran's Index<br>
                    - Downloading the associated data of a country
                    </a>
                      <p>
                 "), br(),
          HTML("
            <p style = 'font-size:15px;font-weight:bold'>
                 To empower yourself with more information regarding
                 COVID-19, and the important role that the WHO is playing
                 in this pandemic, consider subscribing and completing
                 this free online course from the London School of Hygiene
                 and Tropical Medicine: <a href = https://www.futurelearn.com/courses/covid19-novel-coronavirus>https://www.futurelearn.com/courses/covid19-novel-coronavirus
                 </a></p>"),
          HTML("
            <p style = 'font-size:15px;font-weight:bold'> 
    <div style= 'font-size: 14px;color:green;font-weight:bold'> Please only view this page in conjunction with 
  <a href = https://sacoronavirus.co.za/>https://sacoronavirus.co.za/</a> </div>
      <div style= 'font-size: 14px;color:green;font-weight:bold'> Contact us:  <a href = 'mailto: symstat@up.ac.za' > symstat@up.ac.za </a> </div>
            <p>"),
          ## Tweeter Tag.
          # tags$head(
          #   tags$script(async = NA, src = "https://platform.twitter.com/widgets.js")
          # ),
          HTML("
        <p class='twitter-tweet' data-lang='en'><p lang='en' dir='ltr' style = 'font-size:15px;font-weight:bold'>
        <div style= 'font-size: 14px;color:green;font-weight:bold'> Twitter page: 
            <a>https://twitter.com/nicd_sa</a> </div>
        </p> <br>
      "),
        ),
        fluidRow(
          column(width = 1),
          column(
            width = 1,
            img(src = "Pretoria.jpg", height = "130px", width = "120px")
          ),
          column(width = 8),
          column(
            width = 1,
            img(src = "neyshabur.png", height = "130px", width = "90px")
          )
        )
      ),
      tabItem(
        tabName = "OurTeam",

        HTML(
          "<center><h1 style = 'color:red;font-weight: bold;font-size: 40px'>COVID-19 Dashboard</h1></center>"
        ), hr(),
        HTML("
                      <p style = 'font-size:15px;font-weight:bold'>
            The World Health Organisation\'s focus at all times is to ensure that all
            areas of the globe have the information they need to manage the health
            of their people due to the ongoing COVID-19 pandemic. Here, 
            in this statistical data analytics dashboard, we provide the public
            users with the latest demographic information and aim to
            track this disease trends in terms of number of infections, death,
            and recoveries. This R-shiny app will assist in addressing this Big
            Data issue and stimulate the level of public awareness about COVID-19.
            </p>
               "),
        HTML("<h3 style = 'color:green; font-weight:bold;font-size: 22px'>Our Team: </h3>"), br(),
        HTML("<div style = 'font-size: 14.5px;color:black;font-weight:bold'>  <a style = 'font-size: 16px;color:blue;font-weight:bold'; href = http://salehi.neyshabur.ac.ir/math/salehi/ > Mahdi Salehi </a>: Department of Mathematics and Statistics, Faculty of Basic Sciences - University of Neyshabur, Iran  <a style = 'font-size: 14.5px;color:blue;font-weight:bold'; href = https://publons.com/researcher/3069341/mahdi-salehi/publications/> (principal developer) </a></div>"),
        br(),
        HTML("<div style = 'font-size: 14.5px;color:black;font-weight:bold'>  <a style = 'font-size: 16px;color:blue;font-weight:bold'> Foad Esmaeili</a>: Department of Mathematics and Statistics, Faculty of Basic Sciences - University of Neyshabur, Iran </div>"),
        br(),
        HTML("<div style = 'font-size: 14.5px;color:black;font-weight:bold'>  <a style = 'font-size: 16px;color:blue;font-weight:bold'; href = https://www.up.ac.za/statistics/article/2320366/prof-andritte-bekker> Andriette Bekker</a>: Department of Statistics, Faculty of Natural and Agricultural Sciences  - University of Pretoria, South Africa </div>"),
        br(),
        HTML("<div style = 'font-size: 14.5px;color:black;font-weight:bold'>  <a style = 'font-size: 16px;color:blue;font-weight:bold'; href = https://scholar.google.com/citations?user=iBIh60UAAAAJ&hl=en> Mohammad Arashi </a>:  </a color:black; href = orcid.org/0000-0002-5881-9241 > Department of Statistics, Faculty of Mathematical Sciences - Shahrood University of Technology, Iran </a> </div>"),
        br(),
        HTML("<div style = 'font-size: 14.5px;color:black;font-weight:bold'>  <a style = 'font-size: 16px;color:blue;font-weight:bold'; href = https://www.up.ac.za/statistics/article/2324000/mr-johan-ferreira> Johan Ferreira</a>: Department of Statistics, Faculty of Natural and Agricultural Sciences  - University of Pretoria, South Africa </div>"),
        br(),
        HTML("<div style = 'font-size: 14.5px;color:black;font-weight:bold'>  <a style = 'font-size: 16px;color:blue;font-weight:bold'> Frances Motala </a>: Department of Statistics, Faculty of Natural and Agricultural Sciences - University of Pretoria, South Africa </div> <br> <br>
                               <div style= 'font-size: 15px;color:green;font-weight:bold'> Contact us:  <a  style = 'font-size: 15px;color:blue;font-weight:bold' href = 'mailto: symstat@up.ac.za' > symstat@up.ac.za </a> </div>"),
        br()
      ),
      tabItem(
        tabName = "table",
        HTML("
               <p style='color:green;font-size:15px; font-weight:bold'>
               The data is obtained from the online repository GitHub 
               (<a href = https://pomber.github.io/covid19/timeseries.json>
               https://pomber.github.io/covid19/timeseries.json</a>). It is captured per country per day, which results in a large dataframe. This data source includes information since at least the announcement of the PHEIC/pandemic by the WHO up to the past 24 hours (excluding today). It is automatically updated as data becomes available as 
               reported by various international- and country authorities. Moreover, the population data made available from 
               <a href = https://www.worldometers.info/world-population/population-by-country/>
               www.worldometers.info</a>
               <div style='color:green;font-size:15px; font-weight:bold'> You can use the \"search bar\" to type in a country of interest\'s name to obtain raw data by date for that specific country. Similarly, by typing a specific date of the form 'yyyy-mm-dd' you can achieve the associated data. 
               After selecting a sub-data by this manner, you can press the 'CSV' button to have that portion in your own computer. 
               If you need the whole data set, just press it without writing anything in the search bar (this action needs more time to be completed).  </div>
               </p>
               "),
        withSpinner(DTOutput(outputId = "tblData"))
      ),
      tabItem(
        tabName = "mainPage_Over",
        HTML("
          <h1 style= 'font-size: 28px;color:green;font-weight:bold'> Demographic </h1> <br>
               <p style = 'font-size:15px;font-weight:bold'>
               Here we present bar charts that can indicate how different
               countries compare in terms of reported numbers of positive diagnoses
               (confirmed), deaths, and recovery counts.<br>
               You can choose between \"Absolute counts\" and \"Relative counts\".<br><br>
               <a style= 'font-size: 16px;color:green;font-weight:bold'> What is the difference? </a>
               Absolute counts represent the actual counted number of cases 
               (confirmed, deaths, recoveries) for each region/country.
               Relative counts places the absolute counts in context with
               regards to the population size of that region 
               (per 1 million residents). 
               For example, if a region/country A has a population of 60 million people
               and 3000 infected cases, then it has (on a relative scale) 50 infected cases
               per 1 million people.<br><br>
               You can use the bar above each graph to
               \"shift the date backwards\" and explore how the data changed over time,
               up to the date the WHO declared the COVID-19 outbreak a PHEIC 
               (Public Health Emergency of International Concern).<br><br>
               <a style= 'font-size: 16px;color:green;font-weight:bold'> Tip: </a> Use the play button
               below the bar to observe an interactive plot over time! <br> 
               <a style= 'font-size: 16px;color:green;font-weight:bold'> Note:</a>  Please be mindful
               that the count of confirmed cases <a style= 'font-size: 16px;color:black;font-weight:italic'> includes </a>
               count of recovered cases!
               </p>
               ")
      ),
      ## per Country Pages. ####
      tabItem(
        tabName = "mainPage_country",
        ui_mainBody_Page("mainPage__country")
      ),
      tabItem(
        tabName = "tim_over",
        HTML("
          <h1 style= 'font-size: 28px;color:green;font-weight:bold'> Time Series Analysis </h1> <br>
               <p style = 'font-size:15px;font-weight:bold'> 
               Here we present plots of the different counts over time (commonly called a time series plot).
               You can choose between \"Absolute counts\" and \"Relative counts\".
                 <br><br>
               <a style= 'font-size: 16px;color:green;font-weight:bold'> What is the difference? </a>
               Absolute counts represent the actual counted number of cases (confirmed, deaths, recoveries)
               for each region/country. Relative counts places the absolute counts in context with regards to
               the population size of that region (per 1 million residents). For example,
               if a region/country A has a population of 60 million people and 3000 infected cases,
               then it has (on a relative scale) 50 infected cases per 1 million people. <br><br>
               
               On the cumulative scale, you can choose it to display the data using a 
               <a style= 'font-size: 16px;color:black;font-weight:bold'> logarithmic</a>
               scale as well.<br><br>
               
               This changes the y-axis (vertical axis) to 
               <a style= 'font-size: 16px;color:black;font-weight:bold'> display</a>
               the data differently. On the usual scale (non-logarithmic), numbers are equal distances apart - 10, 20, 30, and so on. On a logarithmic scale, numbers 10, 100, 1000 and so on, are equal distances apart. This type of adjustment to how the data are displayed is often employed when a curve grows exponentially - it prevents the graph from getting too big too soon.<br><br>
               <a style= 'font-size: 16px;color:green;font-weight:bold'> Note:</a>  
               Please be mindful that the cumulative count of confirmed cases
               <a style= 'font-size: 16px;color:black;font-weight:bold'> includes </a>
               recovered cases! <br><br>
               <a style= 'font-size: 16px;color:green;font-weight:bold'> Prediction of the future of the pandemic:</a>  
               In this menu, we have also provided a shortcut for fitting some dynamic growth models
               on the absolute cumulative counts of the confirmed cases in the following address <br>
               <a style= 'font-size: 15px;color:blue;font-weight:bold;text-align:center;'> Absolute counts > Confirmed > Cumulative > Prediction of the future.</a><br><br>
               To see more details on the dynamic growth models employed in this dashboard,
                refer to the 'Dynamic growth models' main menu and its overview.
             "),
      ),
      tabItem(
        tabName = "tim_Country",
        ui_timPage("tim_CountryPage")
      ),
      tabItem(
        tabName = "map_over",
        HTML("
          <h1 style= 'font-size: 28px;color:green;font-weight:bold'> Spatial Analysis </h1> <br>
               <p style = 'font-size:15px;font-weight:bold'> Here we present a spatial map indicating counts of confirmed cases and death cases.
               You can choose between \"Absolute counts\" and \"Relative counts\".
                 <br><br>
               <a style= 'font-size: 16px;color:green;font-weight:bold'> What is the difference? </a>
               Absolute counts represent the actual counted number of cases (confirmed,
               deaths, recoveries) for each region/country. Relative counts places the absolute
               counts in context with regards to the population size of that region
               (per 1 million residents). For example, if a region/country A has a population 
               of 60 million people and 3000 infected cases, then it has (on a relative scale) 
               50 infected cases per 1 million people. <br><br>
               </p>
               <p style= 'font-size: 15px;color:black;font-weight:bold'>
               <br><br>
               <a style= 'font-size: 16px;color:green;font-weight:bold'> Moran's Index:  </a>
               The Moran's index, originally defined by Moran, P. A. (1950), is a measure of spatial association or spatial autocorrelation which can be used to find spatial hotspots or clusters and is available in many software applications.  This index has been defined as the measure of choice for scientists, specifically in environmental sciences, ecology and public health.
               Moran's Index has a local and global representation.  The global Moran's I is a global measure for spatial autocorrelation while the local Moran's I index examines the individual locations, enabling hotspots to be identified based on comparisons to the neighbouring samples. 
               The Moran index I takes value on [-1,1] and I=0 shows no spatial correlation between
               the sub-regions for the underlying feature. According to  Gittleman and Kot (1990), there are two ways to identify the weights; 
               by adjacency approach and geographical distance method.<br><br>
               <a style= 'font-size: 16px;color:green;font-weight:bold'> How to interpret the outputs?:  </a>
               In this module, we compute the global Moran's index and test its significance for countries of a given continent based on daily confirmed cases.   
               Both weight matrices mentioned above are employed for this purpose. 
               The null hypothesis of the mentioned two-sided test indicates that there is no significance spatially correlation between the countries, while the alternative hypothesis indicates a significant autocorrelation. 
               Hence, if the corresponding p-value of a test is less than 0.05 (the horizontal dashed line of the first graph), then we have a significant spatial correlation for the countries of that continent, otherwise, there is no strong evidence for making such a decision.
               More precisely, based on the second graph, if the observed value of Moran's Index is significantly greater than the expected value, then the regions
               are positively autocorrelated, whereas if observed << expected, this will indicate negative autocorrelation. 
               In order to see and download the corresponding outputs in detail, you can see the 'Data table' tab.
               <br><br>
               <a style= 'font-size: 16px;color:green;font-weight:bold'> Note: </a>
               In the method using the geographical distance matrix, there is a parameter called 'distance threshold'. 
              Here we have set it to be the sample median of all distances between every two countries of a given continent.
               There is also a possibility to change it to other percentiles as well.<br><br>
               <a style= 'font-size: 16px;color:green;font-weight:bold'> References:</a><br>
               [1] Gittleman JL, Kot M (1990) Adaptation: statistics and a null model for estimating phylogenetic effects, Systematic Zoology 39:227-241.<br>
               [2] Moran, P. A. (1950). Notes on continuous stochastic phenomena. Biometrika, 37(1/2), 17-23.<br>               
               </p>
               ")
      ),
      tabItem(
        tabName = "map_ord_country",
        ui_leafMap_Page("Ordinary_map")
      ),
      tabItem(
        tabName = "moran",
        ui_moran_page("moran")
        #        ui_leafMap_Page("Ordinary_map")
      ),

      # per Continent ####
      tabItem(
        tabName = "mainPage_continent",
        ui_mainBody_Page("mainPage__continent")
      ),
      tabItem(
        tabName = "tim_Continent",
        ui_timPage("tim_ContinentPage")
      ),
      tabItem(
        tabName = "DGM_over",
        HTML("<p style= 'font-size: 15px;color:black;font-weight:bold'><a style= 'font-size: 16px;color:green;font-weight:bold'> Dynamic growth models:</a>  
               In this menu, we have provided two dynamic models for
               forecasting the future of the pandemic. To this end, 
               the logistic growth model (LGM) as well as the Gomperts growth model (GGM), as two special cases of the 
               generalized logistic curve [1], are fitted on the absolute cumulative counts of the confirmed cases (denoted by N(t)).
               More precisely, the following three paramters non-linear mathematical models have been utilized
       as the LGM and GGM, respectively:</p>"),
        shiny::withMathJax(helpText("$$N(t) = \\frac{\\alpha}{1+\\beta\\exp(-kt)} + \\epsilon,$$")),
        shiny::withMathJax(helpText("$$N(t) = \\alpha\\exp(- \\beta\\exp(-kt)) + \\epsilon.$$")),
        HTML("<p style= 'font-size: 15px;color:black;font-weight:bold'>
The generalized logistic curve is commonly used for dynamic modeling in many branches of science including chemistry,
               physics, material science, forestry, disease progression, sociology, etc. See [1] and [2] for more details and applications. <br><br>
                 <a style= 'font-size: 16px;color:green;font-weight:bold'> Note 1:</a>  
  Please be informed that in the plots of this module, 
  the points stand for the observed values 
  and the solid lines show the fitted dynamic models on them.<br>
  <a style= 'font-size: 16px;color:green;font-weight:bold'> Note 2:</a>  
  It is to be noted that the above-mentioned models are not fitted just on
  a few countries data due to the presence of some outliers. In such a situation you may encountor to the following message:<br>
  <a style= 'font-size: 14px;color:red;font-weight:normal;text-align:center'> An error has occurred. Check your logs or contact the app author for clarification.</a>
               <br><br>
               <a style= 'font-size: 16px;color:green;font-weight:bold'> References:</a><br>
               [1] Lei, Y.C.; Zhang, S.Y. Features and partial derivatives of Bertalanffy-Richards growth model in forestry, Nonlinear Anal. Model. Cont. 2004 Volume 9(1), pp. 65-73.<br>
               [2] Richards, F.J. A flexible growth function for empirical use, J. Experimental Botany 1959 Volume 10(2), pp. 290-300.
              </p>"),
      ),

      tabItem(
        tabName = "reg_Country",
        ui_regression("RegCountry")
      ),
      tabItem(
        tabName = "reg_Continent",
        ui_regression("RegContinent")
      )
    )
  })
)
