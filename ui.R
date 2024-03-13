
## Input boxes

#Scenario option selection
scenariooptions<-list(
    list(text="Impact of hypothetical vaccination campaign"),
    list(text="Impact of current influenza vaccination campaign")
)

contextInputs<-tagList(
    ComboBox.shinyInput("scenario", value=list(text="Impact of hypothetical vaccination campaign"),
                        options=scenariooptions, label="Select your scenario"),
    TextField.shinyInput("year", label="Season", value="2021"),
    TextField.shinyInput("country", label="Country/Area", value="CountryXYZ"),
    TextField.shinyInput("targetgroup", label="Target group", value="Adults 18-45"),
    TextField.shinyInput("populationsize", label="Target population size", placeholder="required", value="348073")
)

vaccineParams<-tagList(
    TextField.shinyInput("vaccineeffectiveness", label="VE (proportion from 0-1):", placeholder="required", value="0.57"),
    TextField.shinyInput("VELCI", label="VE lower confidence interval:", value="0.4"),
    TextField.shinyInput("VEUCI", label="VE upper confidence interval:", value="0.7")
)


hospParams<-tagList(
    TextField.shinyInput("hnh", label="Ratio non-hospitalized:hospitalized nH/H:", placeholder="required", value="43.58333333"),
    TextField.shinyInput("lowerhnh", label="Lower ratio nH/H:", placeholder="required", value="30"),
    TextField.shinyInput("upperhnh", label="Upper ratio nH/H:", placeholder="required", value="55"),
    TextField.shinyInput("mai", label="Proportion of infections that were medically attended:", placeholder="required", value="0.644859813")
)


vaccineCov<-tagList(
    TextField.shinyInput("monthstartvac", label="Campaign start month:", type="number", placeholder="required", value=3),
    TextField.shinyInput("vaccinecoveragemonthly", placeholder="month 1", type="number", min="0", max="1", value="0"),
    TextField.shinyInput("month2", placeholder="month 2", type="number", min="0", max="1", value="0"),
    TextField.shinyInput("month3", placeholder="month 3", type="number", min="0", max="1", value="0.135681308231319"),
    TextField.shinyInput("month4", placeholder="month 4", type="number", min="0", max="1", value="0.223435888448687"),
    TextField.shinyInput("month5", placeholder="month 5", type="number", min="0", max="1", value="0.192933666213697"),
    TextField.shinyInput("month6", placeholder="month 6", type="number", min="0", max="1", value="0.0745217238912527"),
    TextField.shinyInput("month7", placeholder="month 7", type="number", min="0", max="1", value="0.0259658175152913"),
    TextField.shinyInput("month8", placeholder="month 8", type="number", min="0", max="1", value="0.0123824600012066"),
    TextField.shinyInput("month9", placeholder="month 9", type="number", min="0", max="1", value="0.00574592111424902"),
    TextField.shinyInput("month10", placeholder="month 10", type="number", min="0", max="1", value="0.00521729637173811"),
    TextField.shinyInput("month11", placeholder="month 11", type="number", min="0", max="1", value="0.00268047219979717"),
    TextField.shinyInput("month12", placeholder="month 12", type="number", min="0", max="1", value="0.00132730777739152")
)

hospData<-tagList(
    div(br(),
        br()),
    TextField.shinyInput("hospitalizationsmonthly", placeholder="month 1", type="number", value="19.76"),
    TextField.shinyInput("hospmonth2", placeholder="month 2", type="number", value="0"),
    TextField.shinyInput("hospmonth3", placeholder="month 3", type="number", value="18.15"),
    TextField.shinyInput("hospmonth4", placeholder="month 4", type="number", value="16.24"),
    TextField.shinyInput("hospmonth5", placeholder="month 5", type="number", value="27.94"),
    TextField.shinyInput("hospmonth6", placeholder="month 6", type="number", value="122.4"),
    TextField.shinyInput("hospmonth7", placeholder="month 7", type="number", value="222"),
    TextField.shinyInput("hospmonth8", placeholder="month 8", type="number", value="253.8"),
    TextField.shinyInput("hospmonth9", placeholder="month 9", type="number", value="87.15"),
    TextField.shinyInput("hospmonth10", placeholder="month 10", type="number", value="114.39"),
    TextField.shinyInput("hospmonth11", placeholder="month 11", type="number", value="18.52"),
    TextField.shinyInput("hospmonth12", placeholder="month 12", type="number", value="13.78")
)



navigation <- Nav(
    groups = list(
        list(links = list(
            list(name = 'How it works', url = '#!/how_it_works', key = 'home', icon = 'DeveloperTools'),
            list(name = 'Estimate burden averted', url = '#!/', key = 'estimation', icon = 'Calculator'),
            list(name = 'Results table', url = '#!/results_table', key = 'results_table', icon = 'Table'),
            list(name = 'Results report', url = '#!/results_report', key = 'results_report', icon = 'AnalyticsReport'),
            list(name = 'Deployment strategy planning', url = '#!/strategy_planning', key = 'strategy_planning', icon = 'Equalizer')
        ))
    ),
    initialSelectedKey = 'home',
    styles = list(
        root = list(
            height = '100%',
            boxSizing = 'border-box',
            overflowY = 'auto'
        )
    )
)


layout <- function(mainUI){
    div(class = "grid-container",
        div(class = "sidenav", navigation),
        div(class = "main", mainUI),
    )
}



estimate_burden_averted <- makePage(
    
    "",
    "",
    Stack(
        tokens = list(childGap = 10), 
        horizontal=FALSE,
        Stack(
            tokens = list(childGap = 10), 
            horizontal=TRUE,
            makeCard("Geographical, season and population context", 
                     contextInputs, size=4, style="max-height:auto; margin:auto; margin-bottom:12px; padding-top:2px; padding-bottom:12px")
        ),
        Stack(
            tokens = list(childGap = 10), 
            horizontal=TRUE,
            horizontalAlign="center",
            Stack(tokens = list(childGap = 2), 
                  horizontal=FALSE,
                  makeCard("Vaccine effectiveness", 
                           vaccineParams, size=12, style="max-height:auto; margin:auto; margin-bottom:2px; margin-top:10px; padding-top:2px; padding-bottom:12px"),
                  makeCard("Multipliers", 
                           hospParams, size=12, style="max-height:auto; margin:auto; margin-top:3px; padding-top:10px; padding-top:2px; padding-bottom:12px")
            ),
            Stack(tokens = list(childGap = 10), 
                  horizontal=TRUE,
                  spacing = 2,
                  makeCard("Vaccine coverage", 
                           vaccineCov, size=4, style="max-height:auto; margin-left:200px; margin-top:10px; ; padding-top:2px; padding-bottom:12px"),
                  makeCard("Number of hospitalizations", 
                           hospData, size=4, style="max-height:auto; margin-left:10px; margin-top:10px; padding-top:2px; padding-bottom:12px")
            )
        )
        #Stack(
        #    horizontalAlign="center",
        #    horizontal=TRUE,
        #    DefaultButton.shinyInput("btn btn-default", text="Reset"),
        #    PrimaryButton.shinyInput("btn btn-primary", text="Submit")
        )
#    )
)


results_table <- makePage(
    "",
    "",
    Stack(
        tokens = list(childGap = 10), 
        horizontal=FALSE,
        makeCard("Results table",
                 gt_output(outputId = "table")
                 
        )
        
    ))


resultsText<-tagList(
    br(),
    br(),
    textOutput(outputId="text1.1"),
    br(),
    textOutput(outputId="text2"),
    br(),
    textOutput(outputId="text3"),
    br(),
    textOutput(outputId="text4"),
    br(),
    textOutput(outputId="text5"),
    br(),
    br(),
    br(),
    plotOutput(outputId="resultsgraph"),
    br(),
    br(),
    br(),
    
)






results_report <- makePage(
    "",
    "",
    Stack(tokens = list(childGap = 10), 
          horizontal=FALSE,
          Stack(
              tokens = list(childGap = 10), 
              horizontal=FALSE,
              makeCard2("Results interpretation", 
                        resultsText, size=10, style="max-height:auto; margin-left:10px; margin-top:10px; padding-top:2px; padding-bottom:12px"
              )
          ),
          #Stack(
          #    horizontalAlign="center",
          #    horizontal=TRUE,
          #    PrimaryButton.shinyInput("download", text="Download report")
          )
    #)
)


vaccinecovstrat<-tagList(
    br(),
    br(),
    Stack(tokens = list(childGap = 10), 
          horizontal=TRUE,
          Stack(tokens = list(childGap = 10), 
                horizontal=FALSE,
                div(style='width:500px;',
                    Slider.shinyInput(inputId = "coverage1", class="slidercov", style='width:800px',
                                      value = 0, min = 0, max = 1, step = 0.01,
                                      label = "Coverage 1",
                                      snapToStep = TRUE)),
                Slider.shinyInput(inputId = "coverage2", class="slidercov",
                                  value = 0, min = 0, max = 1, step = 0.01,
                                  label = "Coverage 2",
                                  snapToStep = TRUE),
                Slider.shinyInput(inputId = "coverage3", class="slidercov",
                                  value = 0, min = 0, max = 1, step = 0.01,
                                  label = "Coverage 3",
                                  snapToStep = TRUE),
                Slider.shinyInput(inputId = "coverage4", class="slidercov",
                                  value = 0, min = 0, max = 1, step = 0.01,
                                  label = "Coverage 4",
                                  snapToStep = TRUE)
          ),
          Stack(tokens = list(childGap = 10), 
                horizontal=FALSE,
                gt_output(outputId = "table_coverage")
          )
    )
)



vaccineCovtimingstrat<-tagList(
    br(),
    div("Input a new distribution by specifying for every month a proportion of the total coverage deployed 
    in that month. The sum of the proportions must be 1. All 4 'campaign smart months'  must be entered to 
    generate a results table. Enter 0 for months where vaccine is not deployed.", style="white-space: pre-wrap;"),
    br(),
    Stack(tokens = list(childGap = 10), 
          horizontal=TRUE,
          Stack(tokens = list(childGap = 10), 
                horizontal=FALSE,
                TextField.shinyInput("monthstartvac1", type="number", placeholder="campaign start month", defaultValue=0, class="txtfieldtime", value=1),
                TextField.shinyInput("vaccinecoveragemonthly1", placeholder="month 1", type="number", min="0", max="1", defaultValue=0, class="txtfieldtime", value=0.5),
                TextField.shinyInput("month21", placeholder="month 2", type="number", min="0", max="1", defaultValue=0, class="txtfieldtime", value=0.5),
                TextField.shinyInput("month31", placeholder="month 3", type="number", min="0", max="1", defaultValue=0, class="txtfieldtime", value=0),
                TextField.shinyInput("month41", placeholder="month 4", type="number", min="0", max="1", defaultValue=0, class="txtfieldtime", value=0),
                TextField.shinyInput("month51", placeholder="month 5", type="number", min="0", max="1", defaultValue=0, class="txtfieldtime", value=0),
                TextField.shinyInput("month61", placeholder="month 6", type="number", min="0", max="1", defaultValue=0, class="txtfieldtime", value=0),
                TextField.shinyInput("month71", placeholder="month 7", type="number", min="0", max="1", defaultValue=0, class="txtfieldtime", value=0),
                TextField.shinyInput("month81", placeholder="month 8", type="number", min="0", max="1", defaultValue=0, class="txtfieldtime", value=0),
                TextField.shinyInput("month91", placeholder="month 9", type="number", min="0", max="1", defaultValue=0, class="txtfieldtime", value=0),
                TextField.shinyInput("month101", placeholder="month 10", type="number", min="0", max="1", defaultValue=0, class="txtfieldtime", value=0),
                TextField.shinyInput("month111", placeholder="month 11", type="number", min="0", max="1", defaultValue=0, class="txtfieldtime", value=0),
                TextField.shinyInput("month121", placeholder="month 12", type="number", min="0", max="1", defaultValue=0, class="txtfieldtime", value=0)
          ),
          Stack(tokens = list(childGap = 10), 
                horizontal=FALSE,
                TextField.shinyInput("monthstartvac2", type="number", placeholder="campaign start month", defaultValue=0, class="txtfieldtime", value=2),
                TextField.shinyInput("vaccinecoveragemonthly2", placeholder="month 1", type="number", min="0", max="1", defaultValue=0, class="txtfieldtime", value=0),
                TextField.shinyInput("month22", placeholder="month 2", type="number", min="0", max="1", defaultValue=0, class="txtfieldtime", value=0.5),
                TextField.shinyInput("month32", placeholder="month 3", type="number", min="0", max="1", defaultValue=0, class="txtfieldtime", value=0.5),
                TextField.shinyInput("month42", placeholder="month 4", type="number", min="0", max="1", defaultValue=0, class="txtfieldtime", value=0),
                TextField.shinyInput("month52", placeholder="month 5", type="number", min="0", max="1", defaultValue=0, class="txtfieldtime", value=0),
                TextField.shinyInput("month62", placeholder="month 6", type="number", min="0", max="1", defaultValue=0, class="txtfieldtime", value=0),
                TextField.shinyInput("month72", placeholder="month 7", type="number", min="0", max="1", defaultValue=0, class="txtfieldtime", value=0),
                TextField.shinyInput("month82", placeholder="month 8", type="number", min="0", max="1", defaultValue=0, class="txtfieldtime", value=0),
                TextField.shinyInput("month92", placeholder="month 9", type="number", min="0", max="1", defaultValue=0, class="txtfieldtime", value=0),
                TextField.shinyInput("month102", placeholder="month 10", type="number", min="0", max="1", defaultValue=0, class="txtfieldtime", value=0),
                TextField.shinyInput("month112", placeholder="month 11", type="number", min="0", max="1", defaultValue=0, class="txtfieldtime", value=0),
                TextField.shinyInput("month122", placeholder="month 12", type="number", min="0", max="1", defaultValue=0, class="txtfieldtime", value=0)
          ),
          Stack(tokens = list(childGap = 10), 
                horizontal=FALSE,
                TextField.shinyInput("monthstartvac3", type="number", placeholder="campaign start month", defaultValue=0, class="txtfieldtime", value=3),
                TextField.shinyInput("vaccinecoveragemonthly3", placeholder="month 1", type="number", min="0", max="1", defaultValue=0, class="txtfieldtime", value=0),
                TextField.shinyInput("month23", placeholder="month 2", type="number", min="0", max="1", defaultValue=0, class="txtfieldtime", value=0),
                TextField.shinyInput("month33", placeholder="month 3", type="number", min="0", max="1", defaultValue=0, class="txtfieldtime", value=0.5),
                TextField.shinyInput("month43", placeholder="month 4", type="number", min="0", max="1", defaultValue=0, class="txtfieldtime", value=0.5),
                TextField.shinyInput("month53", placeholder="month 5", type="number", min="0", max="1", defaultValue=0, class="txtfieldtime", value=0),
                TextField.shinyInput("month63", placeholder="month 6", type="number", min="0", max="1", defaultValue=0, class="txtfieldtime", value=0),
                TextField.shinyInput("month73", placeholder="month 7", type="number", min="0", max="1", defaultValue=0, class="txtfieldtime", value=0),
                TextField.shinyInput("month83", placeholder="month 8", type="number", min="0", max="1", defaultValue=0, class="txtfieldtime", value=0),
                TextField.shinyInput("month93", placeholder="month 9", type="number", min="0", max="1", defaultValue=0, class="txtfieldtime", value=0),
                TextField.shinyInput("month103", placeholder="month 10", type="number", min="0", max="1", defaultValue=0, class="txtfieldtime", value=0),
                TextField.shinyInput("month113", placeholder="month 11", type="number", min="0", max="1", defaultValue=0, class="txtfieldtime", value=0),
                TextField.shinyInput("month123", placeholder="month 12", type="number", min="0", max="1", defaultValue=0, class="txtfieldtime", value=0)
          ),
          Stack(tokens = list(childGap = 10), 
                horizontal=FALSE,
                TextField.shinyInput("monthstartvac4", type="number", placeholder="campaign start month", defaultValue=0, class="txtfieldtime", value=4),
                TextField.shinyInput("vaccinecoveragemonthly4", placeholder="month 1", type="number", min="0", max="1", defaultValue=0, class="txtfieldtime", value=0),
                TextField.shinyInput("month24", placeholder="month 2", type="number", min="0", max="1", defaultValue=0, class="txtfieldtime", value=0),
                TextField.shinyInput("month34", placeholder="month 3", type="number", min="0", max="1", defaultValue=0, class="txtfieldtime", value=0),
                TextField.shinyInput("month44", placeholder="month 4", type="number", min="0", max="1", defaultValue=0, class="txtfieldtime", value=0.5),
                TextField.shinyInput("month54", placeholder="month 5", type="number", min="0", max="1", defaultValue=0, class="txtfieldtime", value=0.5),
                TextField.shinyInput("month64", placeholder="month 6", type="number", min="0", max="1", defaultValue=0, class="txtfieldtime", value=0),
                TextField.shinyInput("month74", placeholder="month 7", type="number", min="0", max="1", defaultValue=0, class="txtfieldtime", value=0),
                TextField.shinyInput("month84", placeholder="month 8", type="number", min="0", max="1", defaultValue=0, class="txtfieldtime", value=0),
                TextField.shinyInput("month94", placeholder="month 9", type="number", min="0", max="1", defaultValue=0, class="txtfieldtime", value=0),
                TextField.shinyInput("month104", placeholder="month 10", type="number", min="0", max="1", defaultValue=0, class="txtfieldtime", value=0),
                TextField.shinyInput("month114", placeholder="month 11", type="number", min="0", max="1", defaultValue=0, class="txtfieldtime", value=0),
                TextField.shinyInput("month124", placeholder="month 12", type="number", min="0", max="1", defaultValue=0, class="txtfieldtime", value=0)
          ),
          
          Stack(
              div(style="margin-left:50px",
                  tokens = list(childGap = 10), 
                  horizontal=FALSE,
                  gt_output(outputId = "table_timing"))
          )
    )
)




stratplotoptions<-list(
    list(text="Prevented fraction"),
    list(text="Hospitalizations"),
    list(text="Non-hospitalized infections"),
    list(text="Medically attended infections"),
    list(text="Total infections"))

stratplotcombo<-tagList(
    Stack(tokens = list(childGap = 10), 
          horizontal=TRUE,
          Stack(tokens = list(childGap = 10), 
                horizontal=TRUE,
                ComboBox.shinyInput("stratplotchoice", value=list(text="Prevented fraction"),
                options=stratplotoptions, label="Select results to graph")
          ),
          Stack(
              div(style="margin-left:50px",
                  tokens = list(childGap = 10), 
                  horizontal=TRUE,
                  plotOutput(outputId = "covtimepgraph", width="800px", height="500px")
          )
    )))
    


strategy_planning <- makePage(
    "",
    "",
    Stack(tokens = list(childGap = 10), 
          horizontal=FALSE,
          Stack(
              tokens = list(childGap = 10),
              horizontal=FALSE,
              makeCard2("Adjust vaccine coverage",vaccinecovstrat, size=12
              )),
          Stack(
              tokens = list(childGap = 10), 
              horizontal=FALSE,
              makeCard2("Adjust timing of vaccine deployment", vaccineCovtimingstrat, size=12
              )),
          #Stack(
          #    tokens = list(childGap = 10), 
          #    horizontal=FALSE,
          #    makeCard2("Strategy results", size=12,
          #              gt_output(outputId = "gt_table_timing1out"))),
          #Stack(
              #tokens = list(childGap = 10), 
              #horizontal=FALSE,
              #makeCard2("Table", size=12,
              #          gt_output(outputId = "gt_table_timing1out1"))),
              Stack(tokens = list(childGap = 10),
                    horizontal=FALSE,
                    makeCard2("Vaccine coverage and deployment timing strategy averted infections", stratplotcombo, size=12,
                        ))
          )
    )


intro_text<-tagList(
    br(),
    div(class="para",
        p("The Influenza Burden Averted Through Vaccination tool is initiative led by the World Health Organization (WHO), The Pan-American Health Organization (PAHO) and the 
          Centers for Disease Control and Prevention (USCDC).")),
    br(),
    div(class="para",
        p("The tool was developed to support countries to assess the impact of seasonal influenza vaccine by estimating the number of influenza illnesses that were prevented 
        through vaccination. The underlying model uses the methodology described in the WHO Manual for Estimating the Disease Burden Associated with Seasonal Influenza. This 
        collaboration entailed development of a model, an update of the Manual and tools to assist with implementation.")),
    br(),
    br(),
    div(class="para ms-fontSize-16 ms-fontWeight-semibold",
        p("Method overview")),
    div(class="para",
        p("The underlying model is based on a susceptible-infected-immune/recovered static compartmental model adapted to an immunization campaign (see manual for more details). 
        An Excel version of this tool is also available.")),
    br(),
    br(),
    img(src="images/Model_diagram.png", style="width: 1000px; display: block; margin-left: auto; margin-right: auto;"),
    br(),
    br(),
    div(class="para",
        p("The tool requires influenza burden, vaccine coverage and vaccine effectiveness data which are entered onto the 'Estimate burden averted' page of the tool. These 
    data are then used to estimate illness that was avoided, including the number of influenza-associated hospitalizations, non-hospitalized influenza illness and the number 
    of influenza-associated medical visits averted through vaccination as well as the number needed to vaccinate to prevent one case in these categories and the prevented fraction.")
        ),
    br(),
    div(class="para",
        p("The tool allows you to estimate the illness prevented in two scenarios:"),
        br(),
        p("- A vaccination campaign was conducted and estiamtes are required to determine the impact influenza vaccination actually made"),
        br(),
        p("- No actual vaccination campaign is conducted but estimates are required to determine the impact influenza vaccination could have made")),
    br(),
    div(class="para",
        p("Each of these scenarios is assessed against its counterfactual - a hypothetical model. The difference in influenza infections between the scenario and the hypothetical 
        model is the burden averted. That is, in the scenario where an actual influenza vaccination campaign is conducted and we want to estimate the impact that 
        the campaign had, the tool asses the season's data (number of actual hospitalizations, vaccine coverage, vaccine effectiveness, etc), against a hypothetical model where
        no vaccination campaign is conducted. The difference between the season's actual number of infections and the number of infections from the simulated model is the burden
          averted. For the scenario where no influenza vaccination campaign was conducted and we want to estimate the impact vaccination may have had, 
        the tool assesses the season's (with no vaccination) data against a hypothetical model where a vaccination campaign is simulated. The difference between the season's actual number of infections, 
        with no vaccination, and the number of infections simulated, utilizing estimated vaccine parameters, is the burden averted.")
          ),
    br(),
    br(),
    img(src="images/Scenarios.png", style="width: 1000px; display: block; margin-left: auto; margin-right: auto;")
        
    )



intro_page<-makePage(
    "",
    "",
    Stack(
        tokens = list(childGap = 10), 
        horizontal=FALSE,
        makeCard2("How it works: Tool development and methodology", size=12,
                  intro_text)))





router <- router_ui(
    route("/", estimate_burden_averted),
    route("how_it_works", intro_page),
    route("results_table", results_table),
    route("results_report", results_report),
    route("strategy_planning", strategy_planning)
)


ui <- fluentPage(
    tags$head(
        tags$link(href = "style.css", rel = "stylesheet", type = "text/css")
    ),
    div(class="header",
        div(class="titlelogowrapper",
            div(class="logos", style="display: flex; justify-content: space-around",
                div(class="left",
                    div(class="bannerlogos ms-fontSize-32 ms-fontWeight-semibold",
                        p(class="titletext", "Influenza Burden Averted Tool")
                    )
                ),
                div(class="bannerpics",
                    div(class="bannerlogos",
                        img(src="images/WHO-EN-C-H2.png")
                    ),
                    div(class="bannerlogos",
                        img(src="images/PAHO_logo.png")
                    ),
                    div(class="bannerlogos",
                        img(src="images/CDC-Logo.png")
                    )
                )
            )
        )
    ),
    layout(router),
    tags$style(".card { padding: 28px; margin-bottom: 28px}"),
    tags$style(type="text/css",
               ".shiny-output-error { visibility: hidden; }",
               ".shiny-output-error:before { visibility: hidden; }"
    )
)
