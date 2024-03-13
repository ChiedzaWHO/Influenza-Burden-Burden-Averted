

server <- function(input, output, session) {
    
    router_server()
    
    scenario <- reactive({
        input$scenario
    })
    
    year <- reactive({
        input$year
    })
    
    country <- reactive({
        input$country
    })
    
    targetgroup <- reactive({
        input$targetgroup
    })
    
    populationsize <- reactive({
        input$populationsize
    })
    
    vaccineeffectiveness <- reactive({
        input$vaccineeffectiveness
    })
    
    VELCI <- reactive({
        input$VELCI
    })
    
    VEUCI <- reactive({
        input$VEUCI
    })
    
    hnh <- reactive({
        input$hnh
    })
    
    lowerhnh <- reactive({
        input$lowerhnh
    })
    
    upperhnh <- reactive({
        input$upperhnh
    })
    
    mai <- reactive({
        input$mai
    })
    
    monthstartvac <- reactive({
        input$monthstartvac
    })
    
    vaccinecoveragemonthly <- reactive({
        input$vaccinecoveragemonthly
    })
    
    month2 <- reactive({
        input$month2
    })
    
    month3 <- reactive({
        input$month3
    })
    
    month4 <- reactive({
        input$month4
    })
    
    month5 <- reactive({
        input$month5
    })
    
    month6 <- reactive({
        input$month6
    })
    
    month7 <- reactive({
        input$month7
    })
    
    month8 <- reactive({
        input$month8
    })
    
    month9 <- reactive({
        input$month9
    })
    
    month10 <- reactive({
        input$month10
    })
    
    month11 <- reactive({
        input$month11
    })
    
    month12 <- reactive({
        input$month12
    })
    
    ## Coverage strategy
    hospitalizationsmonthly <- reactive({
        input$hospitalizationsmonthly
    })
    
    hospmonth2 <- reactive({
        input$hospmonth2
    })
    
    hospmonth3 <- reactive({
        input$hospmonth3
    })
    
    hospmonth4 <- reactive({
        input$hospmonth4
    })
    
    hospmonth5 <- reactive({
        input$hospmonth5
    })
    
    hospmonth6 <- reactive({
        input$hospmonth6
    })
    
    hospmonth7 <- reactive({
        input$hospmonth7
    })
    
    hospmonth8 <- reactive({
        input$hospmonth8
    })
    
    hospmonth9 <- reactive({
        input$hospmonth9
    })
    
    hospmonth10 <- reactive({
        input$hospmonth10
    })
    
    hospmonth11 <- reactive({
        input$hospmonth11
    })
    
    hospmonth12 <- reactive({
        input$hospmonth12
    })
    
    coverage1 <- reactive({
        input$coverage1
    })
    
    coverage2 <- reactive({
        input$coverage2
    })
    
    coverage3 <- reactive({
        input$coverage3
    })
    
    coverage4 <- reactive({
        input$coverage4
    })
    
    
    ## Timing strategy 1
    monthstartvac1 <- reactive({
        input$monthstartvac1
    })
    
    vaccinecoveragemonthly1 <- reactive({
        input$vaccinecoveragemonthly1
    })
    
    month21 <- reactive({
        input$month21
    })
    
    month31 <- reactive({
        input$month31
    })
    
    month41 <- reactive({
        input$month41
    })
    
    month51 <- reactive({
        input$month51
    })
    
    month61 <- reactive({
        input$month61
    })
    
    month71 <- reactive({
        input$month71
    })
    
    month81 <- reactive({
        input$month81
    })
    
    month91 <- reactive({
        input$month91
    })
    
    month101 <- reactive({
        input$month101
    })
    
    month111 <- reactive({
        input$month111
    })
    
    month121 <- reactive({
        input$month121
    })
    
    
    ## Timing strategy 2
    monthstartvac2 <- reactive({
        input$monthstartvac2
    })
    
    vaccinecoveragemonthly2 <- reactive({
        input$vaccinecoveragemonthly2
    })
    
    month22 <- reactive({
        input$month22
    })
    
    month32 <- reactive({
        input$month32
    })
    
    month42 <- reactive({
        input$month42
    })
    
    month52 <- reactive({
        input$month52
    })
    
    month62 <- reactive({
        input$month62
    })
    
    month72 <- reactive({
        input$month72
    })
    
    month82 <- reactive({
        input$month82
    })
    
    month92 <- reactive({
        input$month92
    })
    
    month102 <- reactive({
        input$month102
    })
    
    month112 <- reactive({
        input$month112
    })
    
    month122 <- reactive({
        input$month122
    })
    
    
    ## Timing strategy 3
    monthstartvac3 <- reactive({
        input$monthstartvac3
    })
    
    vaccinecoveragemonthly3 <- reactive({
        input$vaccinecoveragemonthly3
    })
    
    month23 <- reactive({
        input$month23
    })
    
    month33 <- reactive({
        input$month33
    })
    
    month43 <- reactive({
        input$month43
    })
    
    month53 <- reactive({
        input$month53
    })
    
    month63 <- reactive({
        input$month63
    })
    
    month73 <- reactive({
        input$month73
    })
    
    month83 <- reactive({
        input$month83
    })
    
    month93 <- reactive({
        input$month93
    })
    
    month103 <- reactive({
        input$month103
    })
    
    month113 <- reactive({
        input$month113
    })
    
    month123 <- reactive({
        input$month123
    })
    
    
    ## Timing strategy 4
    monthstartvac4 <- reactive({
        input$monthstartvac4
    })
    
    vaccinecoveragemonthly4 <- reactive({
        input$vaccinecoveragemonthly4
    })
    
    month24 <- reactive({
        input$month24
    })
    
    month34 <- reactive({
        input$month34
    })
    
    month44 <- reactive({
        input$month44
    })
    
    month54 <- reactive({
        input$month54
    })
    
    month64 <- reactive({
        input$month64
    })
    
    month74 <- reactive({
        input$month74
    })
    
    month84 <- reactive({
        input$month84
    })
    
    month94 <- reactive({
        input$month94
    })
    
    month104 <- reactive({
        input$month104
    })
    
    month114 <- reactive({
        input$month114
    })
    
    month124 <- reactive({
        input$month124
    })
    
    stratplotchoice <- reactive({
        input$stratplotchoice
    })
    
    
    
    
    ## Total coverage
    
    totalcoverage<-reactive({
        totalcoverage<-c(vaccinecoveragemonthly(), month2(), month3(), month4(), month5(), month6(),
                       month7(), month8(), month9(), month10(), month11(), month12())
        
        totalcoverage<-as.numeric(as.character(totalcoverage))
        
        totalcoverage<-round(sum(totalcoverage, na.rm=TRUE), 1)*100
        

    })
    
    
    ####################################################################################################
    ## Dataset
    ####################################################################################################
    
    
    
    burdendataset<- reactive({
        ################################ The output data when insufficient data has been entered   #####################################################################
        ###########################################################################################################################################################
        
        
        if(scenario()=="Impact of current influenza vaccination campaign"){
            
            
            ##########################################################################################
            # Scenario of influenza circulation in presence of vaccine: Presence to absence of vaccine
            # This is a situation where the country conducts a vaccination campaign, we enter
            # actual burden inputs and vaccine coverage then compare this to a hypothetical scenario
            # where there is no vaccination campaign conducted
            ##########################################################################################
            

            

            hospinf<-c(hospitalizationsmonthly(), hospmonth2(), hospmonth3(), hospmonth4(), hospmonth5(), hospmonth6(),
                       hospmonth7(), hospmonth8(), hospmonth9(), hospmonth10(), hospmonth11(), hospmonth12())
            
            populationsize<-as.numeric(as.character(populationsize()))
            vaccineeffectiveness<-as.numeric(as.character(vaccineeffectiveness()))
            hnh<-as.numeric(as.character(hnh()))
            mai<-as.numeric(as.character(mai()))
            hospinf<-as.numeric(as.character(hospinf))
            
            novaccine<-hospinf%>%
                as.data.frame()%>%
                rename(hospinfwvac=1)%>%
                mutate(hospinfcumulwvac=cumsum(hospinfwvac))%>%
                mutate(nonhospinfwvac=hospinfwvac*hnh)%>%
                mutate(nonhospinfcumulwvac=cumsum(nonhospinfwvac))%>%
                mutate(overallinfwvac=hospinfwvac+nonhospinfwvac)%>%
                mutate(overallinfcumulwvac=cumsum(overallinfwvac))%>%
                mutate(medicalattedtoinfwvac=overallinfwvac*mai)%>%
                mutate(medicallyattendedinfcumulwvac=cumsum(medicalattedtoinfwvac))
            
            
            
            ##bind monthly coverage
            monthvaccov<-c(vaccinecoveragemonthly(), month2(), month3(), month4(), month5(), month6(),
                           month7(), month8(), month9(), month10(), month11(), month12())
            
            monthvaccov<-as.numeric(as.character(monthvaccov))
            
            novaccine<-cbind(novaccine, monthvaccov)
            
            ## month number
            novaccine$monthnum<-c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
            
            ## add monthly effectively vaccinated
            novaccine<-novaccine%>%
                mutate(effectivelyvacc=populationsize*monthvaccov*vaccineeffectiveness)%>%
                mutate(effectivelyvacccumul=cumsum(effectivelyvacc))
            
            
            
            
            #####################################################################
            # Estimated rates
            #####################################################################
            novaccine<-novaccine%>%
                mutate(susceptiblewvacc=populationsize-hospinfcumulwvac-nonhospinfcumulwvac-effectivelyvacccumul)
            
            #Create columns
            novaccine$irhospinf<-NA
            novaccine$irnonhospinf<-NA
            
            #Create first row
            novaccine[1,14]<-novaccine$hospinfwvac[1]/populationsize
            novaccine[1,15]<-novaccine$nonhospinfwvac[1]/populationsize
            
            
            ##Calculation uses last month's susceptible population
            for(i in 2:nrow(novaccine)){
                novaccine$irhospinf[i]<-novaccine$hospinfwvac[i]/novaccine$susceptiblewvacc[i-1]
                novaccine$irnonhospinf[i]<- novaccine$nonhospinfwvac[i]/novaccine$susceptiblewvacc[i-1]
            }
            
            #####################################################################
            # Scenario of influenza circulation in absence of vaccine
            #####################################################################
            
            ## These calculations are initialized by the 'Susceptible at start of month in absence of vaccine'
            ## column, which starts with the Target Population in the first month.
            
            remainsuscstartmonth<-c(populationsize, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
            novaccine<-cbind(novaccine, remainsuscstartmonth)
            
            #Create columns
            novaccine$hospinfwovac<-NA
            novaccine$hospinfcumulwovacc<-NA
            novaccine$nonhospinfwovac<-NA
            novaccine$nonhospinfcumulwovacc<-NA
            novaccine$remainingsusc<-NA
            
            #Create first row
            novaccine[1,17]<-novaccine$remainsuscstartmonth[1]*novaccine$irhospinf[1]
            novaccine[1,18]<-novaccine$hospinfwovac[1]
            novaccine[1,19]<-novaccine$remainsuscstartmonth[1]*novaccine$irnonhospinf[1]
            novaccine[1,20]<-novaccine$nonhospinfwovac[1]
            novaccine[1,21]<-populationsize-novaccine$hospinfcumulwovacc[1]-novaccine$nonhospinfcumulwovacc[1]
            
            
            #Create rows 2:12
            for(i in 2:nrow(novaccine)){
                novaccine$remainsuscstartmonth[i]<-novaccine$remainingsusc[i-1]
                novaccine$hospinfwovac[i]<-novaccine$remainsuscstartmonth[i]*novaccine$irhospinf[i]
                novaccine$hospinfcumulwovacc[i]<-sum(novaccine$hospinfwovac[1:i])
                novaccine$nonhospinfwovac[i]<-novaccine$remainsuscstartmonth[i]*novaccine$irnonhospinf[i]
                novaccine$nonhospinfcumulwovacc[i]<-sum(novaccine$nonhospinfwovac[1:i])
                novaccine$remainingsusc[i]<-populationsize-novaccine$hospinfcumulwovacc[i]-novaccine$nonhospinfcumulwovacc[i]
            }
            
            novaccine<-novaccine%>%
                mutate(overallinfwovac=hospinfwovac+nonhospinfwovac)%>%
                mutate(overallinfcumulwovacc=cumsum(overallinfwovac))%>%
                mutate(medicallyattendedinfwovac=overallinfwovac*mai)%>%
                mutate(medicalattedtoinfcumulwovacc=cumsum(medicallyattendedinfwovac))
            
            ######## Avoided events
            novaccine<-novaccine%>%
                mutate(AEhospinf=hospinfwovac-hospinfwvac)%>%
                mutate(AEnonhospinf=nonhospinfwovac-nonhospinfwvac)%>%
                mutate(AEmedicalattedtoinf=medicallyattendedinfwovac-medicalattedtoinfwvac)%>%
                mutate(AEoverallinf=overallinfwovac-overallinfwvac)
            
            
            ################ Prevented fraction
            ## Hospitalized infections
            ifelse (monthstartvac()==1,
                    novaccine<-novaccine%>%
                        mutate(PFhospinf=sum(AEhospinf)/sum(hospinfwovac)),
                    novaccine<-novaccine%>%
                        mutate(PFhospinf=sum(AEhospinf[monthstartvac():nrow(novaccine)]/sum(hospinfwovac[monthstartvac():nrow(novaccine)])))
            )
            
            ## non-Hospitalized infections
            ifelse (monthstartvac()==1,
                    novaccine<-novaccine%>%
                        mutate(PFnonhospinf=sum(AEnonhospinf)/sum(nonhospinfwovac)),
                    novaccine<-novaccine%>%
                        mutate(PFnonhospinf=sum(AEnonhospinf[monthstartvac():nrow(novaccine)]/sum(nonhospinfwovac[monthstartvac():nrow(novaccine)])))
            )
            
            
            ################ NNV -----------------------------------> why does this only go to month 8 in the Excel formula?????
            ## Hospitalized infections
            ifelse(monthstartvac()==1,
                   novaccine<-novaccine%>%
                       mutate(NNVhospinf=1/((vaccineeffectiveness*(sum(hospinfwovac)))/populationsize)),
                   novaccine<-novaccine%>%
                       mutate(NNVhospinf=1/((vaccineeffectiveness*(sum(hospinfwovac[monthstartvac():nrow(novaccine)])))/populationsize))
            )
            
            
            ## non-Hospitalized infections
            ifelse(monthstartvac()==1,
                   novaccine<-novaccine%>%
                       mutate(NNVnonhospinf=1/((vaccineeffectiveness*(sum(nonhospinfwovac)))/populationsize)),
                   novaccine<-novaccine%>%
                       mutate(NNVnonhospinf=1/((vaccineeffectiveness*(sum(nonhospinfwovac[monthstartvac():nrow(novaccine)])))/populationsize))
            )
            
            ## Medically infections
            ifelse(monthstartvac()==1,
                   novaccine<-novaccine%>%
                       mutate(NNVmedicalattendedinf=1/((vaccineeffectiveness*(sum(medicallyattendedinfwovac)))/populationsize)),
                   novaccine<-novaccine%>%
                       mutate(NNVmedicalattendedinf=1/((vaccineeffectiveness*(sum(medicallyattendedinfwovac[monthstartvac():nrow(novaccine)])))/populationsize))
            )
            
            ## Overall infections
            ifelse(monthstartvac()==1,
                   novaccine<-novaccine%>%
                       mutate(NNVoverallinf=1/((vaccineeffectiveness*(sum(hospinfwovac)+sum(nonhospinfwovac)))/populationsize)),
                   novaccine<-novaccine%>%
                       mutate(NNVoverallinf=1/((vaccineeffectiveness*(sum(hospinfwovac[monthstartvac():nrow(novaccine)])+sum(nonhospinfwovac[monthstartvac():nrow(novaccine)])))/populationsize))
            )
            
            return(novaccine)
            
            
            
        } #close if current campaign
        
        
        
        if(scenario()=="Impact of hypothetical vaccination campaign"){
            
            ##########################################################################################
            # Scenario of influenza circulation in absence of vaccine: Absence to presence of vaccine
            # This is a scenario where the country has yet to conduct a vaccination campaign but would
            # like to see what impact a vaccine campaign may have.
            # The country enters the estimated number of hospitalization they observed and then we compare
            # this to a hypothetical vaccination campaign
            ##########################################################################################
            
            hospinf<-c(hospitalizationsmonthly(), hospmonth2(), hospmonth3(), hospmonth4(), hospmonth5(), hospmonth6(),
                       hospmonth7(), hospmonth8(), hospmonth9(), hospmonth10(), hospmonth11(), hospmonth12())
            
            populationsize<-as.numeric(as.character(populationsize()))
            hnh<-as.numeric(as.character(hnh()))
            mai<-as.numeric(as.character(mai()))
            hospinf<-as.numeric(as.character(hospinf))
            vaccineeffectiveness<-as.numeric(as.character(vaccineeffectiveness()))
            
            #####################Observed influenza in the absence of vaccine
            novaccine<-hospinf%>%
                as.data.frame()%>%
                rename(hospinfwovac=1)%>%
                mutate(hospinfcumulwovac=cumsum(hospinfwovac))%>%
                mutate(nonhospinfwovac=hospinfwovac*hnh)%>%
                mutate(nonhospinfcumulwovac=cumsum(nonhospinfwovac))%>%
                mutate(remainingsusc=populationsize-hospinfcumulwovac-nonhospinfcumulwovac)
            
            #create remainsuscstartmonth column with target population and rows 1:11 of remainingsusc
            remainsuscstartmonth<-populationsize%>%
                as.data.frame()%>%
                rename(remainsuscstartmonth=1)
            
            remainsuscstartmonth2<-as.data.frame(novaccine$remainingsusc[1:11])%>%
                as.data.frame()%>%
                rename(remainsuscstartmonth=1)
            
            remainsuscstartmonth<-rbind(remainsuscstartmonth, remainsuscstartmonth2)
            
            novaccine<-cbind(novaccine, remainsuscstartmonth)
            
            novaccine<-novaccine%>%
                mutate(overallinfwovac=hospinfwovac+nonhospinfwovac)%>%
                mutate(overallinfcumulwovac=cumsum(overallinfwovac))%>%
                mutate(medicallyattendedinfwovac=overallinfwovac*mai)%>%
                mutate(medicallyattendedinfcumulwovac=cumsum(medicallyattendedinfwovac))
            
            
            
            #####################################################################
            # Incidence rates in the absence of vaccine
            #####################################################################
            novaccine<-novaccine%>%
                mutate(irhospinf=hospinfwovac/remainsuscstartmonth)%>%
                mutate(irnonhospinf=nonhospinfwovac/remainsuscstartmonth)
            
            
            
            
            ############################ Burden inputs with hypothetical vaccination campaign
            #Create columns
            novaccine$hospinfwvac<-NA
            novaccine$hospinfcumulwvac<-NA
            novaccine$nonhospinfwvac<-NA
            novaccine$nonhospinfcumulwvac<-NA
            
            
            
            #Create first row
            novaccine[1,13]<-populationsize*novaccine$irhospinf[1]
            novaccine[1,14]<-novaccine$hospinfwvac[1]
            novaccine[1,15]<-populationsize*novaccine$irnonhospinf[1]
            novaccine[1,16]<-novaccine$nonhospinfwvac[1]
            
            ##bind monthly coverage
            monthvaccov<-c(vaccinecoveragemonthly(), month2(), month3(), month4(), month5(), month6(),
                           month7(), month8(), month9(), month10(), month11(), month12())
            
            monthvaccov<-as.numeric(as.character(monthvaccov))
            
            novaccine<-cbind(novaccine, monthvaccov)
            
            
            
            ##monthly effectively vaccinated and susceptible with hypothetical vaccination campaign
            novaccine<-novaccine%>%
                mutate(effectivelyvacc=populationsize*monthvaccov*vaccineeffectiveness)%>%
                mutate(effectivelycumulvacc=cumsum(effectivelyvacc))%>%
                mutate(suscwvacc=populationsize-hospinfcumulwvac-nonhospinfcumulwvac-effectivelyvacc)
            
            ##Then calculate the rest of hospinfwovac and nonhospinfwovac. Calculation uses last month's incidence rate
            for(i in 2:nrow(novaccine)){
                novaccine$hospinfwvac[i]<-novaccine$suscwvacc[i-1]*novaccine$irhospinf[i]
                novaccine$hospinfcumulwvac[i]<-sum(novaccine$hospinfwvac[1:i])
                novaccine$nonhospinfwvac[i]<- novaccine$suscwvacc[i-1]*novaccine$irnonhospinf[i]
                novaccine$nonhospinfcumulwvac[i]<-sum(novaccine$nonhospinfwvac[1:i])
                novaccine$suscwvacc[i]=populationsize-novaccine$hospinfcumulwvac[i]-novaccine$nonhospinfcumulwvac[i]-novaccine$effectivelycumulvacc[i]
            }
            
            
            ## month number
            novaccine$monthnum<-c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
            
            
            ## Overall infections with hypothetical vaccination campaign
            novaccine<-novaccine%>%
                mutate(overallinfwvac=hospinfwvac+nonhospinfwvac)%>%
                mutate(overallinfcumulwvacc=cumsum(overallinfwvac))
            
            ## Medically attended with hypothetical vaccination campaign
            novaccine<-novaccine%>%
                mutate(medicalattedtoinfwvac=overallinfwvac*mai)
            
            
            ######## Avoided events
            novaccine<-novaccine%>%
                mutate(AEhospinf=hospinfwovac-hospinfwvac)%>%
                mutate(AEnonhospinf=nonhospinfwovac-nonhospinfwvac)%>%
                mutate(AEmedicalattedtoinf=medicallyattendedinfwovac-medicalattedtoinfwvac)%>%
                mutate(AEoverallinf=overallinfwovac-overallinfwvac)
            
            
            
            ################ Prevented fraction
            ## Hospitalized infections
            ifelse (monthstartvac()==1,
                    novaccine<-novaccine%>%
                        mutate(PFhospinf=sum(AEhospinf)/sum(hospinfwovac)),
                    novaccine<-novaccine%>%
                        mutate(PFhospinf=sum(AEhospinf[monthstartvac():nrow(novaccine)]/sum(hospinfwovac[monthstartvac():nrow(novaccine)])))
            )
            
            ## non-Hospitalized infections
            ifelse (monthstartvac()==1,
                    novaccine<-novaccine%>%
                        mutate(PFnonhospinf=sum(AEnonhospinf)/sum(nonhospinfwovac)),
                    novaccine<-novaccine%>%
                        mutate(PFnonhospinf=sum(AEnonhospinf[monthstartvac():nrow(novaccine)]/sum(nonhospinfwovac[monthstartvac():nrow(novaccine)])))
            )
            
            
            
            
            ################ NNV -----------------------------------> why does this only go to month 8 in the Excel formula?????
            ## Hospitalized infections
            ifelse(monthstartvac()==1,
                   novaccine<-novaccine%>%
                       mutate(NNVhospinf=1/((vaccineeffectiveness*(sum(hospinfwovac)))/populationsize)),
                   novaccine<-novaccine%>%
                       mutate(NNVhospinf=1/((vaccineeffectiveness*(sum(hospinfwovac[monthstartvac():nrow(novaccine)])))/populationsize))
            )
            
            
            ## non-Hospitalized infections
            ifelse(monthstartvac()==1,
                   novaccine<-novaccine%>%
                       mutate(NNVnonhospinf=1/((vaccineeffectiveness*(sum(nonhospinfwovac)))/populationsize)),
                   novaccine<-novaccine%>%
                       mutate(NNVnonhospinf=1/((vaccineeffectiveness*(sum(nonhospinfwovac[monthstartvac():nrow(novaccine)])))/populationsize))
            )
            
            ## Medically attended infections
            ifelse(monthstartvac()==1,
                   novaccine<-novaccine%>%
                       mutate(NNVmedicalattendedinf=1/((vaccineeffectiveness*(sum(medicallyattendedinfwovac)))/populationsize)),
                   novaccine<-novaccine%>%
                       mutate(NNVmedicalattendedinf=1/((vaccineeffectiveness*(sum(medicallyattendedinfwovac[monthstartvac():nrow(novaccine)])))/populationsize))
            )
            
            ## Total infections
            ifelse(monthstartvac()==1,
                   novaccine<-novaccine%>%
                       mutate(NNVoverallinf=1/((vaccineeffectiveness*(sum(hospinfwovac)+sum(nonhospinfwovac)))/populationsize)),
                   novaccine<-novaccine%>%
                       mutate(NNVoverallinf=1/((vaccineeffectiveness*(sum(hospinfwovac[monthstartvac():nrow(novaccine)])+sum(nonhospinfwovac[monthstartvac():nrow(novaccine)])))/populationsize))
            )
            
            return(novaccine)
            
            
        }#close "if hypothetical campaign"
        
    }) #close burdendataset
    
    
    
    ####################################################################################################
    ## List of burden averted results
    ####################################################################################################
    
    
    burdenavertedresults<-reactive({
        
        populationsize<-as.numeric(as.character(populationsize()))
        hnh<-as.numeric(as.character(hnh()))
        mai<-as.numeric(as.character(mai()))
        vaccineeffectiveness<-as.numeric(as.character(vaccineeffectiveness()))
        
       
        

        #################################################
        ## Results
        #################################################
        nonhospinfwovactot<-sum(burdendataset()$nonhospinfwovac)
        nonhospinfwvacctot<-sum(burdendataset()$nonhospinfwvac)
        nonhospinfaverted<-nonhospinfwovactot-nonhospinfwvacctot
        NNVnonhospinffinal<-burdendataset()$NNVnonhospinf[1]
        
        medicallyattendedinfwovactot<-sum(burdendataset()$medicallyattendedinfwovac)
        medicallyattendedinfwvactot<-sum(burdendataset()$medicalattedtoinfwvac)
        medicallyattendedinfaverted<-medicallyattendedinfwovactot-medicallyattendedinfwvactot
        NNVmedicallyattendtedfinal<-burdendataset()$NNVmedicalattendedinf[1]
        
        Totalinfwovacc<-sum(burdendataset()$overallinfwovac)
        Totalinfwvacc<-sum(burdendataset()$overallinfwvac)
        Totalinfaverted<-Totalinfwovacc-Totalinfwvacc
        NNVtotalinf<-burdendataset()$NNVoverallinf[1]
        
        hospinfwovactot<-sum(burdendataset()$hospinfwovac)
        hospinfwvacctot<-sum(burdendataset()$hospinfwvac)
        hospaverted<-hospinfwovactot-hospinfwvacctot
        NNVhospinffinal<-burdendataset()$NNVhospinf[1]
        
        
        nonhospPreventedfraction<-burdendataset()$PFnonhospinf[1]
        hospPreventedfraction<-burdendataset()$PFhospinf[1]
        
        
        indicator_results<-c(nonhospinfwovactot, nonhospinfwvacctot, nonhospinfaverted, NNVnonhospinffinal,
                             Totalinfwovacc, Totalinfwvacc, Totalinfaverted, NNVtotalinf,
                             hospinfwovactot, hospinfwvacctot, hospaverted, NNVhospinffinal,
                             medicallyattendedinfwovactot, medicallyattendedinfwvactot, medicallyattendedinfaverted, NNVmedicallyattendtedfinal,
                             nonhospPreventedfraction)
        
        indicator_results_titles<-c("nonhospinfwovactot", "nonhospinfwvacctot", "nonhospinfaverted", "NNVnonhospinffinal",
                                    "Totalinfwovacc", "Totalinfwvacc", "Totalinfaverted", "NNVtotalinf",
                                    "hospinfwovactot", "hospinfwvacctot", "hospaverted", "NNVhospinffinal",
                                    "medicallyattendedinfwovactot", "medicallyattendedinfwvactot", "medicallyattendedinfaverted", "NNVmedicallyattendtedfinal",
                                    "nonhospPreventedfraction")
        
        indicator_results<-cbind(indicator_results_titles, indicator_results)
        
        
        return(indicator_results)
        
        
    })#burdenavertedresults
    
    
    
    
    ####################################################################################################
    ## Construct data table to be used for confidence interval estimation
    ####################################################################################################
    
    
    tableforCI<-reactive({
        populationsize<-as.numeric(as.character(populationsize()))
        hnh<-as.numeric(as.character(hnh()))
        mai<-as.numeric(as.character(mai()))
        vaccineeffectiveness<-as.numeric(as.character(vaccineeffectiveness()))
        VELCI<-as.numeric(as.character(VELCI()))
        VEUCI<-as.numeric(as.character(VEUCI()))
        
        if(scenario()=="Impact of current influenza vaccination campaign"){
            in_dat<-c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
            
            in_dat<-in_dat%>%
                as.data.frame()%>%
                rename("index"=1)
            
            in_dat$year<-year()
            in_dat$target_grp<-targetgroup()
            in_dat$population<-populationsize
            in_dat$month<-c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
            
            in_dat$pvinc_nhosp<-(sum(burdendataset()$nonhospinfwvac)/populationsize)*10000
            in_dat$pvinc_hosp<-(sum(burdendataset()$hospinfwvac)/populationsize)*10000
            in_dat$pvinc_ill<-in_dat$pvinc_nhosp+in_dat$pvinc_nhosp
            in_dat$pvinc_ma<-in_dat$pvinc_ill*mai
            
            
            in_dat$avinc_hosp<-0
            in_dat$avinc_nhosp<-0
            in_dat$avinc_ill<-0
            in_dat$avinc_ma<-0
            
            
            in_dat$pvn_hosp<-burdendataset()$hospinfwvac
            in_dat$adj_hosprate<-(in_dat$pvn_hosp/populationsize)*100000
            in_dat$pvn_nohosp<-burdendataset()$nonhospinfwvac
            in_dat$pvn_case<-burdendataset()$hospinfwvac+burdendataset()$nonhospinfwvac
            in_dat$pvn_macase<-in_dat$pvn_case*mai
            
            in_dat$avn_hosp<-0
            in_dat$avn_nohosp<-0
            in_dat$avn_case<-0
            in_dat$avn_macase<-0
            in_dat$mnth_coverage<-burdendataset()$monthvaccov
            in_dat$total_coverage<-sum(in_dat$mnth_coverage)
            in_dat$coverage_se<-sqrt(in_dat$total_coverage/populationsize)
            in_dat$adjve<-vaccineeffectiveness
            in_dat$adjve_lcl<-VELCI
            in_dat$adjve_ucl<-VEUCI
            in_dat$mult_frac<-ifelse(in_dat$month<monthstartvac(), 0, 1)
            
            return(in_dat)
            
        }
        
        
        if(scenario()=="Impact of hypothetical vaccination campaign"){
            
            in_dat<-c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
            
            in_dat<-in_dat%>%
                as.data.frame()%>%
                rename("index"=1)
            
            in_dat$year<-year()
            in_dat$target_grp<-targetgroup()
            in_dat$population<-populationsize
            in_dat$month<-c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
            
            in_dat$pvinc_ill<-0
            in_dat$pvinc_ma<-0
            in_dat$pvinc_nhosp<-0
            in_dat$pvinc_hosp<-0
            
            in_dat$avinc_hosp<-((sum(burdendataset()$hospinfwovac))/populationsize)*10000
            in_dat$avinc_nhosp<-((sum(burdendataset()$nonhospinfwovac))/populationsize)*10000
            in_dat$avinc_ill<-in_dat$avinc_hosp+in_dat$avinc_nhosp
            in_dat$avinc_ma<-in_dat$avinc_ill*mai
            
            in_dat$adj_hosprate<-0
            in_dat$pvn_hosp<-0
            in_dat$pvn_nohosp<-0
            in_dat$pvn_case<-0
            in_dat$pvn_macase<-0
            
            in_dat$avn_hosp<-burdendataset()$hospinfwovac
            in_dat$avn_nohosp<-burdendataset()$nonhospinfwovac
            in_dat$avn_case<-burdendataset()$overallinfwovac
            in_dat$avn_macase<-in_dat$avn_case*mai
            in_dat$mnth_coverage<-burdendataset()$monthvaccov
            in_dat$total_coverage<-sum(in_dat$mnth_coverage)
            in_dat$coverage_se<-sqrt(in_dat$total_coverage/populationsize)
            in_dat$adjve<-vaccineeffectiveness
            in_dat$adjve_lcl<-VELCI
            in_dat$adjve_ucl<-VEUCI
            in_dat$mult_frac<-ifelse(in_dat$month<monthstartvac(), 0, 1)
            
            return(in_dat)
            
            
        }
        
    })
    
    
    ####################################################################################################
    ## Calculate confidence intervals
    ####################################################################################################
    
    CI<-reactive({
        
        
        source("Confidence_intervals.R", local = TRUE)
        
        return(summary)
        
    })
    
    
    ####################  Prepare summary data for confidence intervals ###################################
    #######################################################################################################
    
    CIresultdf<-reactive({
        
        if(scenario()=="Impact of current influenza vaccination campaign"){
            
            nonhospinfavertedCI<-paste0(round(CI()$pvavnhosp_lbound, 2), " - ", round(CI()$pvavnhosp_ubound, 2))
            NNVnonhospinffinalCI<-paste0(round(CI()$pvnnv_nhosp_lbound, 2), " - ", round(CI()$pvnnv_nhosp_ubound, 2))
            TotalinfavertedCI<-paste0(round(CI()$pvavinf_lbound, 2), " - ", round(CI()$pvavinf_ubound, 2))
            NNVtotalinfCI<-paste0(round(CI()$pvnnv_inf_lbound, 2), " - ", round(CI()$pvnnv_inf_ubound, 2))
            hospavertedCI<-paste0(round(CI()$pvavhosp_lbound, 2), " - ", round(CI()$pvavhosp_ubound, 2))
            NNVhospinffinalCI<-paste0(round(CI()$pvnnv_hosp_lbound, 2), " - ", round(CI()$pvnnv_hosp_ubound, 2))
            medicallyattendedinfavertedCI<-paste0(round(CI()$pvavmainf_lbound, 2), " - ", round(CI()$pvavmainf_ubound, 2))
            NNVmedicallyattendtedfinalCI<-paste0(round(CI()$pvnnv_mainf_lbound, 2), " - ", round(CI()$pvnnv_mainf_ubound, 2))
            nonhospPreventedfractionCI<-paste0(round((CI()$pvavhospperc_lbound*100), 2), " - ", round((CI()$pvavhospperc_ubound*100), 2))
            
            
            confidenceint<-c("", "", nonhospinfavertedCI, NNVnonhospinffinalCI,
                             "", "", TotalinfavertedCI, NNVtotalinfCI,
                             "", "", hospavertedCI, NNVhospinffinalCI,
                             "", "", medicallyattendedinfavertedCI, NNVmedicallyattendtedfinalCI,
                             nonhospPreventedfractionCI)
            
            confidenceint<-confidenceint%>%
                as.data.frame()%>%
                rename("confidenceint"=1)
            
            return(confidenceint)
            
        }
        
        if(scenario()=="Impact of hypothetical vaccination campaign"){
            
            nonhospinfavertedCI<-paste0(round(CI()$avavnhosp_lbound, 2), " - ", round(CI()$avavnhosp_ubound, 2))
            TotalinfavertedCI<-paste0(round(CI()$avavinf_lbound, 2), " - ", round(CI()$avavinf_ubound, 2))
            hospavertedCI<-paste0(round(CI()$avavhosp_lbound, 2), " - ", round(CI()$avavhosp_ubound, 2))
            medicallyattendedinfavertedCI<-paste0(round(CI()$avavmainf_lbound, 2), " - ", round(CI()$avavmainf_ubound, 2))
            NNVnonhospinffinalCI<-paste0(round(CI()$avnnv_nhosp_lbound, 2), " - ", round(CI()$avnnv_nhosp_ubound, 2))
            NNVtotalinfCI<-paste0(round(CI()$avnnv_inf_lbound, 2), " - ", round(CI()$avnnv_inf_ubound, 2))
            NNVhospinffinalCI<-paste0(round(CI()$avnnv_hosp_lbound, 2), " - ", round(CI()$avnnv_hosp_ubound, 2))
            NNVmedicallyattendtedfinalCI<-paste0(round(CI()$avnnv_mainf_lbound, 2), " - ", round(CI()$avnnv_mainf_ubound, 2))
            nonhospPreventedfractionCI<-paste0(round((CI()$avavhospperc_lbound*100), 2), " - ", round((CI()$avavhospperc_ubound*100), 2))
            
            
            confidenceint<-c("", "", nonhospinfavertedCI, NNVnonhospinffinalCI,
                             "", "", TotalinfavertedCI, NNVtotalinfCI,
                             "", "", hospavertedCI, NNVhospinffinalCI,
                             "", "", medicallyattendedinfavertedCI, NNVmedicallyattendtedfinalCI,
                             nonhospPreventedfractionCI)
            
            confidenceint<-confidenceint%>%
                as.data.frame()%>%
                rename("confidenceint"=1)
            
            return(confidenceint)
            
        }
        
    })
    
    
    
    gt_table<- reactive({
        
        ####################################
        ## Render results table
        ####################################
        
        populationsize<-as.numeric(as.character(populationsize()))
        hnh<-as.numeric(as.character(hnh()))
        mai<-as.numeric(as.character(mai()))
        vaccineeffectiveness<-as.numeric(as.character(vaccineeffectiveness()))
        VELCI<-as.numeric(as.character(VELCI()))
        VEUCI<-as.numeric(as.character(VEUCI()))
        
        
        burdenavertedresults<-burdenavertedresults()%>%
            as.data.frame()%>%
            mutate(indicator_results=as.numeric(indicator_results))%>%
            select(indicator_results)
        
        
        presenceTable<-cbind(presenceTable, burdenavertedresults, CIresultdf())
        
        
        presenceTablefinal<-presenceTable%>%
            select(indicators, indicator_results, confidenceint)%>%
            gt()%>%
            fmt_number(
                columns=indicator_results,
                decimals=2,
                rows=2:17
            )%>%
            fmt_percent(
                columns = indicator_results,
                decimals = 2,
                rows=17:17
            )%>%
            tab_header(
                title = paste0(scenario(), ", ", year(), ", in ", country()),
                subtitle = paste0("Target group: ", targetgroup())
            )%>%
            cols_label(
                indicators = "",
                indicator_results = "Estimate",
                confidenceint = "Confidence interval",
            )%>%
            tab_row_group(
                label = "Non-hospitalized infections",
                rows = 1:4)%>%
            tab_row_group(
                label = "Total infections",
                rows = 5:8)%>%
            tab_row_group(
                label = "Hospitalized infections",
                rows = 9:12)%>%
            tab_row_group(
                label = "Medically attended infections",
                rows = 13:16)%>%
            tab_row_group(
                label = "Prevented fraction",
                rows = 17:17)%>%
            tab_style(
                locations = cells_row_groups(),
                style     = list(
                    cell_text(weight = "bold")
                ))%>%
            tab_options(
                #Adjust grouped rows to make them stand out
                row_group.background.color = "#e1e3e1")%>%
            tab_style(
                locations = cells_title(groups = "title"),
                style     = list(
                    cell_text(weight = "bold", size = 24)
                ))%>%
            tab_style(
                locations = cells_title(groups = "subtitle"),
                style     = list(
                    cell_text(weight = "bold", size = 18)
                ))%>%
            tab_style(
                locations = cells_column_labels(columns = everything()),
                style     = list(
                    cell_text(weight = "bold")
                )
            )%>%
            row_group_order(groups = c("Hospitalized infections", "Non-hospitalized infections", "Total infections", "Medically attended infections", "Prevented fraction"))
        
        return(presenceTablefinal)
        
    }
    ) ##close table
    
    
    
    output$table<-render_gt({
        table<-gt_table()
        return(table)
    })
    
    
    
    
    ##table for results
    output$table_resultsummaryvacc<-render_gt({
        hospinf<-c(hospitalizationsmonthly(), hospmonth2(), hospmonth3(), hospmonth4(), hospmonth5(), hospmonth6(),
                   hospmonth7(), hospmonth8(), hospmonth9(), hospmonth10(), hospmonth11(), hospmonth12())
        
        hospinf<-as.numeric(as.character(hospinf))
        
        vacc<-c(vaccinecoveragemonthly(), month2(), month3(), month4(), month5(), month6(),
                month7(), month8(), month9(), month10(), month11(), month12())
        
        vacc<-as.numeric(as.character(vacc))
        
        
        month<-c("Month 1", "Month 2", "Month 3", "Month 4", "Month 5", "Month 6", "Month 7", "Month 8", "Month 9",
                 "Month 10", "Month 11", "Month 12")
        
        vacc<-vacc%>%
            as.data.frame()%>%
            mutate_if(is.numeric, round, digits=3)%>%
            rename(vacc=1)
        
        table<-cbind(month, hospinf, vacc)%>%
            as.data.frame()%>%
            gt()%>%
            cols_label(
                month = "Month",
                hospinf = "Hospitalizations",
                vacc = "Vaccination coverage"
            )
    })
    
    output$table_resultmultipliers<-render_gt({
        
        hnhtitle<-c("Ratio non-hospitalized:hospitalized")
        maititle<-c("Proportion of infections that were medically attended")
        
        hnh1<-as.numeric(as.character(hnh()))
        mai1<-as.numeric(as.character(mai()))
        
        hnh<-round(hnh1, 2)
        mai<-round(mai1, 2)
        
        titles<-rbind(hnhtitle, maititle)
        multipliers<-rbind(hnh, mai)
        
        table<-cbind(titles, multipliers)%>%
            as.data.frame()%>%
            gt()%>%
            cols_label(
                V1 = "",
                V2 = "Multiplier")
        
        
    })
    
    
    
    output$resultsgraph<-renderPlot({
        
        
        #Overall infections graph
        
        table<-burdendataset()%>%
            as.data.frame()%>%
            mutate(monthvaccov=(monthvaccov*100))%>%
            select(monthnum, overallinfwvac, AEoverallinf, monthvaccov)%>%
            gather(infections, number, overallinfwvac:AEoverallinf)
        
        
        
        graph_title<-paste0(scenario(), " on influenza infections, ", country(), " , ", year())
        
        
        plot1<-table%>%
            ggplot(aes(x=factor(monthnum)))+
            geom_bar(stat = "identity", aes(y=number, fill=infections))+
            geom_line(aes(y=monthvaccov*700, group=1, color="monthvaccov"), linetype = "dashed")+
            labs(x="Campaign month", y="Number of infections")+
            scale_fill_manual(values=c("#2f6c9a", "#e2eef6"), labels=c("Infections averted", "Infections not averted"))+
            scale_color_manual(values=c("#1f4664"), labels=c("Vaccine coverage"))+
            scale_y_continuous(
                
                # Features of the first axis
                name = "Number of infections",
                
                # Add a second axis and specify its features
                sec.axis = sec_axis( trans=~./700, name="Vaccine coverage")
            ) +
            ggtitle(graph_title)+
            theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                  panel.background = element_blank(), legend.title=element_blank(), plot.title=element_text(size=11, face='bold'))
        
        
        
        ###Hospitalizations graph
        
        table2<-burdendataset()%>%
            as.data.frame()%>%
            mutate(monthvaccov=(monthvaccov*100))%>%
            select(monthnum, hospinfwvac, AEhospinf, monthvaccov)%>%
            gather(hospitalizations, number, hospinfwvac:AEhospinf)
        
        
        graph_title2<-paste0(scenario(), " on influenza-associated hospitalizations, ", country(), " , ", year())
        
        plot2<-table2%>%
            ggplot(aes(x=factor(monthnum)))+
            geom_bar(stat = "identity", aes(y=number, fill=hospitalizations))+
            geom_line(aes(y=monthvaccov*16, group=1, color="monthvaccov"), linetype = "dashed")+
            labs(x="Campaign month", y="Number of hospitalizations")+
            scale_fill_manual(values=c("#2f6c9a", "#e2eef6"), labels=c("Hospitalizations averted", "Hospitalizations not averted"))+
            scale_color_manual(values=c("#1f4664"), labels=c("Vaccine coverage"))+
            scale_y_continuous(
                
                # Features of the first axis
                name = "Number of hospitalizations",
                
                # Add a second axis and specify its features
                sec.axis = sec_axis( trans=~./16, name="Vaccine coverage")
            ) +
            ggtitle(graph_title2)+
            theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                  panel.background = element_blank(), legend.title=element_blank(), plot.title=element_text(size=11, face='bold'))
        
        
        
        combinedplot<-ggarrange(plot1, plot2)
        
        
        
        return(combinedplot)
        
        
    })
    
    
    
    
    
    
    output$text1.1 <- renderText(
        
       
        
        if(is.na(input$vaccineeffectiveness) | is.na(input$hnh) | is.na(input$mai)){
            paste0(" ")
            
        }else if(input$scenario=="Impact of current influenza vaccination campaign"){
            
            vaccineeffectiveness<-vaccineeffectiveness()
            vaccineeffectiveness<-as.numeric(as.character(vaccineeffectiveness))
           
            vaccinecoveragemonthly<-vaccinecoveragemonthly()
            vaccinecoveragemonthly<-as.numeric(as.character(vaccinecoveragemonthly))
            
            month2<-month2()
            month2<-as.numeric(as.character(month2))
            
            month3<-month3()
            month3<-as.numeric(as.character(month3))
            
            month4<-month4()
            month4<-as.numeric(as.character(month4))
            
            month5<-month5()
            month5<-as.numeric(as.character(month5))
            
            month6<-month6()
            month6<-as.numeric(as.character(month6))
            
            month7<-month7()
            month7<-as.numeric(as.character(month7))
            
            month8<-month8()
            month8<-as.numeric(as.character(month8))
            
            month9<-month9()
            month9<-as.numeric(as.character(month9))
            
            month10<-month10()
            month10<-as.numeric(as.character(month10))
            
            month11<-month11()
            month11<-as.numeric(as.character(month11))
            
            month12<-month12()
            month12<-as.numeric(as.character(month12))
            
            
            
            
            paste0('This analysis assessed the impact of a vaccination campaign conducted in ', input$country, ' among ',  input$populationsize, ' ', input$targetgroup, ', with a ', round((sum(vaccinecoveragemonthly, month2, month3, month4, month5, month6, month7, month8, month9,
                                                                                                                                                                    month10, month11, month12)*100), 0),'% vaccine coverage and ', round((vaccineeffectiveness*100), 0), '% vaccine effectiveness.', ' The impact of the vaccination campaign is estimated against a hypothetical scenario where no influenza vaccination campaign was conducted.')
            
        }else{
            #input$vaccineeffectiveness<-as.numeric(as.character(input$vaccineeffectiveness))
            
            vaccineeffectiveness<-vaccineeffectiveness()
            vaccineeffectiveness<-as.numeric(as.character(vaccineeffectiveness))
            
            vaccinecoveragemonthly<-vaccinecoveragemonthly()
            vaccinecoveragemonthly<-as.numeric(as.character(vaccinecoveragemonthly))
            
            month2<-month2()
            month2<-as.numeric(as.character(month2))
            
            month3<-month3()
            month3<-as.numeric(as.character(month3))
            
            month4<-month4()
            month4<-as.numeric(as.character(month4))
            
            month5<-month5()
            month5<-as.numeric(as.character(month5))
            
            month6<-month6()
            month6<-as.numeric(as.character(month6))
            
            month7<-month7()
            month7<-as.numeric(as.character(month7))
            
            month8<-month8()
            month8<-as.numeric(as.character(month8))
            
            month9<-month9()
            month9<-as.numeric(as.character(month9))
            
            month10<-month10()
            month10<-as.numeric(as.character(month10))
            
            month11<-month11()
            month11<-as.numeric(as.character(month11))
            
            month12<-month12()
            month12<-as.numeric(as.character(month12))
            
            paste0('This analysis assessed the impact of a hypothetical influenza vaccination campaign in ', input$country, ' among ', input$populationsize, ' ', input$targetgroup, ', with a ', round((sum(vaccinecoveragemonthly, month2, month3, month4, month5, month6, month7, month8, month9,
                                                                                                                                                                                  month10, month11, month12)*100), 0),'% vaccine coverage and ', round((vaccineeffectiveness*100), 0), '% vaccine effectiveness.', ' The impact of the hypothetical vaccination campaign is estimated against the current scenario where no influenza vaccination campaign was conducted.')
        }
        
    )
    
    
    
    
    #Summary text for hospitalized infections
    
    
    
    output$text2 = renderText(
        
        
        if(is.na(input$vaccineeffectiveness) | is.na(input$hnh) | is.na(input$mai)){
            paste0(" ")
            
            
            
        }else if(input$scenario=="Impact of current influenza vaccination campaign"){
            
            burdenavertedresults<-burdenavertedresults()%>%
                as.data.frame()%>%
                mutate(indicator_results=as.numeric(indicator_results))%>%
                pivot_wider(names_from="indicator_results_titles", values_from="indicator_results")%>%
                as.data.frame()
            
            
            paste0(round(burdenavertedresults$hospinfwvacctot, 0),
                   ' influenza-associated hospitalizations occurred when the vaccination campaign was conducted; whereas, an estimated ', round(burdenavertedresults$hospinfwovactot, 0),
                   ' influenza-associated hospitalizations could have occurred if the vaccination campaign had not been conducted.
             The vaccination campaign prevented ', round(burdenavertedresults$hospaverted, 0), ' (', round((burdenavertedresults$nonhospPreventedfraction*100), 0), '%) influenza-associated hospitalizations.
             On average, ', round(burdenavertedresults$NNVhospinffinal, 0), ' people in the target population would have needed to be vaccinated to prevent one influenza-associated hospitalization.'
            )
            
            
        } else{
            
            burdenavertedresults<-burdenavertedresults()%>%
                as.data.frame()%>%
                mutate(indicator_results=as.numeric(indicator_results))%>%
                pivot_wider(names_from="indicator_results_titles", values_from="indicator_results")%>%
                as.data.frame()
            
            
            paste0(round(burdenavertedresults$hospinfwovactot, 0),
                   ' influenza-associated hospitalizations occured in the absence of a vaccination campaign; whereas, ', round(burdenavertedresults$hospinfwvacctot, 0),
                   ' influenza-associated hospitalizations were estimated to occur if a vaccination campaign had been conducted, resulting in ', round(burdenavertedresults$hospaverted, 0), '
             (', round((burdenavertedresults$nonhospPreventedfraction*100), 0), '%) influenza-associated hospitalizations prevented.
             On average, ', round(burdenavertedresults$NNVhospinffinal, 0), ' people in the target population would have needed to be vaccinated to prevent one influenza-associated hospitalization.'
            )
            
        }#close if
        
        
    )
    
    
    
    ##Summary text for non-hospitalized infections
    
    output$text3 = renderText(
        
        
        if(is.na(input$vaccineeffectiveness) | is.na(input$hnh) | is.na(input$mai)){
            paste0(" ")
            
            
        }else if(input$scenario=="Impact of current influenza vaccination campaign"){
            
            burdenavertedresults<-burdenavertedresults()%>%
                as.data.frame()%>%
                mutate(indicator_results=as.numeric(indicator_results))%>%
                pivot_wider(names_from="indicator_results_titles", values_from="indicator_results")%>%
                as.data.frame()
            
            ##########################################################################################
            # Scenario of influenza circulation in presence of vaccine: Presence to absence of vaccine
            # This is a situation where the country conducts a vaccination campaign, we enter
            # actual burden inputs and vaccine coverage then compare this to a hypothetical scenario
            # where there is no vaccination campaign conducted
            ##########################################################################################
            
            
            paste0( round(burdenavertedresults$nonhospinfwvacctot, 0),
                    ' non-hospitalized influenza infections occurred when the vaccination campaign was conducted; whereas, ', round(burdenavertedresults$nonhospinfwovactot, 0),
                    ' non-hospitalized influenza infections were estimated to occur if the vaccination campaign had not been conducted.
              The vaccination campaign prevented ', round(burdenavertedresults$nonhospinfaverted, 0), ' (', round((burdenavertedresults$nonhospPreventedfraction*100), 0), '%) non-hospitalized influenza infections.
             On average, ', round(burdenavertedresults$NNVnonhospinffinal, 0), ' people in the target population would have needed to be vaccinated to prevent one non-hospitalized influenza infection.'
                    
            )
            
            
        }else{
            
            
            burdenavertedresults<-burdenavertedresults()%>%
                as.data.frame()%>%
                mutate(indicator_results=as.numeric(indicator_results))%>%
                pivot_wider(names_from="indicator_results_titles", values_from="indicator_results")%>%
                as.data.frame()
            
            
            paste0(round(burdenavertedresults$nonhospinfwovactot, 0),
                   ' non-hospitalized influenza infections occured in the absence of a vaccination campaign; whereas, ', round(burdenavertedresults$nonhospinfwvacctot, 0),
                   ' non-hospitalized influenza infections were estimated to occur if a vaccination campaign had been conducted, resulting in ', round(burdenavertedresults$nonhospinfaverted, 0),
                   '(', round((burdenavertedresults$nonhospPreventedfraction*100), 0), '%) non-hospitalized influenza infections prevented.
             On average, ', round(burdenavertedresults$NNVnonhospinffinal, 0), ' people in the target population would have needed to be vaccinated to prevent one non-hospitalized influenza infection.'
            )
            
        }#close if hypotetical non-hosp
        
        
        
    ) #close rendertext
    
    
    
    
    #Summary text for medical attendance
    
    output$text4 = renderText(
        
        
        if(is.na(input$vaccineeffectiveness) | is.na(input$hnh) | is.na(input$mai)){
            paste0(" ")
            
        }else if(input$scenario=="Impact of current influenza vaccination campaign"){
            
            burdenavertedresults<-burdenavertedresults()%>%
                as.data.frame()%>%
                mutate(indicator_results=as.numeric(indicator_results))%>%
                pivot_wider(names_from="indicator_results_titles", values_from="indicator_results")%>%
                as.data.frame()
            
            
            paste0(round(burdenavertedresults$medicallyattendedinfwvactot, 0),
                   ' influenza-associated medical attendances occurred when the vaccination campaign was conducted; whereas, ', round(burdenavertedresults$medicallyattendedinfwovactot, 0),
                   ' influenza-associated medical attendances were estimated to occur if the vaccination campaign had not been conducted.
             The vaccination campaign prevented ', round(burdenavertedresults$medicallyattendedinfaverted, 0), ' (', round((burdenavertedresults$nonhospPreventedfraction*100), 0), '%) influenza-associated medical attendances.
             On average, ', round(burdenavertedresults$NNVmedicallyattendtedfinal, 0), ' people in the target population would have needed to be vaccinated to prevent one influenza-associated medical attendance.'
                   
            )
            
        }#close if
        
        else
            
        {
            
            burdenavertedresults<-burdenavertedresults()%>%
                as.data.frame()%>%
                mutate(indicator_results=as.numeric(indicator_results))%>%
                pivot_wider(names_from="indicator_results_titles", values_from="indicator_results")%>%
                as.data.frame()
            
            
            paste0(round(burdenavertedresults$medicallyattendedinfwovactot, 0),
                   ' influenza-associated medical attendances occured in the absence of a vaccination campaign; whereas, ', round(burdenavertedresults$medicallyattendedinfwvactot, 0),
                   ' influenza-associated medical attendances were estimated to occur if a vaccination campaign had been conducted, resulting in ', round(burdenavertedresults$medicallyattendedinfaverted, 0),
                   '(', round((burdenavertedresults$nonhospPreventedfraction*100), 0), '%) influenza-associated medical attendances prevented.
             On average, ', round(burdenavertedresults$NNVmedicallyattendtedfinal, 0), ' people in the target population would have needed to be vaccinated to prevent one influenza-associated medical attendance.'
            )
            
        }
    ) #close rendertext
    
    
    
    
    
    #Summary text for total infections
    
    output$text5 = renderText(
        
        
        if(is.na(input$vaccineeffectiveness) | is.na(input$hnh) | is.na(input$mai)){
            paste0(" ")
            
        }else if(input$scenario=="Impact of current influenza vaccination campaign"){
            
            burdenavertedresults<-burdenavertedresults()%>%
                as.data.frame()%>%
                mutate(indicator_results=as.numeric(indicator_results))%>%
                pivot_wider(names_from="indicator_results_titles", values_from="indicator_results")%>%
                as.data.frame()
            
            
            paste0('A total of, ', round(burdenavertedresults$Totalinfwvacc, 0),
                   ' influenza-associated infections (hospitalized and non-hospitalized) occurred when the vaccination campaign was conducted; whereas, ', round(burdenavertedresults$Totalinfwovacc, 0),
                   ' influenza-associated infections were estimated to occur if the vaccination campaign had not been conducted.
             The vaccination campaign prevented a total of ', round(burdenavertedresults$Totalinfaverted, 0), ' (', round((burdenavertedresults$nonhospPreventedfraction*100), 0), '%) influenza-associated infections.
             On average, ', round(burdenavertedresults$NNVtotalinf, 0), ' people in the target population would have needed to be vaccinated to prevent one influenza-associated infection.')
            
        }#close if
        
        else
            
        {
            
            burdenavertedresults<-burdenavertedresults()%>%
                as.data.frame()%>%
                mutate(indicator_results=as.numeric(indicator_results))%>%
                pivot_wider(names_from="indicator_results_titles", values_from="indicator_results")%>%
                as.data.frame()
            
            
            paste0('Overall ', round(burdenavertedresults$Totalinfwovacc, 0),
                   ' influenza-associated infections (hospitalized and non-hospitalized) occured in the absence of a vaccination campaign; whereas ', round(burdenavertedresults$Totalinfwvacc, 0),
                   ' influenza infections were estimated to occur if a vaccination campaign had been conducted, resulting in a total of ', round(burdenavertedresults$Totalinfaverted, 0),
                   '(', round((burdenavertedresults$nonhospPreventedfraction*100), 0), '%) influenza infections prevented.
             On average, ', round(burdenavertedresults$NNVtotalinf, 0), ' people in the target population would have needed to be vaccinated to prevent one influenza infection.'
            )
            
        }
    ) #close rendertext
    
    
    
    
    
############################################################################
############################################################################
 
       # Scenario Planning - COVERAGE
    
############################################################################
############################################################################

    
##1 - Generate results from coverage 1-4, extract the column of results from the burdenavertedresultscoverage table and join it onto base results

    
    #######################################################################################################################################################################################
    
    ##Deployment coverage 1
    #######################################################################################################################################################################################
    
    
    burdendataset_coverage1<- reactive({
        ################################ The output data when insufficient data has been entered   ##################################
        ############################################################################################################################
        
        
        if(scenario()=="Impact of current influenza vaccination campaign"){
            
            
            ##########################################################################################
            # Scenario of influenza circulation in presence of vaccine: Presence to absence of vaccine
            # This is a situation where the country conducts a vaccination campaign, we enter
            # actual burden inputs and vaccine coverage then compare this to a hypothetical scenario
            # where there is no vaccination campaign conducted
            ##########################################################################################
            
            
            
            
            hospinf<-c(hospitalizationsmonthly(), hospmonth2(), hospmonth3(), hospmonth4(), hospmonth5(), hospmonth6(),
                       hospmonth7(), hospmonth8(), hospmonth9(), hospmonth10(), hospmonth11(), hospmonth12())
            
            populationsize<-as.numeric(as.character(populationsize()))
            vaccineeffectiveness<-as.numeric(as.character(vaccineeffectiveness()))
            hnh<-as.numeric(as.character(hnh()))
            mai<-as.numeric(as.character(mai()))
            hospinf<-as.numeric(as.character(hospinf))
            
            novaccine<-hospinf%>%
                as.data.frame()%>%
                rename(hospinfwvac=1)%>%
                mutate(hospinfcumulwvac=cumsum(hospinfwvac))%>%
                mutate(nonhospinfwvac=hospinfwvac*hnh)%>%
                mutate(nonhospinfcumulwvac=cumsum(nonhospinfwvac))%>%
                mutate(overallinfwvac=hospinfwvac+nonhospinfwvac)%>%
                mutate(overallinfcumulwvac=cumsum(overallinfwvac))%>%
                mutate(medicalattedtoinfwvac=overallinfwvac*mai)%>%
                mutate(medicallyattendedinfcumulwvac=cumsum(medicalattedtoinfwvac))
            
            
            
            ##bind monthly coverage
            monthvaccov<-c(vaccinecoveragemonthly(), month2(), month3(), month4(), month5(), month6(),
                           month7(), month8(), month9(), month10(), month11(), month12())
            
            monthvaccov<-as.numeric(as.character(monthvaccov))
            
            totcoverage<-sum(monthvaccov)
            
            
            novaccine<-cbind(novaccine, monthvaccov)
            
            novaccine$covergdistrib<-novaccine$monthvaccov/totcoverage
            
            novaccine$monthvaccov<- novaccine$covergdistrib*coverage1()
            
            
            ## month number
            novaccine$monthnum<-c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
            
            ## add monthly effectively vaccinated
            novaccine<-novaccine%>%
                mutate(effectivelyvacc=populationsize*monthvaccov*vaccineeffectiveness)%>%
                mutate(effectivelyvacccumul=cumsum(effectivelyvacc))
            
            
            
            
            #####################################################################
            # Estimated rates
            #####################################################################
            novaccine<-novaccine%>%
                mutate(susceptiblewvacc=populationsize-hospinfcumulwvac-nonhospinfcumulwvac-effectivelyvacccumul)
            
            #Create columns
            novaccine$irhospinf<-NA
            novaccine$irnonhospinf<-NA
            
            #Create first row
            novaccine[1,14]<-novaccine$hospinfwvac[1]/populationsize
            novaccine[1,15]<-novaccine$nonhospinfwvac[1]/populationsize
            
            
            ##Calculation uses last month's susceptible population
            for(i in 2:nrow(novaccine)){
                novaccine$irhospinf[i]<-novaccine$hospinfwvac[i]/novaccine$susceptiblewvacc[i-1]
                novaccine$irnonhospinf[i]<- novaccine$nonhospinfwvac[i]/novaccine$susceptiblewvacc[i-1]
            }
            
            #####################################################################
            # Scenario of influenza circulation in absence of vaccine
            #####################################################################
            
            ## These calculations are initialized by the 'Susceptible at start of month in absence of vaccine'
            ## column, which starts with the Target Population in the first month.
            
            remainsuscstartmonth<-c(populationsize, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
            novaccine<-cbind(novaccine, remainsuscstartmonth)
            
            #Create columns
            novaccine$hospinfwovac<-NA
            novaccine$hospinfcumulwovacc<-NA
            novaccine$nonhospinfwovac<-NA
            novaccine$nonhospinfcumulwovacc<-NA
            novaccine$remainingsusc<-NA
            
            #Create first row
            novaccine[1,17]<-novaccine$remainsuscstartmonth[1]*novaccine$irhospinf[1]
            novaccine[1,18]<-novaccine$hospinfwovac[1]
            novaccine[1,19]<-novaccine$remainsuscstartmonth[1]*novaccine$irnonhospinf[1]
            novaccine[1,20]<-novaccine$nonhospinfwovac[1]
            novaccine[1,21]<-populationsize-novaccine$hospinfcumulwovacc[1]-novaccine$nonhospinfcumulwovacc[1]
            
            
            #Create rows 2:12
            for(i in 2:nrow(novaccine)){
                novaccine$remainsuscstartmonth[i]<-novaccine$remainingsusc[i-1]
                novaccine$hospinfwovac[i]<-novaccine$remainsuscstartmonth[i]*novaccine$irhospinf[i]
                novaccine$hospinfcumulwovacc[i]<-sum(novaccine$hospinfwovac[1:i])
                novaccine$nonhospinfwovac[i]<-novaccine$remainsuscstartmonth[i]*novaccine$irnonhospinf[i]
                novaccine$nonhospinfcumulwovacc[i]<-sum(novaccine$nonhospinfwovac[1:i])
                novaccine$remainingsusc[i]<-populationsize-novaccine$hospinfcumulwovacc[i]-novaccine$nonhospinfcumulwovacc[i]
            }
            
            novaccine<-novaccine%>%
                mutate(overallinfwovac=hospinfwovac+nonhospinfwovac)%>%
                mutate(overallinfcumulwovacc=cumsum(overallinfwovac))%>%
                mutate(medicallyattendedinfwovac=overallinfwovac*mai)%>%
                mutate(medicalattedtoinfcumulwovacc=cumsum(medicallyattendedinfwovac))
            
            ######## Avoided events
            novaccine<-novaccine%>%
                mutate(AEhospinf=hospinfwovac-hospinfwvac)%>%
                mutate(AEnonhospinf=nonhospinfwovac-nonhospinfwvac)%>%
                mutate(AEmedicalattedtoinf=medicallyattendedinfwovac-medicalattedtoinfwvac)%>%
                mutate(AEoverallinf=overallinfwovac-overallinfwvac)
            
            
            ################ Prevented fraction
            ## Hospitalized infections
            ifelse (monthstartvac()==1,
                    novaccine<-novaccine%>%
                        mutate(PFhospinf=sum(AEhospinf)/sum(hospinfwovac)),
                    novaccine<-novaccine%>%
                        mutate(PFhospinf=sum(AEhospinf[monthstartvac():nrow(novaccine)]/sum(hospinfwovac[monthstartvac():nrow(novaccine)])))
            )
            
            ## non-Hospitalized infections
            ifelse (monthstartvac()==1,
                    novaccine<-novaccine%>%
                        mutate(PFnonhospinf=sum(AEnonhospinf)/sum(nonhospinfwovac)),
                    novaccine<-novaccine%>%
                        mutate(PFnonhospinf=sum(AEnonhospinf[monthstartvac():nrow(novaccine)]/sum(nonhospinfwovac[monthstartvac():nrow(novaccine)])))
            )
            
            
            ################ NNV -----------------------------------> why does this only go to month 8 in the Excel formula?????
            ## Hospitalized infections
            ifelse(monthstartvac()==1,
                   novaccine<-novaccine%>%
                       mutate(NNVhospinf=1/((vaccineeffectiveness*(sum(hospinfwovac)))/populationsize)),
                   novaccine<-novaccine%>%
                       mutate(NNVhospinf=1/((vaccineeffectiveness*(sum(hospinfwovac[monthstartvac():nrow(novaccine)])))/populationsize))
            )
            
            
            ## non-Hospitalized infections
            ifelse(monthstartvac()==1,
                   novaccine<-novaccine%>%
                       mutate(NNVnonhospinf=1/((vaccineeffectiveness*(sum(nonhospinfwovac)))/populationsize)),
                   novaccine<-novaccine%>%
                       mutate(NNVnonhospinf=1/((vaccineeffectiveness*(sum(nonhospinfwovac[monthstartvac():nrow(novaccine)])))/populationsize))
            )
            
            ## Medically infections
            ifelse(monthstartvac()==1,
                   novaccine<-novaccine%>%
                       mutate(NNVmedicalattendedinf=1/((vaccineeffectiveness*(sum(medicallyattendedinfwovac)))/populationsize)),
                   novaccine<-novaccine%>%
                       mutate(NNVmedicalattendedinf=1/((vaccineeffectiveness*(sum(medicallyattendedinfwovac[monthstartvac():nrow(novaccine)])))/populationsize))
            )
            
            ## Overall infections
            ifelse(monthstartvac()==1,
                   novaccine<-novaccine%>%
                       mutate(NNVoverallinf=1/((vaccineeffectiveness*(sum(hospinfwovac)+sum(nonhospinfwovac)))/populationsize)),
                   novaccine<-novaccine%>%
                       mutate(NNVoverallinf=1/((vaccineeffectiveness*(sum(hospinfwovac[monthstartvac():nrow(novaccine)])+sum(nonhospinfwovac[monthstartvac():nrow(novaccine)])))/populationsize))
            )
            
            return(novaccine)
            
            
            
        } #close if current campaign
        
        
        
        if(scenario()=="Impact of hypothetical vaccination campaign"){
            
            ##########################################################################################
            # Scenario of influenza circulation in absence of vaccine: Absence to presence of vaccine
            # This is a scenario where the country has yet to conduct a vaccination campaign but would
            # like to see what impact a vaccine campaign may have.
            # The country enters the estimated number of hospitalization they observed and then we compare
            # this to a hypothetical vaccination campaign
            ##########################################################################################
            
            hospinf<-c(hospitalizationsmonthly(), hospmonth2(), hospmonth3(), hospmonth4(), hospmonth5(), hospmonth6(),
                       hospmonth7(), hospmonth8(), hospmonth9(), hospmonth10(), hospmonth11(), hospmonth12())
            
            populationsize<-as.numeric(as.character(populationsize()))
            hnh<-as.numeric(as.character(hnh()))
            mai<-as.numeric(as.character(mai()))
            hospinf<-as.numeric(as.character(hospinf))
            vaccineeffectiveness<-as.numeric(as.character(vaccineeffectiveness()))
            
            #####################Observed influenza in the absence of vaccine
            novaccine<-hospinf%>%
                as.data.frame()%>%
                rename(hospinfwovac=1)%>%
                mutate(hospinfcumulwovac=cumsum(hospinfwovac))%>%
                mutate(nonhospinfwovac=hospinfwovac*hnh)%>%
                mutate(nonhospinfcumulwovac=cumsum(nonhospinfwovac))%>%
                mutate(remainingsusc=populationsize-hospinfcumulwovac-nonhospinfcumulwovac)
            
            #create remainsuscstartmonth column with target population and rows 1:11 of remainingsusc
            remainsuscstartmonth<-populationsize%>%
                as.data.frame()%>%
                rename(remainsuscstartmonth=1)
            
            remainsuscstartmonth2<-as.data.frame(novaccine$remainingsusc[1:11])%>%
                as.data.frame()%>%
                rename(remainsuscstartmonth=1)
            
            remainsuscstartmonth<-rbind(remainsuscstartmonth, remainsuscstartmonth2)
            
            novaccine<-cbind(novaccine, remainsuscstartmonth)
            
            novaccine<-novaccine%>%
                mutate(overallinfwovac=hospinfwovac+nonhospinfwovac)%>%
                mutate(overallinfcumulwovac=cumsum(overallinfwovac))%>%
                mutate(medicallyattendedinfwovac=overallinfwovac*mai)%>%
                mutate(medicallyattendedinfcumulwovac=cumsum(medicallyattendedinfwovac))
            
            
            
            #####################################################################
            # Incidence rates in the absence of vaccine
            #####################################################################
            novaccine<-novaccine%>%
                mutate(irhospinf=hospinfwovac/remainsuscstartmonth)%>%
                mutate(irnonhospinf=nonhospinfwovac/remainsuscstartmonth)
            
            
            
            
            ############################ Burden inputs with hypothetical vaccination campaign
            #Create columns
            novaccine$hospinfwvac<-NA
            novaccine$hospinfcumulwvac<-NA
            novaccine$nonhospinfwvac<-NA
            novaccine$nonhospinfcumulwvac<-NA
            
            
            
            #Create first row
            novaccine[1,13]<-populationsize*novaccine$irhospinf[1]
            novaccine[1,14]<-novaccine$hospinfwvac[1]
            novaccine[1,15]<-populationsize*novaccine$irnonhospinf[1]
            novaccine[1,16]<-novaccine$nonhospinfwvac[1]
            
            ##bind monthly coverage
            monthvaccov<-c(vaccinecoveragemonthly(), month2(), month3(), month4(), month5(), month6(),
                           month7(), month8(), month9(), month10(), month11(), month12())
            
            monthvaccov<-as.numeric(as.character(monthvaccov))
            
            totcoverage<-sum(monthvaccov)
            
            
            novaccine<-cbind(novaccine, monthvaccov)
            
            novaccine$covergdistrib<-novaccine$monthvaccov/totcoverage
            
            novaccine$monthvaccov<- novaccine$covergdistrib*coverage1()
            
            
            ##monthly effectively vaccinated and susceptible with hypothetical vaccination campaign
            novaccine<-novaccine%>%
                mutate(effectivelyvacc=populationsize*monthvaccov*vaccineeffectiveness)%>%
                mutate(effectivelycumulvacc=cumsum(effectivelyvacc))%>%
                mutate(suscwvacc=populationsize-hospinfcumulwvac-nonhospinfcumulwvac-effectivelyvacc)
            
            ##Then calculate the rest of hospinfwovac and nonhospinfwovac. Calculation uses last month's incidence rate
            for(i in 2:nrow(novaccine)){
                novaccine$hospinfwvac[i]<-novaccine$suscwvacc[i-1]*novaccine$irhospinf[i]
                novaccine$hospinfcumulwvac[i]<-sum(novaccine$hospinfwvac[1:i])
                novaccine$nonhospinfwvac[i]<- novaccine$suscwvacc[i-1]*novaccine$irnonhospinf[i]
                novaccine$nonhospinfcumulwvac[i]<-sum(novaccine$nonhospinfwvac[1:i])
                novaccine$suscwvacc[i]=populationsize-novaccine$hospinfcumulwvac[i]-novaccine$nonhospinfcumulwvac[i]-novaccine$effectivelycumulvacc[i]
            }
            
            
            ## month number
            novaccine$monthnum<-c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
            
            
            ## Overall infections with hypothetical vaccination campaign
            novaccine<-novaccine%>%
                mutate(overallinfwvac=hospinfwvac+nonhospinfwvac)%>%
                mutate(overallinfcumulwvacc=cumsum(overallinfwvac))
            
            ## Medically attended with hypothetical vaccination campaign
            novaccine<-novaccine%>%
                mutate(medicalattedtoinfwvac=overallinfwvac*mai)
            
            
            ######## Avoided events
            novaccine<-novaccine%>%
                mutate(AEhospinf=hospinfwovac-hospinfwvac)%>%
                mutate(AEnonhospinf=nonhospinfwovac-nonhospinfwvac)%>%
                mutate(AEmedicalattedtoinf=medicallyattendedinfwovac-medicalattedtoinfwvac)%>%
                mutate(AEoverallinf=overallinfwovac-overallinfwvac)
            
            
            
            ################ Prevented fraction
            ## Hospitalized infections
            ifelse (monthstartvac()==1,
                    novaccine<-novaccine%>%
                        mutate(PFhospinf=sum(AEhospinf)/sum(hospinfwovac)),
                    novaccine<-novaccine%>%
                        mutate(PFhospinf=sum(AEhospinf[monthstartvac():nrow(novaccine)]/sum(hospinfwovac[monthstartvac():nrow(novaccine)])))
            )
            
            ## non-Hospitalized infections
            ifelse (monthstartvac()==1,
                    novaccine<-novaccine%>%
                        mutate(PFnonhospinf=sum(AEnonhospinf)/sum(nonhospinfwovac)),
                    novaccine<-novaccine%>%
                        mutate(PFnonhospinf=sum(AEnonhospinf[monthstartvac():nrow(novaccine)]/sum(nonhospinfwovac[monthstartvac():nrow(novaccine)])))
            )
            
            
            
            
            ################ NNV -----------------------------------> why does this only go to month 8 in the Excel formula?????
            ## Hospitalized infections
            ifelse(monthstartvac()==1,
                   novaccine<-novaccine%>%
                       mutate(NNVhospinf=1/((vaccineeffectiveness*(sum(hospinfwovac)))/populationsize)),
                   novaccine<-novaccine%>%
                       mutate(NNVhospinf=1/((vaccineeffectiveness*(sum(hospinfwovac[monthstartvac():nrow(novaccine)])))/populationsize))
            )
            
            
            ## non-Hospitalized infections
            ifelse(monthstartvac()==1,
                   novaccine<-novaccine%>%
                       mutate(NNVnonhospinf=1/((vaccineeffectiveness*(sum(nonhospinfwovac)))/populationsize)),
                   novaccine<-novaccine%>%
                       mutate(NNVnonhospinf=1/((vaccineeffectiveness*(sum(nonhospinfwovac[monthstartvac():nrow(novaccine)])))/populationsize))
            )
            
            ## Medically attended infections
            ifelse(monthstartvac()==1,
                   novaccine<-novaccine%>%
                       mutate(NNVmedicalattendedinf=1/((vaccineeffectiveness*(sum(medicallyattendedinfwovac)))/populationsize)),
                   novaccine<-novaccine%>%
                       mutate(NNVmedicalattendedinf=1/((vaccineeffectiveness*(sum(medicallyattendedinfwovac[monthstartvac():nrow(novaccine)])))/populationsize))
            )
            
            ## Total infections
            ifelse(monthstartvac()==1,
                   novaccine<-novaccine%>%
                       mutate(NNVoverallinf=1/((vaccineeffectiveness*(sum(hospinfwovac)+sum(nonhospinfwovac)))/populationsize)),
                   novaccine<-novaccine%>%
                       mutate(NNVoverallinf=1/((vaccineeffectiveness*(sum(hospinfwovac[monthstartvac():nrow(novaccine)])+sum(nonhospinfwovac[monthstartvac():nrow(novaccine)])))/populationsize))
            )
            
            return(novaccine)
            
            
        }#close "if hypothetical campaign"
        
    }) #close burdendataset
    
    
    
    ####################################################################################################
    ## List of burden averted results
    ####################################################################################################
    
    
    burdenavertedresultscoverage1<-reactive({
        
        populationsize<-as.numeric(as.character(populationsize()))
        hnh<-as.numeric(as.character(hnh()))
        mai<-as.numeric(as.character(mai()))
        vaccineeffectiveness<-as.numeric(as.character(vaccineeffectiveness()))
        
       
        
        #################################################
        ## Results
        #################################################
        nonhospinfwovactot<-sum(burdendataset_coverage1()$nonhospinfwovac)
        nonhospinfwvacctot<-sum(burdendataset_coverage1()$nonhospinfwvac)
        nonhospinfaverted<-nonhospinfwovactot-nonhospinfwvacctot
        NNVnonhospinffinal<-burdendataset_coverage1()$NNVnonhospinf[1]
        
        medicallyattendedinfwovactot<-sum(burdendataset_coverage1()$medicallyattendedinfwovac)
        medicallyattendedinfwvactot<-sum(burdendataset_coverage1()$medicalattedtoinfwvac)
        medicallyattendedinfaverted<-medicallyattendedinfwovactot-medicallyattendedinfwvactot
        NNVmedicallyattendtedfinal<-burdendataset_coverage1()$NNVmedicalattendedinf[1]
        
        Totalinfwovacc<-sum(burdendataset_coverage1()$overallinfwovac)
        Totalinfwvacc<-sum(burdendataset_coverage1()$overallinfwvac)
        Totalinfaverted<-Totalinfwovacc-Totalinfwvacc
        NNVtotalinf<-burdendataset_coverage1()$NNVoverallinf[1]
        
        hospinfwovactot<-sum(burdendataset_coverage1()$hospinfwovac)
        hospinfwvacctot<-sum(burdendataset_coverage1()$hospinfwvac)
        hospaverted<-hospinfwovactot-hospinfwvacctot
        NNVhospinffinal<-burdendataset_coverage1()$NNVhospinf[1]
        
        
        nonhospPreventedfraction<-burdendataset_coverage1()$PFnonhospinf[1]
        hospPreventedfraction<-burdendataset_coverage1()$PFhospinf[1]
        
        
        indicator_results1<-c(nonhospinfwovactot, nonhospinfwvacctot, nonhospinfaverted, NNVnonhospinffinal,
                             Totalinfwovacc, Totalinfwvacc, Totalinfaverted, NNVtotalinf,
                             hospinfwovactot, hospinfwvacctot, hospaverted, NNVhospinffinal,
                             medicallyattendedinfwovactot, medicallyattendedinfwvactot, medicallyattendedinfaverted, NNVmedicallyattendtedfinal,
                             nonhospPreventedfraction)
        
        indicator_results_titles<-c("nonhospinfwovactot", "nonhospinfwvacctot", "nonhospinfaverted", "NNVnonhospinffinal",
                                    "Totalinfwovacc", "Totalinfwvacc", "Totalinfaverted", "NNVtotalinf",
                                    "hospinfwovactot", "hospinfwvacctot", "hospaverted", "NNVhospinffinal",
                                    "medicallyattendedinfwovactot", "medicallyattendedinfwvactot", "medicallyattendedinfaverted", "NNVmedicallyattendtedfinal",
                                    "nonhospPreventedfraction")
        
        indicator_results1<-cbind(indicator_results_titles, indicator_results1)
        
        
        return(indicator_results1)
        
        
    })#burdenavertedresults_coverage1
    
    
    
    #######################################################################################################################################################################################
    
    ##Deployment coverage 2
    #######################################################################################################################################################################################
    
    
    burdendataset_coverage2<- reactive({
        ################################ The output data when insufficient data has been entered   ##################################
        ############################################################################################################################
        
        
        if(scenario()=="Impact of current influenza vaccination campaign"){
            
            
            ##########################################################################################
            # Scenario of influenza circulation in presence of vaccine: Presence to absence of vaccine
            # This is a situation where the country conducts a vaccination campaign, we enter
            # actual burden inputs and vaccine coverage then compare this to a hypothetical scenario
            # where there is no vaccination campaign conducted
            ##########################################################################################
            
            
            
            
            hospinf<-c(hospitalizationsmonthly(), hospmonth2(), hospmonth3(), hospmonth4(), hospmonth5(), hospmonth6(),
                       hospmonth7(), hospmonth8(), hospmonth9(), hospmonth10(), hospmonth11(), hospmonth12())
            
            populationsize<-as.numeric(as.character(populationsize()))
            vaccineeffectiveness<-as.numeric(as.character(vaccineeffectiveness()))
            hnh<-as.numeric(as.character(hnh()))
            mai<-as.numeric(as.character(mai()))
            hospinf<-as.numeric(as.character(hospinf))
            
            novaccine<-hospinf%>%
                as.data.frame()%>%
                rename(hospinfwvac=1)%>%
                mutate(hospinfcumulwvac=cumsum(hospinfwvac))%>%
                mutate(nonhospinfwvac=hospinfwvac*hnh)%>%
                mutate(nonhospinfcumulwvac=cumsum(nonhospinfwvac))%>%
                mutate(overallinfwvac=hospinfwvac+nonhospinfwvac)%>%
                mutate(overallinfcumulwvac=cumsum(overallinfwvac))%>%
                mutate(medicalattedtoinfwvac=overallinfwvac*mai)%>%
                mutate(medicallyattendedinfcumulwvac=cumsum(medicalattedtoinfwvac))
            
            
            
            ##bind monthly coverage
            monthvaccov<-c(vaccinecoveragemonthly(), month2(), month3(), month4(), month5(), month6(),
                           month7(), month8(), month9(), month10(), month11(), month12())
            
            monthvaccov<-as.numeric(as.character(monthvaccov))
            
            totcoverage<-sum(monthvaccov)
            
            
            novaccine<-cbind(novaccine, monthvaccov)
            
            novaccine$covergdistrib<-novaccine$monthvaccov/totcoverage
            
            novaccine$monthvaccov<- novaccine$covergdistrib*coverage2()
            
            
            ## month number
            novaccine$monthnum<-c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
            
            ## add monthly effectively vaccinated
            novaccine<-novaccine%>%
                mutate(effectivelyvacc=populationsize*monthvaccov*vaccineeffectiveness)%>%
                mutate(effectivelyvacccumul=cumsum(effectivelyvacc))
            
            
            
            
            #####################################################################
            # Estimated rates
            #####################################################################
            novaccine<-novaccine%>%
                mutate(susceptiblewvacc=populationsize-hospinfcumulwvac-nonhospinfcumulwvac-effectivelyvacccumul)
            
            #Create columns
            novaccine$irhospinf<-NA
            novaccine$irnonhospinf<-NA
            
            #Create first row
            novaccine[1,14]<-novaccine$hospinfwvac[1]/populationsize
            novaccine[1,15]<-novaccine$nonhospinfwvac[1]/populationsize
            
            
            ##Calculation uses last month's susceptible population
            for(i in 2:nrow(novaccine)){
                novaccine$irhospinf[i]<-novaccine$hospinfwvac[i]/novaccine$susceptiblewvacc[i-1]
                novaccine$irnonhospinf[i]<- novaccine$nonhospinfwvac[i]/novaccine$susceptiblewvacc[i-1]
            }
            
            #####################################################################
            # Scenario of influenza circulation in absence of vaccine
            #####################################################################
            
            ## These calculations are initialized by the 'Susceptible at start of month in absence of vaccine'
            ## column, which starts with the Target Population in the first month.
            
            remainsuscstartmonth<-c(populationsize, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
            novaccine<-cbind(novaccine, remainsuscstartmonth)
            
            #Create columns
            novaccine$hospinfwovac<-NA
            novaccine$hospinfcumulwovacc<-NA
            novaccine$nonhospinfwovac<-NA
            novaccine$nonhospinfcumulwovacc<-NA
            novaccine$remainingsusc<-NA
            
            #Create first row
            novaccine[1,17]<-novaccine$remainsuscstartmonth[1]*novaccine$irhospinf[1]
            novaccine[1,18]<-novaccine$hospinfwovac[1]
            novaccine[1,19]<-novaccine$remainsuscstartmonth[1]*novaccine$irnonhospinf[1]
            novaccine[1,20]<-novaccine$nonhospinfwovac[1]
            novaccine[1,21]<-populationsize-novaccine$hospinfcumulwovacc[1]-novaccine$nonhospinfcumulwovacc[1]
            
            
            #Create rows 2:12
            for(i in 2:nrow(novaccine)){
                novaccine$remainsuscstartmonth[i]<-novaccine$remainingsusc[i-1]
                novaccine$hospinfwovac[i]<-novaccine$remainsuscstartmonth[i]*novaccine$irhospinf[i]
                novaccine$hospinfcumulwovacc[i]<-sum(novaccine$hospinfwovac[1:i])
                novaccine$nonhospinfwovac[i]<-novaccine$remainsuscstartmonth[i]*novaccine$irnonhospinf[i]
                novaccine$nonhospinfcumulwovacc[i]<-sum(novaccine$nonhospinfwovac[1:i])
                novaccine$remainingsusc[i]<-populationsize-novaccine$hospinfcumulwovacc[i]-novaccine$nonhospinfcumulwovacc[i]
            }
            
            novaccine<-novaccine%>%
                mutate(overallinfwovac=hospinfwovac+nonhospinfwovac)%>%
                mutate(overallinfcumulwovacc=cumsum(overallinfwovac))%>%
                mutate(medicallyattendedinfwovac=overallinfwovac*mai)%>%
                mutate(medicalattedtoinfcumulwovacc=cumsum(medicallyattendedinfwovac))
            
            ######## Avoided events
            novaccine<-novaccine%>%
                mutate(AEhospinf=hospinfwovac-hospinfwvac)%>%
                mutate(AEnonhospinf=nonhospinfwovac-nonhospinfwvac)%>%
                mutate(AEmedicalattedtoinf=medicallyattendedinfwovac-medicalattedtoinfwvac)%>%
                mutate(AEoverallinf=overallinfwovac-overallinfwvac)
            
            
            ################ Prevented fraction
            ## Hospitalized infections
            ifelse (monthstartvac()==1,
                    novaccine<-novaccine%>%
                        mutate(PFhospinf=sum(AEhospinf)/sum(hospinfwovac)),
                    novaccine<-novaccine%>%
                        mutate(PFhospinf=sum(AEhospinf[monthstartvac():nrow(novaccine)]/sum(hospinfwovac[monthstartvac():nrow(novaccine)])))
            )
            
            ## non-Hospitalized infections
            ifelse (monthstartvac()==1,
                    novaccine<-novaccine%>%
                        mutate(PFnonhospinf=sum(AEnonhospinf)/sum(nonhospinfwovac)),
                    novaccine<-novaccine%>%
                        mutate(PFnonhospinf=sum(AEnonhospinf[monthstartvac():nrow(novaccine)]/sum(nonhospinfwovac[monthstartvac():nrow(novaccine)])))
            )
            
            
            ################ NNV -----------------------------------> why does this only go to month 8 in the Excel formula?????
            ## Hospitalized infections
            ifelse(monthstartvac()==1,
                   novaccine<-novaccine%>%
                       mutate(NNVhospinf=1/((vaccineeffectiveness*(sum(hospinfwovac)))/populationsize)),
                   novaccine<-novaccine%>%
                       mutate(NNVhospinf=1/((vaccineeffectiveness*(sum(hospinfwovac[monthstartvac():nrow(novaccine)])))/populationsize))
            )
            
            
            ## non-Hospitalized infections
            ifelse(monthstartvac()==1,
                   novaccine<-novaccine%>%
                       mutate(NNVnonhospinf=1/((vaccineeffectiveness*(sum(nonhospinfwovac)))/populationsize)),
                   novaccine<-novaccine%>%
                       mutate(NNVnonhospinf=1/((vaccineeffectiveness*(sum(nonhospinfwovac[monthstartvac():nrow(novaccine)])))/populationsize))
            )
            
            ## Medically infections
            ifelse(monthstartvac()==1,
                   novaccine<-novaccine%>%
                       mutate(NNVmedicalattendedinf=1/((vaccineeffectiveness*(sum(medicallyattendedinfwovac)))/populationsize)),
                   novaccine<-novaccine%>%
                       mutate(NNVmedicalattendedinf=1/((vaccineeffectiveness*(sum(medicallyattendedinfwovac[monthstartvac():nrow(novaccine)])))/populationsize))
            )
            
            ## Overall infections
            ifelse(monthstartvac()==1,
                   novaccine<-novaccine%>%
                       mutate(NNVoverallinf=1/((vaccineeffectiveness*(sum(hospinfwovac)+sum(nonhospinfwovac)))/populationsize)),
                   novaccine<-novaccine%>%
                       mutate(NNVoverallinf=1/((vaccineeffectiveness*(sum(hospinfwovac[monthstartvac():nrow(novaccine)])+sum(nonhospinfwovac[monthstartvac():nrow(novaccine)])))/populationsize))
            )
            
            return(novaccine)
            
            
            
        } #close if current campaign
        
        
        
        if(scenario()=="Impact of hypothetical vaccination campaign"){
            
            ##########################################################################################
            # Scenario of influenza circulation in absence of vaccine: Absence to presence of vaccine
            # This is a scenario where the country has yet to conduct a vaccination campaign but would
            # like to see what impact a vaccine campaign may have.
            # The country enters the estimated number of hospitalization they observed and then we compare
            # this to a hypothetical vaccination campaign
            ##########################################################################################
            
            hospinf<-c(hospitalizationsmonthly(), hospmonth2(), hospmonth3(), hospmonth4(), hospmonth5(), hospmonth6(),
                       hospmonth7(), hospmonth8(), hospmonth9(), hospmonth10(), hospmonth11(), hospmonth12())
            
            populationsize<-as.numeric(as.character(populationsize()))
            hnh<-as.numeric(as.character(hnh()))
            mai<-as.numeric(as.character(mai()))
            hospinf<-as.numeric(as.character(hospinf))
            vaccineeffectiveness<-as.numeric(as.character(vaccineeffectiveness()))
            
            #####################Observed influenza in the absence of vaccine
            novaccine<-hospinf%>%
                as.data.frame()%>%
                rename(hospinfwovac=1)%>%
                mutate(hospinfcumulwovac=cumsum(hospinfwovac))%>%
                mutate(nonhospinfwovac=hospinfwovac*hnh)%>%
                mutate(nonhospinfcumulwovac=cumsum(nonhospinfwovac))%>%
                mutate(remainingsusc=populationsize-hospinfcumulwovac-nonhospinfcumulwovac)
            
            #create remainsuscstartmonth column with target population and rows 1:11 of remainingsusc
            remainsuscstartmonth<-populationsize%>%
                as.data.frame()%>%
                rename(remainsuscstartmonth=1)
            
            remainsuscstartmonth2<-as.data.frame(novaccine$remainingsusc[1:11])%>%
                as.data.frame()%>%
                rename(remainsuscstartmonth=1)
            
            remainsuscstartmonth<-rbind(remainsuscstartmonth, remainsuscstartmonth2)
            
            novaccine<-cbind(novaccine, remainsuscstartmonth)
            
            novaccine<-novaccine%>%
                mutate(overallinfwovac=hospinfwovac+nonhospinfwovac)%>%
                mutate(overallinfcumulwovac=cumsum(overallinfwovac))%>%
                mutate(medicallyattendedinfwovac=overallinfwovac*mai)%>%
                mutate(medicallyattendedinfcumulwovac=cumsum(medicallyattendedinfwovac))
            
            
            
            #####################################################################
            # Incidence rates in the absence of vaccine
            #####################################################################
            novaccine<-novaccine%>%
                mutate(irhospinf=hospinfwovac/remainsuscstartmonth)%>%
                mutate(irnonhospinf=nonhospinfwovac/remainsuscstartmonth)
            
            
            
            
            ############################ Burden inputs with hypothetical vaccination campaign
            #Create columns
            novaccine$hospinfwvac<-NA
            novaccine$hospinfcumulwvac<-NA
            novaccine$nonhospinfwvac<-NA
            novaccine$nonhospinfcumulwvac<-NA
            
            
            
            #Create first row
            novaccine[1,13]<-populationsize*novaccine$irhospinf[1]
            novaccine[1,14]<-novaccine$hospinfwvac[1]
            novaccine[1,15]<-populationsize*novaccine$irnonhospinf[1]
            novaccine[1,16]<-novaccine$nonhospinfwvac[1]
            
            ##bind monthly coverage
            monthvaccov<-c(vaccinecoveragemonthly(), month2(), month3(), month4(), month5(), month6(),
                           month7(), month8(), month9(), month10(), month11(), month12())
            
            monthvaccov<-as.numeric(as.character(monthvaccov))
            
            totcoverage<-sum(monthvaccov)
            
            
            novaccine<-cbind(novaccine, monthvaccov)
            
            novaccine$covergdistrib<-novaccine$monthvaccov/totcoverage
            
            novaccine$monthvaccov<- novaccine$covergdistrib*coverage2()
            
            
            ##monthly effectively vaccinated and susceptible with hypothetical vaccination campaign
            novaccine<-novaccine%>%
                mutate(effectivelyvacc=populationsize*monthvaccov*vaccineeffectiveness)%>%
                mutate(effectivelycumulvacc=cumsum(effectivelyvacc))%>%
                mutate(suscwvacc=populationsize-hospinfcumulwvac-nonhospinfcumulwvac-effectivelyvacc)
            
            ##Then calculate the rest of hospinfwovac and nonhospinfwovac. Calculation uses last month's incidence rate
            for(i in 2:nrow(novaccine)){
                novaccine$hospinfwvac[i]<-novaccine$suscwvacc[i-1]*novaccine$irhospinf[i]
                novaccine$hospinfcumulwvac[i]<-sum(novaccine$hospinfwvac[1:i])
                novaccine$nonhospinfwvac[i]<- novaccine$suscwvacc[i-1]*novaccine$irnonhospinf[i]
                novaccine$nonhospinfcumulwvac[i]<-sum(novaccine$nonhospinfwvac[1:i])
                novaccine$suscwvacc[i]=populationsize-novaccine$hospinfcumulwvac[i]-novaccine$nonhospinfcumulwvac[i]-novaccine$effectivelycumulvacc[i]
            }
            
            
            ## month number
            novaccine$monthnum<-c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
            
            
            ## Overall infections with hypothetical vaccination campaign
            novaccine<-novaccine%>%
                mutate(overallinfwvac=hospinfwvac+nonhospinfwvac)%>%
                mutate(overallinfcumulwvacc=cumsum(overallinfwvac))
            
            ## Medically attended with hypothetical vaccination campaign
            novaccine<-novaccine%>%
                mutate(medicalattedtoinfwvac=overallinfwvac*mai)
            
            
            ######## Avoided events
            novaccine<-novaccine%>%
                mutate(AEhospinf=hospinfwovac-hospinfwvac)%>%
                mutate(AEnonhospinf=nonhospinfwovac-nonhospinfwvac)%>%
                mutate(AEmedicalattedtoinf=medicallyattendedinfwovac-medicalattedtoinfwvac)%>%
                mutate(AEoverallinf=overallinfwovac-overallinfwvac)
            
            
            
            ################ Prevented fraction
            ## Hospitalized infections
            ifelse (monthstartvac()==1,
                    novaccine<-novaccine%>%
                        mutate(PFhospinf=sum(AEhospinf)/sum(hospinfwovac)),
                    novaccine<-novaccine%>%
                        mutate(PFhospinf=sum(AEhospinf[monthstartvac():nrow(novaccine)]/sum(hospinfwovac[monthstartvac():nrow(novaccine)])))
            )
            
            ## non-Hospitalized infections
            ifelse (monthstartvac()==1,
                    novaccine<-novaccine%>%
                        mutate(PFnonhospinf=sum(AEnonhospinf)/sum(nonhospinfwovac)),
                    novaccine<-novaccine%>%
                        mutate(PFnonhospinf=sum(AEnonhospinf[monthstartvac():nrow(novaccine)]/sum(nonhospinfwovac[monthstartvac():nrow(novaccine)])))
            )
            
            
            
            
            ################ NNV -----------------------------------> why does this only go to month 8 in the Excel formula?????
            ## Hospitalized infections
            ifelse(monthstartvac()==1,
                   novaccine<-novaccine%>%
                       mutate(NNVhospinf=1/((vaccineeffectiveness*(sum(hospinfwovac)))/populationsize)),
                   novaccine<-novaccine%>%
                       mutate(NNVhospinf=1/((vaccineeffectiveness*(sum(hospinfwovac[monthstartvac():nrow(novaccine)])))/populationsize))
            )
            
            
            ## non-Hospitalized infections
            ifelse(monthstartvac()==1,
                   novaccine<-novaccine%>%
                       mutate(NNVnonhospinf=1/((vaccineeffectiveness*(sum(nonhospinfwovac)))/populationsize)),
                   novaccine<-novaccine%>%
                       mutate(NNVnonhospinf=1/((vaccineeffectiveness*(sum(nonhospinfwovac[monthstartvac():nrow(novaccine)])))/populationsize))
            )
            
            ## Medically attended infections
            ifelse(monthstartvac()==1,
                   novaccine<-novaccine%>%
                       mutate(NNVmedicalattendedinf=1/((vaccineeffectiveness*(sum(medicallyattendedinfwovac)))/populationsize)),
                   novaccine<-novaccine%>%
                       mutate(NNVmedicalattendedinf=1/((vaccineeffectiveness*(sum(medicallyattendedinfwovac[monthstartvac():nrow(novaccine)])))/populationsize))
            )
            
            ## Total infections
            ifelse(monthstartvac()==1,
                   novaccine<-novaccine%>%
                       mutate(NNVoverallinf=1/((vaccineeffectiveness*(sum(hospinfwovac)+sum(nonhospinfwovac)))/populationsize)),
                   novaccine<-novaccine%>%
                       mutate(NNVoverallinf=1/((vaccineeffectiveness*(sum(hospinfwovac[monthstartvac():nrow(novaccine)])+sum(nonhospinfwovac[monthstartvac():nrow(novaccine)])))/populationsize))
            )
            
            return(novaccine)
            
            
        }#close "if hypothetical campaign"
        
    }) #close burdendataset2
    
    
    
    ####################################################################################################
    ## List of burden averted results
    ####################################################################################################
    
    
    burdenavertedresultscoverage2<-reactive({
        
        populationsize<-as.numeric(as.character(populationsize()))
        hnh<-as.numeric(as.character(hnh()))
        mai<-as.numeric(as.character(mai()))
        vaccineeffectiveness<-as.numeric(as.character(vaccineeffectiveness()))
        
        
        
        #################################################
        ## Results
        #################################################
        nonhospinfwovactot<-sum(burdendataset_coverage2()$nonhospinfwovac)
        nonhospinfwvacctot<-sum(burdendataset_coverage2()$nonhospinfwvac)
        nonhospinfaverted<-nonhospinfwovactot-nonhospinfwvacctot
        NNVnonhospinffinal<-burdendataset_coverage2()$NNVnonhospinf[1]
        
        medicallyattendedinfwovactot<-sum(burdendataset_coverage2()$medicallyattendedinfwovac)
        medicallyattendedinfwvactot<-sum(burdendataset_coverage2()$medicalattedtoinfwvac)
        medicallyattendedinfaverted<-medicallyattendedinfwovactot-medicallyattendedinfwvactot
        NNVmedicallyattendtedfinal<-burdendataset_coverage2()$NNVmedicalattendedinf[1]
        
        Totalinfwovacc<-sum(burdendataset_coverage2()$overallinfwovac)
        Totalinfwvacc<-sum(burdendataset_coverage2()$overallinfwvac)
        Totalinfaverted<-Totalinfwovacc-Totalinfwvacc
        NNVtotalinf<-burdendataset_coverage2()$NNVoverallinf[1]
        
        hospinfwovactot<-sum(burdendataset_coverage2()$hospinfwovac)
        hospinfwvacctot<-sum(burdendataset_coverage2()$hospinfwvac)
        hospaverted<-hospinfwovactot-hospinfwvacctot
        NNVhospinffinal<-burdendataset_coverage2()$NNVhospinf[1]
        
        
        nonhospPreventedfraction<-burdendataset_coverage2()$PFnonhospinf[1]
        hospPreventedfraction<-burdendataset_coverage2()$PFhospinf[1]
        
        
        indicator_results2<-c(nonhospinfwovactot, nonhospinfwvacctot, nonhospinfaverted, NNVnonhospinffinal,
                              Totalinfwovacc, Totalinfwvacc, Totalinfaverted, NNVtotalinf,
                              hospinfwovactot, hospinfwvacctot, hospaverted, NNVhospinffinal,
                              medicallyattendedinfwovactot, medicallyattendedinfwvactot, medicallyattendedinfaverted, NNVmedicallyattendtedfinal,
                              nonhospPreventedfraction)
        
        indicator_results_titles<-c("nonhospinfwovactot", "nonhospinfwvacctot", "nonhospinfaverted", "NNVnonhospinffinal",
                                    "Totalinfwovacc", "Totalinfwvacc", "Totalinfaverted", "NNVtotalinf",
                                    "hospinfwovactot", "hospinfwvacctot", "hospaverted", "NNVhospinffinal",
                                    "medicallyattendedinfwovactot", "medicallyattendedinfwvactot", "medicallyattendedinfaverted", "NNVmedicallyattendtedfinal",
                                    "nonhospPreventedfraction")
        
        indicator_results2<-cbind(indicator_results_titles, indicator_results2)
        
        
        return(indicator_results2)
        
        
    })#burdenavertedresults_coverage2
    
    
    
    
    
    #######################################################################################################################################################################################
    
    ##Deployment coverage 3
    #######################################################################################################################################################################################
    
    
    burdendataset_coverage3<- reactive({
        ################################ The output data when insufficient data has been entered   ##################################
        ############################################################################################################################
        
        
        if(scenario()=="Impact of current influenza vaccination campaign"){
            
            
            ##########################################################################################
            # Scenario of influenza circulation in presence of vaccine: Presence to absence of vaccine
            # This is a situation where the country conducts a vaccination campaign, we enter
            # actual burden inputs and vaccine coverage then compare this to a hypothetical scenario
            # where there is no vaccination campaign conducted
            ##########################################################################################
            
            
            
            
            hospinf<-c(hospitalizationsmonthly(), hospmonth2(), hospmonth3(), hospmonth4(), hospmonth5(), hospmonth6(),
                       hospmonth7(), hospmonth8(), hospmonth9(), hospmonth10(), hospmonth11(), hospmonth12())
            
            populationsize<-as.numeric(as.character(populationsize()))
            vaccineeffectiveness<-as.numeric(as.character(vaccineeffectiveness()))
            hnh<-as.numeric(as.character(hnh()))
            mai<-as.numeric(as.character(mai()))
            hospinf<-as.numeric(as.character(hospinf))
            
            novaccine<-hospinf%>%
                as.data.frame()%>%
                rename(hospinfwvac=1)%>%
                mutate(hospinfcumulwvac=cumsum(hospinfwvac))%>%
                mutate(nonhospinfwvac=hospinfwvac*hnh)%>%
                mutate(nonhospinfcumulwvac=cumsum(nonhospinfwvac))%>%
                mutate(overallinfwvac=hospinfwvac+nonhospinfwvac)%>%
                mutate(overallinfcumulwvac=cumsum(overallinfwvac))%>%
                mutate(medicalattedtoinfwvac=overallinfwvac*mai)%>%
                mutate(medicallyattendedinfcumulwvac=cumsum(medicalattedtoinfwvac))
            
            
            
            ##bind monthly coverage
            monthvaccov<-c(vaccinecoveragemonthly(), month2(), month3(), month4(), month5(), month6(),
                           month7(), month8(), month9(), month10(), month11(), month12())
            
            monthvaccov<-as.numeric(as.character(monthvaccov))
            
            totcoverage<-sum(monthvaccov)
            
            
            novaccine<-cbind(novaccine, monthvaccov)
            
            novaccine$covergdistrib<-novaccine$monthvaccov/totcoverage
            
            novaccine$monthvaccov<- novaccine$covergdistrib*coverage3()
            
            
            ## month number
            novaccine$monthnum<-c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
            
            ## add monthly effectively vaccinated
            novaccine<-novaccine%>%
                mutate(effectivelyvacc=populationsize*monthvaccov*vaccineeffectiveness)%>%
                mutate(effectivelyvacccumul=cumsum(effectivelyvacc))
            
            
            
            
            #####################################################################
            # Estimated rates
            #####################################################################
            novaccine<-novaccine%>%
                mutate(susceptiblewvacc=populationsize-hospinfcumulwvac-nonhospinfcumulwvac-effectivelyvacccumul)
            
            #Create columns
            novaccine$irhospinf<-NA
            novaccine$irnonhospinf<-NA
            
            #Create first row
            novaccine[1,14]<-novaccine$hospinfwvac[1]/populationsize
            novaccine[1,15]<-novaccine$nonhospinfwvac[1]/populationsize
            
            
            ##Calculation uses last month's susceptible population
            for(i in 2:nrow(novaccine)){
                novaccine$irhospinf[i]<-novaccine$hospinfwvac[i]/novaccine$susceptiblewvacc[i-1]
                novaccine$irnonhospinf[i]<- novaccine$nonhospinfwvac[i]/novaccine$susceptiblewvacc[i-1]
            }
            
            #####################################################################
            # Scenario of influenza circulation in absence of vaccine
            #####################################################################
            
            ## These calculations are initialized by the 'Susceptible at start of month in absence of vaccine'
            ## column, which starts with the Target Population in the first month.
            
            remainsuscstartmonth<-c(populationsize, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
            novaccine<-cbind(novaccine, remainsuscstartmonth)
            
            #Create columns
            novaccine$hospinfwovac<-NA
            novaccine$hospinfcumulwovacc<-NA
            novaccine$nonhospinfwovac<-NA
            novaccine$nonhospinfcumulwovacc<-NA
            novaccine$remainingsusc<-NA
            
            #Create first row
            novaccine[1,17]<-novaccine$remainsuscstartmonth[1]*novaccine$irhospinf[1]
            novaccine[1,18]<-novaccine$hospinfwovac[1]
            novaccine[1,19]<-novaccine$remainsuscstartmonth[1]*novaccine$irnonhospinf[1]
            novaccine[1,20]<-novaccine$nonhospinfwovac[1]
            novaccine[1,21]<-populationsize-novaccine$hospinfcumulwovacc[1]-novaccine$nonhospinfcumulwovacc[1]
            
            
            #Create rows 2:12
            for(i in 2:nrow(novaccine)){
                novaccine$remainsuscstartmonth[i]<-novaccine$remainingsusc[i-1]
                novaccine$hospinfwovac[i]<-novaccine$remainsuscstartmonth[i]*novaccine$irhospinf[i]
                novaccine$hospinfcumulwovacc[i]<-sum(novaccine$hospinfwovac[1:i])
                novaccine$nonhospinfwovac[i]<-novaccine$remainsuscstartmonth[i]*novaccine$irnonhospinf[i]
                novaccine$nonhospinfcumulwovacc[i]<-sum(novaccine$nonhospinfwovac[1:i])
                novaccine$remainingsusc[i]<-populationsize-novaccine$hospinfcumulwovacc[i]-novaccine$nonhospinfcumulwovacc[i]
            }
            
            novaccine<-novaccine%>%
                mutate(overallinfwovac=hospinfwovac+nonhospinfwovac)%>%
                mutate(overallinfcumulwovacc=cumsum(overallinfwovac))%>%
                mutate(medicallyattendedinfwovac=overallinfwovac*mai)%>%
                mutate(medicalattedtoinfcumulwovacc=cumsum(medicallyattendedinfwovac))
            
            ######## Avoided events
            novaccine<-novaccine%>%
                mutate(AEhospinf=hospinfwovac-hospinfwvac)%>%
                mutate(AEnonhospinf=nonhospinfwovac-nonhospinfwvac)%>%
                mutate(AEmedicalattedtoinf=medicallyattendedinfwovac-medicalattedtoinfwvac)%>%
                mutate(AEoverallinf=overallinfwovac-overallinfwvac)
            
            
            ################ Prevented fraction
            ## Hospitalized infections
            ifelse (monthstartvac()==1,
                    novaccine<-novaccine%>%
                        mutate(PFhospinf=sum(AEhospinf)/sum(hospinfwovac)),
                    novaccine<-novaccine%>%
                        mutate(PFhospinf=sum(AEhospinf[monthstartvac():nrow(novaccine)]/sum(hospinfwovac[monthstartvac():nrow(novaccine)])))
            )
            
            ## non-Hospitalized infections
            ifelse (monthstartvac()==1,
                    novaccine<-novaccine%>%
                        mutate(PFnonhospinf=sum(AEnonhospinf)/sum(nonhospinfwovac)),
                    novaccine<-novaccine%>%
                        mutate(PFnonhospinf=sum(AEnonhospinf[monthstartvac():nrow(novaccine)]/sum(nonhospinfwovac[monthstartvac():nrow(novaccine)])))
            )
            
            
            ################ NNV -----------------------------------> why does this only go to month 8 in the Excel formula?????
            ## Hospitalized infections
            ifelse(monthstartvac()==1,
                   novaccine<-novaccine%>%
                       mutate(NNVhospinf=1/((vaccineeffectiveness*(sum(hospinfwovac)))/populationsize)),
                   novaccine<-novaccine%>%
                       mutate(NNVhospinf=1/((vaccineeffectiveness*(sum(hospinfwovac[monthstartvac():nrow(novaccine)])))/populationsize))
            )
            
            
            ## non-Hospitalized infections
            ifelse(monthstartvac()==1,
                   novaccine<-novaccine%>%
                       mutate(NNVnonhospinf=1/((vaccineeffectiveness*(sum(nonhospinfwovac)))/populationsize)),
                   novaccine<-novaccine%>%
                       mutate(NNVnonhospinf=1/((vaccineeffectiveness*(sum(nonhospinfwovac[monthstartvac():nrow(novaccine)])))/populationsize))
            )
            
            ## Medically infections
            ifelse(monthstartvac()==1,
                   novaccine<-novaccine%>%
                       mutate(NNVmedicalattendedinf=1/((vaccineeffectiveness*(sum(medicallyattendedinfwovac)))/populationsize)),
                   novaccine<-novaccine%>%
                       mutate(NNVmedicalattendedinf=1/((vaccineeffectiveness*(sum(medicallyattendedinfwovac[monthstartvac():nrow(novaccine)])))/populationsize))
            )
            
            ## Overall infections
            ifelse(monthstartvac()==1,
                   novaccine<-novaccine%>%
                       mutate(NNVoverallinf=1/((vaccineeffectiveness*(sum(hospinfwovac)+sum(nonhospinfwovac)))/populationsize)),
                   novaccine<-novaccine%>%
                       mutate(NNVoverallinf=1/((vaccineeffectiveness*(sum(hospinfwovac[monthstartvac():nrow(novaccine)])+sum(nonhospinfwovac[monthstartvac():nrow(novaccine)])))/populationsize))
            )
            
            return(novaccine)
            
            
            
        } #close if current campaign
        
        
        
        if(scenario()=="Impact of hypothetical vaccination campaign"){
            
            ##########################################################################################
            # Scenario of influenza circulation in absence of vaccine: Absence to presence of vaccine
            # This is a scenario where the country has yet to conduct a vaccination campaign but would
            # like to see what impact a vaccine campaign may have.
            # The country enters the estimated number of hospitalization they observed and then we compare
            # this to a hypothetical vaccination campaign
            ##########################################################################################
            
            hospinf<-c(hospitalizationsmonthly(), hospmonth2(), hospmonth3(), hospmonth4(), hospmonth5(), hospmonth6(),
                       hospmonth7(), hospmonth8(), hospmonth9(), hospmonth10(), hospmonth11(), hospmonth12())
            
            populationsize<-as.numeric(as.character(populationsize()))
            hnh<-as.numeric(as.character(hnh()))
            mai<-as.numeric(as.character(mai()))
            hospinf<-as.numeric(as.character(hospinf))
            vaccineeffectiveness<-as.numeric(as.character(vaccineeffectiveness()))
            
            #####################Observed influenza in the absence of vaccine
            novaccine<-hospinf%>%
                as.data.frame()%>%
                rename(hospinfwovac=1)%>%
                mutate(hospinfcumulwovac=cumsum(hospinfwovac))%>%
                mutate(nonhospinfwovac=hospinfwovac*hnh)%>%
                mutate(nonhospinfcumulwovac=cumsum(nonhospinfwovac))%>%
                mutate(remainingsusc=populationsize-hospinfcumulwovac-nonhospinfcumulwovac)
            
            #create remainsuscstartmonth column with target population and rows 1:11 of remainingsusc
            remainsuscstartmonth<-populationsize%>%
                as.data.frame()%>%
                rename(remainsuscstartmonth=1)
            
            remainsuscstartmonth2<-as.data.frame(novaccine$remainingsusc[1:11])%>%
                as.data.frame()%>%
                rename(remainsuscstartmonth=1)
            
            remainsuscstartmonth<-rbind(remainsuscstartmonth, remainsuscstartmonth2)
            
            novaccine<-cbind(novaccine, remainsuscstartmonth)
            
            novaccine<-novaccine%>%
                mutate(overallinfwovac=hospinfwovac+nonhospinfwovac)%>%
                mutate(overallinfcumulwovac=cumsum(overallinfwovac))%>%
                mutate(medicallyattendedinfwovac=overallinfwovac*mai)%>%
                mutate(medicallyattendedinfcumulwovac=cumsum(medicallyattendedinfwovac))
            
            
            
            #####################################################################
            # Incidence rates in the absence of vaccine
            #####################################################################
            novaccine<-novaccine%>%
                mutate(irhospinf=hospinfwovac/remainsuscstartmonth)%>%
                mutate(irnonhospinf=nonhospinfwovac/remainsuscstartmonth)
            
            
            
            
            ############################ Burden inputs with hypothetical vaccination campaign
            #Create columns
            novaccine$hospinfwvac<-NA
            novaccine$hospinfcumulwvac<-NA
            novaccine$nonhospinfwvac<-NA
            novaccine$nonhospinfcumulwvac<-NA
            
            
            
            #Create first row
            novaccine[1,13]<-populationsize*novaccine$irhospinf[1]
            novaccine[1,14]<-novaccine$hospinfwvac[1]
            novaccine[1,15]<-populationsize*novaccine$irnonhospinf[1]
            novaccine[1,16]<-novaccine$nonhospinfwvac[1]
            
            ##bind monthly coverage
            monthvaccov<-c(vaccinecoveragemonthly(), month2(), month3(), month4(), month5(), month6(),
                           month7(), month8(), month9(), month10(), month11(), month12())
            
            monthvaccov<-as.numeric(as.character(monthvaccov))
            
            totcoverage<-sum(monthvaccov)
            
            
            novaccine<-cbind(novaccine, monthvaccov)
            
            novaccine$covergdistrib<-novaccine$monthvaccov/totcoverage
            
            novaccine$monthvaccov<- novaccine$covergdistrib*coverage3()
            
            
            ##monthly effectively vaccinated and susceptible with hypothetical vaccination campaign
            novaccine<-novaccine%>%
                mutate(effectivelyvacc=populationsize*monthvaccov*vaccineeffectiveness)%>%
                mutate(effectivelycumulvacc=cumsum(effectivelyvacc))%>%
                mutate(suscwvacc=populationsize-hospinfcumulwvac-nonhospinfcumulwvac-effectivelyvacc)
            
            ##Then calculate the rest of hospinfwovac and nonhospinfwovac. Calculation uses last month's incidence rate
            for(i in 2:nrow(novaccine)){
                novaccine$hospinfwvac[i]<-novaccine$suscwvacc[i-1]*novaccine$irhospinf[i]
                novaccine$hospinfcumulwvac[i]<-sum(novaccine$hospinfwvac[1:i])
                novaccine$nonhospinfwvac[i]<- novaccine$suscwvacc[i-1]*novaccine$irnonhospinf[i]
                novaccine$nonhospinfcumulwvac[i]<-sum(novaccine$nonhospinfwvac[1:i])
                novaccine$suscwvacc[i]=populationsize-novaccine$hospinfcumulwvac[i]-novaccine$nonhospinfcumulwvac[i]-novaccine$effectivelycumulvacc[i]
            }
            
            
            ## month number
            novaccine$monthnum<-c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
            
            
            ## Overall infections with hypothetical vaccination campaign
            novaccine<-novaccine%>%
                mutate(overallinfwvac=hospinfwvac+nonhospinfwvac)%>%
                mutate(overallinfcumulwvacc=cumsum(overallinfwvac))
            
            ## Medically attended with hypothetical vaccination campaign
            novaccine<-novaccine%>%
                mutate(medicalattedtoinfwvac=overallinfwvac*mai)
            
            
            ######## Avoided events
            novaccine<-novaccine%>%
                mutate(AEhospinf=hospinfwovac-hospinfwvac)%>%
                mutate(AEnonhospinf=nonhospinfwovac-nonhospinfwvac)%>%
                mutate(AEmedicalattedtoinf=medicallyattendedinfwovac-medicalattedtoinfwvac)%>%
                mutate(AEoverallinf=overallinfwovac-overallinfwvac)
            
            
            
            ################ Prevented fraction
            ## Hospitalized infections
            ifelse (monthstartvac()==1,
                    novaccine<-novaccine%>%
                        mutate(PFhospinf=sum(AEhospinf)/sum(hospinfwovac)),
                    novaccine<-novaccine%>%
                        mutate(PFhospinf=sum(AEhospinf[monthstartvac():nrow(novaccine)]/sum(hospinfwovac[monthstartvac():nrow(novaccine)])))
            )
            
            ## non-Hospitalized infections
            ifelse (monthstartvac()==1,
                    novaccine<-novaccine%>%
                        mutate(PFnonhospinf=sum(AEnonhospinf)/sum(nonhospinfwovac)),
                    novaccine<-novaccine%>%
                        mutate(PFnonhospinf=sum(AEnonhospinf[monthstartvac():nrow(novaccine)]/sum(nonhospinfwovac[monthstartvac():nrow(novaccine)])))
            )
            
            
            
            
            ################ NNV -----------------------------------> why does this only go to month 8 in the Excel formula?????
            ## Hospitalized infections
            ifelse(monthstartvac()==1,
                   novaccine<-novaccine%>%
                       mutate(NNVhospinf=1/((vaccineeffectiveness*(sum(hospinfwovac)))/populationsize)),
                   novaccine<-novaccine%>%
                       mutate(NNVhospinf=1/((vaccineeffectiveness*(sum(hospinfwovac[monthstartvac():nrow(novaccine)])))/populationsize))
            )
            
            
            ## non-Hospitalized infections
            ifelse(monthstartvac()==1,
                   novaccine<-novaccine%>%
                       mutate(NNVnonhospinf=1/((vaccineeffectiveness*(sum(nonhospinfwovac)))/populationsize)),
                   novaccine<-novaccine%>%
                       mutate(NNVnonhospinf=1/((vaccineeffectiveness*(sum(nonhospinfwovac[monthstartvac():nrow(novaccine)])))/populationsize))
            )
            
            ## Medically attended infections
            ifelse(monthstartvac()==1,
                   novaccine<-novaccine%>%
                       mutate(NNVmedicalattendedinf=1/((vaccineeffectiveness*(sum(medicallyattendedinfwovac)))/populationsize)),
                   novaccine<-novaccine%>%
                       mutate(NNVmedicalattendedinf=1/((vaccineeffectiveness*(sum(medicallyattendedinfwovac[monthstartvac():nrow(novaccine)])))/populationsize))
            )
            
            ## Total infections
            ifelse(monthstartvac()==1,
                   novaccine<-novaccine%>%
                       mutate(NNVoverallinf=1/((vaccineeffectiveness*(sum(hospinfwovac)+sum(nonhospinfwovac)))/populationsize)),
                   novaccine<-novaccine%>%
                       mutate(NNVoverallinf=1/((vaccineeffectiveness*(sum(hospinfwovac[monthstartvac():nrow(novaccine)])+sum(nonhospinfwovac[monthstartvac():nrow(novaccine)])))/populationsize))
            )
            
            return(novaccine)
            
            
        }#close "if hypothetical campaign"
        
    }) #close burdendataset
    
    
    
    ####################################################################################################
    ## List of burden averted results
    ####################################################################################################
    
    
    burdenavertedresultscoverage3<-reactive({
        
        populationsize<-as.numeric(as.character(populationsize()))
        hnh<-as.numeric(as.character(hnh()))
        mai<-as.numeric(as.character(mai()))
        vaccineeffectiveness<-as.numeric(as.character(vaccineeffectiveness()))
        
        
        
        #################################################
        ## Results
        #################################################
        nonhospinfwovactot<-sum(burdendataset_coverage3()$nonhospinfwovac)
        nonhospinfwvacctot<-sum(burdendataset_coverage3()$nonhospinfwvac)
        nonhospinfaverted<-nonhospinfwovactot-nonhospinfwvacctot
        NNVnonhospinffinal<-burdendataset_coverage3()$NNVnonhospinf[1]
        
        medicallyattendedinfwovactot<-sum(burdendataset_coverage3()$medicallyattendedinfwovac)
        medicallyattendedinfwvactot<-sum(burdendataset_coverage3()$medicalattedtoinfwvac)
        medicallyattendedinfaverted<-medicallyattendedinfwovactot-medicallyattendedinfwvactot
        NNVmedicallyattendtedfinal<-burdendataset_coverage3()$NNVmedicalattendedinf[1]
        
        Totalinfwovacc<-sum(burdendataset_coverage3()$overallinfwovac)
        Totalinfwvacc<-sum(burdendataset_coverage3()$overallinfwvac)
        Totalinfaverted<-Totalinfwovacc-Totalinfwvacc
        NNVtotalinf<-burdendataset_coverage3()$NNVoverallinf[1]
        
        hospinfwovactot<-sum(burdendataset_coverage3()$hospinfwovac)
        hospinfwvacctot<-sum(burdendataset_coverage3()$hospinfwvac)
        hospaverted<-hospinfwovactot-hospinfwvacctot
        NNVhospinffinal<-burdendataset_coverage3()$NNVhospinf[1]
        
        
        nonhospPreventedfraction<-burdendataset_coverage3()$PFnonhospinf[1]
        hospPreventedfraction<-burdendataset_coverage3()$PFhospinf[1]
        
        
        indicator_results3<-c(nonhospinfwovactot, nonhospinfwvacctot, nonhospinfaverted, NNVnonhospinffinal,
                              Totalinfwovacc, Totalinfwvacc, Totalinfaverted, NNVtotalinf,
                              hospinfwovactot, hospinfwvacctot, hospaverted, NNVhospinffinal,
                              medicallyattendedinfwovactot, medicallyattendedinfwvactot, medicallyattendedinfaverted, NNVmedicallyattendtedfinal,
                              nonhospPreventedfraction)
        
        indicator_results_titles<-c("nonhospinfwovactot", "nonhospinfwvacctot", "nonhospinfaverted", "NNVnonhospinffinal",
                                    "Totalinfwovacc", "Totalinfwvacc", "Totalinfaverted", "NNVtotalinf",
                                    "hospinfwovactot", "hospinfwvacctot", "hospaverted", "NNVhospinffinal",
                                    "medicallyattendedinfwovactot", "medicallyattendedinfwvactot", "medicallyattendedinfaverted", "NNVmedicallyattendtedfinal",
                                    "nonhospPreventedfraction")
        
        indicator_results3<-cbind(indicator_results_titles, indicator_results3)
        
        
        return(indicator_results3)
        
        
    })#burdenavertedresults_coverage3
    
    
    
    
    
    
    #######################################################################################################################################################################################
    
    ##Deployment coverage 4
    #######################################################################################################################################################################################
    
    
    burdendataset_coverage4<- reactive({
        ################################ The output data when insufficient data has been entered   ##################################
        ############################################################################################################################
        
        
        if(scenario()=="Impact of current influenza vaccination campaign"){
            
            
            ##########################################################################################
            # Scenario of influenza circulation in presence of vaccine: Presence to absence of vaccine
            # This is a situation where the country conducts a vaccination campaign, we enter
            # actual burden inputs and vaccine coverage then compare this to a hypothetical scenario
            # where there is no vaccination campaign conducted
            ##########################################################################################
            
            
            
            
            hospinf<-c(hospitalizationsmonthly(), hospmonth2(), hospmonth3(), hospmonth4(), hospmonth5(), hospmonth6(),
                       hospmonth7(), hospmonth8(), hospmonth9(), hospmonth10(), hospmonth11(), hospmonth12())
            
            populationsize<-as.numeric(as.character(populationsize()))
            vaccineeffectiveness<-as.numeric(as.character(vaccineeffectiveness()))
            hnh<-as.numeric(as.character(hnh()))
            mai<-as.numeric(as.character(mai()))
            hospinf<-as.numeric(as.character(hospinf))
            
            novaccine<-hospinf%>%
                as.data.frame()%>%
                rename(hospinfwvac=1)%>%
                mutate(hospinfcumulwvac=cumsum(hospinfwvac))%>%
                mutate(nonhospinfwvac=hospinfwvac*hnh)%>%
                mutate(nonhospinfcumulwvac=cumsum(nonhospinfwvac))%>%
                mutate(overallinfwvac=hospinfwvac+nonhospinfwvac)%>%
                mutate(overallinfcumulwvac=cumsum(overallinfwvac))%>%
                mutate(medicalattedtoinfwvac=overallinfwvac*mai)%>%
                mutate(medicallyattendedinfcumulwvac=cumsum(medicalattedtoinfwvac))
            
            
            
            ##bind monthly coverage
            monthvaccov<-c(vaccinecoveragemonthly(), month2(), month3(), month4(), month5(), month6(),
                           month7(), month8(), month9(), month10(), month11(), month12())
            
            monthvaccov<-as.numeric(as.character(monthvaccov))
            
            totcoverage<-sum(monthvaccov)
            
            
            novaccine<-cbind(novaccine, monthvaccov)
            
            novaccine$covergdistrib<-novaccine$monthvaccov/totcoverage
            
            novaccine$monthvaccov<- novaccine$covergdistrib*coverage4()
            
            
            ## month number
            novaccine$monthnum<-c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
            
            ## add monthly effectively vaccinated
            novaccine<-novaccine%>%
                mutate(effectivelyvacc=populationsize*monthvaccov*vaccineeffectiveness)%>%
                mutate(effectivelyvacccumul=cumsum(effectivelyvacc))
            
            
            
            
            #####################################################################
            # Estimated rates
            #####################################################################
            novaccine<-novaccine%>%
                mutate(susceptiblewvacc=populationsize-hospinfcumulwvac-nonhospinfcumulwvac-effectivelyvacccumul)
            
            #Create columns
            novaccine$irhospinf<-NA
            novaccine$irnonhospinf<-NA
            
            #Create first row
            novaccine[1,14]<-novaccine$hospinfwvac[1]/populationsize
            novaccine[1,15]<-novaccine$nonhospinfwvac[1]/populationsize
            
            
            ##Calculation uses last month's susceptible population
            for(i in 2:nrow(novaccine)){
                novaccine$irhospinf[i]<-novaccine$hospinfwvac[i]/novaccine$susceptiblewvacc[i-1]
                novaccine$irnonhospinf[i]<- novaccine$nonhospinfwvac[i]/novaccine$susceptiblewvacc[i-1]
            }
            
            #####################################################################
            # Scenario of influenza circulation in absence of vaccine
            #####################################################################
            
            ## These calculations are initialized by the 'Susceptible at start of month in absence of vaccine'
            ## column, which starts with the Target Population in the first month.
            
            remainsuscstartmonth<-c(populationsize, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
            novaccine<-cbind(novaccine, remainsuscstartmonth)
            
            #Create columns
            novaccine$hospinfwovac<-NA
            novaccine$hospinfcumulwovacc<-NA
            novaccine$nonhospinfwovac<-NA
            novaccine$nonhospinfcumulwovacc<-NA
            novaccine$remainingsusc<-NA
            
            #Create first row
            novaccine[1,17]<-novaccine$remainsuscstartmonth[1]*novaccine$irhospinf[1]
            novaccine[1,18]<-novaccine$hospinfwovac[1]
            novaccine[1,19]<-novaccine$remainsuscstartmonth[1]*novaccine$irnonhospinf[1]
            novaccine[1,20]<-novaccine$nonhospinfwovac[1]
            novaccine[1,21]<-populationsize-novaccine$hospinfcumulwovacc[1]-novaccine$nonhospinfcumulwovacc[1]
            
            
            #Create rows 2:12
            for(i in 2:nrow(novaccine)){
                novaccine$remainsuscstartmonth[i]<-novaccine$remainingsusc[i-1]
                novaccine$hospinfwovac[i]<-novaccine$remainsuscstartmonth[i]*novaccine$irhospinf[i]
                novaccine$hospinfcumulwovacc[i]<-sum(novaccine$hospinfwovac[1:i])
                novaccine$nonhospinfwovac[i]<-novaccine$remainsuscstartmonth[i]*novaccine$irnonhospinf[i]
                novaccine$nonhospinfcumulwovacc[i]<-sum(novaccine$nonhospinfwovac[1:i])
                novaccine$remainingsusc[i]<-populationsize-novaccine$hospinfcumulwovacc[i]-novaccine$nonhospinfcumulwovacc[i]
            }
            
            novaccine<-novaccine%>%
                mutate(overallinfwovac=hospinfwovac+nonhospinfwovac)%>%
                mutate(overallinfcumulwovacc=cumsum(overallinfwovac))%>%
                mutate(medicallyattendedinfwovac=overallinfwovac*mai)%>%
                mutate(medicalattedtoinfcumulwovacc=cumsum(medicallyattendedinfwovac))
            
            ######## Avoided events
            novaccine<-novaccine%>%
                mutate(AEhospinf=hospinfwovac-hospinfwvac)%>%
                mutate(AEnonhospinf=nonhospinfwovac-nonhospinfwvac)%>%
                mutate(AEmedicalattedtoinf=medicallyattendedinfwovac-medicalattedtoinfwvac)%>%
                mutate(AEoverallinf=overallinfwovac-overallinfwvac)
            
            
            ################ Prevented fraction
            ## Hospitalized infections
            ifelse (monthstartvac()==1,
                    novaccine<-novaccine%>%
                        mutate(PFhospinf=sum(AEhospinf)/sum(hospinfwovac)),
                    novaccine<-novaccine%>%
                        mutate(PFhospinf=sum(AEhospinf[monthstartvac():nrow(novaccine)]/sum(hospinfwovac[monthstartvac():nrow(novaccine)])))
            )
            
            ## non-Hospitalized infections
            ifelse (monthstartvac()==1,
                    novaccine<-novaccine%>%
                        mutate(PFnonhospinf=sum(AEnonhospinf)/sum(nonhospinfwovac)),
                    novaccine<-novaccine%>%
                        mutate(PFnonhospinf=sum(AEnonhospinf[monthstartvac():nrow(novaccine)]/sum(nonhospinfwovac[monthstartvac():nrow(novaccine)])))
            )
            
            
            ################ NNV -----------------------------------> why does this only go to month 8 in the Excel formula?????
            ## Hospitalized infections
            ifelse(monthstartvac()==1,
                   novaccine<-novaccine%>%
                       mutate(NNVhospinf=1/((vaccineeffectiveness*(sum(hospinfwovac)))/populationsize)),
                   novaccine<-novaccine%>%
                       mutate(NNVhospinf=1/((vaccineeffectiveness*(sum(hospinfwovac[monthstartvac():nrow(novaccine)])))/populationsize))
            )
            
            
            ## non-Hospitalized infections
            ifelse(monthstartvac()==1,
                   novaccine<-novaccine%>%
                       mutate(NNVnonhospinf=1/((vaccineeffectiveness*(sum(nonhospinfwovac)))/populationsize)),
                   novaccine<-novaccine%>%
                       mutate(NNVnonhospinf=1/((vaccineeffectiveness*(sum(nonhospinfwovac[monthstartvac():nrow(novaccine)])))/populationsize))
            )
            
            ## Medically infections
            ifelse(monthstartvac()==1,
                   novaccine<-novaccine%>%
                       mutate(NNVmedicalattendedinf=1/((vaccineeffectiveness*(sum(medicallyattendedinfwovac)))/populationsize)),
                   novaccine<-novaccine%>%
                       mutate(NNVmedicalattendedinf=1/((vaccineeffectiveness*(sum(medicallyattendedinfwovac[monthstartvac():nrow(novaccine)])))/populationsize))
            )
            
            ## Overall infections
            ifelse(monthstartvac()==1,
                   novaccine<-novaccine%>%
                       mutate(NNVoverallinf=1/((vaccineeffectiveness*(sum(hospinfwovac)+sum(nonhospinfwovac)))/populationsize)),
                   novaccine<-novaccine%>%
                       mutate(NNVoverallinf=1/((vaccineeffectiveness*(sum(hospinfwovac[monthstartvac():nrow(novaccine)])+sum(nonhospinfwovac[monthstartvac():nrow(novaccine)])))/populationsize))
            )
            
            return(novaccine)
            
            
            
        } #close if current campaign
        
        
        
        if(scenario()=="Impact of hypothetical vaccination campaign"){
            
            ##########################################################################################
            # Scenario of influenza circulation in absence of vaccine: Absence to presence of vaccine
            # This is a scenario where the country has yet to conduct a vaccination campaign but would
            # like to see what impact a vaccine campaign may have.
            # The country enters the estimated number of hospitalization they observed and then we compare
            # this to a hypothetical vaccination campaign
            ##########################################################################################
            
            hospinf<-c(hospitalizationsmonthly(), hospmonth2(), hospmonth3(), hospmonth4(), hospmonth5(), hospmonth6(),
                       hospmonth7(), hospmonth8(), hospmonth9(), hospmonth10(), hospmonth11(), hospmonth12())
            
            populationsize<-as.numeric(as.character(populationsize()))
            hnh<-as.numeric(as.character(hnh()))
            mai<-as.numeric(as.character(mai()))
            hospinf<-as.numeric(as.character(hospinf))
            vaccineeffectiveness<-as.numeric(as.character(vaccineeffectiveness()))
            
            #####################Observed influenza in the absence of vaccine
            novaccine<-hospinf%>%
                as.data.frame()%>%
                rename(hospinfwovac=1)%>%
                mutate(hospinfcumulwovac=cumsum(hospinfwovac))%>%
                mutate(nonhospinfwovac=hospinfwovac*hnh)%>%
                mutate(nonhospinfcumulwovac=cumsum(nonhospinfwovac))%>%
                mutate(remainingsusc=populationsize-hospinfcumulwovac-nonhospinfcumulwovac)
            
            #create remainsuscstartmonth column with target population and rows 1:11 of remainingsusc
            remainsuscstartmonth<-populationsize%>%
                as.data.frame()%>%
                rename(remainsuscstartmonth=1)
            
            remainsuscstartmonth2<-as.data.frame(novaccine$remainingsusc[1:11])%>%
                as.data.frame()%>%
                rename(remainsuscstartmonth=1)
            
            remainsuscstartmonth<-rbind(remainsuscstartmonth, remainsuscstartmonth2)
            
            novaccine<-cbind(novaccine, remainsuscstartmonth)
            
            novaccine<-novaccine%>%
                mutate(overallinfwovac=hospinfwovac+nonhospinfwovac)%>%
                mutate(overallinfcumulwovac=cumsum(overallinfwovac))%>%
                mutate(medicallyattendedinfwovac=overallinfwovac*mai)%>%
                mutate(medicallyattendedinfcumulwovac=cumsum(medicallyattendedinfwovac))
            
            
            
            #####################################################################
            # Incidence rates in the absence of vaccine
            #####################################################################
            novaccine<-novaccine%>%
                mutate(irhospinf=hospinfwovac/remainsuscstartmonth)%>%
                mutate(irnonhospinf=nonhospinfwovac/remainsuscstartmonth)
            
            
            
            
            ############################ Burden inputs with hypothetical vaccination campaign
            #Create columns
            novaccine$hospinfwvac<-NA
            novaccine$hospinfcumulwvac<-NA
            novaccine$nonhospinfwvac<-NA
            novaccine$nonhospinfcumulwvac<-NA
            
            
            
            #Create first row
            novaccine[1,13]<-populationsize*novaccine$irhospinf[1]
            novaccine[1,14]<-novaccine$hospinfwvac[1]
            novaccine[1,15]<-populationsize*novaccine$irnonhospinf[1]
            novaccine[1,16]<-novaccine$nonhospinfwvac[1]
            
            ##bind monthly coverage
            monthvaccov<-c(vaccinecoveragemonthly(), month2(), month3(), month4(), month5(), month6(),
                           month7(), month8(), month9(), month10(), month11(), month12())
            
            monthvaccov<-as.numeric(as.character(monthvaccov))
            
            totcoverage<-sum(monthvaccov)
            
            
            novaccine<-cbind(novaccine, monthvaccov)
            
            novaccine$covergdistrib<-novaccine$monthvaccov/totcoverage
            
            novaccine$monthvaccov<- novaccine$covergdistrib*coverage4()
            
            
            ##monthly effectively vaccinated and susceptible with hypothetical vaccination campaign
            novaccine<-novaccine%>%
                mutate(effectivelyvacc=populationsize*monthvaccov*vaccineeffectiveness)%>%
                mutate(effectivelycumulvacc=cumsum(effectivelyvacc))%>%
                mutate(suscwvacc=populationsize-hospinfcumulwvac-nonhospinfcumulwvac-effectivelyvacc)
            
            ##Then calculate the rest of hospinfwovac and nonhospinfwovac. Calculation uses last month's incidence rate
            for(i in 2:nrow(novaccine)){
                novaccine$hospinfwvac[i]<-novaccine$suscwvacc[i-1]*novaccine$irhospinf[i]
                novaccine$hospinfcumulwvac[i]<-sum(novaccine$hospinfwvac[1:i])
                novaccine$nonhospinfwvac[i]<- novaccine$suscwvacc[i-1]*novaccine$irnonhospinf[i]
                novaccine$nonhospinfcumulwvac[i]<-sum(novaccine$nonhospinfwvac[1:i])
                novaccine$suscwvacc[i]=populationsize-novaccine$hospinfcumulwvac[i]-novaccine$nonhospinfcumulwvac[i]-novaccine$effectivelycumulvacc[i]
            }
            
            
            ## month number
            novaccine$monthnum<-c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
            
            
            ## Overall infections with hypothetical vaccination campaign
            novaccine<-novaccine%>%
                mutate(overallinfwvac=hospinfwvac+nonhospinfwvac)%>%
                mutate(overallinfcumulwvacc=cumsum(overallinfwvac))
            
            ## Medically attended with hypothetical vaccination campaign
            novaccine<-novaccine%>%
                mutate(medicalattedtoinfwvac=overallinfwvac*mai)
            
            
            ######## Avoided events
            novaccine<-novaccine%>%
                mutate(AEhospinf=hospinfwovac-hospinfwvac)%>%
                mutate(AEnonhospinf=nonhospinfwovac-nonhospinfwvac)%>%
                mutate(AEmedicalattedtoinf=medicallyattendedinfwovac-medicalattedtoinfwvac)%>%
                mutate(AEoverallinf=overallinfwovac-overallinfwvac)
            
            
            
            ################ Prevented fraction
            ## Hospitalized infections
            ifelse (monthstartvac()==1,
                    novaccine<-novaccine%>%
                        mutate(PFhospinf=sum(AEhospinf)/sum(hospinfwovac)),
                    novaccine<-novaccine%>%
                        mutate(PFhospinf=sum(AEhospinf[monthstartvac():nrow(novaccine)]/sum(hospinfwovac[monthstartvac():nrow(novaccine)])))
            )
            
            ## non-Hospitalized infections
            ifelse (monthstartvac()==1,
                    novaccine<-novaccine%>%
                        mutate(PFnonhospinf=sum(AEnonhospinf)/sum(nonhospinfwovac)),
                    novaccine<-novaccine%>%
                        mutate(PFnonhospinf=sum(AEnonhospinf[monthstartvac():nrow(novaccine)]/sum(nonhospinfwovac[monthstartvac():nrow(novaccine)])))
            )
            
            
            
            
            ################ NNV -----------------------------------> why does this only go to month 8 in the Excel formula?????
            ## Hospitalized infections
            ifelse(monthstartvac()==1,
                   novaccine<-novaccine%>%
                       mutate(NNVhospinf=1/((vaccineeffectiveness*(sum(hospinfwovac)))/populationsize)),
                   novaccine<-novaccine%>%
                       mutate(NNVhospinf=1/((vaccineeffectiveness*(sum(hospinfwovac[monthstartvac():nrow(novaccine)])))/populationsize))
            )
            
            
            ## non-Hospitalized infections
            ifelse(monthstartvac()==1,
                   novaccine<-novaccine%>%
                       mutate(NNVnonhospinf=1/((vaccineeffectiveness*(sum(nonhospinfwovac)))/populationsize)),
                   novaccine<-novaccine%>%
                       mutate(NNVnonhospinf=1/((vaccineeffectiveness*(sum(nonhospinfwovac[monthstartvac():nrow(novaccine)])))/populationsize))
            )
            
            ## Medically attended infections
            ifelse(monthstartvac()==1,
                   novaccine<-novaccine%>%
                       mutate(NNVmedicalattendedinf=1/((vaccineeffectiveness*(sum(medicallyattendedinfwovac)))/populationsize)),
                   novaccine<-novaccine%>%
                       mutate(NNVmedicalattendedinf=1/((vaccineeffectiveness*(sum(medicallyattendedinfwovac[monthstartvac():nrow(novaccine)])))/populationsize))
            )
            
            ## Total infections
            ifelse(monthstartvac()==1,
                   novaccine<-novaccine%>%
                       mutate(NNVoverallinf=1/((vaccineeffectiveness*(sum(hospinfwovac)+sum(nonhospinfwovac)))/populationsize)),
                   novaccine<-novaccine%>%
                       mutate(NNVoverallinf=1/((vaccineeffectiveness*(sum(hospinfwovac[monthstartvac():nrow(novaccine)])+sum(nonhospinfwovac[monthstartvac():nrow(novaccine)])))/populationsize))
            )
            
            return(novaccine)
            
            
        }#close "if hypothetical campaign"
        
    }) #close burdendataset4
    
    
    
    ####################################################################################################
    ## List of burden averted results
    ####################################################################################################
    
    
    burdenavertedresultscoverage4<-reactive({
        
        populationsize<-as.numeric(as.character(populationsize()))
        hnh<-as.numeric(as.character(hnh()))
        mai<-as.numeric(as.character(mai()))
        vaccineeffectiveness<-as.numeric(as.character(vaccineeffectiveness()))
        
        
        
        #################################################
        ## Results
        #################################################
        nonhospinfwovactot<-sum(burdendataset_coverage4()$nonhospinfwovac)
        nonhospinfwvacctot<-sum(burdendataset_coverage4()$nonhospinfwvac)
        nonhospinfaverted<-nonhospinfwovactot-nonhospinfwvacctot
        NNVnonhospinffinal<-burdendataset_coverage4()$NNVnonhospinf[1]
        
        medicallyattendedinfwovactot<-sum(burdendataset_coverage4()$medicallyattendedinfwovac)
        medicallyattendedinfwvactot<-sum(burdendataset_coverage4()$medicalattedtoinfwvac)
        medicallyattendedinfaverted<-medicallyattendedinfwovactot-medicallyattendedinfwvactot
        NNVmedicallyattendtedfinal<-burdendataset_coverage4()$NNVmedicalattendedinf[1]
        
        Totalinfwovacc<-sum(burdendataset_coverage4()$overallinfwovac)
        Totalinfwvacc<-sum(burdendataset_coverage4()$overallinfwvac)
        Totalinfaverted<-Totalinfwovacc-Totalinfwvacc
        NNVtotalinf<-burdendataset_coverage4()$NNVoverallinf[1]
        
        hospinfwovactot<-sum(burdendataset_coverage4()$hospinfwovac)
        hospinfwvacctot<-sum(burdendataset_coverage4()$hospinfwvac)
        hospaverted<-hospinfwovactot-hospinfwvacctot
        NNVhospinffinal<-burdendataset_coverage4()$NNVhospinf[1]
        
        
        nonhospPreventedfraction<-burdendataset_coverage4()$PFnonhospinf[1]
        hospPreventedfraction<-burdendataset_coverage4()$PFhospinf[1]
        
        
        indicator_results4<-c(nonhospinfwovactot, nonhospinfwvacctot, nonhospinfaverted, NNVnonhospinffinal,
                              Totalinfwovacc, Totalinfwvacc, Totalinfaverted, NNVtotalinf,
                              hospinfwovactot, hospinfwvacctot, hospaverted, NNVhospinffinal,
                              medicallyattendedinfwovactot, medicallyattendedinfwvactot, medicallyattendedinfaverted, NNVmedicallyattendtedfinal,
                              nonhospPreventedfraction)
        
        indicator_results_titles<-c("nonhospinfwovactot", "nonhospinfwvacctot", "nonhospinfaverted", "NNVnonhospinffinal",
                                    "Totalinfwovacc", "Totalinfwvacc", "Totalinfaverted", "NNVtotalinf",
                                    "hospinfwovactot", "hospinfwvacctot", "hospaverted", "NNVhospinffinal",
                                    "medicallyattendedinfwovactot", "medicallyattendedinfwvactot", "medicallyattendedinfaverted", "NNVmedicallyattendtedfinal",
                                    "nonhospPreventedfraction")
        
        indicator_results4<-cbind(indicator_results_titles, indicator_results4)
        
        
        return(indicator_results4)
        
        
    })#burdenavertedresults_coverage4
    
    
    
    
    
    
    
    
    
    
    ##2 - Reprint base results table and attach results from coverage 1-4
    gt_table_coverage<- reactive({
        
        ####################################
        ## Render results table
        ####################################
        
        populationsize<-as.numeric(as.character(populationsize()))
        hnh<-as.numeric(as.character(hnh()))
        mai<-as.numeric(as.character(mai()))
        vaccineeffectiveness<-as.numeric(as.character(vaccineeffectiveness()))
        VELCI<-as.numeric(as.character(VELCI()))
        VEUCI<-as.numeric(as.character(VEUCI()))
        
        
        burdenavertedresults<-burdenavertedresults()%>%
            as.data.frame()%>%
            mutate(indicator_results=as.numeric(indicator_results))%>%
            select(indicator_results)
        
        
        
        burdenavertedresultscoverage1<-burdenavertedresultscoverage1()%>%
            as.data.frame()%>%
            mutate(indicator_results1=as.numeric(indicator_results1))%>%
            select(indicator_results1)
    
        burdenavertedresultscoverage2<-burdenavertedresultscoverage2()%>%
            as.data.frame()%>%
            mutate(indicator_results2=as.numeric(indicator_results2))%>%
            select(indicator_results2)
        
        burdenavertedresultscoverage3<-burdenavertedresultscoverage3()%>%
            as.data.frame()%>%
            mutate(indicator_results3=as.numeric(indicator_results3))%>%
            select(indicator_results3)
        
        
        burdenavertedresultscoverage4<-burdenavertedresultscoverage4()%>%
            as.data.frame()%>%
            mutate(indicator_results4=as.numeric(indicator_results4))%>%
            select(indicator_results4)
        
        
        
        presenceTable<-cbind(presenceTable, burdenavertedresults, burdenavertedresultscoverage1, burdenavertedresultscoverage2, burdenavertedresultscoverage3, burdenavertedresultscoverage4)
        
        
        
        presenceTablefinal<-presenceTable%>%
            select(indicators, indicator_results, indicator_results1, indicator_results2, indicator_results3, indicator_results4)%>%
            gt()%>%
            fmt_number(
                columns=c(indicator_results, indicator_results1, indicator_results2, indicator_results3, indicator_results4),
                decimals=2,
                rows=2:17
            )%>%
            fmt_percent(
                columns = c(indicator_results,indicator_results1, indicator_results2, indicator_results3, indicator_results4),
                decimals = 2,
                rows=17:17
            )%>%
            tab_header(
                title = paste0("Vaccine coverage strategy results"),
                subtitle = paste0(targetgroup(),", ", country())
            )%>%
            cols_label(
                indicators = "",
                indicator_results = "Base scenario",
                indicator_results1 = paste0((coverage1()*100), " % coverage"),
                indicator_results2 = paste0((coverage2()*100), " % coverage"),
                indicator_results3 = paste0((coverage3()*100), " % coverage"),
                indicator_results4 = paste0((coverage4()*100), " % coverage")
            )%>%
            tab_row_group(
                label = "Non-hospitalized infections",
                rows = 1:4)%>%
            tab_row_group(
                label = "Total infections",
                rows = 5:8)%>%
            tab_row_group(
                label = "Hospitalized infections",
                rows = 9:12)%>%
            tab_row_group(
                label = "Medically attended infections",
                rows = 13:16)%>%
            tab_row_group(
                label = "Prevented fraction",
                rows = 17:17)%>%
            tab_style(
                locations = cells_row_groups(),
                style     = list(
                    cell_text(weight = "bold")
                ))%>%
            tab_options(
                #Adjust grouped rows to make them stand out
                row_group.background.color = "#e1e3e1")%>%
            tab_style(
                locations = cells_title(groups = "title"),
                style     = list(
                    cell_text(weight = "bold", size = 24)
                ))%>%
            tab_style(
                locations = cells_title(groups = "subtitle"),
                style     = list(
                    cell_text(weight = "bold", size = 18)
                ))%>%
            tab_style(
                locations = cells_column_labels(columns = everything()),
                style     = list(
                    cell_text(weight = "bold")
                )
            )%>%
            row_group_order(groups = c("Hospitalized infections", "Non-hospitalized infections", "Total infections", "Medically attended infections", "Prevented fraction"))
        
        return(presenceTablefinal)
        
    }
    ) ##close table
    
    
    
    
    output$table_coverage<-render_gt({
        
        table<-gt_table_coverage()
        return(table)
    })



    
    
    
    
    
    ############################################################################
    ############################################################################
    
    # Scenario Planning - Timing
    
    ############################################################################
    ############################################################################    
    

    #######################################################################################################################################################################################
    
    ##Deployment timing 1
    #######################################################################################################################################################################################
    
    
    burdendataset_timing1<- reactive({
        ################################ The output data when insufficient data has been entered   ##################################
        ############################################################################################################################
        
        
        if(scenario()=="Impact of current influenza vaccination campaign"){
            
            
            ##########################################################################################
            # Scenario of influenza circulation in presence of vaccine: Presence to absence of vaccine
            # This is a situation where the country conducts a vaccination campaign, we enter
            # actual burden inputs and vaccine coverage then compare this to a hypothetical scenario
            # where there is no vaccination campaign conducted
            ##########################################################################################
            
            
            
            
            hospinf<-c(hospitalizationsmonthly(), hospmonth2(), hospmonth3(), hospmonth4(), hospmonth5(), hospmonth6(),
                       hospmonth7(), hospmonth8(), hospmonth9(), hospmonth10(), hospmonth11(), hospmonth12())
            
            populationsize<-as.numeric(as.character(populationsize()))
            vaccineeffectiveness<-as.numeric(as.character(vaccineeffectiveness()))
            hnh<-as.numeric(as.character(hnh()))
            mai<-as.numeric(as.character(mai()))
            hospinf<-as.numeric(as.character(hospinf))
            
            novaccine<-hospinf%>%
                as.data.frame()%>%
                rename(hospinfwvac=1)%>%
                mutate(hospinfcumulwvac=cumsum(hospinfwvac))%>%
                mutate(nonhospinfwvac=hospinfwvac*hnh)%>%
                mutate(nonhospinfcumulwvac=cumsum(nonhospinfwvac))%>%
                mutate(overallinfwvac=hospinfwvac+nonhospinfwvac)%>%
                mutate(overallinfcumulwvac=cumsum(overallinfwvac))%>%
                mutate(medicalattedtoinfwvac=overallinfwvac*mai)%>%
                mutate(medicallyattendedinfcumulwvac=cumsum(medicalattedtoinfwvac))
            
            
            
            ##bind monthly coverage
            monthvaccov<-c(vaccinecoveragemonthly(), month2(), month3(), month4(), month5(), month6(),
                           month7(), month8(), month9(), month10(), month11(), month12())
            
            monthvaccov<-as.numeric(as.character(monthvaccov))
            
            totcoverage<-sum(monthvaccov, na.rm=TRUE)
            
            
            novaccine<-cbind(novaccine, monthvaccov)
            
            novaccine$covergdistrib<- c(vaccinecoveragemonthly1(), month21(), month31(), month41(), month51(), month61(), month71(), month81(), month91(), month101(), month111(), month121())
            
            novaccine$covergdistrib<-as.numeric(as.character(novaccine$covergdistrib))
            
            novaccine$monthvaccov<- novaccine$covergdistrib*totcoverage
            
            
            ## month number
            novaccine$monthnum<-c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
            
            ## add monthly effectively vaccinated
            novaccine<-novaccine%>%
                mutate(effectivelyvacc=populationsize*monthvaccov*vaccineeffectiveness)%>%
                mutate(effectivelyvacccumul=cumsum(effectivelyvacc))
            
            
            
            
            #####################################################################
            # Estimated rates
            #####################################################################
            novaccine<-novaccine%>%
                mutate(susceptiblewvacc=populationsize-hospinfcumulwvac-nonhospinfcumulwvac-effectivelyvacccumul)
            
            #Create columns
            novaccine$irhospinf<-NA
            novaccine$irnonhospinf<-NA
            
            #Create first row
            novaccine[1,14]<-novaccine$hospinfwvac[1]/populationsize
            novaccine[1,15]<-novaccine$nonhospinfwvac[1]/populationsize
            
            
            ##Calculation uses last month's susceptible population
            for(i in 2:nrow(novaccine)){
                novaccine$irhospinf[i]<-novaccine$hospinfwvac[i]/novaccine$susceptiblewvacc[i-1]
                novaccine$irnonhospinf[i]<- novaccine$nonhospinfwvac[i]/novaccine$susceptiblewvacc[i-1]
            }
            
            #####################################################################
            # Scenario of influenza circulation in absence of vaccine
            #####################################################################
            
            ## These calculations are initialized by the 'Susceptible at start of month in absence of vaccine'
            ## column, which starts with the Target Population in the first month.
            
            remainsuscstartmonth<-c(populationsize, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
            novaccine<-cbind(novaccine, remainsuscstartmonth)
            
            #Create columns
            novaccine$hospinfwovac<-NA
            novaccine$hospinfcumulwovacc<-NA
            novaccine$nonhospinfwovac<-NA
            novaccine$nonhospinfcumulwovacc<-NA
            novaccine$remainingsusc<-NA
            
            #Create first row
            novaccine[1,17]<-novaccine$remainsuscstartmonth[1]*novaccine$irhospinf[1]
            novaccine[1,18]<-novaccine$hospinfwovac[1]
            novaccine[1,19]<-novaccine$remainsuscstartmonth[1]*novaccine$irnonhospinf[1]
            novaccine[1,20]<-novaccine$nonhospinfwovac[1]
            novaccine[1,21]<-populationsize-novaccine$hospinfcumulwovacc[1]-novaccine$nonhospinfcumulwovacc[1]
            
            
            #Create rows 2:12
            for(i in 2:nrow(novaccine)){
                novaccine$remainsuscstartmonth[i]<-novaccine$remainingsusc[i-1]
                novaccine$hospinfwovac[i]<-novaccine$remainsuscstartmonth[i]*novaccine$irhospinf[i]
                novaccine$hospinfcumulwovacc[i]<-sum(novaccine$hospinfwovac[1:i])
                novaccine$nonhospinfwovac[i]<-novaccine$remainsuscstartmonth[i]*novaccine$irnonhospinf[i]
                novaccine$nonhospinfcumulwovacc[i]<-sum(novaccine$nonhospinfwovac[1:i])
                novaccine$remainingsusc[i]<-populationsize-novaccine$hospinfcumulwovacc[i]-novaccine$nonhospinfcumulwovacc[i]
            }
            
            novaccine<-novaccine%>%
                mutate(overallinfwovac=hospinfwovac+nonhospinfwovac)%>%
                mutate(overallinfcumulwovacc=cumsum(overallinfwovac))%>%
                mutate(medicallyattendedinfwovac=overallinfwovac*mai)%>%
                mutate(medicalattedtoinfcumulwovacc=cumsum(medicallyattendedinfwovac))
            
            ######## Avoided events
            novaccine<-novaccine%>%
                mutate(AEhospinf=hospinfwovac-hospinfwvac)%>%
                mutate(AEnonhospinf=nonhospinfwovac-nonhospinfwvac)%>%
                mutate(AEmedicalattedtoinf=medicallyattendedinfwovac-medicalattedtoinfwvac)%>%
                mutate(AEoverallinf=overallinfwovac-overallinfwvac)
            
            
            ################ Prevented fraction
            ## Hospitalized infections
            ifelse (monthstartvac1()==1,
                    novaccine<-novaccine%>%
                        mutate(PFhospinf=sum(AEhospinf)/sum(hospinfwovac)),
                    novaccine<-novaccine%>%
                        mutate(PFhospinf=sum(AEhospinf[monthstartvac1():nrow(novaccine)]/sum(hospinfwovac[monthstartvac1():nrow(novaccine)])))
            )
            
            ## non-Hospitalized infections
            ifelse (monthstartvac1()==1,
                    novaccine<-novaccine%>%
                        mutate(PFnonhospinf=sum(AEnonhospinf)/sum(nonhospinfwovac)),
                    novaccine<-novaccine%>%
                        mutate(PFnonhospinf=sum(AEnonhospinf[monthstartvac1():nrow(novaccine)]/sum(nonhospinfwovac[monthstartvac1():nrow(novaccine)])))
            )
            
            
            ################ NNV -----------------------------------> why does this only go to month 8 in the Excel formula?????
            ## Hospitalized infections
            ifelse(monthstartvac1()==1,
                   novaccine<-novaccine%>%
                       mutate(NNVhospinf=1/((vaccineeffectiveness*(sum(hospinfwovac)))/populationsize)),
                   novaccine<-novaccine%>%
                       mutate(NNVhospinf=1/((vaccineeffectiveness*(sum(hospinfwovac[monthstartvac1():nrow(novaccine)])))/populationsize))
            )
            
            
            ## non-Hospitalized infections
            ifelse(monthstartvac1()==1,
                   novaccine<-novaccine%>%
                       mutate(NNVnonhospinf=1/((vaccineeffectiveness*(sum(nonhospinfwovac)))/populationsize)),
                   novaccine<-novaccine%>%
                       mutate(NNVnonhospinf=1/((vaccineeffectiveness*(sum(nonhospinfwovac[monthstartvac1():nrow(novaccine)])))/populationsize))
            )
            
            ## Medically infections
            ifelse(monthstartvac1()==1,
                   novaccine<-novaccine%>%
                       mutate(NNVmedicalattendedinf=1/((vaccineeffectiveness*(sum(medicallyattendedinfwovac)))/populationsize)),
                   novaccine<-novaccine%>%
                       mutate(NNVmedicalattendedinf=1/((vaccineeffectiveness*(sum(medicallyattendedinfwovac[monthstartvac1():nrow(novaccine)])))/populationsize))
            )
            
            ## Overall infections
            ifelse(monthstartvac1()==1,
                   novaccine<-novaccine%>%
                       mutate(NNVoverallinf=1/((vaccineeffectiveness*(sum(hospinfwovac)+sum(nonhospinfwovac)))/populationsize)),
                   novaccine<-novaccine%>%
                       mutate(NNVoverallinf=1/((vaccineeffectiveness*(sum(hospinfwovac[monthstartvac1():nrow(novaccine)])+sum(nonhospinfwovac[monthstartvac1():nrow(novaccine)])))/populationsize))
            )
            
            return(novaccine)
            
            
            
        } #close if current campaign
        
        
        
        if(scenario()=="Impact of hypothetical vaccination campaign"){
            
            ##########################################################################################
            # Scenario of influenza circulation in absence of vaccine: Absence to presence of vaccine
            # This is a scenario where the country has yet to conduct a vaccination campaign but would
            # like to see what impact a vaccine campaign may have.
            # The country enters the estimated number of hospitalization they observed and then we compare
            # this to a hypothetical vaccination campaign
            ##########################################################################################
            
            hospinf<-c(hospitalizationsmonthly(), hospmonth2(), hospmonth3(), hospmonth4(), hospmonth5(), hospmonth6(),
                       hospmonth7(), hospmonth8(), hospmonth9(), hospmonth10(), hospmonth11(), hospmonth12())
            
            populationsize<-as.numeric(as.character(populationsize()))
            hnh<-as.numeric(as.character(hnh()))
            mai<-as.numeric(as.character(mai()))
            hospinf<-as.numeric(as.character(hospinf))
            vaccineeffectiveness<-as.numeric(as.character(vaccineeffectiveness()))
            
            #####################Observed influenza in the absence of vaccine
            novaccine<-hospinf%>%
                as.data.frame()%>%
                rename(hospinfwovac=1)%>%
                mutate(hospinfcumulwovac=cumsum(hospinfwovac))%>%
                mutate(nonhospinfwovac=hospinfwovac*hnh)%>%
                mutate(nonhospinfcumulwovac=cumsum(nonhospinfwovac))%>%
                mutate(remainingsusc=populationsize-hospinfcumulwovac-nonhospinfcumulwovac)
            
            #create remainsuscstartmonth column with target population and rows 1:11 of remainingsusc
            remainsuscstartmonth<-populationsize%>%
                as.data.frame()%>%
                rename(remainsuscstartmonth=1)
            
            remainsuscstartmonth2<-as.data.frame(novaccine$remainingsusc[1:11])%>%
                as.data.frame()%>%
                rename(remainsuscstartmonth=1)
            
            remainsuscstartmonth<-rbind(remainsuscstartmonth, remainsuscstartmonth2)
            
            novaccine<-cbind(novaccine, remainsuscstartmonth)
            
            novaccine<-novaccine%>%
                mutate(overallinfwovac=hospinfwovac+nonhospinfwovac)%>%
                mutate(overallinfcumulwovac=cumsum(overallinfwovac))%>%
                mutate(medicallyattendedinfwovac=overallinfwovac*mai)%>%
                mutate(medicallyattendedinfcumulwovac=cumsum(medicallyattendedinfwovac))
            
            
            
            #####################################################################
            # Incidence rates in the absence of vaccine
            #####################################################################
            novaccine<-novaccine%>%
                mutate(irhospinf=hospinfwovac/remainsuscstartmonth)%>%
                mutate(irnonhospinf=nonhospinfwovac/remainsuscstartmonth)
            
            
            
            
            ############################ Burden inputs with hypothetical vaccination campaign
            #Create columns
            novaccine$hospinfwvac<-NA
            novaccine$hospinfcumulwvac<-NA
            novaccine$nonhospinfwvac<-NA
            novaccine$nonhospinfcumulwvac<-NA
            
            
            
            #Create first row
            novaccine[1,13]<-populationsize*novaccine$irhospinf[1]
            novaccine[1,14]<-novaccine$hospinfwvac[1]
            novaccine[1,15]<-populationsize*novaccine$irnonhospinf[1]
            novaccine[1,16]<-novaccine$nonhospinfwvac[1]
            
            ##bind monthly coverage
            monthvaccov<-c(vaccinecoveragemonthly(), month2(), month3(), month4(), month5(), month6(),
                           month7(), month8(), month9(), month10(), month11(), month12())
            
            monthvaccov<-as.numeric(as.character(monthvaccov))
            
            totcoverage<-sum(monthvaccov, na.rm=TRUE)
            
            
            novaccine<-cbind(novaccine, monthvaccov)
            
            novaccine$covergdistrib<- c(vaccinecoveragemonthly1(), month21(), month31(), month41(), month51(), month61(), month71(), month81(), month91(), month101(), month111(), month121())
            
            novaccine$covergdistrib<-as.numeric(as.character(novaccine$covergdistrib))
            
            novaccine$monthvaccov<- novaccine$covergdistrib*totcoverage
            
            
            ##monthly effectively vaccinated and susceptible with hypothetical vaccination campaign
            novaccine<-novaccine%>%
                mutate(effectivelyvacc=populationsize*monthvaccov*vaccineeffectiveness)%>%
                mutate(effectivelycumulvacc=cumsum(effectivelyvacc))%>%
                mutate(suscwvacc=populationsize-hospinfcumulwvac-nonhospinfcumulwvac-effectivelyvacc)
            
            ##Then calculate the rest of hospinfwovac and nonhospinfwovac. Calculation uses last month's incidence rate
            for(i in 2:nrow(novaccine)){
                novaccine$hospinfwvac[i]<-novaccine$suscwvacc[i-1]*novaccine$irhospinf[i]
                novaccine$hospinfcumulwvac[i]<-sum(novaccine$hospinfwvac[1:i])
                novaccine$nonhospinfwvac[i]<- novaccine$suscwvacc[i-1]*novaccine$irnonhospinf[i]
                novaccine$nonhospinfcumulwvac[i]<-sum(novaccine$nonhospinfwvac[1:i])
                novaccine$suscwvacc[i]=populationsize-novaccine$hospinfcumulwvac[i]-novaccine$nonhospinfcumulwvac[i]-novaccine$effectivelycumulvacc[i]
            }
            
            
            ## month number
            novaccine$monthnum<-c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
            
            
            ## Overall infections with hypothetical vaccination campaign
            novaccine<-novaccine%>%
                mutate(overallinfwvac=hospinfwvac+nonhospinfwvac)%>%
                mutate(overallinfcumulwvacc=cumsum(overallinfwvac))
            
            ## Medically attended with hypothetical vaccination campaign
            novaccine<-novaccine%>%
                mutate(medicalattedtoinfwvac=overallinfwvac*mai)
            
            
            ######## Avoided events
            novaccine<-novaccine%>%
                mutate(AEhospinf=hospinfwovac-hospinfwvac)%>%
                mutate(AEnonhospinf=nonhospinfwovac-nonhospinfwvac)%>%
                mutate(AEmedicalattedtoinf=medicallyattendedinfwovac-medicalattedtoinfwvac)%>%
                mutate(AEoverallinf=overallinfwovac-overallinfwvac)
            
            
            
            ################ Prevented fraction
            ## Hospitalized infections
            ifelse (monthstartvac1()==1,
                    novaccine<-novaccine%>%
                        mutate(PFhospinf=sum(AEhospinf)/sum(hospinfwovac)),
                    novaccine<-novaccine%>%
                        mutate(PFhospinf=sum(AEhospinf[monthstartvac1():nrow(novaccine)]/sum(hospinfwovac[monthstartvac1():nrow(novaccine)])))
            )
            
            ## non-Hospitalized infections
            ifelse (monthstartvac1()==1,
                    novaccine<-novaccine%>%
                        mutate(PFnonhospinf=sum(AEnonhospinf)/sum(nonhospinfwovac)),
                    novaccine<-novaccine%>%
                        mutate(PFnonhospinf=sum(AEnonhospinf[monthstartvac1():nrow(novaccine)]/sum(nonhospinfwovac[monthstartvac1():nrow(novaccine)])))
            )
            
            
            
            
            ################ NNV -----------------------------------> why does this only go to month 8 in the Excel formula?????
            ## Hospitalized infections
            ifelse(monthstartvac1()==1,
                   novaccine<-novaccine%>%
                       mutate(NNVhospinf=1/((vaccineeffectiveness*(sum(hospinfwovac)))/populationsize)),
                   novaccine<-novaccine%>%
                       mutate(NNVhospinf=1/((vaccineeffectiveness*(sum(hospinfwovac[monthstartvac1():nrow(novaccine)])))/populationsize))
            )
            
            
            ## non-Hospitalized infections
            ifelse(monthstartvac1()==1,
                   novaccine<-novaccine%>%
                       mutate(NNVnonhospinf=1/((vaccineeffectiveness*(sum(nonhospinfwovac)))/populationsize)),
                   novaccine<-novaccine%>%
                       mutate(NNVnonhospinf=1/((vaccineeffectiveness*(sum(nonhospinfwovac[monthstartvac1():nrow(novaccine)])))/populationsize))
            )
            
            ## Medically attended infections
            ifelse(monthstartvac1()==1,
                   novaccine<-novaccine%>%
                       mutate(NNVmedicalattendedinf=1/((vaccineeffectiveness*(sum(medicallyattendedinfwovac)))/populationsize)),
                   novaccine<-novaccine%>%
                       mutate(NNVmedicalattendedinf=1/((vaccineeffectiveness*(sum(medicallyattendedinfwovac[monthstartvac1():nrow(novaccine)])))/populationsize))
            )
            
            ## Total infections
            ifelse(monthstartvac1()==1,
                   novaccine<-novaccine%>%
                       mutate(NNVoverallinf=1/((vaccineeffectiveness*(sum(hospinfwovac)+sum(nonhospinfwovac)))/populationsize)),
                   novaccine<-novaccine%>%
                       mutate(NNVoverallinf=1/((vaccineeffectiveness*(sum(hospinfwovac[monthstartvac1():nrow(novaccine)])+sum(nonhospinfwovac[monthstartvac1():nrow(novaccine)])))/populationsize))
            )
            
            return(novaccine)
            
            
        }#close "if hypothetical campaign"
        
    }) #close burdendataset
    
    
    
    ####################################################################################################
    ## List of burden averted results
    ####################################################################################################
    
    
    burdenavertedresults_timing1<-reactive({
        
        populationsize<-as.numeric(as.character(populationsize()))
        hnh<-as.numeric(as.character(hnh()))
        mai<-as.numeric(as.character(mai()))
        vaccineeffectiveness<-as.numeric(as.character(vaccineeffectiveness()))
        
        
        
        #################################################
        ## Results
        #################################################
        nonhospinfwovactot<-sum(burdendataset_timing1()$nonhospinfwovac)
        nonhospinfwvacctot<-sum(burdendataset_timing1()$nonhospinfwvac)
        nonhospinfaverted<-nonhospinfwovactot-nonhospinfwvacctot
        NNVnonhospinffinal<-burdendataset_timing1()$NNVnonhospinf[1]
        
        medicallyattendedinfwovactot<-sum(burdendataset_timing1()$medicallyattendedinfwovac)
        medicallyattendedinfwvactot<-sum(burdendataset_timing1()$medicalattedtoinfwvac)
        medicallyattendedinfaverted<-medicallyattendedinfwovactot-medicallyattendedinfwvactot
        NNVmedicallyattendtedfinal<-burdendataset_timing1()$NNVmedicalattendedinf[1]
        
        Totalinfwovacc<-sum(burdendataset_timing1()$overallinfwovac)
        Totalinfwvacc<-sum(burdendataset_timing1()$overallinfwvac)
        Totalinfaverted<-Totalinfwovacc-Totalinfwvacc
        NNVtotalinf<-burdendataset_timing1()$NNVoverallinf[1]
        
        hospinfwovactot<-sum(burdendataset_timing1()$hospinfwovac)
        hospinfwvacctot<-sum(burdendataset_timing1()$hospinfwvac)
        hospaverted<-hospinfwovactot-hospinfwvacctot
        NNVhospinffinal<-burdendataset_timing1()$NNVhospinf[1]
        
        
        nonhospPreventedfraction<-burdendataset_timing1()$PFnonhospinf[1]
        hospPreventedfraction<-burdendataset_timing1()$PFhospinf[1]
        
        
        indicator_results1<-c(nonhospinfwovactot, nonhospinfwvacctot, nonhospinfaverted, NNVnonhospinffinal,
                              Totalinfwovacc, Totalinfwvacc, Totalinfaverted, NNVtotalinf,
                              hospinfwovactot, hospinfwvacctot, hospaverted, NNVhospinffinal,
                              medicallyattendedinfwovactot, medicallyattendedinfwvactot, medicallyattendedinfaverted, NNVmedicallyattendtedfinal,
                              nonhospPreventedfraction)
        
        indicator_results_titles<-c("nonhospinfwovactot", "nonhospinfwvacctot", "nonhospinfaverted", "NNVnonhospinffinal",
                                    "Totalinfwovacc", "Totalinfwvacc", "Totalinfaverted", "NNVtotalinf",
                                    "hospinfwovactot", "hospinfwvacctot", "hospaverted", "NNVhospinffinal",
                                    "medicallyattendedinfwovactot", "medicallyattendedinfwvactot", "medicallyattendedinfaverted", "NNVmedicallyattendtedfinal",
                                    "nonhospPreventedfraction")
        
        indicator_results1<-cbind(indicator_results_titles, indicator_results1)
        
        
        return(indicator_results1)
        
        
    })#burdenavertedresults_timing1
    
    
    
    
    
    #######################################################################################################################################################################################
    
    ##Deployment timing 2
    #######################################################################################################################################################################################
    
    
    burdendataset_timing2<- reactive({
        ################################ The output data when insufficient data has been entered   ##################################
        ############################################################################################################################
        
        
        if(scenario()=="Impact of current influenza vaccination campaign"){
            
            
            ##########################################################################################
            # Scenario of influenza circulation in presence of vaccine: Presence to absence of vaccine
            # This is a situation where the country conducts a vaccination campaign, we enter
            # actual burden inputs and vaccine coverage then compare this to a hypothetical scenario
            # where there is no vaccination campaign conducted
            ##########################################################################################
            
            
            
            
            hospinf<-c(hospitalizationsmonthly(), hospmonth2(), hospmonth3(), hospmonth4(), hospmonth5(), hospmonth6(),
                       hospmonth7(), hospmonth8(), hospmonth9(), hospmonth10(), hospmonth11(), hospmonth12())
            
            populationsize<-as.numeric(as.character(populationsize()))
            vaccineeffectiveness<-as.numeric(as.character(vaccineeffectiveness()))
            hnh<-as.numeric(as.character(hnh()))
            mai<-as.numeric(as.character(mai()))
            hospinf<-as.numeric(as.character(hospinf))
            
            novaccine<-hospinf%>%
                as.data.frame()%>%
                rename(hospinfwvac=1)%>%
                mutate(hospinfcumulwvac=cumsum(hospinfwvac))%>%
                mutate(nonhospinfwvac=hospinfwvac*hnh)%>%
                mutate(nonhospinfcumulwvac=cumsum(nonhospinfwvac))%>%
                mutate(overallinfwvac=hospinfwvac+nonhospinfwvac)%>%
                mutate(overallinfcumulwvac=cumsum(overallinfwvac))%>%
                mutate(medicalattedtoinfwvac=overallinfwvac*mai)%>%
                mutate(medicallyattendedinfcumulwvac=cumsum(medicalattedtoinfwvac))
            
            
            
            ##bind monthly coverage
            monthvaccov<-c(vaccinecoveragemonthly(), month2(), month3(), month4(), month5(), month6(),
                           month7(), month8(), month9(), month10(), month11(), month12())
            
            monthvaccov<-as.numeric(as.character(monthvaccov))
            
            totcoverage<-sum(monthvaccov, na.rm=TRUE)
            
            
            novaccine<-cbind(novaccine, monthvaccov)
            
            novaccine$covergdistrib<- c(vaccinecoveragemonthly2(), month22(), month32(), month42(), month52(), month62(), month72(), month82(), month92(), month102(), month112(), month122())
            
            novaccine$covergdistrib<-as.numeric(as.character(novaccine$covergdistrib))
            
            novaccine$monthvaccov<- novaccine$covergdistrib*totcoverage
            
            
            ## month number
            novaccine$monthnum<-c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
            
            ## add monthly effectively vaccinated
            novaccine<-novaccine%>%
                mutate(effectivelyvacc=populationsize*monthvaccov*vaccineeffectiveness)%>%
                mutate(effectivelyvacccumul=cumsum(effectivelyvacc))
            
            
            
            
            #####################################################################
            # Estimated rates
            #####################################################################
            novaccine<-novaccine%>%
                mutate(susceptiblewvacc=populationsize-hospinfcumulwvac-nonhospinfcumulwvac-effectivelyvacccumul)
            
            #Create columns
            novaccine$irhospinf<-NA
            novaccine$irnonhospinf<-NA
            
            #Create first row
            novaccine[1,14]<-novaccine$hospinfwvac[1]/populationsize
            novaccine[1,15]<-novaccine$nonhospinfwvac[1]/populationsize
            
            
            ##Calculation uses last month's susceptible population
            for(i in 2:nrow(novaccine)){
                novaccine$irhospinf[i]<-novaccine$hospinfwvac[i]/novaccine$susceptiblewvacc[i-1]
                novaccine$irnonhospinf[i]<- novaccine$nonhospinfwvac[i]/novaccine$susceptiblewvacc[i-1]
            }
            
            #####################################################################
            # Scenario of influenza circulation in absence of vaccine
            #####################################################################
            
            ## These calculations are initialized by the 'Susceptible at start of month in absence of vaccine'
            ## column, which starts with the Target Population in the first month.
            
            remainsuscstartmonth<-c(populationsize, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
            novaccine<-cbind(novaccine, remainsuscstartmonth)
            
            #Create columns
            novaccine$hospinfwovac<-NA
            novaccine$hospinfcumulwovacc<-NA
            novaccine$nonhospinfwovac<-NA
            novaccine$nonhospinfcumulwovacc<-NA
            novaccine$remainingsusc<-NA
            
            #Create first row
            novaccine[1,17]<-novaccine$remainsuscstartmonth[1]*novaccine$irhospinf[1]
            novaccine[1,18]<-novaccine$hospinfwovac[1]
            novaccine[1,19]<-novaccine$remainsuscstartmonth[1]*novaccine$irnonhospinf[1]
            novaccine[1,20]<-novaccine$nonhospinfwovac[1]
            novaccine[1,21]<-populationsize-novaccine$hospinfcumulwovacc[1]-novaccine$nonhospinfcumulwovacc[1]
            
            
            #Create rows 2:12
            for(i in 2:nrow(novaccine)){
                novaccine$remainsuscstartmonth[i]<-novaccine$remainingsusc[i-1]
                novaccine$hospinfwovac[i]<-novaccine$remainsuscstartmonth[i]*novaccine$irhospinf[i]
                novaccine$hospinfcumulwovacc[i]<-sum(novaccine$hospinfwovac[1:i])
                novaccine$nonhospinfwovac[i]<-novaccine$remainsuscstartmonth[i]*novaccine$irnonhospinf[i]
                novaccine$nonhospinfcumulwovacc[i]<-sum(novaccine$nonhospinfwovac[1:i])
                novaccine$remainingsusc[i]<-populationsize-novaccine$hospinfcumulwovacc[i]-novaccine$nonhospinfcumulwovacc[i]
            }
            
            novaccine<-novaccine%>%
                mutate(overallinfwovac=hospinfwovac+nonhospinfwovac)%>%
                mutate(overallinfcumulwovacc=cumsum(overallinfwovac))%>%
                mutate(medicallyattendedinfwovac=overallinfwovac*mai)%>%
                mutate(medicalattedtoinfcumulwovacc=cumsum(medicallyattendedinfwovac))
            
            ######## Avoided events
            novaccine<-novaccine%>%
                mutate(AEhospinf=hospinfwovac-hospinfwvac)%>%
                mutate(AEnonhospinf=nonhospinfwovac-nonhospinfwvac)%>%
                mutate(AEmedicalattedtoinf=medicallyattendedinfwovac-medicalattedtoinfwvac)%>%
                mutate(AEoverallinf=overallinfwovac-overallinfwvac)
            
            
            ################ Prevented fraction
            ## Hospitalized infections
            ifelse (monthstartvac2()==1,
                    novaccine<-novaccine%>%
                        mutate(PFhospinf=sum(AEhospinf)/sum(hospinfwovac)),
                    novaccine<-novaccine%>%
                        mutate(PFhospinf=sum(AEhospinf[monthstartvac2():nrow(novaccine)]/sum(hospinfwovac[monthstartvac2():nrow(novaccine)])))
            )
            
            ## non-Hospitalized infections
            ifelse (monthstartvac2()==1,
                    novaccine<-novaccine%>%
                        mutate(PFnonhospinf=sum(AEnonhospinf)/sum(nonhospinfwovac)),
                    novaccine<-novaccine%>%
                        mutate(PFnonhospinf=sum(AEnonhospinf[monthstartvac2():nrow(novaccine)]/sum(nonhospinfwovac[monthstartvac2():nrow(novaccine)])))
            )
            
            
            ################ NNV -----------------------------------> why does this only go to month 8 in the Excel formula?????
            ## Hospitalized infections
            ifelse(monthstartvac2()==1,
                   novaccine<-novaccine%>%
                       mutate(NNVhospinf=1/((vaccineeffectiveness*(sum(hospinfwovac)))/populationsize)),
                   novaccine<-novaccine%>%
                       mutate(NNVhospinf=1/((vaccineeffectiveness*(sum(hospinfwovac[monthstartvac2():nrow(novaccine)])))/populationsize))
            )
            
            
            ## non-Hospitalized infections
            ifelse(monthstartvac2()==1,
                   novaccine<-novaccine%>%
                       mutate(NNVnonhospinf=1/((vaccineeffectiveness*(sum(nonhospinfwovac)))/populationsize)),
                   novaccine<-novaccine%>%
                       mutate(NNVnonhospinf=1/((vaccineeffectiveness*(sum(nonhospinfwovac[monthstartvac2():nrow(novaccine)])))/populationsize))
            )
            
            ## Medically infections
            ifelse(monthstartvac2()==1,
                   novaccine<-novaccine%>%
                       mutate(NNVmedicalattendedinf=1/((vaccineeffectiveness*(sum(medicallyattendedinfwovac)))/populationsize)),
                   novaccine<-novaccine%>%
                       mutate(NNVmedicalattendedinf=1/((vaccineeffectiveness*(sum(medicallyattendedinfwovac[monthstartvac2():nrow(novaccine)])))/populationsize))
            )
            
            ## Overall infections
            ifelse(monthstartvac2()==1,
                   novaccine<-novaccine%>%
                       mutate(NNVoverallinf=1/((vaccineeffectiveness*(sum(hospinfwovac)+sum(nonhospinfwovac)))/populationsize)),
                   novaccine<-novaccine%>%
                       mutate(NNVoverallinf=1/((vaccineeffectiveness*(sum(hospinfwovac[monthstartvac2():nrow(novaccine)])+sum(nonhospinfwovac[monthstartvac2():nrow(novaccine)])))/populationsize))
            )
            
            return(novaccine)
            
            
            
        } #close if current campaign
        
        
        
        if(scenario()=="Impact of hypothetical vaccination campaign"){
            
            ##########################################################################################
            # Scenario of influenza circulation in absence of vaccine: Absence to presence of vaccine
            # This is a scenario where the country has yet to conduct a vaccination campaign but would
            # like to see what impact a vaccine campaign may have.
            # The country enters the estimated number of hospitalization they observed and then we compare
            # this to a hypothetical vaccination campaign
            ##########################################################################################
            
            hospinf<-c(hospitalizationsmonthly(), hospmonth2(), hospmonth3(), hospmonth4(), hospmonth5(), hospmonth6(),
                       hospmonth7(), hospmonth8(), hospmonth9(), hospmonth10(), hospmonth11(), hospmonth12())
            
            populationsize<-as.numeric(as.character(populationsize()))
            hnh<-as.numeric(as.character(hnh()))
            mai<-as.numeric(as.character(mai()))
            hospinf<-as.numeric(as.character(hospinf))
            vaccineeffectiveness<-as.numeric(as.character(vaccineeffectiveness()))
            
            #####################Observed influenza in the absence of vaccine
            novaccine<-hospinf%>%
                as.data.frame()%>%
                rename(hospinfwovac=1)%>%
                mutate(hospinfcumulwovac=cumsum(hospinfwovac))%>%
                mutate(nonhospinfwovac=hospinfwovac*hnh)%>%
                mutate(nonhospinfcumulwovac=cumsum(nonhospinfwovac))%>%
                mutate(remainingsusc=populationsize-hospinfcumulwovac-nonhospinfcumulwovac)
            
            #create remainsuscstartmonth column with target population and rows 1:11 of remainingsusc
            remainsuscstartmonth<-populationsize%>%
                as.data.frame()%>%
                rename(remainsuscstartmonth=1)
            
            remainsuscstartmonth2<-as.data.frame(novaccine$remainingsusc[1:11])%>%
                as.data.frame()%>%
                rename(remainsuscstartmonth=1)
            
            remainsuscstartmonth<-rbind(remainsuscstartmonth, remainsuscstartmonth2)
            
            novaccine<-cbind(novaccine, remainsuscstartmonth)
            
            novaccine<-novaccine%>%
                mutate(overallinfwovac=hospinfwovac+nonhospinfwovac)%>%
                mutate(overallinfcumulwovac=cumsum(overallinfwovac))%>%
                mutate(medicallyattendedinfwovac=overallinfwovac*mai)%>%
                mutate(medicallyattendedinfcumulwovac=cumsum(medicallyattendedinfwovac))
            
            
            
            #####################################################################
            # Incidence rates in the absence of vaccine
            #####################################################################
            novaccine<-novaccine%>%
                mutate(irhospinf=hospinfwovac/remainsuscstartmonth)%>%
                mutate(irnonhospinf=nonhospinfwovac/remainsuscstartmonth)
            
            
            
            
            ############################ Burden inputs with hypothetical vaccination campaign
            #Create columns
            novaccine$hospinfwvac<-NA
            novaccine$hospinfcumulwvac<-NA
            novaccine$nonhospinfwvac<-NA
            novaccine$nonhospinfcumulwvac<-NA
            
            
            
            #Create first row
            novaccine[1,13]<-populationsize*novaccine$irhospinf[1]
            novaccine[1,14]<-novaccine$hospinfwvac[1]
            novaccine[1,15]<-populationsize*novaccine$irnonhospinf[1]
            novaccine[1,16]<-novaccine$nonhospinfwvac[1]
            
            ##bind monthly coverage
            monthvaccov<-c(vaccinecoveragemonthly(), month2(), month3(), month4(), month5(), month6(),
                           month7(), month8(), month9(), month10(), month11(), month12())
            
            monthvaccov<-as.numeric(as.character(monthvaccov))
            
            totcoverage<-sum(monthvaccov, na.rm=TRUE)
            
            
            novaccine<-cbind(novaccine, monthvaccov)
            
            novaccine$covergdistrib<- c(vaccinecoveragemonthly2(), month22(), month32(), month42(), month52(), month62(), month72(), month82(), month92(), month102(), month112(), month122())
            
            novaccine$covergdistrib<-as.numeric(as.character(novaccine$covergdistrib))
            
            novaccine$monthvaccov<- novaccine$covergdistrib*totcoverage
            
            
            ##monthly effectively vaccinated and susceptible with hypothetical vaccination campaign
            novaccine<-novaccine%>%
                mutate(effectivelyvacc=populationsize*monthvaccov*vaccineeffectiveness)%>%
                mutate(effectivelycumulvacc=cumsum(effectivelyvacc))%>%
                mutate(suscwvacc=populationsize-hospinfcumulwvac-nonhospinfcumulwvac-effectivelyvacc)
            
            ##Then calculate the rest of hospinfwovac and nonhospinfwovac. Calculation uses last month's incidence rate
            for(i in 2:nrow(novaccine)){
                novaccine$hospinfwvac[i]<-novaccine$suscwvacc[i-1]*novaccine$irhospinf[i]
                novaccine$hospinfcumulwvac[i]<-sum(novaccine$hospinfwvac[1:i])
                novaccine$nonhospinfwvac[i]<- novaccine$suscwvacc[i-1]*novaccine$irnonhospinf[i]
                novaccine$nonhospinfcumulwvac[i]<-sum(novaccine$nonhospinfwvac[1:i])
                novaccine$suscwvacc[i]=populationsize-novaccine$hospinfcumulwvac[i]-novaccine$nonhospinfcumulwvac[i]-novaccine$effectivelycumulvacc[i]
            }
            
            
            ## month number
            novaccine$monthnum<-c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
            
            
            ## Overall infections with hypothetical vaccination campaign
            novaccine<-novaccine%>%
                mutate(overallinfwvac=hospinfwvac+nonhospinfwvac)%>%
                mutate(overallinfcumulwvacc=cumsum(overallinfwvac))
            
            ## Medically attended with hypothetical vaccination campaign
            novaccine<-novaccine%>%
                mutate(medicalattedtoinfwvac=overallinfwvac*mai)
            
            
            ######## Avoided events
            novaccine<-novaccine%>%
                mutate(AEhospinf=hospinfwovac-hospinfwvac)%>%
                mutate(AEnonhospinf=nonhospinfwovac-nonhospinfwvac)%>%
                mutate(AEmedicalattedtoinf=medicallyattendedinfwovac-medicalattedtoinfwvac)%>%
                mutate(AEoverallinf=overallinfwovac-overallinfwvac)
            
            
            
            ################ Prevented fraction
            ## Hospitalized infections
            ifelse (monthstartvac2()==1,
                    novaccine<-novaccine%>%
                        mutate(PFhospinf=sum(AEhospinf)/sum(hospinfwovac)),
                    novaccine<-novaccine%>%
                        mutate(PFhospinf=sum(AEhospinf[monthstartvac2():nrow(novaccine)]/sum(hospinfwovac[monthstartvac2():nrow(novaccine)])))
            )
            
            ## non-Hospitalized infections
            ifelse (monthstartvac2()==1,
                    novaccine<-novaccine%>%
                        mutate(PFnonhospinf=sum(AEnonhospinf)/sum(nonhospinfwovac)),
                    novaccine<-novaccine%>%
                        mutate(PFnonhospinf=sum(AEnonhospinf[monthstartvac2():nrow(novaccine)]/sum(nonhospinfwovac[monthstartvac2():nrow(novaccine)])))
            )
            
            
            
            
            ################ NNV -----------------------------------> why does this only go to month 8 in the Excel formula?????
            ## Hospitalized infections
            ifelse(monthstartvac2()==1,
                   novaccine<-novaccine%>%
                       mutate(NNVhospinf=1/((vaccineeffectiveness*(sum(hospinfwovac)))/populationsize)),
                   novaccine<-novaccine%>%
                       mutate(NNVhospinf=1/((vaccineeffectiveness*(sum(hospinfwovac[monthstartvac2():nrow(novaccine)])))/populationsize))
            )
            
            
            ## non-Hospitalized infections
            ifelse(monthstartvac2()==1,
                   novaccine<-novaccine%>%
                       mutate(NNVnonhospinf=1/((vaccineeffectiveness*(sum(nonhospinfwovac)))/populationsize)),
                   novaccine<-novaccine%>%
                       mutate(NNVnonhospinf=1/((vaccineeffectiveness*(sum(nonhospinfwovac[monthstartvac2():nrow(novaccine)])))/populationsize))
            )
            
            ## Medically attended infections
            ifelse(monthstartvac2()==1,
                   novaccine<-novaccine%>%
                       mutate(NNVmedicalattendedinf=1/((vaccineeffectiveness*(sum(medicallyattendedinfwovac)))/populationsize)),
                   novaccine<-novaccine%>%
                       mutate(NNVmedicalattendedinf=1/((vaccineeffectiveness*(sum(medicallyattendedinfwovac[monthstartvac2():nrow(novaccine)])))/populationsize))
            )
            
            ## Total infections
            ifelse(monthstartvac2()==1,
                   novaccine<-novaccine%>%
                       mutate(NNVoverallinf=1/((vaccineeffectiveness*(sum(hospinfwovac)+sum(nonhospinfwovac)))/populationsize)),
                   novaccine<-novaccine%>%
                       mutate(NNVoverallinf=1/((vaccineeffectiveness*(sum(hospinfwovac[monthstartvac2():nrow(novaccine)])+sum(nonhospinfwovac[monthstartvac2():nrow(novaccine)])))/populationsize))
            )
            
            return(novaccine)
            
            
        }#close "if hypothetical campaign"
        
    }) #close burdendataset
    
    
    
    ####################################################################################################
    ## List of burden averted results
    ####################################################################################################
    
    
    burdenavertedresults_timing2<-reactive({
        
        populationsize<-as.numeric(as.character(populationsize()))
        hnh<-as.numeric(as.character(hnh()))
        mai<-as.numeric(as.character(mai()))
        vaccineeffectiveness<-as.numeric(as.character(vaccineeffectiveness()))
        
        
        
        #################################################
        ## Results
        #################################################
        nonhospinfwovactot<-sum(burdendataset_timing2()$nonhospinfwovac)
        nonhospinfwvacctot<-sum(burdendataset_timing2()$nonhospinfwvac)
        nonhospinfaverted<-nonhospinfwovactot-nonhospinfwvacctot
        NNVnonhospinffinal<-burdendataset_timing2()$NNVnonhospinf[1]
        
        medicallyattendedinfwovactot<-sum(burdendataset_timing2()$medicallyattendedinfwovac)
        medicallyattendedinfwvactot<-sum(burdendataset_timing2()$medicalattedtoinfwvac)
        medicallyattendedinfaverted<-medicallyattendedinfwovactot-medicallyattendedinfwvactot
        NNVmedicallyattendtedfinal<-burdendataset_timing2()$NNVmedicalattendedinf[1]
        
        Totalinfwovacc<-sum(burdendataset_timing2()$overallinfwovac)
        Totalinfwvacc<-sum(burdendataset_timing2()$overallinfwvac)
        Totalinfaverted<-Totalinfwovacc-Totalinfwvacc
        NNVtotalinf<-burdendataset_timing2()$NNVoverallinf[1]
        
        hospinfwovactot<-sum(burdendataset_timing2()$hospinfwovac)
        hospinfwvacctot<-sum(burdendataset_timing2()$hospinfwvac)
        hospaverted<-hospinfwovactot-hospinfwvacctot
        NNVhospinffinal<-burdendataset_timing2()$NNVhospinf[1]
        
        
        nonhospPreventedfraction<-burdendataset_timing2()$PFnonhospinf[1]
        hospPreventedfraction<-burdendataset_timing2()$PFhospinf[1]
        
        
        indicator_results2<-c(nonhospinfwovactot, nonhospinfwvacctot, nonhospinfaverted, NNVnonhospinffinal,
                              Totalinfwovacc, Totalinfwvacc, Totalinfaverted, NNVtotalinf,
                              hospinfwovactot, hospinfwvacctot, hospaverted, NNVhospinffinal,
                              medicallyattendedinfwovactot, medicallyattendedinfwvactot, medicallyattendedinfaverted, NNVmedicallyattendtedfinal,
                              nonhospPreventedfraction)
        
        indicator_results_titles<-c("nonhospinfwovactot", "nonhospinfwvacctot", "nonhospinfaverted", "NNVnonhospinffinal",
                                    "Totalinfwovacc", "Totalinfwvacc", "Totalinfaverted", "NNVtotalinf",
                                    "hospinfwovactot", "hospinfwvacctot", "hospaverted", "NNVhospinffinal",
                                    "medicallyattendedinfwovactot", "medicallyattendedinfwvactot", "medicallyattendedinfaverted", "NNVmedicallyattendtedfinal",
                                    "nonhospPreventedfraction")
        
        indicator_results2<-cbind(indicator_results_titles, indicator_results2)
        
        
        return(indicator_results2)
        
        
    })#burdenavertedresults_timing2
    
    
    
    
    #######################################################################################################################################################################################
    
    ##Deployment timing 3
    #######################################################################################################################################################################################
    
    
    burdendataset_timing3<- reactive({
        ################################ The output data when insufficient data has been entered   ##################################
        ############################################################################################################################
        
        
        if(scenario()=="Impact of current influenza vaccination campaign"){
            
            
            ##########################################################################################
            # Scenario of influenza circulation in presence of vaccine: Presence to absence of vaccine
            # This is a situation where the country conducts a vaccination campaign, we enter
            # actual burden inputs and vaccine coverage then compare this to a hypothetical scenario
            # where there is no vaccination campaign conducted
            ##########################################################################################
            
            
            
            
            hospinf<-c(hospitalizationsmonthly(), hospmonth2(), hospmonth3(), hospmonth4(), hospmonth5(), hospmonth6(),
                       hospmonth7(), hospmonth8(), hospmonth9(), hospmonth10(), hospmonth11(), hospmonth12())
            
            populationsize<-as.numeric(as.character(populationsize()))
            vaccineeffectiveness<-as.numeric(as.character(vaccineeffectiveness()))
            hnh<-as.numeric(as.character(hnh()))
            mai<-as.numeric(as.character(mai()))
            hospinf<-as.numeric(as.character(hospinf))
            
            novaccine<-hospinf%>%
                as.data.frame()%>%
                rename(hospinfwvac=1)%>%
                mutate(hospinfcumulwvac=cumsum(hospinfwvac))%>%
                mutate(nonhospinfwvac=hospinfwvac*hnh)%>%
                mutate(nonhospinfcumulwvac=cumsum(nonhospinfwvac))%>%
                mutate(overallinfwvac=hospinfwvac+nonhospinfwvac)%>%
                mutate(overallinfcumulwvac=cumsum(overallinfwvac))%>%
                mutate(medicalattedtoinfwvac=overallinfwvac*mai)%>%
                mutate(medicallyattendedinfcumulwvac=cumsum(medicalattedtoinfwvac))
            
            
            
            ##bind monthly coverage
            monthvaccov<-c(vaccinecoveragemonthly(), month2(), month3(), month4(), month5(), month6(),
                           month7(), month8(), month9(), month10(), month11(), month12())
            
            monthvaccov<-as.numeric(as.character(monthvaccov))
            
            totcoverage<-sum(monthvaccov, na.rm=TRUE)
            
            
            novaccine<-cbind(novaccine, monthvaccov)
            
            novaccine$covergdistrib<- c(vaccinecoveragemonthly3(), month23(), month33(), month43(), month53(), month63(), month73(), month83(), month93(), month103(), month113(), month123())
            
            novaccine$covergdistrib<-as.numeric(as.character(novaccine$covergdistrib))
            
            novaccine$monthvaccov<- novaccine$covergdistrib*totcoverage
            
            
            ## month number
            novaccine$monthnum<-c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
            
            ## add monthly effectively vaccinated
            novaccine<-novaccine%>%
                mutate(effectivelyvacc=populationsize*monthvaccov*vaccineeffectiveness)%>%
                mutate(effectivelyvacccumul=cumsum(effectivelyvacc))
            
            
            
            
            #####################################################################
            # Estimated rates
            #####################################################################
            novaccine<-novaccine%>%
                mutate(susceptiblewvacc=populationsize-hospinfcumulwvac-nonhospinfcumulwvac-effectivelyvacccumul)
            
            #Create columns
            novaccine$irhospinf<-NA
            novaccine$irnonhospinf<-NA
            
            #Create first row
            novaccine[1,14]<-novaccine$hospinfwvac[1]/populationsize
            novaccine[1,15]<-novaccine$nonhospinfwvac[1]/populationsize
            
            
            ##Calculation uses last month's susceptible population
            for(i in 2:nrow(novaccine)){
                novaccine$irhospinf[i]<-novaccine$hospinfwvac[i]/novaccine$susceptiblewvacc[i-1]
                novaccine$irnonhospinf[i]<- novaccine$nonhospinfwvac[i]/novaccine$susceptiblewvacc[i-1]
            }
            
            #####################################################################
            # Scenario of influenza circulation in absence of vaccine
            #####################################################################
            
            ## These calculations are initialized by the 'Susceptible at start of month in absence of vaccine'
            ## column, which starts with the Target Population in the first month.
            
            remainsuscstartmonth<-c(populationsize, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
            novaccine<-cbind(novaccine, remainsuscstartmonth)
            
            #Create columns
            novaccine$hospinfwovac<-NA
            novaccine$hospinfcumulwovacc<-NA
            novaccine$nonhospinfwovac<-NA
            novaccine$nonhospinfcumulwovacc<-NA
            novaccine$remainingsusc<-NA
            
            #Create first row
            novaccine[1,17]<-novaccine$remainsuscstartmonth[1]*novaccine$irhospinf[1]
            novaccine[1,18]<-novaccine$hospinfwovac[1]
            novaccine[1,19]<-novaccine$remainsuscstartmonth[1]*novaccine$irnonhospinf[1]
            novaccine[1,20]<-novaccine$nonhospinfwovac[1]
            novaccine[1,21]<-populationsize-novaccine$hospinfcumulwovacc[1]-novaccine$nonhospinfcumulwovacc[1]
            
            
            #Create rows 2:12
            for(i in 2:nrow(novaccine)){
                novaccine$remainsuscstartmonth[i]<-novaccine$remainingsusc[i-1]
                novaccine$hospinfwovac[i]<-novaccine$remainsuscstartmonth[i]*novaccine$irhospinf[i]
                novaccine$hospinfcumulwovacc[i]<-sum(novaccine$hospinfwovac[1:i])
                novaccine$nonhospinfwovac[i]<-novaccine$remainsuscstartmonth[i]*novaccine$irnonhospinf[i]
                novaccine$nonhospinfcumulwovacc[i]<-sum(novaccine$nonhospinfwovac[1:i])
                novaccine$remainingsusc[i]<-populationsize-novaccine$hospinfcumulwovacc[i]-novaccine$nonhospinfcumulwovacc[i]
            }
            
            novaccine<-novaccine%>%
                mutate(overallinfwovac=hospinfwovac+nonhospinfwovac)%>%
                mutate(overallinfcumulwovacc=cumsum(overallinfwovac))%>%
                mutate(medicallyattendedinfwovac=overallinfwovac*mai)%>%
                mutate(medicalattedtoinfcumulwovacc=cumsum(medicallyattendedinfwovac))
            
            ######## Avoided events
            novaccine<-novaccine%>%
                mutate(AEhospinf=hospinfwovac-hospinfwvac)%>%
                mutate(AEnonhospinf=nonhospinfwovac-nonhospinfwvac)%>%
                mutate(AEmedicalattedtoinf=medicallyattendedinfwovac-medicalattedtoinfwvac)%>%
                mutate(AEoverallinf=overallinfwovac-overallinfwvac)
            
            
            ################ Prevented fraction
            ## Hospitalized infections
            ifelse (monthstartvac3()==1,
                    novaccine<-novaccine%>%
                        mutate(PFhospinf=sum(AEhospinf)/sum(hospinfwovac)),
                    novaccine<-novaccine%>%
                        mutate(PFhospinf=sum(AEhospinf[monthstartvac3():nrow(novaccine)]/sum(hospinfwovac[monthstartvac3():nrow(novaccine)])))
            )
            
            ## non-Hospitalized infections
            ifelse (monthstartvac3()==1,
                    novaccine<-novaccine%>%
                        mutate(PFnonhospinf=sum(AEnonhospinf)/sum(nonhospinfwovac)),
                    novaccine<-novaccine%>%
                        mutate(PFnonhospinf=sum(AEnonhospinf[monthstartvac3():nrow(novaccine)]/sum(nonhospinfwovac[monthstartvac3():nrow(novaccine)])))
            )
            
            
            ################ NNV -----------------------------------> why does this only go to month 8 in the Excel formula?????
            ## Hospitalized infections
            ifelse(monthstartvac3()==1,
                   novaccine<-novaccine%>%
                       mutate(NNVhospinf=1/((vaccineeffectiveness*(sum(hospinfwovac)))/populationsize)),
                   novaccine<-novaccine%>%
                       mutate(NNVhospinf=1/((vaccineeffectiveness*(sum(hospinfwovac[monthstartvac3():nrow(novaccine)])))/populationsize))
            )
            
            
            ## non-Hospitalized infections
            ifelse(monthstartvac3()==1,
                   novaccine<-novaccine%>%
                       mutate(NNVnonhospinf=1/((vaccineeffectiveness*(sum(nonhospinfwovac)))/populationsize)),
                   novaccine<-novaccine%>%
                       mutate(NNVnonhospinf=1/((vaccineeffectiveness*(sum(nonhospinfwovac[monthstartvac3():nrow(novaccine)])))/populationsize))
            )
            
            ## Medically infections
            ifelse(monthstartvac3()==1,
                   novaccine<-novaccine%>%
                       mutate(NNVmedicalattendedinf=1/((vaccineeffectiveness*(sum(medicallyattendedinfwovac)))/populationsize)),
                   novaccine<-novaccine%>%
                       mutate(NNVmedicalattendedinf=1/((vaccineeffectiveness*(sum(medicallyattendedinfwovac[monthstartvac3():nrow(novaccine)])))/populationsize))
            )
            
            ## Overall infections
            ifelse(monthstartvac3()==1,
                   novaccine<-novaccine%>%
                       mutate(NNVoverallinf=1/((vaccineeffectiveness*(sum(hospinfwovac)+sum(nonhospinfwovac)))/populationsize)),
                   novaccine<-novaccine%>%
                       mutate(NNVoverallinf=1/((vaccineeffectiveness*(sum(hospinfwovac[monthstartvac3():nrow(novaccine)])+sum(nonhospinfwovac[monthstartvac3():nrow(novaccine)])))/populationsize))
            )
            
            return(novaccine)
            
            
            
        } #close if current campaign
        
        
        
        if(scenario()=="Impact of hypothetical vaccination campaign"){
            
            ##########################################################################################
            # Scenario of influenza circulation in absence of vaccine: Absence to presence of vaccine
            # This is a scenario where the country has yet to conduct a vaccination campaign but would
            # like to see what impact a vaccine campaign may have.
            # The country enters the estimated number of hospitalization they observed and then we compare
            # this to a hypothetical vaccination campaign
            ##########################################################################################
            
            hospinf<-c(hospitalizationsmonthly(), hospmonth2(), hospmonth3(), hospmonth4(), hospmonth5(), hospmonth6(),
                       hospmonth7(), hospmonth8(), hospmonth9(), hospmonth10(), hospmonth11(), hospmonth12())
            
            populationsize<-as.numeric(as.character(populationsize()))
            hnh<-as.numeric(as.character(hnh()))
            mai<-as.numeric(as.character(mai()))
            hospinf<-as.numeric(as.character(hospinf))
            vaccineeffectiveness<-as.numeric(as.character(vaccineeffectiveness()))
            
            #####################Observed influenza in the absence of vaccine
            novaccine<-hospinf%>%
                as.data.frame()%>%
                rename(hospinfwovac=1)%>%
                mutate(hospinfcumulwovac=cumsum(hospinfwovac))%>%
                mutate(nonhospinfwovac=hospinfwovac*hnh)%>%
                mutate(nonhospinfcumulwovac=cumsum(nonhospinfwovac))%>%
                mutate(remainingsusc=populationsize-hospinfcumulwovac-nonhospinfcumulwovac)
            
            #create remainsuscstartmonth column with target population and rows 1:11 of remainingsusc
            remainsuscstartmonth<-populationsize%>%
                as.data.frame()%>%
                rename(remainsuscstartmonth=1)
            
            remainsuscstartmonth2<-as.data.frame(novaccine$remainingsusc[1:11])%>%
                as.data.frame()%>%
                rename(remainsuscstartmonth=1)
            
            remainsuscstartmonth<-rbind(remainsuscstartmonth, remainsuscstartmonth2)
            
            novaccine<-cbind(novaccine, remainsuscstartmonth)
            
            novaccine<-novaccine%>%
                mutate(overallinfwovac=hospinfwovac+nonhospinfwovac)%>%
                mutate(overallinfcumulwovac=cumsum(overallinfwovac))%>%
                mutate(medicallyattendedinfwovac=overallinfwovac*mai)%>%
                mutate(medicallyattendedinfcumulwovac=cumsum(medicallyattendedinfwovac))
            
            
            
            #####################################################################
            # Incidence rates in the absence of vaccine
            #####################################################################
            novaccine<-novaccine%>%
                mutate(irhospinf=hospinfwovac/remainsuscstartmonth)%>%
                mutate(irnonhospinf=nonhospinfwovac/remainsuscstartmonth)
            
            
            
            
            ############################ Burden inputs with hypothetical vaccination campaign
            #Create columns
            novaccine$hospinfwvac<-NA
            novaccine$hospinfcumulwvac<-NA
            novaccine$nonhospinfwvac<-NA
            novaccine$nonhospinfcumulwvac<-NA
            
            
            
            #Create first row
            novaccine[1,13]<-populationsize*novaccine$irhospinf[1]
            novaccine[1,14]<-novaccine$hospinfwvac[1]
            novaccine[1,15]<-populationsize*novaccine$irnonhospinf[1]
            novaccine[1,16]<-novaccine$nonhospinfwvac[1]
            
            ##bind monthly coverage
            monthvaccov<-c(vaccinecoveragemonthly(), month2(), month3(), month4(), month5(), month6(),
                           month7(), month8(), month9(), month10(), month11(), month12())
            
            monthvaccov<-as.numeric(as.character(monthvaccov))
            
            totcoverage<-sum(monthvaccov, na.rm=TRUE)
            
            
            novaccine<-cbind(novaccine, monthvaccov)
            
            novaccine$covergdistrib<- c(vaccinecoveragemonthly3(), month23(), month33(), month43(), month53(), month63(), month73(), month83(), month93(), month103(), month113(), month123())
            
            novaccine$covergdistrib<-as.numeric(as.character(novaccine$covergdistrib))
            
            novaccine$monthvaccov<- novaccine$covergdistrib*totcoverage
            
            
            ##monthly effectively vaccinated and susceptible with hypothetical vaccination campaign
            novaccine<-novaccine%>%
                mutate(effectivelyvacc=populationsize*monthvaccov*vaccineeffectiveness)%>%
                mutate(effectivelycumulvacc=cumsum(effectivelyvacc))%>%
                mutate(suscwvacc=populationsize-hospinfcumulwvac-nonhospinfcumulwvac-effectivelyvacc)
            
            ##Then calculate the rest of hospinfwovac and nonhospinfwovac. Calculation uses last month's incidence rate
            for(i in 2:nrow(novaccine)){
                novaccine$hospinfwvac[i]<-novaccine$suscwvacc[i-1]*novaccine$irhospinf[i]
                novaccine$hospinfcumulwvac[i]<-sum(novaccine$hospinfwvac[1:i])
                novaccine$nonhospinfwvac[i]<- novaccine$suscwvacc[i-1]*novaccine$irnonhospinf[i]
                novaccine$nonhospinfcumulwvac[i]<-sum(novaccine$nonhospinfwvac[1:i])
                novaccine$suscwvacc[i]=populationsize-novaccine$hospinfcumulwvac[i]-novaccine$nonhospinfcumulwvac[i]-novaccine$effectivelycumulvacc[i]
            }
            
            
            ## month number
            novaccine$monthnum<-c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
            
            
            ## Overall infections with hypothetical vaccination campaign
            novaccine<-novaccine%>%
                mutate(overallinfwvac=hospinfwvac+nonhospinfwvac)%>%
                mutate(overallinfcumulwvacc=cumsum(overallinfwvac))
            
            ## Medically attended with hypothetical vaccination campaign
            novaccine<-novaccine%>%
                mutate(medicalattedtoinfwvac=overallinfwvac*mai)
            
            
            ######## Avoided events
            novaccine<-novaccine%>%
                mutate(AEhospinf=hospinfwovac-hospinfwvac)%>%
                mutate(AEnonhospinf=nonhospinfwovac-nonhospinfwvac)%>%
                mutate(AEmedicalattedtoinf=medicallyattendedinfwovac-medicalattedtoinfwvac)%>%
                mutate(AEoverallinf=overallinfwovac-overallinfwvac)
            
            
            
            ################ Prevented fraction
            ## Hospitalized infections
            ifelse (monthstartvac3()==1,
                    novaccine<-novaccine%>%
                        mutate(PFhospinf=sum(AEhospinf)/sum(hospinfwovac)),
                    novaccine<-novaccine%>%
                        mutate(PFhospinf=sum(AEhospinf[monthstartvac3():nrow(novaccine)]/sum(hospinfwovac[monthstartvac3():nrow(novaccine)])))
            )
            
            ## non-Hospitalized infections
            ifelse (monthstartvac3()==1,
                    novaccine<-novaccine%>%
                        mutate(PFnonhospinf=sum(AEnonhospinf)/sum(nonhospinfwovac)),
                    novaccine<-novaccine%>%
                        mutate(PFnonhospinf=sum(AEnonhospinf[monthstartvac3():nrow(novaccine)]/sum(nonhospinfwovac[monthstartvac3():nrow(novaccine)])))
            )
            
            
            
            
            ################ NNV -----------------------------------> why does this only go to month 8 in the Excel formula?????
            ## Hospitalized infections
            ifelse(monthstartvac3()==1,
                   novaccine<-novaccine%>%
                       mutate(NNVhospinf=1/((vaccineeffectiveness*(sum(hospinfwovac)))/populationsize)),
                   novaccine<-novaccine%>%
                       mutate(NNVhospinf=1/((vaccineeffectiveness*(sum(hospinfwovac[monthstartvac3():nrow(novaccine)])))/populationsize))
            )
            
            
            ## non-Hospitalized infections
            ifelse(monthstartvac3()==1,
                   novaccine<-novaccine%>%
                       mutate(NNVnonhospinf=1/((vaccineeffectiveness*(sum(nonhospinfwovac)))/populationsize)),
                   novaccine<-novaccine%>%
                       mutate(NNVnonhospinf=1/((vaccineeffectiveness*(sum(nonhospinfwovac[monthstartvac3():nrow(novaccine)])))/populationsize))
            )
            
            ## Medically attended infections
            ifelse(monthstartvac3()==1,
                   novaccine<-novaccine%>%
                       mutate(NNVmedicalattendedinf=1/((vaccineeffectiveness*(sum(medicallyattendedinfwovac)))/populationsize)),
                   novaccine<-novaccine%>%
                       mutate(NNVmedicalattendedinf=1/((vaccineeffectiveness*(sum(medicallyattendedinfwovac[monthstartvac3():nrow(novaccine)])))/populationsize))
            )
            
            ## Total infections
            ifelse(monthstartvac3()==1,
                   novaccine<-novaccine%>%
                       mutate(NNVoverallinf=1/((vaccineeffectiveness*(sum(hospinfwovac)+sum(nonhospinfwovac)))/populationsize)),
                   novaccine<-novaccine%>%
                       mutate(NNVoverallinf=1/((vaccineeffectiveness*(sum(hospinfwovac[monthstartvac3():nrow(novaccine)])+sum(nonhospinfwovac[monthstartvac3():nrow(novaccine)])))/populationsize))
            )
            
            return(novaccine)
            
            
        }#close "if hypothetical campaign"
        
    }) #close burdendataset
    
    
    
    ####################################################################################################
    ## List of burden averted results
    ####################################################################################################
    
    
    burdenavertedresults_timing3<-reactive({
        
        populationsize<-as.numeric(as.character(populationsize()))
        hnh<-as.numeric(as.character(hnh()))
        mai<-as.numeric(as.character(mai()))
        vaccineeffectiveness<-as.numeric(as.character(vaccineeffectiveness()))
        
        
        
        #################################################
        ## Results
        #################################################
        nonhospinfwovactot<-sum(burdendataset_timing3()$nonhospinfwovac)
        nonhospinfwvacctot<-sum(burdendataset_timing3()$nonhospinfwvac)
        nonhospinfaverted<-nonhospinfwovactot-nonhospinfwvacctot
        NNVnonhospinffinal<-burdendataset_timing3()$NNVnonhospinf[1]
        
        medicallyattendedinfwovactot<-sum(burdendataset_timing3()$medicallyattendedinfwovac)
        medicallyattendedinfwvactot<-sum(burdendataset_timing3()$medicalattedtoinfwvac)
        medicallyattendedinfaverted<-medicallyattendedinfwovactot-medicallyattendedinfwvactot
        NNVmedicallyattendtedfinal<-burdendataset_timing3()$NNVmedicalattendedinf[1]
        
        Totalinfwovacc<-sum(burdendataset_timing3()$overallinfwovac)
        Totalinfwvacc<-sum(burdendataset_timing3()$overallinfwvac)
        Totalinfaverted<-Totalinfwovacc-Totalinfwvacc
        NNVtotalinf<-burdendataset_timing3()$NNVoverallinf[1]
        
        hospinfwovactot<-sum(burdendataset_timing3()$hospinfwovac)
        hospinfwvacctot<-sum(burdendataset_timing3()$hospinfwvac)
        hospaverted<-hospinfwovactot-hospinfwvacctot
        NNVhospinffinal<-burdendataset_timing3()$NNVhospinf[1]
        
        
        nonhospPreventedfraction<-burdendataset_timing3()$PFnonhospinf[1]
        hospPreventedfraction<-burdendataset_timing3()$PFhospinf[1]
        
        
        indicator_results3<-c(nonhospinfwovactot, nonhospinfwvacctot, nonhospinfaverted, NNVnonhospinffinal,
                              Totalinfwovacc, Totalinfwvacc, Totalinfaverted, NNVtotalinf,
                              hospinfwovactot, hospinfwvacctot, hospaverted, NNVhospinffinal,
                              medicallyattendedinfwovactot, medicallyattendedinfwvactot, medicallyattendedinfaverted, NNVmedicallyattendtedfinal,
                              nonhospPreventedfraction)
        
        indicator_results_titles<-c("nonhospinfwovactot", "nonhospinfwvacctot", "nonhospinfaverted", "NNVnonhospinffinal",
                                    "Totalinfwovacc", "Totalinfwvacc", "Totalinfaverted", "NNVtotalinf",
                                    "hospinfwovactot", "hospinfwvacctot", "hospaverted", "NNVhospinffinal",
                                    "medicallyattendedinfwovactot", "medicallyattendedinfwvactot", "medicallyattendedinfaverted", "NNVmedicallyattendtedfinal",
                                    "nonhospPreventedfraction")
        
        indicator_results3<-cbind(indicator_results_titles, indicator_results3)
        
        
        return(indicator_results3)
        
        
    })#burdenavertedresults_timing3
    
    
    
    
    
    
    #######################################################################################################################################################################################
    
    ##Deployment timing 4
    #######################################################################################################################################################################################
    
    
    burdendataset_timing4<- reactive({
        ################################ The output data when insufficient data has been entered   ##################################
        ############################################################################################################################
        
        
        if(scenario()=="Impact of current influenza vaccination campaign"){
            
            
            ##########################################################################################
            # Scenario of influenza circulation in presence of vaccine: Presence to absence of vaccine
            # This is a situation where the country conducts a vaccination campaign, we enter
            # actual burden inputs and vaccine coverage then compare this to a hypothetical scenario
            # where there is no vaccination campaign conducted
            ##########################################################################################
            
            
            
            
            hospinf<-c(hospitalizationsmonthly(), hospmonth2(), hospmonth3(), hospmonth4(), hospmonth5(), hospmonth6(),
                       hospmonth7(), hospmonth8(), hospmonth9(), hospmonth10(), hospmonth11(), hospmonth12())
            
            populationsize<-as.numeric(as.character(populationsize()))
            vaccineeffectiveness<-as.numeric(as.character(vaccineeffectiveness()))
            hnh<-as.numeric(as.character(hnh()))
            mai<-as.numeric(as.character(mai()))
            hospinf<-as.numeric(as.character(hospinf))
            
            novaccine<-hospinf%>%
                as.data.frame()%>%
                rename(hospinfwvac=1)%>%
                mutate(hospinfcumulwvac=cumsum(hospinfwvac))%>%
                mutate(nonhospinfwvac=hospinfwvac*hnh)%>%
                mutate(nonhospinfcumulwvac=cumsum(nonhospinfwvac))%>%
                mutate(overallinfwvac=hospinfwvac+nonhospinfwvac)%>%
                mutate(overallinfcumulwvac=cumsum(overallinfwvac))%>%
                mutate(medicalattedtoinfwvac=overallinfwvac*mai)%>%
                mutate(medicallyattendedinfcumulwvac=cumsum(medicalattedtoinfwvac))
            
            
            
            ##bind monthly coverage
            monthvaccov<-c(vaccinecoveragemonthly(), month2(), month3(), month4(), month5(), month6(),
                           month7(), month8(), month9(), month10(), month11(), month12())
            
            monthvaccov<-as.numeric(as.character(monthvaccov))
            
            totcoverage<-sum(monthvaccov, na.rm=TRUE)
            
            
            novaccine<-cbind(novaccine, monthvaccov)
            
            novaccine$covergdistrib<- c(vaccinecoveragemonthly4(), month24(), month34(), month44(), month54(), month64(), month74(), month84(), month94(), month104(), month114(), month124())
            
            novaccine$covergdistrib<-as.numeric(as.character(novaccine$covergdistrib))
            
            novaccine$monthvaccov<- novaccine$covergdistrib*totcoverage
            
            
            ## month number
            novaccine$monthnum<-c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
            
            ## add monthly effectively vaccinated
            novaccine<-novaccine%>%
                mutate(effectivelyvacc=populationsize*monthvaccov*vaccineeffectiveness)%>%
                mutate(effectivelyvacccumul=cumsum(effectivelyvacc))
            
            
            
            
            #####################################################################
            # Estimated rates
            #####################################################################
            novaccine<-novaccine%>%
                mutate(susceptiblewvacc=populationsize-hospinfcumulwvac-nonhospinfcumulwvac-effectivelyvacccumul)
            
            #Create columns
            novaccine$irhospinf<-NA
            novaccine$irnonhospinf<-NA
            
            #Create first row
            novaccine[1,14]<-novaccine$hospinfwvac[1]/populationsize
            novaccine[1,15]<-novaccine$nonhospinfwvac[1]/populationsize
            
            
            ##Calculation uses last month's susceptible population
            for(i in 2:nrow(novaccine)){
                novaccine$irhospinf[i]<-novaccine$hospinfwvac[i]/novaccine$susceptiblewvacc[i-1]
                novaccine$irnonhospinf[i]<- novaccine$nonhospinfwvac[i]/novaccine$susceptiblewvacc[i-1]
            }
            
            #####################################################################
            # Scenario of influenza circulation in absence of vaccine
            #####################################################################
            
            ## These calculations are initialized by the 'Susceptible at start of month in absence of vaccine'
            ## column, which starts with the Target Population in the first month.
            
            remainsuscstartmonth<-c(populationsize, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
            novaccine<-cbind(novaccine, remainsuscstartmonth)
            
            #Create columns
            novaccine$hospinfwovac<-NA
            novaccine$hospinfcumulwovacc<-NA
            novaccine$nonhospinfwovac<-NA
            novaccine$nonhospinfcumulwovacc<-NA
            novaccine$remainingsusc<-NA
            
            #Create first row
            novaccine[1,17]<-novaccine$remainsuscstartmonth[1]*novaccine$irhospinf[1]
            novaccine[1,18]<-novaccine$hospinfwovac[1]
            novaccine[1,19]<-novaccine$remainsuscstartmonth[1]*novaccine$irnonhospinf[1]
            novaccine[1,20]<-novaccine$nonhospinfwovac[1]
            novaccine[1,21]<-populationsize-novaccine$hospinfcumulwovacc[1]-novaccine$nonhospinfcumulwovacc[1]
            
            
            #Create rows 2:12
            for(i in 2:nrow(novaccine)){
                novaccine$remainsuscstartmonth[i]<-novaccine$remainingsusc[i-1]
                novaccine$hospinfwovac[i]<-novaccine$remainsuscstartmonth[i]*novaccine$irhospinf[i]
                novaccine$hospinfcumulwovacc[i]<-sum(novaccine$hospinfwovac[1:i])
                novaccine$nonhospinfwovac[i]<-novaccine$remainsuscstartmonth[i]*novaccine$irnonhospinf[i]
                novaccine$nonhospinfcumulwovacc[i]<-sum(novaccine$nonhospinfwovac[1:i])
                novaccine$remainingsusc[i]<-populationsize-novaccine$hospinfcumulwovacc[i]-novaccine$nonhospinfcumulwovacc[i]
            }
            
            novaccine<-novaccine%>%
                mutate(overallinfwovac=hospinfwovac+nonhospinfwovac)%>%
                mutate(overallinfcumulwovacc=cumsum(overallinfwovac))%>%
                mutate(medicallyattendedinfwovac=overallinfwovac*mai)%>%
                mutate(medicalattedtoinfcumulwovacc=cumsum(medicallyattendedinfwovac))
            
            ######## Avoided events
            novaccine<-novaccine%>%
                mutate(AEhospinf=hospinfwovac-hospinfwvac)%>%
                mutate(AEnonhospinf=nonhospinfwovac-nonhospinfwvac)%>%
                mutate(AEmedicalattedtoinf=medicallyattendedinfwovac-medicalattedtoinfwvac)%>%
                mutate(AEoverallinf=overallinfwovac-overallinfwvac)
            
            
            ################ Prevented fraction
            ## Hospitalized infections
            ifelse (monthstartvac4()==1,
                    novaccine<-novaccine%>%
                        mutate(PFhospinf=sum(AEhospinf)/sum(hospinfwovac)),
                    novaccine<-novaccine%>%
                        mutate(PFhospinf=sum(AEhospinf[monthstartvac4():nrow(novaccine)]/sum(hospinfwovac[monthstartvac4():nrow(novaccine)])))
            )
            
            ## non-Hospitalized infections
            ifelse (monthstartvac4()==1,
                    novaccine<-novaccine%>%
                        mutate(PFnonhospinf=sum(AEnonhospinf)/sum(nonhospinfwovac)),
                    novaccine<-novaccine%>%
                        mutate(PFnonhospinf=sum(AEnonhospinf[monthstartvac4():nrow(novaccine)]/sum(nonhospinfwovac[monthstartvac4():nrow(novaccine)])))
            )
            
            
            ################ NNV -----------------------------------> why does this only go to month 8 in the Excel formula?????
            ## Hospitalized infections
            ifelse(monthstartvac4()==1,
                   novaccine<-novaccine%>%
                       mutate(NNVhospinf=1/((vaccineeffectiveness*(sum(hospinfwovac)))/populationsize)),
                   novaccine<-novaccine%>%
                       mutate(NNVhospinf=1/((vaccineeffectiveness*(sum(hospinfwovac[monthstartvac2():nrow(novaccine)])))/populationsize))
            )
            
            
            ## non-Hospitalized infections
            ifelse(monthstartvac4()==1,
                   novaccine<-novaccine%>%
                       mutate(NNVnonhospinf=1/((vaccineeffectiveness*(sum(nonhospinfwovac)))/populationsize)),
                   novaccine<-novaccine%>%
                       mutate(NNVnonhospinf=1/((vaccineeffectiveness*(sum(nonhospinfwovac[monthstartvac4():nrow(novaccine)])))/populationsize))
            )
            
            ## Medically infections
            ifelse(monthstartvac4()==1,
                   novaccine<-novaccine%>%
                       mutate(NNVmedicalattendedinf=1/((vaccineeffectiveness*(sum(medicallyattendedinfwovac)))/populationsize)),
                   novaccine<-novaccine%>%
                       mutate(NNVmedicalattendedinf=1/((vaccineeffectiveness*(sum(medicallyattendedinfwovac[monthstartvac4():nrow(novaccine)])))/populationsize))
            )
            
            ## Overall infections
            ifelse(monthstartvac4()==1,
                   novaccine<-novaccine%>%
                       mutate(NNVoverallinf=1/((vaccineeffectiveness*(sum(hospinfwovac)+sum(nonhospinfwovac)))/populationsize)),
                   novaccine<-novaccine%>%
                       mutate(NNVoverallinf=1/((vaccineeffectiveness*(sum(hospinfwovac[monthstartvac4():nrow(novaccine)])+sum(nonhospinfwovac[monthstartvac4():nrow(novaccine)])))/populationsize))
            )
            
            return(novaccine)
            
            
            
        } #close if current campaign
        
        
        
        if(scenario()=="Impact of hypothetical vaccination campaign"){
            
            ##########################################################################################
            # Scenario of influenza circulation in absence of vaccine: Absence to presence of vaccine
            # This is a scenario where the country has yet to conduct a vaccination campaign but would
            # like to see what impact a vaccine campaign may have.
            # The country enters the estimated number of hospitalization they observed and then we compare
            # this to a hypothetical vaccination campaign
            ##########################################################################################
            
            hospinf<-c(hospitalizationsmonthly(), hospmonth2(), hospmonth3(), hospmonth4(), hospmonth5(), hospmonth6(),
                       hospmonth7(), hospmonth8(), hospmonth9(), hospmonth10(), hospmonth11(), hospmonth12())
            
            populationsize<-as.numeric(as.character(populationsize()))
            hnh<-as.numeric(as.character(hnh()))
            mai<-as.numeric(as.character(mai()))
            hospinf<-as.numeric(as.character(hospinf))
            vaccineeffectiveness<-as.numeric(as.character(vaccineeffectiveness()))
            
            #####################Observed influenza in the absence of vaccine
            novaccine<-hospinf%>%
                as.data.frame()%>%
                rename(hospinfwovac=1)%>%
                mutate(hospinfcumulwovac=cumsum(hospinfwovac))%>%
                mutate(nonhospinfwovac=hospinfwovac*hnh)%>%
                mutate(nonhospinfcumulwovac=cumsum(nonhospinfwovac))%>%
                mutate(remainingsusc=populationsize-hospinfcumulwovac-nonhospinfcumulwovac)
            
            #create remainsuscstartmonth column with target population and rows 1:11 of remainingsusc
            remainsuscstartmonth<-populationsize%>%
                as.data.frame()%>%
                rename(remainsuscstartmonth=1)
            
            remainsuscstartmonth2<-as.data.frame(novaccine$remainingsusc[1:11])%>%
                as.data.frame()%>%
                rename(remainsuscstartmonth=1)
            
            remainsuscstartmonth<-rbind(remainsuscstartmonth, remainsuscstartmonth2)
            
            novaccine<-cbind(novaccine, remainsuscstartmonth)
            
            novaccine<-novaccine%>%
                mutate(overallinfwovac=hospinfwovac+nonhospinfwovac)%>%
                mutate(overallinfcumulwovac=cumsum(overallinfwovac))%>%
                mutate(medicallyattendedinfwovac=overallinfwovac*mai)%>%
                mutate(medicallyattendedinfcumulwovac=cumsum(medicallyattendedinfwovac))
            
            
            
            #####################################################################
            # Incidence rates in the absence of vaccine
            #####################################################################
            novaccine<-novaccine%>%
                mutate(irhospinf=hospinfwovac/remainsuscstartmonth)%>%
                mutate(irnonhospinf=nonhospinfwovac/remainsuscstartmonth)
            
            
            
            
            ############################ Burden inputs with hypothetical vaccination campaign
            #Create columns
            novaccine$hospinfwvac<-NA
            novaccine$hospinfcumulwvac<-NA
            novaccine$nonhospinfwvac<-NA
            novaccine$nonhospinfcumulwvac<-NA
            
            
            
            #Create first row
            novaccine[1,13]<-populationsize*novaccine$irhospinf[1]
            novaccine[1,14]<-novaccine$hospinfwvac[1]
            novaccine[1,15]<-populationsize*novaccine$irnonhospinf[1]
            novaccine[1,16]<-novaccine$nonhospinfwvac[1]
            
            ##bind monthly coverage
            monthvaccov<-c(vaccinecoveragemonthly(), month2(), month3(), month4(), month5(), month6(),
                           month7(), month8(), month9(), month10(), month11(), month12())
            
            monthvaccov<-as.numeric(as.character(monthvaccov))
            
            totcoverage<-sum(monthvaccov, na.rm=TRUE)
            
            
            novaccine<-cbind(novaccine, monthvaccov)
            
            novaccine$covergdistrib<- c(vaccinecoveragemonthly4(), month24(), month34(), month44(), month54(), month64(), month74(), month84(), month94(), month104(), month114(), month124())
            
            novaccine$covergdistrib<-as.numeric(as.character(novaccine$covergdistrib))
            
            novaccine$monthvaccov<- novaccine$covergdistrib*totcoverage
            
            
            ##monthly effectively vaccinated and susceptible with hypothetical vaccination campaign
            novaccine<-novaccine%>%
                mutate(effectivelyvacc=populationsize*monthvaccov*vaccineeffectiveness)%>%
                mutate(effectivelycumulvacc=cumsum(effectivelyvacc))%>%
                mutate(suscwvacc=populationsize-hospinfcumulwvac-nonhospinfcumulwvac-effectivelyvacc)
            
            ##Then calculate the rest of hospinfwovac and nonhospinfwovac. Calculation uses last month's incidence rate
            for(i in 2:nrow(novaccine)){
                novaccine$hospinfwvac[i]<-novaccine$suscwvacc[i-1]*novaccine$irhospinf[i]
                novaccine$hospinfcumulwvac[i]<-sum(novaccine$hospinfwvac[1:i])
                novaccine$nonhospinfwvac[i]<- novaccine$suscwvacc[i-1]*novaccine$irnonhospinf[i]
                novaccine$nonhospinfcumulwvac[i]<-sum(novaccine$nonhospinfwvac[1:i])
                novaccine$suscwvacc[i]=populationsize-novaccine$hospinfcumulwvac[i]-novaccine$nonhospinfcumulwvac[i]-novaccine$effectivelycumulvacc[i]
            }
            
            
            ## month number
            novaccine$monthnum<-c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
            
            
            ## Overall infections with hypothetical vaccination campaign
            novaccine<-novaccine%>%
                mutate(overallinfwvac=hospinfwvac+nonhospinfwvac)%>%
                mutate(overallinfcumulwvacc=cumsum(overallinfwvac))
            
            ## Medically attended with hypothetical vaccination campaign
            novaccine<-novaccine%>%
                mutate(medicalattedtoinfwvac=overallinfwvac*mai)
            
            
            ######## Avoided events
            novaccine<-novaccine%>%
                mutate(AEhospinf=hospinfwovac-hospinfwvac)%>%
                mutate(AEnonhospinf=nonhospinfwovac-nonhospinfwvac)%>%
                mutate(AEmedicalattedtoinf=medicallyattendedinfwovac-medicalattedtoinfwvac)%>%
                mutate(AEoverallinf=overallinfwovac-overallinfwvac)
            
            
            
            ################ Prevented fraction
            ## Hospitalized infections
            ifelse (monthstartvac4()==1,
                    novaccine<-novaccine%>%
                        mutate(PFhospinf=sum(AEhospinf)/sum(hospinfwovac)),
                    novaccine<-novaccine%>%
                        mutate(PFhospinf=sum(AEhospinf[monthstartvac4():nrow(novaccine)]/sum(hospinfwovac[monthstartvac4():nrow(novaccine)])))
            )
            
            ## non-Hospitalized infections
            ifelse (monthstartvac4()==1,
                    novaccine<-novaccine%>%
                        mutate(PFnonhospinf=sum(AEnonhospinf)/sum(nonhospinfwovac)),
                    novaccine<-novaccine%>%
                        mutate(PFnonhospinf=sum(AEnonhospinf[monthstartvac4():nrow(novaccine)]/sum(nonhospinfwovac[monthstartvac4():nrow(novaccine)])))
            )
            
            
            
            
            ################ NNV -----------------------------------> why does this only go to month 8 in the Excel formula?????
            ## Hospitalized infections
            ifelse(monthstartvac4()==1,
                   novaccine<-novaccine%>%
                       mutate(NNVhospinf=1/((vaccineeffectiveness*(sum(hospinfwovac)))/populationsize)),
                   novaccine<-novaccine%>%
                       mutate(NNVhospinf=1/((vaccineeffectiveness*(sum(hospinfwovac[monthstartvac4():nrow(novaccine)])))/populationsize))
            )
            
            
            ## non-Hospitalized infections
            ifelse(monthstartvac4()==1,
                   novaccine<-novaccine%>%
                       mutate(NNVnonhospinf=1/((vaccineeffectiveness*(sum(nonhospinfwovac)))/populationsize)),
                   novaccine<-novaccine%>%
                       mutate(NNVnonhospinf=1/((vaccineeffectiveness*(sum(nonhospinfwovac[monthstartvac4():nrow(novaccine)])))/populationsize))
            )
            
            ## Medically attended infections
            ifelse(monthstartvac4()==1,
                   novaccine<-novaccine%>%
                       mutate(NNVmedicalattendedinf=1/((vaccineeffectiveness*(sum(medicallyattendedinfwovac)))/populationsize)),
                   novaccine<-novaccine%>%
                       mutate(NNVmedicalattendedinf=1/((vaccineeffectiveness*(sum(medicallyattendedinfwovac[monthstartvac4():nrow(novaccine)])))/populationsize))
            )
            
            ## Total infections
            ifelse(monthstartvac4()==1,
                   novaccine<-novaccine%>%
                       mutate(NNVoverallinf=1/((vaccineeffectiveness*(sum(hospinfwovac)+sum(nonhospinfwovac)))/populationsize)),
                   novaccine<-novaccine%>%
                       mutate(NNVoverallinf=1/((vaccineeffectiveness*(sum(hospinfwovac[monthstartvac4():nrow(novaccine)])+sum(nonhospinfwovac[monthstartvac4():nrow(novaccine)])))/populationsize))
            )
            
            return(novaccine)
            
            
        }#close "if hypothetical campaign"
        
    }) #close burdendataset
    
    
    
    ####################################################################################################
    ## List of burden averted results
    ####################################################################################################
    
    
    burdenavertedresults_timing4<-reactive({
        
        populationsize<-as.numeric(as.character(populationsize()))
        hnh<-as.numeric(as.character(hnh()))
        mai<-as.numeric(as.character(mai()))
        vaccineeffectiveness<-as.numeric(as.character(vaccineeffectiveness()))
        
        
        
        #################################################
        ## Results
        #################################################
        nonhospinfwovactot<-sum(burdendataset_timing4()$nonhospinfwovac)
        nonhospinfwvacctot<-sum(burdendataset_timing4()$nonhospinfwvac)
        nonhospinfaverted<-nonhospinfwovactot-nonhospinfwvacctot
        NNVnonhospinffinal<-burdendataset_timing4()$NNVnonhospinf[1]
        
        medicallyattendedinfwovactot<-sum(burdendataset_timing4()$medicallyattendedinfwovac)
        medicallyattendedinfwvactot<-sum(burdendataset_timing4()$medicalattedtoinfwvac)
        medicallyattendedinfaverted<-medicallyattendedinfwovactot-medicallyattendedinfwvactot
        NNVmedicallyattendtedfinal<-burdendataset_timing4()$NNVmedicalattendedinf[1]
        
        Totalinfwovacc<-sum(burdendataset_timing4()$overallinfwovac)
        Totalinfwvacc<-sum(burdendataset_timing4()$overallinfwvac)
        Totalinfaverted<-Totalinfwovacc-Totalinfwvacc
        NNVtotalinf<-burdendataset_timing4()$NNVoverallinf[1]
        
        hospinfwovactot<-sum(burdendataset_timing4()$hospinfwovac)
        hospinfwvacctot<-sum(burdendataset_timing4()$hospinfwvac)
        hospaverted<-hospinfwovactot-hospinfwvacctot
        NNVhospinffinal<-burdendataset_timing4()$NNVhospinf[1]
        
        
        nonhospPreventedfraction<-burdendataset_timing4()$PFnonhospinf[1]
        hospPreventedfraction<-burdendataset_timing4()$PFhospinf[1]
        
        
        indicator_results4<-c(nonhospinfwovactot, nonhospinfwvacctot, nonhospinfaverted, NNVnonhospinffinal,
                              Totalinfwovacc, Totalinfwvacc, Totalinfaverted, NNVtotalinf,
                              hospinfwovactot, hospinfwvacctot, hospaverted, NNVhospinffinal,
                              medicallyattendedinfwovactot, medicallyattendedinfwvactot, medicallyattendedinfaverted, NNVmedicallyattendtedfinal,
                              nonhospPreventedfraction)
        
        indicator_results_titles<-c("nonhospinfwovactot", "nonhospinfwvacctot", "nonhospinfaverted", "NNVnonhospinffinal",
                                    "Totalinfwovacc", "Totalinfwvacc", "Totalinfaverted", "NNVtotalinf",
                                    "hospinfwovactot", "hospinfwvacctot", "hospaverted", "NNVhospinffinal",
                                    "medicallyattendedinfwovactot", "medicallyattendedinfwvactot", "medicallyattendedinfaverted", "NNVmedicallyattendtedfinal",
                                    "nonhospPreventedfraction")
        
        indicator_results4<-cbind(indicator_results_titles, indicator_results4)
        
        
        return(indicator_results4)
        
        
    })#burdenavertedresults_timing4
    
    
    
    
    
    
    
    
    
    
    ##2 - Reprint base results table and attach results from coverage 1-4
    gt_table_timing<- reactive({
        
        ####################################
        ## Render results table
        ####################################
        
        populationsize<-as.numeric(as.character(populationsize()))
        hnh<-as.numeric(as.character(hnh()))
        mai<-as.numeric(as.character(mai()))
        vaccineeffectiveness<-as.numeric(as.character(vaccineeffectiveness()))
        VELCI<-as.numeric(as.character(VELCI()))
        VEUCI<-as.numeric(as.character(VEUCI()))
        
        
        burdenavertedresults<-burdenavertedresults()%>%
            as.data.frame()%>%
            mutate(indicator_results=as.numeric(indicator_results))%>%
            select(indicator_results)
        
        
        burdenavertedresults_timing1<-burdenavertedresults_timing1()%>%
            as.data.frame()%>%
            mutate(indicator_results1=as.numeric(indicator_results1))%>%
            select(indicator_results1)
        
        burdenavertedresults_timing2<-burdenavertedresults_timing2()%>%
            as.data.frame()%>%
            mutate(indicator_results2=as.numeric(indicator_results2))%>%
            select(indicator_results2)
        
        
        burdenavertedresults_timing3<-burdenavertedresults_timing3()%>%
            as.data.frame()%>%
            mutate(indicator_results3=as.numeric(indicator_results3))%>%
            select(indicator_results3)
        
        burdenavertedresults_timing4<-burdenavertedresults_timing4()%>%
            as.data.frame()%>%
            mutate(indicator_results4=as.numeric(indicator_results4))%>%
            select(indicator_results4)
        

        
        
        
        
        presenceTable<-cbind(presenceTable, burdenavertedresults, burdenavertedresults_timing1, burdenavertedresults_timing2, burdenavertedresults_timing3, burdenavertedresults_timing4)
        
        
        
        presenceTablefinal<-presenceTable%>%
            select(indicators, indicator_results, indicator_results1, indicator_results2, indicator_results3, indicator_results4)%>%
            gt()%>%
            fmt_number(
                columns=c(indicator_results, indicator_results1, indicator_results2, indicator_results3, indicator_results4),
                decimals=2,
                rows=2:17
            )%>%
            fmt_percent(
                columns = c(indicator_results,indicator_results1, indicator_results2, indicator_results3, indicator_results4),
                decimals = 2,
                rows=17:17
            )%>%
            tab_header(
                title = paste0("Vaccine deployment timing strategy results"),
                subtitle = paste0(targetgroup(),", ", country())
            )%>%
            cols_label(
                indicators = "",
                indicator_results1 = "Strategy 1",
                indicator_results2 = "Strategy 2",
                indicator_results3 = "Strategy 3",
                indicator_results4 = "Strategy 4",
            )%>%
            tab_row_group(
                label = "Non-hospitalized infections",
                rows = 1:4)%>%
            tab_row_group(
                label = "Total infections",
                rows = 5:8)%>%
            tab_row_group(
                label = "Hospitalized infections",
                rows = 9:12)%>%
            tab_row_group(
                label = "Medically attended infections",
                rows = 13:16)%>%
            tab_row_group(
                label = "Prevented fraction",
                rows = 17:17)%>%
            tab_style(
                locations = cells_row_groups(),
                style     = list(
                    cell_text(weight = "bold")
                ))%>%
            tab_options(
                #Adjust grouped rows to make them stand out
                row_group.background.color = "#e1e3e1")%>%
            tab_style(
                locations = cells_title(groups = "title"),
                style     = list(
                    cell_text(weight = "bold", size = 24)
                ))%>%
            tab_style(
                locations = cells_title(groups = "subtitle"),
                style     = list(
                    cell_text(weight = "bold", size = 18)
                ))%>%
            tab_style(
                locations = cells_column_labels(columns = everything()),
                style     = list(
                    cell_text(weight = "bold")
                )
            )%>%
            row_group_order(groups = c("Hospitalized infections", "Non-hospitalized infections", "Total infections", "Medically attended infections", "Prevented fraction"))
        
        return(presenceTablefinal)
        
    }
    ) ##close table
    
    
    
    
    output$table_timing<-render_gt({
        table<-gt_table_timing()
        return(table)
    })
    
    
    #######################################################
    
    ##Deployment coverage 1, timing 1
    ######################################################
    
    
    burdendataset_coverage1timing1<- reactive({
        ################################ The output data when insufficient data has been entered   ##################################
        ############################################################################################################################
        
        
        if(scenario()=="Impact of current influenza vaccination campaign"){
            
            
            ##########################################################################################
            # Scenario of influenza circulation in presence of vaccine: Presence to absence of vaccine
            # This is a situation where the country conducts a vaccination campaign, we enter
            # actual burden inputs and vaccine coverage then compare this to a hypothetical scenario
            # where there is no vaccination campaign conducted
            ##########################################################################################
            
            
            
            
            hospinf<-c(hospitalizationsmonthly(), hospmonth2(), hospmonth3(), hospmonth4(), hospmonth5(), hospmonth6(),
                       hospmonth7(), hospmonth8(), hospmonth9(), hospmonth10(), hospmonth11(), hospmonth12())
            
            populationsize<-as.numeric(as.character(populationsize()))
            vaccineeffectiveness<-as.numeric(as.character(vaccineeffectiveness()))
            hnh<-as.numeric(as.character(hnh()))
            mai<-as.numeric(as.character(mai()))
            hospinf<-as.numeric(as.character(hospinf))
            
            novaccine<-hospinf%>%
                as.data.frame()%>%
                rename(hospinfwvac=1)%>%
                mutate(hospinfcumulwvac=cumsum(hospinfwvac))%>%
                mutate(nonhospinfwvac=hospinfwvac*hnh)%>%
                mutate(nonhospinfcumulwvac=cumsum(nonhospinfwvac))%>%
                mutate(overallinfwvac=hospinfwvac+nonhospinfwvac)%>%
                mutate(overallinfcumulwvac=cumsum(overallinfwvac))%>%
                mutate(medicalattedtoinfwvac=overallinfwvac*mai)%>%
                mutate(medicallyattendedinfcumulwvac=cumsum(medicalattedtoinfwvac))
            
            
            
            ##bind monthly coverage
            monthvaccov<-c(vaccinecoveragemonthly(), month2(), month3(), month4(), month5(), month6(),
                           month7(), month8(), month9(), month10(), month11(), month12())
            
            monthvaccov<-as.numeric(as.character(monthvaccov))
            
            totcoverage<-sum(monthvaccov)
            
            
            novaccine<-cbind(novaccine, monthvaccov)
            
            novaccine$covergdistrib<- c(vaccinecoveragemonthly1(), month21(), month31(), month41(), month51(), month61(), month71(), month81(), month91(), month101(), month111(), month121())
            
            novaccine$covergdistrib<-as.numeric(as.character(novaccine$covergdistrib))
            
            novaccine$monthvaccov<- novaccine$covergdistrib*coverage1() ## replace original monthvaccov column with the new monthly coverage - this is so that the variable doesn't have to be changed in the rest of the script
            
            
            ## month number
            novaccine$monthnum<-c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
            
            ## add monthly effectively vaccinated
            novaccine<-novaccine%>%
                mutate(effectivelyvacc=populationsize*monthvaccov*vaccineeffectiveness)%>%
                mutate(effectivelyvacccumul=cumsum(effectivelyvacc))
            
            
            
            
            #####################################################################
            # Estimated rates
            #####################################################################
            novaccine<-novaccine%>%
                mutate(susceptiblewvacc=populationsize-hospinfcumulwvac-nonhospinfcumulwvac-effectivelyvacccumul)
            
            #Create columns
            novaccine$irhospinf<-NA
            novaccine$irnonhospinf<-NA
            
            #Create first row
            novaccine[1,14]<-novaccine$hospinfwvac[1]/populationsize
            novaccine[1,15]<-novaccine$nonhospinfwvac[1]/populationsize
            
            
            ##Calculation uses last month's susceptible population
            for(i in 2:nrow(novaccine)){
                novaccine$irhospinf[i]<-novaccine$hospinfwvac[i]/novaccine$susceptiblewvacc[i-1]
                novaccine$irnonhospinf[i]<- novaccine$nonhospinfwvac[i]/novaccine$susceptiblewvacc[i-1]
            }
            
            #####################################################################
            # Scenario of influenza circulation in absence of vaccine
            #####################################################################
            
            ## These calculations are initialized by the 'Susceptible at start of month in absence of vaccine'
            ## column, which starts with the Target Population in the first month.
            
            remainsuscstartmonth<-c(populationsize, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
            novaccine<-cbind(novaccine, remainsuscstartmonth)
            
            #Create columns
            novaccine$hospinfwovac<-NA
            novaccine$hospinfcumulwovacc<-NA
            novaccine$nonhospinfwovac<-NA
            novaccine$nonhospinfcumulwovacc<-NA
            novaccine$remainingsusc<-NA
            
            #Create first row
            novaccine[1,17]<-novaccine$remainsuscstartmonth[1]*novaccine$irhospinf[1]
            novaccine[1,18]<-novaccine$hospinfwovac[1]
            novaccine[1,19]<-novaccine$remainsuscstartmonth[1]*novaccine$irnonhospinf[1]
            novaccine[1,20]<-novaccine$nonhospinfwovac[1]
            novaccine[1,21]<-populationsize-novaccine$hospinfcumulwovacc[1]-novaccine$nonhospinfcumulwovacc[1]
            
            
            #Create rows 2:12
            for(i in 2:nrow(novaccine)){
                novaccine$remainsuscstartmonth[i]<-novaccine$remainingsusc[i-1]
                novaccine$hospinfwovac[i]<-novaccine$remainsuscstartmonth[i]*novaccine$irhospinf[i]
                novaccine$hospinfcumulwovacc[i]<-sum(novaccine$hospinfwovac[1:i])
                novaccine$nonhospinfwovac[i]<-novaccine$remainsuscstartmonth[i]*novaccine$irnonhospinf[i]
                novaccine$nonhospinfcumulwovacc[i]<-sum(novaccine$nonhospinfwovac[1:i])
                novaccine$remainingsusc[i]<-populationsize-novaccine$hospinfcumulwovacc[i]-novaccine$nonhospinfcumulwovacc[i]
            }
            
            novaccine<-novaccine%>%
                mutate(overallinfwovac=hospinfwovac+nonhospinfwovac)%>%
                mutate(overallinfcumulwovacc=cumsum(overallinfwovac))%>%
                mutate(medicallyattendedinfwovac=overallinfwovac*mai)%>%
                mutate(medicalattedtoinfcumulwovacc=cumsum(medicallyattendedinfwovac))
            
            ######## Avoided events
            novaccine<-novaccine%>%
                mutate(AEhospinf=hospinfwovac-hospinfwvac)%>%
                mutate(AEnonhospinf=nonhospinfwovac-nonhospinfwvac)%>%
                mutate(AEmedicalattedtoinf=medicallyattendedinfwovac-medicalattedtoinfwvac)%>%
                mutate(AEoverallinf=overallinfwovac-overallinfwvac)
            
            
            ################ Prevented fraction
            ## Hospitalized infections
            ifelse (monthstartvac1()==1,
                    novaccine<-novaccine%>%
                        mutate(PFhospinf=sum(AEhospinf)/sum(hospinfwovac)),
                    novaccine<-novaccine%>%
                        mutate(PFhospinf=sum(AEhospinf[monthstartvac1():nrow(novaccine)]/sum(hospinfwovac[monthstartvac1():nrow(novaccine)])))
            )
            
            ## non-Hospitalized infections
            ifelse (monthstartvac1()==1,
                    novaccine<-novaccine%>%
                        mutate(PFnonhospinf=sum(AEnonhospinf)/sum(nonhospinfwovac)),
                    novaccine<-novaccine%>%
                        mutate(PFnonhospinf=sum(AEnonhospinf[monthstartvac1():nrow(novaccine)]/sum(nonhospinfwovac[monthstartvac1():nrow(novaccine)])))
            )
            
            
            ################ NNV -----------------------------------> why does this only go to month 8 in the Excel formula?????
            ## Hospitalized infections
            ifelse(monthstartvac1()==1,
                   novaccine<-novaccine%>%
                       mutate(NNVhospinf=1/((vaccineeffectiveness*(sum(hospinfwovac)))/populationsize)),
                   novaccine<-novaccine%>%
                       mutate(NNVhospinf=1/((vaccineeffectiveness*(sum(hospinfwovac[monthstartvac1():nrow(novaccine)])))/populationsize))
            )
            
            
            ## non-Hospitalized infections
            ifelse(monthstartvac1()==1,
                   novaccine<-novaccine%>%
                       mutate(NNVnonhospinf=1/((vaccineeffectiveness*(sum(nonhospinfwovac)))/populationsize)),
                   novaccine<-novaccine%>%
                       mutate(NNVnonhospinf=1/((vaccineeffectiveness*(sum(nonhospinfwovac[monthstartvac1():nrow(novaccine)])))/populationsize))
            )
            
            ## Medically infections
            ifelse(monthstartvac1()==1,
                   novaccine<-novaccine%>%
                       mutate(NNVmedicalattendedinf=1/((vaccineeffectiveness*(sum(medicallyattendedinfwovac)))/populationsize)),
                   novaccine<-novaccine%>%
                       mutate(NNVmedicalattendedinf=1/((vaccineeffectiveness*(sum(medicallyattendedinfwovac[monthstartvac1():nrow(novaccine)])))/populationsize))
            )
            
            ## Overall infections
            ifelse(monthstartvac1()==1,
                   novaccine<-novaccine%>%
                       mutate(NNVoverallinf=1/((vaccineeffectiveness*(sum(hospinfwovac)+sum(nonhospinfwovac)))/populationsize)),
                   novaccine<-novaccine%>%
                       mutate(NNVoverallinf=1/((vaccineeffectiveness*(sum(hospinfwovac[monthstartvac1():nrow(novaccine)])+sum(nonhospinfwovac[monthstartvac1():nrow(novaccine)])))/populationsize))
            )
            
            return(novaccine)
            
            
            
        } #close if current campaign
        
        
        
        if(scenario()=="Impact of hypothetical vaccination campaign"){
            
            ##########################################################################################
            # Scenario of influenza circulation in absence of vaccine: Absence to presence of vaccine
            # This is a scenario where the country has yet to conduct a vaccination campaign but would
            # like to see what impact a vaccine campaign may have.
            # The country enters the estimated number of hospitalization they observed and then we compare
            # this to a hypothetical vaccination campaign
            ##########################################################################################
            
            hospinf<-c(hospitalizationsmonthly(), hospmonth2(), hospmonth3(), hospmonth4(), hospmonth5(), hospmonth6(),
                       hospmonth7(), hospmonth8(), hospmonth9(), hospmonth10(), hospmonth11(), hospmonth12())
            
            populationsize<-as.numeric(as.character(populationsize()))
            hnh<-as.numeric(as.character(hnh()))
            mai<-as.numeric(as.character(mai()))
            hospinf<-as.numeric(as.character(hospinf))
            vaccineeffectiveness<-as.numeric(as.character(vaccineeffectiveness()))
            
            #####################Observed influenza in the absence of vaccine
            novaccine<-hospinf%>%
                as.data.frame()%>%
                rename(hospinfwovac=1)%>%
                mutate(hospinfcumulwovac=cumsum(hospinfwovac))%>%
                mutate(nonhospinfwovac=hospinfwovac*hnh)%>%
                mutate(nonhospinfcumulwovac=cumsum(nonhospinfwovac))%>%
                mutate(remainingsusc=populationsize-hospinfcumulwovac-nonhospinfcumulwovac)
            
            #create remainsuscstartmonth column with target population and rows 1:11 of remainingsusc
            remainsuscstartmonth<-populationsize%>%
                as.data.frame()%>%
                rename(remainsuscstartmonth=1)
            
            remainsuscstartmonth2<-as.data.frame(novaccine$remainingsusc[1:11])%>%
                as.data.frame()%>%
                rename(remainsuscstartmonth=1)
            
            remainsuscstartmonth<-rbind(remainsuscstartmonth, remainsuscstartmonth2)
            
            novaccine<-cbind(novaccine, remainsuscstartmonth)
            
            novaccine<-novaccine%>%
                mutate(overallinfwovac=hospinfwovac+nonhospinfwovac)%>%
                mutate(overallinfcumulwovac=cumsum(overallinfwovac))%>%
                mutate(medicallyattendedinfwovac=overallinfwovac*mai)%>%
                mutate(medicallyattendedinfcumulwovac=cumsum(medicallyattendedinfwovac))
            
            
            
            #####################################################################
            # Incidence rates in the absence of vaccine
            #####################################################################
            novaccine<-novaccine%>%
                mutate(irhospinf=hospinfwovac/remainsuscstartmonth)%>%
                mutate(irnonhospinf=nonhospinfwovac/remainsuscstartmonth)
            
            
            
            
            ############################ Burden inputs with hypothetical vaccination campaign
            #Create columns
            novaccine$hospinfwvac<-NA
            novaccine$hospinfcumulwvac<-NA
            novaccine$nonhospinfwvac<-NA
            novaccine$nonhospinfcumulwvac<-NA
            
            
            
            #Create first row
            novaccine[1,13]<-populationsize*novaccine$irhospinf[1]
            novaccine[1,14]<-novaccine$hospinfwvac[1]
            novaccine[1,15]<-populationsize*novaccine$irnonhospinf[1]
            novaccine[1,16]<-novaccine$nonhospinfwvac[1]
            
            ##bind monthly coverage
            monthvaccov<-c(vaccinecoveragemonthly(), month2(), month3(), month4(), month5(), month6(),
                           month7(), month8(), month9(), month10(), month11(), month12())
            
            monthvaccov<-as.numeric(as.character(monthvaccov))
            
            totcoverage<-sum(monthvaccov)
            
            
            novaccine<-cbind(novaccine, monthvaccov)
            
            novaccine$covergdistrib<- c(vaccinecoveragemonthly1(), month21(), month31(), month41(), month51(), month61(), month71(), month81(), month91(), month101(), month111(), month121())
            
            novaccine$covergdistrib<-as.numeric(as.character(novaccine$covergdistrib))
            
            novaccine$monthvaccov<- novaccine$covergdistrib*coverage1() ## replace original monthvaccov column with the new monthly coverage - this is so that the variable doesn't have to be changed in the rest of the script
            
            
            ##monthly effectively vaccinated and susceptible with hypothetical vaccination campaign
            novaccine<-novaccine%>%
                mutate(effectivelyvacc=populationsize*monthvaccov*vaccineeffectiveness)%>%
                mutate(effectivelycumulvacc=cumsum(effectivelyvacc))%>%
                mutate(suscwvacc=populationsize-hospinfcumulwvac-nonhospinfcumulwvac-effectivelyvacc)
            
            ##Then calculate the rest of hospinfwovac and nonhospinfwovac. Calculation uses last month's incidence rate
            for(i in 2:nrow(novaccine)){
                novaccine$hospinfwvac[i]<-novaccine$suscwvacc[i-1]*novaccine$irhospinf[i]
                novaccine$hospinfcumulwvac[i]<-sum(novaccine$hospinfwvac[1:i])
                novaccine$nonhospinfwvac[i]<- novaccine$suscwvacc[i-1]*novaccine$irnonhospinf[i]
                novaccine$nonhospinfcumulwvac[i]<-sum(novaccine$nonhospinfwvac[1:i])
                novaccine$suscwvacc[i]=populationsize-novaccine$hospinfcumulwvac[i]-novaccine$nonhospinfcumulwvac[i]-novaccine$effectivelycumulvacc[i]
            }
            
            
            ## month number
            novaccine$monthnum<-c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
            
            
            ## Overall infections with hypothetical vaccination campaign
            novaccine<-novaccine%>%
                mutate(overallinfwvac=hospinfwvac+nonhospinfwvac)%>%
                mutate(overallinfcumulwvacc=cumsum(overallinfwvac))
            
            ## Medically attended with hypothetical vaccination campaign
            novaccine<-novaccine%>%
                mutate(medicalattedtoinfwvac=overallinfwvac*mai)
            
            
            ######## Avoided events
            novaccine<-novaccine%>%
                mutate(AEhospinf=hospinfwovac-hospinfwvac)%>%
                mutate(AEnonhospinf=nonhospinfwovac-nonhospinfwvac)%>%
                mutate(AEmedicalattedtoinf=medicallyattendedinfwovac-medicalattedtoinfwvac)%>%
                mutate(AEoverallinf=overallinfwovac-overallinfwvac)
            
            
            
            ################ Prevented fraction
            ## Hospitalized infections
            ifelse (monthstartvac1()==1,
                    novaccine<-novaccine%>%
                        mutate(PFhospinf=sum(AEhospinf)/sum(hospinfwovac)),
                    novaccine<-novaccine%>%
                        mutate(PFhospinf=sum(AEhospinf[monthstartvac1():nrow(novaccine)]/sum(hospinfwovac[monthstartvac1():nrow(novaccine)])))
            )
            
            ## non-Hospitalized infections
            ifelse (monthstartvac1()==1,
                    novaccine<-novaccine%>%
                        mutate(PFnonhospinf=sum(AEnonhospinf)/sum(nonhospinfwovac)),
                    novaccine<-novaccine%>%
                        mutate(PFnonhospinf=sum(AEnonhospinf[monthstartvac1():nrow(novaccine)]/sum(nonhospinfwovac[monthstartvac1():nrow(novaccine)])))
            )
            
            
            
            
            ################ NNV -----------------------------------> why does this only go to month 8 in the Excel formula?????
            ## Hospitalized infections
            ifelse(monthstartvac1()==1,
                   novaccine<-novaccine%>%
                       mutate(NNVhospinf=1/((vaccineeffectiveness*(sum(hospinfwovac)))/populationsize)),
                   novaccine<-novaccine%>%
                       mutate(NNVhospinf=1/((vaccineeffectiveness*(sum(hospinfwovac[monthstartvac1():nrow(novaccine)])))/populationsize))
            )
            
            
            ## non-Hospitalized infections
            ifelse(monthstartvac1()==1,
                   novaccine<-novaccine%>%
                       mutate(NNVnonhospinf=1/((vaccineeffectiveness*(sum(nonhospinfwovac)))/populationsize)),
                   novaccine<-novaccine%>%
                       mutate(NNVnonhospinf=1/((vaccineeffectiveness*(sum(nonhospinfwovac[monthstartvac1():nrow(novaccine)])))/populationsize))
            )
            
            ## Medically attended infections
            ifelse(monthstartvac1()==1,
                   novaccine<-novaccine%>%
                       mutate(NNVmedicalattendedinf=1/((vaccineeffectiveness*(sum(medicallyattendedinfwovac)))/populationsize)),
                   novaccine<-novaccine%>%
                       mutate(NNVmedicalattendedinf=1/((vaccineeffectiveness*(sum(medicallyattendedinfwovac[monthstartvac1():nrow(novaccine)])))/populationsize))
            )
            
            ## Total infections
            ifelse(monthstartvac1()==1,
                   novaccine<-novaccine%>%
                       mutate(NNVoverallinf=1/((vaccineeffectiveness*(sum(hospinfwovac)+sum(nonhospinfwovac)))/populationsize)),
                   novaccine<-novaccine%>%
                       mutate(NNVoverallinf=1/((vaccineeffectiveness*(sum(hospinfwovac[monthstartvac1():nrow(novaccine)])+sum(nonhospinfwovac[monthstartvac1():nrow(novaccine)])))/populationsize))
            )
            
            return(novaccine)
            
            
        }#close "if hypothetical campaign"
        
    }) #close burdendataset
    
    
    
    ####################################################################################################
    ## List of burden averted results
    ####################################################################################################
    
    
    burdenavertedresultscoverage1timing1<-reactive({
        
        populationsize<-as.numeric(as.character(populationsize()))
        hnh<-as.numeric(as.character(hnh()))
        mai<-as.numeric(as.character(mai()))
        vaccineeffectiveness<-as.numeric(as.character(vaccineeffectiveness()))
        
        
        
        #################################################
        ## Results
        #################################################
        nonhospinfwovactot<-sum(burdendataset_coverage1timing1()$nonhospinfwovac)
        nonhospinfwvacctot<-sum(burdendataset_coverage1timing1()$nonhospinfwvac)
        nonhospinfaverted<-nonhospinfwovactot-nonhospinfwvacctot
        NNVnonhospinffinal<-burdendataset_coverage1timing1()$NNVnonhospinf[1]
        
        medicallyattendedinfwovactot<-sum(burdendataset_coverage1timing1()$medicallyattendedinfwovac)
        medicallyattendedinfwvactot<-sum(burdendataset_coverage1timing1()$medicalattedtoinfwvac)
        medicallyattendedinfaverted<-medicallyattendedinfwovactot-medicallyattendedinfwvactot
        NNVmedicallyattendtedfinal<-burdendataset_coverage1timing1()$NNVmedicalattendedinf[1]
        
        Totalinfwovacc<-sum(burdendataset_coverage1timing1()$overallinfwovac)
        Totalinfwvacc<-sum(burdendataset_coverage1timing1()$overallinfwvac)
        Totalinfaverted<-Totalinfwovacc-Totalinfwvacc
        NNVtotalinf<-burdendataset_coverage1timing1()$NNVoverallinf[1]
        
        hospinfwovactot<-sum(burdendataset_coverage1timing1()$hospinfwovac)
        hospinfwvacctot<-sum(burdendataset_coverage1timing1()$hospinfwvac)
        hospaverted<-hospinfwovactot-hospinfwvacctot
        NNVhospinffinal<-burdendataset_coverage1timing1()$NNVhospinf[1]
        
        
        nonhospPreventedfraction<-burdendataset_coverage1timing1()$PFnonhospinf[1]
        hospPreventedfraction<-burdendataset_coverage1timing1()$PFhospinf[1]
        
        
        indicator_results1<-c(nonhospinfwovactot, nonhospinfwvacctot, nonhospinfaverted, NNVnonhospinffinal,
                              Totalinfwovacc, Totalinfwvacc, Totalinfaverted, NNVtotalinf,
                              hospinfwovactot, hospinfwvacctot, hospaverted, NNVhospinffinal,
                              medicallyattendedinfwovactot, medicallyattendedinfwvactot, medicallyattendedinfaverted, NNVmedicallyattendtedfinal,
                              nonhospPreventedfraction)
        
        indicator_results_titles<-c("nonhospinfwovactot", "nonhospinfwvacctot", "nonhospinfaverted", "NNVnonhospinffinal",
                                    "Totalinfwovacc", "Totalinfwvacc", "Totalinfaverted", "NNVtotalinf",
                                    "hospinfwovactot", "hospinfwvacctot", "hospaverted", "NNVhospinffinal",
                                    "medicallyattendedinfwovactot", "medicallyattendedinfwvactot", "medicallyattendedinfaverted", "NNVmedicallyattendtedfinal",
                                    "nonhospPreventedfraction")
        
        indicator_results1<-cbind(indicator_results_titles, indicator_results1)
        
        
        return(indicator_results1)
        
        
    })#burdendataset_coverage1timing1
    
    
    
    
    #######################################################
    
    ##Deployment coverage 1, timing 2
    ######################################################
    
    
    burdendataset_coverage1timing2<- reactive({
        ################################ The output data when insufficient data has been entered   ##################################
        ############################################################################################################################
        
        
        if(scenario()=="Impact of current influenza vaccination campaign"){
            
            
            ##########################################################################################
            # Scenario of influenza circulation in presence of vaccine: Presence to absence of vaccine
            # This is a situation where the country conducts a vaccination campaign, we enter
            # actual burden inputs and vaccine coverage then compare this to a hypothetical scenario
            # where there is no vaccination campaign conducted
            ##########################################################################################
            
            
            
            
            hospinf<-c(hospitalizationsmonthly(), hospmonth2(), hospmonth3(), hospmonth4(), hospmonth5(), hospmonth6(),
                       hospmonth7(), hospmonth8(), hospmonth9(), hospmonth10(), hospmonth11(), hospmonth12())
            
            populationsize<-as.numeric(as.character(populationsize()))
            vaccineeffectiveness<-as.numeric(as.character(vaccineeffectiveness()))
            hnh<-as.numeric(as.character(hnh()))
            mai<-as.numeric(as.character(mai()))
            hospinf<-as.numeric(as.character(hospinf))
            
            novaccine<-hospinf%>%
                as.data.frame()%>%
                rename(hospinfwvac=1)%>%
                mutate(hospinfcumulwvac=cumsum(hospinfwvac))%>%
                mutate(nonhospinfwvac=hospinfwvac*hnh)%>%
                mutate(nonhospinfcumulwvac=cumsum(nonhospinfwvac))%>%
                mutate(overallinfwvac=hospinfwvac+nonhospinfwvac)%>%
                mutate(overallinfcumulwvac=cumsum(overallinfwvac))%>%
                mutate(medicalattedtoinfwvac=overallinfwvac*mai)%>%
                mutate(medicallyattendedinfcumulwvac=cumsum(medicalattedtoinfwvac))
            
            
            
            ##bind monthly coverage
            monthvaccov<-c(vaccinecoveragemonthly(), month2(), month3(), month4(), month5(), month6(),
                           month7(), month8(), month9(), month10(), month11(), month12())
            
            monthvaccov<-as.numeric(as.character(monthvaccov))
            
            totcoverage<-sum(monthvaccov)
            
            
            novaccine<-cbind(novaccine, monthvaccov)
            
            novaccine$covergdistrib<- c(vaccinecoveragemonthly2(), month22(), month32(), month42(), month52(), month62(), month72(), month82(), month92(), month102(), month112(), month122())
            
            novaccine$covergdistrib<-as.numeric(as.character(novaccine$covergdistrib))
            
            novaccine$monthvaccov<- novaccine$covergdistrib*coverage1() ## replace original monthvaccov column with the new monthly coverage - this is so that the variable doesn't have to be changed in the rest of the script
            
            
            ## month number
            novaccine$monthnum<-c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
            
            ## add monthly effectively vaccinated
            novaccine<-novaccine%>%
                mutate(effectivelyvacc=populationsize*monthvaccov*vaccineeffectiveness)%>%
                mutate(effectivelyvacccumul=cumsum(effectivelyvacc))
            
            
            
            
            #####################################################################
            # Estimated rates
            #####################################################################
            novaccine<-novaccine%>%
                mutate(susceptiblewvacc=populationsize-hospinfcumulwvac-nonhospinfcumulwvac-effectivelyvacccumul)
            
            #Create columns
            novaccine$irhospinf<-NA
            novaccine$irnonhospinf<-NA
            
            #Create first row
            novaccine[1,14]<-novaccine$hospinfwvac[1]/populationsize
            novaccine[1,15]<-novaccine$nonhospinfwvac[1]/populationsize
            
            
            ##Calculation uses last month's susceptible population
            for(i in 2:nrow(novaccine)){
                novaccine$irhospinf[i]<-novaccine$hospinfwvac[i]/novaccine$susceptiblewvacc[i-1]
                novaccine$irnonhospinf[i]<- novaccine$nonhospinfwvac[i]/novaccine$susceptiblewvacc[i-1]
            }
            
            #####################################################################
            # Scenario of influenza circulation in absence of vaccine
            #####################################################################
            
            ## These calculations are initialized by the 'Susceptible at start of month in absence of vaccine'
            ## column, which starts with the Target Population in the first month.
            
            remainsuscstartmonth<-c(populationsize, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
            novaccine<-cbind(novaccine, remainsuscstartmonth)
            
            #Create columns
            novaccine$hospinfwovac<-NA
            novaccine$hospinfcumulwovacc<-NA
            novaccine$nonhospinfwovac<-NA
            novaccine$nonhospinfcumulwovacc<-NA
            novaccine$remainingsusc<-NA
            
            #Create first row
            novaccine[1,17]<-novaccine$remainsuscstartmonth[1]*novaccine$irhospinf[1]
            novaccine[1,18]<-novaccine$hospinfwovac[1]
            novaccine[1,19]<-novaccine$remainsuscstartmonth[1]*novaccine$irnonhospinf[1]
            novaccine[1,20]<-novaccine$nonhospinfwovac[1]
            novaccine[1,21]<-populationsize-novaccine$hospinfcumulwovacc[1]-novaccine$nonhospinfcumulwovacc[1]
            
            
            #Create rows 2:12
            for(i in 2:nrow(novaccine)){
                novaccine$remainsuscstartmonth[i]<-novaccine$remainingsusc[i-1]
                novaccine$hospinfwovac[i]<-novaccine$remainsuscstartmonth[i]*novaccine$irhospinf[i]
                novaccine$hospinfcumulwovacc[i]<-sum(novaccine$hospinfwovac[1:i])
                novaccine$nonhospinfwovac[i]<-novaccine$remainsuscstartmonth[i]*novaccine$irnonhospinf[i]
                novaccine$nonhospinfcumulwovacc[i]<-sum(novaccine$nonhospinfwovac[1:i])
                novaccine$remainingsusc[i]<-populationsize-novaccine$hospinfcumulwovacc[i]-novaccine$nonhospinfcumulwovacc[i]
            }
            
            novaccine<-novaccine%>%
                mutate(overallinfwovac=hospinfwovac+nonhospinfwovac)%>%
                mutate(overallinfcumulwovacc=cumsum(overallinfwovac))%>%
                mutate(medicallyattendedinfwovac=overallinfwovac*mai)%>%
                mutate(medicalattedtoinfcumulwovacc=cumsum(medicallyattendedinfwovac))
            
            ######## Avoided events
            novaccine<-novaccine%>%
                mutate(AEhospinf=hospinfwovac-hospinfwvac)%>%
                mutate(AEnonhospinf=nonhospinfwovac-nonhospinfwvac)%>%
                mutate(AEmedicalattedtoinf=medicallyattendedinfwovac-medicalattedtoinfwvac)%>%
                mutate(AEoverallinf=overallinfwovac-overallinfwvac)
            
            
            ################ Prevented fraction
            ## Hospitalized infections
            ifelse (monthstartvac2()==1,
                    novaccine<-novaccine%>%
                        mutate(PFhospinf=sum(AEhospinf)/sum(hospinfwovac)),
                    novaccine<-novaccine%>%
                        mutate(PFhospinf=sum(AEhospinf[monthstartvac2():nrow(novaccine)]/sum(hospinfwovac[monthstartvac2():nrow(novaccine)])))
            )
            
            ## non-Hospitalized infections
            ifelse (monthstartvac2()==1,
                    novaccine<-novaccine%>%
                        mutate(PFnonhospinf=sum(AEnonhospinf)/sum(nonhospinfwovac)),
                    novaccine<-novaccine%>%
                        mutate(PFnonhospinf=sum(AEnonhospinf[monthstartvac2():nrow(novaccine)]/sum(nonhospinfwovac[monthstartvac2():nrow(novaccine)])))
            )
            
            
            ################ NNV -----------------------------------> why does this only go to month 8 in the Excel formula?????
            ## Hospitalized infections
            ifelse(monthstartvac2()==1,
                   novaccine<-novaccine%>%
                       mutate(NNVhospinf=1/((vaccineeffectiveness*(sum(hospinfwovac)))/populationsize)),
                   novaccine<-novaccine%>%
                       mutate(NNVhospinf=1/((vaccineeffectiveness*(sum(hospinfwovac[monthstartvac2():nrow(novaccine)])))/populationsize))
            )
            
            
            ## non-Hospitalized infections
            ifelse(monthstartvac2()==1,
                   novaccine<-novaccine%>%
                       mutate(NNVnonhospinf=1/((vaccineeffectiveness*(sum(nonhospinfwovac)))/populationsize)),
                   novaccine<-novaccine%>%
                       mutate(NNVnonhospinf=1/((vaccineeffectiveness*(sum(nonhospinfwovac[monthstartvac2():nrow(novaccine)])))/populationsize))
            )
            
            ## Medically infections
            ifelse(monthstartvac2()==1,
                   novaccine<-novaccine%>%
                       mutate(NNVmedicalattendedinf=1/((vaccineeffectiveness*(sum(medicallyattendedinfwovac)))/populationsize)),
                   novaccine<-novaccine%>%
                       mutate(NNVmedicalattendedinf=1/((vaccineeffectiveness*(sum(medicallyattendedinfwovac[monthstartvac2():nrow(novaccine)])))/populationsize))
            )
            
            ## Overall infections
            ifelse(monthstartvac2()==1,
                   novaccine<-novaccine%>%
                       mutate(NNVoverallinf=1/((vaccineeffectiveness*(sum(hospinfwovac)+sum(nonhospinfwovac)))/populationsize)),
                   novaccine<-novaccine%>%
                       mutate(NNVoverallinf=1/((vaccineeffectiveness*(sum(hospinfwovac[monthstartvac2():nrow(novaccine)])+sum(nonhospinfwovac[monthstartvac2():nrow(novaccine)])))/populationsize))
            )
            
            return(novaccine)
            
            
            
        } #close if current campaign
        
        
        
        if(scenario()=="Impact of hypothetical vaccination campaign"){
            
            ##########################################################################################
            # Scenario of influenza circulation in absence of vaccine: Absence to presence of vaccine
            # This is a scenario where the country has yet to conduct a vaccination campaign but would
            # like to see what impact a vaccine campaign may have.
            # The country enters the estimated number of hospitalization they observed and then we compare
            # this to a hypothetical vaccination campaign
            ##########################################################################################
            
            hospinf<-c(hospitalizationsmonthly(), hospmonth2(), hospmonth3(), hospmonth4(), hospmonth5(), hospmonth6(),
                       hospmonth7(), hospmonth8(), hospmonth9(), hospmonth10(), hospmonth11(), hospmonth12())
            
            populationsize<-as.numeric(as.character(populationsize()))
            hnh<-as.numeric(as.character(hnh()))
            mai<-as.numeric(as.character(mai()))
            hospinf<-as.numeric(as.character(hospinf))
            vaccineeffectiveness<-as.numeric(as.character(vaccineeffectiveness()))
            
            #####################Observed influenza in the absence of vaccine
            novaccine<-hospinf%>%
                as.data.frame()%>%
                rename(hospinfwovac=1)%>%
                mutate(hospinfcumulwovac=cumsum(hospinfwovac))%>%
                mutate(nonhospinfwovac=hospinfwovac*hnh)%>%
                mutate(nonhospinfcumulwovac=cumsum(nonhospinfwovac))%>%
                mutate(remainingsusc=populationsize-hospinfcumulwovac-nonhospinfcumulwovac)
            
            #create remainsuscstartmonth column with target population and rows 1:11 of remainingsusc
            remainsuscstartmonth<-populationsize%>%
                as.data.frame()%>%
                rename(remainsuscstartmonth=1)
            
            remainsuscstartmonth2<-as.data.frame(novaccine$remainingsusc[1:11])%>%
                as.data.frame()%>%
                rename(remainsuscstartmonth=1)
            
            remainsuscstartmonth<-rbind(remainsuscstartmonth, remainsuscstartmonth2)
            
            novaccine<-cbind(novaccine, remainsuscstartmonth)
            
            novaccine<-novaccine%>%
                mutate(overallinfwovac=hospinfwovac+nonhospinfwovac)%>%
                mutate(overallinfcumulwovac=cumsum(overallinfwovac))%>%
                mutate(medicallyattendedinfwovac=overallinfwovac*mai)%>%
                mutate(medicallyattendedinfcumulwovac=cumsum(medicallyattendedinfwovac))
            
            
            
            #####################################################################
            # Incidence rates in the absence of vaccine
            #####################################################################
            novaccine<-novaccine%>%
                mutate(irhospinf=hospinfwovac/remainsuscstartmonth)%>%
                mutate(irnonhospinf=nonhospinfwovac/remainsuscstartmonth)
            
            
            
            
            ############################ Burden inputs with hypothetical vaccination campaign
            #Create columns
            novaccine$hospinfwvac<-NA
            novaccine$hospinfcumulwvac<-NA
            novaccine$nonhospinfwvac<-NA
            novaccine$nonhospinfcumulwvac<-NA
            
            
            
            #Create first row
            novaccine[1,13]<-populationsize*novaccine$irhospinf[1]
            novaccine[1,14]<-novaccine$hospinfwvac[1]
            novaccine[1,15]<-populationsize*novaccine$irnonhospinf[1]
            novaccine[1,16]<-novaccine$nonhospinfwvac[1]
            
            ##bind monthly coverage
            monthvaccov<-c(vaccinecoveragemonthly(), month2(), month3(), month4(), month5(), month6(),
                           month7(), month8(), month9(), month10(), month11(), month12())
            
            monthvaccov<-as.numeric(as.character(monthvaccov))
            
            totcoverage<-sum(monthvaccov)
            
            
            novaccine<-cbind(novaccine, monthvaccov)
            
            novaccine$covergdistrib<- c(vaccinecoveragemonthly2(), month22(), month32(), month42(), month52(), month62(), month72(), month82(), month92(), month102(), month112(), month122())
            
            novaccine$covergdistrib<-as.numeric(as.character(novaccine$covergdistrib))
            
            novaccine$monthvaccov<- novaccine$covergdistrib*coverage1() ## replace original monthvaccov column with the new monthly coverage - this is so that the variable doesn't have to be changed in the rest of the script
            
            
            ##monthly effectively vaccinated and susceptible with hypothetical vaccination campaign
            novaccine<-novaccine%>%
                mutate(effectivelyvacc=populationsize*monthvaccov*vaccineeffectiveness)%>%
                mutate(effectivelycumulvacc=cumsum(effectivelyvacc))%>%
                mutate(suscwvacc=populationsize-hospinfcumulwvac-nonhospinfcumulwvac-effectivelyvacc)
            
            ##Then calculate the rest of hospinfwovac and nonhospinfwovac. Calculation uses last month's incidence rate
            for(i in 2:nrow(novaccine)){
                novaccine$hospinfwvac[i]<-novaccine$suscwvacc[i-1]*novaccine$irhospinf[i]
                novaccine$hospinfcumulwvac[i]<-sum(novaccine$hospinfwvac[1:i])
                novaccine$nonhospinfwvac[i]<- novaccine$suscwvacc[i-1]*novaccine$irnonhospinf[i]
                novaccine$nonhospinfcumulwvac[i]<-sum(novaccine$nonhospinfwvac[1:i])
                novaccine$suscwvacc[i]=populationsize-novaccine$hospinfcumulwvac[i]-novaccine$nonhospinfcumulwvac[i]-novaccine$effectivelycumulvacc[i]
            }
            
            
            ## month number
            novaccine$monthnum<-c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
            
            
            ## Overall infections with hypothetical vaccination campaign
            novaccine<-novaccine%>%
                mutate(overallinfwvac=hospinfwvac+nonhospinfwvac)%>%
                mutate(overallinfcumulwvacc=cumsum(overallinfwvac))
            
            ## Medically attended with hypothetical vaccination campaign
            novaccine<-novaccine%>%
                mutate(medicalattedtoinfwvac=overallinfwvac*mai)
            
            
            ######## Avoided events
            novaccine<-novaccine%>%
                mutate(AEhospinf=hospinfwovac-hospinfwvac)%>%
                mutate(AEnonhospinf=nonhospinfwovac-nonhospinfwvac)%>%
                mutate(AEmedicalattedtoinf=medicallyattendedinfwovac-medicalattedtoinfwvac)%>%
                mutate(AEoverallinf=overallinfwovac-overallinfwvac)
            
            
            
            ################ Prevented fraction
            ## Hospitalized infections
            ifelse (monthstartvac2()==1,
                    novaccine<-novaccine%>%
                        mutate(PFhospinf=sum(AEhospinf)/sum(hospinfwovac)),
                    novaccine<-novaccine%>%
                        mutate(PFhospinf=sum(AEhospinf[monthstartvac2():nrow(novaccine)]/sum(hospinfwovac[monthstartvac2():nrow(novaccine)])))
            )
            
            ## non-Hospitalized infections
            ifelse (monthstartvac2()==1,
                    novaccine<-novaccine%>%
                        mutate(PFnonhospinf=sum(AEnonhospinf)/sum(nonhospinfwovac)),
                    novaccine<-novaccine%>%
                        mutate(PFnonhospinf=sum(AEnonhospinf[monthstartvac2():nrow(novaccine)]/sum(nonhospinfwovac[monthstartvac2():nrow(novaccine)])))
            )
            
            
            
            
            ################ NNV -----------------------------------> why does this only go to month 8 in the Excel formula?????
            ## Hospitalized infections
            ifelse(monthstartvac2()==1,
                   novaccine<-novaccine%>%
                       mutate(NNVhospinf=1/((vaccineeffectiveness*(sum(hospinfwovac)))/populationsize)),
                   novaccine<-novaccine%>%
                       mutate(NNVhospinf=1/((vaccineeffectiveness*(sum(hospinfwovac[monthstartvac2():nrow(novaccine)])))/populationsize))
            )
            
            
            ## non-Hospitalized infections
            ifelse(monthstartvac2()==1,
                   novaccine<-novaccine%>%
                       mutate(NNVnonhospinf=1/((vaccineeffectiveness*(sum(nonhospinfwovac)))/populationsize)),
                   novaccine<-novaccine%>%
                       mutate(NNVnonhospinf=1/((vaccineeffectiveness*(sum(nonhospinfwovac[monthstartvac2():nrow(novaccine)])))/populationsize))
            )
            
            ## Medically attended infections
            ifelse(monthstartvac2()==1,
                   novaccine<-novaccine%>%
                       mutate(NNVmedicalattendedinf=1/((vaccineeffectiveness*(sum(medicallyattendedinfwovac)))/populationsize)),
                   novaccine<-novaccine%>%
                       mutate(NNVmedicalattendedinf=1/((vaccineeffectiveness*(sum(medicallyattendedinfwovac[monthstartvac2():nrow(novaccine)])))/populationsize))
            )
            
            ## Total infections
            ifelse(monthstartvac2()==1,
                   novaccine<-novaccine%>%
                       mutate(NNVoverallinf=1/((vaccineeffectiveness*(sum(hospinfwovac)+sum(nonhospinfwovac)))/populationsize)),
                   novaccine<-novaccine%>%
                       mutate(NNVoverallinf=1/((vaccineeffectiveness*(sum(hospinfwovac[monthstartvac2():nrow(novaccine)])+sum(nonhospinfwovac[monthstartvac2():nrow(novaccine)])))/populationsize))
            )
            
            return(novaccine)
            
            
        }#close "if hypothetical campaign"
        
    }) #close burdendataset
    
    
    
    ####################################################################################################
    ## List of burden averted results
    ####################################################################################################
    
    
    burdenavertedresultscoverage1timing2<-reactive({
        
        populationsize<-as.numeric(as.character(populationsize()))
        hnh<-as.numeric(as.character(hnh()))
        mai<-as.numeric(as.character(mai()))
        vaccineeffectiveness<-as.numeric(as.character(vaccineeffectiveness()))
        
        
        
        #################################################
        ## Results
        #################################################
        nonhospinfwovactot<-sum(burdendataset_coverage1timing2()$nonhospinfwovac)
        nonhospinfwvacctot<-sum(burdendataset_coverage1timing2()$nonhospinfwvac)
        nonhospinfaverted<-nonhospinfwovactot-nonhospinfwvacctot
        NNVnonhospinffinal<-burdendataset_coverage1timing2()$NNVnonhospinf[1]
        
        medicallyattendedinfwovactot<-sum(burdendataset_coverage1timing2()$medicallyattendedinfwovac)
        medicallyattendedinfwvactot<-sum(burdendataset_coverage1timing2()$medicalattedtoinfwvac)
        medicallyattendedinfaverted<-medicallyattendedinfwovactot-medicallyattendedinfwvactot
        NNVmedicallyattendtedfinal<-burdendataset_coverage1timing2()$NNVmedicalattendedinf[1]
        
        Totalinfwovacc<-sum(burdendataset_coverage1timing2()$overallinfwovac)
        Totalinfwvacc<-sum(burdendataset_coverage1timing2()$overallinfwvac)
        Totalinfaverted<-Totalinfwovacc-Totalinfwvacc
        NNVtotalinf<-burdendataset_coverage1timing2()$NNVoverallinf[1]
        
        hospinfwovactot<-sum(burdendataset_coverage1timing2()$hospinfwovac)
        hospinfwvacctot<-sum(burdendataset_coverage1timing2()$hospinfwvac)
        hospaverted<-hospinfwovactot-hospinfwvacctot
        NNVhospinffinal<-burdendataset_coverage1timing2()$NNVhospinf[1]
        
        
        nonhospPreventedfraction<-burdendataset_coverage1timing2()$PFnonhospinf[1]
        hospPreventedfraction<-burdendataset_coverage1timing2()$PFhospinf[1]
        
        
        indicator_results1<-c(nonhospinfwovactot, nonhospinfwvacctot, nonhospinfaverted, NNVnonhospinffinal,
                              Totalinfwovacc, Totalinfwvacc, Totalinfaverted, NNVtotalinf,
                              hospinfwovactot, hospinfwvacctot, hospaverted, NNVhospinffinal,
                              medicallyattendedinfwovactot, medicallyattendedinfwvactot, medicallyattendedinfaverted, NNVmedicallyattendtedfinal,
                              nonhospPreventedfraction)
        
        indicator_results_titles<-c("nonhospinfwovactot", "nonhospinfwvacctot", "nonhospinfaverted", "NNVnonhospinffinal",
                                    "Totalinfwovacc", "Totalinfwvacc", "Totalinfaverted", "NNVtotalinf",
                                    "hospinfwovactot", "hospinfwvacctot", "hospaverted", "NNVhospinffinal",
                                    "medicallyattendedinfwovactot", "medicallyattendedinfwvactot", "medicallyattendedinfaverted", "NNVmedicallyattendtedfinal",
                                    "nonhospPreventedfraction")
        
        indicator_results1<-cbind(indicator_results_titles, indicator_results1)
        
        
        return(indicator_results1)
        
        
    })#burdendataset_coverage1timing2
    
    
    
    #######################################################
    
    ##Deployment coverage 1, timing 3
    ######################################################
    
    
    burdendataset_coverage1timing3<- reactive({
        ################################ The output data when insufficient data has been entered   ##################################
        ############################################################################################################################
        
        
        if(scenario()=="Impact of current influenza vaccination campaign"){
            
            
            ##########################################################################################
            # Scenario of influenza circulation in presence of vaccine: Presence to absence of vaccine
            # This is a situation where the country conducts a vaccination campaign, we enter
            # actual burden inputs and vaccine coverage then compare this to a hypothetical scenario
            # where there is no vaccination campaign conducted
            ##########################################################################################
            
            
            
            
            hospinf<-c(hospitalizationsmonthly(), hospmonth2(), hospmonth3(), hospmonth4(), hospmonth5(), hospmonth6(),
                       hospmonth7(), hospmonth8(), hospmonth9(), hospmonth10(), hospmonth11(), hospmonth12())
            
            populationsize<-as.numeric(as.character(populationsize()))
            vaccineeffectiveness<-as.numeric(as.character(vaccineeffectiveness()))
            hnh<-as.numeric(as.character(hnh()))
            mai<-as.numeric(as.character(mai()))
            hospinf<-as.numeric(as.character(hospinf))
            
            novaccine<-hospinf%>%
                as.data.frame()%>%
                rename(hospinfwvac=1)%>%
                mutate(hospinfcumulwvac=cumsum(hospinfwvac))%>%
                mutate(nonhospinfwvac=hospinfwvac*hnh)%>%
                mutate(nonhospinfcumulwvac=cumsum(nonhospinfwvac))%>%
                mutate(overallinfwvac=hospinfwvac+nonhospinfwvac)%>%
                mutate(overallinfcumulwvac=cumsum(overallinfwvac))%>%
                mutate(medicalattedtoinfwvac=overallinfwvac*mai)%>%
                mutate(medicallyattendedinfcumulwvac=cumsum(medicalattedtoinfwvac))
            
            
            
            ##bind monthly coverage
            monthvaccov<-c(vaccinecoveragemonthly(), month2(), month3(), month4(), month5(), month6(),
                           month7(), month8(), month9(), month10(), month11(), month12())
            
            monthvaccov<-as.numeric(as.character(monthvaccov))
            
            totcoverage<-sum(monthvaccov)
            
            
            novaccine<-cbind(novaccine, monthvaccov)
            
            novaccine$covergdistrib<- c(vaccinecoveragemonthly3(), month23(), month33(), month43(), month53(), month63(), month73(), month83(), month93(), month103(), month113(), month123())
            
            novaccine$covergdistrib<-as.numeric(as.character(novaccine$covergdistrib))
            
            novaccine$monthvaccov<- novaccine$covergdistrib*coverage1() ## replace original monthvaccov column with the new monthly coverage - this is so that the variable doesn't have to be changed in the rest of the script
            
            
            ## month number
            novaccine$monthnum<-c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
            
            ## add monthly effectively vaccinated
            novaccine<-novaccine%>%
                mutate(effectivelyvacc=populationsize*monthvaccov*vaccineeffectiveness)%>%
                mutate(effectivelyvacccumul=cumsum(effectivelyvacc))
            
            
            
            
            #####################################################################
            # Estimated rates
            #####################################################################
            novaccine<-novaccine%>%
                mutate(susceptiblewvacc=populationsize-hospinfcumulwvac-nonhospinfcumulwvac-effectivelyvacccumul)
            
            #Create columns
            novaccine$irhospinf<-NA
            novaccine$irnonhospinf<-NA
            
            #Create first row
            novaccine[1,14]<-novaccine$hospinfwvac[1]/populationsize
            novaccine[1,15]<-novaccine$nonhospinfwvac[1]/populationsize
            
            
            ##Calculation uses last month's susceptible population
            for(i in 2:nrow(novaccine)){
                novaccine$irhospinf[i]<-novaccine$hospinfwvac[i]/novaccine$susceptiblewvacc[i-1]
                novaccine$irnonhospinf[i]<- novaccine$nonhospinfwvac[i]/novaccine$susceptiblewvacc[i-1]
            }
            
            #####################################################################
            # Scenario of influenza circulation in absence of vaccine
            #####################################################################
            
            ## These calculations are initialized by the 'Susceptible at start of month in absence of vaccine'
            ## column, which starts with the Target Population in the first month.
            
            remainsuscstartmonth<-c(populationsize, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
            novaccine<-cbind(novaccine, remainsuscstartmonth)
            
            #Create columns
            novaccine$hospinfwovac<-NA
            novaccine$hospinfcumulwovacc<-NA
            novaccine$nonhospinfwovac<-NA
            novaccine$nonhospinfcumulwovacc<-NA
            novaccine$remainingsusc<-NA
            
            #Create first row
            novaccine[1,17]<-novaccine$remainsuscstartmonth[1]*novaccine$irhospinf[1]
            novaccine[1,18]<-novaccine$hospinfwovac[1]
            novaccine[1,19]<-novaccine$remainsuscstartmonth[1]*novaccine$irnonhospinf[1]
            novaccine[1,20]<-novaccine$nonhospinfwovac[1]
            novaccine[1,21]<-populationsize-novaccine$hospinfcumulwovacc[1]-novaccine$nonhospinfcumulwovacc[1]
            
            
            #Create rows 2:12
            for(i in 2:nrow(novaccine)){
                novaccine$remainsuscstartmonth[i]<-novaccine$remainingsusc[i-1]
                novaccine$hospinfwovac[i]<-novaccine$remainsuscstartmonth[i]*novaccine$irhospinf[i]
                novaccine$hospinfcumulwovacc[i]<-sum(novaccine$hospinfwovac[1:i])
                novaccine$nonhospinfwovac[i]<-novaccine$remainsuscstartmonth[i]*novaccine$irnonhospinf[i]
                novaccine$nonhospinfcumulwovacc[i]<-sum(novaccine$nonhospinfwovac[1:i])
                novaccine$remainingsusc[i]<-populationsize-novaccine$hospinfcumulwovacc[i]-novaccine$nonhospinfcumulwovacc[i]
            }
            
            novaccine<-novaccine%>%
                mutate(overallinfwovac=hospinfwovac+nonhospinfwovac)%>%
                mutate(overallinfcumulwovacc=cumsum(overallinfwovac))%>%
                mutate(medicallyattendedinfwovac=overallinfwovac*mai)%>%
                mutate(medicalattedtoinfcumulwovacc=cumsum(medicallyattendedinfwovac))
            
            ######## Avoided events
            novaccine<-novaccine%>%
                mutate(AEhospinf=hospinfwovac-hospinfwvac)%>%
                mutate(AEnonhospinf=nonhospinfwovac-nonhospinfwvac)%>%
                mutate(AEmedicalattedtoinf=medicallyattendedinfwovac-medicalattedtoinfwvac)%>%
                mutate(AEoverallinf=overallinfwovac-overallinfwvac)
            
            
            ################ Prevented fraction
            ## Hospitalized infections
            ifelse (monthstartvac3()==1,
                    novaccine<-novaccine%>%
                        mutate(PFhospinf=sum(AEhospinf)/sum(hospinfwovac)),
                    novaccine<-novaccine%>%
                        mutate(PFhospinf=sum(AEhospinf[monthstartvac3():nrow(novaccine)]/sum(hospinfwovac[monthstartvac3():nrow(novaccine)])))
            )
            
            ## non-Hospitalized infections
            ifelse (monthstartvac3()==1,
                    novaccine<-novaccine%>%
                        mutate(PFnonhospinf=sum(AEnonhospinf)/sum(nonhospinfwovac)),
                    novaccine<-novaccine%>%
                        mutate(PFnonhospinf=sum(AEnonhospinf[monthstartvac3():nrow(novaccine)]/sum(nonhospinfwovac[monthstartvac3():nrow(novaccine)])))
            )
            
            
            ################ NNV -----------------------------------> why does this only go to month 8 in the Excel formula?????
            ## Hospitalized infections
            ifelse(monthstartvac3()==1,
                   novaccine<-novaccine%>%
                       mutate(NNVhospinf=1/((vaccineeffectiveness*(sum(hospinfwovac)))/populationsize)),
                   novaccine<-novaccine%>%
                       mutate(NNVhospinf=1/((vaccineeffectiveness*(sum(hospinfwovac[monthstartvac3():nrow(novaccine)])))/populationsize))
            )
            
            
            ## non-Hospitalized infections
            ifelse(monthstartvac3()==1,
                   novaccine<-novaccine%>%
                       mutate(NNVnonhospinf=1/((vaccineeffectiveness*(sum(nonhospinfwovac)))/populationsize)),
                   novaccine<-novaccine%>%
                       mutate(NNVnonhospinf=1/((vaccineeffectiveness*(sum(nonhospinfwovac[monthstartvac3():nrow(novaccine)])))/populationsize))
            )
            
            ## Medically infections
            ifelse(monthstartvac3()==1,
                   novaccine<-novaccine%>%
                       mutate(NNVmedicalattendedinf=1/((vaccineeffectiveness*(sum(medicallyattendedinfwovac)))/populationsize)),
                   novaccine<-novaccine%>%
                       mutate(NNVmedicalattendedinf=1/((vaccineeffectiveness*(sum(medicallyattendedinfwovac[monthstartvac3():nrow(novaccine)])))/populationsize))
            )
            
            ## Overall infections
            ifelse(monthstartvac3()==1,
                   novaccine<-novaccine%>%
                       mutate(NNVoverallinf=1/((vaccineeffectiveness*(sum(hospinfwovac)+sum(nonhospinfwovac)))/populationsize)),
                   novaccine<-novaccine%>%
                       mutate(NNVoverallinf=1/((vaccineeffectiveness*(sum(hospinfwovac[monthstartvac3():nrow(novaccine)])+sum(nonhospinfwovac[monthstartvac3():nrow(novaccine)])))/populationsize))
            )
            
            return(novaccine)
            
            
            
        } #close if current campaign
        
        
        
        if(scenario()=="Impact of hypothetical vaccination campaign"){
            
            ##########################################################################################
            # Scenario of influenza circulation in absence of vaccine: Absence to presence of vaccine
            # This is a scenario where the country has yet to conduct a vaccination campaign but would
            # like to see what impact a vaccine campaign may have.
            # The country enters the estimated number of hospitalization they observed and then we compare
            # this to a hypothetical vaccination campaign
            ##########################################################################################
            
            hospinf<-c(hospitalizationsmonthly(), hospmonth2(), hospmonth3(), hospmonth4(), hospmonth5(), hospmonth6(),
                       hospmonth7(), hospmonth8(), hospmonth9(), hospmonth10(), hospmonth11(), hospmonth12())
            
            populationsize<-as.numeric(as.character(populationsize()))
            hnh<-as.numeric(as.character(hnh()))
            mai<-as.numeric(as.character(mai()))
            hospinf<-as.numeric(as.character(hospinf))
            vaccineeffectiveness<-as.numeric(as.character(vaccineeffectiveness()))
            
            #####################Observed influenza in the absence of vaccine
            novaccine<-hospinf%>%
                as.data.frame()%>%
                rename(hospinfwovac=1)%>%
                mutate(hospinfcumulwovac=cumsum(hospinfwovac))%>%
                mutate(nonhospinfwovac=hospinfwovac*hnh)%>%
                mutate(nonhospinfcumulwovac=cumsum(nonhospinfwovac))%>%
                mutate(remainingsusc=populationsize-hospinfcumulwovac-nonhospinfcumulwovac)
            
            #create remainsuscstartmonth column with target population and rows 1:11 of remainingsusc
            remainsuscstartmonth<-populationsize%>%
                as.data.frame()%>%
                rename(remainsuscstartmonth=1)
            
            remainsuscstartmonth2<-as.data.frame(novaccine$remainingsusc[1:11])%>%
                as.data.frame()%>%
                rename(remainsuscstartmonth=1)
            
            remainsuscstartmonth<-rbind(remainsuscstartmonth, remainsuscstartmonth2)
            
            novaccine<-cbind(novaccine, remainsuscstartmonth)
            
            novaccine<-novaccine%>%
                mutate(overallinfwovac=hospinfwovac+nonhospinfwovac)%>%
                mutate(overallinfcumulwovac=cumsum(overallinfwovac))%>%
                mutate(medicallyattendedinfwovac=overallinfwovac*mai)%>%
                mutate(medicallyattendedinfcumulwovac=cumsum(medicallyattendedinfwovac))
            
            
            
            #####################################################################
            # Incidence rates in the absence of vaccine
            #####################################################################
            novaccine<-novaccine%>%
                mutate(irhospinf=hospinfwovac/remainsuscstartmonth)%>%
                mutate(irnonhospinf=nonhospinfwovac/remainsuscstartmonth)
            
            
            
            
            ############################ Burden inputs with hypothetical vaccination campaign
            #Create columns
            novaccine$hospinfwvac<-NA
            novaccine$hospinfcumulwvac<-NA
            novaccine$nonhospinfwvac<-NA
            novaccine$nonhospinfcumulwvac<-NA
            
            
            
            #Create first row
            novaccine[1,13]<-populationsize*novaccine$irhospinf[1]
            novaccine[1,14]<-novaccine$hospinfwvac[1]
            novaccine[1,15]<-populationsize*novaccine$irnonhospinf[1]
            novaccine[1,16]<-novaccine$nonhospinfwvac[1]
            
            ##bind monthly coverage
            monthvaccov<-c(vaccinecoveragemonthly(), month2(), month3(), month4(), month5(), month6(),
                           month7(), month8(), month9(), month10(), month11(), month12())
            
            monthvaccov<-as.numeric(as.character(monthvaccov))
            
            totcoverage<-sum(monthvaccov)
            
            
            novaccine<-cbind(novaccine, monthvaccov)
            
            novaccine$covergdistrib<- c(vaccinecoveragemonthly3(), month23(), month33(), month43(), month53(), month63(), month73(), month83(), month93(), month103(), month113(), month123())
            
            novaccine$covergdistrib<-as.numeric(as.character(novaccine$covergdistrib))
            
            novaccine$monthvaccov<- novaccine$covergdistrib*coverage1() ## replace original monthvaccov column with the new monthly coverage - this is so that the variable doesn't have to be changed in the rest of the script
            
            
            ##monthly effectively vaccinated and susceptible with hypothetical vaccination campaign
            novaccine<-novaccine%>%
                mutate(effectivelyvacc=populationsize*monthvaccov*vaccineeffectiveness)%>%
                mutate(effectivelycumulvacc=cumsum(effectivelyvacc))%>%
                mutate(suscwvacc=populationsize-hospinfcumulwvac-nonhospinfcumulwvac-effectivelyvacc)
            
            ##Then calculate the rest of hospinfwovac and nonhospinfwovac. Calculation uses last month's incidence rate
            for(i in 2:nrow(novaccine)){
                novaccine$hospinfwvac[i]<-novaccine$suscwvacc[i-1]*novaccine$irhospinf[i]
                novaccine$hospinfcumulwvac[i]<-sum(novaccine$hospinfwvac[1:i])
                novaccine$nonhospinfwvac[i]<- novaccine$suscwvacc[i-1]*novaccine$irnonhospinf[i]
                novaccine$nonhospinfcumulwvac[i]<-sum(novaccine$nonhospinfwvac[1:i])
                novaccine$suscwvacc[i]=populationsize-novaccine$hospinfcumulwvac[i]-novaccine$nonhospinfcumulwvac[i]-novaccine$effectivelycumulvacc[i]
            }
            
            
            ## month number
            novaccine$monthnum<-c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
            
            
            ## Overall infections with hypothetical vaccination campaign
            novaccine<-novaccine%>%
                mutate(overallinfwvac=hospinfwvac+nonhospinfwvac)%>%
                mutate(overallinfcumulwvacc=cumsum(overallinfwvac))
            
            ## Medically attended with hypothetical vaccination campaign
            novaccine<-novaccine%>%
                mutate(medicalattedtoinfwvac=overallinfwvac*mai)
            
            
            ######## Avoided events
            novaccine<-novaccine%>%
                mutate(AEhospinf=hospinfwovac-hospinfwvac)%>%
                mutate(AEnonhospinf=nonhospinfwovac-nonhospinfwvac)%>%
                mutate(AEmedicalattedtoinf=medicallyattendedinfwovac-medicalattedtoinfwvac)%>%
                mutate(AEoverallinf=overallinfwovac-overallinfwvac)
            
            
            
            ################ Prevented fraction
            ## Hospitalized infections
            ifelse (monthstartvac3()==1,
                    novaccine<-novaccine%>%
                        mutate(PFhospinf=sum(AEhospinf)/sum(hospinfwovac)),
                    novaccine<-novaccine%>%
                        mutate(PFhospinf=sum(AEhospinf[monthstartvac3():nrow(novaccine)]/sum(hospinfwovac[monthstartvac3():nrow(novaccine)])))
            )
            
            ## non-Hospitalized infections
            ifelse (monthstartvac3()==1,
                    novaccine<-novaccine%>%
                        mutate(PFnonhospinf=sum(AEnonhospinf)/sum(nonhospinfwovac)),
                    novaccine<-novaccine%>%
                        mutate(PFnonhospinf=sum(AEnonhospinf[monthstartvac3():nrow(novaccine)]/sum(nonhospinfwovac[monthstartvac3():nrow(novaccine)])))
            )
            
            
            
            
            ################ NNV -----------------------------------> why does this only go to month 8 in the Excel formula?????
            ## Hospitalized infections
            ifelse(monthstartvac3()==1,
                   novaccine<-novaccine%>%
                       mutate(NNVhospinf=1/((vaccineeffectiveness*(sum(hospinfwovac)))/populationsize)),
                   novaccine<-novaccine%>%
                       mutate(NNVhospinf=1/((vaccineeffectiveness*(sum(hospinfwovac[monthstartvac3():nrow(novaccine)])))/populationsize))
            )
            
            
            ## non-Hospitalized infections
            ifelse(monthstartvac3()==1,
                   novaccine<-novaccine%>%
                       mutate(NNVnonhospinf=1/((vaccineeffectiveness*(sum(nonhospinfwovac)))/populationsize)),
                   novaccine<-novaccine%>%
                       mutate(NNVnonhospinf=1/((vaccineeffectiveness*(sum(nonhospinfwovac[monthstartvac3():nrow(novaccine)])))/populationsize))
            )
            
            ## Medically attended infections
            ifelse(monthstartvac3()==1,
                   novaccine<-novaccine%>%
                       mutate(NNVmedicalattendedinf=1/((vaccineeffectiveness*(sum(medicallyattendedinfwovac)))/populationsize)),
                   novaccine<-novaccine%>%
                       mutate(NNVmedicalattendedinf=1/((vaccineeffectiveness*(sum(medicallyattendedinfwovac[monthstartvac3():nrow(novaccine)])))/populationsize))
            )
            
            ## Total infections
            ifelse(monthstartvac3()==1,
                   novaccine<-novaccine%>%
                       mutate(NNVoverallinf=1/((vaccineeffectiveness*(sum(hospinfwovac)+sum(nonhospinfwovac)))/populationsize)),
                   novaccine<-novaccine%>%
                       mutate(NNVoverallinf=1/((vaccineeffectiveness*(sum(hospinfwovac[monthstartvac3():nrow(novaccine)])+sum(nonhospinfwovac[monthstartvac3():nrow(novaccine)])))/populationsize))
            )
            
            return(novaccine)
            
            
        }#close "if hypothetical campaign"
        
    }) #close burdendataset
    
    
    
    ####################################################################################################
    ## List of burden averted results
    ####################################################################################################
    
    
    burdenavertedresultscoverage1timing3<-reactive({
        
        populationsize<-as.numeric(as.character(populationsize()))
        hnh<-as.numeric(as.character(hnh()))
        mai<-as.numeric(as.character(mai()))
        vaccineeffectiveness<-as.numeric(as.character(vaccineeffectiveness()))
        
        
        
        #################################################
        ## Results
        #################################################
        nonhospinfwovactot<-sum(burdendataset_coverage1timing3()$nonhospinfwovac)
        nonhospinfwvacctot<-sum(burdendataset_coverage1timing3()$nonhospinfwvac)
        nonhospinfaverted<-nonhospinfwovactot-nonhospinfwvacctot
        NNVnonhospinffinal<-burdendataset_coverage1timing3()$NNVnonhospinf[1]
        
        medicallyattendedinfwovactot<-sum(burdendataset_coverage1timing3()$medicallyattendedinfwovac)
        medicallyattendedinfwvactot<-sum(burdendataset_coverage1timing3()$medicalattedtoinfwvac)
        medicallyattendedinfaverted<-medicallyattendedinfwovactot-medicallyattendedinfwvactot
        NNVmedicallyattendtedfinal<-burdendataset_coverage1timing3()$NNVmedicalattendedinf[1]
        
        Totalinfwovacc<-sum(burdendataset_coverage1timing3()$overallinfwovac)
        Totalinfwvacc<-sum(burdendataset_coverage1timing3()$overallinfwvac)
        Totalinfaverted<-Totalinfwovacc-Totalinfwvacc
        NNVtotalinf<-burdendataset_coverage1timing3()$NNVoverallinf[1]
        
        hospinfwovactot<-sum(burdendataset_coverage1timing3()$hospinfwovac)
        hospinfwvacctot<-sum(burdendataset_coverage1timing3()$hospinfwvac)
        hospaverted<-hospinfwovactot-hospinfwvacctot
        NNVhospinffinal<-burdendataset_coverage1timing3()$NNVhospinf[1]
        
        
        nonhospPreventedfraction<-burdendataset_coverage1timing3()$PFnonhospinf[1]
        hospPreventedfraction<-burdendataset_coverage1timing3()$PFhospinf[1]
        
        
        indicator_results1<-c(nonhospinfwovactot, nonhospinfwvacctot, nonhospinfaverted, NNVnonhospinffinal,
                              Totalinfwovacc, Totalinfwvacc, Totalinfaverted, NNVtotalinf,
                              hospinfwovactot, hospinfwvacctot, hospaverted, NNVhospinffinal,
                              medicallyattendedinfwovactot, medicallyattendedinfwvactot, medicallyattendedinfaverted, NNVmedicallyattendtedfinal,
                              nonhospPreventedfraction)
        
        indicator_results_titles<-c("nonhospinfwovactot", "nonhospinfwvacctot", "nonhospinfaverted", "NNVnonhospinffinal",
                                    "Totalinfwovacc", "Totalinfwvacc", "Totalinfaverted", "NNVtotalinf",
                                    "hospinfwovactot", "hospinfwvacctot", "hospaverted", "NNVhospinffinal",
                                    "medicallyattendedinfwovactot", "medicallyattendedinfwvactot", "medicallyattendedinfaverted", "NNVmedicallyattendtedfinal",
                                    "nonhospPreventedfraction")
        
        indicator_results1<-cbind(indicator_results_titles, indicator_results1)
        
        
        return(indicator_results1)
        
        
    })#burdendataset_coverage1timing3
    
    
    
    
    #######################################################
    
    ##Deployment coverage 1, timing 4
    ######################################################
    
    
    burdendataset_coverage1timing4<- reactive({
        ################################ The output data when insufficient data has been entered   ##################################
        ############################################################################################################################
        
        
        if(scenario()=="Impact of current influenza vaccination campaign"){
            
            
            ##########################################################################################
            # Scenario of influenza circulation in presence of vaccine: Presence to absence of vaccine
            # This is a situation where the country conducts a vaccination campaign, we enter
            # actual burden inputs and vaccine coverage then compare this to a hypothetical scenario
            # where there is no vaccination campaign conducted
            ##########################################################################################
            
            
            
            
            hospinf<-c(hospitalizationsmonthly(), hospmonth2(), hospmonth3(), hospmonth4(), hospmonth5(), hospmonth6(),
                       hospmonth7(), hospmonth8(), hospmonth9(), hospmonth10(), hospmonth11(), hospmonth12())
            
            populationsize<-as.numeric(as.character(populationsize()))
            vaccineeffectiveness<-as.numeric(as.character(vaccineeffectiveness()))
            hnh<-as.numeric(as.character(hnh()))
            mai<-as.numeric(as.character(mai()))
            hospinf<-as.numeric(as.character(hospinf))
            
            novaccine<-hospinf%>%
                as.data.frame()%>%
                rename(hospinfwvac=1)%>%
                mutate(hospinfcumulwvac=cumsum(hospinfwvac))%>%
                mutate(nonhospinfwvac=hospinfwvac*hnh)%>%
                mutate(nonhospinfcumulwvac=cumsum(nonhospinfwvac))%>%
                mutate(overallinfwvac=hospinfwvac+nonhospinfwvac)%>%
                mutate(overallinfcumulwvac=cumsum(overallinfwvac))%>%
                mutate(medicalattedtoinfwvac=overallinfwvac*mai)%>%
                mutate(medicallyattendedinfcumulwvac=cumsum(medicalattedtoinfwvac))
            
            
            
            ##bind monthly coverage
            monthvaccov<-c(vaccinecoveragemonthly(), month2(), month3(), month4(), month5(), month6(),
                           month7(), month8(), month9(), month10(), month11(), month12())
            
            monthvaccov<-as.numeric(as.character(monthvaccov))
            
            totcoverage<-sum(monthvaccov)
            
            
            novaccine<-cbind(novaccine, monthvaccov)
            
            novaccine$covergdistrib<- c(vaccinecoveragemonthly4(), month24(), month34(), month44(), month54(), month64(), month74(), month84(), month94(), month104(), month114(), month124())
            
            novaccine$covergdistrib<-as.numeric(as.character(novaccine$covergdistrib))
            
            novaccine$monthvaccov<- novaccine$covergdistrib*coverage1() ## replace original monthvaccov column with the new monthly coverage - this is so that the variable doesn't have to be changed in the rest of the script
            
            
            ## month number
            novaccine$monthnum<-c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
            
            ## add monthly effectively vaccinated
            novaccine<-novaccine%>%
                mutate(effectivelyvacc=populationsize*monthvaccov*vaccineeffectiveness)%>%
                mutate(effectivelyvacccumul=cumsum(effectivelyvacc))
            
            
            
            
            #####################################################################
            # Estimated rates
            #####################################################################
            novaccine<-novaccine%>%
                mutate(susceptiblewvacc=populationsize-hospinfcumulwvac-nonhospinfcumulwvac-effectivelyvacccumul)
            
            #Create columns
            novaccine$irhospinf<-NA
            novaccine$irnonhospinf<-NA
            
            #Create first row
            novaccine[1,14]<-novaccine$hospinfwvac[1]/populationsize
            novaccine[1,15]<-novaccine$nonhospinfwvac[1]/populationsize
            
            
            ##Calculation uses last month's susceptible population
            for(i in 2:nrow(novaccine)){
                novaccine$irhospinf[i]<-novaccine$hospinfwvac[i]/novaccine$susceptiblewvacc[i-1]
                novaccine$irnonhospinf[i]<- novaccine$nonhospinfwvac[i]/novaccine$susceptiblewvacc[i-1]
            }
            
            #####################################################################
            # Scenario of influenza circulation in absence of vaccine
            #####################################################################
            
            ## These calculations are initialized by the 'Susceptible at start of month in absence of vaccine'
            ## column, which starts with the Target Population in the first month.
            
            remainsuscstartmonth<-c(populationsize, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
            novaccine<-cbind(novaccine, remainsuscstartmonth)
            
            #Create columns
            novaccine$hospinfwovac<-NA
            novaccine$hospinfcumulwovacc<-NA
            novaccine$nonhospinfwovac<-NA
            novaccine$nonhospinfcumulwovacc<-NA
            novaccine$remainingsusc<-NA
            
            #Create first row
            novaccine[1,17]<-novaccine$remainsuscstartmonth[1]*novaccine$irhospinf[1]
            novaccine[1,18]<-novaccine$hospinfwovac[1]
            novaccine[1,19]<-novaccine$remainsuscstartmonth[1]*novaccine$irnonhospinf[1]
            novaccine[1,20]<-novaccine$nonhospinfwovac[1]
            novaccine[1,21]<-populationsize-novaccine$hospinfcumulwovacc[1]-novaccine$nonhospinfcumulwovacc[1]
            
            
            #Create rows 2:12
            for(i in 2:nrow(novaccine)){
                novaccine$remainsuscstartmonth[i]<-novaccine$remainingsusc[i-1]
                novaccine$hospinfwovac[i]<-novaccine$remainsuscstartmonth[i]*novaccine$irhospinf[i]
                novaccine$hospinfcumulwovacc[i]<-sum(novaccine$hospinfwovac[1:i])
                novaccine$nonhospinfwovac[i]<-novaccine$remainsuscstartmonth[i]*novaccine$irnonhospinf[i]
                novaccine$nonhospinfcumulwovacc[i]<-sum(novaccine$nonhospinfwovac[1:i])
                novaccine$remainingsusc[i]<-populationsize-novaccine$hospinfcumulwovacc[i]-novaccine$nonhospinfcumulwovacc[i]
            }
            
            novaccine<-novaccine%>%
                mutate(overallinfwovac=hospinfwovac+nonhospinfwovac)%>%
                mutate(overallinfcumulwovacc=cumsum(overallinfwovac))%>%
                mutate(medicallyattendedinfwovac=overallinfwovac*mai)%>%
                mutate(medicalattedtoinfcumulwovacc=cumsum(medicallyattendedinfwovac))
            
            ######## Avoided events
            novaccine<-novaccine%>%
                mutate(AEhospinf=hospinfwovac-hospinfwvac)%>%
                mutate(AEnonhospinf=nonhospinfwovac-nonhospinfwvac)%>%
                mutate(AEmedicalattedtoinf=medicallyattendedinfwovac-medicalattedtoinfwvac)%>%
                mutate(AEoverallinf=overallinfwovac-overallinfwvac)
            
            
            ################ Prevented fraction
            ## Hospitalized infections
            ifelse (monthstartvac4()==1,
                    novaccine<-novaccine%>%
                        mutate(PFhospinf=sum(AEhospinf)/sum(hospinfwovac)),
                    novaccine<-novaccine%>%
                        mutate(PFhospinf=sum(AEhospinf[monthstartvac4():nrow(novaccine)]/sum(hospinfwovac[monthstartvac4():nrow(novaccine)])))
            )
            
            ## non-Hospitalized infections
            ifelse (monthstartvac4()==1,
                    novaccine<-novaccine%>%
                        mutate(PFnonhospinf=sum(AEnonhospinf)/sum(nonhospinfwovac)),
                    novaccine<-novaccine%>%
                        mutate(PFnonhospinf=sum(AEnonhospinf[monthstartvac4():nrow(novaccine)]/sum(nonhospinfwovac[monthstartvac4():nrow(novaccine)])))
            )
            
            
            ################ NNV -----------------------------------> why does this only go to month 8 in the Excel formula?????
            ## Hospitalized infections
            ifelse(monthstartvac4()==1,
                   novaccine<-novaccine%>%
                       mutate(NNVhospinf=1/((vaccineeffectiveness*(sum(hospinfwovac)))/populationsize)),
                   novaccine<-novaccine%>%
                       mutate(NNVhospinf=1/((vaccineeffectiveness*(sum(hospinfwovac[monthstartvac4():nrow(novaccine)])))/populationsize))
            )
            
            
            ## non-Hospitalized infections
            ifelse(monthstartvac4()==1,
                   novaccine<-novaccine%>%
                       mutate(NNVnonhospinf=1/((vaccineeffectiveness*(sum(nonhospinfwovac)))/populationsize)),
                   novaccine<-novaccine%>%
                       mutate(NNVnonhospinf=1/((vaccineeffectiveness*(sum(nonhospinfwovac[monthstartvac4():nrow(novaccine)])))/populationsize))
            )
            
            ## Medically infections
            ifelse(monthstartvac4()==1,
                   novaccine<-novaccine%>%
                       mutate(NNVmedicalattendedinf=1/((vaccineeffectiveness*(sum(medicallyattendedinfwovac)))/populationsize)),
                   novaccine<-novaccine%>%
                       mutate(NNVmedicalattendedinf=1/((vaccineeffectiveness*(sum(medicallyattendedinfwovac[monthstartvac4():nrow(novaccine)])))/populationsize))
            )
            
            ## Overall infections
            ifelse(monthstartvac4()==1,
                   novaccine<-novaccine%>%
                       mutate(NNVoverallinf=1/((vaccineeffectiveness*(sum(hospinfwovac)+sum(nonhospinfwovac)))/populationsize)),
                   novaccine<-novaccine%>%
                       mutate(NNVoverallinf=1/((vaccineeffectiveness*(sum(hospinfwovac[monthstartvac4():nrow(novaccine)])+sum(nonhospinfwovac[monthstartvac4():nrow(novaccine)])))/populationsize))
            )
            
            return(novaccine)
            
            
            
        } #close if current campaign
        
        
        
        if(scenario()=="Impact of hypothetical vaccination campaign"){
            
            ##########################################################################################
            # Scenario of influenza circulation in absence of vaccine: Absence to presence of vaccine
            # This is a scenario where the country has yet to conduct a vaccination campaign but would
            # like to see what impact a vaccine campaign may have.
            # The country enters the estimated number of hospitalization they observed and then we compare
            # this to a hypothetical vaccination campaign
            ##########################################################################################
            
            hospinf<-c(hospitalizationsmonthly(), hospmonth2(), hospmonth3(), hospmonth4(), hospmonth5(), hospmonth6(),
                       hospmonth7(), hospmonth8(), hospmonth9(), hospmonth10(), hospmonth11(), hospmonth12())
            
            populationsize<-as.numeric(as.character(populationsize()))
            hnh<-as.numeric(as.character(hnh()))
            mai<-as.numeric(as.character(mai()))
            hospinf<-as.numeric(as.character(hospinf))
            vaccineeffectiveness<-as.numeric(as.character(vaccineeffectiveness()))
            
            #####################Observed influenza in the absence of vaccine
            novaccine<-hospinf%>%
                as.data.frame()%>%
                rename(hospinfwovac=1)%>%
                mutate(hospinfcumulwovac=cumsum(hospinfwovac))%>%
                mutate(nonhospinfwovac=hospinfwovac*hnh)%>%
                mutate(nonhospinfcumulwovac=cumsum(nonhospinfwovac))%>%
                mutate(remainingsusc=populationsize-hospinfcumulwovac-nonhospinfcumulwovac)
            
            #create remainsuscstartmonth column with target population and rows 1:11 of remainingsusc
            remainsuscstartmonth<-populationsize%>%
                as.data.frame()%>%
                rename(remainsuscstartmonth=1)
            
            remainsuscstartmonth2<-as.data.frame(novaccine$remainingsusc[1:11])%>%
                as.data.frame()%>%
                rename(remainsuscstartmonth=1)
            
            remainsuscstartmonth<-rbind(remainsuscstartmonth, remainsuscstartmonth2)
            
            novaccine<-cbind(novaccine, remainsuscstartmonth)
            
            novaccine<-novaccine%>%
                mutate(overallinfwovac=hospinfwovac+nonhospinfwovac)%>%
                mutate(overallinfcumulwovac=cumsum(overallinfwovac))%>%
                mutate(medicallyattendedinfwovac=overallinfwovac*mai)%>%
                mutate(medicallyattendedinfcumulwovac=cumsum(medicallyattendedinfwovac))
            
            
            
            #####################################################################
            # Incidence rates in the absence of vaccine
            #####################################################################
            novaccine<-novaccine%>%
                mutate(irhospinf=hospinfwovac/remainsuscstartmonth)%>%
                mutate(irnonhospinf=nonhospinfwovac/remainsuscstartmonth)
            
            
            
            
            ############################ Burden inputs with hypothetical vaccination campaign
            #Create columns
            novaccine$hospinfwvac<-NA
            novaccine$hospinfcumulwvac<-NA
            novaccine$nonhospinfwvac<-NA
            novaccine$nonhospinfcumulwvac<-NA
            
            
            
            #Create first row
            novaccine[1,13]<-populationsize*novaccine$irhospinf[1]
            novaccine[1,14]<-novaccine$hospinfwvac[1]
            novaccine[1,15]<-populationsize*novaccine$irnonhospinf[1]
            novaccine[1,16]<-novaccine$nonhospinfwvac[1]
            
            ##bind monthly coverage
            monthvaccov<-c(vaccinecoveragemonthly(), month2(), month3(), month4(), month5(), month6(),
                           month7(), month8(), month9(), month10(), month11(), month12())
            
            monthvaccov<-as.numeric(as.character(monthvaccov))
            
            totcoverage<-sum(monthvaccov)
            
            
            novaccine<-cbind(novaccine, monthvaccov)
            
            novaccine$covergdistrib<- c(vaccinecoveragemonthly4(), month24(), month34(), month44(), month54(), month64(), month74(), month84(), month94(), month104(), month114(), month124())
            
            novaccine$covergdistrib<-as.numeric(as.character(novaccine$covergdistrib))
            
            novaccine$monthvaccov<- novaccine$covergdistrib*coverage1() ## replace original monthvaccov column with the new monthly coverage - this is so that the variable doesn't have to be changed in the rest of the script
            
            
            ##monthly effectively vaccinated and susceptible with hypothetical vaccination campaign
            novaccine<-novaccine%>%
                mutate(effectivelyvacc=populationsize*monthvaccov*vaccineeffectiveness)%>%
                mutate(effectivelycumulvacc=cumsum(effectivelyvacc))%>%
                mutate(suscwvacc=populationsize-hospinfcumulwvac-nonhospinfcumulwvac-effectivelyvacc)
            
            ##Then calculate the rest of hospinfwovac and nonhospinfwovac. Calculation uses last month's incidence rate
            for(i in 2:nrow(novaccine)){
                novaccine$hospinfwvac[i]<-novaccine$suscwvacc[i-1]*novaccine$irhospinf[i]
                novaccine$hospinfcumulwvac[i]<-sum(novaccine$hospinfwvac[1:i])
                novaccine$nonhospinfwvac[i]<- novaccine$suscwvacc[i-1]*novaccine$irnonhospinf[i]
                novaccine$nonhospinfcumulwvac[i]<-sum(novaccine$nonhospinfwvac[1:i])
                novaccine$suscwvacc[i]=populationsize-novaccine$hospinfcumulwvac[i]-novaccine$nonhospinfcumulwvac[i]-novaccine$effectivelycumulvacc[i]
            }
            
            
            ## month number
            novaccine$monthnum<-c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
            
            
            ## Overall infections with hypothetical vaccination campaign
            novaccine<-novaccine%>%
                mutate(overallinfwvac=hospinfwvac+nonhospinfwvac)%>%
                mutate(overallinfcumulwvacc=cumsum(overallinfwvac))
            
            ## Medically attended with hypothetical vaccination campaign
            novaccine<-novaccine%>%
                mutate(medicalattedtoinfwvac=overallinfwvac*mai)
            
            
            ######## Avoided events
            novaccine<-novaccine%>%
                mutate(AEhospinf=hospinfwovac-hospinfwvac)%>%
                mutate(AEnonhospinf=nonhospinfwovac-nonhospinfwvac)%>%
                mutate(AEmedicalattedtoinf=medicallyattendedinfwovac-medicalattedtoinfwvac)%>%
                mutate(AEoverallinf=overallinfwovac-overallinfwvac)
            
            
            
            ################ Prevented fraction
            ## Hospitalized infections
            ifelse (monthstartvac4()==1,
                    novaccine<-novaccine%>%
                        mutate(PFhospinf=sum(AEhospinf)/sum(hospinfwovac)),
                    novaccine<-novaccine%>%
                        mutate(PFhospinf=sum(AEhospinf[monthstartvac4():nrow(novaccine)]/sum(hospinfwovac[monthstartvac4():nrow(novaccine)])))
            )
            
            ## non-Hospitalized infections
            ifelse (monthstartvac4()==1,
                    novaccine<-novaccine%>%
                        mutate(PFnonhospinf=sum(AEnonhospinf)/sum(nonhospinfwovac)),
                    novaccine<-novaccine%>%
                        mutate(PFnonhospinf=sum(AEnonhospinf[monthstartvac4():nrow(novaccine)]/sum(nonhospinfwovac[monthstartvac4():nrow(novaccine)])))
            )
            
            
            
            
            ################ NNV -----------------------------------> why does this only go to month 8 in the Excel formula?????
            ## Hospitalized infections
            ifelse(monthstartvac4()==1,
                   novaccine<-novaccine%>%
                       mutate(NNVhospinf=1/((vaccineeffectiveness*(sum(hospinfwovac)))/populationsize)),
                   novaccine<-novaccine%>%
                       mutate(NNVhospinf=1/((vaccineeffectiveness*(sum(hospinfwovac[monthstartvac4():nrow(novaccine)])))/populationsize))
            )
            
            
            ## non-Hospitalized infections
            ifelse(monthstartvac4()==1,
                   novaccine<-novaccine%>%
                       mutate(NNVnonhospinf=1/((vaccineeffectiveness*(sum(nonhospinfwovac)))/populationsize)),
                   novaccine<-novaccine%>%
                       mutate(NNVnonhospinf=1/((vaccineeffectiveness*(sum(nonhospinfwovac[monthstartvac4():nrow(novaccine)])))/populationsize))
            )
            
            ## Medically attended infections
            ifelse(monthstartvac4()==1,
                   novaccine<-novaccine%>%
                       mutate(NNVmedicalattendedinf=1/((vaccineeffectiveness*(sum(medicallyattendedinfwovac)))/populationsize)),
                   novaccine<-novaccine%>%
                       mutate(NNVmedicalattendedinf=1/((vaccineeffectiveness*(sum(medicallyattendedinfwovac[monthstartvac4():nrow(novaccine)])))/populationsize))
            )
            
            ## Total infections
            ifelse(monthstartvac4()==1,
                   novaccine<-novaccine%>%
                       mutate(NNVoverallinf=1/((vaccineeffectiveness*(sum(hospinfwovac)+sum(nonhospinfwovac)))/populationsize)),
                   novaccine<-novaccine%>%
                       mutate(NNVoverallinf=1/((vaccineeffectiveness*(sum(hospinfwovac[monthstartvac4():nrow(novaccine)])+sum(nonhospinfwovac[monthstartvac4():nrow(novaccine)])))/populationsize))
            )
            
            return(novaccine)
            
            
        }#close "if hypothetical campaign"
        
    }) #close burdendataset
    
    
    
    ####################################################################################################
    ## List of burden averted results
    ####################################################################################################
    
    
    burdenavertedresultscoverage1timing4<-reactive({
        
        populationsize<-as.numeric(as.character(populationsize()))
        hnh<-as.numeric(as.character(hnh()))
        mai<-as.numeric(as.character(mai()))
        vaccineeffectiveness<-as.numeric(as.character(vaccineeffectiveness()))
        
        
        
        #################################################
        ## Results
        #################################################
        nonhospinfwovactot<-sum(burdendataset_coverage1timing4()$nonhospinfwovac)
        nonhospinfwvacctot<-sum(burdendataset_coverage1timing4()$nonhospinfwvac)
        nonhospinfaverted<-nonhospinfwovactot-nonhospinfwvacctot
        NNVnonhospinffinal<-burdendataset_coverage1timing4()$NNVnonhospinf[1]
        
        medicallyattendedinfwovactot<-sum(burdendataset_coverage1timing4()$medicallyattendedinfwovac)
        medicallyattendedinfwvactot<-sum(burdendataset_coverage1timing4()$medicalattedtoinfwvac)
        medicallyattendedinfaverted<-medicallyattendedinfwovactot-medicallyattendedinfwvactot
        NNVmedicallyattendtedfinal<-burdendataset_coverage1timing4()$NNVmedicalattendedinf[1]
        
        Totalinfwovacc<-sum(burdendataset_coverage1timing4()$overallinfwovac)
        Totalinfwvacc<-sum(burdendataset_coverage1timing4()$overallinfwvac)
        Totalinfaverted<-Totalinfwovacc-Totalinfwvacc
        NNVtotalinf<-burdendataset_coverage1timing4()$NNVoverallinf[1]
        
        hospinfwovactot<-sum(burdendataset_coverage1timing4()$hospinfwovac)
        hospinfwvacctot<-sum(burdendataset_coverage1timing4()$hospinfwvac)
        hospaverted<-hospinfwovactot-hospinfwvacctot
        NNVhospinffinal<-burdendataset_coverage1timing4()$NNVhospinf[1]
        
        
        nonhospPreventedfraction<-burdendataset_coverage1timing4()$PFnonhospinf[1]
        hospPreventedfraction<-burdendataset_coverage1timing4()$PFhospinf[1]
        
        
        indicator_results1<-c(nonhospinfwovactot, nonhospinfwvacctot, nonhospinfaverted, NNVnonhospinffinal,
                              Totalinfwovacc, Totalinfwvacc, Totalinfaverted, NNVtotalinf,
                              hospinfwovactot, hospinfwvacctot, hospaverted, NNVhospinffinal,
                              medicallyattendedinfwovactot, medicallyattendedinfwvactot, medicallyattendedinfaverted, NNVmedicallyattendtedfinal,
                              nonhospPreventedfraction)
        
        indicator_results_titles<-c("nonhospinfwovactot", "nonhospinfwvacctot", "nonhospinfaverted", "NNVnonhospinffinal",
                                    "Totalinfwovacc", "Totalinfwvacc", "Totalinfaverted", "NNVtotalinf",
                                    "hospinfwovactot", "hospinfwvacctot", "hospaverted", "NNVhospinffinal",
                                    "medicallyattendedinfwovactot", "medicallyattendedinfwvactot", "medicallyattendedinfaverted", "NNVmedicallyattendtedfinal",
                                    "nonhospPreventedfraction")
        
        indicator_results1<-cbind(indicator_results_titles, indicator_results1)
        
        
        return(indicator_results1)
        
        
    })#burdendataset_coverage1timing4
    
    
    
    
    #######################################################
    
    ##Deployment coverage 2, timing 1
    ######################################################
    
    
    burdendataset_coverage2timing1<- reactive({
        ################################ The output data when insufficient data has been entered   ##################################
        ############################################################################################################################
        
        
        if(scenario()=="Impact of current influenza vaccination campaign"){
            
            
            ##########################################################################################
            # Scenario of influenza circulation in presence of vaccine: Presence to absence of vaccine
            # This is a situation where the country conducts a vaccination campaign, we enter
            # actual burden inputs and vaccine coverage then compare this to a hypothetical scenario
            # where there is no vaccination campaign conducted
            ##########################################################################################
            
            
            
            
            hospinf<-c(hospitalizationsmonthly(), hospmonth2(), hospmonth3(), hospmonth4(), hospmonth5(), hospmonth6(),
                       hospmonth7(), hospmonth8(), hospmonth9(), hospmonth10(), hospmonth11(), hospmonth12())
            
            populationsize<-as.numeric(as.character(populationsize()))
            vaccineeffectiveness<-as.numeric(as.character(vaccineeffectiveness()))
            hnh<-as.numeric(as.character(hnh()))
            mai<-as.numeric(as.character(mai()))
            hospinf<-as.numeric(as.character(hospinf))
            
            novaccine<-hospinf%>%
                as.data.frame()%>%
                rename(hospinfwvac=1)%>%
                mutate(hospinfcumulwvac=cumsum(hospinfwvac))%>%
                mutate(nonhospinfwvac=hospinfwvac*hnh)%>%
                mutate(nonhospinfcumulwvac=cumsum(nonhospinfwvac))%>%
                mutate(overallinfwvac=hospinfwvac+nonhospinfwvac)%>%
                mutate(overallinfcumulwvac=cumsum(overallinfwvac))%>%
                mutate(medicalattedtoinfwvac=overallinfwvac*mai)%>%
                mutate(medicallyattendedinfcumulwvac=cumsum(medicalattedtoinfwvac))
            
            
            
            ##bind monthly coverage
            monthvaccov<-c(vaccinecoveragemonthly(), month2(), month3(), month4(), month5(), month6(),
                           month7(), month8(), month9(), month10(), month11(), month12())
            
            monthvaccov<-as.numeric(as.character(monthvaccov))
            
            totcoverage<-sum(monthvaccov)
            
            
            novaccine<-cbind(novaccine, monthvaccov)
            
            novaccine$covergdistrib<- c(vaccinecoveragemonthly1(), month21(), month31(), month41(), month51(), month61(), month71(), month81(), month91(), month101(), month111(), month121())
            
            novaccine$covergdistrib<-as.numeric(as.character(novaccine$covergdistrib))
            
            novaccine$monthvaccov<- novaccine$covergdistrib*coverage2() ## replace original monthvaccov column with the new monthly coverage - this is so that the variable doesn't have to be changed in the rest of the script
            
            
            ## month number
            novaccine$monthnum<-c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
            
            ## add monthly effectively vaccinated
            novaccine<-novaccine%>%
                mutate(effectivelyvacc=populationsize*monthvaccov*vaccineeffectiveness)%>%
                mutate(effectivelyvacccumul=cumsum(effectivelyvacc))
            
            
            
            
            #####################################################################
            # Estimated rates
            #####################################################################
            novaccine<-novaccine%>%
                mutate(susceptiblewvacc=populationsize-hospinfcumulwvac-nonhospinfcumulwvac-effectivelyvacccumul)
            
            #Create columns
            novaccine$irhospinf<-NA
            novaccine$irnonhospinf<-NA
            
            #Create first row
            novaccine[1,14]<-novaccine$hospinfwvac[1]/populationsize
            novaccine[1,15]<-novaccine$nonhospinfwvac[1]/populationsize
            
            
            ##Calculation uses last month's susceptible population
            for(i in 2:nrow(novaccine)){
                novaccine$irhospinf[i]<-novaccine$hospinfwvac[i]/novaccine$susceptiblewvacc[i-1]
                novaccine$irnonhospinf[i]<- novaccine$nonhospinfwvac[i]/novaccine$susceptiblewvacc[i-1]
            }
            
            #####################################################################
            # Scenario of influenza circulation in absence of vaccine
            #####################################################################
            
            ## These calculations are initialized by the 'Susceptible at start of month in absence of vaccine'
            ## column, which starts with the Target Population in the first month.
            
            remainsuscstartmonth<-c(populationsize, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
            novaccine<-cbind(novaccine, remainsuscstartmonth)
            
            #Create columns
            novaccine$hospinfwovac<-NA
            novaccine$hospinfcumulwovacc<-NA
            novaccine$nonhospinfwovac<-NA
            novaccine$nonhospinfcumulwovacc<-NA
            novaccine$remainingsusc<-NA
            
            #Create first row
            novaccine[1,17]<-novaccine$remainsuscstartmonth[1]*novaccine$irhospinf[1]
            novaccine[1,18]<-novaccine$hospinfwovac[1]
            novaccine[1,19]<-novaccine$remainsuscstartmonth[1]*novaccine$irnonhospinf[1]
            novaccine[1,20]<-novaccine$nonhospinfwovac[1]
            novaccine[1,21]<-populationsize-novaccine$hospinfcumulwovacc[1]-novaccine$nonhospinfcumulwovacc[1]
            
            
            #Create rows 2:12
            for(i in 2:nrow(novaccine)){
                novaccine$remainsuscstartmonth[i]<-novaccine$remainingsusc[i-1]
                novaccine$hospinfwovac[i]<-novaccine$remainsuscstartmonth[i]*novaccine$irhospinf[i]
                novaccine$hospinfcumulwovacc[i]<-sum(novaccine$hospinfwovac[1:i])
                novaccine$nonhospinfwovac[i]<-novaccine$remainsuscstartmonth[i]*novaccine$irnonhospinf[i]
                novaccine$nonhospinfcumulwovacc[i]<-sum(novaccine$nonhospinfwovac[1:i])
                novaccine$remainingsusc[i]<-populationsize-novaccine$hospinfcumulwovacc[i]-novaccine$nonhospinfcumulwovacc[i]
            }
            
            novaccine<-novaccine%>%
                mutate(overallinfwovac=hospinfwovac+nonhospinfwovac)%>%
                mutate(overallinfcumulwovacc=cumsum(overallinfwovac))%>%
                mutate(medicallyattendedinfwovac=overallinfwovac*mai)%>%
                mutate(medicalattedtoinfcumulwovacc=cumsum(medicallyattendedinfwovac))
            
            ######## Avoided events
            novaccine<-novaccine%>%
                mutate(AEhospinf=hospinfwovac-hospinfwvac)%>%
                mutate(AEnonhospinf=nonhospinfwovac-nonhospinfwvac)%>%
                mutate(AEmedicalattedtoinf=medicallyattendedinfwovac-medicalattedtoinfwvac)%>%
                mutate(AEoverallinf=overallinfwovac-overallinfwvac)
            
            
            ################ Prevented fraction
            ## Hospitalized infections
            ifelse (monthstartvac1()==1,
                    novaccine<-novaccine%>%
                        mutate(PFhospinf=sum(AEhospinf)/sum(hospinfwovac)),
                    novaccine<-novaccine%>%
                        mutate(PFhospinf=sum(AEhospinf[monthstartvac1():nrow(novaccine)]/sum(hospinfwovac[monthstartvac1():nrow(novaccine)])))
            )
            
            ## non-Hospitalized infections
            ifelse (monthstartvac1()==1,
                    novaccine<-novaccine%>%
                        mutate(PFnonhospinf=sum(AEnonhospinf)/sum(nonhospinfwovac)),
                    novaccine<-novaccine%>%
                        mutate(PFnonhospinf=sum(AEnonhospinf[monthstartvac1():nrow(novaccine)]/sum(nonhospinfwovac[monthstartvac1():nrow(novaccine)])))
            )
            
            
            ################ NNV -----------------------------------> why does this only go to month 8 in the Excel formula?????
            ## Hospitalized infections
            ifelse(monthstartvac1()==1,
                   novaccine<-novaccine%>%
                       mutate(NNVhospinf=1/((vaccineeffectiveness*(sum(hospinfwovac)))/populationsize)),
                   novaccine<-novaccine%>%
                       mutate(NNVhospinf=1/((vaccineeffectiveness*(sum(hospinfwovac[monthstartvac1():nrow(novaccine)])))/populationsize))
            )
            
            
            ## non-Hospitalized infections
            ifelse(monthstartvac1()==1,
                   novaccine<-novaccine%>%
                       mutate(NNVnonhospinf=1/((vaccineeffectiveness*(sum(nonhospinfwovac)))/populationsize)),
                   novaccine<-novaccine%>%
                       mutate(NNVnonhospinf=1/((vaccineeffectiveness*(sum(nonhospinfwovac[monthstartvac1():nrow(novaccine)])))/populationsize))
            )
            
            ## Medically infections
            ifelse(monthstartvac1()==1,
                   novaccine<-novaccine%>%
                       mutate(NNVmedicalattendedinf=1/((vaccineeffectiveness*(sum(medicallyattendedinfwovac)))/populationsize)),
                   novaccine<-novaccine%>%
                       mutate(NNVmedicalattendedinf=1/((vaccineeffectiveness*(sum(medicallyattendedinfwovac[monthstartvac1():nrow(novaccine)])))/populationsize))
            )
            
            ## Overall infections
            ifelse(monthstartvac1()==1,
                   novaccine<-novaccine%>%
                       mutate(NNVoverallinf=1/((vaccineeffectiveness*(sum(hospinfwovac)+sum(nonhospinfwovac)))/populationsize)),
                   novaccine<-novaccine%>%
                       mutate(NNVoverallinf=1/((vaccineeffectiveness*(sum(hospinfwovac[monthstartvac1():nrow(novaccine)])+sum(nonhospinfwovac[monthstartvac1():nrow(novaccine)])))/populationsize))
            )
            
            return(novaccine)
            
            
            
        } #close if current campaign
        
        
        
        if(scenario()=="Impact of hypothetical vaccination campaign"){
            
            ##########################################################################################
            # Scenario of influenza circulation in absence of vaccine: Absence to presence of vaccine
            # This is a scenario where the country has yet to conduct a vaccination campaign but would
            # like to see what impact a vaccine campaign may have.
            # The country enters the estimated number of hospitalization they observed and then we compare
            # this to a hypothetical vaccination campaign
            ##########################################################################################
            
            hospinf<-c(hospitalizationsmonthly(), hospmonth2(), hospmonth3(), hospmonth4(), hospmonth5(), hospmonth6(),
                       hospmonth7(), hospmonth8(), hospmonth9(), hospmonth10(), hospmonth11(), hospmonth12())
            
            populationsize<-as.numeric(as.character(populationsize()))
            hnh<-as.numeric(as.character(hnh()))
            mai<-as.numeric(as.character(mai()))
            hospinf<-as.numeric(as.character(hospinf))
            vaccineeffectiveness<-as.numeric(as.character(vaccineeffectiveness()))
            
            #####################Observed influenza in the absence of vaccine
            novaccine<-hospinf%>%
                as.data.frame()%>%
                rename(hospinfwovac=1)%>%
                mutate(hospinfcumulwovac=cumsum(hospinfwovac))%>%
                mutate(nonhospinfwovac=hospinfwovac*hnh)%>%
                mutate(nonhospinfcumulwovac=cumsum(nonhospinfwovac))%>%
                mutate(remainingsusc=populationsize-hospinfcumulwovac-nonhospinfcumulwovac)
            
            #create remainsuscstartmonth column with target population and rows 1:11 of remainingsusc
            remainsuscstartmonth<-populationsize%>%
                as.data.frame()%>%
                rename(remainsuscstartmonth=1)
            
            remainsuscstartmonth2<-as.data.frame(novaccine$remainingsusc[1:11])%>%
                as.data.frame()%>%
                rename(remainsuscstartmonth=1)
            
            remainsuscstartmonth<-rbind(remainsuscstartmonth, remainsuscstartmonth2)
            
            novaccine<-cbind(novaccine, remainsuscstartmonth)
            
            novaccine<-novaccine%>%
                mutate(overallinfwovac=hospinfwovac+nonhospinfwovac)%>%
                mutate(overallinfcumulwovac=cumsum(overallinfwovac))%>%
                mutate(medicallyattendedinfwovac=overallinfwovac*mai)%>%
                mutate(medicallyattendedinfcumulwovac=cumsum(medicallyattendedinfwovac))
            
            
            
            #####################################################################
            # Incidence rates in the absence of vaccine
            #####################################################################
            novaccine<-novaccine%>%
                mutate(irhospinf=hospinfwovac/remainsuscstartmonth)%>%
                mutate(irnonhospinf=nonhospinfwovac/remainsuscstartmonth)
            
            
            
            
            ############################ Burden inputs with hypothetical vaccination campaign
            #Create columns
            novaccine$hospinfwvac<-NA
            novaccine$hospinfcumulwvac<-NA
            novaccine$nonhospinfwvac<-NA
            novaccine$nonhospinfcumulwvac<-NA
            
            
            
            #Create first row
            novaccine[1,13]<-populationsize*novaccine$irhospinf[1]
            novaccine[1,14]<-novaccine$hospinfwvac[1]
            novaccine[1,15]<-populationsize*novaccine$irnonhospinf[1]
            novaccine[1,16]<-novaccine$nonhospinfwvac[1]
            
            ##bind monthly coverage
            monthvaccov<-c(vaccinecoveragemonthly(), month2(), month3(), month4(), month5(), month6(),
                           month7(), month8(), month9(), month10(), month11(), month12())
            
            monthvaccov<-as.numeric(as.character(monthvaccov))
            
            totcoverage<-sum(monthvaccov)
            
            
            novaccine<-cbind(novaccine, monthvaccov)
            
            novaccine$covergdistrib<- c(vaccinecoveragemonthly1(), month21(), month31(), month41(), month51(), month61(), month71(), month81(), month91(), month101(), month111(), month121())
            
            novaccine$covergdistrib<-as.numeric(as.character(novaccine$covergdistrib))
            
            novaccine$monthvaccov<- novaccine$covergdistrib*coverage2() ## replace original monthvaccov column with the new monthly coverage - this is so that the variable doesn't have to be changed in the rest of the script
            
            
            ##monthly effectively vaccinated and susceptible with hypothetical vaccination campaign
            novaccine<-novaccine%>%
                mutate(effectivelyvacc=populationsize*monthvaccov*vaccineeffectiveness)%>%
                mutate(effectivelycumulvacc=cumsum(effectivelyvacc))%>%
                mutate(suscwvacc=populationsize-hospinfcumulwvac-nonhospinfcumulwvac-effectivelyvacc)
            
            ##Then calculate the rest of hospinfwovac and nonhospinfwovac. Calculation uses last month's incidence rate
            for(i in 2:nrow(novaccine)){
                novaccine$hospinfwvac[i]<-novaccine$suscwvacc[i-1]*novaccine$irhospinf[i]
                novaccine$hospinfcumulwvac[i]<-sum(novaccine$hospinfwvac[1:i])
                novaccine$nonhospinfwvac[i]<- novaccine$suscwvacc[i-1]*novaccine$irnonhospinf[i]
                novaccine$nonhospinfcumulwvac[i]<-sum(novaccine$nonhospinfwvac[1:i])
                novaccine$suscwvacc[i]=populationsize-novaccine$hospinfcumulwvac[i]-novaccine$nonhospinfcumulwvac[i]-novaccine$effectivelycumulvacc[i]
            }
            
            
            ## month number
            novaccine$monthnum<-c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
            
            
            ## Overall infections with hypothetical vaccination campaign
            novaccine<-novaccine%>%
                mutate(overallinfwvac=hospinfwvac+nonhospinfwvac)%>%
                mutate(overallinfcumulwvacc=cumsum(overallinfwvac))
            
            ## Medically attended with hypothetical vaccination campaign
            novaccine<-novaccine%>%
                mutate(medicalattedtoinfwvac=overallinfwvac*mai)
            
            
            ######## Avoided events
            novaccine<-novaccine%>%
                mutate(AEhospinf=hospinfwovac-hospinfwvac)%>%
                mutate(AEnonhospinf=nonhospinfwovac-nonhospinfwvac)%>%
                mutate(AEmedicalattedtoinf=medicallyattendedinfwovac-medicalattedtoinfwvac)%>%
                mutate(AEoverallinf=overallinfwovac-overallinfwvac)
            
            
            
            ################ Prevented fraction
            ## Hospitalized infections
            ifelse (monthstartvac1()==1,
                    novaccine<-novaccine%>%
                        mutate(PFhospinf=sum(AEhospinf)/sum(hospinfwovac)),
                    novaccine<-novaccine%>%
                        mutate(PFhospinf=sum(AEhospinf[monthstartvac1():nrow(novaccine)]/sum(hospinfwovac[monthstartvac1():nrow(novaccine)])))
            )
            
            ## non-Hospitalized infections
            ifelse (monthstartvac1()==1,
                    novaccine<-novaccine%>%
                        mutate(PFnonhospinf=sum(AEnonhospinf)/sum(nonhospinfwovac)),
                    novaccine<-novaccine%>%
                        mutate(PFnonhospinf=sum(AEnonhospinf[monthstartvac1():nrow(novaccine)]/sum(nonhospinfwovac[monthstartvac1():nrow(novaccine)])))
            )
            
            
            
            
            ################ NNV -----------------------------------> why does this only go to month 8 in the Excel formula?????
            ## Hospitalized infections
            ifelse(monthstartvac1()==1,
                   novaccine<-novaccine%>%
                       mutate(NNVhospinf=1/((vaccineeffectiveness*(sum(hospinfwovac)))/populationsize)),
                   novaccine<-novaccine%>%
                       mutate(NNVhospinf=1/((vaccineeffectiveness*(sum(hospinfwovac[monthstartvac1():nrow(novaccine)])))/populationsize))
            )
            
            
            ## non-Hospitalized infections
            ifelse(monthstartvac1()==1,
                   novaccine<-novaccine%>%
                       mutate(NNVnonhospinf=1/((vaccineeffectiveness*(sum(nonhospinfwovac)))/populationsize)),
                   novaccine<-novaccine%>%
                       mutate(NNVnonhospinf=1/((vaccineeffectiveness*(sum(nonhospinfwovac[monthstartvac1():nrow(novaccine)])))/populationsize))
            )
            
            ## Medically attended infections
            ifelse(monthstartvac1()==1,
                   novaccine<-novaccine%>%
                       mutate(NNVmedicalattendedinf=1/((vaccineeffectiveness*(sum(medicallyattendedinfwovac)))/populationsize)),
                   novaccine<-novaccine%>%
                       mutate(NNVmedicalattendedinf=1/((vaccineeffectiveness*(sum(medicallyattendedinfwovac[monthstartvac1():nrow(novaccine)])))/populationsize))
            )
            
            ## Total infections
            ifelse(monthstartvac1()==1,
                   novaccine<-novaccine%>%
                       mutate(NNVoverallinf=1/((vaccineeffectiveness*(sum(hospinfwovac)+sum(nonhospinfwovac)))/populationsize)),
                   novaccine<-novaccine%>%
                       mutate(NNVoverallinf=1/((vaccineeffectiveness*(sum(hospinfwovac[monthstartvac1():nrow(novaccine)])+sum(nonhospinfwovac[monthstartvac1():nrow(novaccine)])))/populationsize))
            )
            
            return(novaccine)
            
            
        }#close "if hypothetical campaign"
        
    }) #close burdendataset
    
    
    
    ####################################################################################################
    ## List of burden averted results
    ####################################################################################################
    
    
    burdenavertedresultscoverage2timing1<-reactive({
        
        populationsize<-as.numeric(as.character(populationsize()))
        hnh<-as.numeric(as.character(hnh()))
        mai<-as.numeric(as.character(mai()))
        vaccineeffectiveness<-as.numeric(as.character(vaccineeffectiveness()))
        
        
        
        #################################################
        ## Results
        #################################################
        nonhospinfwovactot<-sum(burdendataset_coverage2timing1()$nonhospinfwovac)
        nonhospinfwvacctot<-sum(burdendataset_coverage2timing1()$nonhospinfwvac)
        nonhospinfaverted<-nonhospinfwovactot-nonhospinfwvacctot
        NNVnonhospinffinal<-burdendataset_coverage2timing1()$NNVnonhospinf[1]
        
        medicallyattendedinfwovactot<-sum(burdendataset_coverage2timing1()$medicallyattendedinfwovac)
        medicallyattendedinfwvactot<-sum(burdendataset_coverage2timing1()$medicalattedtoinfwvac)
        medicallyattendedinfaverted<-medicallyattendedinfwovactot-medicallyattendedinfwvactot
        NNVmedicallyattendtedfinal<-burdendataset_coverage2timing1()$NNVmedicalattendedinf[1]
        
        Totalinfwovacc<-sum(burdendataset_coverage2timing1()$overallinfwovac)
        Totalinfwvacc<-sum(burdendataset_coverage2timing1()$overallinfwvac)
        Totalinfaverted<-Totalinfwovacc-Totalinfwvacc
        NNVtotalinf<-burdendataset_coverage2timing1()$NNVoverallinf[1]
        
        hospinfwovactot<-sum(burdendataset_coverage2timing1()$hospinfwovac)
        hospinfwvacctot<-sum(burdendataset_coverage2timing1()$hospinfwvac)
        hospaverted<-hospinfwovactot-hospinfwvacctot
        NNVhospinffinal<-burdendataset_coverage2timing1()$NNVhospinf[1]
        
        
        nonhospPreventedfraction<-burdendataset_coverage2timing1()$PFnonhospinf[1]
        hospPreventedfraction<-burdendataset_coverage2timing1()$PFhospinf[1]
        
        
        indicator_results1<-c(nonhospinfwovactot, nonhospinfwvacctot, nonhospinfaverted, NNVnonhospinffinal,
                              Totalinfwovacc, Totalinfwvacc, Totalinfaverted, NNVtotalinf,
                              hospinfwovactot, hospinfwvacctot, hospaverted, NNVhospinffinal,
                              medicallyattendedinfwovactot, medicallyattendedinfwvactot, medicallyattendedinfaverted, NNVmedicallyattendtedfinal,
                              nonhospPreventedfraction)
        
        indicator_results_titles<-c("nonhospinfwovactot", "nonhospinfwvacctot", "nonhospinfaverted", "NNVnonhospinffinal",
                                    "Totalinfwovacc", "Totalinfwvacc", "Totalinfaverted", "NNVtotalinf",
                                    "hospinfwovactot", "hospinfwvacctot", "hospaverted", "NNVhospinffinal",
                                    "medicallyattendedinfwovactot", "medicallyattendedinfwvactot", "medicallyattendedinfaverted", "NNVmedicallyattendtedfinal",
                                    "nonhospPreventedfraction")
        
        indicator_results1<-cbind(indicator_results_titles, indicator_results1)
        
        
        return(indicator_results1)
        
        
    })#burdendataset_coverage2timing1
    
    
    
    
    
    #######################################################
    
    ##Deployment coverage 2, timing 2
    ######################################################
    
    
    burdendataset_coverage2timing2<- reactive({
        ################################ The output data when insufficient data has been entered   ##################################
        ############################################################################################################################
        
        
        if(scenario()=="Impact of current influenza vaccination campaign"){
            
            
            ##########################################################################################
            # Scenario of influenza circulation in presence of vaccine: Presence to absence of vaccine
            # This is a situation where the country conducts a vaccination campaign, we enter
            # actual burden inputs and vaccine coverage then compare this to a hypothetical scenario
            # where there is no vaccination campaign conducted
            ##########################################################################################
            
            
            
            
            hospinf<-c(hospitalizationsmonthly(), hospmonth2(), hospmonth3(), hospmonth4(), hospmonth5(), hospmonth6(),
                       hospmonth7(), hospmonth8(), hospmonth9(), hospmonth10(), hospmonth11(), hospmonth12())
            
            populationsize<-as.numeric(as.character(populationsize()))
            vaccineeffectiveness<-as.numeric(as.character(vaccineeffectiveness()))
            hnh<-as.numeric(as.character(hnh()))
            mai<-as.numeric(as.character(mai()))
            hospinf<-as.numeric(as.character(hospinf))
            
            novaccine<-hospinf%>%
                as.data.frame()%>%
                rename(hospinfwvac=1)%>%
                mutate(hospinfcumulwvac=cumsum(hospinfwvac))%>%
                mutate(nonhospinfwvac=hospinfwvac*hnh)%>%
                mutate(nonhospinfcumulwvac=cumsum(nonhospinfwvac))%>%
                mutate(overallinfwvac=hospinfwvac+nonhospinfwvac)%>%
                mutate(overallinfcumulwvac=cumsum(overallinfwvac))%>%
                mutate(medicalattedtoinfwvac=overallinfwvac*mai)%>%
                mutate(medicallyattendedinfcumulwvac=cumsum(medicalattedtoinfwvac))
            
            
            
            ##bind monthly coverage
            monthvaccov<-c(vaccinecoveragemonthly(), month2(), month3(), month4(), month5(), month6(),
                           month7(), month8(), month9(), month10(), month11(), month12())
            
            monthvaccov<-as.numeric(as.character(monthvaccov))
            
            totcoverage<-sum(monthvaccov)
            
            
            novaccine<-cbind(novaccine, monthvaccov)
            
            novaccine$covergdistrib<- c(vaccinecoveragemonthly2(), month22(), month32(), month42(), month52(), month62(), month72(), month82(), month92(), month102(), month112(), month122())
            
            novaccine$covergdistrib<-as.numeric(as.character(novaccine$covergdistrib))
            
            novaccine$monthvaccov<- novaccine$covergdistrib*coverage2() ## replace original monthvaccov column with the new monthly coverage - this is so that the variable doesn't have to be changed in the rest of the script
            
            
            ## month number
            novaccine$monthnum<-c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
            
            ## add monthly effectively vaccinated
            novaccine<-novaccine%>%
                mutate(effectivelyvacc=populationsize*monthvaccov*vaccineeffectiveness)%>%
                mutate(effectivelyvacccumul=cumsum(effectivelyvacc))
            
            
            
            
            #####################################################################
            # Estimated rates
            #####################################################################
            novaccine<-novaccine%>%
                mutate(susceptiblewvacc=populationsize-hospinfcumulwvac-nonhospinfcumulwvac-effectivelyvacccumul)
            
            #Create columns
            novaccine$irhospinf<-NA
            novaccine$irnonhospinf<-NA
            
            #Create first row
            novaccine[1,14]<-novaccine$hospinfwvac[1]/populationsize
            novaccine[1,15]<-novaccine$nonhospinfwvac[1]/populationsize
            
            
            ##Calculation uses last month's susceptible population
            for(i in 2:nrow(novaccine)){
                novaccine$irhospinf[i]<-novaccine$hospinfwvac[i]/novaccine$susceptiblewvacc[i-1]
                novaccine$irnonhospinf[i]<- novaccine$nonhospinfwvac[i]/novaccine$susceptiblewvacc[i-1]
            }
            
            #####################################################################
            # Scenario of influenza circulation in absence of vaccine
            #####################################################################
            
            ## These calculations are initialized by the 'Susceptible at start of month in absence of vaccine'
            ## column, which starts with the Target Population in the first month.
            
            remainsuscstartmonth<-c(populationsize, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
            novaccine<-cbind(novaccine, remainsuscstartmonth)
            
            #Create columns
            novaccine$hospinfwovac<-NA
            novaccine$hospinfcumulwovacc<-NA
            novaccine$nonhospinfwovac<-NA
            novaccine$nonhospinfcumulwovacc<-NA
            novaccine$remainingsusc<-NA
            
            #Create first row
            novaccine[1,17]<-novaccine$remainsuscstartmonth[1]*novaccine$irhospinf[1]
            novaccine[1,18]<-novaccine$hospinfwovac[1]
            novaccine[1,19]<-novaccine$remainsuscstartmonth[1]*novaccine$irnonhospinf[1]
            novaccine[1,20]<-novaccine$nonhospinfwovac[1]
            novaccine[1,21]<-populationsize-novaccine$hospinfcumulwovacc[1]-novaccine$nonhospinfcumulwovacc[1]
            
            
            #Create rows 2:12
            for(i in 2:nrow(novaccine)){
                novaccine$remainsuscstartmonth[i]<-novaccine$remainingsusc[i-1]
                novaccine$hospinfwovac[i]<-novaccine$remainsuscstartmonth[i]*novaccine$irhospinf[i]
                novaccine$hospinfcumulwovacc[i]<-sum(novaccine$hospinfwovac[1:i])
                novaccine$nonhospinfwovac[i]<-novaccine$remainsuscstartmonth[i]*novaccine$irnonhospinf[i]
                novaccine$nonhospinfcumulwovacc[i]<-sum(novaccine$nonhospinfwovac[1:i])
                novaccine$remainingsusc[i]<-populationsize-novaccine$hospinfcumulwovacc[i]-novaccine$nonhospinfcumulwovacc[i]
            }
            
            novaccine<-novaccine%>%
                mutate(overallinfwovac=hospinfwovac+nonhospinfwovac)%>%
                mutate(overallinfcumulwovacc=cumsum(overallinfwovac))%>%
                mutate(medicallyattendedinfwovac=overallinfwovac*mai)%>%
                mutate(medicalattedtoinfcumulwovacc=cumsum(medicallyattendedinfwovac))
            
            ######## Avoided events
            novaccine<-novaccine%>%
                mutate(AEhospinf=hospinfwovac-hospinfwvac)%>%
                mutate(AEnonhospinf=nonhospinfwovac-nonhospinfwvac)%>%
                mutate(AEmedicalattedtoinf=medicallyattendedinfwovac-medicalattedtoinfwvac)%>%
                mutate(AEoverallinf=overallinfwovac-overallinfwvac)
            
            
            ################ Prevented fraction
            ## Hospitalized infections
            ifelse (monthstartvac2()==1,
                    novaccine<-novaccine%>%
                        mutate(PFhospinf=sum(AEhospinf)/sum(hospinfwovac)),
                    novaccine<-novaccine%>%
                        mutate(PFhospinf=sum(AEhospinf[monthstartvac2():nrow(novaccine)]/sum(hospinfwovac[monthstartvac2():nrow(novaccine)])))
            )
            
            ## non-Hospitalized infections
            ifelse (monthstartvac2()==1,
                    novaccine<-novaccine%>%
                        mutate(PFnonhospinf=sum(AEnonhospinf)/sum(nonhospinfwovac)),
                    novaccine<-novaccine%>%
                        mutate(PFnonhospinf=sum(AEnonhospinf[monthstartvac2():nrow(novaccine)]/sum(nonhospinfwovac[monthstartvac2():nrow(novaccine)])))
            )
            
            
            ################ NNV -----------------------------------> why does this only go to month 8 in the Excel formula?????
            ## Hospitalized infections
            ifelse(monthstartvac2()==1,
                   novaccine<-novaccine%>%
                       mutate(NNVhospinf=1/((vaccineeffectiveness*(sum(hospinfwovac)))/populationsize)),
                   novaccine<-novaccine%>%
                       mutate(NNVhospinf=1/((vaccineeffectiveness*(sum(hospinfwovac[monthstartvac2():nrow(novaccine)])))/populationsize))
            )
            
            
            ## non-Hospitalized infections
            ifelse(monthstartvac2()==1,
                   novaccine<-novaccine%>%
                       mutate(NNVnonhospinf=1/((vaccineeffectiveness*(sum(nonhospinfwovac)))/populationsize)),
                   novaccine<-novaccine%>%
                       mutate(NNVnonhospinf=1/((vaccineeffectiveness*(sum(nonhospinfwovac[monthstartvac2():nrow(novaccine)])))/populationsize))
            )
            
            ## Medically infections
            ifelse(monthstartvac2()==1,
                   novaccine<-novaccine%>%
                       mutate(NNVmedicalattendedinf=1/((vaccineeffectiveness*(sum(medicallyattendedinfwovac)))/populationsize)),
                   novaccine<-novaccine%>%
                       mutate(NNVmedicalattendedinf=1/((vaccineeffectiveness*(sum(medicallyattendedinfwovac[monthstartvac2():nrow(novaccine)])))/populationsize))
            )
            
            ## Overall infections
            ifelse(monthstartvac2()==1,
                   novaccine<-novaccine%>%
                       mutate(NNVoverallinf=1/((vaccineeffectiveness*(sum(hospinfwovac)+sum(nonhospinfwovac)))/populationsize)),
                   novaccine<-novaccine%>%
                       mutate(NNVoverallinf=1/((vaccineeffectiveness*(sum(hospinfwovac[monthstartvac2():nrow(novaccine)])+sum(nonhospinfwovac[monthstartvac2():nrow(novaccine)])))/populationsize))
            )
            
            return(novaccine)
            
            
            
        } #close if current campaign
        
        
        
        if(scenario()=="Impact of hypothetical vaccination campaign"){
            
            ##########################################################################################
            # Scenario of influenza circulation in absence of vaccine: Absence to presence of vaccine
            # This is a scenario where the country has yet to conduct a vaccination campaign but would
            # like to see what impact a vaccine campaign may have.
            # The country enters the estimated number of hospitalization they observed and then we compare
            # this to a hypothetical vaccination campaign
            ##########################################################################################
            
            hospinf<-c(hospitalizationsmonthly(), hospmonth2(), hospmonth3(), hospmonth4(), hospmonth5(), hospmonth6(),
                       hospmonth7(), hospmonth8(), hospmonth9(), hospmonth10(), hospmonth11(), hospmonth12())
            
            populationsize<-as.numeric(as.character(populationsize()))
            hnh<-as.numeric(as.character(hnh()))
            mai<-as.numeric(as.character(mai()))
            hospinf<-as.numeric(as.character(hospinf))
            vaccineeffectiveness<-as.numeric(as.character(vaccineeffectiveness()))
            
            #####################Observed influenza in the absence of vaccine
            novaccine<-hospinf%>%
                as.data.frame()%>%
                rename(hospinfwovac=1)%>%
                mutate(hospinfcumulwovac=cumsum(hospinfwovac))%>%
                mutate(nonhospinfwovac=hospinfwovac*hnh)%>%
                mutate(nonhospinfcumulwovac=cumsum(nonhospinfwovac))%>%
                mutate(remainingsusc=populationsize-hospinfcumulwovac-nonhospinfcumulwovac)
            
            #create remainsuscstartmonth column with target population and rows 1:11 of remainingsusc
            remainsuscstartmonth<-populationsize%>%
                as.data.frame()%>%
                rename(remainsuscstartmonth=1)
            
            remainsuscstartmonth2<-as.data.frame(novaccine$remainingsusc[1:11])%>%
                as.data.frame()%>%
                rename(remainsuscstartmonth=1)
            
            remainsuscstartmonth<-rbind(remainsuscstartmonth, remainsuscstartmonth2)
            
            novaccine<-cbind(novaccine, remainsuscstartmonth)
            
            novaccine<-novaccine%>%
                mutate(overallinfwovac=hospinfwovac+nonhospinfwovac)%>%
                mutate(overallinfcumulwovac=cumsum(overallinfwovac))%>%
                mutate(medicallyattendedinfwovac=overallinfwovac*mai)%>%
                mutate(medicallyattendedinfcumulwovac=cumsum(medicallyattendedinfwovac))
            
            
            
            #####################################################################
            # Incidence rates in the absence of vaccine
            #####################################################################
            novaccine<-novaccine%>%
                mutate(irhospinf=hospinfwovac/remainsuscstartmonth)%>%
                mutate(irnonhospinf=nonhospinfwovac/remainsuscstartmonth)
            
            
            
            
            ############################ Burden inputs with hypothetical vaccination campaign
            #Create columns
            novaccine$hospinfwvac<-NA
            novaccine$hospinfcumulwvac<-NA
            novaccine$nonhospinfwvac<-NA
            novaccine$nonhospinfcumulwvac<-NA
            
            
            
            #Create first row
            novaccine[1,13]<-populationsize*novaccine$irhospinf[1]
            novaccine[1,14]<-novaccine$hospinfwvac[1]
            novaccine[1,15]<-populationsize*novaccine$irnonhospinf[1]
            novaccine[1,16]<-novaccine$nonhospinfwvac[1]
            
            ##bind monthly coverage
            monthvaccov<-c(vaccinecoveragemonthly(), month2(), month3(), month4(), month5(), month6(),
                           month7(), month8(), month9(), month10(), month11(), month12())
            
            monthvaccov<-as.numeric(as.character(monthvaccov))
            
            totcoverage<-sum(monthvaccov)
            
            
            novaccine<-cbind(novaccine, monthvaccov)
            
            novaccine$covergdistrib<- c(vaccinecoveragemonthly2(), month22(), month32(), month42(), month52(), month62(), month72(), month82(), month92(), month102(), month112(), month122())
            
            novaccine$covergdistrib<-as.numeric(as.character(novaccine$covergdistrib))
            
            novaccine$monthvaccov<- novaccine$covergdistrib*coverage2() ## replace original monthvaccov column with the new monthly coverage - this is so that the variable doesn't have to be changed in the rest of the script
            
            
            ##monthly effectively vaccinated and susceptible with hypothetical vaccination campaign
            novaccine<-novaccine%>%
                mutate(effectivelyvacc=populationsize*monthvaccov*vaccineeffectiveness)%>%
                mutate(effectivelycumulvacc=cumsum(effectivelyvacc))%>%
                mutate(suscwvacc=populationsize-hospinfcumulwvac-nonhospinfcumulwvac-effectivelyvacc)
            
            ##Then calculate the rest of hospinfwovac and nonhospinfwovac. Calculation uses last month's incidence rate
            for(i in 2:nrow(novaccine)){
                novaccine$hospinfwvac[i]<-novaccine$suscwvacc[i-1]*novaccine$irhospinf[i]
                novaccine$hospinfcumulwvac[i]<-sum(novaccine$hospinfwvac[1:i])
                novaccine$nonhospinfwvac[i]<- novaccine$suscwvacc[i-1]*novaccine$irnonhospinf[i]
                novaccine$nonhospinfcumulwvac[i]<-sum(novaccine$nonhospinfwvac[1:i])
                novaccine$suscwvacc[i]=populationsize-novaccine$hospinfcumulwvac[i]-novaccine$nonhospinfcumulwvac[i]-novaccine$effectivelycumulvacc[i]
            }
            
            
            ## month number
            novaccine$monthnum<-c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
            
            
            ## Overall infections with hypothetical vaccination campaign
            novaccine<-novaccine%>%
                mutate(overallinfwvac=hospinfwvac+nonhospinfwvac)%>%
                mutate(overallinfcumulwvacc=cumsum(overallinfwvac))
            
            ## Medically attended with hypothetical vaccination campaign
            novaccine<-novaccine%>%
                mutate(medicalattedtoinfwvac=overallinfwvac*mai)
            
            
            ######## Avoided events
            novaccine<-novaccine%>%
                mutate(AEhospinf=hospinfwovac-hospinfwvac)%>%
                mutate(AEnonhospinf=nonhospinfwovac-nonhospinfwvac)%>%
                mutate(AEmedicalattedtoinf=medicallyattendedinfwovac-medicalattedtoinfwvac)%>%
                mutate(AEoverallinf=overallinfwovac-overallinfwvac)
            
            
            
            ################ Prevented fraction
            ## Hospitalized infections
            ifelse (monthstartvac2()==1,
                    novaccine<-novaccine%>%
                        mutate(PFhospinf=sum(AEhospinf)/sum(hospinfwovac)),
                    novaccine<-novaccine%>%
                        mutate(PFhospinf=sum(AEhospinf[monthstartvac2():nrow(novaccine)]/sum(hospinfwovac[monthstartvac2():nrow(novaccine)])))
            )
            
            ## non-Hospitalized infections
            ifelse (monthstartvac2()==1,
                    novaccine<-novaccine%>%
                        mutate(PFnonhospinf=sum(AEnonhospinf)/sum(nonhospinfwovac)),
                    novaccine<-novaccine%>%
                        mutate(PFnonhospinf=sum(AEnonhospinf[monthstartvac2():nrow(novaccine)]/sum(nonhospinfwovac[monthstartvac2():nrow(novaccine)])))
            )
            
            
            
            
            ################ NNV -----------------------------------> why does this only go to month 8 in the Excel formula?????
            ## Hospitalized infections
            ifelse(monthstartvac2()==1,
                   novaccine<-novaccine%>%
                       mutate(NNVhospinf=1/((vaccineeffectiveness*(sum(hospinfwovac)))/populationsize)),
                   novaccine<-novaccine%>%
                       mutate(NNVhospinf=1/((vaccineeffectiveness*(sum(hospinfwovac[monthstartvac2():nrow(novaccine)])))/populationsize))
            )
            
            
            ## non-Hospitalized infections
            ifelse(monthstartvac2()==1,
                   novaccine<-novaccine%>%
                       mutate(NNVnonhospinf=1/((vaccineeffectiveness*(sum(nonhospinfwovac)))/populationsize)),
                   novaccine<-novaccine%>%
                       mutate(NNVnonhospinf=1/((vaccineeffectiveness*(sum(nonhospinfwovac[monthstartvac2():nrow(novaccine)])))/populationsize))
            )
            
            ## Medically attended infections
            ifelse(monthstartvac2()==1,
                   novaccine<-novaccine%>%
                       mutate(NNVmedicalattendedinf=1/((vaccineeffectiveness*(sum(medicallyattendedinfwovac)))/populationsize)),
                   novaccine<-novaccine%>%
                       mutate(NNVmedicalattendedinf=1/((vaccineeffectiveness*(sum(medicallyattendedinfwovac[monthstartvac2():nrow(novaccine)])))/populationsize))
            )
            
            ## Total infections
            ifelse(monthstartvac2()==1,
                   novaccine<-novaccine%>%
                       mutate(NNVoverallinf=1/((vaccineeffectiveness*(sum(hospinfwovac)+sum(nonhospinfwovac)))/populationsize)),
                   novaccine<-novaccine%>%
                       mutate(NNVoverallinf=1/((vaccineeffectiveness*(sum(hospinfwovac[monthstartvac2():nrow(novaccine)])+sum(nonhospinfwovac[monthstartvac2():nrow(novaccine)])))/populationsize))
            )
            
            return(novaccine)
            
            
        }#close "if hypothetical campaign"
        
    }) #close burdendataset
    
    
    
    ####################################################################################################
    ## List of burden averted results
    ####################################################################################################
    
    
    burdenavertedresultscoverage2timing2<-reactive({
        
        populationsize<-as.numeric(as.character(populationsize()))
        hnh<-as.numeric(as.character(hnh()))
        mai<-as.numeric(as.character(mai()))
        vaccineeffectiveness<-as.numeric(as.character(vaccineeffectiveness()))
        
        
        
        #################################################
        ## Results
        #################################################
        nonhospinfwovactot<-sum(burdendataset_coverage2timing2()$nonhospinfwovac)
        nonhospinfwvacctot<-sum(burdendataset_coverage2timing2()$nonhospinfwvac)
        nonhospinfaverted<-nonhospinfwovactot-nonhospinfwvacctot
        NNVnonhospinffinal<-burdendataset_coverage2timing2()$NNVnonhospinf[1]
        
        medicallyattendedinfwovactot<-sum(burdendataset_coverage2timing2()$medicallyattendedinfwovac)
        medicallyattendedinfwvactot<-sum(burdendataset_coverage2timing2()$medicalattedtoinfwvac)
        medicallyattendedinfaverted<-medicallyattendedinfwovactot-medicallyattendedinfwvactot
        NNVmedicallyattendtedfinal<-burdendataset_coverage2timing2()$NNVmedicalattendedinf[1]
        
        Totalinfwovacc<-sum(burdendataset_coverage2timing2()$overallinfwovac)
        Totalinfwvacc<-sum(burdendataset_coverage2timing2()$overallinfwvac)
        Totalinfaverted<-Totalinfwovacc-Totalinfwvacc
        NNVtotalinf<-burdendataset_coverage2timing2()$NNVoverallinf[1]
        
        hospinfwovactot<-sum(burdendataset_coverage2timing2()$hospinfwovac)
        hospinfwvacctot<-sum(burdendataset_coverage2timing2()$hospinfwvac)
        hospaverted<-hospinfwovactot-hospinfwvacctot
        NNVhospinffinal<-burdendataset_coverage2timing2()$NNVhospinf[1]
        
        
        nonhospPreventedfraction<-burdendataset_coverage2timing2()$PFnonhospinf[1]
        hospPreventedfraction<-burdendataset_coverage2timing2()$PFhospinf[1]
        
        
        indicator_results1<-c(nonhospinfwovactot, nonhospinfwvacctot, nonhospinfaverted, NNVnonhospinffinal,
                              Totalinfwovacc, Totalinfwvacc, Totalinfaverted, NNVtotalinf,
                              hospinfwovactot, hospinfwvacctot, hospaverted, NNVhospinffinal,
                              medicallyattendedinfwovactot, medicallyattendedinfwvactot, medicallyattendedinfaverted, NNVmedicallyattendtedfinal,
                              nonhospPreventedfraction)
        
        indicator_results_titles<-c("nonhospinfwovactot", "nonhospinfwvacctot", "nonhospinfaverted", "NNVnonhospinffinal",
                                    "Totalinfwovacc", "Totalinfwvacc", "Totalinfaverted", "NNVtotalinf",
                                    "hospinfwovactot", "hospinfwvacctot", "hospaverted", "NNVhospinffinal",
                                    "medicallyattendedinfwovactot", "medicallyattendedinfwvactot", "medicallyattendedinfaverted", "NNVmedicallyattendtedfinal",
                                    "nonhospPreventedfraction")
        
        indicator_results1<-cbind(indicator_results_titles, indicator_results1)
        
        
        return(indicator_results1)
        
        
    })#burdendataset_coverage2timing2
    
   
    
    #######################################################
    
    ##Deployment coverage 2, timing 3
    ######################################################
    
    
    burdendataset_coverage2timing3<- reactive({
        ################################ The output data when insufficient data has been entered   ##################################
        ############################################################################################################################
        
        
        if(scenario()=="Impact of current influenza vaccination campaign"){
            
            
            ##########################################################################################
            # Scenario of influenza circulation in presence of vaccine: Presence to absence of vaccine
            # This is a situation where the country conducts a vaccination campaign, we enter
            # actual burden inputs and vaccine coverage then compare this to a hypothetical scenario
            # where there is no vaccination campaign conducted
            ##########################################################################################
            
            
            
            
            hospinf<-c(hospitalizationsmonthly(), hospmonth2(), hospmonth3(), hospmonth4(), hospmonth5(), hospmonth6(),
                       hospmonth7(), hospmonth8(), hospmonth9(), hospmonth10(), hospmonth11(), hospmonth12())
            
            populationsize<-as.numeric(as.character(populationsize()))
            vaccineeffectiveness<-as.numeric(as.character(vaccineeffectiveness()))
            hnh<-as.numeric(as.character(hnh()))
            mai<-as.numeric(as.character(mai()))
            hospinf<-as.numeric(as.character(hospinf))
            
            novaccine<-hospinf%>%
                as.data.frame()%>%
                rename(hospinfwvac=1)%>%
                mutate(hospinfcumulwvac=cumsum(hospinfwvac))%>%
                mutate(nonhospinfwvac=hospinfwvac*hnh)%>%
                mutate(nonhospinfcumulwvac=cumsum(nonhospinfwvac))%>%
                mutate(overallinfwvac=hospinfwvac+nonhospinfwvac)%>%
                mutate(overallinfcumulwvac=cumsum(overallinfwvac))%>%
                mutate(medicalattedtoinfwvac=overallinfwvac*mai)%>%
                mutate(medicallyattendedinfcumulwvac=cumsum(medicalattedtoinfwvac))
            
            
            
            ##bind monthly coverage
            monthvaccov<-c(vaccinecoveragemonthly(), month2(), month3(), month4(), month5(), month6(),
                           month7(), month8(), month9(), month10(), month11(), month12())
            
            monthvaccov<-as.numeric(as.character(monthvaccov))
            
            totcoverage<-sum(monthvaccov)
            
            
            novaccine<-cbind(novaccine, monthvaccov)
            
            novaccine$covergdistrib<- c(vaccinecoveragemonthly3(), month23(), month33(), month43(), month53(), month63(), month73(), month83(), month93(), month103(), month113(), month123())
            
            novaccine$covergdistrib<-as.numeric(as.character(novaccine$covergdistrib))
            
            novaccine$monthvaccov<- novaccine$covergdistrib*coverage2() ## replace original monthvaccov column with the new monthly coverage - this is so that the variable doesn't have to be changed in the rest of the script
            
            
            ## month number
            novaccine$monthnum<-c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
            
            ## add monthly effectively vaccinated
            novaccine<-novaccine%>%
                mutate(effectivelyvacc=populationsize*monthvaccov*vaccineeffectiveness)%>%
                mutate(effectivelyvacccumul=cumsum(effectivelyvacc))
            
            
            
            
            #####################################################################
            # Estimated rates
            #####################################################################
            novaccine<-novaccine%>%
                mutate(susceptiblewvacc=populationsize-hospinfcumulwvac-nonhospinfcumulwvac-effectivelyvacccumul)
            
            #Create columns
            novaccine$irhospinf<-NA
            novaccine$irnonhospinf<-NA
            
            #Create first row
            novaccine[1,14]<-novaccine$hospinfwvac[1]/populationsize
            novaccine[1,15]<-novaccine$nonhospinfwvac[1]/populationsize
            
            
            ##Calculation uses last month's susceptible population
            for(i in 2:nrow(novaccine)){
                novaccine$irhospinf[i]<-novaccine$hospinfwvac[i]/novaccine$susceptiblewvacc[i-1]
                novaccine$irnonhospinf[i]<- novaccine$nonhospinfwvac[i]/novaccine$susceptiblewvacc[i-1]
            }
            
            #####################################################################
            # Scenario of influenza circulation in absence of vaccine
            #####################################################################
            
            ## These calculations are initialized by the 'Susceptible at start of month in absence of vaccine'
            ## column, which starts with the Target Population in the first month.
            
            remainsuscstartmonth<-c(populationsize, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
            novaccine<-cbind(novaccine, remainsuscstartmonth)
            
            #Create columns
            novaccine$hospinfwovac<-NA
            novaccine$hospinfcumulwovacc<-NA
            novaccine$nonhospinfwovac<-NA
            novaccine$nonhospinfcumulwovacc<-NA
            novaccine$remainingsusc<-NA
            
            #Create first row
            novaccine[1,17]<-novaccine$remainsuscstartmonth[1]*novaccine$irhospinf[1]
            novaccine[1,18]<-novaccine$hospinfwovac[1]
            novaccine[1,19]<-novaccine$remainsuscstartmonth[1]*novaccine$irnonhospinf[1]
            novaccine[1,20]<-novaccine$nonhospinfwovac[1]
            novaccine[1,21]<-populationsize-novaccine$hospinfcumulwovacc[1]-novaccine$nonhospinfcumulwovacc[1]
            
            
            #Create rows 2:12
            for(i in 2:nrow(novaccine)){
                novaccine$remainsuscstartmonth[i]<-novaccine$remainingsusc[i-1]
                novaccine$hospinfwovac[i]<-novaccine$remainsuscstartmonth[i]*novaccine$irhospinf[i]
                novaccine$hospinfcumulwovacc[i]<-sum(novaccine$hospinfwovac[1:i])
                novaccine$nonhospinfwovac[i]<-novaccine$remainsuscstartmonth[i]*novaccine$irnonhospinf[i]
                novaccine$nonhospinfcumulwovacc[i]<-sum(novaccine$nonhospinfwovac[1:i])
                novaccine$remainingsusc[i]<-populationsize-novaccine$hospinfcumulwovacc[i]-novaccine$nonhospinfcumulwovacc[i]
            }
            
            novaccine<-novaccine%>%
                mutate(overallinfwovac=hospinfwovac+nonhospinfwovac)%>%
                mutate(overallinfcumulwovacc=cumsum(overallinfwovac))%>%
                mutate(medicallyattendedinfwovac=overallinfwovac*mai)%>%
                mutate(medicalattedtoinfcumulwovacc=cumsum(medicallyattendedinfwovac))
            
            ######## Avoided events
            novaccine<-novaccine%>%
                mutate(AEhospinf=hospinfwovac-hospinfwvac)%>%
                mutate(AEnonhospinf=nonhospinfwovac-nonhospinfwvac)%>%
                mutate(AEmedicalattedtoinf=medicallyattendedinfwovac-medicalattedtoinfwvac)%>%
                mutate(AEoverallinf=overallinfwovac-overallinfwvac)
            
            
            ################ Prevented fraction
            ## Hospitalized infections
            ifelse (monthstartvac3()==1,
                    novaccine<-novaccine%>%
                        mutate(PFhospinf=sum(AEhospinf)/sum(hospinfwovac)),
                    novaccine<-novaccine%>%
                        mutate(PFhospinf=sum(AEhospinf[monthstartvac3():nrow(novaccine)]/sum(hospinfwovac[monthstartvac3():nrow(novaccine)])))
            )
            
            ## non-Hospitalized infections
            ifelse (monthstartvac3()==1,
                    novaccine<-novaccine%>%
                        mutate(PFnonhospinf=sum(AEnonhospinf)/sum(nonhospinfwovac)),
                    novaccine<-novaccine%>%
                        mutate(PFnonhospinf=sum(AEnonhospinf[monthstartvac3():nrow(novaccine)]/sum(nonhospinfwovac[monthstartvac3():nrow(novaccine)])))
            )
            
            
            ################ NNV -----------------------------------> why does this only go to month 8 in the Excel formula?????
            ## Hospitalized infections
            ifelse(monthstartvac3()==1,
                   novaccine<-novaccine%>%
                       mutate(NNVhospinf=1/((vaccineeffectiveness*(sum(hospinfwovac)))/populationsize)),
                   novaccine<-novaccine%>%
                       mutate(NNVhospinf=1/((vaccineeffectiveness*(sum(hospinfwovac[monthstartvac3():nrow(novaccine)])))/populationsize))
            )
            
            
            ## non-Hospitalized infections
            ifelse(monthstartvac3()==1,
                   novaccine<-novaccine%>%
                       mutate(NNVnonhospinf=1/((vaccineeffectiveness*(sum(nonhospinfwovac)))/populationsize)),
                   novaccine<-novaccine%>%
                       mutate(NNVnonhospinf=1/((vaccineeffectiveness*(sum(nonhospinfwovac[monthstartvac3():nrow(novaccine)])))/populationsize))
            )
            
            ## Medically infections
            ifelse(monthstartvac3()==1,
                   novaccine<-novaccine%>%
                       mutate(NNVmedicalattendedinf=1/((vaccineeffectiveness*(sum(medicallyattendedinfwovac)))/populationsize)),
                   novaccine<-novaccine%>%
                       mutate(NNVmedicalattendedinf=1/((vaccineeffectiveness*(sum(medicallyattendedinfwovac[monthstartvac3():nrow(novaccine)])))/populationsize))
            )
            
            ## Overall infections
            ifelse(monthstartvac3()==1,
                   novaccine<-novaccine%>%
                       mutate(NNVoverallinf=1/((vaccineeffectiveness*(sum(hospinfwovac)+sum(nonhospinfwovac)))/populationsize)),
                   novaccine<-novaccine%>%
                       mutate(NNVoverallinf=1/((vaccineeffectiveness*(sum(hospinfwovac[monthstartvac3():nrow(novaccine)])+sum(nonhospinfwovac[monthstartvac3():nrow(novaccine)])))/populationsize))
            )
            
            return(novaccine)
            
            
            
        } #close if current campaign
        
        
        
        if(scenario()=="Impact of hypothetical vaccination campaign"){
            
            ##########################################################################################
            # Scenario of influenza circulation in absence of vaccine: Absence to presence of vaccine
            # This is a scenario where the country has yet to conduct a vaccination campaign but would
            # like to see what impact a vaccine campaign may have.
            # The country enters the estimated number of hospitalization they observed and then we compare
            # this to a hypothetical vaccination campaign
            ##########################################################################################
            
            hospinf<-c(hospitalizationsmonthly(), hospmonth2(), hospmonth3(), hospmonth4(), hospmonth5(), hospmonth6(),
                       hospmonth7(), hospmonth8(), hospmonth9(), hospmonth10(), hospmonth11(), hospmonth12())
            
            populationsize<-as.numeric(as.character(populationsize()))
            hnh<-as.numeric(as.character(hnh()))
            mai<-as.numeric(as.character(mai()))
            hospinf<-as.numeric(as.character(hospinf))
            vaccineeffectiveness<-as.numeric(as.character(vaccineeffectiveness()))
            
            #####################Observed influenza in the absence of vaccine
            novaccine<-hospinf%>%
                as.data.frame()%>%
                rename(hospinfwovac=1)%>%
                mutate(hospinfcumulwovac=cumsum(hospinfwovac))%>%
                mutate(nonhospinfwovac=hospinfwovac*hnh)%>%
                mutate(nonhospinfcumulwovac=cumsum(nonhospinfwovac))%>%
                mutate(remainingsusc=populationsize-hospinfcumulwovac-nonhospinfcumulwovac)
            
            #create remainsuscstartmonth column with target population and rows 1:11 of remainingsusc
            remainsuscstartmonth<-populationsize%>%
                as.data.frame()%>%
                rename(remainsuscstartmonth=1)
            
            remainsuscstartmonth2<-as.data.frame(novaccine$remainingsusc[1:11])%>%
                as.data.frame()%>%
                rename(remainsuscstartmonth=1)
            
            remainsuscstartmonth<-rbind(remainsuscstartmonth, remainsuscstartmonth2)
            
            novaccine<-cbind(novaccine, remainsuscstartmonth)
            
            novaccine<-novaccine%>%
                mutate(overallinfwovac=hospinfwovac+nonhospinfwovac)%>%
                mutate(overallinfcumulwovac=cumsum(overallinfwovac))%>%
                mutate(medicallyattendedinfwovac=overallinfwovac*mai)%>%
                mutate(medicallyattendedinfcumulwovac=cumsum(medicallyattendedinfwovac))
            
            
            
            #####################################################################
            # Incidence rates in the absence of vaccine
            #####################################################################
            novaccine<-novaccine%>%
                mutate(irhospinf=hospinfwovac/remainsuscstartmonth)%>%
                mutate(irnonhospinf=nonhospinfwovac/remainsuscstartmonth)
            
            
            
            
            ############################ Burden inputs with hypothetical vaccination campaign
            #Create columns
            novaccine$hospinfwvac<-NA
            novaccine$hospinfcumulwvac<-NA
            novaccine$nonhospinfwvac<-NA
            novaccine$nonhospinfcumulwvac<-NA
            
            
            
            #Create first row
            novaccine[1,13]<-populationsize*novaccine$irhospinf[1]
            novaccine[1,14]<-novaccine$hospinfwvac[1]
            novaccine[1,15]<-populationsize*novaccine$irnonhospinf[1]
            novaccine[1,16]<-novaccine$nonhospinfwvac[1]
            
            ##bind monthly coverage
            monthvaccov<-c(vaccinecoveragemonthly(), month2(), month3(), month4(), month5(), month6(),
                           month7(), month8(), month9(), month10(), month11(), month12())
            
            monthvaccov<-as.numeric(as.character(monthvaccov))
            
            totcoverage<-sum(monthvaccov)
            
            
            novaccine<-cbind(novaccine, monthvaccov)
            
            novaccine$covergdistrib<- c(vaccinecoveragemonthly3(), month23(), month33(), month43(), month53(), month63(), month73(), month83(), month93(), month103(), month113(), month123())
            
            novaccine$covergdistrib<-as.numeric(as.character(novaccine$covergdistrib))
            
            novaccine$monthvaccov<- novaccine$covergdistrib*coverage2() ## replace original monthvaccov column with the new monthly coverage - this is so that the variable doesn't have to be changed in the rest of the script
            
            
            ##monthly effectively vaccinated and susceptible with hypothetical vaccination campaign
            novaccine<-novaccine%>%
                mutate(effectivelyvacc=populationsize*monthvaccov*vaccineeffectiveness)%>%
                mutate(effectivelycumulvacc=cumsum(effectivelyvacc))%>%
                mutate(suscwvacc=populationsize-hospinfcumulwvac-nonhospinfcumulwvac-effectivelyvacc)
            
            ##Then calculate the rest of hospinfwovac and nonhospinfwovac. Calculation uses last month's incidence rate
            for(i in 2:nrow(novaccine)){
                novaccine$hospinfwvac[i]<-novaccine$suscwvacc[i-1]*novaccine$irhospinf[i]
                novaccine$hospinfcumulwvac[i]<-sum(novaccine$hospinfwvac[1:i])
                novaccine$nonhospinfwvac[i]<- novaccine$suscwvacc[i-1]*novaccine$irnonhospinf[i]
                novaccine$nonhospinfcumulwvac[i]<-sum(novaccine$nonhospinfwvac[1:i])
                novaccine$suscwvacc[i]=populationsize-novaccine$hospinfcumulwvac[i]-novaccine$nonhospinfcumulwvac[i]-novaccine$effectivelycumulvacc[i]
            }
            
            
            ## month number
            novaccine$monthnum<-c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
            
            
            ## Overall infections with hypothetical vaccination campaign
            novaccine<-novaccine%>%
                mutate(overallinfwvac=hospinfwvac+nonhospinfwvac)%>%
                mutate(overallinfcumulwvacc=cumsum(overallinfwvac))
            
            ## Medically attended with hypothetical vaccination campaign
            novaccine<-novaccine%>%
                mutate(medicalattedtoinfwvac=overallinfwvac*mai)
            
            
            ######## Avoided events
            novaccine<-novaccine%>%
                mutate(AEhospinf=hospinfwovac-hospinfwvac)%>%
                mutate(AEnonhospinf=nonhospinfwovac-nonhospinfwvac)%>%
                mutate(AEmedicalattedtoinf=medicallyattendedinfwovac-medicalattedtoinfwvac)%>%
                mutate(AEoverallinf=overallinfwovac-overallinfwvac)
            
            
            
            ################ Prevented fraction
            ## Hospitalized infections
            ifelse (monthstartvac3()==1,
                    novaccine<-novaccine%>%
                        mutate(PFhospinf=sum(AEhospinf)/sum(hospinfwovac)),
                    novaccine<-novaccine%>%
                        mutate(PFhospinf=sum(AEhospinf[monthstartvac3():nrow(novaccine)]/sum(hospinfwovac[monthstartvac3():nrow(novaccine)])))
            )
            
            ## non-Hospitalized infections
            ifelse (monthstartvac3()==1,
                    novaccine<-novaccine%>%
                        mutate(PFnonhospinf=sum(AEnonhospinf)/sum(nonhospinfwovac)),
                    novaccine<-novaccine%>%
                        mutate(PFnonhospinf=sum(AEnonhospinf[monthstartvac3():nrow(novaccine)]/sum(nonhospinfwovac[monthstartvac3():nrow(novaccine)])))
            )
            
            
            
            
            ################ NNV -----------------------------------> why does this only go to month 8 in the Excel formula?????
            ## Hospitalized infections
            ifelse(monthstartvac3()==1,
                   novaccine<-novaccine%>%
                       mutate(NNVhospinf=1/((vaccineeffectiveness*(sum(hospinfwovac)))/populationsize)),
                   novaccine<-novaccine%>%
                       mutate(NNVhospinf=1/((vaccineeffectiveness*(sum(hospinfwovac[monthstartvac3():nrow(novaccine)])))/populationsize))
            )
            
            
            ## non-Hospitalized infections
            ifelse(monthstartvac3()==1,
                   novaccine<-novaccine%>%
                       mutate(NNVnonhospinf=1/((vaccineeffectiveness*(sum(nonhospinfwovac)))/populationsize)),
                   novaccine<-novaccine%>%
                       mutate(NNVnonhospinf=1/((vaccineeffectiveness*(sum(nonhospinfwovac[monthstartvac3():nrow(novaccine)])))/populationsize))
            )
            
            ## Medically attended infections
            ifelse(monthstartvac3()==1,
                   novaccine<-novaccine%>%
                       mutate(NNVmedicalattendedinf=1/((vaccineeffectiveness*(sum(medicallyattendedinfwovac)))/populationsize)),
                   novaccine<-novaccine%>%
                       mutate(NNVmedicalattendedinf=1/((vaccineeffectiveness*(sum(medicallyattendedinfwovac[monthstartvac3():nrow(novaccine)])))/populationsize))
            )
            
            ## Total infections
            ifelse(monthstartvac3()==1,
                   novaccine<-novaccine%>%
                       mutate(NNVoverallinf=1/((vaccineeffectiveness*(sum(hospinfwovac)+sum(nonhospinfwovac)))/populationsize)),
                   novaccine<-novaccine%>%
                       mutate(NNVoverallinf=1/((vaccineeffectiveness*(sum(hospinfwovac[monthstartvac3():nrow(novaccine)])+sum(nonhospinfwovac[monthstartvac3():nrow(novaccine)])))/populationsize))
            )
            
            return(novaccine)
            
            
        }#close "if hypothetical campaign"
        
    }) #close burdendataset
    
    
    
    ####################################################################################################
    ## List of burden averted results
    ####################################################################################################
    
    
    burdenavertedresultscoverage2timing3<-reactive({
        
        populationsize<-as.numeric(as.character(populationsize()))
        hnh<-as.numeric(as.character(hnh()))
        mai<-as.numeric(as.character(mai()))
        vaccineeffectiveness<-as.numeric(as.character(vaccineeffectiveness()))
        
        
        
        #################################################
        ## Results
        #################################################
        nonhospinfwovactot<-sum(burdendataset_coverage2timing3()$nonhospinfwovac)
        nonhospinfwvacctot<-sum(burdendataset_coverage2timing3()$nonhospinfwvac)
        nonhospinfaverted<-nonhospinfwovactot-nonhospinfwvacctot
        NNVnonhospinffinal<-burdendataset_coverage2timing3()$NNVnonhospinf[1]
        
        medicallyattendedinfwovactot<-sum(burdendataset_coverage2timing3()$medicallyattendedinfwovac)
        medicallyattendedinfwvactot<-sum(burdendataset_coverage2timing3()$medicalattedtoinfwvac)
        medicallyattendedinfaverted<-medicallyattendedinfwovactot-medicallyattendedinfwvactot
        NNVmedicallyattendtedfinal<-burdendataset_coverage2timing3()$NNVmedicalattendedinf[1]
        
        Totalinfwovacc<-sum(burdendataset_coverage2timing3()$overallinfwovac)
        Totalinfwvacc<-sum(burdendataset_coverage2timing3()$overallinfwvac)
        Totalinfaverted<-Totalinfwovacc-Totalinfwvacc
        NNVtotalinf<-burdendataset_coverage2timing3()$NNVoverallinf[1]
        
        hospinfwovactot<-sum(burdendataset_coverage2timing3()$hospinfwovac)
        hospinfwvacctot<-sum(burdendataset_coverage2timing3()$hospinfwvac)
        hospaverted<-hospinfwovactot-hospinfwvacctot
        NNVhospinffinal<-burdendataset_coverage2timing3()$NNVhospinf[1]
        
        
        nonhospPreventedfraction<-burdendataset_coverage2timing3()$PFnonhospinf[1]
        hospPreventedfraction<-burdendataset_coverage2timing3()$PFhospinf[1]
        
        
        indicator_results1<-c(nonhospinfwovactot, nonhospinfwvacctot, nonhospinfaverted, NNVnonhospinffinal,
                              Totalinfwovacc, Totalinfwvacc, Totalinfaverted, NNVtotalinf,
                              hospinfwovactot, hospinfwvacctot, hospaverted, NNVhospinffinal,
                              medicallyattendedinfwovactot, medicallyattendedinfwvactot, medicallyattendedinfaverted, NNVmedicallyattendtedfinal,
                              nonhospPreventedfraction)
        
        indicator_results_titles<-c("nonhospinfwovactot", "nonhospinfwvacctot", "nonhospinfaverted", "NNVnonhospinffinal",
                                    "Totalinfwovacc", "Totalinfwvacc", "Totalinfaverted", "NNVtotalinf",
                                    "hospinfwovactot", "hospinfwvacctot", "hospaverted", "NNVhospinffinal",
                                    "medicallyattendedinfwovactot", "medicallyattendedinfwvactot", "medicallyattendedinfaverted", "NNVmedicallyattendtedfinal",
                                    "nonhospPreventedfraction")
        
        indicator_results1<-cbind(indicator_results_titles, indicator_results1)
        
        
        return(indicator_results1)
        
        
    })#burdendataset_coverage2timing3
    
    
    
    #######################################################
    
    ##Deployment coverage 2, timing 4
    ######################################################
    
    
    burdendataset_coverage2timing4<- reactive({
        ################################ The output data when insufficient data has been entered   ##################################
        ############################################################################################################################
        
        
        if(scenario()=="Impact of current influenza vaccination campaign"){
            
            
            ##########################################################################################
            # Scenario of influenza circulation in presence of vaccine: Presence to absence of vaccine
            # This is a situation where the country conducts a vaccination campaign, we enter
            # actual burden inputs and vaccine coverage then compare this to a hypothetical scenario
            # where there is no vaccination campaign conducted
            ##########################################################################################
            
            
            
            
            hospinf<-c(hospitalizationsmonthly(), hospmonth2(), hospmonth3(), hospmonth4(), hospmonth5(), hospmonth6(),
                       hospmonth7(), hospmonth8(), hospmonth9(), hospmonth10(), hospmonth11(), hospmonth12())
            
            populationsize<-as.numeric(as.character(populationsize()))
            vaccineeffectiveness<-as.numeric(as.character(vaccineeffectiveness()))
            hnh<-as.numeric(as.character(hnh()))
            mai<-as.numeric(as.character(mai()))
            hospinf<-as.numeric(as.character(hospinf))
            
            novaccine<-hospinf%>%
                as.data.frame()%>%
                rename(hospinfwvac=1)%>%
                mutate(hospinfcumulwvac=cumsum(hospinfwvac))%>%
                mutate(nonhospinfwvac=hospinfwvac*hnh)%>%
                mutate(nonhospinfcumulwvac=cumsum(nonhospinfwvac))%>%
                mutate(overallinfwvac=hospinfwvac+nonhospinfwvac)%>%
                mutate(overallinfcumulwvac=cumsum(overallinfwvac))%>%
                mutate(medicalattedtoinfwvac=overallinfwvac*mai)%>%
                mutate(medicallyattendedinfcumulwvac=cumsum(medicalattedtoinfwvac))
            
            
            
            ##bind monthly coverage
            monthvaccov<-c(vaccinecoveragemonthly(), month2(), month3(), month4(), month5(), month6(),
                           month7(), month8(), month9(), month10(), month11(), month12())
            
            monthvaccov<-as.numeric(as.character(monthvaccov))
            
            totcoverage<-sum(monthvaccov)
            
            
            novaccine<-cbind(novaccine, monthvaccov)
            
            novaccine$covergdistrib<- c(vaccinecoveragemonthly4(), month24(), month34(), month44(), month54(), month64(), month74(), month84(), month94(), month104(), month114(), month124())
            
            novaccine$covergdistrib<-as.numeric(as.character(novaccine$covergdistrib))
            
            novaccine$monthvaccov<- novaccine$covergdistrib*coverage2() ## replace original monthvaccov column with the new monthly coverage - this is so that the variable doesn't have to be changed in the rest of the script
            
            
            ## month number
            novaccine$monthnum<-c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
            
            ## add monthly effectively vaccinated
            novaccine<-novaccine%>%
                mutate(effectivelyvacc=populationsize*monthvaccov*vaccineeffectiveness)%>%
                mutate(effectivelyvacccumul=cumsum(effectivelyvacc))
            
            
            
            
            #####################################################################
            # Estimated rates
            #####################################################################
            novaccine<-novaccine%>%
                mutate(susceptiblewvacc=populationsize-hospinfcumulwvac-nonhospinfcumulwvac-effectivelyvacccumul)
            
            #Create columns
            novaccine$irhospinf<-NA
            novaccine$irnonhospinf<-NA
            
            #Create first row
            novaccine[1,14]<-novaccine$hospinfwvac[1]/populationsize
            novaccine[1,15]<-novaccine$nonhospinfwvac[1]/populationsize
            
            
            ##Calculation uses last month's susceptible population
            for(i in 2:nrow(novaccine)){
                novaccine$irhospinf[i]<-novaccine$hospinfwvac[i]/novaccine$susceptiblewvacc[i-1]
                novaccine$irnonhospinf[i]<- novaccine$nonhospinfwvac[i]/novaccine$susceptiblewvacc[i-1]
            }
            
            #####################################################################
            # Scenario of influenza circulation in absence of vaccine
            #####################################################################
            
            ## These calculations are initialized by the 'Susceptible at start of month in absence of vaccine'
            ## column, which starts with the Target Population in the first month.
            
            remainsuscstartmonth<-c(populationsize, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
            novaccine<-cbind(novaccine, remainsuscstartmonth)
            
            #Create columns
            novaccine$hospinfwovac<-NA
            novaccine$hospinfcumulwovacc<-NA
            novaccine$nonhospinfwovac<-NA
            novaccine$nonhospinfcumulwovacc<-NA
            novaccine$remainingsusc<-NA
            
            #Create first row
            novaccine[1,17]<-novaccine$remainsuscstartmonth[1]*novaccine$irhospinf[1]
            novaccine[1,18]<-novaccine$hospinfwovac[1]
            novaccine[1,19]<-novaccine$remainsuscstartmonth[1]*novaccine$irnonhospinf[1]
            novaccine[1,20]<-novaccine$nonhospinfwovac[1]
            novaccine[1,21]<-populationsize-novaccine$hospinfcumulwovacc[1]-novaccine$nonhospinfcumulwovacc[1]
            
            
            #Create rows 2:12
            for(i in 2:nrow(novaccine)){
                novaccine$remainsuscstartmonth[i]<-novaccine$remainingsusc[i-1]
                novaccine$hospinfwovac[i]<-novaccine$remainsuscstartmonth[i]*novaccine$irhospinf[i]
                novaccine$hospinfcumulwovacc[i]<-sum(novaccine$hospinfwovac[1:i])
                novaccine$nonhospinfwovac[i]<-novaccine$remainsuscstartmonth[i]*novaccine$irnonhospinf[i]
                novaccine$nonhospinfcumulwovacc[i]<-sum(novaccine$nonhospinfwovac[1:i])
                novaccine$remainingsusc[i]<-populationsize-novaccine$hospinfcumulwovacc[i]-novaccine$nonhospinfcumulwovacc[i]
            }
            
            novaccine<-novaccine%>%
                mutate(overallinfwovac=hospinfwovac+nonhospinfwovac)%>%
                mutate(overallinfcumulwovacc=cumsum(overallinfwovac))%>%
                mutate(medicallyattendedinfwovac=overallinfwovac*mai)%>%
                mutate(medicalattedtoinfcumulwovacc=cumsum(medicallyattendedinfwovac))
            
            ######## Avoided events
            novaccine<-novaccine%>%
                mutate(AEhospinf=hospinfwovac-hospinfwvac)%>%
                mutate(AEnonhospinf=nonhospinfwovac-nonhospinfwvac)%>%
                mutate(AEmedicalattedtoinf=medicallyattendedinfwovac-medicalattedtoinfwvac)%>%
                mutate(AEoverallinf=overallinfwovac-overallinfwvac)
            
            
            ################ Prevented fraction
            ## Hospitalized infections
            ifelse (monthstartvac4()==1,
                    novaccine<-novaccine%>%
                        mutate(PFhospinf=sum(AEhospinf)/sum(hospinfwovac)),
                    novaccine<-novaccine%>%
                        mutate(PFhospinf=sum(AEhospinf[monthstartvac4():nrow(novaccine)]/sum(hospinfwovac[monthstartvac4():nrow(novaccine)])))
            )
            
            ## non-Hospitalized infections
            ifelse (monthstartvac4()==1,
                    novaccine<-novaccine%>%
                        mutate(PFnonhospinf=sum(AEnonhospinf)/sum(nonhospinfwovac)),
                    novaccine<-novaccine%>%
                        mutate(PFnonhospinf=sum(AEnonhospinf[monthstartvac4():nrow(novaccine)]/sum(nonhospinfwovac[monthstartvac4():nrow(novaccine)])))
            )
            
            
            ################ NNV -----------------------------------> why does this only go to month 8 in the Excel formula?????
            ## Hospitalized infections
            ifelse(monthstartvac4()==1,
                   novaccine<-novaccine%>%
                       mutate(NNVhospinf=1/((vaccineeffectiveness*(sum(hospinfwovac)))/populationsize)),
                   novaccine<-novaccine%>%
                       mutate(NNVhospinf=1/((vaccineeffectiveness*(sum(hospinfwovac[monthstartvac4():nrow(novaccine)])))/populationsize))
            )
            
            
            ## non-Hospitalized infections
            ifelse(monthstartvac4()==1,
                   novaccine<-novaccine%>%
                       mutate(NNVnonhospinf=1/((vaccineeffectiveness*(sum(nonhospinfwovac)))/populationsize)),
                   novaccine<-novaccine%>%
                       mutate(NNVnonhospinf=1/((vaccineeffectiveness*(sum(nonhospinfwovac[monthstartvac4():nrow(novaccine)])))/populationsize))
            )
            
            ## Medically infections
            ifelse(monthstartvac4()==1,
                   novaccine<-novaccine%>%
                       mutate(NNVmedicalattendedinf=1/((vaccineeffectiveness*(sum(medicallyattendedinfwovac)))/populationsize)),
                   novaccine<-novaccine%>%
                       mutate(NNVmedicalattendedinf=1/((vaccineeffectiveness*(sum(medicallyattendedinfwovac[monthstartvac4():nrow(novaccine)])))/populationsize))
            )
            
            ## Overall infections
            ifelse(monthstartvac4()==1,
                   novaccine<-novaccine%>%
                       mutate(NNVoverallinf=1/((vaccineeffectiveness*(sum(hospinfwovac)+sum(nonhospinfwovac)))/populationsize)),
                   novaccine<-novaccine%>%
                       mutate(NNVoverallinf=1/((vaccineeffectiveness*(sum(hospinfwovac[monthstartvac4():nrow(novaccine)])+sum(nonhospinfwovac[monthstartvac4():nrow(novaccine)])))/populationsize))
            )
            
            return(novaccine)
            
            
            
        } #close if current campaign
        
        
        
        if(scenario()=="Impact of hypothetical vaccination campaign"){
            
            ##########################################################################################
            # Scenario of influenza circulation in absence of vaccine: Absence to presence of vaccine
            # This is a scenario where the country has yet to conduct a vaccination campaign but would
            # like to see what impact a vaccine campaign may have.
            # The country enters the estimated number of hospitalization they observed and then we compare
            # this to a hypothetical vaccination campaign
            ##########################################################################################
            
            hospinf<-c(hospitalizationsmonthly(), hospmonth2(), hospmonth3(), hospmonth4(), hospmonth5(), hospmonth6(),
                       hospmonth7(), hospmonth8(), hospmonth9(), hospmonth10(), hospmonth11(), hospmonth12())
            
            populationsize<-as.numeric(as.character(populationsize()))
            hnh<-as.numeric(as.character(hnh()))
            mai<-as.numeric(as.character(mai()))
            hospinf<-as.numeric(as.character(hospinf))
            vaccineeffectiveness<-as.numeric(as.character(vaccineeffectiveness()))
            
            #####################Observed influenza in the absence of vaccine
            novaccine<-hospinf%>%
                as.data.frame()%>%
                rename(hospinfwovac=1)%>%
                mutate(hospinfcumulwovac=cumsum(hospinfwovac))%>%
                mutate(nonhospinfwovac=hospinfwovac*hnh)%>%
                mutate(nonhospinfcumulwovac=cumsum(nonhospinfwovac))%>%
                mutate(remainingsusc=populationsize-hospinfcumulwovac-nonhospinfcumulwovac)
            
            #create remainsuscstartmonth column with target population and rows 1:11 of remainingsusc
            remainsuscstartmonth<-populationsize%>%
                as.data.frame()%>%
                rename(remainsuscstartmonth=1)
            
            remainsuscstartmonth2<-as.data.frame(novaccine$remainingsusc[1:11])%>%
                as.data.frame()%>%
                rename(remainsuscstartmonth=1)
            
            remainsuscstartmonth<-rbind(remainsuscstartmonth, remainsuscstartmonth2)
            
            novaccine<-cbind(novaccine, remainsuscstartmonth)
            
            novaccine<-novaccine%>%
                mutate(overallinfwovac=hospinfwovac+nonhospinfwovac)%>%
                mutate(overallinfcumulwovac=cumsum(overallinfwovac))%>%
                mutate(medicallyattendedinfwovac=overallinfwovac*mai)%>%
                mutate(medicallyattendedinfcumulwovac=cumsum(medicallyattendedinfwovac))
            
            
            
            #####################################################################
            # Incidence rates in the absence of vaccine
            #####################################################################
            novaccine<-novaccine%>%
                mutate(irhospinf=hospinfwovac/remainsuscstartmonth)%>%
                mutate(irnonhospinf=nonhospinfwovac/remainsuscstartmonth)
            
            
            
            
            ############################ Burden inputs with hypothetical vaccination campaign
            #Create columns
            novaccine$hospinfwvac<-NA
            novaccine$hospinfcumulwvac<-NA
            novaccine$nonhospinfwvac<-NA
            novaccine$nonhospinfcumulwvac<-NA
            
            
            
            #Create first row
            novaccine[1,13]<-populationsize*novaccine$irhospinf[1]
            novaccine[1,14]<-novaccine$hospinfwvac[1]
            novaccine[1,15]<-populationsize*novaccine$irnonhospinf[1]
            novaccine[1,16]<-novaccine$nonhospinfwvac[1]
            
            ##bind monthly coverage
            monthvaccov<-c(vaccinecoveragemonthly(), month2(), month3(), month4(), month5(), month6(),
                           month7(), month8(), month9(), month10(), month11(), month12())
            
            monthvaccov<-as.numeric(as.character(monthvaccov))
            
            totcoverage<-sum(monthvaccov)
            
            
            novaccine<-cbind(novaccine, monthvaccov)
            
            novaccine$covergdistrib<- c(vaccinecoveragemonthly4(), month24(), month34(), month44(), month54(), month64(), month74(), month84(), month94(), month104(), month114(), month124())
            
            novaccine$covergdistrib<-as.numeric(as.character(novaccine$covergdistrib))
            
            novaccine$monthvaccov<- novaccine$covergdistrib*coverage2() ## replace original monthvaccov column with the new monthly coverage - this is so that the variable doesn't have to be changed in the rest of the script
            
            
            ##monthly effectively vaccinated and susceptible with hypothetical vaccination campaign
            novaccine<-novaccine%>%
                mutate(effectivelyvacc=populationsize*monthvaccov*vaccineeffectiveness)%>%
                mutate(effectivelycumulvacc=cumsum(effectivelyvacc))%>%
                mutate(suscwvacc=populationsize-hospinfcumulwvac-nonhospinfcumulwvac-effectivelyvacc)
            
            ##Then calculate the rest of hospinfwovac and nonhospinfwovac. Calculation uses last month's incidence rate
            for(i in 2:nrow(novaccine)){
                novaccine$hospinfwvac[i]<-novaccine$suscwvacc[i-1]*novaccine$irhospinf[i]
                novaccine$hospinfcumulwvac[i]<-sum(novaccine$hospinfwvac[1:i])
                novaccine$nonhospinfwvac[i]<- novaccine$suscwvacc[i-1]*novaccine$irnonhospinf[i]
                novaccine$nonhospinfcumulwvac[i]<-sum(novaccine$nonhospinfwvac[1:i])
                novaccine$suscwvacc[i]=populationsize-novaccine$hospinfcumulwvac[i]-novaccine$nonhospinfcumulwvac[i]-novaccine$effectivelycumulvacc[i]
            }
            
            
            ## month number
            novaccine$monthnum<-c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
            
            
            ## Overall infections with hypothetical vaccination campaign
            novaccine<-novaccine%>%
                mutate(overallinfwvac=hospinfwvac+nonhospinfwvac)%>%
                mutate(overallinfcumulwvacc=cumsum(overallinfwvac))
            
            ## Medically attended with hypothetical vaccination campaign
            novaccine<-novaccine%>%
                mutate(medicalattedtoinfwvac=overallinfwvac*mai)
            
            
            ######## Avoided events
            novaccine<-novaccine%>%
                mutate(AEhospinf=hospinfwovac-hospinfwvac)%>%
                mutate(AEnonhospinf=nonhospinfwovac-nonhospinfwvac)%>%
                mutate(AEmedicalattedtoinf=medicallyattendedinfwovac-medicalattedtoinfwvac)%>%
                mutate(AEoverallinf=overallinfwovac-overallinfwvac)
            
            
            
            ################ Prevented fraction
            ## Hospitalized infections
            ifelse (monthstartvac4()==1,
                    novaccine<-novaccine%>%
                        mutate(PFhospinf=sum(AEhospinf)/sum(hospinfwovac)),
                    novaccine<-novaccine%>%
                        mutate(PFhospinf=sum(AEhospinf[monthstartvac4():nrow(novaccine)]/sum(hospinfwovac[monthstartvac4():nrow(novaccine)])))
            )
            
            ## non-Hospitalized infections
            ifelse (monthstartvac4()==1,
                    novaccine<-novaccine%>%
                        mutate(PFnonhospinf=sum(AEnonhospinf)/sum(nonhospinfwovac)),
                    novaccine<-novaccine%>%
                        mutate(PFnonhospinf=sum(AEnonhospinf[monthstartvac4():nrow(novaccine)]/sum(nonhospinfwovac[monthstartvac4():nrow(novaccine)])))
            )
            
            
            
            
            ################ NNV -----------------------------------> why does this only go to month 8 in the Excel formula?????
            ## Hospitalized infections
            ifelse(monthstartvac4()==1,
                   novaccine<-novaccine%>%
                       mutate(NNVhospinf=1/((vaccineeffectiveness*(sum(hospinfwovac)))/populationsize)),
                   novaccine<-novaccine%>%
                       mutate(NNVhospinf=1/((vaccineeffectiveness*(sum(hospinfwovac[monthstartvac4():nrow(novaccine)])))/populationsize))
            )
            
            
            ## non-Hospitalized infections
            ifelse(monthstartvac4()==1,
                   novaccine<-novaccine%>%
                       mutate(NNVnonhospinf=1/((vaccineeffectiveness*(sum(nonhospinfwovac)))/populationsize)),
                   novaccine<-novaccine%>%
                       mutate(NNVnonhospinf=1/((vaccineeffectiveness*(sum(nonhospinfwovac[monthstartvac4():nrow(novaccine)])))/populationsize))
            )
            
            ## Medically attended infections
            ifelse(monthstartvac4()==1,
                   novaccine<-novaccine%>%
                       mutate(NNVmedicalattendedinf=1/((vaccineeffectiveness*(sum(medicallyattendedinfwovac)))/populationsize)),
                   novaccine<-novaccine%>%
                       mutate(NNVmedicalattendedinf=1/((vaccineeffectiveness*(sum(medicallyattendedinfwovac[monthstartvac4():nrow(novaccine)])))/populationsize))
            )
            
            ## Total infections
            ifelse(monthstartvac4()==1,
                   novaccine<-novaccine%>%
                       mutate(NNVoverallinf=1/((vaccineeffectiveness*(sum(hospinfwovac)+sum(nonhospinfwovac)))/populationsize)),
                   novaccine<-novaccine%>%
                       mutate(NNVoverallinf=1/((vaccineeffectiveness*(sum(hospinfwovac[monthstartvac4():nrow(novaccine)])+sum(nonhospinfwovac[monthstartvac4():nrow(novaccine)])))/populationsize))
            )
            
            return(novaccine)
            
            
        }#close "if hypothetical campaign"
        
    }) #close burdendataset
    
    
    
    ####################################################################################################
    ## List of burden averted results
    ####################################################################################################
    
    
    burdenavertedresultscoverage2timing4<-reactive({
        
        populationsize<-as.numeric(as.character(populationsize()))
        hnh<-as.numeric(as.character(hnh()))
        mai<-as.numeric(as.character(mai()))
        vaccineeffectiveness<-as.numeric(as.character(vaccineeffectiveness()))
        
        
        
        #################################################
        ## Results
        #################################################
        nonhospinfwovactot<-sum(burdendataset_coverage1timing4()$nonhospinfwovac)
        nonhospinfwvacctot<-sum(burdendataset_coverage1timing4()$nonhospinfwvac)
        nonhospinfaverted<-nonhospinfwovactot-nonhospinfwvacctot
        NNVnonhospinffinal<-burdendataset_coverage1timing4()$NNVnonhospinf[1]
        
        medicallyattendedinfwovactot<-sum(burdendataset_coverage1timing4()$medicallyattendedinfwovac)
        medicallyattendedinfwvactot<-sum(burdendataset_coverage1timing4()$medicalattedtoinfwvac)
        medicallyattendedinfaverted<-medicallyattendedinfwovactot-medicallyattendedinfwvactot
        NNVmedicallyattendtedfinal<-burdendataset_coverage1timing4()$NNVmedicalattendedinf[1]
        
        Totalinfwovacc<-sum(burdendataset_coverage1timing4()$overallinfwovac)
        Totalinfwvacc<-sum(burdendataset_coverage1timing4()$overallinfwvac)
        Totalinfaverted<-Totalinfwovacc-Totalinfwvacc
        NNVtotalinf<-burdendataset_coverage1timing4()$NNVoverallinf[1]
        
        hospinfwovactot<-sum(burdendataset_coverage1timing4()$hospinfwovac)
        hospinfwvacctot<-sum(burdendataset_coverage1timing4()$hospinfwvac)
        hospaverted<-hospinfwovactot-hospinfwvacctot
        NNVhospinffinal<-burdendataset_coverage1timing4()$NNVhospinf[1]
        
        
        nonhospPreventedfraction<-burdendataset_coverage1timing4()$PFnonhospinf[1]
        hospPreventedfraction<-burdendataset_coverage1timing4()$PFhospinf[1]
        
        
        indicator_results1<-c(nonhospinfwovactot, nonhospinfwvacctot, nonhospinfaverted, NNVnonhospinffinal,
                              Totalinfwovacc, Totalinfwvacc, Totalinfaverted, NNVtotalinf,
                              hospinfwovactot, hospinfwvacctot, hospaverted, NNVhospinffinal,
                              medicallyattendedinfwovactot, medicallyattendedinfwvactot, medicallyattendedinfaverted, NNVmedicallyattendtedfinal,
                              nonhospPreventedfraction)
        
        indicator_results_titles<-c("nonhospinfwovactot", "nonhospinfwvacctot", "nonhospinfaverted", "NNVnonhospinffinal",
                                    "Totalinfwovacc", "Totalinfwvacc", "Totalinfaverted", "NNVtotalinf",
                                    "hospinfwovactot", "hospinfwvacctot", "hospaverted", "NNVhospinffinal",
                                    "medicallyattendedinfwovactot", "medicallyattendedinfwvactot", "medicallyattendedinfaverted", "NNVmedicallyattendtedfinal",
                                    "nonhospPreventedfraction")
        
        indicator_results1<-cbind(indicator_results_titles, indicator_results1)
        
        
        return(indicator_results1)
        
        
    })#burdendataset_coverage1timing4
    
    
    
    
    
    #######################################################
    
    ##Deployment coverage 3, timing 1
    ######################################################
    
    
    burdendataset_coverage3timing1<- reactive({
        ################################ The output data when insufficient data has been entered   ##################################
        ############################################################################################################################
        
        
        if(scenario()=="Impact of current influenza vaccination campaign"){
            
            
            ##########################################################################################
            # Scenario of influenza circulation in presence of vaccine: Presence to absence of vaccine
            # This is a situation where the country conducts a vaccination campaign, we enter
            # actual burden inputs and vaccine coverage then compare this to a hypothetical scenario
            # where there is no vaccination campaign conducted
            ##########################################################################################
            
            
            
            
            hospinf<-c(hospitalizationsmonthly(), hospmonth2(), hospmonth3(), hospmonth4(), hospmonth5(), hospmonth6(),
                       hospmonth7(), hospmonth8(), hospmonth9(), hospmonth10(), hospmonth11(), hospmonth12())
            
            populationsize<-as.numeric(as.character(populationsize()))
            vaccineeffectiveness<-as.numeric(as.character(vaccineeffectiveness()))
            hnh<-as.numeric(as.character(hnh()))
            mai<-as.numeric(as.character(mai()))
            hospinf<-as.numeric(as.character(hospinf))
            
            novaccine<-hospinf%>%
                as.data.frame()%>%
                rename(hospinfwvac=1)%>%
                mutate(hospinfcumulwvac=cumsum(hospinfwvac))%>%
                mutate(nonhospinfwvac=hospinfwvac*hnh)%>%
                mutate(nonhospinfcumulwvac=cumsum(nonhospinfwvac))%>%
                mutate(overallinfwvac=hospinfwvac+nonhospinfwvac)%>%
                mutate(overallinfcumulwvac=cumsum(overallinfwvac))%>%
                mutate(medicalattedtoinfwvac=overallinfwvac*mai)%>%
                mutate(medicallyattendedinfcumulwvac=cumsum(medicalattedtoinfwvac))
            
            
            
            ##bind monthly coverage
            monthvaccov<-c(vaccinecoveragemonthly(), month2(), month3(), month4(), month5(), month6(),
                           month7(), month8(), month9(), month10(), month11(), month12())
            
            monthvaccov<-as.numeric(as.character(monthvaccov))
            
            totcoverage<-sum(monthvaccov)
            
            
            novaccine<-cbind(novaccine, monthvaccov)
            
            novaccine$covergdistrib<- c(vaccinecoveragemonthly1(), month21(), month31(), month41(), month51(), month61(), month71(), month81(), month91(), month101(), month111(), month121())
            
            novaccine$covergdistrib<-as.numeric(as.character(novaccine$covergdistrib))
            
            novaccine$monthvaccov<- novaccine$covergdistrib*coverage3() ## replace original monthvaccov column with the new monthly coverage - this is so that the variable doesn't have to be changed in the rest of the script
            
            
            ## month number
            novaccine$monthnum<-c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
            
            ## add monthly effectively vaccinated
            novaccine<-novaccine%>%
                mutate(effectivelyvacc=populationsize*monthvaccov*vaccineeffectiveness)%>%
                mutate(effectivelyvacccumul=cumsum(effectivelyvacc))
            
            
            
            
            #####################################################################
            # Estimated rates
            #####################################################################
            novaccine<-novaccine%>%
                mutate(susceptiblewvacc=populationsize-hospinfcumulwvac-nonhospinfcumulwvac-effectivelyvacccumul)
            
            #Create columns
            novaccine$irhospinf<-NA
            novaccine$irnonhospinf<-NA
            
            #Create first row
            novaccine[1,14]<-novaccine$hospinfwvac[1]/populationsize
            novaccine[1,15]<-novaccine$nonhospinfwvac[1]/populationsize
            
            
            ##Calculation uses last month's susceptible population
            for(i in 2:nrow(novaccine)){
                novaccine$irhospinf[i]<-novaccine$hospinfwvac[i]/novaccine$susceptiblewvacc[i-1]
                novaccine$irnonhospinf[i]<- novaccine$nonhospinfwvac[i]/novaccine$susceptiblewvacc[i-1]
            }
            
            #####################################################################
            # Scenario of influenza circulation in absence of vaccine
            #####################################################################
            
            ## These calculations are initialized by the 'Susceptible at start of month in absence of vaccine'
            ## column, which starts with the Target Population in the first month.
            
            remainsuscstartmonth<-c(populationsize, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
            novaccine<-cbind(novaccine, remainsuscstartmonth)
            
            #Create columns
            novaccine$hospinfwovac<-NA
            novaccine$hospinfcumulwovacc<-NA
            novaccine$nonhospinfwovac<-NA
            novaccine$nonhospinfcumulwovacc<-NA
            novaccine$remainingsusc<-NA
            
            #Create first row
            novaccine[1,17]<-novaccine$remainsuscstartmonth[1]*novaccine$irhospinf[1]
            novaccine[1,18]<-novaccine$hospinfwovac[1]
            novaccine[1,19]<-novaccine$remainsuscstartmonth[1]*novaccine$irnonhospinf[1]
            novaccine[1,20]<-novaccine$nonhospinfwovac[1]
            novaccine[1,21]<-populationsize-novaccine$hospinfcumulwovacc[1]-novaccine$nonhospinfcumulwovacc[1]
            
            
            #Create rows 2:12
            for(i in 2:nrow(novaccine)){
                novaccine$remainsuscstartmonth[i]<-novaccine$remainingsusc[i-1]
                novaccine$hospinfwovac[i]<-novaccine$remainsuscstartmonth[i]*novaccine$irhospinf[i]
                novaccine$hospinfcumulwovacc[i]<-sum(novaccine$hospinfwovac[1:i])
                novaccine$nonhospinfwovac[i]<-novaccine$remainsuscstartmonth[i]*novaccine$irnonhospinf[i]
                novaccine$nonhospinfcumulwovacc[i]<-sum(novaccine$nonhospinfwovac[1:i])
                novaccine$remainingsusc[i]<-populationsize-novaccine$hospinfcumulwovacc[i]-novaccine$nonhospinfcumulwovacc[i]
            }
            
            novaccine<-novaccine%>%
                mutate(overallinfwovac=hospinfwovac+nonhospinfwovac)%>%
                mutate(overallinfcumulwovacc=cumsum(overallinfwovac))%>%
                mutate(medicallyattendedinfwovac=overallinfwovac*mai)%>%
                mutate(medicalattedtoinfcumulwovacc=cumsum(medicallyattendedinfwovac))
            
            ######## Avoided events
            novaccine<-novaccine%>%
                mutate(AEhospinf=hospinfwovac-hospinfwvac)%>%
                mutate(AEnonhospinf=nonhospinfwovac-nonhospinfwvac)%>%
                mutate(AEmedicalattedtoinf=medicallyattendedinfwovac-medicalattedtoinfwvac)%>%
                mutate(AEoverallinf=overallinfwovac-overallinfwvac)
            
            
            ################ Prevented fraction
            ## Hospitalized infections
            ifelse (monthstartvac1()==1,
                    novaccine<-novaccine%>%
                        mutate(PFhospinf=sum(AEhospinf)/sum(hospinfwovac)),
                    novaccine<-novaccine%>%
                        mutate(PFhospinf=sum(AEhospinf[monthstartvac1():nrow(novaccine)]/sum(hospinfwovac[monthstartvac1():nrow(novaccine)])))
            )
            
            ## non-Hospitalized infections
            ifelse (monthstartvac1()==1,
                    novaccine<-novaccine%>%
                        mutate(PFnonhospinf=sum(AEnonhospinf)/sum(nonhospinfwovac)),
                    novaccine<-novaccine%>%
                        mutate(PFnonhospinf=sum(AEnonhospinf[monthstartvac1():nrow(novaccine)]/sum(nonhospinfwovac[monthstartvac1():nrow(novaccine)])))
            )
            
            
            ################ NNV -----------------------------------> why does this only go to month 8 in the Excel formula?????
            ## Hospitalized infections
            ifelse(monthstartvac1()==1,
                   novaccine<-novaccine%>%
                       mutate(NNVhospinf=1/((vaccineeffectiveness*(sum(hospinfwovac)))/populationsize)),
                   novaccine<-novaccine%>%
                       mutate(NNVhospinf=1/((vaccineeffectiveness*(sum(hospinfwovac[monthstartvac1():nrow(novaccine)])))/populationsize))
            )
            
            
            ## non-Hospitalized infections
            ifelse(monthstartvac1()==1,
                   novaccine<-novaccine%>%
                       mutate(NNVnonhospinf=1/((vaccineeffectiveness*(sum(nonhospinfwovac)))/populationsize)),
                   novaccine<-novaccine%>%
                       mutate(NNVnonhospinf=1/((vaccineeffectiveness*(sum(nonhospinfwovac[monthstartvac1():nrow(novaccine)])))/populationsize))
            )
            
            ## Medically infections
            ifelse(monthstartvac1()==1,
                   novaccine<-novaccine%>%
                       mutate(NNVmedicalattendedinf=1/((vaccineeffectiveness*(sum(medicallyattendedinfwovac)))/populationsize)),
                   novaccine<-novaccine%>%
                       mutate(NNVmedicalattendedinf=1/((vaccineeffectiveness*(sum(medicallyattendedinfwovac[monthstartvac1():nrow(novaccine)])))/populationsize))
            )
            
            ## Overall infections
            ifelse(monthstartvac1()==1,
                   novaccine<-novaccine%>%
                       mutate(NNVoverallinf=1/((vaccineeffectiveness*(sum(hospinfwovac)+sum(nonhospinfwovac)))/populationsize)),
                   novaccine<-novaccine%>%
                       mutate(NNVoverallinf=1/((vaccineeffectiveness*(sum(hospinfwovac[monthstartvac1():nrow(novaccine)])+sum(nonhospinfwovac[monthstartvac1():nrow(novaccine)])))/populationsize))
            )
            
            return(novaccine)
            
            
            
        } #close if current campaign
        
        
        
        if(scenario()=="Impact of hypothetical vaccination campaign"){
            
            ##########################################################################################
            # Scenario of influenza circulation in absence of vaccine: Absence to presence of vaccine
            # This is a scenario where the country has yet to conduct a vaccination campaign but would
            # like to see what impact a vaccine campaign may have.
            # The country enters the estimated number of hospitalization they observed and then we compare
            # this to a hypothetical vaccination campaign
            ##########################################################################################
            
            hospinf<-c(hospitalizationsmonthly(), hospmonth2(), hospmonth3(), hospmonth4(), hospmonth5(), hospmonth6(),
                       hospmonth7(), hospmonth8(), hospmonth9(), hospmonth10(), hospmonth11(), hospmonth12())
            
            populationsize<-as.numeric(as.character(populationsize()))
            hnh<-as.numeric(as.character(hnh()))
            mai<-as.numeric(as.character(mai()))
            hospinf<-as.numeric(as.character(hospinf))
            vaccineeffectiveness<-as.numeric(as.character(vaccineeffectiveness()))
            
            #####################Observed influenza in the absence of vaccine
            novaccine<-hospinf%>%
                as.data.frame()%>%
                rename(hospinfwovac=1)%>%
                mutate(hospinfcumulwovac=cumsum(hospinfwovac))%>%
                mutate(nonhospinfwovac=hospinfwovac*hnh)%>%
                mutate(nonhospinfcumulwovac=cumsum(nonhospinfwovac))%>%
                mutate(remainingsusc=populationsize-hospinfcumulwovac-nonhospinfcumulwovac)
            
            #create remainsuscstartmonth column with target population and rows 1:11 of remainingsusc
            remainsuscstartmonth<-populationsize%>%
                as.data.frame()%>%
                rename(remainsuscstartmonth=1)
            
            remainsuscstartmonth2<-as.data.frame(novaccine$remainingsusc[1:11])%>%
                as.data.frame()%>%
                rename(remainsuscstartmonth=1)
            
            remainsuscstartmonth<-rbind(remainsuscstartmonth, remainsuscstartmonth2)
            
            novaccine<-cbind(novaccine, remainsuscstartmonth)
            
            novaccine<-novaccine%>%
                mutate(overallinfwovac=hospinfwovac+nonhospinfwovac)%>%
                mutate(overallinfcumulwovac=cumsum(overallinfwovac))%>%
                mutate(medicallyattendedinfwovac=overallinfwovac*mai)%>%
                mutate(medicallyattendedinfcumulwovac=cumsum(medicallyattendedinfwovac))
            
            
            
            #####################################################################
            # Incidence rates in the absence of vaccine
            #####################################################################
            novaccine<-novaccine%>%
                mutate(irhospinf=hospinfwovac/remainsuscstartmonth)%>%
                mutate(irnonhospinf=nonhospinfwovac/remainsuscstartmonth)
            
            
            
            
            ############################ Burden inputs with hypothetical vaccination campaign
            #Create columns
            novaccine$hospinfwvac<-NA
            novaccine$hospinfcumulwvac<-NA
            novaccine$nonhospinfwvac<-NA
            novaccine$nonhospinfcumulwvac<-NA
            
            
            
            #Create first row
            novaccine[1,13]<-populationsize*novaccine$irhospinf[1]
            novaccine[1,14]<-novaccine$hospinfwvac[1]
            novaccine[1,15]<-populationsize*novaccine$irnonhospinf[1]
            novaccine[1,16]<-novaccine$nonhospinfwvac[1]
            
            ##bind monthly coverage
            monthvaccov<-c(vaccinecoveragemonthly(), month2(), month3(), month4(), month5(), month6(),
                           month7(), month8(), month9(), month10(), month11(), month12())
            
            monthvaccov<-as.numeric(as.character(monthvaccov))
            
            totcoverage<-sum(monthvaccov)
            
            
            novaccine<-cbind(novaccine, monthvaccov)
            
            novaccine$covergdistrib<- c(vaccinecoveragemonthly1(), month21(), month31(), month41(), month51(), month61(), month71(), month81(), month91(), month101(), month111(), month121())
            
            novaccine$covergdistrib<-as.numeric(as.character(novaccine$covergdistrib))
            
            novaccine$monthvaccov<- novaccine$covergdistrib*coverage3() ## replace original monthvaccov column with the new monthly coverage - this is so that the variable doesn't have to be changed in the rest of the script
            
            
            ##monthly effectively vaccinated and susceptible with hypothetical vaccination campaign
            novaccine<-novaccine%>%
                mutate(effectivelyvacc=populationsize*monthvaccov*vaccineeffectiveness)%>%
                mutate(effectivelycumulvacc=cumsum(effectivelyvacc))%>%
                mutate(suscwvacc=populationsize-hospinfcumulwvac-nonhospinfcumulwvac-effectivelyvacc)
            
            ##Then calculate the rest of hospinfwovac and nonhospinfwovac. Calculation uses last month's incidence rate
            for(i in 2:nrow(novaccine)){
                novaccine$hospinfwvac[i]<-novaccine$suscwvacc[i-1]*novaccine$irhospinf[i]
                novaccine$hospinfcumulwvac[i]<-sum(novaccine$hospinfwvac[1:i])
                novaccine$nonhospinfwvac[i]<- novaccine$suscwvacc[i-1]*novaccine$irnonhospinf[i]
                novaccine$nonhospinfcumulwvac[i]<-sum(novaccine$nonhospinfwvac[1:i])
                novaccine$suscwvacc[i]=populationsize-novaccine$hospinfcumulwvac[i]-novaccine$nonhospinfcumulwvac[i]-novaccine$effectivelycumulvacc[i]
            }
            
            
            ## month number
            novaccine$monthnum<-c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
            
            
            ## Overall infections with hypothetical vaccination campaign
            novaccine<-novaccine%>%
                mutate(overallinfwvac=hospinfwvac+nonhospinfwvac)%>%
                mutate(overallinfcumulwvacc=cumsum(overallinfwvac))
            
            ## Medically attended with hypothetical vaccination campaign
            novaccine<-novaccine%>%
                mutate(medicalattedtoinfwvac=overallinfwvac*mai)
            
            
            ######## Avoided events
            novaccine<-novaccine%>%
                mutate(AEhospinf=hospinfwovac-hospinfwvac)%>%
                mutate(AEnonhospinf=nonhospinfwovac-nonhospinfwvac)%>%
                mutate(AEmedicalattedtoinf=medicallyattendedinfwovac-medicalattedtoinfwvac)%>%
                mutate(AEoverallinf=overallinfwovac-overallinfwvac)
            
            
            
            ################ Prevented fraction
            ## Hospitalized infections
            ifelse (monthstartvac1()==1,
                    novaccine<-novaccine%>%
                        mutate(PFhospinf=sum(AEhospinf)/sum(hospinfwovac)),
                    novaccine<-novaccine%>%
                        mutate(PFhospinf=sum(AEhospinf[monthstartvac1():nrow(novaccine)]/sum(hospinfwovac[monthstartvac1():nrow(novaccine)])))
            )
            
            ## non-Hospitalized infections
            ifelse (monthstartvac1()==1,
                    novaccine<-novaccine%>%
                        mutate(PFnonhospinf=sum(AEnonhospinf)/sum(nonhospinfwovac)),
                    novaccine<-novaccine%>%
                        mutate(PFnonhospinf=sum(AEnonhospinf[monthstartvac1():nrow(novaccine)]/sum(nonhospinfwovac[monthstartvac1():nrow(novaccine)])))
            )
            
            
            
            
            ################ NNV -----------------------------------> why does this only go to month 8 in the Excel formula?????
            ## Hospitalized infections
            ifelse(monthstartvac1()==1,
                   novaccine<-novaccine%>%
                       mutate(NNVhospinf=1/((vaccineeffectiveness*(sum(hospinfwovac)))/populationsize)),
                   novaccine<-novaccine%>%
                       mutate(NNVhospinf=1/((vaccineeffectiveness*(sum(hospinfwovac[monthstartvac1():nrow(novaccine)])))/populationsize))
            )
            
            
            ## non-Hospitalized infections
            ifelse(monthstartvac1()==1,
                   novaccine<-novaccine%>%
                       mutate(NNVnonhospinf=1/((vaccineeffectiveness*(sum(nonhospinfwovac)))/populationsize)),
                   novaccine<-novaccine%>%
                       mutate(NNVnonhospinf=1/((vaccineeffectiveness*(sum(nonhospinfwovac[monthstartvac1():nrow(novaccine)])))/populationsize))
            )
            
            ## Medically attended infections
            ifelse(monthstartvac1()==1,
                   novaccine<-novaccine%>%
                       mutate(NNVmedicalattendedinf=1/((vaccineeffectiveness*(sum(medicallyattendedinfwovac)))/populationsize)),
                   novaccine<-novaccine%>%
                       mutate(NNVmedicalattendedinf=1/((vaccineeffectiveness*(sum(medicallyattendedinfwovac[monthstartvac1():nrow(novaccine)])))/populationsize))
            )
            
            ## Total infections
            ifelse(monthstartvac1()==1,
                   novaccine<-novaccine%>%
                       mutate(NNVoverallinf=1/((vaccineeffectiveness*(sum(hospinfwovac)+sum(nonhospinfwovac)))/populationsize)),
                   novaccine<-novaccine%>%
                       mutate(NNVoverallinf=1/((vaccineeffectiveness*(sum(hospinfwovac[monthstartvac1():nrow(novaccine)])+sum(nonhospinfwovac[monthstartvac1():nrow(novaccine)])))/populationsize))
            )
            
            return(novaccine)
            
            
        }#close "if hypothetical campaign"
        
    }) #close burdendataset
    
    
    
    ####################################################################################################
    ## List of burden averted results
    ####################################################################################################
    
    
    burdenavertedresultscoverage3timing1<-reactive({
        
        populationsize<-as.numeric(as.character(populationsize()))
        hnh<-as.numeric(as.character(hnh()))
        mai<-as.numeric(as.character(mai()))
        vaccineeffectiveness<-as.numeric(as.character(vaccineeffectiveness()))
        
        
        
        #################################################
        ## Results
        #################################################
        nonhospinfwovactot<-sum(burdendataset_coverage3timing1()$nonhospinfwovac)
        nonhospinfwvacctot<-sum(burdendataset_coverage3timing1()$nonhospinfwvac)
        nonhospinfaverted<-nonhospinfwovactot-nonhospinfwvacctot
        NNVnonhospinffinal<-burdendataset_coverage3timing1()$NNVnonhospinf[1]
        
        medicallyattendedinfwovactot<-sum(burdendataset_coverage3timing1()$medicallyattendedinfwovac)
        medicallyattendedinfwvactot<-sum(burdendataset_coverage3timing1()$medicalattedtoinfwvac)
        medicallyattendedinfaverted<-medicallyattendedinfwovactot-medicallyattendedinfwvactot
        NNVmedicallyattendtedfinal<-burdendataset_coverage3timing1()$NNVmedicalattendedinf[1]
        
        Totalinfwovacc<-sum(burdendataset_coverage3timing1()$overallinfwovac)
        Totalinfwvacc<-sum(burdendataset_coverage3timing1()$overallinfwvac)
        Totalinfaverted<-Totalinfwovacc-Totalinfwvacc
        NNVtotalinf<-burdendataset_coverage3timing1()$NNVoverallinf[1]
        
        hospinfwovactot<-sum(burdendataset_coverage3timing1()$hospinfwovac)
        hospinfwvacctot<-sum(burdendataset_coverage3timing1()$hospinfwvac)
        hospaverted<-hospinfwovactot-hospinfwvacctot
        NNVhospinffinal<-burdendataset_coverage3timing1()$NNVhospinf[1]
        
        
        nonhospPreventedfraction<-burdendataset_coverage3timing1()$PFnonhospinf[1]
        hospPreventedfraction<-burdendataset_coverage3timing1()$PFhospinf[1]
        
        
        indicator_results1<-c(nonhospinfwovactot, nonhospinfwvacctot, nonhospinfaverted, NNVnonhospinffinal,
                              Totalinfwovacc, Totalinfwvacc, Totalinfaverted, NNVtotalinf,
                              hospinfwovactot, hospinfwvacctot, hospaverted, NNVhospinffinal,
                              medicallyattendedinfwovactot, medicallyattendedinfwvactot, medicallyattendedinfaverted, NNVmedicallyattendtedfinal,
                              nonhospPreventedfraction)
        
        indicator_results_titles<-c("nonhospinfwovactot", "nonhospinfwvacctot", "nonhospinfaverted", "NNVnonhospinffinal",
                                    "Totalinfwovacc", "Totalinfwvacc", "Totalinfaverted", "NNVtotalinf",
                                    "hospinfwovactot", "hospinfwvacctot", "hospaverted", "NNVhospinffinal",
                                    "medicallyattendedinfwovactot", "medicallyattendedinfwvactot", "medicallyattendedinfaverted", "NNVmedicallyattendtedfinal",
                                    "nonhospPreventedfraction")
        
        indicator_results1<-cbind(indicator_results_titles, indicator_results1)
        
        
        return(indicator_results1)
        
        
    })#burdendataset_coverage3timing1
    
    
    
    #######################################################
    
    ##Deployment coverage 3, timing 2
    ######################################################
    
    
    burdendataset_coverage3timing2<- reactive({
        ################################ The output data when insufficient data has been entered   ##################################
        ############################################################################################################################
        
        
        if(scenario()=="Impact of current influenza vaccination campaign"){
            
            
            ##########################################################################################
            # Scenario of influenza circulation in presence of vaccine: Presence to absence of vaccine
            # This is a situation where the country conducts a vaccination campaign, we enter
            # actual burden inputs and vaccine coverage then compare this to a hypothetical scenario
            # where there is no vaccination campaign conducted
            ##########################################################################################
            
            
            
            
            hospinf<-c(hospitalizationsmonthly(), hospmonth2(), hospmonth3(), hospmonth4(), hospmonth5(), hospmonth6(),
                       hospmonth7(), hospmonth8(), hospmonth9(), hospmonth10(), hospmonth11(), hospmonth12())
            
            populationsize<-as.numeric(as.character(populationsize()))
            vaccineeffectiveness<-as.numeric(as.character(vaccineeffectiveness()))
            hnh<-as.numeric(as.character(hnh()))
            mai<-as.numeric(as.character(mai()))
            hospinf<-as.numeric(as.character(hospinf))
            
            novaccine<-hospinf%>%
                as.data.frame()%>%
                rename(hospinfwvac=1)%>%
                mutate(hospinfcumulwvac=cumsum(hospinfwvac))%>%
                mutate(nonhospinfwvac=hospinfwvac*hnh)%>%
                mutate(nonhospinfcumulwvac=cumsum(nonhospinfwvac))%>%
                mutate(overallinfwvac=hospinfwvac+nonhospinfwvac)%>%
                mutate(overallinfcumulwvac=cumsum(overallinfwvac))%>%
                mutate(medicalattedtoinfwvac=overallinfwvac*mai)%>%
                mutate(medicallyattendedinfcumulwvac=cumsum(medicalattedtoinfwvac))
            
            
            
            ##bind monthly coverage
            monthvaccov<-c(vaccinecoveragemonthly(), month2(), month3(), month4(), month5(), month6(),
                           month7(), month8(), month9(), month10(), month11(), month12())
            
            monthvaccov<-as.numeric(as.character(monthvaccov))
            
            totcoverage<-sum(monthvaccov)
            
            
            novaccine<-cbind(novaccine, monthvaccov)
            
            novaccine$covergdistrib<- c(vaccinecoveragemonthly2(), month22(), month32(), month42(), month52(), month62(), month72(), month82(), month92(), month102(), month112(), month122())
            
            novaccine$covergdistrib<-as.numeric(as.character(novaccine$covergdistrib))
            
            novaccine$monthvaccov<- novaccine$covergdistrib*coverage3() ## replace original monthvaccov column with the new monthly coverage - this is so that the variable doesn't have to be changed in the rest of the script
            
            
            ## month number
            novaccine$monthnum<-c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
            
            ## add monthly effectively vaccinated
            novaccine<-novaccine%>%
                mutate(effectivelyvacc=populationsize*monthvaccov*vaccineeffectiveness)%>%
                mutate(effectivelyvacccumul=cumsum(effectivelyvacc))
            
            
            
            
            #####################################################################
            # Estimated rates
            #####################################################################
            novaccine<-novaccine%>%
                mutate(susceptiblewvacc=populationsize-hospinfcumulwvac-nonhospinfcumulwvac-effectivelyvacccumul)
            
            #Create columns
            novaccine$irhospinf<-NA
            novaccine$irnonhospinf<-NA
            
            #Create first row
            novaccine[1,14]<-novaccine$hospinfwvac[1]/populationsize
            novaccine[1,15]<-novaccine$nonhospinfwvac[1]/populationsize
            
            
            ##Calculation uses last month's susceptible population
            for(i in 2:nrow(novaccine)){
                novaccine$irhospinf[i]<-novaccine$hospinfwvac[i]/novaccine$susceptiblewvacc[i-1]
                novaccine$irnonhospinf[i]<- novaccine$nonhospinfwvac[i]/novaccine$susceptiblewvacc[i-1]
            }
            
            #####################################################################
            # Scenario of influenza circulation in absence of vaccine
            #####################################################################
            
            ## These calculations are initialized by the 'Susceptible at start of month in absence of vaccine'
            ## column, which starts with the Target Population in the first month.
            
            remainsuscstartmonth<-c(populationsize, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
            novaccine<-cbind(novaccine, remainsuscstartmonth)
            
            #Create columns
            novaccine$hospinfwovac<-NA
            novaccine$hospinfcumulwovacc<-NA
            novaccine$nonhospinfwovac<-NA
            novaccine$nonhospinfcumulwovacc<-NA
            novaccine$remainingsusc<-NA
            
            #Create first row
            novaccine[1,17]<-novaccine$remainsuscstartmonth[1]*novaccine$irhospinf[1]
            novaccine[1,18]<-novaccine$hospinfwovac[1]
            novaccine[1,19]<-novaccine$remainsuscstartmonth[1]*novaccine$irnonhospinf[1]
            novaccine[1,20]<-novaccine$nonhospinfwovac[1]
            novaccine[1,21]<-populationsize-novaccine$hospinfcumulwovacc[1]-novaccine$nonhospinfcumulwovacc[1]
            
            
            #Create rows 2:12
            for(i in 2:nrow(novaccine)){
                novaccine$remainsuscstartmonth[i]<-novaccine$remainingsusc[i-1]
                novaccine$hospinfwovac[i]<-novaccine$remainsuscstartmonth[i]*novaccine$irhospinf[i]
                novaccine$hospinfcumulwovacc[i]<-sum(novaccine$hospinfwovac[1:i])
                novaccine$nonhospinfwovac[i]<-novaccine$remainsuscstartmonth[i]*novaccine$irnonhospinf[i]
                novaccine$nonhospinfcumulwovacc[i]<-sum(novaccine$nonhospinfwovac[1:i])
                novaccine$remainingsusc[i]<-populationsize-novaccine$hospinfcumulwovacc[i]-novaccine$nonhospinfcumulwovacc[i]
            }
            
            novaccine<-novaccine%>%
                mutate(overallinfwovac=hospinfwovac+nonhospinfwovac)%>%
                mutate(overallinfcumulwovacc=cumsum(overallinfwovac))%>%
                mutate(medicallyattendedinfwovac=overallinfwovac*mai)%>%
                mutate(medicalattedtoinfcumulwovacc=cumsum(medicallyattendedinfwovac))
            
            ######## Avoided events
            novaccine<-novaccine%>%
                mutate(AEhospinf=hospinfwovac-hospinfwvac)%>%
                mutate(AEnonhospinf=nonhospinfwovac-nonhospinfwvac)%>%
                mutate(AEmedicalattedtoinf=medicallyattendedinfwovac-medicalattedtoinfwvac)%>%
                mutate(AEoverallinf=overallinfwovac-overallinfwvac)
            
            
            ################ Prevented fraction
            ## Hospitalized infections
            ifelse (monthstartvac2()==1,
                    novaccine<-novaccine%>%
                        mutate(PFhospinf=sum(AEhospinf)/sum(hospinfwovac)),
                    novaccine<-novaccine%>%
                        mutate(PFhospinf=sum(AEhospinf[monthstartvac2():nrow(novaccine)]/sum(hospinfwovac[monthstartvac2():nrow(novaccine)])))
            )
            
            ## non-Hospitalized infections
            ifelse (monthstartvac2()==1,
                    novaccine<-novaccine%>%
                        mutate(PFnonhospinf=sum(AEnonhospinf)/sum(nonhospinfwovac)),
                    novaccine<-novaccine%>%
                        mutate(PFnonhospinf=sum(AEnonhospinf[monthstartvac2():nrow(novaccine)]/sum(nonhospinfwovac[monthstartvac2():nrow(novaccine)])))
            )
            
            
            ################ NNV -----------------------------------> why does this only go to month 8 in the Excel formula?????
            ## Hospitalized infections
            ifelse(monthstartvac2()==1,
                   novaccine<-novaccine%>%
                       mutate(NNVhospinf=1/((vaccineeffectiveness*(sum(hospinfwovac)))/populationsize)),
                   novaccine<-novaccine%>%
                       mutate(NNVhospinf=1/((vaccineeffectiveness*(sum(hospinfwovac[monthstartvac2():nrow(novaccine)])))/populationsize))
            )
            
            
            ## non-Hospitalized infections
            ifelse(monthstartvac2()==1,
                   novaccine<-novaccine%>%
                       mutate(NNVnonhospinf=1/((vaccineeffectiveness*(sum(nonhospinfwovac)))/populationsize)),
                   novaccine<-novaccine%>%
                       mutate(NNVnonhospinf=1/((vaccineeffectiveness*(sum(nonhospinfwovac[monthstartvac2():nrow(novaccine)])))/populationsize))
            )
            
            ## Medically infections
            ifelse(monthstartvac2()==1,
                   novaccine<-novaccine%>%
                       mutate(NNVmedicalattendedinf=1/((vaccineeffectiveness*(sum(medicallyattendedinfwovac)))/populationsize)),
                   novaccine<-novaccine%>%
                       mutate(NNVmedicalattendedinf=1/((vaccineeffectiveness*(sum(medicallyattendedinfwovac[monthstartvac2():nrow(novaccine)])))/populationsize))
            )
            
            ## Overall infections
            ifelse(monthstartvac2()==1,
                   novaccine<-novaccine%>%
                       mutate(NNVoverallinf=1/((vaccineeffectiveness*(sum(hospinfwovac)+sum(nonhospinfwovac)))/populationsize)),
                   novaccine<-novaccine%>%
                       mutate(NNVoverallinf=1/((vaccineeffectiveness*(sum(hospinfwovac[monthstartvac2():nrow(novaccine)])+sum(nonhospinfwovac[monthstartvac2():nrow(novaccine)])))/populationsize))
            )
            
            return(novaccine)
            
            
            
        } #close if current campaign
        
        
        
        if(scenario()=="Impact of hypothetical vaccination campaign"){
            
            ##########################################################################################
            # Scenario of influenza circulation in absence of vaccine: Absence to presence of vaccine
            # This is a scenario where the country has yet to conduct a vaccination campaign but would
            # like to see what impact a vaccine campaign may have.
            # The country enters the estimated number of hospitalization they observed and then we compare
            # this to a hypothetical vaccination campaign
            ##########################################################################################
            
            hospinf<-c(hospitalizationsmonthly(), hospmonth2(), hospmonth3(), hospmonth4(), hospmonth5(), hospmonth6(),
                       hospmonth7(), hospmonth8(), hospmonth9(), hospmonth10(), hospmonth11(), hospmonth12())
            
            populationsize<-as.numeric(as.character(populationsize()))
            hnh<-as.numeric(as.character(hnh()))
            mai<-as.numeric(as.character(mai()))
            hospinf<-as.numeric(as.character(hospinf))
            vaccineeffectiveness<-as.numeric(as.character(vaccineeffectiveness()))
            
            #####################Observed influenza in the absence of vaccine
            novaccine<-hospinf%>%
                as.data.frame()%>%
                rename(hospinfwovac=1)%>%
                mutate(hospinfcumulwovac=cumsum(hospinfwovac))%>%
                mutate(nonhospinfwovac=hospinfwovac*hnh)%>%
                mutate(nonhospinfcumulwovac=cumsum(nonhospinfwovac))%>%
                mutate(remainingsusc=populationsize-hospinfcumulwovac-nonhospinfcumulwovac)
            
            #create remainsuscstartmonth column with target population and rows 1:11 of remainingsusc
            remainsuscstartmonth<-populationsize%>%
                as.data.frame()%>%
                rename(remainsuscstartmonth=1)
            
            remainsuscstartmonth2<-as.data.frame(novaccine$remainingsusc[1:11])%>%
                as.data.frame()%>%
                rename(remainsuscstartmonth=1)
            
            remainsuscstartmonth<-rbind(remainsuscstartmonth, remainsuscstartmonth2)
            
            novaccine<-cbind(novaccine, remainsuscstartmonth)
            
            novaccine<-novaccine%>%
                mutate(overallinfwovac=hospinfwovac+nonhospinfwovac)%>%
                mutate(overallinfcumulwovac=cumsum(overallinfwovac))%>%
                mutate(medicallyattendedinfwovac=overallinfwovac*mai)%>%
                mutate(medicallyattendedinfcumulwovac=cumsum(medicallyattendedinfwovac))
            
            
            
            #####################################################################
            # Incidence rates in the absence of vaccine
            #####################################################################
            novaccine<-novaccine%>%
                mutate(irhospinf=hospinfwovac/remainsuscstartmonth)%>%
                mutate(irnonhospinf=nonhospinfwovac/remainsuscstartmonth)
            
            
            
            
            ############################ Burden inputs with hypothetical vaccination campaign
            #Create columns
            novaccine$hospinfwvac<-NA
            novaccine$hospinfcumulwvac<-NA
            novaccine$nonhospinfwvac<-NA
            novaccine$nonhospinfcumulwvac<-NA
            
            
            
            #Create first row
            novaccine[1,13]<-populationsize*novaccine$irhospinf[1]
            novaccine[1,14]<-novaccine$hospinfwvac[1]
            novaccine[1,15]<-populationsize*novaccine$irnonhospinf[1]
            novaccine[1,16]<-novaccine$nonhospinfwvac[1]
            
            ##bind monthly coverage
            monthvaccov<-c(vaccinecoveragemonthly(), month2(), month3(), month4(), month5(), month6(),
                           month7(), month8(), month9(), month10(), month11(), month12())
            
            monthvaccov<-as.numeric(as.character(monthvaccov))
            
            totcoverage<-sum(monthvaccov)
            
            
            novaccine<-cbind(novaccine, monthvaccov)
            
            novaccine$covergdistrib<- c(vaccinecoveragemonthly2(), month22(), month32(), month42(), month52(), month62(), month72(), month82(), month92(), month102(), month112(), month122())
            
            novaccine$covergdistrib<-as.numeric(as.character(novaccine$covergdistrib))
            
            novaccine$monthvaccov<- novaccine$covergdistrib*coverage3() ## replace original monthvaccov column with the new monthly coverage - this is so that the variable doesn't have to be changed in the rest of the script
            
            
            ##monthly effectively vaccinated and susceptible with hypothetical vaccination campaign
            novaccine<-novaccine%>%
                mutate(effectivelyvacc=populationsize*monthvaccov*vaccineeffectiveness)%>%
                mutate(effectivelycumulvacc=cumsum(effectivelyvacc))%>%
                mutate(suscwvacc=populationsize-hospinfcumulwvac-nonhospinfcumulwvac-effectivelyvacc)
            
            ##Then calculate the rest of hospinfwovac and nonhospinfwovac. Calculation uses last month's incidence rate
            for(i in 2:nrow(novaccine)){
                novaccine$hospinfwvac[i]<-novaccine$suscwvacc[i-1]*novaccine$irhospinf[i]
                novaccine$hospinfcumulwvac[i]<-sum(novaccine$hospinfwvac[1:i])
                novaccine$nonhospinfwvac[i]<- novaccine$suscwvacc[i-1]*novaccine$irnonhospinf[i]
                novaccine$nonhospinfcumulwvac[i]<-sum(novaccine$nonhospinfwvac[1:i])
                novaccine$suscwvacc[i]=populationsize-novaccine$hospinfcumulwvac[i]-novaccine$nonhospinfcumulwvac[i]-novaccine$effectivelycumulvacc[i]
            }
            
            
            ## month number
            novaccine$monthnum<-c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
            
            
            ## Overall infections with hypothetical vaccination campaign
            novaccine<-novaccine%>%
                mutate(overallinfwvac=hospinfwvac+nonhospinfwvac)%>%
                mutate(overallinfcumulwvacc=cumsum(overallinfwvac))
            
            ## Medically attended with hypothetical vaccination campaign
            novaccine<-novaccine%>%
                mutate(medicalattedtoinfwvac=overallinfwvac*mai)
            
            
            ######## Avoided events
            novaccine<-novaccine%>%
                mutate(AEhospinf=hospinfwovac-hospinfwvac)%>%
                mutate(AEnonhospinf=nonhospinfwovac-nonhospinfwvac)%>%
                mutate(AEmedicalattedtoinf=medicallyattendedinfwovac-medicalattedtoinfwvac)%>%
                mutate(AEoverallinf=overallinfwovac-overallinfwvac)
            
            
            
            ################ Prevented fraction
            ## Hospitalized infections
            ifelse (monthstartvac2()==1,
                    novaccine<-novaccine%>%
                        mutate(PFhospinf=sum(AEhospinf)/sum(hospinfwovac)),
                    novaccine<-novaccine%>%
                        mutate(PFhospinf=sum(AEhospinf[monthstartvac2():nrow(novaccine)]/sum(hospinfwovac[monthstartvac2():nrow(novaccine)])))
            )
            
            ## non-Hospitalized infections
            ifelse (monthstartvac2()==1,
                    novaccine<-novaccine%>%
                        mutate(PFnonhospinf=sum(AEnonhospinf)/sum(nonhospinfwovac)),
                    novaccine<-novaccine%>%
                        mutate(PFnonhospinf=sum(AEnonhospinf[monthstartvac2():nrow(novaccine)]/sum(nonhospinfwovac[monthstartvac2():nrow(novaccine)])))
            )
            
            
            
            
            ################ NNV -----------------------------------> why does this only go to month 8 in the Excel formula?????
            ## Hospitalized infections
            ifelse(monthstartvac2()==1,
                   novaccine<-novaccine%>%
                       mutate(NNVhospinf=1/((vaccineeffectiveness*(sum(hospinfwovac)))/populationsize)),
                   novaccine<-novaccine%>%
                       mutate(NNVhospinf=1/((vaccineeffectiveness*(sum(hospinfwovac[monthstartvac2():nrow(novaccine)])))/populationsize))
            )
            
            
            ## non-Hospitalized infections
            ifelse(monthstartvac2()==1,
                   novaccine<-novaccine%>%
                       mutate(NNVnonhospinf=1/((vaccineeffectiveness*(sum(nonhospinfwovac)))/populationsize)),
                   novaccine<-novaccine%>%
                       mutate(NNVnonhospinf=1/((vaccineeffectiveness*(sum(nonhospinfwovac[monthstartvac2():nrow(novaccine)])))/populationsize))
            )
            
            ## Medically attended infections
            ifelse(monthstartvac2()==1,
                   novaccine<-novaccine%>%
                       mutate(NNVmedicalattendedinf=1/((vaccineeffectiveness*(sum(medicallyattendedinfwovac)))/populationsize)),
                   novaccine<-novaccine%>%
                       mutate(NNVmedicalattendedinf=1/((vaccineeffectiveness*(sum(medicallyattendedinfwovac[monthstartvac2():nrow(novaccine)])))/populationsize))
            )
            
            ## Total infections
            ifelse(monthstartvac2()==1,
                   novaccine<-novaccine%>%
                       mutate(NNVoverallinf=1/((vaccineeffectiveness*(sum(hospinfwovac)+sum(nonhospinfwovac)))/populationsize)),
                   novaccine<-novaccine%>%
                       mutate(NNVoverallinf=1/((vaccineeffectiveness*(sum(hospinfwovac[monthstartvac2():nrow(novaccine)])+sum(nonhospinfwovac[monthstartvac2():nrow(novaccine)])))/populationsize))
            )
            
            return(novaccine)
            
            
        }#close "if hypothetical campaign"
        
    }) #close burdendataset
    
    
    
    ####################################################################################################
    ## List of burden averted results
    ####################################################################################################
    
    
    burdenavertedresultscoverage3timing2<-reactive({
        
        populationsize<-as.numeric(as.character(populationsize()))
        hnh<-as.numeric(as.character(hnh()))
        mai<-as.numeric(as.character(mai()))
        vaccineeffectiveness<-as.numeric(as.character(vaccineeffectiveness()))
        
        
        
        #################################################
        ## Results
        #################################################
        nonhospinfwovactot<-sum(burdendataset_coverage3timing2()$nonhospinfwovac)
        nonhospinfwvacctot<-sum(burdendataset_coverage3timing2()$nonhospinfwvac)
        nonhospinfaverted<-nonhospinfwovactot-nonhospinfwvacctot
        NNVnonhospinffinal<-burdendataset_coverage3timing2()$NNVnonhospinf[1]
        
        medicallyattendedinfwovactot<-sum(burdendataset_coverage3timing2()$medicallyattendedinfwovac)
        medicallyattendedinfwvactot<-sum(burdendataset_coverage3timing2()$medicalattedtoinfwvac)
        medicallyattendedinfaverted<-medicallyattendedinfwovactot-medicallyattendedinfwvactot
        NNVmedicallyattendtedfinal<-burdendataset_coverage3timing2()$NNVmedicalattendedinf[1]
        
        Totalinfwovacc<-sum(burdendataset_coverage3timing2()$overallinfwovac)
        Totalinfwvacc<-sum(burdendataset_coverage3timing2()$overallinfwvac)
        Totalinfaverted<-Totalinfwovacc-Totalinfwvacc
        NNVtotalinf<-burdendataset_coverage3timing2()$NNVoverallinf[1]
        
        hospinfwovactot<-sum(burdendataset_coverage3timing2()$hospinfwovac)
        hospinfwvacctot<-sum(burdendataset_coverage2timing2()$hospinfwvac)
        hospaverted<-hospinfwovactot-hospinfwvacctot
        NNVhospinffinal<-burdendataset_coverage3timing2()$NNVhospinf[1]
        
        
        nonhospPreventedfraction<-burdendataset_coverage3timing2()$PFnonhospinf[1]
        hospPreventedfraction<-burdendataset_coverage3timing2()$PFhospinf[1]
        
        
        indicator_results1<-c(nonhospinfwovactot, nonhospinfwvacctot, nonhospinfaverted, NNVnonhospinffinal,
                              Totalinfwovacc, Totalinfwvacc, Totalinfaverted, NNVtotalinf,
                              hospinfwovactot, hospinfwvacctot, hospaverted, NNVhospinffinal,
                              medicallyattendedinfwovactot, medicallyattendedinfwvactot, medicallyattendedinfaverted, NNVmedicallyattendtedfinal,
                              nonhospPreventedfraction)
        
        indicator_results_titles<-c("nonhospinfwovactot", "nonhospinfwvacctot", "nonhospinfaverted", "NNVnonhospinffinal",
                                    "Totalinfwovacc", "Totalinfwvacc", "Totalinfaverted", "NNVtotalinf",
                                    "hospinfwovactot", "hospinfwvacctot", "hospaverted", "NNVhospinffinal",
                                    "medicallyattendedinfwovactot", "medicallyattendedinfwvactot", "medicallyattendedinfaverted", "NNVmedicallyattendtedfinal",
                                    "nonhospPreventedfraction")
        
        indicator_results1<-cbind(indicator_results_titles, indicator_results1)
        
        
        return(indicator_results1)
        
        
    })#burdendataset_coverage3timing2
    
    
    
    
    
    #######################################################
    
    ##Deployment coverage 3, timing 3
    ######################################################
    
    
    burdendataset_coverage3timing3<- reactive({
        ################################ The output data when insufficient data has been entered   ##################################
        ############################################################################################################################
        
        
        if(scenario()=="Impact of current influenza vaccination campaign"){
            
            
            ##########################################################################################
            # Scenario of influenza circulation in presence of vaccine: Presence to absence of vaccine
            # This is a situation where the country conducts a vaccination campaign, we enter
            # actual burden inputs and vaccine coverage then compare this to a hypothetical scenario
            # where there is no vaccination campaign conducted
            ##########################################################################################
            
            
            
            
            hospinf<-c(hospitalizationsmonthly(), hospmonth2(), hospmonth3(), hospmonth4(), hospmonth5(), hospmonth6(),
                       hospmonth7(), hospmonth8(), hospmonth9(), hospmonth10(), hospmonth11(), hospmonth12())
            
            populationsize<-as.numeric(as.character(populationsize()))
            vaccineeffectiveness<-as.numeric(as.character(vaccineeffectiveness()))
            hnh<-as.numeric(as.character(hnh()))
            mai<-as.numeric(as.character(mai()))
            hospinf<-as.numeric(as.character(hospinf))
            
            novaccine<-hospinf%>%
                as.data.frame()%>%
                rename(hospinfwvac=1)%>%
                mutate(hospinfcumulwvac=cumsum(hospinfwvac))%>%
                mutate(nonhospinfwvac=hospinfwvac*hnh)%>%
                mutate(nonhospinfcumulwvac=cumsum(nonhospinfwvac))%>%
                mutate(overallinfwvac=hospinfwvac+nonhospinfwvac)%>%
                mutate(overallinfcumulwvac=cumsum(overallinfwvac))%>%
                mutate(medicalattedtoinfwvac=overallinfwvac*mai)%>%
                mutate(medicallyattendedinfcumulwvac=cumsum(medicalattedtoinfwvac))
            
            
            
            ##bind monthly coverage
            monthvaccov<-c(vaccinecoveragemonthly(), month2(), month3(), month4(), month5(), month6(),
                           month7(), month8(), month9(), month10(), month11(), month12())
            
            monthvaccov<-as.numeric(as.character(monthvaccov))
            
            totcoverage<-sum(monthvaccov)
            
            
            novaccine<-cbind(novaccine, monthvaccov)
            
            novaccine$covergdistrib<- c(vaccinecoveragemonthly3(), month23(), month33(), month43(), month53(), month63(), month73(), month83(), month93(), month103(), month113(), month123())
            
            novaccine$covergdistrib<-as.numeric(as.character(novaccine$covergdistrib))
            
            novaccine$monthvaccov<- novaccine$covergdistrib*coverage3() ## replace original monthvaccov column with the new monthly coverage - this is so that the variable doesn't have to be changed in the rest of the script
            
            
            ## month number
            novaccine$monthnum<-c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
            
            ## add monthly effectively vaccinated
            novaccine<-novaccine%>%
                mutate(effectivelyvacc=populationsize*monthvaccov*vaccineeffectiveness)%>%
                mutate(effectivelyvacccumul=cumsum(effectivelyvacc))
            
            
            
            
            #####################################################################
            # Estimated rates
            #####################################################################
            novaccine<-novaccine%>%
                mutate(susceptiblewvacc=populationsize-hospinfcumulwvac-nonhospinfcumulwvac-effectivelyvacccumul)
            
            #Create columns
            novaccine$irhospinf<-NA
            novaccine$irnonhospinf<-NA
            
            #Create first row
            novaccine[1,14]<-novaccine$hospinfwvac[1]/populationsize
            novaccine[1,15]<-novaccine$nonhospinfwvac[1]/populationsize
            
            
            ##Calculation uses last month's susceptible population
            for(i in 2:nrow(novaccine)){
                novaccine$irhospinf[i]<-novaccine$hospinfwvac[i]/novaccine$susceptiblewvacc[i-1]
                novaccine$irnonhospinf[i]<- novaccine$nonhospinfwvac[i]/novaccine$susceptiblewvacc[i-1]
            }
            
            #####################################################################
            # Scenario of influenza circulation in absence of vaccine
            #####################################################################
            
            ## These calculations are initialized by the 'Susceptible at start of month in absence of vaccine'
            ## column, which starts with the Target Population in the first month.
            
            remainsuscstartmonth<-c(populationsize, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
            novaccine<-cbind(novaccine, remainsuscstartmonth)
            
            #Create columns
            novaccine$hospinfwovac<-NA
            novaccine$hospinfcumulwovacc<-NA
            novaccine$nonhospinfwovac<-NA
            novaccine$nonhospinfcumulwovacc<-NA
            novaccine$remainingsusc<-NA
            
            #Create first row
            novaccine[1,17]<-novaccine$remainsuscstartmonth[1]*novaccine$irhospinf[1]
            novaccine[1,18]<-novaccine$hospinfwovac[1]
            novaccine[1,19]<-novaccine$remainsuscstartmonth[1]*novaccine$irnonhospinf[1]
            novaccine[1,20]<-novaccine$nonhospinfwovac[1]
            novaccine[1,21]<-populationsize-novaccine$hospinfcumulwovacc[1]-novaccine$nonhospinfcumulwovacc[1]
            
            
            #Create rows 2:12
            for(i in 2:nrow(novaccine)){
                novaccine$remainsuscstartmonth[i]<-novaccine$remainingsusc[i-1]
                novaccine$hospinfwovac[i]<-novaccine$remainsuscstartmonth[i]*novaccine$irhospinf[i]
                novaccine$hospinfcumulwovacc[i]<-sum(novaccine$hospinfwovac[1:i])
                novaccine$nonhospinfwovac[i]<-novaccine$remainsuscstartmonth[i]*novaccine$irnonhospinf[i]
                novaccine$nonhospinfcumulwovacc[i]<-sum(novaccine$nonhospinfwovac[1:i])
                novaccine$remainingsusc[i]<-populationsize-novaccine$hospinfcumulwovacc[i]-novaccine$nonhospinfcumulwovacc[i]
            }
            
            novaccine<-novaccine%>%
                mutate(overallinfwovac=hospinfwovac+nonhospinfwovac)%>%
                mutate(overallinfcumulwovacc=cumsum(overallinfwovac))%>%
                mutate(medicallyattendedinfwovac=overallinfwovac*mai)%>%
                mutate(medicalattedtoinfcumulwovacc=cumsum(medicallyattendedinfwovac))
            
            ######## Avoided events
            novaccine<-novaccine%>%
                mutate(AEhospinf=hospinfwovac-hospinfwvac)%>%
                mutate(AEnonhospinf=nonhospinfwovac-nonhospinfwvac)%>%
                mutate(AEmedicalattedtoinf=medicallyattendedinfwovac-medicalattedtoinfwvac)%>%
                mutate(AEoverallinf=overallinfwovac-overallinfwvac)
            
            
            ################ Prevented fraction
            ## Hospitalized infections
            ifelse (monthstartvac3()==1,
                    novaccine<-novaccine%>%
                        mutate(PFhospinf=sum(AEhospinf)/sum(hospinfwovac)),
                    novaccine<-novaccine%>%
                        mutate(PFhospinf=sum(AEhospinf[monthstartvac3():nrow(novaccine)]/sum(hospinfwovac[monthstartvac3():nrow(novaccine)])))
            )
            
            ## non-Hospitalized infections
            ifelse (monthstartvac3()==1,
                    novaccine<-novaccine%>%
                        mutate(PFnonhospinf=sum(AEnonhospinf)/sum(nonhospinfwovac)),
                    novaccine<-novaccine%>%
                        mutate(PFnonhospinf=sum(AEnonhospinf[monthstartvac3():nrow(novaccine)]/sum(nonhospinfwovac[monthstartvac3():nrow(novaccine)])))
            )
            
            
            ################ NNV -----------------------------------> why does this only go to month 8 in the Excel formula?????
            ## Hospitalized infections
            ifelse(monthstartvac3()==1,
                   novaccine<-novaccine%>%
                       mutate(NNVhospinf=1/((vaccineeffectiveness*(sum(hospinfwovac)))/populationsize)),
                   novaccine<-novaccine%>%
                       mutate(NNVhospinf=1/((vaccineeffectiveness*(sum(hospinfwovac[monthstartvac3():nrow(novaccine)])))/populationsize))
            )
            
            
            ## non-Hospitalized infections
            ifelse(monthstartvac3()==1,
                   novaccine<-novaccine%>%
                       mutate(NNVnonhospinf=1/((vaccineeffectiveness*(sum(nonhospinfwovac)))/populationsize)),
                   novaccine<-novaccine%>%
                       mutate(NNVnonhospinf=1/((vaccineeffectiveness*(sum(nonhospinfwovac[monthstartvac3():nrow(novaccine)])))/populationsize))
            )
            
            ## Medically infections
            ifelse(monthstartvac3()==1,
                   novaccine<-novaccine%>%
                       mutate(NNVmedicalattendedinf=1/((vaccineeffectiveness*(sum(medicallyattendedinfwovac)))/populationsize)),
                   novaccine<-novaccine%>%
                       mutate(NNVmedicalattendedinf=1/((vaccineeffectiveness*(sum(medicallyattendedinfwovac[monthstartvac3():nrow(novaccine)])))/populationsize))
            )
            
            ## Overall infections
            ifelse(monthstartvac3()==1,
                   novaccine<-novaccine%>%
                       mutate(NNVoverallinf=1/((vaccineeffectiveness*(sum(hospinfwovac)+sum(nonhospinfwovac)))/populationsize)),
                   novaccine<-novaccine%>%
                       mutate(NNVoverallinf=1/((vaccineeffectiveness*(sum(hospinfwovac[monthstartvac3():nrow(novaccine)])+sum(nonhospinfwovac[monthstartvac3():nrow(novaccine)])))/populationsize))
            )
            
            return(novaccine)
            
            
            
        } #close if current campaign
        
        
        
        if(scenario()=="Impact of hypothetical vaccination campaign"){
            
            ##########################################################################################
            # Scenario of influenza circulation in absence of vaccine: Absence to presence of vaccine
            # This is a scenario where the country has yet to conduct a vaccination campaign but would
            # like to see what impact a vaccine campaign may have.
            # The country enters the estimated number of hospitalization they observed and then we compare
            # this to a hypothetical vaccination campaign
            ##########################################################################################
            
            hospinf<-c(hospitalizationsmonthly(), hospmonth2(), hospmonth3(), hospmonth4(), hospmonth5(), hospmonth6(),
                       hospmonth7(), hospmonth8(), hospmonth9(), hospmonth10(), hospmonth11(), hospmonth12())
            
            populationsize<-as.numeric(as.character(populationsize()))
            hnh<-as.numeric(as.character(hnh()))
            mai<-as.numeric(as.character(mai()))
            hospinf<-as.numeric(as.character(hospinf))
            vaccineeffectiveness<-as.numeric(as.character(vaccineeffectiveness()))
            
            #####################Observed influenza in the absence of vaccine
            novaccine<-hospinf%>%
                as.data.frame()%>%
                rename(hospinfwovac=1)%>%
                mutate(hospinfcumulwovac=cumsum(hospinfwovac))%>%
                mutate(nonhospinfwovac=hospinfwovac*hnh)%>%
                mutate(nonhospinfcumulwovac=cumsum(nonhospinfwovac))%>%
                mutate(remainingsusc=populationsize-hospinfcumulwovac-nonhospinfcumulwovac)
            
            #create remainsuscstartmonth column with target population and rows 1:11 of remainingsusc
            remainsuscstartmonth<-populationsize%>%
                as.data.frame()%>%
                rename(remainsuscstartmonth=1)
            
            remainsuscstartmonth2<-as.data.frame(novaccine$remainingsusc[1:11])%>%
                as.data.frame()%>%
                rename(remainsuscstartmonth=1)
            
            remainsuscstartmonth<-rbind(remainsuscstartmonth, remainsuscstartmonth2)
            
            novaccine<-cbind(novaccine, remainsuscstartmonth)
            
            novaccine<-novaccine%>%
                mutate(overallinfwovac=hospinfwovac+nonhospinfwovac)%>%
                mutate(overallinfcumulwovac=cumsum(overallinfwovac))%>%
                mutate(medicallyattendedinfwovac=overallinfwovac*mai)%>%
                mutate(medicallyattendedinfcumulwovac=cumsum(medicallyattendedinfwovac))
            
            
            
            #####################################################################
            # Incidence rates in the absence of vaccine
            #####################################################################
            novaccine<-novaccine%>%
                mutate(irhospinf=hospinfwovac/remainsuscstartmonth)%>%
                mutate(irnonhospinf=nonhospinfwovac/remainsuscstartmonth)
            
            
            
            
            ############################ Burden inputs with hypothetical vaccination campaign
            #Create columns
            novaccine$hospinfwvac<-NA
            novaccine$hospinfcumulwvac<-NA
            novaccine$nonhospinfwvac<-NA
            novaccine$nonhospinfcumulwvac<-NA
            
            
            
            #Create first row
            novaccine[1,13]<-populationsize*novaccine$irhospinf[1]
            novaccine[1,14]<-novaccine$hospinfwvac[1]
            novaccine[1,15]<-populationsize*novaccine$irnonhospinf[1]
            novaccine[1,16]<-novaccine$nonhospinfwvac[1]
            
            ##bind monthly coverage
            monthvaccov<-c(vaccinecoveragemonthly(), month2(), month3(), month4(), month5(), month6(),
                           month7(), month8(), month9(), month10(), month11(), month12())
            
            monthvaccov<-as.numeric(as.character(monthvaccov))
            
            totcoverage<-sum(monthvaccov)
            
            
            novaccine<-cbind(novaccine, monthvaccov)
            
            novaccine$covergdistrib<- c(vaccinecoveragemonthly3(), month23(), month33(), month43(), month53(), month63(), month73(), month83(), month93(), month103(), month113(), month123())
            
            novaccine$covergdistrib<-as.numeric(as.character(novaccine$covergdistrib))
            
            novaccine$monthvaccov<- novaccine$covergdistrib*coverage3() ## replace original monthvaccov column with the new monthly coverage - this is so that the variable doesn't have to be changed in the rest of the script
            
            
            ##monthly effectively vaccinated and susceptible with hypothetical vaccination campaign
            novaccine<-novaccine%>%
                mutate(effectivelyvacc=populationsize*monthvaccov*vaccineeffectiveness)%>%
                mutate(effectivelycumulvacc=cumsum(effectivelyvacc))%>%
                mutate(suscwvacc=populationsize-hospinfcumulwvac-nonhospinfcumulwvac-effectivelyvacc)
            
            ##Then calculate the rest of hospinfwovac and nonhospinfwovac. Calculation uses last month's incidence rate
            for(i in 2:nrow(novaccine)){
                novaccine$hospinfwvac[i]<-novaccine$suscwvacc[i-1]*novaccine$irhospinf[i]
                novaccine$hospinfcumulwvac[i]<-sum(novaccine$hospinfwvac[1:i])
                novaccine$nonhospinfwvac[i]<- novaccine$suscwvacc[i-1]*novaccine$irnonhospinf[i]
                novaccine$nonhospinfcumulwvac[i]<-sum(novaccine$nonhospinfwvac[1:i])
                novaccine$suscwvacc[i]=populationsize-novaccine$hospinfcumulwvac[i]-novaccine$nonhospinfcumulwvac[i]-novaccine$effectivelycumulvacc[i]
            }
            
            
            ## month number
            novaccine$monthnum<-c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
            
            
            ## Overall infections with hypothetical vaccination campaign
            novaccine<-novaccine%>%
                mutate(overallinfwvac=hospinfwvac+nonhospinfwvac)%>%
                mutate(overallinfcumulwvacc=cumsum(overallinfwvac))
            
            ## Medically attended with hypothetical vaccination campaign
            novaccine<-novaccine%>%
                mutate(medicalattedtoinfwvac=overallinfwvac*mai)
            
            
            ######## Avoided events
            novaccine<-novaccine%>%
                mutate(AEhospinf=hospinfwovac-hospinfwvac)%>%
                mutate(AEnonhospinf=nonhospinfwovac-nonhospinfwvac)%>%
                mutate(AEmedicalattedtoinf=medicallyattendedinfwovac-medicalattedtoinfwvac)%>%
                mutate(AEoverallinf=overallinfwovac-overallinfwvac)
            
            
            
            ################ Prevented fraction
            ## Hospitalized infections
            ifelse (monthstartvac3()==1,
                    novaccine<-novaccine%>%
                        mutate(PFhospinf=sum(AEhospinf)/sum(hospinfwovac)),
                    novaccine<-novaccine%>%
                        mutate(PFhospinf=sum(AEhospinf[monthstartvac3():nrow(novaccine)]/sum(hospinfwovac[monthstartvac3():nrow(novaccine)])))
            )
            
            ## non-Hospitalized infections
            ifelse (monthstartvac3()==1,
                    novaccine<-novaccine%>%
                        mutate(PFnonhospinf=sum(AEnonhospinf)/sum(nonhospinfwovac)),
                    novaccine<-novaccine%>%
                        mutate(PFnonhospinf=sum(AEnonhospinf[monthstartvac3():nrow(novaccine)]/sum(nonhospinfwovac[monthstartvac3():nrow(novaccine)])))
            )
            
            
            
            
            ################ NNV -----------------------------------> why does this only go to month 8 in the Excel formula?????
            ## Hospitalized infections
            ifelse(monthstartvac3()==1,
                   novaccine<-novaccine%>%
                       mutate(NNVhospinf=1/((vaccineeffectiveness*(sum(hospinfwovac)))/populationsize)),
                   novaccine<-novaccine%>%
                       mutate(NNVhospinf=1/((vaccineeffectiveness*(sum(hospinfwovac[monthstartvac3():nrow(novaccine)])))/populationsize))
            )
            
            
            ## non-Hospitalized infections
            ifelse(monthstartvac3()==1,
                   novaccine<-novaccine%>%
                       mutate(NNVnonhospinf=1/((vaccineeffectiveness*(sum(nonhospinfwovac)))/populationsize)),
                   novaccine<-novaccine%>%
                       mutate(NNVnonhospinf=1/((vaccineeffectiveness*(sum(nonhospinfwovac[monthstartvac3():nrow(novaccine)])))/populationsize))
            )
            
            ## Medically attended infections
            ifelse(monthstartvac3()==1,
                   novaccine<-novaccine%>%
                       mutate(NNVmedicalattendedinf=1/((vaccineeffectiveness*(sum(medicallyattendedinfwovac)))/populationsize)),
                   novaccine<-novaccine%>%
                       mutate(NNVmedicalattendedinf=1/((vaccineeffectiveness*(sum(medicallyattendedinfwovac[monthstartvac3():nrow(novaccine)])))/populationsize))
            )
            
            ## Total infections
            ifelse(monthstartvac3()==1,
                   novaccine<-novaccine%>%
                       mutate(NNVoverallinf=1/((vaccineeffectiveness*(sum(hospinfwovac)+sum(nonhospinfwovac)))/populationsize)),
                   novaccine<-novaccine%>%
                       mutate(NNVoverallinf=1/((vaccineeffectiveness*(sum(hospinfwovac[monthstartvac3():nrow(novaccine)])+sum(nonhospinfwovac[monthstartvac3():nrow(novaccine)])))/populationsize))
            )
            
            return(novaccine)
            
            
        }#close "if hypothetical campaign"
        
    }) #close burdendataset
    
    
    
    ####################################################################################################
    ## List of burden averted results
    ####################################################################################################
    
    
    burdenavertedresultscoverage3timing3<-reactive({
        
        populationsize<-as.numeric(as.character(populationsize()))
        hnh<-as.numeric(as.character(hnh()))
        mai<-as.numeric(as.character(mai()))
        vaccineeffectiveness<-as.numeric(as.character(vaccineeffectiveness()))
        
        
        
        #################################################
        ## Results
        #################################################
        nonhospinfwovactot<-sum(burdendataset_coverage3timing3()$nonhospinfwovac)
        nonhospinfwvacctot<-sum(burdendataset_coverage3timing3()$nonhospinfwvac)
        nonhospinfaverted<-nonhospinfwovactot-nonhospinfwvacctot
        NNVnonhospinffinal<-burdendataset_coverage3timing3()$NNVnonhospinf[1]
        
        medicallyattendedinfwovactot<-sum(burdendataset_coverage3timing3()$medicallyattendedinfwovac)
        medicallyattendedinfwvactot<-sum(burdendataset_coverage3timing3()$medicalattedtoinfwvac)
        medicallyattendedinfaverted<-medicallyattendedinfwovactot-medicallyattendedinfwvactot
        NNVmedicallyattendtedfinal<-burdendataset_coverage3timing3()$NNVmedicalattendedinf[1]
        
        Totalinfwovacc<-sum(burdendataset_coverage3timing3()$overallinfwovac)
        Totalinfwvacc<-sum(burdendataset_coverage3timing3()$overallinfwvac)
        Totalinfaverted<-Totalinfwovacc-Totalinfwvacc
        NNVtotalinf<-burdendataset_coverage3timing3()$NNVoverallinf[1]
        
        hospinfwovactot<-sum(burdendataset_coverage3timing3()$hospinfwovac)
        hospinfwvacctot<-sum(burdendataset_coverage3timing3()$hospinfwvac)
        hospaverted<-hospinfwovactot-hospinfwvacctot
        NNVhospinffinal<-burdendataset_coverage3timing3()$NNVhospinf[1]
        
        
        nonhospPreventedfraction<-burdendataset_coverage3timing3()$PFnonhospinf[1]
        hospPreventedfraction<-burdendataset_coverage3timing3()$PFhospinf[1]
        
        
        indicator_results1<-c(nonhospinfwovactot, nonhospinfwvacctot, nonhospinfaverted, NNVnonhospinffinal,
                              Totalinfwovacc, Totalinfwvacc, Totalinfaverted, NNVtotalinf,
                              hospinfwovactot, hospinfwvacctot, hospaverted, NNVhospinffinal,
                              medicallyattendedinfwovactot, medicallyattendedinfwvactot, medicallyattendedinfaverted, NNVmedicallyattendtedfinal,
                              nonhospPreventedfraction)
        
        indicator_results_titles<-c("nonhospinfwovactot", "nonhospinfwvacctot", "nonhospinfaverted", "NNVnonhospinffinal",
                                    "Totalinfwovacc", "Totalinfwvacc", "Totalinfaverted", "NNVtotalinf",
                                    "hospinfwovactot", "hospinfwvacctot", "hospaverted", "NNVhospinffinal",
                                    "medicallyattendedinfwovactot", "medicallyattendedinfwvactot", "medicallyattendedinfaverted", "NNVmedicallyattendtedfinal",
                                    "nonhospPreventedfraction")
        
        indicator_results1<-cbind(indicator_results_titles, indicator_results1)
        
        
        return(indicator_results1)
        
        
    })#burdendataset_coverage3timing3
    
    
    
    
    #######################################################
    
    ##Deployment coverage 3, timing 4
    ######################################################
    
    
    burdendataset_coverage3timing4<- reactive({
        ################################ The output data when insufficient data has been entered   ##################################
        ############################################################################################################################
        
        
        if(scenario()=="Impact of current influenza vaccination campaign"){
            
            
            ##########################################################################################
            # Scenario of influenza circulation in presence of vaccine: Presence to absence of vaccine
            # This is a situation where the country conducts a vaccination campaign, we enter
            # actual burden inputs and vaccine coverage then compare this to a hypothetical scenario
            # where there is no vaccination campaign conducted
            ##########################################################################################
            
            
            
            
            hospinf<-c(hospitalizationsmonthly(), hospmonth2(), hospmonth3(), hospmonth4(), hospmonth5(), hospmonth6(),
                       hospmonth7(), hospmonth8(), hospmonth9(), hospmonth10(), hospmonth11(), hospmonth12())
            
            populationsize<-as.numeric(as.character(populationsize()))
            vaccineeffectiveness<-as.numeric(as.character(vaccineeffectiveness()))
            hnh<-as.numeric(as.character(hnh()))
            mai<-as.numeric(as.character(mai()))
            hospinf<-as.numeric(as.character(hospinf))
            
            novaccine<-hospinf%>%
                as.data.frame()%>%
                rename(hospinfwvac=1)%>%
                mutate(hospinfcumulwvac=cumsum(hospinfwvac))%>%
                mutate(nonhospinfwvac=hospinfwvac*hnh)%>%
                mutate(nonhospinfcumulwvac=cumsum(nonhospinfwvac))%>%
                mutate(overallinfwvac=hospinfwvac+nonhospinfwvac)%>%
                mutate(overallinfcumulwvac=cumsum(overallinfwvac))%>%
                mutate(medicalattedtoinfwvac=overallinfwvac*mai)%>%
                mutate(medicallyattendedinfcumulwvac=cumsum(medicalattedtoinfwvac))
            
            
            
            ##bind monthly coverage
            monthvaccov<-c(vaccinecoveragemonthly(), month2(), month3(), month4(), month5(), month6(),
                           month7(), month8(), month9(), month10(), month11(), month12())
            
            monthvaccov<-as.numeric(as.character(monthvaccov))
            
            totcoverage<-sum(monthvaccov)
            
            
            novaccine<-cbind(novaccine, monthvaccov)
            
            novaccine$covergdistrib<- c(vaccinecoveragemonthly4(), month24(), month34(), month44(), month54(), month64(), month74(), month84(), month94(), month104(), month114(), month124())
            
            novaccine$covergdistrib<-as.numeric(as.character(novaccine$covergdistrib))
            
            novaccine$monthvaccov<- novaccine$covergdistrib*coverage3() ## replace original monthvaccov column with the new monthly coverage - this is so that the variable doesn't have to be changed in the rest of the script
            
            
            ## month number
            novaccine$monthnum<-c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
            
            ## add monthly effectively vaccinated
            novaccine<-novaccine%>%
                mutate(effectivelyvacc=populationsize*monthvaccov*vaccineeffectiveness)%>%
                mutate(effectivelyvacccumul=cumsum(effectivelyvacc))
            
            
            
            
            #####################################################################
            # Estimated rates
            #####################################################################
            novaccine<-novaccine%>%
                mutate(susceptiblewvacc=populationsize-hospinfcumulwvac-nonhospinfcumulwvac-effectivelyvacccumul)
            
            #Create columns
            novaccine$irhospinf<-NA
            novaccine$irnonhospinf<-NA
            
            #Create first row
            novaccine[1,14]<-novaccine$hospinfwvac[1]/populationsize
            novaccine[1,15]<-novaccine$nonhospinfwvac[1]/populationsize
            
            
            ##Calculation uses last month's susceptible population
            for(i in 2:nrow(novaccine)){
                novaccine$irhospinf[i]<-novaccine$hospinfwvac[i]/novaccine$susceptiblewvacc[i-1]
                novaccine$irnonhospinf[i]<- novaccine$nonhospinfwvac[i]/novaccine$susceptiblewvacc[i-1]
            }
            
            #####################################################################
            # Scenario of influenza circulation in absence of vaccine
            #####################################################################
            
            ## These calculations are initialized by the 'Susceptible at start of month in absence of vaccine'
            ## column, which starts with the Target Population in the first month.
            
            remainsuscstartmonth<-c(populationsize, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
            novaccine<-cbind(novaccine, remainsuscstartmonth)
            
            #Create columns
            novaccine$hospinfwovac<-NA
            novaccine$hospinfcumulwovacc<-NA
            novaccine$nonhospinfwovac<-NA
            novaccine$nonhospinfcumulwovacc<-NA
            novaccine$remainingsusc<-NA
            
            #Create first row
            novaccine[1,17]<-novaccine$remainsuscstartmonth[1]*novaccine$irhospinf[1]
            novaccine[1,18]<-novaccine$hospinfwovac[1]
            novaccine[1,19]<-novaccine$remainsuscstartmonth[1]*novaccine$irnonhospinf[1]
            novaccine[1,20]<-novaccine$nonhospinfwovac[1]
            novaccine[1,21]<-populationsize-novaccine$hospinfcumulwovacc[1]-novaccine$nonhospinfcumulwovacc[1]
            
            
            #Create rows 2:12
            for(i in 2:nrow(novaccine)){
                novaccine$remainsuscstartmonth[i]<-novaccine$remainingsusc[i-1]
                novaccine$hospinfwovac[i]<-novaccine$remainsuscstartmonth[i]*novaccine$irhospinf[i]
                novaccine$hospinfcumulwovacc[i]<-sum(novaccine$hospinfwovac[1:i])
                novaccine$nonhospinfwovac[i]<-novaccine$remainsuscstartmonth[i]*novaccine$irnonhospinf[i]
                novaccine$nonhospinfcumulwovacc[i]<-sum(novaccine$nonhospinfwovac[1:i])
                novaccine$remainingsusc[i]<-populationsize-novaccine$hospinfcumulwovacc[i]-novaccine$nonhospinfcumulwovacc[i]
            }
            
            novaccine<-novaccine%>%
                mutate(overallinfwovac=hospinfwovac+nonhospinfwovac)%>%
                mutate(overallinfcumulwovacc=cumsum(overallinfwovac))%>%
                mutate(medicallyattendedinfwovac=overallinfwovac*mai)%>%
                mutate(medicalattedtoinfcumulwovacc=cumsum(medicallyattendedinfwovac))
            
            ######## Avoided events
            novaccine<-novaccine%>%
                mutate(AEhospinf=hospinfwovac-hospinfwvac)%>%
                mutate(AEnonhospinf=nonhospinfwovac-nonhospinfwvac)%>%
                mutate(AEmedicalattedtoinf=medicallyattendedinfwovac-medicalattedtoinfwvac)%>%
                mutate(AEoverallinf=overallinfwovac-overallinfwvac)
            
            
            ################ Prevented fraction
            ## Hospitalized infections
            ifelse (monthstartvac4()==1,
                    novaccine<-novaccine%>%
                        mutate(PFhospinf=sum(AEhospinf)/sum(hospinfwovac)),
                    novaccine<-novaccine%>%
                        mutate(PFhospinf=sum(AEhospinf[monthstartvac4():nrow(novaccine)]/sum(hospinfwovac[monthstartvac4():nrow(novaccine)])))
            )
            
            ## non-Hospitalized infections
            ifelse (monthstartvac4()==1,
                    novaccine<-novaccine%>%
                        mutate(PFnonhospinf=sum(AEnonhospinf)/sum(nonhospinfwovac)),
                    novaccine<-novaccine%>%
                        mutate(PFnonhospinf=sum(AEnonhospinf[monthstartvac4():nrow(novaccine)]/sum(nonhospinfwovac[monthstartvac4():nrow(novaccine)])))
            )
            
            
            ################ NNV -----------------------------------> why does this only go to month 8 in the Excel formula?????
            ## Hospitalized infections
            ifelse(monthstartvac4()==1,
                   novaccine<-novaccine%>%
                       mutate(NNVhospinf=1/((vaccineeffectiveness*(sum(hospinfwovac)))/populationsize)),
                   novaccine<-novaccine%>%
                       mutate(NNVhospinf=1/((vaccineeffectiveness*(sum(hospinfwovac[monthstartvac4():nrow(novaccine)])))/populationsize))
            )
            
            
            ## non-Hospitalized infections
            ifelse(monthstartvac4()==1,
                   novaccine<-novaccine%>%
                       mutate(NNVnonhospinf=1/((vaccineeffectiveness*(sum(nonhospinfwovac)))/populationsize)),
                   novaccine<-novaccine%>%
                       mutate(NNVnonhospinf=1/((vaccineeffectiveness*(sum(nonhospinfwovac[monthstartvac4():nrow(novaccine)])))/populationsize))
            )
            
            ## Medically infections
            ifelse(monthstartvac4()==1,
                   novaccine<-novaccine%>%
                       mutate(NNVmedicalattendedinf=1/((vaccineeffectiveness*(sum(medicallyattendedinfwovac)))/populationsize)),
                   novaccine<-novaccine%>%
                       mutate(NNVmedicalattendedinf=1/((vaccineeffectiveness*(sum(medicallyattendedinfwovac[monthstartvac4():nrow(novaccine)])))/populationsize))
            )
            
            ## Overall infections
            ifelse(monthstartvac4()==1,
                   novaccine<-novaccine%>%
                       mutate(NNVoverallinf=1/((vaccineeffectiveness*(sum(hospinfwovac)+sum(nonhospinfwovac)))/populationsize)),
                   novaccine<-novaccine%>%
                       mutate(NNVoverallinf=1/((vaccineeffectiveness*(sum(hospinfwovac[monthstartvac4():nrow(novaccine)])+sum(nonhospinfwovac[monthstartvac4():nrow(novaccine)])))/populationsize))
            )
            
            return(novaccine)
            
            
            
        } #close if current campaign
        
        
        
        if(scenario()=="Impact of hypothetical vaccination campaign"){
            
            ##########################################################################################
            # Scenario of influenza circulation in absence of vaccine: Absence to presence of vaccine
            # This is a scenario where the country has yet to conduct a vaccination campaign but would
            # like to see what impact a vaccine campaign may have.
            # The country enters the estimated number of hospitalization they observed and then we compare
            # this to a hypothetical vaccination campaign
            ##########################################################################################
            
            hospinf<-c(hospitalizationsmonthly(), hospmonth2(), hospmonth3(), hospmonth4(), hospmonth5(), hospmonth6(),
                       hospmonth7(), hospmonth8(), hospmonth9(), hospmonth10(), hospmonth11(), hospmonth12())
            
            populationsize<-as.numeric(as.character(populationsize()))
            hnh<-as.numeric(as.character(hnh()))
            mai<-as.numeric(as.character(mai()))
            hospinf<-as.numeric(as.character(hospinf))
            vaccineeffectiveness<-as.numeric(as.character(vaccineeffectiveness()))
            
            #####################Observed influenza in the absence of vaccine
            novaccine<-hospinf%>%
                as.data.frame()%>%
                rename(hospinfwovac=1)%>%
                mutate(hospinfcumulwovac=cumsum(hospinfwovac))%>%
                mutate(nonhospinfwovac=hospinfwovac*hnh)%>%
                mutate(nonhospinfcumulwovac=cumsum(nonhospinfwovac))%>%
                mutate(remainingsusc=populationsize-hospinfcumulwovac-nonhospinfcumulwovac)
            
            #create remainsuscstartmonth column with target population and rows 1:11 of remainingsusc
            remainsuscstartmonth<-populationsize%>%
                as.data.frame()%>%
                rename(remainsuscstartmonth=1)
            
            remainsuscstartmonth2<-as.data.frame(novaccine$remainingsusc[1:11])%>%
                as.data.frame()%>%
                rename(remainsuscstartmonth=1)
            
            remainsuscstartmonth<-rbind(remainsuscstartmonth, remainsuscstartmonth2)
            
            novaccine<-cbind(novaccine, remainsuscstartmonth)
            
            novaccine<-novaccine%>%
                mutate(overallinfwovac=hospinfwovac+nonhospinfwovac)%>%
                mutate(overallinfcumulwovac=cumsum(overallinfwovac))%>%
                mutate(medicallyattendedinfwovac=overallinfwovac*mai)%>%
                mutate(medicallyattendedinfcumulwovac=cumsum(medicallyattendedinfwovac))
            
            
            
            #####################################################################
            # Incidence rates in the absence of vaccine
            #####################################################################
            novaccine<-novaccine%>%
                mutate(irhospinf=hospinfwovac/remainsuscstartmonth)%>%
                mutate(irnonhospinf=nonhospinfwovac/remainsuscstartmonth)
            
            
            
            
            ############################ Burden inputs with hypothetical vaccination campaign
            #Create columns
            novaccine$hospinfwvac<-NA
            novaccine$hospinfcumulwvac<-NA
            novaccine$nonhospinfwvac<-NA
            novaccine$nonhospinfcumulwvac<-NA
            
            
            
            #Create first row
            novaccine[1,13]<-populationsize*novaccine$irhospinf[1]
            novaccine[1,14]<-novaccine$hospinfwvac[1]
            novaccine[1,15]<-populationsize*novaccine$irnonhospinf[1]
            novaccine[1,16]<-novaccine$nonhospinfwvac[1]
            
            ##bind monthly coverage
            monthvaccov<-c(vaccinecoveragemonthly(), month2(), month3(), month4(), month5(), month6(),
                           month7(), month8(), month9(), month10(), month11(), month12())
            
            monthvaccov<-as.numeric(as.character(monthvaccov))
            
            totcoverage<-sum(monthvaccov)
            
            
            novaccine<-cbind(novaccine, monthvaccov)
            
            novaccine$covergdistrib<- c(vaccinecoveragemonthly4(), month24(), month34(), month44(), month54(), month64(), month74(), month84(), month94(), month104(), month114(), month124())
            
            novaccine$covergdistrib<-as.numeric(as.character(novaccine$covergdistrib))
            
            novaccine$monthvaccov<- novaccine$covergdistrib*coverage3() ## replace original monthvaccov column with the new monthly coverage - this is so that the variable doesn't have to be changed in the rest of the script
            
            
            ##monthly effectively vaccinated and susceptible with hypothetical vaccination campaign
            novaccine<-novaccine%>%
                mutate(effectivelyvacc=populationsize*monthvaccov*vaccineeffectiveness)%>%
                mutate(effectivelycumulvacc=cumsum(effectivelyvacc))%>%
                mutate(suscwvacc=populationsize-hospinfcumulwvac-nonhospinfcumulwvac-effectivelyvacc)
            
            ##Then calculate the rest of hospinfwovac and nonhospinfwovac. Calculation uses last month's incidence rate
            for(i in 2:nrow(novaccine)){
                novaccine$hospinfwvac[i]<-novaccine$suscwvacc[i-1]*novaccine$irhospinf[i]
                novaccine$hospinfcumulwvac[i]<-sum(novaccine$hospinfwvac[1:i])
                novaccine$nonhospinfwvac[i]<- novaccine$suscwvacc[i-1]*novaccine$irnonhospinf[i]
                novaccine$nonhospinfcumulwvac[i]<-sum(novaccine$nonhospinfwvac[1:i])
                novaccine$suscwvacc[i]=populationsize-novaccine$hospinfcumulwvac[i]-novaccine$nonhospinfcumulwvac[i]-novaccine$effectivelycumulvacc[i]
            }
            
            
            ## month number
            novaccine$monthnum<-c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
            
            
            ## Overall infections with hypothetical vaccination campaign
            novaccine<-novaccine%>%
                mutate(overallinfwvac=hospinfwvac+nonhospinfwvac)%>%
                mutate(overallinfcumulwvacc=cumsum(overallinfwvac))
            
            ## Medically attended with hypothetical vaccination campaign
            novaccine<-novaccine%>%
                mutate(medicalattedtoinfwvac=overallinfwvac*mai)
            
            
            ######## Avoided events
            novaccine<-novaccine%>%
                mutate(AEhospinf=hospinfwovac-hospinfwvac)%>%
                mutate(AEnonhospinf=nonhospinfwovac-nonhospinfwvac)%>%
                mutate(AEmedicalattedtoinf=medicallyattendedinfwovac-medicalattedtoinfwvac)%>%
                mutate(AEoverallinf=overallinfwovac-overallinfwvac)
            
            
            
            ################ Prevented fraction
            ## Hospitalized infections
            ifelse (monthstartvac4()==1,
                    novaccine<-novaccine%>%
                        mutate(PFhospinf=sum(AEhospinf)/sum(hospinfwovac)),
                    novaccine<-novaccine%>%
                        mutate(PFhospinf=sum(AEhospinf[monthstartvac4():nrow(novaccine)]/sum(hospinfwovac[monthstartvac4():nrow(novaccine)])))
            )
            
            ## non-Hospitalized infections
            ifelse (monthstartvac4()==1,
                    novaccine<-novaccine%>%
                        mutate(PFnonhospinf=sum(AEnonhospinf)/sum(nonhospinfwovac)),
                    novaccine<-novaccine%>%
                        mutate(PFnonhospinf=sum(AEnonhospinf[monthstartvac4():nrow(novaccine)]/sum(nonhospinfwovac[monthstartvac4():nrow(novaccine)])))
            )
            
            
            
            
            ################ NNV -----------------------------------> why does this only go to month 8 in the Excel formula?????
            ## Hospitalized infections
            ifelse(monthstartvac4()==1,
                   novaccine<-novaccine%>%
                       mutate(NNVhospinf=1/((vaccineeffectiveness*(sum(hospinfwovac)))/populationsize)),
                   novaccine<-novaccine%>%
                       mutate(NNVhospinf=1/((vaccineeffectiveness*(sum(hospinfwovac[monthstartvac4():nrow(novaccine)])))/populationsize))
            )
            
            
            ## non-Hospitalized infections
            ifelse(monthstartvac4()==1,
                   novaccine<-novaccine%>%
                       mutate(NNVnonhospinf=1/((vaccineeffectiveness*(sum(nonhospinfwovac)))/populationsize)),
                   novaccine<-novaccine%>%
                       mutate(NNVnonhospinf=1/((vaccineeffectiveness*(sum(nonhospinfwovac[monthstartvac4():nrow(novaccine)])))/populationsize))
            )
            
            ## Medically attended infections
            ifelse(monthstartvac4()==1,
                   novaccine<-novaccine%>%
                       mutate(NNVmedicalattendedinf=1/((vaccineeffectiveness*(sum(medicallyattendedinfwovac)))/populationsize)),
                   novaccine<-novaccine%>%
                       mutate(NNVmedicalattendedinf=1/((vaccineeffectiveness*(sum(medicallyattendedinfwovac[monthstartvac4():nrow(novaccine)])))/populationsize))
            )
            
            ## Total infections
            ifelse(monthstartvac4()==1,
                   novaccine<-novaccine%>%
                       mutate(NNVoverallinf=1/((vaccineeffectiveness*(sum(hospinfwovac)+sum(nonhospinfwovac)))/populationsize)),
                   novaccine<-novaccine%>%
                       mutate(NNVoverallinf=1/((vaccineeffectiveness*(sum(hospinfwovac[monthstartvac4():nrow(novaccine)])+sum(nonhospinfwovac[monthstartvac4():nrow(novaccine)])))/populationsize))
            )
            
            return(novaccine)
            
            
        }#close "if hypothetical campaign"
        
    }) #close burdendataset
    
    
    
    ####################################################################################################
    ## List of burden averted results
    ####################################################################################################
    
    
    burdenavertedresultscoverage3timing4<-reactive({
        
        populationsize<-as.numeric(as.character(populationsize()))
        hnh<-as.numeric(as.character(hnh()))
        mai<-as.numeric(as.character(mai()))
        vaccineeffectiveness<-as.numeric(as.character(vaccineeffectiveness()))
        
        
        
        #################################################
        ## Results
        #################################################
        nonhospinfwovactot<-sum(burdendataset_coverage3timing4()$nonhospinfwovac)
        nonhospinfwvacctot<-sum(burdendataset_coverage3timing4()$nonhospinfwvac)
        nonhospinfaverted<-nonhospinfwovactot-nonhospinfwvacctot
        NNVnonhospinffinal<-burdendataset_coverage3timing4()$NNVnonhospinf[1]
        
        medicallyattendedinfwovactot<-sum(burdendataset_coverage3timing4()$medicallyattendedinfwovac)
        medicallyattendedinfwvactot<-sum(burdendataset_coverage3timing4()$medicalattedtoinfwvac)
        medicallyattendedinfaverted<-medicallyattendedinfwovactot-medicallyattendedinfwvactot
        NNVmedicallyattendtedfinal<-burdendataset_coverage3timing4()$NNVmedicalattendedinf[1]
        
        Totalinfwovacc<-sum(burdendataset_coverage3timing4()$overallinfwovac)
        Totalinfwvacc<-sum(burdendataset_coverage3timing4()$overallinfwvac)
        Totalinfaverted<-Totalinfwovacc-Totalinfwvacc
        NNVtotalinf<-burdendataset_coverage3timing4()$NNVoverallinf[1]
        
        hospinfwovactot<-sum(burdendataset_coverage3timing4()$hospinfwovac)
        hospinfwvacctot<-sum(burdendataset_coverage3timing4()$hospinfwvac)
        hospaverted<-hospinfwovactot-hospinfwvacctot
        NNVhospinffinal<-burdendataset_coverage3timing4()$NNVhospinf[1]
        
        
        nonhospPreventedfraction<-burdendataset_coverage3timing4()$PFnonhospinf[1]
        hospPreventedfraction<-burdendataset_coverage3timing4()$PFhospinf[1]
        
        
        indicator_results1<-c(nonhospinfwovactot, nonhospinfwvacctot, nonhospinfaverted, NNVnonhospinffinal,
                              Totalinfwovacc, Totalinfwvacc, Totalinfaverted, NNVtotalinf,
                              hospinfwovactot, hospinfwvacctot, hospaverted, NNVhospinffinal,
                              medicallyattendedinfwovactot, medicallyattendedinfwvactot, medicallyattendedinfaverted, NNVmedicallyattendtedfinal,
                              nonhospPreventedfraction)
        
        indicator_results_titles<-c("nonhospinfwovactot", "nonhospinfwvacctot", "nonhospinfaverted", "NNVnonhospinffinal",
                                    "Totalinfwovacc", "Totalinfwvacc", "Totalinfaverted", "NNVtotalinf",
                                    "hospinfwovactot", "hospinfwvacctot", "hospaverted", "NNVhospinffinal",
                                    "medicallyattendedinfwovactot", "medicallyattendedinfwvactot", "medicallyattendedinfaverted", "NNVmedicallyattendtedfinal",
                                    "nonhospPreventedfraction")
        
        indicator_results1<-cbind(indicator_results_titles, indicator_results1)
        
        
        return(indicator_results1)
        
        
    })#burdendataset_coverage3timing4
    
    
    
    
    
    #######################################################
    
    ##Deployment coverage 4, timing 1
    ######################################################
    
    
    burdendataset_coverage4timing1<- reactive({
        ################################ The output data when insufficient data has been entered   ##################################
        ############################################################################################################################
        
        
        if(scenario()=="Impact of current influenza vaccination campaign"){
            
            
            ##########################################################################################
            # Scenario of influenza circulation in presence of vaccine: Presence to absence of vaccine
            # This is a situation where the country conducts a vaccination campaign, we enter
            # actual burden inputs and vaccine coverage then compare this to a hypothetical scenario
            # where there is no vaccination campaign conducted
            ##########################################################################################
            
            
            
            
            hospinf<-c(hospitalizationsmonthly(), hospmonth2(), hospmonth3(), hospmonth4(), hospmonth5(), hospmonth6(),
                       hospmonth7(), hospmonth8(), hospmonth9(), hospmonth10(), hospmonth11(), hospmonth12())
            
            populationsize<-as.numeric(as.character(populationsize()))
            vaccineeffectiveness<-as.numeric(as.character(vaccineeffectiveness()))
            hnh<-as.numeric(as.character(hnh()))
            mai<-as.numeric(as.character(mai()))
            hospinf<-as.numeric(as.character(hospinf))
            
            novaccine<-hospinf%>%
                as.data.frame()%>%
                rename(hospinfwvac=1)%>%
                mutate(hospinfcumulwvac=cumsum(hospinfwvac))%>%
                mutate(nonhospinfwvac=hospinfwvac*hnh)%>%
                mutate(nonhospinfcumulwvac=cumsum(nonhospinfwvac))%>%
                mutate(overallinfwvac=hospinfwvac+nonhospinfwvac)%>%
                mutate(overallinfcumulwvac=cumsum(overallinfwvac))%>%
                mutate(medicalattedtoinfwvac=overallinfwvac*mai)%>%
                mutate(medicallyattendedinfcumulwvac=cumsum(medicalattedtoinfwvac))
            
            
            
            ##bind monthly coverage
            monthvaccov<-c(vaccinecoveragemonthly(), month2(), month3(), month4(), month5(), month6(),
                           month7(), month8(), month9(), month10(), month11(), month12())
            
            monthvaccov<-as.numeric(as.character(monthvaccov))
            
            totcoverage<-sum(monthvaccov)
            
            
            novaccine<-cbind(novaccine, monthvaccov)
            
            novaccine$covergdistrib<- c(vaccinecoveragemonthly1(), month21(), month31(), month41(), month51(), month61(), month71(), month81(), month91(), month101(), month111(), month121())
            
            novaccine$covergdistrib<-as.numeric(as.character(novaccine$covergdistrib))
            
            novaccine$monthvaccov<- novaccine$covergdistrib*coverage4() ## replace original monthvaccov column with the new monthly coverage - this is so that the variable doesn't have to be changed in the rest of the script
            
            
            ## month number
            novaccine$monthnum<-c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
            
            ## add monthly effectively vaccinated
            novaccine<-novaccine%>%
                mutate(effectivelyvacc=populationsize*monthvaccov*vaccineeffectiveness)%>%
                mutate(effectivelyvacccumul=cumsum(effectivelyvacc))
            
            
            
            
            #####################################################################
            # Estimated rates
            #####################################################################
            novaccine<-novaccine%>%
                mutate(susceptiblewvacc=populationsize-hospinfcumulwvac-nonhospinfcumulwvac-effectivelyvacccumul)
            
            #Create columns
            novaccine$irhospinf<-NA
            novaccine$irnonhospinf<-NA
            
            #Create first row
            novaccine[1,14]<-novaccine$hospinfwvac[1]/populationsize
            novaccine[1,15]<-novaccine$nonhospinfwvac[1]/populationsize
            
            
            ##Calculation uses last month's susceptible population
            for(i in 2:nrow(novaccine)){
                novaccine$irhospinf[i]<-novaccine$hospinfwvac[i]/novaccine$susceptiblewvacc[i-1]
                novaccine$irnonhospinf[i]<- novaccine$nonhospinfwvac[i]/novaccine$susceptiblewvacc[i-1]
            }
            
            #####################################################################
            # Scenario of influenza circulation in absence of vaccine
            #####################################################################
            
            ## These calculations are initialized by the 'Susceptible at start of month in absence of vaccine'
            ## column, which starts with the Target Population in the first month.
            
            remainsuscstartmonth<-c(populationsize, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
            novaccine<-cbind(novaccine, remainsuscstartmonth)
            
            #Create columns
            novaccine$hospinfwovac<-NA
            novaccine$hospinfcumulwovacc<-NA
            novaccine$nonhospinfwovac<-NA
            novaccine$nonhospinfcumulwovacc<-NA
            novaccine$remainingsusc<-NA
            
            #Create first row
            novaccine[1,17]<-novaccine$remainsuscstartmonth[1]*novaccine$irhospinf[1]
            novaccine[1,18]<-novaccine$hospinfwovac[1]
            novaccine[1,19]<-novaccine$remainsuscstartmonth[1]*novaccine$irnonhospinf[1]
            novaccine[1,20]<-novaccine$nonhospinfwovac[1]
            novaccine[1,21]<-populationsize-novaccine$hospinfcumulwovacc[1]-novaccine$nonhospinfcumulwovacc[1]
            
            
            #Create rows 2:12
            for(i in 2:nrow(novaccine)){
                novaccine$remainsuscstartmonth[i]<-novaccine$remainingsusc[i-1]
                novaccine$hospinfwovac[i]<-novaccine$remainsuscstartmonth[i]*novaccine$irhospinf[i]
                novaccine$hospinfcumulwovacc[i]<-sum(novaccine$hospinfwovac[1:i])
                novaccine$nonhospinfwovac[i]<-novaccine$remainsuscstartmonth[i]*novaccine$irnonhospinf[i]
                novaccine$nonhospinfcumulwovacc[i]<-sum(novaccine$nonhospinfwovac[1:i])
                novaccine$remainingsusc[i]<-populationsize-novaccine$hospinfcumulwovacc[i]-novaccine$nonhospinfcumulwovacc[i]
            }
            
            novaccine<-novaccine%>%
                mutate(overallinfwovac=hospinfwovac+nonhospinfwovac)%>%
                mutate(overallinfcumulwovacc=cumsum(overallinfwovac))%>%
                mutate(medicallyattendedinfwovac=overallinfwovac*mai)%>%
                mutate(medicalattedtoinfcumulwovacc=cumsum(medicallyattendedinfwovac))
            
            ######## Avoided events
            novaccine<-novaccine%>%
                mutate(AEhospinf=hospinfwovac-hospinfwvac)%>%
                mutate(AEnonhospinf=nonhospinfwovac-nonhospinfwvac)%>%
                mutate(AEmedicalattedtoinf=medicallyattendedinfwovac-medicalattedtoinfwvac)%>%
                mutate(AEoverallinf=overallinfwovac-overallinfwvac)
            
            
            ################ Prevented fraction
            ## Hospitalized infections
            ifelse (monthstartvac1()==1,
                    novaccine<-novaccine%>%
                        mutate(PFhospinf=sum(AEhospinf)/sum(hospinfwovac)),
                    novaccine<-novaccine%>%
                        mutate(PFhospinf=sum(AEhospinf[monthstartvac1():nrow(novaccine)]/sum(hospinfwovac[monthstartvac1():nrow(novaccine)])))
            )
            
            ## non-Hospitalized infections
            ifelse (monthstartvac1()==1,
                    novaccine<-novaccine%>%
                        mutate(PFnonhospinf=sum(AEnonhospinf)/sum(nonhospinfwovac)),
                    novaccine<-novaccine%>%
                        mutate(PFnonhospinf=sum(AEnonhospinf[monthstartvac1():nrow(novaccine)]/sum(nonhospinfwovac[monthstartvac1():nrow(novaccine)])))
            )
            
            
            ################ NNV -----------------------------------> why does this only go to month 8 in the Excel formula?????
            ## Hospitalized infections
            ifelse(monthstartvac1()==1,
                   novaccine<-novaccine%>%
                       mutate(NNVhospinf=1/((vaccineeffectiveness*(sum(hospinfwovac)))/populationsize)),
                   novaccine<-novaccine%>%
                       mutate(NNVhospinf=1/((vaccineeffectiveness*(sum(hospinfwovac[monthstartvac1():nrow(novaccine)])))/populationsize))
            )
            
            
            ## non-Hospitalized infections
            ifelse(monthstartvac1()==1,
                   novaccine<-novaccine%>%
                       mutate(NNVnonhospinf=1/((vaccineeffectiveness*(sum(nonhospinfwovac)))/populationsize)),
                   novaccine<-novaccine%>%
                       mutate(NNVnonhospinf=1/((vaccineeffectiveness*(sum(nonhospinfwovac[monthstartvac1():nrow(novaccine)])))/populationsize))
            )
            
            ## Medically infections
            ifelse(monthstartvac1()==1,
                   novaccine<-novaccine%>%
                       mutate(NNVmedicalattendedinf=1/((vaccineeffectiveness*(sum(medicallyattendedinfwovac)))/populationsize)),
                   novaccine<-novaccine%>%
                       mutate(NNVmedicalattendedinf=1/((vaccineeffectiveness*(sum(medicallyattendedinfwovac[monthstartvac1():nrow(novaccine)])))/populationsize))
            )
            
            ## Overall infections
            ifelse(monthstartvac1()==1,
                   novaccine<-novaccine%>%
                       mutate(NNVoverallinf=1/((vaccineeffectiveness*(sum(hospinfwovac)+sum(nonhospinfwovac)))/populationsize)),
                   novaccine<-novaccine%>%
                       mutate(NNVoverallinf=1/((vaccineeffectiveness*(sum(hospinfwovac[monthstartvac1():nrow(novaccine)])+sum(nonhospinfwovac[monthstartvac1():nrow(novaccine)])))/populationsize))
            )
            
            return(novaccine)
            
            
            
        } #close if current campaign
        
        
        
        if(scenario()=="Impact of hypothetical vaccination campaign"){
            
            ##########################################################################################
            # Scenario of influenza circulation in absence of vaccine: Absence to presence of vaccine
            # This is a scenario where the country has yet to conduct a vaccination campaign but would
            # like to see what impact a vaccine campaign may have.
            # The country enters the estimated number of hospitalization they observed and then we compare
            # this to a hypothetical vaccination campaign
            ##########################################################################################
            
            hospinf<-c(hospitalizationsmonthly(), hospmonth2(), hospmonth3(), hospmonth4(), hospmonth5(), hospmonth6(),
                       hospmonth7(), hospmonth8(), hospmonth9(), hospmonth10(), hospmonth11(), hospmonth12())
            
            populationsize<-as.numeric(as.character(populationsize()))
            hnh<-as.numeric(as.character(hnh()))
            mai<-as.numeric(as.character(mai()))
            hospinf<-as.numeric(as.character(hospinf))
            vaccineeffectiveness<-as.numeric(as.character(vaccineeffectiveness()))
            
            #####################Observed influenza in the absence of vaccine
            novaccine<-hospinf%>%
                as.data.frame()%>%
                rename(hospinfwovac=1)%>%
                mutate(hospinfcumulwovac=cumsum(hospinfwovac))%>%
                mutate(nonhospinfwovac=hospinfwovac*hnh)%>%
                mutate(nonhospinfcumulwovac=cumsum(nonhospinfwovac))%>%
                mutate(remainingsusc=populationsize-hospinfcumulwovac-nonhospinfcumulwovac)
            
            #create remainsuscstartmonth column with target population and rows 1:11 of remainingsusc
            remainsuscstartmonth<-populationsize%>%
                as.data.frame()%>%
                rename(remainsuscstartmonth=1)
            
            remainsuscstartmonth2<-as.data.frame(novaccine$remainingsusc[1:11])%>%
                as.data.frame()%>%
                rename(remainsuscstartmonth=1)
            
            remainsuscstartmonth<-rbind(remainsuscstartmonth, remainsuscstartmonth2)
            
            novaccine<-cbind(novaccine, remainsuscstartmonth)
            
            novaccine<-novaccine%>%
                mutate(overallinfwovac=hospinfwovac+nonhospinfwovac)%>%
                mutate(overallinfcumulwovac=cumsum(overallinfwovac))%>%
                mutate(medicallyattendedinfwovac=overallinfwovac*mai)%>%
                mutate(medicallyattendedinfcumulwovac=cumsum(medicallyattendedinfwovac))
            
            
            
            #####################################################################
            # Incidence rates in the absence of vaccine
            #####################################################################
            novaccine<-novaccine%>%
                mutate(irhospinf=hospinfwovac/remainsuscstartmonth)%>%
                mutate(irnonhospinf=nonhospinfwovac/remainsuscstartmonth)
            
            
            
            
            ############################ Burden inputs with hypothetical vaccination campaign
            #Create columns
            novaccine$hospinfwvac<-NA
            novaccine$hospinfcumulwvac<-NA
            novaccine$nonhospinfwvac<-NA
            novaccine$nonhospinfcumulwvac<-NA
            
            
            
            #Create first row
            novaccine[1,13]<-populationsize*novaccine$irhospinf[1]
            novaccine[1,14]<-novaccine$hospinfwvac[1]
            novaccine[1,15]<-populationsize*novaccine$irnonhospinf[1]
            novaccine[1,16]<-novaccine$nonhospinfwvac[1]
            
            ##bind monthly coverage
            monthvaccov<-c(vaccinecoveragemonthly(), month2(), month3(), month4(), month5(), month6(),
                           month7(), month8(), month9(), month10(), month11(), month12())
            
            monthvaccov<-as.numeric(as.character(monthvaccov))
            
            totcoverage<-sum(monthvaccov)
            
            
            novaccine<-cbind(novaccine, monthvaccov)
            
            novaccine$covergdistrib<- c(vaccinecoveragemonthly1(), month21(), month31(), month41(), month51(), month61(), month71(), month81(), month91(), month101(), month111(), month121())
            
            novaccine$covergdistrib<-as.numeric(as.character(novaccine$covergdistrib))
            
            novaccine$monthvaccov<- novaccine$covergdistrib*coverage4() ## replace original monthvaccov column with the new monthly coverage - this is so that the variable doesn't have to be changed in the rest of the script
            
            
            ##monthly effectively vaccinated and susceptible with hypothetical vaccination campaign
            novaccine<-novaccine%>%
                mutate(effectivelyvacc=populationsize*monthvaccov*vaccineeffectiveness)%>%
                mutate(effectivelycumulvacc=cumsum(effectivelyvacc))%>%
                mutate(suscwvacc=populationsize-hospinfcumulwvac-nonhospinfcumulwvac-effectivelyvacc)
            
            ##Then calculate the rest of hospinfwovac and nonhospinfwovac. Calculation uses last month's incidence rate
            for(i in 2:nrow(novaccine)){
                novaccine$hospinfwvac[i]<-novaccine$suscwvacc[i-1]*novaccine$irhospinf[i]
                novaccine$hospinfcumulwvac[i]<-sum(novaccine$hospinfwvac[1:i])
                novaccine$nonhospinfwvac[i]<- novaccine$suscwvacc[i-1]*novaccine$irnonhospinf[i]
                novaccine$nonhospinfcumulwvac[i]<-sum(novaccine$nonhospinfwvac[1:i])
                novaccine$suscwvacc[i]=populationsize-novaccine$hospinfcumulwvac[i]-novaccine$nonhospinfcumulwvac[i]-novaccine$effectivelycumulvacc[i]
            }
            
            
            ## month number
            novaccine$monthnum<-c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
            
            
            ## Overall infections with hypothetical vaccination campaign
            novaccine<-novaccine%>%
                mutate(overallinfwvac=hospinfwvac+nonhospinfwvac)%>%
                mutate(overallinfcumulwvacc=cumsum(overallinfwvac))
            
            ## Medically attended with hypothetical vaccination campaign
            novaccine<-novaccine%>%
                mutate(medicalattedtoinfwvac=overallinfwvac*mai)
            
            
            ######## Avoided events
            novaccine<-novaccine%>%
                mutate(AEhospinf=hospinfwovac-hospinfwvac)%>%
                mutate(AEnonhospinf=nonhospinfwovac-nonhospinfwvac)%>%
                mutate(AEmedicalattedtoinf=medicallyattendedinfwovac-medicalattedtoinfwvac)%>%
                mutate(AEoverallinf=overallinfwovac-overallinfwvac)
            
            
            
            ################ Prevented fraction
            ## Hospitalized infections
            ifelse (monthstartvac1()==1,
                    novaccine<-novaccine%>%
                        mutate(PFhospinf=sum(AEhospinf)/sum(hospinfwovac)),
                    novaccine<-novaccine%>%
                        mutate(PFhospinf=sum(AEhospinf[monthstartvac1():nrow(novaccine)]/sum(hospinfwovac[monthstartvac1():nrow(novaccine)])))
            )
            
            ## non-Hospitalized infections
            ifelse (monthstartvac1()==1,
                    novaccine<-novaccine%>%
                        mutate(PFnonhospinf=sum(AEnonhospinf)/sum(nonhospinfwovac)),
                    novaccine<-novaccine%>%
                        mutate(PFnonhospinf=sum(AEnonhospinf[monthstartvac1():nrow(novaccine)]/sum(nonhospinfwovac[monthstartvac1():nrow(novaccine)])))
            )
            
            
            
            
            ################ NNV -----------------------------------> why does this only go to month 8 in the Excel formula?????
            ## Hospitalized infections
            ifelse(monthstartvac1()==1,
                   novaccine<-novaccine%>%
                       mutate(NNVhospinf=1/((vaccineeffectiveness*(sum(hospinfwovac)))/populationsize)),
                   novaccine<-novaccine%>%
                       mutate(NNVhospinf=1/((vaccineeffectiveness*(sum(hospinfwovac[monthstartvac1():nrow(novaccine)])))/populationsize))
            )
            
            
            ## non-Hospitalized infections
            ifelse(monthstartvac1()==1,
                   novaccine<-novaccine%>%
                       mutate(NNVnonhospinf=1/((vaccineeffectiveness*(sum(nonhospinfwovac)))/populationsize)),
                   novaccine<-novaccine%>%
                       mutate(NNVnonhospinf=1/((vaccineeffectiveness*(sum(nonhospinfwovac[monthstartvac1():nrow(novaccine)])))/populationsize))
            )
            
            ## Medically attended infections
            ifelse(monthstartvac1()==1,
                   novaccine<-novaccine%>%
                       mutate(NNVmedicalattendedinf=1/((vaccineeffectiveness*(sum(medicallyattendedinfwovac)))/populationsize)),
                   novaccine<-novaccine%>%
                       mutate(NNVmedicalattendedinf=1/((vaccineeffectiveness*(sum(medicallyattendedinfwovac[monthstartvac1():nrow(novaccine)])))/populationsize))
            )
            
            ## Total infections
            ifelse(monthstartvac1()==1,
                   novaccine<-novaccine%>%
                       mutate(NNVoverallinf=1/((vaccineeffectiveness*(sum(hospinfwovac)+sum(nonhospinfwovac)))/populationsize)),
                   novaccine<-novaccine%>%
                       mutate(NNVoverallinf=1/((vaccineeffectiveness*(sum(hospinfwovac[monthstartvac1():nrow(novaccine)])+sum(nonhospinfwovac[monthstartvac1():nrow(novaccine)])))/populationsize))
            )
            
            return(novaccine)
            
            
        }#close "if hypothetical campaign"
        
    }) #close burdendataset
    
    
    
    ####################################################################################################
    ## List of burden averted results
    ####################################################################################################
    
    
    burdenavertedresultscoverage4timing1<-reactive({
        
        populationsize<-as.numeric(as.character(populationsize()))
        hnh<-as.numeric(as.character(hnh()))
        mai<-as.numeric(as.character(mai()))
        vaccineeffectiveness<-as.numeric(as.character(vaccineeffectiveness()))
        
        
        
        #################################################
        ## Results
        #################################################
        nonhospinfwovactot<-sum(burdendataset_coverage4timing1()$nonhospinfwovac)
        nonhospinfwvacctot<-sum(burdendataset_coverage4timing1()$nonhospinfwvac)
        nonhospinfaverted<-nonhospinfwovactot-nonhospinfwvacctot
        NNVnonhospinffinal<-burdendataset_coverage4timing1()$NNVnonhospinf[1]
        
        medicallyattendedinfwovactot<-sum(burdendataset_coverage4timing1()$medicallyattendedinfwovac)
        medicallyattendedinfwvactot<-sum(burdendataset_coverage4timing1()$medicalattedtoinfwvac)
        medicallyattendedinfaverted<-medicallyattendedinfwovactot-medicallyattendedinfwvactot
        NNVmedicallyattendtedfinal<-burdendataset_coverage4timing1()$NNVmedicalattendedinf[1]
        
        Totalinfwovacc<-sum(burdendataset_coverage4timing1()$overallinfwovac)
        Totalinfwvacc<-sum(burdendataset_coverage4timing1()$overallinfwvac)
        Totalinfaverted<-Totalinfwovacc-Totalinfwvacc
        NNVtotalinf<-burdendataset_coverage4timing1()$NNVoverallinf[1]
        
        hospinfwovactot<-sum(burdendataset_coverage4timing1()$hospinfwovac)
        hospinfwvacctot<-sum(burdendataset_coverage4timing1()$hospinfwvac)
        hospaverted<-hospinfwovactot-hospinfwvacctot
        NNVhospinffinal<-burdendataset_coverage4timing1()$NNVhospinf[1]
        
        
        nonhospPreventedfraction<-burdendataset_coverage4timing1()$PFnonhospinf[1]
        hospPreventedfraction<-burdendataset_coverage4timing1()$PFhospinf[1]
        
        
        indicator_results1<-c(nonhospinfwovactot, nonhospinfwvacctot, nonhospinfaverted, NNVnonhospinffinal,
                              Totalinfwovacc, Totalinfwvacc, Totalinfaverted, NNVtotalinf,
                              hospinfwovactot, hospinfwvacctot, hospaverted, NNVhospinffinal,
                              medicallyattendedinfwovactot, medicallyattendedinfwvactot, medicallyattendedinfaverted, NNVmedicallyattendtedfinal,
                              nonhospPreventedfraction)
        
        indicator_results_titles<-c("nonhospinfwovactot", "nonhospinfwvacctot", "nonhospinfaverted", "NNVnonhospinffinal",
                                    "Totalinfwovacc", "Totalinfwvacc", "Totalinfaverted", "NNVtotalinf",
                                    "hospinfwovactot", "hospinfwvacctot", "hospaverted", "NNVhospinffinal",
                                    "medicallyattendedinfwovactot", "medicallyattendedinfwvactot", "medicallyattendedinfaverted", "NNVmedicallyattendtedfinal",
                                    "nonhospPreventedfraction")
        
        indicator_results1<-cbind(indicator_results_titles, indicator_results1)
        
        
        return(indicator_results1)
        
        
    })#burdendataset_coverage4timing1
    
    
    
    #######################################################
    
    ##Deployment coverage 4, timing 2
    ######################################################
    
    
    burdendataset_coverage4timing2<- reactive({
        ################################ The output data when insufficient data has been entered   ##################################
        ############################################################################################################################
        
        
        if(scenario()=="Impact of current influenza vaccination campaign"){
            
            
            ##########################################################################################
            # Scenario of influenza circulation in presence of vaccine: Presence to absence of vaccine
            # This is a situation where the country conducts a vaccination campaign, we enter
            # actual burden inputs and vaccine coverage then compare this to a hypothetical scenario
            # where there is no vaccination campaign conducted
            ##########################################################################################
            
            
            
            
            hospinf<-c(hospitalizationsmonthly(), hospmonth2(), hospmonth3(), hospmonth4(), hospmonth5(), hospmonth6(),
                       hospmonth7(), hospmonth8(), hospmonth9(), hospmonth10(), hospmonth11(), hospmonth12())
            
            populationsize<-as.numeric(as.character(populationsize()))
            vaccineeffectiveness<-as.numeric(as.character(vaccineeffectiveness()))
            hnh<-as.numeric(as.character(hnh()))
            mai<-as.numeric(as.character(mai()))
            hospinf<-as.numeric(as.character(hospinf))
            
            novaccine<-hospinf%>%
                as.data.frame()%>%
                rename(hospinfwvac=1)%>%
                mutate(hospinfcumulwvac=cumsum(hospinfwvac))%>%
                mutate(nonhospinfwvac=hospinfwvac*hnh)%>%
                mutate(nonhospinfcumulwvac=cumsum(nonhospinfwvac))%>%
                mutate(overallinfwvac=hospinfwvac+nonhospinfwvac)%>%
                mutate(overallinfcumulwvac=cumsum(overallinfwvac))%>%
                mutate(medicalattedtoinfwvac=overallinfwvac*mai)%>%
                mutate(medicallyattendedinfcumulwvac=cumsum(medicalattedtoinfwvac))
            
            
            
            ##bind monthly coverage
            monthvaccov<-c(vaccinecoveragemonthly(), month2(), month3(), month4(), month5(), month6(),
                           month7(), month8(), month9(), month10(), month11(), month12())
            
            monthvaccov<-as.numeric(as.character(monthvaccov))
            
            totcoverage<-sum(monthvaccov)
            
            
            novaccine<-cbind(novaccine, monthvaccov)
            
            novaccine$covergdistrib<- c(vaccinecoveragemonthly2(), month22(), month32(), month42(), month52(), month62(), month72(), month82(), month92(), month102(), month112(), month122())
            
            novaccine$covergdistrib<-as.numeric(as.character(novaccine$covergdistrib))
            
            novaccine$monthvaccov<- novaccine$covergdistrib*coverage4() ## replace original monthvaccov column with the new monthly coverage - this is so that the variable doesn't have to be changed in the rest of the script
            
            
            ## month number
            novaccine$monthnum<-c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
            
            ## add monthly effectively vaccinated
            novaccine<-novaccine%>%
                mutate(effectivelyvacc=populationsize*monthvaccov*vaccineeffectiveness)%>%
                mutate(effectivelyvacccumul=cumsum(effectivelyvacc))
            
            
            
            
            #####################################################################
            # Estimated rates
            #####################################################################
            novaccine<-novaccine%>%
                mutate(susceptiblewvacc=populationsize-hospinfcumulwvac-nonhospinfcumulwvac-effectivelyvacccumul)
            
            #Create columns
            novaccine$irhospinf<-NA
            novaccine$irnonhospinf<-NA
            
            #Create first row
            novaccine[1,14]<-novaccine$hospinfwvac[1]/populationsize
            novaccine[1,15]<-novaccine$nonhospinfwvac[1]/populationsize
            
            
            ##Calculation uses last month's susceptible population
            for(i in 2:nrow(novaccine)){
                novaccine$irhospinf[i]<-novaccine$hospinfwvac[i]/novaccine$susceptiblewvacc[i-1]
                novaccine$irnonhospinf[i]<- novaccine$nonhospinfwvac[i]/novaccine$susceptiblewvacc[i-1]
            }
            
            #####################################################################
            # Scenario of influenza circulation in absence of vaccine
            #####################################################################
            
            ## These calculations are initialized by the 'Susceptible at start of month in absence of vaccine'
            ## column, which starts with the Target Population in the first month.
            
            remainsuscstartmonth<-c(populationsize, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
            novaccine<-cbind(novaccine, remainsuscstartmonth)
            
            #Create columns
            novaccine$hospinfwovac<-NA
            novaccine$hospinfcumulwovacc<-NA
            novaccine$nonhospinfwovac<-NA
            novaccine$nonhospinfcumulwovacc<-NA
            novaccine$remainingsusc<-NA
            
            #Create first row
            novaccine[1,17]<-novaccine$remainsuscstartmonth[1]*novaccine$irhospinf[1]
            novaccine[1,18]<-novaccine$hospinfwovac[1]
            novaccine[1,19]<-novaccine$remainsuscstartmonth[1]*novaccine$irnonhospinf[1]
            novaccine[1,20]<-novaccine$nonhospinfwovac[1]
            novaccine[1,21]<-populationsize-novaccine$hospinfcumulwovacc[1]-novaccine$nonhospinfcumulwovacc[1]
            
            
            #Create rows 2:12
            for(i in 2:nrow(novaccine)){
                novaccine$remainsuscstartmonth[i]<-novaccine$remainingsusc[i-1]
                novaccine$hospinfwovac[i]<-novaccine$remainsuscstartmonth[i]*novaccine$irhospinf[i]
                novaccine$hospinfcumulwovacc[i]<-sum(novaccine$hospinfwovac[1:i])
                novaccine$nonhospinfwovac[i]<-novaccine$remainsuscstartmonth[i]*novaccine$irnonhospinf[i]
                novaccine$nonhospinfcumulwovacc[i]<-sum(novaccine$nonhospinfwovac[1:i])
                novaccine$remainingsusc[i]<-populationsize-novaccine$hospinfcumulwovacc[i]-novaccine$nonhospinfcumulwovacc[i]
            }
            
            novaccine<-novaccine%>%
                mutate(overallinfwovac=hospinfwovac+nonhospinfwovac)%>%
                mutate(overallinfcumulwovacc=cumsum(overallinfwovac))%>%
                mutate(medicallyattendedinfwovac=overallinfwovac*mai)%>%
                mutate(medicalattedtoinfcumulwovacc=cumsum(medicallyattendedinfwovac))
            
            ######## Avoided events
            novaccine<-novaccine%>%
                mutate(AEhospinf=hospinfwovac-hospinfwvac)%>%
                mutate(AEnonhospinf=nonhospinfwovac-nonhospinfwvac)%>%
                mutate(AEmedicalattedtoinf=medicallyattendedinfwovac-medicalattedtoinfwvac)%>%
                mutate(AEoverallinf=overallinfwovac-overallinfwvac)
            
            
            ################ Prevented fraction
            ## Hospitalized infections
            ifelse (monthstartvac2()==1,
                    novaccine<-novaccine%>%
                        mutate(PFhospinf=sum(AEhospinf)/sum(hospinfwovac)),
                    novaccine<-novaccine%>%
                        mutate(PFhospinf=sum(AEhospinf[monthstartvac2():nrow(novaccine)]/sum(hospinfwovac[monthstartvac2():nrow(novaccine)])))
            )
            
            ## non-Hospitalized infections
            ifelse (monthstartvac2()==1,
                    novaccine<-novaccine%>%
                        mutate(PFnonhospinf=sum(AEnonhospinf)/sum(nonhospinfwovac)),
                    novaccine<-novaccine%>%
                        mutate(PFnonhospinf=sum(AEnonhospinf[monthstartvac2():nrow(novaccine)]/sum(nonhospinfwovac[monthstartvac2():nrow(novaccine)])))
            )
            
            
            ################ NNV -----------------------------------> why does this only go to month 8 in the Excel formula?????
            ## Hospitalized infections
            ifelse(monthstartvac2()==1,
                   novaccine<-novaccine%>%
                       mutate(NNVhospinf=1/((vaccineeffectiveness*(sum(hospinfwovac)))/populationsize)),
                   novaccine<-novaccine%>%
                       mutate(NNVhospinf=1/((vaccineeffectiveness*(sum(hospinfwovac[monthstartvac2():nrow(novaccine)])))/populationsize))
            )
            
            
            ## non-Hospitalized infections
            ifelse(monthstartvac2()==1,
                   novaccine<-novaccine%>%
                       mutate(NNVnonhospinf=1/((vaccineeffectiveness*(sum(nonhospinfwovac)))/populationsize)),
                   novaccine<-novaccine%>%
                       mutate(NNVnonhospinf=1/((vaccineeffectiveness*(sum(nonhospinfwovac[monthstartvac2():nrow(novaccine)])))/populationsize))
            )
            
            ## Medically infections
            ifelse(monthstartvac2()==1,
                   novaccine<-novaccine%>%
                       mutate(NNVmedicalattendedinf=1/((vaccineeffectiveness*(sum(medicallyattendedinfwovac)))/populationsize)),
                   novaccine<-novaccine%>%
                       mutate(NNVmedicalattendedinf=1/((vaccineeffectiveness*(sum(medicallyattendedinfwovac[monthstartvac2():nrow(novaccine)])))/populationsize))
            )
            
            ## Overall infections
            ifelse(monthstartvac2()==1,
                   novaccine<-novaccine%>%
                       mutate(NNVoverallinf=1/((vaccineeffectiveness*(sum(hospinfwovac)+sum(nonhospinfwovac)))/populationsize)),
                   novaccine<-novaccine%>%
                       mutate(NNVoverallinf=1/((vaccineeffectiveness*(sum(hospinfwovac[monthstartvac2():nrow(novaccine)])+sum(nonhospinfwovac[monthstartvac2():nrow(novaccine)])))/populationsize))
            )
            
            return(novaccine)
            
            
            
        } #close if current campaign
        
        
        
        if(scenario()=="Impact of hypothetical vaccination campaign"){
            
            ##########################################################################################
            # Scenario of influenza circulation in absence of vaccine: Absence to presence of vaccine
            # This is a scenario where the country has yet to conduct a vaccination campaign but would
            # like to see what impact a vaccine campaign may have.
            # The country enters the estimated number of hospitalization they observed and then we compare
            # this to a hypothetical vaccination campaign
            ##########################################################################################
            
            hospinf<-c(hospitalizationsmonthly(), hospmonth2(), hospmonth3(), hospmonth4(), hospmonth5(), hospmonth6(),
                       hospmonth7(), hospmonth8(), hospmonth9(), hospmonth10(), hospmonth11(), hospmonth12())
            
            populationsize<-as.numeric(as.character(populationsize()))
            hnh<-as.numeric(as.character(hnh()))
            mai<-as.numeric(as.character(mai()))
            hospinf<-as.numeric(as.character(hospinf))
            vaccineeffectiveness<-as.numeric(as.character(vaccineeffectiveness()))
            
            #####################Observed influenza in the absence of vaccine
            novaccine<-hospinf%>%
                as.data.frame()%>%
                rename(hospinfwovac=1)%>%
                mutate(hospinfcumulwovac=cumsum(hospinfwovac))%>%
                mutate(nonhospinfwovac=hospinfwovac*hnh)%>%
                mutate(nonhospinfcumulwovac=cumsum(nonhospinfwovac))%>%
                mutate(remainingsusc=populationsize-hospinfcumulwovac-nonhospinfcumulwovac)
            
            #create remainsuscstartmonth column with target population and rows 1:11 of remainingsusc
            remainsuscstartmonth<-populationsize%>%
                as.data.frame()%>%
                rename(remainsuscstartmonth=1)
            
            remainsuscstartmonth2<-as.data.frame(novaccine$remainingsusc[1:11])%>%
                as.data.frame()%>%
                rename(remainsuscstartmonth=1)
            
            remainsuscstartmonth<-rbind(remainsuscstartmonth, remainsuscstartmonth2)
            
            novaccine<-cbind(novaccine, remainsuscstartmonth)
            
            novaccine<-novaccine%>%
                mutate(overallinfwovac=hospinfwovac+nonhospinfwovac)%>%
                mutate(overallinfcumulwovac=cumsum(overallinfwovac))%>%
                mutate(medicallyattendedinfwovac=overallinfwovac*mai)%>%
                mutate(medicallyattendedinfcumulwovac=cumsum(medicallyattendedinfwovac))
            
            
            
            #####################################################################
            # Incidence rates in the absence of vaccine
            #####################################################################
            novaccine<-novaccine%>%
                mutate(irhospinf=hospinfwovac/remainsuscstartmonth)%>%
                mutate(irnonhospinf=nonhospinfwovac/remainsuscstartmonth)
            
            
            
            
            ############################ Burden inputs with hypothetical vaccination campaign
            #Create columns
            novaccine$hospinfwvac<-NA
            novaccine$hospinfcumulwvac<-NA
            novaccine$nonhospinfwvac<-NA
            novaccine$nonhospinfcumulwvac<-NA
            
            
            
            #Create first row
            novaccine[1,13]<-populationsize*novaccine$irhospinf[1]
            novaccine[1,14]<-novaccine$hospinfwvac[1]
            novaccine[1,15]<-populationsize*novaccine$irnonhospinf[1]
            novaccine[1,16]<-novaccine$nonhospinfwvac[1]
            
            ##bind monthly coverage
            monthvaccov<-c(vaccinecoveragemonthly(), month2(), month3(), month4(), month5(), month6(),
                           month7(), month8(), month9(), month10(), month11(), month12())
            
            monthvaccov<-as.numeric(as.character(monthvaccov))
            
            totcoverage<-sum(monthvaccov)
            
            
            novaccine<-cbind(novaccine, monthvaccov)
            
            novaccine$covergdistrib<- c(vaccinecoveragemonthly2(), month22(), month32(), month42(), month52(), month62(), month72(), month82(), month92(), month102(), month112(), month122())
            
            novaccine$covergdistrib<-as.numeric(as.character(novaccine$covergdistrib))
            
            novaccine$monthvaccov<- novaccine$covergdistrib*coverage4() ## replace original monthvaccov column with the new monthly coverage - this is so that the variable doesn't have to be changed in the rest of the script
            
            
            ##monthly effectively vaccinated and susceptible with hypothetical vaccination campaign
            novaccine<-novaccine%>%
                mutate(effectivelyvacc=populationsize*monthvaccov*vaccineeffectiveness)%>%
                mutate(effectivelycumulvacc=cumsum(effectivelyvacc))%>%
                mutate(suscwvacc=populationsize-hospinfcumulwvac-nonhospinfcumulwvac-effectivelyvacc)
            
            ##Then calculate the rest of hospinfwovac and nonhospinfwovac. Calculation uses last month's incidence rate
            for(i in 2:nrow(novaccine)){
                novaccine$hospinfwvac[i]<-novaccine$suscwvacc[i-1]*novaccine$irhospinf[i]
                novaccine$hospinfcumulwvac[i]<-sum(novaccine$hospinfwvac[1:i])
                novaccine$nonhospinfwvac[i]<- novaccine$suscwvacc[i-1]*novaccine$irnonhospinf[i]
                novaccine$nonhospinfcumulwvac[i]<-sum(novaccine$nonhospinfwvac[1:i])
                novaccine$suscwvacc[i]=populationsize-novaccine$hospinfcumulwvac[i]-novaccine$nonhospinfcumulwvac[i]-novaccine$effectivelycumulvacc[i]
            }
            
            
            ## month number
            novaccine$monthnum<-c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
            
            
            ## Overall infections with hypothetical vaccination campaign
            novaccine<-novaccine%>%
                mutate(overallinfwvac=hospinfwvac+nonhospinfwvac)%>%
                mutate(overallinfcumulwvacc=cumsum(overallinfwvac))
            
            ## Medically attended with hypothetical vaccination campaign
            novaccine<-novaccine%>%
                mutate(medicalattedtoinfwvac=overallinfwvac*mai)
            
            
            ######## Avoided events
            novaccine<-novaccine%>%
                mutate(AEhospinf=hospinfwovac-hospinfwvac)%>%
                mutate(AEnonhospinf=nonhospinfwovac-nonhospinfwvac)%>%
                mutate(AEmedicalattedtoinf=medicallyattendedinfwovac-medicalattedtoinfwvac)%>%
                mutate(AEoverallinf=overallinfwovac-overallinfwvac)
            
            
            
            ################ Prevented fraction
            ## Hospitalized infections
            ifelse (monthstartvac2()==1,
                    novaccine<-novaccine%>%
                        mutate(PFhospinf=sum(AEhospinf)/sum(hospinfwovac)),
                    novaccine<-novaccine%>%
                        mutate(PFhospinf=sum(AEhospinf[monthstartvac2():nrow(novaccine)]/sum(hospinfwovac[monthstartvac2():nrow(novaccine)])))
            )
            
            ## non-Hospitalized infections
            ifelse (monthstartvac2()==1,
                    novaccine<-novaccine%>%
                        mutate(PFnonhospinf=sum(AEnonhospinf)/sum(nonhospinfwovac)),
                    novaccine<-novaccine%>%
                        mutate(PFnonhospinf=sum(AEnonhospinf[monthstartvac2():nrow(novaccine)]/sum(nonhospinfwovac[monthstartvac2():nrow(novaccine)])))
            )
            
            
            
            
            ################ NNV -----------------------------------> why does this only go to month 8 in the Excel formula?????
            ## Hospitalized infections
            ifelse(monthstartvac2()==1,
                   novaccine<-novaccine%>%
                       mutate(NNVhospinf=1/((vaccineeffectiveness*(sum(hospinfwovac)))/populationsize)),
                   novaccine<-novaccine%>%
                       mutate(NNVhospinf=1/((vaccineeffectiveness*(sum(hospinfwovac[monthstartvac2():nrow(novaccine)])))/populationsize))
            )
            
            
            ## non-Hospitalized infections
            ifelse(monthstartvac2()==1,
                   novaccine<-novaccine%>%
                       mutate(NNVnonhospinf=1/((vaccineeffectiveness*(sum(nonhospinfwovac)))/populationsize)),
                   novaccine<-novaccine%>%
                       mutate(NNVnonhospinf=1/((vaccineeffectiveness*(sum(nonhospinfwovac[monthstartvac2():nrow(novaccine)])))/populationsize))
            )
            
            ## Medically attended infections
            ifelse(monthstartvac2()==1,
                   novaccine<-novaccine%>%
                       mutate(NNVmedicalattendedinf=1/((vaccineeffectiveness*(sum(medicallyattendedinfwovac)))/populationsize)),
                   novaccine<-novaccine%>%
                       mutate(NNVmedicalattendedinf=1/((vaccineeffectiveness*(sum(medicallyattendedinfwovac[monthstartvac2():nrow(novaccine)])))/populationsize))
            )
            
            ## Total infections
            ifelse(monthstartvac2()==1,
                   novaccine<-novaccine%>%
                       mutate(NNVoverallinf=1/((vaccineeffectiveness*(sum(hospinfwovac)+sum(nonhospinfwovac)))/populationsize)),
                   novaccine<-novaccine%>%
                       mutate(NNVoverallinf=1/((vaccineeffectiveness*(sum(hospinfwovac[monthstartvac2():nrow(novaccine)])+sum(nonhospinfwovac[monthstartvac2():nrow(novaccine)])))/populationsize))
            )
            
            return(novaccine)
            
            
        }#close "if hypothetical campaign"
        
    }) #close burdendataset
    
    
    
    ####################################################################################################
    ## List of burden averted results
    ####################################################################################################
    
    
    burdenavertedresultscoverage4timing2<-reactive({
        
        populationsize<-as.numeric(as.character(populationsize()))
        hnh<-as.numeric(as.character(hnh()))
        mai<-as.numeric(as.character(mai()))
        vaccineeffectiveness<-as.numeric(as.character(vaccineeffectiveness()))
        
        
        
        #################################################
        ## Results
        #################################################
        nonhospinfwovactot<-sum(burdendataset_coverage4timing2()$nonhospinfwovac)
        nonhospinfwvacctot<-sum(burdendataset_coverage4timing2()$nonhospinfwvac)
        nonhospinfaverted<-nonhospinfwovactot-nonhospinfwvacctot
        NNVnonhospinffinal<-burdendataset_coverage4timing2()$NNVnonhospinf[1]
        
        medicallyattendedinfwovactot<-sum(burdendataset_coverage4timing2()$medicallyattendedinfwovac)
        medicallyattendedinfwvactot<-sum(burdendataset_coverage4timing2()$medicalattedtoinfwvac)
        medicallyattendedinfaverted<-medicallyattendedinfwovactot-medicallyattendedinfwvactot
        NNVmedicallyattendtedfinal<-burdendataset_coverage4timing2()$NNVmedicalattendedinf[1]
        
        Totalinfwovacc<-sum(burdendataset_coverage4timing2()$overallinfwovac)
        Totalinfwvacc<-sum(burdendataset_coverage4timing2()$overallinfwvac)
        Totalinfaverted<-Totalinfwovacc-Totalinfwvacc
        NNVtotalinf<-burdendataset_coverage4timing2()$NNVoverallinf[1]
        
        hospinfwovactot<-sum(burdendataset_coverage4timing2()$hospinfwovac)
        hospinfwvacctot<-sum(burdendataset_coverage4timing2()$hospinfwvac)
        hospaverted<-hospinfwovactot-hospinfwvacctot
        NNVhospinffinal<-burdendataset_coverage4timing2()$NNVhospinf[1]
        
        
        nonhospPreventedfraction<-burdendataset_coverage4timing2()$PFnonhospinf[1]
        hospPreventedfraction<-burdendataset_coverage4timing2()$PFhospinf[1]
        
        
        indicator_results1<-c(nonhospinfwovactot, nonhospinfwvacctot, nonhospinfaverted, NNVnonhospinffinal,
                              Totalinfwovacc, Totalinfwvacc, Totalinfaverted, NNVtotalinf,
                              hospinfwovactot, hospinfwvacctot, hospaverted, NNVhospinffinal,
                              medicallyattendedinfwovactot, medicallyattendedinfwvactot, medicallyattendedinfaverted, NNVmedicallyattendtedfinal,
                              nonhospPreventedfraction)
        
        indicator_results_titles<-c("nonhospinfwovactot", "nonhospinfwvacctot", "nonhospinfaverted", "NNVnonhospinffinal",
                                    "Totalinfwovacc", "Totalinfwvacc", "Totalinfaverted", "NNVtotalinf",
                                    "hospinfwovactot", "hospinfwvacctot", "hospaverted", "NNVhospinffinal",
                                    "medicallyattendedinfwovactot", "medicallyattendedinfwvactot", "medicallyattendedinfaverted", "NNVmedicallyattendtedfinal",
                                    "nonhospPreventedfraction")
        
        indicator_results1<-cbind(indicator_results_titles, indicator_results1)
        
        
        return(indicator_results1)
        
        
    })#burdendataset_coverage4timing2
    
    
    #######################################################
    
    ##Deployment coverage 4, timing 3
    ######################################################
    
    
    burdendataset_coverage4timing3<- reactive({
        ################################ The output data when insufficient data has been entered   ##################################
        ############################################################################################################################
        
        
        if(scenario()=="Impact of current influenza vaccination campaign"){
            
            
            ##########################################################################################
            # Scenario of influenza circulation in presence of vaccine: Presence to absence of vaccine
            # This is a situation where the country conducts a vaccination campaign, we enter
            # actual burden inputs and vaccine coverage then compare this to a hypothetical scenario
            # where there is no vaccination campaign conducted
            ##########################################################################################
            
            
            
            
            hospinf<-c(hospitalizationsmonthly(), hospmonth2(), hospmonth3(), hospmonth4(), hospmonth5(), hospmonth6(),
                       hospmonth7(), hospmonth8(), hospmonth9(), hospmonth10(), hospmonth11(), hospmonth12())
            
            populationsize<-as.numeric(as.character(populationsize()))
            vaccineeffectiveness<-as.numeric(as.character(vaccineeffectiveness()))
            hnh<-as.numeric(as.character(hnh()))
            mai<-as.numeric(as.character(mai()))
            hospinf<-as.numeric(as.character(hospinf))
            
            novaccine<-hospinf%>%
                as.data.frame()%>%
                rename(hospinfwvac=1)%>%
                mutate(hospinfcumulwvac=cumsum(hospinfwvac))%>%
                mutate(nonhospinfwvac=hospinfwvac*hnh)%>%
                mutate(nonhospinfcumulwvac=cumsum(nonhospinfwvac))%>%
                mutate(overallinfwvac=hospinfwvac+nonhospinfwvac)%>%
                mutate(overallinfcumulwvac=cumsum(overallinfwvac))%>%
                mutate(medicalattedtoinfwvac=overallinfwvac*mai)%>%
                mutate(medicallyattendedinfcumulwvac=cumsum(medicalattedtoinfwvac))
            
            
            
            ##bind monthly coverage
            monthvaccov<-c(vaccinecoveragemonthly(), month2(), month3(), month4(), month5(), month6(),
                           month7(), month8(), month9(), month10(), month11(), month12())
            
            monthvaccov<-as.numeric(as.character(monthvaccov))
            
            totcoverage<-sum(monthvaccov)
            
            
            novaccine<-cbind(novaccine, monthvaccov)
            
            novaccine$covergdistrib<- c(vaccinecoveragemonthly3(), month23(), month33(), month43(), month53(), month63(), month73(), month83(), month93(), month103(), month113(), month123())
            
            novaccine$covergdistrib<-as.numeric(as.character(novaccine$covergdistrib))
            
            novaccine$monthvaccov<- novaccine$covergdistrib*coverage4() ## replace original monthvaccov column with the new monthly coverage - this is so that the variable doesn't have to be changed in the rest of the script
            
            
            ## month number
            novaccine$monthnum<-c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
            
            ## add monthly effectively vaccinated
            novaccine<-novaccine%>%
                mutate(effectivelyvacc=populationsize*monthvaccov*vaccineeffectiveness)%>%
                mutate(effectivelyvacccumul=cumsum(effectivelyvacc))
            
            
            
            
            #####################################################################
            # Estimated rates
            #####################################################################
            novaccine<-novaccine%>%
                mutate(susceptiblewvacc=populationsize-hospinfcumulwvac-nonhospinfcumulwvac-effectivelyvacccumul)
            
            #Create columns
            novaccine$irhospinf<-NA
            novaccine$irnonhospinf<-NA
            
            #Create first row
            novaccine[1,14]<-novaccine$hospinfwvac[1]/populationsize
            novaccine[1,15]<-novaccine$nonhospinfwvac[1]/populationsize
            
            
            ##Calculation uses last month's susceptible population
            for(i in 2:nrow(novaccine)){
                novaccine$irhospinf[i]<-novaccine$hospinfwvac[i]/novaccine$susceptiblewvacc[i-1]
                novaccine$irnonhospinf[i]<- novaccine$nonhospinfwvac[i]/novaccine$susceptiblewvacc[i-1]
            }
            
            #####################################################################
            # Scenario of influenza circulation in absence of vaccine
            #####################################################################
            
            ## These calculations are initialized by the 'Susceptible at start of month in absence of vaccine'
            ## column, which starts with the Target Population in the first month.
            
            remainsuscstartmonth<-c(populationsize, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
            novaccine<-cbind(novaccine, remainsuscstartmonth)
            
            #Create columns
            novaccine$hospinfwovac<-NA
            novaccine$hospinfcumulwovacc<-NA
            novaccine$nonhospinfwovac<-NA
            novaccine$nonhospinfcumulwovacc<-NA
            novaccine$remainingsusc<-NA
            
            #Create first row
            novaccine[1,17]<-novaccine$remainsuscstartmonth[1]*novaccine$irhospinf[1]
            novaccine[1,18]<-novaccine$hospinfwovac[1]
            novaccine[1,19]<-novaccine$remainsuscstartmonth[1]*novaccine$irnonhospinf[1]
            novaccine[1,20]<-novaccine$nonhospinfwovac[1]
            novaccine[1,21]<-populationsize-novaccine$hospinfcumulwovacc[1]-novaccine$nonhospinfcumulwovacc[1]
            
            
            #Create rows 2:12
            for(i in 2:nrow(novaccine)){
                novaccine$remainsuscstartmonth[i]<-novaccine$remainingsusc[i-1]
                novaccine$hospinfwovac[i]<-novaccine$remainsuscstartmonth[i]*novaccine$irhospinf[i]
                novaccine$hospinfcumulwovacc[i]<-sum(novaccine$hospinfwovac[1:i])
                novaccine$nonhospinfwovac[i]<-novaccine$remainsuscstartmonth[i]*novaccine$irnonhospinf[i]
                novaccine$nonhospinfcumulwovacc[i]<-sum(novaccine$nonhospinfwovac[1:i])
                novaccine$remainingsusc[i]<-populationsize-novaccine$hospinfcumulwovacc[i]-novaccine$nonhospinfcumulwovacc[i]
            }
            
            novaccine<-novaccine%>%
                mutate(overallinfwovac=hospinfwovac+nonhospinfwovac)%>%
                mutate(overallinfcumulwovacc=cumsum(overallinfwovac))%>%
                mutate(medicallyattendedinfwovac=overallinfwovac*mai)%>%
                mutate(medicalattedtoinfcumulwovacc=cumsum(medicallyattendedinfwovac))
            
            ######## Avoided events
            novaccine<-novaccine%>%
                mutate(AEhospinf=hospinfwovac-hospinfwvac)%>%
                mutate(AEnonhospinf=nonhospinfwovac-nonhospinfwvac)%>%
                mutate(AEmedicalattedtoinf=medicallyattendedinfwovac-medicalattedtoinfwvac)%>%
                mutate(AEoverallinf=overallinfwovac-overallinfwvac)
            
            
            ################ Prevented fraction
            ## Hospitalized infections
            ifelse (monthstartvac3()==1,
                    novaccine<-novaccine%>%
                        mutate(PFhospinf=sum(AEhospinf)/sum(hospinfwovac)),
                    novaccine<-novaccine%>%
                        mutate(PFhospinf=sum(AEhospinf[monthstartvac3():nrow(novaccine)]/sum(hospinfwovac[monthstartvac3():nrow(novaccine)])))
            )
            
            ## non-Hospitalized infections
            ifelse (monthstartvac3()==1,
                    novaccine<-novaccine%>%
                        mutate(PFnonhospinf=sum(AEnonhospinf)/sum(nonhospinfwovac)),
                    novaccine<-novaccine%>%
                        mutate(PFnonhospinf=sum(AEnonhospinf[monthstartvac3():nrow(novaccine)]/sum(nonhospinfwovac[monthstartvac3():nrow(novaccine)])))
            )
            
            
            ################ NNV -----------------------------------> why does this only go to month 8 in the Excel formula?????
            ## Hospitalized infections
            ifelse(monthstartvac3()==1,
                   novaccine<-novaccine%>%
                       mutate(NNVhospinf=1/((vaccineeffectiveness*(sum(hospinfwovac)))/populationsize)),
                   novaccine<-novaccine%>%
                       mutate(NNVhospinf=1/((vaccineeffectiveness*(sum(hospinfwovac[monthstartvac3():nrow(novaccine)])))/populationsize))
            )
            
            
            ## non-Hospitalized infections
            ifelse(monthstartvac3()==1,
                   novaccine<-novaccine%>%
                       mutate(NNVnonhospinf=1/((vaccineeffectiveness*(sum(nonhospinfwovac)))/populationsize)),
                   novaccine<-novaccine%>%
                       mutate(NNVnonhospinf=1/((vaccineeffectiveness*(sum(nonhospinfwovac[monthstartvac3():nrow(novaccine)])))/populationsize))
            )
            
            ## Medically infections
            ifelse(monthstartvac3()==1,
                   novaccine<-novaccine%>%
                       mutate(NNVmedicalattendedinf=1/((vaccineeffectiveness*(sum(medicallyattendedinfwovac)))/populationsize)),
                   novaccine<-novaccine%>%
                       mutate(NNVmedicalattendedinf=1/((vaccineeffectiveness*(sum(medicallyattendedinfwovac[monthstartvac3():nrow(novaccine)])))/populationsize))
            )
            
            ## Overall infections
            ifelse(monthstartvac3()==1,
                   novaccine<-novaccine%>%
                       mutate(NNVoverallinf=1/((vaccineeffectiveness*(sum(hospinfwovac)+sum(nonhospinfwovac)))/populationsize)),
                   novaccine<-novaccine%>%
                       mutate(NNVoverallinf=1/((vaccineeffectiveness*(sum(hospinfwovac[monthstartvac3():nrow(novaccine)])+sum(nonhospinfwovac[monthstartvac3():nrow(novaccine)])))/populationsize))
            )
            
            return(novaccine)
            
            
            
        } #close if current campaign
        
        
        
        if(scenario()=="Impact of hypothetical vaccination campaign"){
            
            ##########################################################################################
            # Scenario of influenza circulation in absence of vaccine: Absence to presence of vaccine
            # This is a scenario where the country has yet to conduct a vaccination campaign but would
            # like to see what impact a vaccine campaign may have.
            # The country enters the estimated number of hospitalization they observed and then we compare
            # this to a hypothetical vaccination campaign
            ##########################################################################################
            
            hospinf<-c(hospitalizationsmonthly(), hospmonth2(), hospmonth3(), hospmonth4(), hospmonth5(), hospmonth6(),
                       hospmonth7(), hospmonth8(), hospmonth9(), hospmonth10(), hospmonth11(), hospmonth12())
            
            populationsize<-as.numeric(as.character(populationsize()))
            hnh<-as.numeric(as.character(hnh()))
            mai<-as.numeric(as.character(mai()))
            hospinf<-as.numeric(as.character(hospinf))
            vaccineeffectiveness<-as.numeric(as.character(vaccineeffectiveness()))
            
            #####################Observed influenza in the absence of vaccine
            novaccine<-hospinf%>%
                as.data.frame()%>%
                rename(hospinfwovac=1)%>%
                mutate(hospinfcumulwovac=cumsum(hospinfwovac))%>%
                mutate(nonhospinfwovac=hospinfwovac*hnh)%>%
                mutate(nonhospinfcumulwovac=cumsum(nonhospinfwovac))%>%
                mutate(remainingsusc=populationsize-hospinfcumulwovac-nonhospinfcumulwovac)
            
            #create remainsuscstartmonth column with target population and rows 1:11 of remainingsusc
            remainsuscstartmonth<-populationsize%>%
                as.data.frame()%>%
                rename(remainsuscstartmonth=1)
            
            remainsuscstartmonth2<-as.data.frame(novaccine$remainingsusc[1:11])%>%
                as.data.frame()%>%
                rename(remainsuscstartmonth=1)
            
            remainsuscstartmonth<-rbind(remainsuscstartmonth, remainsuscstartmonth2)
            
            novaccine<-cbind(novaccine, remainsuscstartmonth)
            
            novaccine<-novaccine%>%
                mutate(overallinfwovac=hospinfwovac+nonhospinfwovac)%>%
                mutate(overallinfcumulwovac=cumsum(overallinfwovac))%>%
                mutate(medicallyattendedinfwovac=overallinfwovac*mai)%>%
                mutate(medicallyattendedinfcumulwovac=cumsum(medicallyattendedinfwovac))
            
            
            
            #####################################################################
            # Incidence rates in the absence of vaccine
            #####################################################################
            novaccine<-novaccine%>%
                mutate(irhospinf=hospinfwovac/remainsuscstartmonth)%>%
                mutate(irnonhospinf=nonhospinfwovac/remainsuscstartmonth)
            
            
            
            
            ############################ Burden inputs with hypothetical vaccination campaign
            #Create columns
            novaccine$hospinfwvac<-NA
            novaccine$hospinfcumulwvac<-NA
            novaccine$nonhospinfwvac<-NA
            novaccine$nonhospinfcumulwvac<-NA
            
            
            
            #Create first row
            novaccine[1,13]<-populationsize*novaccine$irhospinf[1]
            novaccine[1,14]<-novaccine$hospinfwvac[1]
            novaccine[1,15]<-populationsize*novaccine$irnonhospinf[1]
            novaccine[1,16]<-novaccine$nonhospinfwvac[1]
            
            ##bind monthly coverage
            monthvaccov<-c(vaccinecoveragemonthly(), month2(), month3(), month4(), month5(), month6(),
                           month7(), month8(), month9(), month10(), month11(), month12())
            
            monthvaccov<-as.numeric(as.character(monthvaccov))
            
            totcoverage<-sum(monthvaccov)
            
            
            novaccine<-cbind(novaccine, monthvaccov)
            
            novaccine$covergdistrib<- c(vaccinecoveragemonthly3(), month23(), month33(), month43(), month53(), month63(), month73(), month83(), month93(), month103(), month113(), month123())
            
            novaccine$covergdistrib<-as.numeric(as.character(novaccine$covergdistrib))
            
            novaccine$monthvaccov<- novaccine$covergdistrib*coverage4() ## replace original monthvaccov column with the new monthly coverage - this is so that the variable doesn't have to be changed in the rest of the script
            
            
            ##monthly effectively vaccinated and susceptible with hypothetical vaccination campaign
            novaccine<-novaccine%>%
                mutate(effectivelyvacc=populationsize*monthvaccov*vaccineeffectiveness)%>%
                mutate(effectivelycumulvacc=cumsum(effectivelyvacc))%>%
                mutate(suscwvacc=populationsize-hospinfcumulwvac-nonhospinfcumulwvac-effectivelyvacc)
            
            ##Then calculate the rest of hospinfwovac and nonhospinfwovac. Calculation uses last month's incidence rate
            for(i in 2:nrow(novaccine)){
                novaccine$hospinfwvac[i]<-novaccine$suscwvacc[i-1]*novaccine$irhospinf[i]
                novaccine$hospinfcumulwvac[i]<-sum(novaccine$hospinfwvac[1:i])
                novaccine$nonhospinfwvac[i]<- novaccine$suscwvacc[i-1]*novaccine$irnonhospinf[i]
                novaccine$nonhospinfcumulwvac[i]<-sum(novaccine$nonhospinfwvac[1:i])
                novaccine$suscwvacc[i]=populationsize-novaccine$hospinfcumulwvac[i]-novaccine$nonhospinfcumulwvac[i]-novaccine$effectivelycumulvacc[i]
            }
            
            
            ## month number
            novaccine$monthnum<-c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
            
            
            ## Overall infections with hypothetical vaccination campaign
            novaccine<-novaccine%>%
                mutate(overallinfwvac=hospinfwvac+nonhospinfwvac)%>%
                mutate(overallinfcumulwvacc=cumsum(overallinfwvac))
            
            ## Medically attended with hypothetical vaccination campaign
            novaccine<-novaccine%>%
                mutate(medicalattedtoinfwvac=overallinfwvac*mai)
            
            
            ######## Avoided events
            novaccine<-novaccine%>%
                mutate(AEhospinf=hospinfwovac-hospinfwvac)%>%
                mutate(AEnonhospinf=nonhospinfwovac-nonhospinfwvac)%>%
                mutate(AEmedicalattedtoinf=medicallyattendedinfwovac-medicalattedtoinfwvac)%>%
                mutate(AEoverallinf=overallinfwovac-overallinfwvac)
            
            
            
            ################ Prevented fraction
            ## Hospitalized infections
            ifelse (monthstartvac3()==1,
                    novaccine<-novaccine%>%
                        mutate(PFhospinf=sum(AEhospinf)/sum(hospinfwovac)),
                    novaccine<-novaccine%>%
                        mutate(PFhospinf=sum(AEhospinf[monthstartvac3():nrow(novaccine)]/sum(hospinfwovac[monthstartvac3():nrow(novaccine)])))
            )
            
            ## non-Hospitalized infections
            ifelse (monthstartvac3()==1,
                    novaccine<-novaccine%>%
                        mutate(PFnonhospinf=sum(AEnonhospinf)/sum(nonhospinfwovac)),
                    novaccine<-novaccine%>%
                        mutate(PFnonhospinf=sum(AEnonhospinf[monthstartvac3():nrow(novaccine)]/sum(nonhospinfwovac[monthstartvac3():nrow(novaccine)])))
            )
            
            
            
            
            ################ NNV -----------------------------------> why does this only go to month 8 in the Excel formula?????
            ## Hospitalized infections
            ifelse(monthstartvac3()==1,
                   novaccine<-novaccine%>%
                       mutate(NNVhospinf=1/((vaccineeffectiveness*(sum(hospinfwovac)))/populationsize)),
                   novaccine<-novaccine%>%
                       mutate(NNVhospinf=1/((vaccineeffectiveness*(sum(hospinfwovac[monthstartvac3():nrow(novaccine)])))/populationsize))
            )
            
            
            ## non-Hospitalized infections
            ifelse(monthstartvac3()==1,
                   novaccine<-novaccine%>%
                       mutate(NNVnonhospinf=1/((vaccineeffectiveness*(sum(nonhospinfwovac)))/populationsize)),
                   novaccine<-novaccine%>%
                       mutate(NNVnonhospinf=1/((vaccineeffectiveness*(sum(nonhospinfwovac[monthstartvac3():nrow(novaccine)])))/populationsize))
            )
            
            ## Medically attended infections
            ifelse(monthstartvac3()==1,
                   novaccine<-novaccine%>%
                       mutate(NNVmedicalattendedinf=1/((vaccineeffectiveness*(sum(medicallyattendedinfwovac)))/populationsize)),
                   novaccine<-novaccine%>%
                       mutate(NNVmedicalattendedinf=1/((vaccineeffectiveness*(sum(medicallyattendedinfwovac[monthstartvac3():nrow(novaccine)])))/populationsize))
            )
            
            ## Total infections
            ifelse(monthstartvac3()==1,
                   novaccine<-novaccine%>%
                       mutate(NNVoverallinf=1/((vaccineeffectiveness*(sum(hospinfwovac)+sum(nonhospinfwovac)))/populationsize)),
                   novaccine<-novaccine%>%
                       mutate(NNVoverallinf=1/((vaccineeffectiveness*(sum(hospinfwovac[monthstartvac3():nrow(novaccine)])+sum(nonhospinfwovac[monthstartvac3():nrow(novaccine)])))/populationsize))
            )
            
            return(novaccine)
            
            
        }#close "if hypothetical campaign"
        
    }) #close burdendataset
    
    
    
    ####################################################################################################
    ## List of burden averted results
    ####################################################################################################
    
    
    burdenavertedresultscoverage4timing3<-reactive({
        
        populationsize<-as.numeric(as.character(populationsize()))
        hnh<-as.numeric(as.character(hnh()))
        mai<-as.numeric(as.character(mai()))
        vaccineeffectiveness<-as.numeric(as.character(vaccineeffectiveness()))
        
        
        
        #################################################
        ## Results
        #################################################
        nonhospinfwovactot<-sum(burdendataset_coverage4timing3()$nonhospinfwovac)
        nonhospinfwvacctot<-sum(burdendataset_coverage4timing3()$nonhospinfwvac)
        nonhospinfaverted<-nonhospinfwovactot-nonhospinfwvacctot
        NNVnonhospinffinal<-burdendataset_coverage4timing3()$NNVnonhospinf[1]
        
        medicallyattendedinfwovactot<-sum(burdendataset_coverage4timing3()$medicallyattendedinfwovac)
        medicallyattendedinfwvactot<-sum(burdendataset_coverage4timing3()$medicalattedtoinfwvac)
        medicallyattendedinfaverted<-medicallyattendedinfwovactot-medicallyattendedinfwvactot
        NNVmedicallyattendtedfinal<-burdendataset_coverage4timing3()$NNVmedicalattendedinf[1]
        
        Totalinfwovacc<-sum(burdendataset_coverage4timing3()$overallinfwovac)
        Totalinfwvacc<-sum(burdendataset_coverage4timing3()$overallinfwvac)
        Totalinfaverted<-Totalinfwovacc-Totalinfwvacc
        NNVtotalinf<-burdendataset_coverage4timing3()$NNVoverallinf[1]
        
        hospinfwovactot<-sum(burdendataset_coverage4timing3()$hospinfwovac)
        hospinfwvacctot<-sum(burdendataset_coverage4timing3()$hospinfwvac)
        hospaverted<-hospinfwovactot-hospinfwvacctot
        NNVhospinffinal<-burdendataset_coverage4timing3()$NNVhospinf[1]
        
        
        nonhospPreventedfraction<-burdendataset_coverage4timing3()$PFnonhospinf[1]
        hospPreventedfraction<-burdendataset_coverage4timing3()$PFhospinf[1]
        
        
        indicator_results1<-c(nonhospinfwovactot, nonhospinfwvacctot, nonhospinfaverted, NNVnonhospinffinal,
                              Totalinfwovacc, Totalinfwvacc, Totalinfaverted, NNVtotalinf,
                              hospinfwovactot, hospinfwvacctot, hospaverted, NNVhospinffinal,
                              medicallyattendedinfwovactot, medicallyattendedinfwvactot, medicallyattendedinfaverted, NNVmedicallyattendtedfinal,
                              nonhospPreventedfraction)
        
        indicator_results_titles<-c("nonhospinfwovactot", "nonhospinfwvacctot", "nonhospinfaverted", "NNVnonhospinffinal",
                                    "Totalinfwovacc", "Totalinfwvacc", "Totalinfaverted", "NNVtotalinf",
                                    "hospinfwovactot", "hospinfwvacctot", "hospaverted", "NNVhospinffinal",
                                    "medicallyattendedinfwovactot", "medicallyattendedinfwvactot", "medicallyattendedinfaverted", "NNVmedicallyattendtedfinal",
                                    "nonhospPreventedfraction")
        
        indicator_results1<-cbind(indicator_results_titles, indicator_results1)
        
        
        return(indicator_results1)
        
        
    })#burdendataset_coverage4timing3
    
    
    
    
    #######################################################
    
    ##Deployment coverage 1, timing 4
    ######################################################
    
    
    burdendataset_coverage4timing4<- reactive({
        ################################ The output data when insufficient data has been entered   ##################################
        ############################################################################################################################
        
        
        if(scenario()=="Impact of current influenza vaccination campaign"){
            
            
            ##########################################################################################
            # Scenario of influenza circulation in presence of vaccine: Presence to absence of vaccine
            # This is a situation where the country conducts a vaccination campaign, we enter
            # actual burden inputs and vaccine coverage then compare this to a hypothetical scenario
            # where there is no vaccination campaign conducted
            ##########################################################################################
            
            
            
            
            hospinf<-c(hospitalizationsmonthly(), hospmonth2(), hospmonth3(), hospmonth4(), hospmonth5(), hospmonth6(),
                       hospmonth7(), hospmonth8(), hospmonth9(), hospmonth10(), hospmonth11(), hospmonth12())
            
            populationsize<-as.numeric(as.character(populationsize()))
            vaccineeffectiveness<-as.numeric(as.character(vaccineeffectiveness()))
            hnh<-as.numeric(as.character(hnh()))
            mai<-as.numeric(as.character(mai()))
            hospinf<-as.numeric(as.character(hospinf))
            
            novaccine<-hospinf%>%
                as.data.frame()%>%
                rename(hospinfwvac=1)%>%
                mutate(hospinfcumulwvac=cumsum(hospinfwvac))%>%
                mutate(nonhospinfwvac=hospinfwvac*hnh)%>%
                mutate(nonhospinfcumulwvac=cumsum(nonhospinfwvac))%>%
                mutate(overallinfwvac=hospinfwvac+nonhospinfwvac)%>%
                mutate(overallinfcumulwvac=cumsum(overallinfwvac))%>%
                mutate(medicalattedtoinfwvac=overallinfwvac*mai)%>%
                mutate(medicallyattendedinfcumulwvac=cumsum(medicalattedtoinfwvac))
            
            
            
            ##bind monthly coverage
            monthvaccov<-c(vaccinecoveragemonthly(), month2(), month3(), month4(), month5(), month6(),
                           month7(), month8(), month9(), month10(), month11(), month12())
            
            monthvaccov<-as.numeric(as.character(monthvaccov))
            
            totcoverage<-sum(monthvaccov)
            
            
            novaccine<-cbind(novaccine, monthvaccov)
            
            novaccine$covergdistrib<- c(vaccinecoveragemonthly4(), month24(), month34(), month44(), month54(), month64(), month74(), month84(), month94(), month104(), month114(), month124())
            
            novaccine$covergdistrib<-as.numeric(as.character(novaccine$covergdistrib))
            
            novaccine$monthvaccov<- novaccine$covergdistrib*coverage4() ## replace original monthvaccov column with the new monthly coverage - this is so that the variable doesn't have to be changed in the rest of the script
            
            
            ## month number
            novaccine$monthnum<-c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
            
            ## add monthly effectively vaccinated
            novaccine<-novaccine%>%
                mutate(effectivelyvacc=populationsize*monthvaccov*vaccineeffectiveness)%>%
                mutate(effectivelyvacccumul=cumsum(effectivelyvacc))
            
            
            
            
            #####################################################################
            # Estimated rates
            #####################################################################
            novaccine<-novaccine%>%
                mutate(susceptiblewvacc=populationsize-hospinfcumulwvac-nonhospinfcumulwvac-effectivelyvacccumul)
            
            #Create columns
            novaccine$irhospinf<-NA
            novaccine$irnonhospinf<-NA
            
            #Create first row
            novaccine[1,14]<-novaccine$hospinfwvac[1]/populationsize
            novaccine[1,15]<-novaccine$nonhospinfwvac[1]/populationsize
            
            
            ##Calculation uses last month's susceptible population
            for(i in 2:nrow(novaccine)){
                novaccine$irhospinf[i]<-novaccine$hospinfwvac[i]/novaccine$susceptiblewvacc[i-1]
                novaccine$irnonhospinf[i]<- novaccine$nonhospinfwvac[i]/novaccine$susceptiblewvacc[i-1]
            }
            
            #####################################################################
            # Scenario of influenza circulation in absence of vaccine
            #####################################################################
            
            ## These calculations are initialized by the 'Susceptible at start of month in absence of vaccine'
            ## column, which starts with the Target Population in the first month.
            
            remainsuscstartmonth<-c(populationsize, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
            novaccine<-cbind(novaccine, remainsuscstartmonth)
            
            #Create columns
            novaccine$hospinfwovac<-NA
            novaccine$hospinfcumulwovacc<-NA
            novaccine$nonhospinfwovac<-NA
            novaccine$nonhospinfcumulwovacc<-NA
            novaccine$remainingsusc<-NA
            
            #Create first row
            novaccine[1,17]<-novaccine$remainsuscstartmonth[1]*novaccine$irhospinf[1]
            novaccine[1,18]<-novaccine$hospinfwovac[1]
            novaccine[1,19]<-novaccine$remainsuscstartmonth[1]*novaccine$irnonhospinf[1]
            novaccine[1,20]<-novaccine$nonhospinfwovac[1]
            novaccine[1,21]<-populationsize-novaccine$hospinfcumulwovacc[1]-novaccine$nonhospinfcumulwovacc[1]
            
            
            #Create rows 2:12
            for(i in 2:nrow(novaccine)){
                novaccine$remainsuscstartmonth[i]<-novaccine$remainingsusc[i-1]
                novaccine$hospinfwovac[i]<-novaccine$remainsuscstartmonth[i]*novaccine$irhospinf[i]
                novaccine$hospinfcumulwovacc[i]<-sum(novaccine$hospinfwovac[1:i])
                novaccine$nonhospinfwovac[i]<-novaccine$remainsuscstartmonth[i]*novaccine$irnonhospinf[i]
                novaccine$nonhospinfcumulwovacc[i]<-sum(novaccine$nonhospinfwovac[1:i])
                novaccine$remainingsusc[i]<-populationsize-novaccine$hospinfcumulwovacc[i]-novaccine$nonhospinfcumulwovacc[i]
            }
            
            novaccine<-novaccine%>%
                mutate(overallinfwovac=hospinfwovac+nonhospinfwovac)%>%
                mutate(overallinfcumulwovacc=cumsum(overallinfwovac))%>%
                mutate(medicallyattendedinfwovac=overallinfwovac*mai)%>%
                mutate(medicalattedtoinfcumulwovacc=cumsum(medicallyattendedinfwovac))
            
            ######## Avoided events
            novaccine<-novaccine%>%
                mutate(AEhospinf=hospinfwovac-hospinfwvac)%>%
                mutate(AEnonhospinf=nonhospinfwovac-nonhospinfwvac)%>%
                mutate(AEmedicalattedtoinf=medicallyattendedinfwovac-medicalattedtoinfwvac)%>%
                mutate(AEoverallinf=overallinfwovac-overallinfwvac)
            
            
            ################ Prevented fraction
            ## Hospitalized infections
            ifelse (monthstartvac4()==1,
                    novaccine<-novaccine%>%
                        mutate(PFhospinf=sum(AEhospinf)/sum(hospinfwovac)),
                    novaccine<-novaccine%>%
                        mutate(PFhospinf=sum(AEhospinf[monthstartvac4():nrow(novaccine)]/sum(hospinfwovac[monthstartvac4():nrow(novaccine)])))
            )
            
            ## non-Hospitalized infections
            ifelse (monthstartvac4()==1,
                    novaccine<-novaccine%>%
                        mutate(PFnonhospinf=sum(AEnonhospinf)/sum(nonhospinfwovac)),
                    novaccine<-novaccine%>%
                        mutate(PFnonhospinf=sum(AEnonhospinf[monthstartvac4():nrow(novaccine)]/sum(nonhospinfwovac[monthstartvac4():nrow(novaccine)])))
            )
            
            
            ################ NNV -----------------------------------> why does this only go to month 8 in the Excel formula?????
            ## Hospitalized infections
            ifelse(monthstartvac4()==1,
                   novaccine<-novaccine%>%
                       mutate(NNVhospinf=1/((vaccineeffectiveness*(sum(hospinfwovac)))/populationsize)),
                   novaccine<-novaccine%>%
                       mutate(NNVhospinf=1/((vaccineeffectiveness*(sum(hospinfwovac[monthstartvac4():nrow(novaccine)])))/populationsize))
            )
            
            
            ## non-Hospitalized infections
            ifelse(monthstartvac4()==1,
                   novaccine<-novaccine%>%
                       mutate(NNVnonhospinf=1/((vaccineeffectiveness*(sum(nonhospinfwovac)))/populationsize)),
                   novaccine<-novaccine%>%
                       mutate(NNVnonhospinf=1/((vaccineeffectiveness*(sum(nonhospinfwovac[monthstartvac4():nrow(novaccine)])))/populationsize))
            )
            
            ## Medically infections
            ifelse(monthstartvac4()==1,
                   novaccine<-novaccine%>%
                       mutate(NNVmedicalattendedinf=1/((vaccineeffectiveness*(sum(medicallyattendedinfwovac)))/populationsize)),
                   novaccine<-novaccine%>%
                       mutate(NNVmedicalattendedinf=1/((vaccineeffectiveness*(sum(medicallyattendedinfwovac[monthstartvac4():nrow(novaccine)])))/populationsize))
            )
            
            ## Overall infections
            ifelse(monthstartvac4()==1,
                   novaccine<-novaccine%>%
                       mutate(NNVoverallinf=1/((vaccineeffectiveness*(sum(hospinfwovac)+sum(nonhospinfwovac)))/populationsize)),
                   novaccine<-novaccine%>%
                       mutate(NNVoverallinf=1/((vaccineeffectiveness*(sum(hospinfwovac[monthstartvac4():nrow(novaccine)])+sum(nonhospinfwovac[monthstartvac4():nrow(novaccine)])))/populationsize))
            )
            
            return(novaccine)
            
            
            
        } #close if current campaign
        
        
        
        if(scenario()=="Impact of hypothetical vaccination campaign"){
            
            ##########################################################################################
            # Scenario of influenza circulation in absence of vaccine: Absence to presence of vaccine
            # This is a scenario where the country has yet to conduct a vaccination campaign but would
            # like to see what impact a vaccine campaign may have.
            # The country enters the estimated number of hospitalization they observed and then we compare
            # this to a hypothetical vaccination campaign
            ##########################################################################################
            
            hospinf<-c(hospitalizationsmonthly(), hospmonth2(), hospmonth3(), hospmonth4(), hospmonth5(), hospmonth6(),
                       hospmonth7(), hospmonth8(), hospmonth9(), hospmonth10(), hospmonth11(), hospmonth12())
            
            populationsize<-as.numeric(as.character(populationsize()))
            hnh<-as.numeric(as.character(hnh()))
            mai<-as.numeric(as.character(mai()))
            hospinf<-as.numeric(as.character(hospinf))
            vaccineeffectiveness<-as.numeric(as.character(vaccineeffectiveness()))
            
            #####################Observed influenza in the absence of vaccine
            novaccine<-hospinf%>%
                as.data.frame()%>%
                rename(hospinfwovac=1)%>%
                mutate(hospinfcumulwovac=cumsum(hospinfwovac))%>%
                mutate(nonhospinfwovac=hospinfwovac*hnh)%>%
                mutate(nonhospinfcumulwovac=cumsum(nonhospinfwovac))%>%
                mutate(remainingsusc=populationsize-hospinfcumulwovac-nonhospinfcumulwovac)
            
            #create remainsuscstartmonth column with target population and rows 1:11 of remainingsusc
            remainsuscstartmonth<-populationsize%>%
                as.data.frame()%>%
                rename(remainsuscstartmonth=1)
            
            remainsuscstartmonth2<-as.data.frame(novaccine$remainingsusc[1:11])%>%
                as.data.frame()%>%
                rename(remainsuscstartmonth=1)
            
            remainsuscstartmonth<-rbind(remainsuscstartmonth, remainsuscstartmonth2)
            
            novaccine<-cbind(novaccine, remainsuscstartmonth)
            
            novaccine<-novaccine%>%
                mutate(overallinfwovac=hospinfwovac+nonhospinfwovac)%>%
                mutate(overallinfcumulwovac=cumsum(overallinfwovac))%>%
                mutate(medicallyattendedinfwovac=overallinfwovac*mai)%>%
                mutate(medicallyattendedinfcumulwovac=cumsum(medicallyattendedinfwovac))
            
            
            
            #####################################################################
            # Incidence rates in the absence of vaccine
            #####################################################################
            novaccine<-novaccine%>%
                mutate(irhospinf=hospinfwovac/remainsuscstartmonth)%>%
                mutate(irnonhospinf=nonhospinfwovac/remainsuscstartmonth)
            
            
            
            
            ############################ Burden inputs with hypothetical vaccination campaign
            #Create columns
            novaccine$hospinfwvac<-NA
            novaccine$hospinfcumulwvac<-NA
            novaccine$nonhospinfwvac<-NA
            novaccine$nonhospinfcumulwvac<-NA
            
            
            
            #Create first row
            novaccine[1,13]<-populationsize*novaccine$irhospinf[1]
            novaccine[1,14]<-novaccine$hospinfwvac[1]
            novaccine[1,15]<-populationsize*novaccine$irnonhospinf[1]
            novaccine[1,16]<-novaccine$nonhospinfwvac[1]
            
            ##bind monthly coverage
            monthvaccov<-c(vaccinecoveragemonthly(), month2(), month3(), month4(), month5(), month6(),
                           month7(), month8(), month9(), month10(), month11(), month12())
            
            monthvaccov<-as.numeric(as.character(monthvaccov))
            
            totcoverage<-sum(monthvaccov)
            
            
            novaccine<-cbind(novaccine, monthvaccov)
            
            novaccine$covergdistrib<- c(vaccinecoveragemonthly4(), month24(), month34(), month44(), month54(), month64(), month74(), month84(), month94(), month104(), month114(), month124())
            
            novaccine$covergdistrib<-as.numeric(as.character(novaccine$covergdistrib))
            
            novaccine$monthvaccov<- novaccine$covergdistrib*coverage4() ## replace original monthvaccov column with the new monthly coverage - this is so that the variable doesn't have to be changed in the rest of the script
            
            
            ##monthly effectively vaccinated and susceptible with hypothetical vaccination campaign
            novaccine<-novaccine%>%
                mutate(effectivelyvacc=populationsize*monthvaccov*vaccineeffectiveness)%>%
                mutate(effectivelycumulvacc=cumsum(effectivelyvacc))%>%
                mutate(suscwvacc=populationsize-hospinfcumulwvac-nonhospinfcumulwvac-effectivelyvacc)
            
            ##Then calculate the rest of hospinfwovac and nonhospinfwovac. Calculation uses last month's incidence rate
            for(i in 2:nrow(novaccine)){
                novaccine$hospinfwvac[i]<-novaccine$suscwvacc[i-1]*novaccine$irhospinf[i]
                novaccine$hospinfcumulwvac[i]<-sum(novaccine$hospinfwvac[1:i])
                novaccine$nonhospinfwvac[i]<- novaccine$suscwvacc[i-1]*novaccine$irnonhospinf[i]
                novaccine$nonhospinfcumulwvac[i]<-sum(novaccine$nonhospinfwvac[1:i])
                novaccine$suscwvacc[i]=populationsize-novaccine$hospinfcumulwvac[i]-novaccine$nonhospinfcumulwvac[i]-novaccine$effectivelycumulvacc[i]
            }
            
            
            ## month number
            novaccine$monthnum<-c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
            
            
            ## Overall infections with hypothetical vaccination campaign
            novaccine<-novaccine%>%
                mutate(overallinfwvac=hospinfwvac+nonhospinfwvac)%>%
                mutate(overallinfcumulwvacc=cumsum(overallinfwvac))
            
            ## Medically attended with hypothetical vaccination campaign
            novaccine<-novaccine%>%
                mutate(medicalattedtoinfwvac=overallinfwvac*mai)
            
            
            ######## Avoided events
            novaccine<-novaccine%>%
                mutate(AEhospinf=hospinfwovac-hospinfwvac)%>%
                mutate(AEnonhospinf=nonhospinfwovac-nonhospinfwvac)%>%
                mutate(AEmedicalattedtoinf=medicallyattendedinfwovac-medicalattedtoinfwvac)%>%
                mutate(AEoverallinf=overallinfwovac-overallinfwvac)
            
            
            
            ################ Prevented fraction
            ## Hospitalized infections
            ifelse (monthstartvac4()==1,
                    novaccine<-novaccine%>%
                        mutate(PFhospinf=sum(AEhospinf)/sum(hospinfwovac)),
                    novaccine<-novaccine%>%
                        mutate(PFhospinf=sum(AEhospinf[monthstartvac4():nrow(novaccine)]/sum(hospinfwovac[monthstartvac4():nrow(novaccine)])))
            )
            
            ## non-Hospitalized infections
            ifelse (monthstartvac4()==1,
                    novaccine<-novaccine%>%
                        mutate(PFnonhospinf=sum(AEnonhospinf)/sum(nonhospinfwovac)),
                    novaccine<-novaccine%>%
                        mutate(PFnonhospinf=sum(AEnonhospinf[monthstartvac4():nrow(novaccine)]/sum(nonhospinfwovac[monthstartvac4():nrow(novaccine)])))
            )
            
            
            
            
            ################ NNV -----------------------------------> why does this only go to month 8 in the Excel formula?????
            ## Hospitalized infections
            ifelse(monthstartvac4()==1,
                   novaccine<-novaccine%>%
                       mutate(NNVhospinf=1/((vaccineeffectiveness*(sum(hospinfwovac)))/populationsize)),
                   novaccine<-novaccine%>%
                       mutate(NNVhospinf=1/((vaccineeffectiveness*(sum(hospinfwovac[monthstartvac4():nrow(novaccine)])))/populationsize))
            )
            
            
            ## non-Hospitalized infections
            ifelse(monthstartvac4()==1,
                   novaccine<-novaccine%>%
                       mutate(NNVnonhospinf=1/((vaccineeffectiveness*(sum(nonhospinfwovac)))/populationsize)),
                   novaccine<-novaccine%>%
                       mutate(NNVnonhospinf=1/((vaccineeffectiveness*(sum(nonhospinfwovac[monthstartvac4():nrow(novaccine)])))/populationsize))
            )
            
            ## Medically attended infections
            ifelse(monthstartvac4()==1,
                   novaccine<-novaccine%>%
                       mutate(NNVmedicalattendedinf=1/((vaccineeffectiveness*(sum(medicallyattendedinfwovac)))/populationsize)),
                   novaccine<-novaccine%>%
                       mutate(NNVmedicalattendedinf=1/((vaccineeffectiveness*(sum(medicallyattendedinfwovac[monthstartvac4():nrow(novaccine)])))/populationsize))
            )
            
            ## Total infections
            ifelse(monthstartvac4()==1,
                   novaccine<-novaccine%>%
                       mutate(NNVoverallinf=1/((vaccineeffectiveness*(sum(hospinfwovac)+sum(nonhospinfwovac)))/populationsize)),
                   novaccine<-novaccine%>%
                       mutate(NNVoverallinf=1/((vaccineeffectiveness*(sum(hospinfwovac[monthstartvac4():nrow(novaccine)])+sum(nonhospinfwovac[monthstartvac4():nrow(novaccine)])))/populationsize))
            )
            
            return(novaccine)
            
            
        }#close "if hypothetical campaign"
        
    }) #close burdendataset
    
    
    
    ####################################################################################################
    ## List of burden averted results
    ####################################################################################################
    
    
    burdenavertedresultscoverage4timing4<-reactive({
        
        populationsize<-as.numeric(as.character(populationsize()))
        hnh<-as.numeric(as.character(hnh()))
        mai<-as.numeric(as.character(mai()))
        vaccineeffectiveness<-as.numeric(as.character(vaccineeffectiveness()))
        
        
        
        #################################################
        ## Results
        #################################################
        nonhospinfwovactot<-sum(burdendataset_coverage4timing4()$nonhospinfwovac)
        nonhospinfwvacctot<-sum(burdendataset_coverage4timing4()$nonhospinfwvac)
        nonhospinfaverted<-nonhospinfwovactot-nonhospinfwvacctot
        NNVnonhospinffinal<-burdendataset_coverage4timing4()$NNVnonhospinf[1]
        
        medicallyattendedinfwovactot<-sum(burdendataset_coverage4timing4()$medicallyattendedinfwovac)
        medicallyattendedinfwvactot<-sum(burdendataset_coverage4timing4()$medicalattedtoinfwvac)
        medicallyattendedinfaverted<-medicallyattendedinfwovactot-medicallyattendedinfwvactot
        NNVmedicallyattendtedfinal<-burdendataset_coverage4timing4()$NNVmedicalattendedinf[1]
        
        Totalinfwovacc<-sum(burdendataset_coverage4timing4()$overallinfwovac)
        Totalinfwvacc<-sum(burdendataset_coverage4timing4()$overallinfwvac)
        Totalinfaverted<-Totalinfwovacc-Totalinfwvacc
        NNVtotalinf<-burdendataset_coverage4timing4()$NNVoverallinf[1]
        
        hospinfwovactot<-sum(burdendataset_coverage4timing4()$hospinfwovac)
        hospinfwvacctot<-sum(burdendataset_coverage4timing4()$hospinfwvac)
        hospaverted<-hospinfwovactot-hospinfwvacctot
        NNVhospinffinal<-burdendataset_coverage4timing4()$NNVhospinf[1]
        
        
        nonhospPreventedfraction<-burdendataset_coverage4timing4()$PFnonhospinf[1]
        hospPreventedfraction<-burdendataset_coverage4timing4()$PFhospinf[1]
        
        
        indicator_results1<-c(nonhospinfwovactot, nonhospinfwvacctot, nonhospinfaverted, NNVnonhospinffinal,
                              Totalinfwovacc, Totalinfwvacc, Totalinfaverted, NNVtotalinf,
                              hospinfwovactot, hospinfwvacctot, hospaverted, NNVhospinffinal,
                              medicallyattendedinfwovactot, medicallyattendedinfwvactot, medicallyattendedinfaverted, NNVmedicallyattendtedfinal,
                              nonhospPreventedfraction)
        
        indicator_results_titles<-c("nonhospinfwovactot", "nonhospinfwvacctot", "nonhospinfaverted", "NNVnonhospinffinal",
                                    "Totalinfwovacc", "Totalinfwvacc", "Totalinfaverted", "NNVtotalinf",
                                    "hospinfwovactot", "hospinfwvacctot", "hospaverted", "NNVhospinffinal",
                                    "medicallyattendedinfwovactot", "medicallyattendedinfwvactot", "medicallyattendedinfaverted", "NNVmedicallyattendtedfinal",
                                    "nonhospPreventedfraction")
        
        indicator_results1<-cbind(indicator_results_titles, indicator_results1)
        
        
        return(indicator_results1)
        
        
    })#burdendataset_coverage4timing4
    
    
    
    
    #################################################################################################################################
    #################################################################################################################################
    
    
    ##Coverage and timing - combined
    #################################################################################################################################
    #################################################################################################################################   
    
    gt_table_covtiming<- reactive({
        
        ####################################
        ## Render results table
        ####################################
        
        populationsize<-as.numeric(as.character(populationsize()))
        hnh<-as.numeric(as.character(hnh()))
        mai<-as.numeric(as.character(mai()))
        vaccineeffectiveness<-as.numeric(as.character(vaccineeffectiveness()))
        VELCI<-as.numeric(as.character(VELCI()))
        VEUCI<-as.numeric(as.character(VEUCI()))
        
        
        
        burdenavertedresults<-burdenavertedresults()%>%
            as.data.frame()%>%
            mutate(indicator_results=as.numeric(indicator_results))%>%
            select("basecovbasetiming" = indicator_results)
        

        burdenavertedresults_timing1<-burdenavertedresults_timing1()%>%
            as.data.frame()%>%
            mutate(indicator_results1=as.numeric(indicator_results1))%>%
            select("basecovtiming1" = indicator_results1)
        
        burdenavertedresults_timing2<-burdenavertedresults_timing2()%>%
            as.data.frame()%>%
            mutate(indicator_results2=as.numeric(indicator_results2))%>%
            select("basecovtiming2" = indicator_results2)
        
        
        burdenavertedresults_timing3<-burdenavertedresults_timing3()%>%
            as.data.frame()%>%
            mutate(indicator_results3=as.numeric(indicator_results3))%>%
            select("basecovtiming3" =indicator_results3)
        
        burdenavertedresults_timing4<-burdenavertedresults_timing4()%>%
            as.data.frame()%>%
            mutate(indicator_results4=as.numeric(indicator_results4))%>%
            select("basecovtiming4" =indicator_results4)
        
        
        burdenavertedresultscoverage1<-burdenavertedresultscoverage1()%>%
            as.data.frame()%>%
            mutate(indicator_results1=as.numeric(indicator_results1))%>%
            select("coverage1basetiming" = indicator_results1)
        
        burdenavertedresultscoverage2<-burdenavertedresultscoverage2()%>%
            as.data.frame()%>%
            mutate(indicator_results2=as.numeric(indicator_results2))%>%
            select("coverage2basetiming" =indicator_results2)
        
        burdenavertedresultscoverage3<-burdenavertedresultscoverage3()%>%
            as.data.frame()%>%
            mutate(indicator_results3=as.numeric(indicator_results3))%>%
            select("coverage3basetiming" = indicator_results3)
        
        
        burdenavertedresultscoverage4<-burdenavertedresultscoverage4()%>%
            as.data.frame()%>%
            mutate(indicator_results4=as.numeric(indicator_results4))%>%
            select("coverage4basetiming" = indicator_results4)
        
        
        burdenavertedresultscoverage1timing1<-burdenavertedresultscoverage1timing1()%>%
            as.data.frame()%>%
            mutate(indicator_results1=as.numeric(indicator_results1))%>%
            select("coverage1timing1" = indicator_results1)
        
        
        burdenavertedresultscoverage1timing2<-burdenavertedresultscoverage1timing2()%>%
            as.data.frame()%>%
            mutate(indicator_results1=as.numeric(indicator_results1))%>%
            select("coverage1timing2" = indicator_results1)
        
        
        burdenavertedresultscoverage1timing3<-burdenavertedresultscoverage1timing3()%>%
            as.data.frame()%>%
            mutate(indicator_results1=as.numeric(indicator_results1))%>%
            select("coverage1timing3" = indicator_results1)
        
        
        burdenavertedresultscoverage1timing4<-burdenavertedresultscoverage1timing4()%>%
            as.data.frame()%>%
            mutate(indicator_results1=as.numeric(indicator_results1))%>%
            select("coverage1timing4" = indicator_results1)
        
        
        burdenavertedresultscoverage2timing1<-burdenavertedresultscoverage2timing1()%>%
            as.data.frame()%>%
            mutate(indicator_results1=as.numeric(indicator_results1))%>%
            select("coverage2timing1" = indicator_results1)
        
        burdenavertedresultscoverage2timing2<-burdenavertedresultscoverage2timing2()%>%
            as.data.frame()%>%
            mutate(indicator_results1=as.numeric(indicator_results1))%>%
            select("coverage2timing2" = indicator_results1)
        
        
        burdenavertedresultscoverage2timing3<-burdenavertedresultscoverage2timing3()%>%
            as.data.frame()%>%
            mutate(indicator_results1=as.numeric(indicator_results1))%>%
            select("coverage2timing3" = indicator_results1)
        
        
        burdenavertedresultscoverage2timing4<-burdenavertedresultscoverage2timing4()%>%
            as.data.frame()%>%
            mutate(indicator_results1=as.numeric(indicator_results1))%>%
            select("coverage2timing4" = indicator_results1)
        
        
        burdenavertedresultscoverage3timing1<-burdenavertedresultscoverage3timing1()%>%
            as.data.frame()%>%
            mutate(indicator_results1=as.numeric(indicator_results1))%>%
            select("coverage3timing1" = indicator_results1)
        
        
        burdenavertedresultscoverage3timing2<-burdenavertedresultscoverage3timing2()%>%
            as.data.frame()%>%
            mutate(indicator_results1=as.numeric(indicator_results1))%>%
            select("coverage3timing2" = indicator_results1)
        
        burdenavertedresultscoverage3timing3<-burdenavertedresultscoverage3timing3()%>%
            as.data.frame()%>%
            mutate(indicator_results1=as.numeric(indicator_results1))%>%
            select("coverage3timing3" = indicator_results1)
        
        burdenavertedresultscoverage3timing4<-burdenavertedresultscoverage3timing4()%>%
            as.data.frame()%>%
            mutate(indicator_results1=as.numeric(indicator_results1))%>%
            select("coverage3timing4" = indicator_results1)
            
        burdenavertedresultscoverage4timing1<-burdenavertedresultscoverage4timing1()%>%
            as.data.frame()%>%
            mutate(indicator_results1=as.numeric(indicator_results1))%>%
            select("coverage4timing1" = indicator_results1)
            
            
        burdenavertedresultscoverage4timing2<-burdenavertedresultscoverage4timing2()%>%
            as.data.frame()%>%
            mutate(indicator_results1=as.numeric(indicator_results1))%>%
            select("coverage4timing2" = indicator_results1)
            
        
        burdenavertedresultscoverage4timing3<-burdenavertedresultscoverage4timing3()%>%
            as.data.frame()%>%
            mutate(indicator_results1=as.numeric(indicator_results1))%>%
            select("coverage4timing3" = indicator_results1)
        
        
        burdenavertedresultscoverage4timing4<-burdenavertedresultscoverage4timing4()%>%
            as.data.frame()%>%
            mutate(indicator_results1=as.numeric(indicator_results1))%>%
            select("coverage4timing4" = indicator_results1)
            
        
        presenceTable<-cbind(presenceTable, burdenavertedresults, burdenavertedresults_timing1, burdenavertedresults_timing2, burdenavertedresults_timing3, burdenavertedresults_timing4, 
                             burdenavertedresultscoverage1, burdenavertedresultscoverage2, burdenavertedresultscoverage3, burdenavertedresultscoverage4, burdenavertedresultscoverage1timing1, burdenavertedresultscoverage1timing2,
                             burdenavertedresultscoverage1timing3, burdenavertedresultscoverage1timing4, burdenavertedresultscoverage2timing1, burdenavertedresultscoverage2timing2, burdenavertedresultscoverage2timing3,
                             burdenavertedresultscoverage2timing4, burdenavertedresultscoverage3timing1, burdenavertedresultscoverage3timing2, burdenavertedresultscoverage3timing3, burdenavertedresultscoverage3timing4,
                             burdenavertedresultscoverage4timing1, burdenavertedresultscoverage4timing2, burdenavertedresultscoverage4timing3, burdenavertedresultscoverage4timing4)
        
        
        
        presenceTablefinal<-presenceTable%>%
            select(indicators, basecovbasetiming, basecovtiming1, basecovtiming2, basecovtiming3, basecovtiming4, coverage1basetiming, coverage2basetiming, coverage3basetiming, coverage4basetiming, coverage1timing1, coverage1timing2,
                   coverage1timing3, coverage1timing4, coverage2timing1, coverage2timing2, coverage2timing3, coverage2timing4, coverage3timing1, coverage3timing2, coverage3timing3, coverage3timing4, coverage4timing1,
                   coverage4timing2, coverage4timing3, coverage4timing4)%>%
            gt()%>%
            fmt_number(
                columns=c(basecovbasetiming, basecovtiming1, basecovtiming2, basecovtiming3, basecovtiming4, coverage1basetiming, coverage2basetiming, coverage3basetiming, coverage4basetiming, coverage1timing1, coverage1timing2,
                          coverage1timing3, coverage1timing4, coverage2timing1, coverage2timing2, coverage2timing3, coverage2timing4, coverage3timing1, coverage3timing2, coverage3timing3, coverage3timing4, coverage4timing1, 
                          coverage4timing2, coverage4timing3, coverage4timing4),
                decimals=2,
                rows=2:17
            )%>%
            fmt_percent(
                columns = c(basecovbasetiming, basecovtiming1, basecovtiming2, basecovtiming3, basecovtiming4, coverage1basetiming, coverage2basetiming, coverage3basetiming, coverage4basetiming, coverage1timing1, coverage1timing2,
                            coverage1timing3, coverage1timing4, coverage2timing1, coverage2timing2, coverage2timing3, coverage2timing4, coverage3timing1, coverage3timing2, coverage3timing3, coverage3timing4, coverage4timing1, 
                            coverage4timing2, coverage4timing3, coverage4timing4),
                decimals = 2,
                rows=17:17
            )%>%
            tab_header(
                title = paste0("Vaccine deployment timing strategy results"),
                subtitle = paste0(targetgroup(),", ", country())
            )%>%
            cols_label(
                indicators = "",
            )%>%
            tab_row_group(
                label = "Non-hospitalized infections",
                rows = 1:4)%>%
            tab_row_group(
                label = "Total infections",
                rows = 5:8)%>%
            tab_row_group(
                label = "Hospitalized infections",
                rows = 9:12)%>%
            tab_row_group(
                label = "Medically attended infections",
                rows = 13:16)%>%
            tab_row_group(
                label = "Prevented fraction",
                rows = 17:17)%>%
            tab_style(
                locations = cells_row_groups(),
                style     = list(
                    cell_text(weight = "bold")
                ))%>%
            tab_options(
                #Adjust grouped rows to make them stand out
                row_group.background.color = "#e1e3e1")%>%
            tab_style(
                locations = cells_title(groups = "title"),
                style     = list(
                    cell_text(weight = "bold", size = 24)
                ))%>%
            tab_style(
                locations = cells_title(groups = "subtitle"),
                style     = list(
                    cell_text(weight = "bold", size = 18)
                ))%>%
            tab_style(
                locations = cells_column_labels(columns = everything()),
                style     = list(
                    cell_text(weight = "bold")
                )
            )%>%
            row_group_order(groups = c("Hospitalized infections", "Non-hospitalized infections", "Total infections", "Medically attended infections", "Prevented fraction"))
        
        return(presenceTablefinal)
        
    }
    ) ##close table
    
    
    
    
    
    
    output$gt_table_timing1out<-render_gt({
        table<-gt_table_covtiming()
        return(table)
    })
   
    
    
    
    gt_table_covtiming1<- reactive({
        
        ####################################
        ## Render results table
        ####################################
        
        populationsize<-as.numeric(as.character(populationsize()))
        hnh<-as.numeric(as.character(hnh()))
        mai<-as.numeric(as.character(mai()))
        vaccineeffectiveness<-as.numeric(as.character(vaccineeffectiveness()))
        VELCI<-as.numeric(as.character(VELCI()))
        VEUCI<-as.numeric(as.character(VEUCI()))
        
        
        
        burdenavertedresults<-burdenavertedresults()%>%
            as.data.frame()%>%
            mutate(indicator_results=as.numeric(indicator_results))%>%
            select("basecovbasetiming" = indicator_results)
        
        
        burdenavertedresults_timing1<-burdenavertedresults_timing1()%>%
            as.data.frame()%>%
            mutate(indicator_results1=as.numeric(indicator_results1))%>%
            select("basecovtiming1" = indicator_results1)
        
        burdenavertedresults_timing2<-burdenavertedresults_timing2()%>%
            as.data.frame()%>%
            mutate(indicator_results2=as.numeric(indicator_results2))%>%
            select("basecovtiming2" = indicator_results2)
        
        
        burdenavertedresults_timing3<-burdenavertedresults_timing3()%>%
            as.data.frame()%>%
            mutate(indicator_results3=as.numeric(indicator_results3))%>%
            select("basecovtiming3" =indicator_results3)
        
        burdenavertedresults_timing4<-burdenavertedresults_timing4()%>%
            as.data.frame()%>%
            mutate(indicator_results4=as.numeric(indicator_results4))%>%
            select("basecovtiming4" =indicator_results4)
        
        
        burdenavertedresultscoverage1<-burdenavertedresultscoverage1()%>%
            as.data.frame()%>%
            mutate(indicator_results1=as.numeric(indicator_results1))%>%
            select("coverage1basetiming" = indicator_results1)
        
        burdenavertedresultscoverage2<-burdenavertedresultscoverage2()%>%
            as.data.frame()%>%
            mutate(indicator_results2=as.numeric(indicator_results2))%>%
            select("coverage2basetiming" =indicator_results2)
        
        burdenavertedresultscoverage3<-burdenavertedresultscoverage3()%>%
            as.data.frame()%>%
            mutate(indicator_results3=as.numeric(indicator_results3))%>%
            select("coverage3basetiming" = indicator_results3)
        
        
        burdenavertedresultscoverage4<-burdenavertedresultscoverage4()%>%
            as.data.frame()%>%
            mutate(indicator_results4=as.numeric(indicator_results4))%>%
            select("coverage4basetiming" = indicator_results4)
        
        
        burdenavertedresultscoverage1timing1<-burdenavertedresultscoverage1timing1()%>%
            as.data.frame()%>%
            mutate(indicator_results1=as.numeric(indicator_results1))%>%
            select("coverage1timing1" = indicator_results1)
        
        
        burdenavertedresultscoverage1timing2<-burdenavertedresultscoverage1timing2()%>%
            as.data.frame()%>%
            mutate(indicator_results1=as.numeric(indicator_results1))%>%
            select("coverage1timing2" = indicator_results1)
        
        
        burdenavertedresultscoverage1timing3<-burdenavertedresultscoverage1timing3()%>%
            as.data.frame()%>%
            mutate(indicator_results1=as.numeric(indicator_results1))%>%
            select("coverage1timing3" = indicator_results1)
        
        
        burdenavertedresultscoverage1timing4<-burdenavertedresultscoverage1timing4()%>%
            as.data.frame()%>%
            mutate(indicator_results1=as.numeric(indicator_results1))%>%
            select("coverage1timing4" = indicator_results1)
        
        
        burdenavertedresultscoverage2timing1<-burdenavertedresultscoverage2timing1()%>%
            as.data.frame()%>%
            mutate(indicator_results1=as.numeric(indicator_results1))%>%
            select("coverage2timing1" = indicator_results1)
        
        burdenavertedresultscoverage2timing2<-burdenavertedresultscoverage2timing2()%>%
            as.data.frame()%>%
            mutate(indicator_results1=as.numeric(indicator_results1))%>%
            select("coverage2timing2" = indicator_results1)
        
        
        burdenavertedresultscoverage2timing3<-burdenavertedresultscoverage2timing3()%>%
            as.data.frame()%>%
            mutate(indicator_results1=as.numeric(indicator_results1))%>%
            select("coverage2timing3" = indicator_results1)
        
        
        burdenavertedresultscoverage2timing4<-burdenavertedresultscoverage2timing4()%>%
            as.data.frame()%>%
            mutate(indicator_results1=as.numeric(indicator_results1))%>%
            select("coverage2timing4" = indicator_results1)
        
        
        burdenavertedresultscoverage3timing1<-burdenavertedresultscoverage3timing1()%>%
            as.data.frame()%>%
            mutate(indicator_results1=as.numeric(indicator_results1))%>%
            select("coverage3timing1" = indicator_results1)
        
        
        burdenavertedresultscoverage3timing2<-burdenavertedresultscoverage3timing2()%>%
            as.data.frame()%>%
            mutate(indicator_results1=as.numeric(indicator_results1))%>%
            select("coverage3timing2" = indicator_results1)
        
        burdenavertedresultscoverage3timing3<-burdenavertedresultscoverage3timing3()%>%
            as.data.frame()%>%
            mutate(indicator_results1=as.numeric(indicator_results1))%>%
            select("coverage3timing3" = indicator_results1)
        
        burdenavertedresultscoverage3timing4<-burdenavertedresultscoverage3timing4()%>%
            as.data.frame()%>%
            mutate(indicator_results1=as.numeric(indicator_results1))%>%
            select("coverage3timing4" = indicator_results1)
        
        burdenavertedresultscoverage4timing1<-burdenavertedresultscoverage4timing1()%>%
            as.data.frame()%>%
            mutate(indicator_results1=as.numeric(indicator_results1))%>%
            select("coverage4timing1" = indicator_results1)
        
        
        burdenavertedresultscoverage4timing2<-burdenavertedresultscoverage4timing2()%>%
            as.data.frame()%>%
            mutate(indicator_results1=as.numeric(indicator_results1))%>%
            select("coverage4timing2" = indicator_results1)
        
        
        burdenavertedresultscoverage4timing3<-burdenavertedresultscoverage4timing3()%>%
            as.data.frame()%>%
            mutate(indicator_results1=as.numeric(indicator_results1))%>%
            select("coverage4timing3" = indicator_results1)
        
        
        burdenavertedresultscoverage4timing4<-burdenavertedresultscoverage4timing4()%>%
            as.data.frame()%>%
            mutate(indicator_results1=as.numeric(indicator_results1))%>%
            select("coverage4timing4" = indicator_results1)
        
        
        presenceTable<-cbind(presenceTable, burdenavertedresults, burdenavertedresults_timing1, burdenavertedresults_timing2, burdenavertedresults_timing3, burdenavertedresults_timing4, 
                             burdenavertedresultscoverage1, burdenavertedresultscoverage2, burdenavertedresultscoverage3, burdenavertedresultscoverage4, burdenavertedresultscoverage1timing1, burdenavertedresultscoverage1timing2,
                             burdenavertedresultscoverage1timing3, burdenavertedresultscoverage1timing4, burdenavertedresultscoverage2timing1, burdenavertedresultscoverage2timing2, burdenavertedresultscoverage2timing3,
                             burdenavertedresultscoverage2timing4, burdenavertedresultscoverage3timing1, burdenavertedresultscoverage3timing2, burdenavertedresultscoverage3timing3, burdenavertedresultscoverage3timing4,
                             burdenavertedresultscoverage4timing1, burdenavertedresultscoverage4timing2, burdenavertedresultscoverage4timing3, burdenavertedresultscoverage4timing4)
        
        
        

        presenceTablefinal<-presenceTable%>%
            select(indicators, basecovbasetiming, basecovtiming1, basecovtiming2, basecovtiming3, basecovtiming4, coverage1basetiming, coverage2basetiming, coverage3basetiming, coverage4basetiming, coverage1timing1, coverage1timing2,
                   coverage1timing3, coverage1timing4, coverage2timing1, coverage2timing2, coverage2timing3, coverage2timing4, coverage3timing1, coverage3timing2, coverage3timing3, coverage3timing4, coverage4timing1,
                   coverage4timing2, coverage4timing3, coverage4timing4)%>%
            filter(grepl("averted|Prevented", indicators))%>%
            pivot_longer(-indicators) %>%
            pivot_wider(name, names_from="indicators", values_from="value") %>%
            rename(indicators=name)%>%
            mutate(Coverage = case_when(str_detect(indicators, "basecov" ) ~ "Base coverage",
                                     str_detect(indicators, "coverage1" ) ~ "Coverage 1",
                                     str_detect(indicators, "coverage2" ) ~ "Coverage 2",
                                     str_detect(indicators, "coverage3" ) ~ "Coverage 3",
                                     str_detect(indicators, "coverage4" ) ~ "Coverage 4"))%>%
            mutate(Timing = case_when(str_detect(indicators, "basetiming" ) ~ 0,
                                        str_detect(indicators, "timing1" ) ~  1,
                                        str_detect(indicators, "timing2" ) ~ 2,
                                        str_detect(indicators, "timing3" ) ~ 3,
                                        str_detect(indicators, "timing4" ) ~ 4))%>%
            rename(indicators1 = 1, nonhospaverted1 = 2, totaverted1=3, hospaverted1=4, medicallyaverted=5, preventedfrac=6)
        
        presenceTablefinal2<-presenceTablefinal%>%
            #select(indicators1, nonhospaverted1, Coverage, Timing)%>%
            #filter(Coverage=="Base coverage")%>%
            gt()
           
        
        return(presenceTablefinal2)
        
    }
    ) ##close table
    
    
    
    
    
    
    output$gt_table_timing1out1<-render_gt({
        table<-gt_table_covtiming1()
        return(table)
    })
    
    
    
    output$covtimepgraph<-renderPlot({
        
        populationsize<-as.numeric(as.character(populationsize()))
        hnh<-as.numeric(as.character(hnh()))
        mai<-as.numeric(as.character(mai()))
        vaccineeffectiveness<-as.numeric(as.character(vaccineeffectiveness()))
        VELCI<-as.numeric(as.character(VELCI()))
        VEUCI<-as.numeric(as.character(VEUCI()))
        
        
        
        burdenavertedresults<-burdenavertedresults()%>%
            as.data.frame()%>%
            mutate(indicator_results=as.numeric(indicator_results))%>%
            select("basecovbasetiming" = indicator_results)
        
        
        burdenavertedresults_timing1<-burdenavertedresults_timing1()%>%
            as.data.frame()%>%
            mutate(indicator_results1=as.numeric(indicator_results1))%>%
            select("basecovtiming1" = indicator_results1)
        
        burdenavertedresults_timing2<-burdenavertedresults_timing2()%>%
            as.data.frame()%>%
            mutate(indicator_results2=as.numeric(indicator_results2))%>%
            select("basecovtiming2" = indicator_results2)
        
        
        burdenavertedresults_timing3<-burdenavertedresults_timing3()%>%
            as.data.frame()%>%
            mutate(indicator_results3=as.numeric(indicator_results3))%>%
            select("basecovtiming3" =indicator_results3)
        
        burdenavertedresults_timing4<-burdenavertedresults_timing4()%>%
            as.data.frame()%>%
            mutate(indicator_results4=as.numeric(indicator_results4))%>%
            select("basecovtiming4" =indicator_results4)
        
        
        burdenavertedresultscoverage1<-burdenavertedresultscoverage1()%>%
            as.data.frame()%>%
            mutate(indicator_results1=as.numeric(indicator_results1))%>%
            select("coverage1basetiming" = indicator_results1)
        
        burdenavertedresultscoverage2<-burdenavertedresultscoverage2()%>%
            as.data.frame()%>%
            mutate(indicator_results2=as.numeric(indicator_results2))%>%
            select("coverage2basetiming" =indicator_results2)
        
        burdenavertedresultscoverage3<-burdenavertedresultscoverage3()%>%
            as.data.frame()%>%
            mutate(indicator_results3=as.numeric(indicator_results3))%>%
            select("coverage3basetiming" = indicator_results3)
        
        
        burdenavertedresultscoverage4<-burdenavertedresultscoverage4()%>%
            as.data.frame()%>%
            mutate(indicator_results4=as.numeric(indicator_results4))%>%
            select("coverage4basetiming" = indicator_results4)
        
        
        burdenavertedresultscoverage1timing1<-burdenavertedresultscoverage1timing1()%>%
            as.data.frame()%>%
            mutate(indicator_results1=as.numeric(indicator_results1))%>%
            select("coverage1timing1" = indicator_results1)
        
        
        burdenavertedresultscoverage1timing2<-burdenavertedresultscoverage1timing2()%>%
            as.data.frame()%>%
            mutate(indicator_results1=as.numeric(indicator_results1))%>%
            select("coverage1timing2" = indicator_results1)
        
        
        burdenavertedresultscoverage1timing3<-burdenavertedresultscoverage1timing3()%>%
            as.data.frame()%>%
            mutate(indicator_results1=as.numeric(indicator_results1))%>%
            select("coverage1timing3" = indicator_results1)
        
        
        burdenavertedresultscoverage1timing4<-burdenavertedresultscoverage1timing4()%>%
            as.data.frame()%>%
            mutate(indicator_results1=as.numeric(indicator_results1))%>%
            select("coverage1timing4" = indicator_results1)
        
        
        burdenavertedresultscoverage2timing1<-burdenavertedresultscoverage2timing1()%>%
            as.data.frame()%>%
            mutate(indicator_results1=as.numeric(indicator_results1))%>%
            select("coverage2timing1" = indicator_results1)
        
        burdenavertedresultscoverage2timing2<-burdenavertedresultscoverage2timing2()%>%
            as.data.frame()%>%
            mutate(indicator_results1=as.numeric(indicator_results1))%>%
            select("coverage2timing2" = indicator_results1)
        
        
        burdenavertedresultscoverage2timing3<-burdenavertedresultscoverage2timing3()%>%
            as.data.frame()%>%
            mutate(indicator_results1=as.numeric(indicator_results1))%>%
            select("coverage2timing3" = indicator_results1)
        
        
        burdenavertedresultscoverage2timing4<-burdenavertedresultscoverage2timing4()%>%
            as.data.frame()%>%
            mutate(indicator_results1=as.numeric(indicator_results1))%>%
            select("coverage2timing4" = indicator_results1)
        
        
        burdenavertedresultscoverage3timing1<-burdenavertedresultscoverage3timing1()%>%
            as.data.frame()%>%
            mutate(indicator_results1=as.numeric(indicator_results1))%>%
            select("coverage3timing1" = indicator_results1)
        
        
        burdenavertedresultscoverage3timing2<-burdenavertedresultscoverage3timing2()%>%
            as.data.frame()%>%
            mutate(indicator_results1=as.numeric(indicator_results1))%>%
            select("coverage3timing2" = indicator_results1)
        
        burdenavertedresultscoverage3timing3<-burdenavertedresultscoverage3timing3()%>%
            as.data.frame()%>%
            mutate(indicator_results1=as.numeric(indicator_results1))%>%
            select("coverage3timing3" = indicator_results1)
        
        burdenavertedresultscoverage3timing4<-burdenavertedresultscoverage3timing4()%>%
            as.data.frame()%>%
            mutate(indicator_results1=as.numeric(indicator_results1))%>%
            select("coverage3timing4" = indicator_results1)
        
        burdenavertedresultscoverage4timing1<-burdenavertedresultscoverage4timing1()%>%
            as.data.frame()%>%
            mutate(indicator_results1=as.numeric(indicator_results1))%>%
            select("coverage4timing1" = indicator_results1)
        
        
        burdenavertedresultscoverage4timing2<-burdenavertedresultscoverage4timing2()%>%
            as.data.frame()%>%
            mutate(indicator_results1=as.numeric(indicator_results1))%>%
            select("coverage4timing2" = indicator_results1)
        
        
        burdenavertedresultscoverage4timing3<-burdenavertedresultscoverage4timing3()%>%
            as.data.frame()%>%
            mutate(indicator_results1=as.numeric(indicator_results1))%>%
            select("coverage4timing3" = indicator_results1)
        
        
        burdenavertedresultscoverage4timing4<-burdenavertedresultscoverage4timing4()%>%
            as.data.frame()%>%
            mutate(indicator_results1=as.numeric(indicator_results1))%>%
            select("coverage4timing4" = indicator_results1)
        
        
        presenceTable<-cbind(presenceTable, burdenavertedresults, burdenavertedresults_timing1, burdenavertedresults_timing2, burdenavertedresults_timing3, burdenavertedresults_timing4, 
                             burdenavertedresultscoverage1, burdenavertedresultscoverage2, burdenavertedresultscoverage3, burdenavertedresultscoverage4, burdenavertedresultscoverage1timing1, burdenavertedresultscoverage1timing2,
                             burdenavertedresultscoverage1timing3, burdenavertedresultscoverage1timing4, burdenavertedresultscoverage2timing1, burdenavertedresultscoverage2timing2, burdenavertedresultscoverage2timing3,
                             burdenavertedresultscoverage2timing4, burdenavertedresultscoverage3timing1, burdenavertedresultscoverage3timing2, burdenavertedresultscoverage3timing3, burdenavertedresultscoverage3timing4,
                             burdenavertedresultscoverage4timing1, burdenavertedresultscoverage4timing2, burdenavertedresultscoverage4timing3, burdenavertedresultscoverage4timing4)
        
        
        

        presenceTablefinal<-presenceTable%>%
            select(indicators, basecovbasetiming, basecovtiming1, basecovtiming2, basecovtiming3, basecovtiming4, coverage1basetiming, coverage2basetiming, coverage3basetiming, coverage4basetiming, coverage1timing1, coverage1timing2,
                   coverage1timing3, coverage1timing4, coverage2timing1, coverage2timing2, coverage2timing3, coverage2timing4, coverage3timing1, coverage3timing2, coverage3timing3, coverage3timing4, coverage4timing1,
                   coverage4timing2, coverage4timing3, coverage4timing4)%>%
            filter(grepl("averted|Prevented", indicators))%>%
            pivot_longer(-indicators) %>%
            pivot_wider(name, names_from="indicators", values_from="value") %>%
            rename(indicators=name)%>%
            mutate(Coverage = case_when(str_detect(indicators, "basecov" ) ~ paste0("Base coverage ", totalcoverage(), '%'),
                                        str_detect(indicators, "coverage1" ) ~ paste0(coverage1()*100, '%'),
                                        str_detect(indicators, "coverage2" ) ~ paste0(coverage2()*100, '%'),
                                        str_detect(indicators, "coverage3" ) ~ paste0(coverage3()*100, '%'),
                                        str_detect(indicators, "coverage4" ) ~ paste0(coverage4()*100, '%')))%>%
            mutate(Timing = case_when(str_detect(indicators, "basetiming" ) ~ 0,
                                      str_detect(indicators, "timing1" ) ~  1,
                                      str_detect(indicators, "timing2" ) ~ 2,
                                      str_detect(indicators, "timing3" ) ~ 3,
                                      str_detect(indicators, "timing4" ) ~ 4))%>%
            rename(indicators1 = 1, nonhospaverted1 = 2, totaverted1=3, hospaverted1=4, medicallyaverted=5, preventedfrac=6)
        
        
        
        
        #title=paste0("Non-hospitalized infections averted across different deployment strategies"), 
        #subtitle = paste0(targetgroup(), " , ", country(), " , ", year())
        
        
        
        pfplot<-presenceTablefinal%>%
            select(indicators1, preventedfrac, Coverage, Timing)%>%
            ggplot(aes(x=Timing, y=preventedfrac, group=Coverage, color=Coverage)) +
            geom_line(aes(linetype=Coverage), size=1.5)+
            scale_color_manual(values=c('#000000','#1F8A87', '#035B2B', '#004673', '#00243B'))+
            geom_point(aes(shape=Coverage), size=4.2)+
            labs(title="Prevented fraction", 
                 subtitle = paste0(targetgroup(), " , ", country(), " , ", year()),
                 x="Deployment timing", 
                 y = "Prevented fraction",
                 linetype="Coverage")+ 
            theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.y = element_text(face="bold", size=12),
                  axis.text.x = element_text(face="bold", size=12),
                  axis.title=element_text(size=12,face="bold"),
                  panel.background = element_blank(), plot.title=element_text(size=16, face='bold'),
                  plot.subtitle = element_text(size = 14), legend.text = element_text(size = 12), legend.title = element_text(size = 12, face="bold"))
        
        hospplot<-presenceTablefinal%>%
            select(indicators1, hospaverted1, Coverage, Timing)%>%
            ggplot(aes(x=Timing, y=hospaverted1, group=Coverage, color=Coverage)) +
            geom_line(aes(linetype=Coverage), size=1.5)+
            scale_color_manual(values=c('#000000','#1F8A87', '#035B2B', '#004673', '#00243B'))+
            geom_point(aes(shape=Coverage), size=4.2)+
            labs(title="Hospitalizations", 
                 subtitle = paste0("Among ", targetgroup(), " in, ", country(), " , ", year()),
                 x="Deployment timing", 
                 y = "Number of infections averted",
                 linetype="Coverage")+ 
            theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.y = element_text(face="bold", size=12),
                  axis.text.x = element_text(face="bold", size=12),
                  axis.title=element_text(size=12,face="bold"),
                  panel.background = element_blank(), plot.title=element_text(size=16, face='bold'),
                  plot.subtitle = element_text(size = 14), legend.text = element_text(size = 12), legend.title = element_text(size = 12, face="bold"))
       
         nonhospplot<-presenceTablefinal%>%
             select(indicators1, nonhospaverted1, Coverage, Timing)%>%
             ggplot(aes(x=Timing, y=nonhospaverted1, group=Coverage, color=Coverage)) +
             geom_line(aes(linetype=Coverage), size=1.5)+
             scale_color_manual(values=c('#000000','#1F8A87', '#035B2B', '#004673', '#00243B'))+
             geom_point(aes(shape=Coverage), size=4.2)+
             labs(title="Prevented fraction", 
                  subtitle = paste0(targetgroup(), " , ", country(), " , ", year()),
                  x="Deployment timing", 
                  y = "Prevented fraction",
                  linetype="Coverage")+ 
             theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.y = element_text(face="bold", size=12),
                   axis.text.x = element_text(face="bold", size=12),
                   axis.title=element_text(size=12,face="bold"),
                   panel.background = element_blank(), plot.title=element_text(size=16, face='bold'),
                   plot.subtitle = element_text(size = 14), legend.text = element_text(size = 12), legend.title = element_text(size = 12, face="bold"))
         
         medicalattplot<-presenceTablefinal%>%
             select(indicators1, medicallyaverted, Coverage, Timing)%>%
             ggplot(aes(x=Timing, y=medicallyaverted, group=Coverage, color=Coverage)) +
             geom_line(aes(linetype=Coverage), size=1.5)+
             scale_color_manual(values=c('#000000','#1F8A87', '#035B2B', '#004673', '#00243B'))+
             geom_point(aes(shape=Coverage), size=4.2)+
             labs(title="Prevented fraction", 
                  subtitle = paste0(targetgroup(), " , ", country(), " , ", year()),
                  x="Deployment timing", 
                  y = "Prevented fraction",
                  linetype="Coverage")+ 
             theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.y = element_text(face="bold", size=12),
                   axis.text.x = element_text(face="bold", size=12),
                   axis.title=element_text(size=12,face="bold"),
                   panel.background = element_blank(), plot.title=element_text(size=16, face='bold'),
                   plot.subtitle = element_text(size = 14), legend.text = element_text(size = 12), legend.title = element_text(size = 12, face="bold"))
         
         totplot<-presenceTablefinal%>%
             select(indicators1, totaverted1, Coverage, Timing)%>%
             ggplot(aes(x=Timing, y=totaverted1, group=Coverage, color=Coverage)) +
             geom_line(aes(linetype=Coverage), size=1.5)+
             scale_color_manual(values=c('#000000','#1F8A87', '#035B2B', '#004673', '#00243B'))+
             geom_point(aes(shape=Coverage,), size=4.2)+
             labs(title="Total infections",
                  x="Deployment timing", 
                  y = "Number of infections averted",
                  linetype="Coverage")+ 
             theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.y = element_text(face="bold", size=12),
                   axis.text.x = element_text(face="bold", size=12),
                   axis.title=element_text(size=12,face="bold"),
                   panel.background = element_blank(), plot.title=element_text(size=16, face='bold'),
                   plot.subtitle = element_text(size = 14), legend.text = element_text(size = 12), legend.title = element_text(size = 12, face="bold"),
                   legend.margin=margin(t = 1, b=1, unit='cm'))
         
         
         
         if (stratplotchoice()=="Prevented fraction") {
             return(pfplot)
         } else if (stratplotchoice()=="Hospitalizations") {
             return(hospplot)
         } else if (stratplotchoice()=="Non-hospitalized infections") {
             return(nonhospplot)
         } else if (stratplotchoice()=="Medically attended infections") {
             return(medicalattplot)
         } else {
             return(totplot)
         }

         
         
         



    })
    
        
        
        
        
    

    
    
    
} #close server
