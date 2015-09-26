library(shiny)
library(ggplot2)
library(gridExtra)

# Set RNG seed
set.seed(42)

# Define server logic required to generate and plot voter distrubtion
shinyServer(function(input, output) {

    # Dynamic UI output: Reactively changes computation to the slelected distribution
    output$distControls <- renderUI({
        selectInput("dist", 
                    "Distribution", 
                    choices = c("exponential", "poisson"),
                    selected = "exponential")
    })

    # Stores the selected distribution type in distInput for later use
    distInput <- reactive({
        if (is.null(input$dist)) return("rexp")
        distSelected <- switch(input$dist,
                               "exponential" = c("rexp"),
                               "poisson" = c("rpois"))
        distSelected
    })
        
    # Reactively computes SIMULATED sample means for the selected distribution
    # isolate the calculation if nsim is unchanged from sliderInput
    valuesSimulated <- reactive({
        means <- NULL
        switch(distInput(),
#                rexp = isolate(for (i in 1:input$nsim) means = c(means, 
#                                                         mean(rexp(input$n, 
#                                                                   input$lambda)))),
#                rpois = isolate(for (i in 1:input$nsim) means = c(means, 
#                                                          mean(rpois(input$n, 
#                                                                     input$lambda))))
               rexp = for (i in 1:input$nsim) means = c(means,
                                                        mean(rexp(input$n,
                                                                  input$lambda))),
               rpois = for (i in 1:input$nsim) means = c(means,
                                                         mean(rpois(input$n,
                                                                    input$lambda)))
               )
        means
    })

    # Reactively computes EXPECTED mean and variance values for the selected distribution
    # Expected mean of Exponential distribution is 1/lambda, var is (1/lambda/sqrt(n))^2
    # Expected mean and variance of Poisson distribution both are lambda
    valuesExpected <- reactive({
        expected <- switch(distInput(),
                           rexp = c(1/input$lambda, # Expected mean
                                    variance=((1/input$lambda)/sqrt(input$n))^2), # Expected variance
                           rpois = c(mean=input$lambda, # Expected mean
                                     (variance=input$lambda)) # Expected variance
                           )
        expected
    })

    # Reactively computes the selected distribution of n observations
    valuesObserved <- reactive({
        observed <- NULL
        observed <- switch(distInput(),
                               rexp = rexp(input$n, input$lambda), # for exponential distribution
                               rpois = rpois(input$n, input$lambda) # for poisson distribution
                           )
        observed
    })

    # Output Text "dist": Application title
    output$dist <- renderText({
        paste0("Differences between the simulated and expected mean/variances for the ", 
               input$dist, " distribution")
    })

#     valuesM2 <- reactive({
#         m2 <- NULL
#         lapply(1:5, function(i) m2[i] = mean(rexp(input$n, input$lambda)))
#         #for (i in 1:input$nsim) m2 = c(m2, mean(rexp(input$n, input$lambda)))
#         m2[i]
#     })

    # Output Table "view_sim": Simulated vs Expected mean and variance
    output$view_sim <- renderTable({
        mS <- mean(valuesSimulated()) # Sample mean mS
        vS <- sd(valuesSimulated())^2 # Sample variance vS
        mE <- valuesExpected()[1]
        vE <- valuesExpected()[2]        
        stats <- rbind(mean=c(mS, mE), variance=c(vS, vE))
        #stats <- cbind(mean=valuesSimulated(), variance=valuesExpected())
        colnames(stats) <- c("Simulated", "Expected")
        print(stats)
    })

    # Output Tables "view_largeN_obs: Observed vs Expected mean and variance
    output$view_largeN_obs <- renderTable({
        mE <- valuesExpected()[1] # Expected mean mS
        vE <- valuesExpected()[2] # Expected variance vS
        cat(valuesExpected())
        stats1 <- rbind(mean=c(mean(valuesObserved()), mE), variance=c(var(valuesObserved()), vE))
        colnames(stats1) <- c("Observed", "Expected")
        print(stats1)
    })

    # Expression that generates a plot of the distribution
    # Expression is wrapped in a call to renderPlot indicating:
    # 1) It is "reactive" and therefore should be automatically re-executed when inputs change
    # 2) Its output type is a plot

    # Output Plot "simPlot": Plot of sample means from input$nsim simulations
    output$simPlot <- renderPlot({
        # Grab all the needed values to make the plots, create the labels
        mS <- valuesSimulated()
        mE <- valuesExpected()[1]
        vE <- valuesExpected()[2]
        lambdaLabel <- paste0("with lambda=", input$lambda)
        # Plot of sample means from n simulations
        g <- ggplot(data.frame(mS), aes(x = mS)) + 
            geom_histogram(alpha = .2, colour = "blue", aes(y = ..density..))
        g <- g + geom_vline(xintercept = mean(mS), size = 1, color = "red", linetype = "longdash") +
            geom_text(x=mean(mS), aes(label="mean of simulated means", 
                                      y=quantile(dexp(0.5, mS), 0.25)[[1]]),
                      colour="red", angle=90, vjust = 0, hjust=0, text=element_text(size=6))
        g <- g + geom_vline(xintercept = mE, size = 1, color = "black") + 
            geom_text(x=mE, aes(label="expected mean", y=0), 
                      colour="black", angle=90, vjust = 1, hjust=0, text=element_text(size=6))
        g <- g + labs(title = paste0("Distribution of sample Means from ", 
                                     input$nsim," Simulations of ", 
                                     input$n," observations ", lambdaLabel),
                      x = "mean_simulated")
        g + stat_function(fun = dnorm, args = list(mean = mE, sd = sqrt(vE)))
        #grid.arrange(g, g1, nrow = 2)
        #g + stat_function(fun = dnorm, args=list(mean=mE, sd=sqrt(vE)))
    })

    # Output Plot "obsPlot": Plot of distribution of input$n observations
    output$obsPlot <- renderPlot({
        # Grab all the needed values to make the plots, create the labels
        mO <- valuesObserved()
        mE <- valuesExpected()[1]
        vE <- valuesExpected()[2]
        lambdaLabel <- paste0("with lambda=", input$lambda)
        # Plot of mean of n observations
        g1 <- ggplot(data.frame(mO), aes(x = mO)) + 
            geom_histogram(alpha = .20, colour = "blue", aes(y = ..density..))
        g1 <- g1 + geom_vline(xintercept = mean(mO), size=1, color = "red", linetype = "longdash") +
            geom_text(x=mean(mO), aes(label="observed mean", y=0), colour="red", 
                      angle=90, vjust = 0, hjust=0, text=element_text(size=6))
        g1 <- g1 + geom_vline(xintercept = mE, size = 1, color = "black") +
            geom_text(x=mE, aes(label="expected mean", y=0), colour="black", 
                      angle=90, vjust = 1, hjust=0, text=element_text(size=6))
        g1 <- g1 + stat_function(fun = dnorm, args = list(mean = mE, sd=sqrt(vE)))
        g1 <- g1 + labs(title = paste0("Distribution of ", input$n, " observations ", lambdaLabel),
                        x = "observed")
        g1 + stat_function(fun = dnorm, args = list(mean = mE, sd = sqrt(vE)))
    })
    
})