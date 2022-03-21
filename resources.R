# FUNCTIONS ----

## Little helpers ----

bold <- function(string, color = NULL) {
  if (is.null(color)) {
    return(paste0("<b>", string, "</b>"))
  } else {
    return(paste0("<b style='color:", color, ";'>", string, "</b>"))
  }
}

paragraph <- function(..., lineheight = "150%") {
  paste0("<p style = 'line-height: ", lineheight, ";'>",...,"</p>")
}

## Lottery description ----

get_result_names <- function(n) {
  alphabet <- str_split("A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z", ",") %>% 
    unlist()
  return(alphabet[1:n])
}

calc_expected_value <- function(values, percentages) {
  return(sum(values * percentages))
}

segment_number_string <- function(number_string) {
  return(
    number_string %>% 
      str_split(";") %>% 
      unlist() %>% 
      str_trim() %>% 
      as.numeric()
  )
}

test_inputs <- function(values, percentages) {
  return((
    class(values) == "try-error" || class(percentages) == "try-error" || any(is.na(values)) || any(is.na(percentages)) || length(values) != length(percentages) || sum(percentages) != 1 
  ))
}

describe_option <- function(result_name, value, percentage) {
  return(paste("<li>Outcome", bold(result_name), 
               "pays", ifelse(value >= 0, bold(value, color = "green"), bold(value, color = "red")), 
               "with a", bold(paste0(round(percentage*100), "%")), "chance</li>")
  )
}

description_header <- function(description) {
  paste0(
    h2("Lotery returns"), "<br><ul>", paste(description, collapse=""), "</ul><br><br>"
  )
}

cumulate_percentages <- function(percentages) {
  n <- 1:length(percentages)
  cum_percentages <- map_dbl(n, ~sum(percentages[1:.]))
  return(cum_percentages)
}

output_description <- function(description_list) {
  if (description_list$result_names[[1]] == "error") {
    return("<p style='color:red;'>The inputs were not provided correctly. Please follow the instructions.</p>")
  } else {
    return(description_list$descriptions)
  }
}

theoretical_deviation <- function(theoretical_distribution) {
  values <- as.numeric(theoretical_distribution$Values)
  frequencies <- theoretical_distribution$Frequency
  dist <- map2(values, frequencies, rep) %>% unlist()
  return(sd(dist))
}

describe_lottery <- function(values_string, percentages_string) {
  values <- try(segment_number_string(values_string), silent = TRUE)
  percentages <- try(segment_number_string(percentages_string), silent = TRUE)
  problem_with_inputs <- test_inputs(values, percentages)
  if (problem_with_inputs) {
    return(list(
      descriptions = "<br><p style = 'color:red;'><b>Incorrect input. Please try again.</b></p>"
      )
    )
  } else {
    n <- length(values)
    cum_percentages <- cumulate_percentages(percentages)
    expected_value <- calc_expected_value(values, percentages)
    result_names <- get_result_names(n)
    descriptions <- map_chr(1:n, ~describe_option(result_names[.], values[.], percentages[.])) %>% 
      description_header()
    theoretical_distribution <- tibble(Values = fct_infreq(factor(values)), Frequency = round(percentages*100))
    return(list(
      result_names = result_names,
      values = values,
      percentages = percentages,
      cum_percentages = cum_percentages,
      expected_value = expected_value,
      descriptions = descriptions,
      theoretical_distribution = theoretical_distribution,
      theoretical_deviation = theoretical_deviation(theoretical_distribution)
    ))
  }
}

summarise_lottery <- function(description_list) {
  header <- h2("Solution")
  expected_value <- paste0(
    "<li style = 'color:#1e2938'><b>Expected value: ", as.character(
      round(description_list$expected_value, digits=2)
    ), "</b></li>"
  )
  theoretical_variance <- paste0(
    "<li style = 'color:gray'>Theoretical standard deviation: ", as.character(
      round(theoretical_deviation(description_list$theoretical_distribution), digits=2)
    ), "</li>"
  )
  return(paste0(
    header, "<br>", "<ul style='margin-left=8px; margin-bottom=8px;'>",
    expected_value, theoretical_variance, "</ul><br>"
  ))
}

## Lottery simulation ----


select_result <- function(value, cum_percentages, result_names) {
  return(result_names[min(which(cum_percentages >= value))])
}

simulate_n_tickets <- function(n, cum_percentages, result_names) {
  dist <- runif(n)
  return(
    map_chr(dist, ~select_result(., cum_percentages, result_names))
  )
}

n_tickets_register <- function(simulation, result_names, values) {
  output <- list()
  output[["Tickets"]] <- length(simulation)
  total_value <- 0
  for (i in seq_along(result_names)) {
    output[[result_names[[i]]]] <- length(simulation[simulation == result_names[[i]]])
    total_value <- total_value + (output[[result_names[[i]]]] * values[[i]])
  }
  output[["Returns"]] <- total_value
  return(bind_cols(output))
}

get_sim_table <- function(n, cum_percentages, result_names, values, expected_value) {
  tickets <- 1:n
  sims <- map(tickets, ~simulate_n_tickets(., cum_percentages, result_names))
  sim_table <- map_dfr(sims, ~n_tickets_register(., result_names, values)) %>% 
    mutate(`Mean Returns` = round(Returns / Tickets, digits = 2), 
           Profit = round((`Mean Returns` - expected_value) * Tickets, digits = 2))
  return(sim_table)
}

# Ploting functions ----

plot_theoretical_distribution <- function(description_list) {
  description_list$theoretical_distribution %>% 
    ggplot(aes(x = Values, y = Frequency)) +
    geom_col(fill = "#344e71", color = "#1e2938", width = 0.95) +
    labs(title = "Theoretical Distribution of Returns (100 Tickets)") +
    theme_bw()
}

plot_expected_value <- function(description_list) {
  expected_value <- description_list$expected_value
  deviation <- expected_value * description_list$theoretical_deviation
  value_min <- expected_value - (2 * deviation)
  value_low <- expected_value - deviation
  value_high <- expected_value + deviation
  value_max <- expected_value + (2 * deviation)
  tibble(
    object = "Lottery",
    expected_value = expected_value,
    deviation = deviation
  ) %>% 
    ggplot(aes(x = object, y = expected_value, 
               ymin = expected_value - deviation,
               ymax = expected_value + deviation)) +
    geom_errorbar(color = "#1e2938", width = 0.5) +
    geom_point(color = "#1e2938", size = 3) +
    scale_y_continuous(breaks = c(value_min, value_low, expected_value, value_high, value_max),
                       limits = c(value_min, value_max)) +
    labs(title = "Expected value and deviation",
         x = "", y = "") +
    theme_bw()
}

plot_percentage_comparison <- function(table_1, table_2, tickets) {
  table <- bind_rows(
    pivot_longer(table_1, names(table_1), names_to = "Outcome") %>% 
      mutate(Frequency = value / tickets,
             Lottery = "Lottery 1"),
    pivot_longer(table_2, names(table_2), names_to = "Outcome") %>% 
      mutate(Frequency = value / tickets,
             Lottery = "Lottery 2")
  )
  table %>% 
    ggplot(aes(x = Outcome, y = Frequency)) +
    geom_col(fill = "#344e71", color = "#1e2938", width = 0.95) +
    scale_y_continuous(labels = label_percent(accuracy = 1),
                       limits = c(0, 1), breaks = c(0, 0.25, 0.5, 0.75, 1)) +
    labs(title = paste0(tickets, " Ticket", ifelse(tickets == 1, "", "s"), ": Relative Frequency of Outcomes"), 
         y = "Percentage Frequency\n\n") +
    theme_bw() +
    facet_wrap(~Lottery, scales = "free_x")
}

plot_dot_mean_returns <- function(table, lottery, expected_value) {
  table %>% 
    ggplot(aes(x = Tickets, y = `Mean Returns`)) +
    geom_point(color = "#344e71", size = 1, alpha = 0.7) +
    geom_hline(yintercept = expected_value, linetype = "dashed", color = "red") +
    labs(title = paste0("Mean Returns of ", lottery, " (simulation results)"),
         x = "Number of Tickets") +
    theme_bw()
}

# OTHER VARIABLES ----

instructions <- paste0(h3("Instructions"),
                       "<ul style = 'margin-left:8px;'><li style = 'margin-bottom:8px;'>Payoffs and percentages should be provided as lists of numbers separated by semicolons (;);</li>",
                       "<li style = 'margin-bottom:8px;'>Payoffs can be positive or negative numbers, to indicate a negative number simply use the minus sign (-);</li>",
                       "<li style = 'margin-bottom:8px;'>Percentages should be decimal numbers between 0 and 1;</li>",
                       "<li style = 'margin-bottom:8px;'>Use the American convention for decimal numbers: use a dot (.), not a comma (,);</li>",
                       "<li style = 'margin-bottom:8px;'>The sum of all percentages must equal 1 (100%);</li>",
                       "<li style = 'margin-bottom:8px;'>The length of both lists must be equal and the maximum length is 20.</li></ul><br><br>")

about <- paste(
  h2("About this project"),
  paragraph(
    "This app is an educational resource developped to help FGV's Law students better understand the concept of expected value. ",
    "It is part of a course on Decision Theory at <b>FGV's Rio de Janeiro Law School</b>, in which the students have to grapple with the problem of how to evaluate multiple 'States of the World' in the context of decisions under risk. ",
    "We use the metaphor of a lottery in the course as a simplification of this evaluation problem, and the first evaluation method the students learn is to calculate the expected value of a lottery that has finite discrete outcomes and known probabilities. ",
    "In this context, the problem becomes: how much would you be willing to pay for a lottery ticket that can give you a prize X, Y or Z with probabilities Px, Py and Pz, respectively?"
  ),
  paragraph(
    "While preparing the course, the idea then came to their teacher to use a little bit of programming to simulate different lotteries, and to compare the actual results of these simulations with the expected value of each lottery. ",
    "The goal is to let the students play with different scenarios, so they can better understand (and convince themselves) that the mean outcome of a probabilistic experiment tend towards its theoretical expected value when the experiment is reproduced many times. ",
    "Another objective of this app is to show the limitations of the concept of expected value, particularly it's disregard - as a measure of central tendency - for the dispersion of the outcomes in different lotteries. ",
    "This is why the app offers the possibility of comparing two lotteries, and the idea is to compare two lotteries with different sets of outcomes and probabilities, but with the same expected value."
  ),
  paragraph(
    "I sincerely hope you enjoy this little experiment, even if it is a bit nerdy."
  ),
  paragraph(
    "<i>Lucas Thevenard Gomes</i>"
  ))

disclaimer <- "<br><i>Please provide the information about the lotteries you want to simulate according to the instructions, then press 'Simulate'.</i><br>"

