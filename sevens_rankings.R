#Elo for rugby 7's starting with 1999/2000 series

#Weights: 20 = friendly
#         30 = tournament    
#         40 =
#         50 =
#         60 = 

#limited Elo: games aren't weighted...(or all weighted same)

library(ggplot2)
library(directlabels)
load("elo_rugby/sevens.rdata")

team_record <- function(match_data, teamn) {
    match_loc <- 
        match_data$teams.name1 == teams[teamn] | match_data$teams.name2 == teams[teamn]
    team_matches <- match_data[match_loc,]
    
    team_matches <- as.data.frame(lapply(team_matches, function(x) if(is.factor(x)) factor(x) else x))
    
    tm <- sapply(team_matches, is.factor)
    team_matches[tm] <- lapply(team_matches[tm], as.character)
    
    for (i in 1:length(team_matches$matchId)) {
        print("********************************")
        print(team_matches$events.label[i])
        print(team_matches$time.label[i])
        print(c(team_matches$teams.name1[i],team_matches$teams.name2[i]))
        print(c(team_matches$scores1[i],team_matches$scores2[i]))
        print(c(team_matches$teams.elo1[i],team_matches$teams.elo2[i]))
    }
    
}

elo_update <- function(elo1, elo2, score1, score2, weight = 30) {
    # k is adjusted weight of game.  Includes margin of victory adjustmenta and
    # NFL-Elo type adjustment for to avoid inflation due to favorites with 
    # large victoriesadjusted by     

    k <- weight*log(2 + abs(score1 - score2))*(2.2/(2.2 + abs(elo1 - elo2)/1000))
    we1 <- 1.0/(10**((elo2-elo1)/400)+1)
    we2 <- 1.0/(10**((elo1-elo2)/400)+1)
    
    if (score1 == score2) {
        new_elo1 <- elo1 + k*(0.5 - we1)
        new_elo2 <- elo2 + k*(0.5 - we2)
    } else {
        if (score1 > score2) {
            new_elo1 <- elo1 + k*(1 - we1)
            new_elo2 <- elo2 + k*(0 - we2)
        } else {
            new_elo1 <- elo1 + k*(0 - we1)
            new_elo2 <- elo2 + k*(1 - we2)
        }
    }
    
    return(c(new_elo1, new_elo2))
}

ws99_data <- subset(sevens_data, grepl("1999/00", sevens_data$events.label))
ws99_data <- as.data.frame(lapply(ws99_data, function(x) if(is.factor(x)) factor(x) else x))
ws99_data$teams.elo1 <- 0
ws99_data$teams.elo2 <- 0

ws99_data$teams.name1 <- 
    sub(" 7s", "", ws99_data$teams.name1, ignore.case = TRUE)
ws99_data$teams.name2 <- 
    sub(" 7s", "", ws99_data$teams.name2, ignore.case = TRUE)
ws99_data$events.label <- 
    sub("1999/00 IRB Sevens World Series - ", "", ws99_data$events.label, ignore.case = TRUE)
ws99_data$events.label <- 
    sub("1999/00 IRB Sevens World Series- ", "", ws99_data$events.label, ignore.case = TRUE)

#teams <- sort(unique(c(levels(ws99_data$teams.name2),levels(ws99_data$teams.name1))))
teams <- sort(unique(c(ws99_data$teams.name2,ws99_data$teams.name1)))

nteams <- length(teams)
nmatches <- length(ws99_data$matchId)
match_order <- rank(ws99_data$time.millis, ties.method = "first")

#create Elo history array

#elos <- matrix(data = 0, nrow = nteams, ncol = nmatches, dimnames = as.list(teams))
elos <- array(data = 0, dim = c(nteams,nmatches), dimnames = teams)
elos[,1] <- 1500 # initialize all teams to 1500

elo_data <- data.frame(matchID = integer(2*nmatches),
                       matchNUM = integer(2*nmatches),
                       time.milli = double(2*nmatches), 
                       time.label = character(2*nmatches),
                       event = character(2*nmatches),
                       country = character(2*nmatches),
                       opponent = character(2*nmatches),
                       score = integer(2*nmatches),
                       score_opp = integer(2*nmatches),
                       elo.i = double(2*nmatches),
                       elo.f = double(2*nmatches),
                       stringsAsFactors = FALSE)

for (i in 1:nmatches) {
    if (i > 1) {              # copy previous matches data over
        elos[,i] <- elos[,i-1]
    }   
    score1 <- ws99_data$scores1[i]         
    score2 <- ws99_data$scores2[i]
        
    elo1 <- elos[ws99_data$teams.name1[i] == teams,i]
    elo2 <- elos[ws99_data$teams.name2[i] == teams,i]
    
    new_elos <- elo_update(elo1, elo2, score1, score2, weight = 30)

    elos[ws99_data$teams.name1[i] == teams,i] <- new_elos[1]
    elos[ws99_data$teams.name2[i] == teams,i] <- new_elos[2]
    
    ws99_data$teams.elo1[i] <- new_elos[1]
    ws99_data$teams.elo2[i] <- new_elos[2]
    
    elo_data$matchID[2*i-1] <- ws99_data$matchId[i]
    elo_data$matchNUM[2*i-1] <- match_order[i]
    elo_data$time.milli[2*i-1] <- ws99_data$time.millis[i]
    elo_data$time.label[2*i-1] <- as.character(ws99_data$time.label[i])
    elo_data$event[2*i-1] <- as.character(ws99_data$events.label[i])
    elo_data$country[2*i-1] <- as.character(ws99_data$teams.name1[i])
    elo_data$opponent[2*i-1] <- as.character(ws99_data$teams.name2[i])
    elo_data$score[2*i-1] <- score1
    elo_data$score_opp[2*i-1] <- score2
    elo_data$elo.i[2*i-1] <- elo1
    elo_data$elo.f[2*i-1] <- new_elos[1] 
    
    elo_data$matchID[2*i] <- ws99_data$matchId[i]
    elo_data$matchNUM[2*i] <- match_order[i]
    elo_data$time.milli[2*i] <- ws99_data$time.millis[i]
    elo_data$time.label[2*i] <- as.character(ws99_data$time.label[i])
    elo_data$event[2*i] <- as.character(ws99_data$events.label[i])
    elo_data$country[2*i] <- as.character(ws99_data$teams.name2[i])
    elo_data$opponent[2*i] <- as.character(ws99_data$teams.name1[i])
    elo_data$score[2*i] <- score2
    elo_data$score_opp[2*i] <- score1
    elo_data$elo.i[2*i] <- elo2
    elo_data$elo.f[2*i] <- new_elos[2] 
    
}

#convert tournament list from alphabetical to chronological:
elo_data$event = factor(elo_data$event,levels(factor(elo_data$event))
                        [c(2,7,6,4,10,8,1,3,9,5)])

#Find core teams (teams participating in all tournaments)
country_games <- table(as.factor(elo_data$country))
core_teams <- labels(country_games[country_games > 27])

save(ws99_data, elo_data, elos, file = 'sws19992000.rdata')

#factor event AND get step to work AND fix horizontal axis AND annotate?
#core team overall
p1 <- ggplot(elo_data[elo_data$country %in% core_teams[[1]], ], 
             aes(matchNUM, elo.f, color = country)) + 
    geom_point() + 
    geom_step() + 
    ggtitle(expression("Elo Rating for core teams 1999/2000 IRB Sevens Series")) +
    ylab(expression("Elo Rating after each match")) +
    xlab(expression("Match Number"))
    
#core team by tournament
p2 <- ggplot(elo_data[elo_data$country %in% core_teams[[1]], ], 
       aes(matchNUM, elo.f, color = country)) + 
    geom_point() + 
    geom_step() + 
    facet_grid(. ~ event, space = "free") + 
    facet_wrap(~ event, scales = "free_x") +
    ggtitle(expression("Elo Rating for core teams 1999/2000 IRB Sevens Series")) +
    ylab(expression("Elo Rating after each match")) +
    xlab(expression("Match Number"))

#all teams, summary
p3 <- ggplot(elo_data, aes(matchNUM, elo.f, color = country)) + 
    geom_point() + 
    geom_step() + 
    ggtitle(expression("Elo Rating for 1999/2000 IRB Sevens Series")) +
    ylab(expression("Elo Rating after each match")) +
    xlab(expression("Match Number"))
direct.label(p3, list(cex = 0.5, last.points, hjust = -0.05))

#all teams by tournaments
p4 <- ggplot(elo_data, aes(matchNUM, elo.f, color = country)) + 
    geom_point() + 
    geom_step() + 
    facet_grid(. ~ event, space = "free") + 
    facet_wrap(~ event, scales = "free_x") +
    ggtitle(expression("Elo Rating for 1999/2000 IRB Sevens Series")) +
    ylab(expression("Elo Rating after each match")) +
    xlab(expression("Match Number"))

#plot elo at end of each tournament for core teams

#plot elo at end of each tournament for all teams


#Plot the graph based on ggplot2
#ggplot(neiBaltimore, aes(year, Emissions, color=type)) +
#    geom_line(stat="summary", fun.y = "sum") +
#    ylab(expression('Total PM'[2.5]*" Emission")) +
#    ggtitle(expression("Total Emissions of PM" [2.5]*" in Baltimore from 1999 to 2008"))

#ggplot(ws99_data, aes())

#tie together names with final weeks ratings and sort

#tie together ratings and names after each event and sort

#Elo evolution (after each tournament)



