### creating Figure 2
# set the parameters
df_paras <- expand_grid(
                doubling_times=c(5,7) , # doubling times of the epidemic (in days)
                gamma=1/seq(from=5,to=15,by=2) , # 1 / mean duration of detectable infection (in days) 
                d=c(3:14)  # time that a visitor spends in Wuhan (exposed) (in days)
)

# compute the ratio
pf <- df_paras %>% 
                # compute mean infect_dur = 1/gamma
                # compute growth_r=log(2)/doubling_times
                mutate( infect_dur=1/gamma,
                        growth_r=log(2)/doubling_times ) %>% 
                # compute from this the approximation for ratio_visitor_resident
                mutate( ratio_visitor_resident= 1-exp(-(growth_r+gamma)*d) )

pf %>% 
                # plot
                ggplot( aes(d, infect_dur , fill = ratio_visitor_resident) ) +
                geom_tile(colour = "white",  show.legend = F) +
                # here you add the numbers insited the tiles
                geom_text( aes(label=round(ratio_visitor_resident,2) ),
                           # choose text size according to crowdedness
                           size=2.7, color="black" ) +
                scale_fill_gradient(low="orange", high="white") +
                facet_grid(doubling_times~.) +
                # scale of plot
                coord_cartesian(xlim=c(3-0.2,14+0.2),ylim=c(5-0.8,15+0.8)) +
                scale_x_continuous(breaks=c(1:15)) +
                scale_y_continuous(breaks=seq(from=5,to=15,by = 2)) +
                # change how the plot looks
                theme( panel.background = element_rect(fill="lightgrey"), 
                       panel.grid = element_blank(), 
                       axis.ticks = element_blank()) +
                # give axis texts
                labs(x="Duration of visit (days)",
                     y="Time-to-recovery (days)",
                     #fill="Ratio of visitor to resident prevalence",
                     title=""); ggsave("~/Desktop/vis_res_ratio.pdf",width = 17.1, height = 13, unit = "cm")
                     
 # creating Supplement Figure 2
 # set the parameters
df_paras <- expand_grid(
                epsilon=c(0.5,1,1.5), # the ratio of (traveler exposure)/resident exposure
                doubling_times=c(5) , # doubling times of the epidemic (in days)
                gamma=1/seq(from=5,to=15,by=2) , # 1 / mean duration of detectable infection (in days) 
                d=c(3:14)  # time that a visitor spends in Wuhan (exposed) (in days)
)

# compute the ratio
pf <- df_paras %>% 
                # compute mean infect_dur = 1/gamma
                # compute growth_r=log(2)/doubling_times
                mutate( infect_dur=1/gamma,
                        growth_r=log(2)/doubling_times ) %>% 
                # compute from this the approximation for ratio_visitor_resident
                mutate( ratio_visitor_resident= epsilon*(1-exp(-(growth_r+gamma)*d)) )

pf %>% 
                # plot
                ggplot( aes(d, infect_dur , fill = ratio_visitor_resident) ) +
                geom_tile(colour = "white",  show.legend = F) +
                # here you add the numbers insited the tiles
                geom_text( aes(label=round(ratio_visitor_resident,2) ),
                           # choose text size according to crowdedness
                           size=2.7, color="black" ) +
                scale_fill_gradient2(low="orange", midpoint=1, mid = "white" , high="blue") +
                facet_grid(epsilon~.) +
                # scale of plot
                coord_cartesian(xlim=c(3-0.2,14+0.2),ylim=c(5-0.8,15+0.8)) +
                scale_x_continuous(breaks=c(1:15)) +
                scale_y_continuous(breaks=seq(from=5,to=15,by = 2)) +
                # change how the plot looks
                theme( panel.background = element_rect(fill="lightgrey"), 
                       panel.grid = element_blank(), 
                       axis.ticks = element_blank()) +
                # give axis texts
                labs(x="Duration of visit (days)",
                     y="Time-to-recovery (days)",
                     #fill="Ratio of visitor to resident prevalence",
                     title=""); ggsave("~/Desktop/vis_res_ratio_suppl.pdf",width = 17.1, height = 13, unit = "cm")
