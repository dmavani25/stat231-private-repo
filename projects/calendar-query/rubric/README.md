
# Calendar Query Proposal 

Identify **2–4 primary questions** of interest to you about how you spend your time. We may be sharing results with peers, so **be sure you identify questions you feel comfortable sharing with your peers and instructor**. You will be collecting data daily for 14 days (or more!) after your proposal is approved. 


Feel free to expand upon the basic question of "How do I spend my time?" or explore a variation of it. Some other ideas include...
* Document *intended time* doing things (e.g. studying, sleeping) versus *actual time* doing those things, and compare results

* Document time spent on each course, and/or time spent on different parts of a course (e.g. in class, reading, homework, etc.)

* Document time spent on school vs. work vs. leisure vs. rest, etc.

* If you already use Google Calendar as a way to keep track of your schedule, you could compare how your time was spent last year at this time versus how your time is being spent this semester.

---

*Note that you are not wed to the ideas you record here.  The visualizations and table can change before your final submission.  But, I want to make sure your plan aligns with your questions and that you're on the right track.Note that you are not wed to the ideas you record here.  The visualizations and table can change before your final submission.  But, I want to make sure your plan aligns with your questions and that you're on the right track.*


---
**Copy and paste these questions into a new issue in your private repo. Respond to these questions by 10pm ET on Thursday, September 9. I will respond with my approval or comments for revision.**

## Identify what questions you are planning to focus on
1. Document *intended time* doing things (career related work, sleeping, playing sports, meditating, procrastination) versus *actual time* doing those things, and compare results.

2. Document time spent in (school + career + work) vs (recreation + sports + leisure) vs sleep

3. Document time spent on social media vs time spent in (recreation +sports +other leisure)



## Describe two visualizations (type of plot, coordinates, visual cues, etc.) you imagine creating that help address your questions of interest

1. (bar graph, day on x-axis, time on y-axis, colors for different activities)

2. (Pie chart, angle indicating time, color indicating activities)


## Describe one table (what will the rows be?  what will the columns be?) you imagine creating that helps address your questions of interest

The rows would be time spent doing that activity day-wise and the average time row additionally.

The columns will indicate different activities so that I can fill up the repective day-wise times on cells


........................
ggplot(data = pie_chart_dataset,
         mapping = aes(x = new_activities,
                       y = time,
                       color = new_activities,
                       fill = new_activities)) +
   geom_col()+
    labs(title = "Actual time spent by activity Pie Chart",
         subtitle = "in 2 week period",
         y = "Time",
         x = "Activity")+
  coord_polar(theta = "y") +
  theme(legend.position="none")+
  theme_void()

ggplot(data = pie_chart_dataset,
         mapping = aes(#x = new_activities,
                       y = time,
                       color = new_activities,
                       fill = new_activities)) +
   geom_bar(stat = "count")+
    labs(title = "Actual time spent by activity Pie Chart",
         subtitle = "in 2 week period",
         y = "Time",
         x = "Activity")+
  coord_polar(theta = "y") +
  theme(legend.position="none")+
  theme_void()

geom_col(data = time_activities)+
    labs(title = "Actual/intended time vs activity bar graph",
         subtitle = "in 2 week period",
         y = "Time",
         x = "Activity")
