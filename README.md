# Wide Receiver Role-Based Evaluation
## An Exploratory NFL Analysis

## Why This Project
Wide Receiver (WR) salaries continue to rise across the NFL, but production at the position remains volatile and highly dependent on context.  From the outside, it often 
feels unclear what teams are truly paying for, raw production, role within an offense, past performance, or a combination of all three.

At the same time, WRs are no longer used interchangeably.  A true WR1, a vertical deep threat, and a slot/gadget receiver can impact an offense in very different ways while 
posting similar box-score numbers.  When those roles are evaluated together, it becomes harder to understand how usage, efficiency, and contract value actually align. 

This project was built as an exploratory attempt to look at WR value through a role-based lens, focusing on how receivers are used, how they produce, and how that production 
relates to contract cost.  

## Data Overview
The analysis combines publicly available NFL data from the 2021-2024 seasons, including:
- WR performance metrics (FantasyPros, Pro Football Reference)
- Contract information based on average annual salary (Over the Cap)

To focus on stable usage patterns, the dataset was limited to wide receivers who played at least 8 games in a season.  Performance variables were chosen to reflect different 
dimensions of receiver contribution, including volume, efficiency, situational usage, and reliability.  

## Analytical approach 
Using standardized 2024 performance data and an unsupervised modeling approach (Partitioning Around Medoids).  This process resulted in four intuitive wide receiver 
archetypes.
- Do-it-all WRs (Cluster 1): Players with strong volume, red zone usage, and consistent production across multiple metrics.
- Field-Stretchers (Cluster 2): Deep threats with high yards per reception and fewer overall targets.
- Slot/Gadget WRs (Cluster 3): Short-area receivers with high YAC/R and lower scoring totals.
- Boom-or-Bust Depth WRs (Cluster 4): Players who rely on touchdowns and deep shots but had inconsistent usage and drop rate concerns.


Once these roles were defined, the analysis shifted to exploring how production and contract cost relate within each group.

<img width="500" height="350" alt="image" src="https://github.com/user-attachments/assets/4de2e9aa-0254-4dda-acf3-27c53955408f" />

## Exploring Production and Contract Value
To examine value beyond raw production totals, performance was evaluated relative to salary.  Rather than relying on a single metric, the analysis explored which performance
measures tend to align most closely with pay, then combined those metrics into a value score.  

<img width="450" height="300" alt="image" src="https://github.com/user-attachments/assets/1f3dcc59-a35c-4755-ad52-a34b088bc6f4" />

Reliability was also incorporated by penalizing higher drop rates, which help separate consistent contributors from players who production was more volatile.  This adjustment
reshaped value rankings and highlighted how efficiency and reliability can change the interpretation of production relative to cost.  
- <img width="321" height="44" alt="image" src="https://github.com/user-attachments/assets/847c313b-a941-4cda-ae6c-699020861107" />


A notable pattern that emerged was the influence of contract timing (Top 10 snapshot of WR1/Cluster 1 below). Many of the strongest value signals came from younger, high 
performing receivers still on rookie contracts, while some veteran receivers appeared less efficient as salaries reflected prior production rather than current role.

<img width="500" height="450" alt="image" src="https://github.com/user-attachments/assets/4627d048-0104-45cd-9461-498d49d080b4" />

## Key Takeaways
- WRs naturally fall into role-based performance profiles rather than forming a single continuum.
- Teams tend to pay primarily for volume and scoring opportunity.
- Reliability (drop rate) can meaningfully change how production should be interpreted relative to salary.
- Contract timing, especially rookie versus veteran deals, plays a major role in perceived value.
These results reinforce that WR value is highly contextual.  Scheme, quarterback play, surrounding personnel, and role within the offense all influence how production
translates to contract efficiency.

## Final Note
This analysis is exploratory, not prescriptive.  It is not intended to rank receivers definitively or serve as a production ready valuation model.  With more in-depth 
context, such as route data, alignment, quarterback tendencies, or scheme information, this approach could be extended further.  At a high level, it is an example of how 
clustering, contract data, and football context can be combined to ask more nuanced questions about player value.

This README is the condensed story; the full write-up in writeup/ includes the full exploratory workflow, supporting figures, and the reasoning behind each modeling choice.



