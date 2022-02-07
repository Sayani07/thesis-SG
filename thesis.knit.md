---
title: "Visualization and analysis of probability distributions of large temporal data"
author: "Sayani Gupta"
degreetype: Doctor of Philosophy
site: bookdown::bookdown_site
degrees: M.Stat, B.Sc (Stat Hons)
bibliography: [bib/gravitas.bib, bib/hakear.bib, bib/gracsr.bib, bib/thesisrefs.bib]
link-citations: yes
---



<!-- 

# Welcome {-}

This is the website for my PhD thesis at Monash University (Australia), titled "Visualization and analysis of probability distributions of large temporal data".

2022-02-07

-->

<!--chapter:end:index.Rmd-->

 

# Copyright notice {- #ch-copyright}

\textcopyright { }Sayani Gupta (\number\the\year). 

I certify that I have made all reasonable efforts to secure copyright permissions for third-party content included in this thesis and have not knowingly added copyright content to my work without the owner's permission.

\newpage



<!--chapter:end:Rmd//00-a-copyright.Rmd-->

# Abstract {- #ch-abstract}

The research was motivated by the desire to understand some Australian smart meter data, which was collected half-hourly for two years at the household level. With temporal data available at ever finer scales, exploring periodicity can become overwhelming with so many possible temporal deconstructions to explore.
Analysts are expected to comprehensively explore the many ways to view and consider temporal data. However, the plethora of choices and the lack of a systematic approach to do so quickly can make the task daunting.

<!-- <WHY> <WHAT> <HOW> -->

This work investigates how time may be dissected, resulting in alternative data segmentation and, as a result, different visualizations that can aid in the identification of underlying patterns. The first contribution (Chapter \@ref(ch-gravitas)) describes classes of time deconstructions using linear and cyclic time granularities. It provides tools to compute possible cyclic granularities from an ordered (usually temporal) index and also a framework to systematically explore the distribution of a univariate variable conditional on two cyclic time granularities by defining "harmony". A "harmony" denotes pairs of granularities that could be effectively analyzed together and reduces the search from all possible options. This approach is still overwhelming for human consumption due to the vast number of harmonies remaining. The second contribution (Chapter \@ref(ch-hakear)) refines the search for informative granularities by identifying those for which the differences between the displayed distributions are greatest and also rating them in order of importance of capturing maximum variation. The third contribution (Chapter \@ref(ch-gracsr)) builds upon the first two to provide methods for exploring heterogeneities in repetitive behavior for many time series and over multiple granularities. It accomplishes this by providing a way to cluster time series based on probability distributions across informative cyclic granularities. Although we were motivated by the smart meter example, the problem and the solutions we propose are practically relevant to any temporal data observed more than once per year.


\newpage

<!--chapter:end:Rmd//00-b-abstract.Rmd-->

# Declaration {-#ch-declare}

This thesis is an original work of my research and contains no material which has been accepted for the award of any other degree or diploma at any university or equivalent institution and that, to the best of my knowledge and belief, this thesis contains no material previously published or written by another person, except where due reference is made in the text of the thesis.

\vspace{4em}

\textbf{Student signature:}


\begin{flushleft}\includegraphics[width=0.2\linewidth]{img/signature} \end{flushleft}

\textbf{Student name:} Sayani Gupta

\textbf{Date:} 2021-11-19

<!--chapter:end:Rmd//00-c-declaration.Rmd-->

# Acknowledgements {-#ch-thanks}


<!-- - supervisors -->

I am deeply grateful to my supervisors Rob J Hyndman and Dianne Cook, who are phenomenal leaders in their respective fields and lead by example. I am inspired by their creativity, wisdom, discipline, and dedication to contribute to the society through their research. Thank you for constantly pushing me to improve as a programmer and researcher and regularly sharing good practices for conducting research. Looking back, I am grateful for how my thoughts and work in statistical computing, graphics, and data analytics, in general, have evolved over the years. I still have a lot to learn, but I am a much more self-reliant and independent researcher than when I started. Di has been instrumental in exposing me to the potential benefits of effective data visualization. As a female researcher, I am encouraged by her willingness to pursue unconventional avenues and have frequently noticed her make conscious and proactive choices to question existing stereotypes and biases. Thank you, Di, for being a fantastic female role model. I would also like to express my gratitude to my supervisor Dr. Peter Toscas of Data61 CSIRO, for always being available to help and offering feedback on practical elements of analyzing smart meter data at our monthly meetings.


<!-- - Department and conferences -->

I want to thank the Department of Econometrics and Business Statistics, Monash Business School, ARC Centre of Excellence for Mathematical and Statistical Frontiers (ACEMS), Data61 CSIRO for their financial support. Thanks to the Department, I attended a few conferences in person (before COVID hit) and had a great time networking and being inspired by many people. A few highlights are UseR! 2018 (Brisbane), Young Statistician's Conference 2019 (Canberra), WOMBAT 2019 (Melbourne), and ROpenSci 2019 (Sydney). Thank you to Gael Martin, Farshid Wahid, Klaus Ackermann, and Didier Nibbering from my PhD committee for providing such a supportive environment.


 <!-- - PhD friends and R community -->
Thank you, Puwasala Gamakumara, Stephanie Kobakian and Nicholas Tierney, for being so warm, kind and welcoming when I began my PhD. Being a part of such an inclusive environment meant a lot to me as an international student. Thank you, Nicholas Spyrison and Mitchell O'Hara-Wild, for the numerous informative discussions, brainstorming sessions, as well as for generously sharing your expertise and experience in software development. A special thanks goes to Stephanie, Emi Tanaka, Nicholas Tierney, and Nicholas Spyrison for taking the time out of their busy schedules to proofread parts of my thesis. Thank you to each of you, NUMBATs, and the broader R community for directly and indirectly helping me countless times. You never fail to inspire me.

<!-- - Friends -->

Doing a PhD in COVID has been hard, and I would not have been able to make it through this if not for the emotional support of my friends and family. Thank you Puwasala, Sium, and Ian, for being so kind and acting as a pillar of emotional support, especially during the last leg of my journey. Thank you Tushar, Samarpita and Nairita for always motivating me and believing in me when I did not believe in myself. Thank you to each one of you for being available, listening to me, and putting things in perspective when I lost sight. Thanks to my housemates Anjali, Surbhi, and Dulaji for the fun company, food and conversations that kept me sane in the COVID lockdowns.


<!-- - Family -->

A big thank you to my family for always being supportive of my choices. Thanks to my mum (Nupur Gupta) and dad (Arun Prasad Gupta), from whom I learnt that no matter where you start from, if you persevere and are sincere in your efforts, you can sail through any difficult situation. They are my constant cheerleaders for all little and big endeavors. Thanks to my brother (Avijit) for always having my back and inspiring me to dream bigger. Thank you, Juhi (sister-in-law), for reminding me of the value of organization in all aspects of life and the importance of prioritizing my physical health from time to time.
Lastly, thank you to my one-year-old nephew (Rayan) and niece (Mehr) for being the ultimate stress relievers. Thank you all for being a part of this journey with me. I love you.

<!-- Thank you for making me feel at home away from home, Somdatta (cousin). -->
<!-- add people who are helping you with proofreading -->
<!-- Although the journey is coming to an end, you wish to take these life learnings forward -->
<!-- add Earo and Stuart that you followed their model of workflow and templates, using the power of reproducible and accessible  research -->

Throughout my PhD candidature, I have had the great privilege of working with and being inspired by my advisers and many other people, which resulted in a great lot of learning in both academics and life. Although the journey is coming to a close, I want to carry these life lessons forward, and I would like to conclude with one of my favorites from Rob. "Be kind; remember you are collaborating with your future self."

<!--chapter:end:Rmd//00-d-acknowlegements.Rmd-->

# Preface {-#ch-preface}

Chapter \@ref(ch-gravitas) has been published online at the [_Journal of Computational and Graphical Statistics_](https://www.tandfonline.com/eprint/NXKCSZHIBFXCJD2NKB68/full?target=10.1080/10618600.2021.1938588). It has won the ACEMS Business Analytics Prize in 2020. The accompanying R package `gravitas` is on CRAN. Chapter \@ref(ch-hakear) and Chapter \@ref(ch-gracsr) are yet to be submitted.

### Open and reproducible research {-}

This thesis is written in R Markdown [@rmarkdown] with `bookdown` [@bookdown], using `renv` [@renv] to create reproducible environments. The online version of this thesis is hosted at <https://sayani07.github.io/thesis-SG>. All materials (including the data sets and source files) required to reproduce this document can be found at the Github repository <https://github.com/Sayani07/thesis-SG>.

### License {-}

<a rel="license" href="http://creativecommons.org/licenses/by-nc-sa/4.0/"><img alt="Creative Commons Licence" style="border-width:0" src="https://i.creativecommons.org/l/by-nc-sa/4.0/88x31.png" /></a>

This work is licensed under a [Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License](http://creativecommons.org/licenses/by-nc-sa/4.0/).

The code used in this document is available under the [MIT license](https://opensource.org/licenses/MIT).

\clearpage\pagenumbering{arabic}\setcounter{page}{0}

<!--chapter:end:Rmd//00-e-preface.Rmd-->

# Introduction {#ch-intro}

<!-- Introduction -->
<!-- Motivation & Background -->
<!-- Outline of the thesis -->

<!-- To tame the complexity of time, breaking it into years, months, weeks, days and so on in a hierarchical manner is a common way to relate data to time. Such discrete human made abstractions of time can be thought of as time granularities.(Aigner et al., 2008). -->

<!-- outline the intro chapter structure -->

<!-- This chapter provides an introduction to the study by first discussing the background and context, followed by the overarching research aim and objectives, each of which correspond to each chapter in this thesis. -->

<!-- Background -->

The Smart Grid, Smart City (SGSC) project (2010–2014) available through the [Department of the Environment and Energy](https://data.gov.au/data/organization/doee) provides half-hourly data of over 13,000 Australian electricity smart meter customers distributed unevenly from October 2011 to March 2014. The wide variety of customers means that there will be large variance in behavior, leading to greater uncertainty in the data. Behavioral patterns vary significantly due to differences in size, location, and amenities such as solar panels, central heating, and air conditioning. For example, some families use a dryer, while others hang their clothes to dry. This could be reflected in their weekly profile. They may vary on a monthly basis, with some customers using more air conditioners or heaters than others despite having comparable electrical equipment and weather conditions. Some customers are night owls, while others are morning larks, which may show up in their daily profile. Customers' day-off energy consumption varies depending on whether they stay at home or go outside. 

<!-- introducing the overall field-->
With the availability of data at finer and finer time scales, exploration of time series data may be required to be carried out across both finer and coarser scales to draw useful inferences about the underlying process. To reduce the complexity of time, it is typical to divide it into years, months, weeks, days, and so on in a hierarchical manner [@aigner2011visualization]. These discrete abstractions of time are known as time granularities. Linear time granularities [@Bettini1998-ed], such as hours, days, weeks and months, respect the linear progression of time and are non-repeating. Cyclic temporal granularities representing cyclical repetitions in time (such as hour-of-the-day, work-day/weekend) are effective for analyzing repetitive patterns in time series data.


To acquire a comprehensive view of the repeated patterns, it is necessary to navigate through all of the conceivable cyclic granularities. This approach is consistent with the concept of exploratory data analysis EDA [@Tukey1977-jx], which stresses the utilization of multiple perspectives on data to assist with formulating hypotheses before proceeding to formal inferences or modeling. This, however, is a challenging process since it throws up a myriad of possible hypotheses. Furthermore, the transition from linear to cyclic granularities results in restructured data, with each level of the temporal deconstruction corresponding to multiple values of the observed variable. This motivates the research presented in this thesis, which aims to provide a platform for systematically exploring probability distributions induced by these multiple observations to support the discovery of regular patterns or anomalies, as well as the exploration of clusters of behaviors or the summarization of the behavior.



<!-- There could potentially be many, many ways to analyze the data across cyclic granularities. -->


<!-- For example, data collected at an hourly scale could be analyzed using coarser temporal scales such as days, months or quarters. This approach requires deconstructing time in various possible ways. Moreover, it might often be interesting to capture calendar or periodic effects like month-of-year, day-of-week, or hour-of-day. They help us answer questions like if certain levels of those time deconstructions are characterized by unusual/typical values of the observed variable. For example, certain days of the week or months of the year are likely to be characterized by higher values. Visualizations using linear time granularities are often ineffective in conveying repetitive behaviors because multiple observations are squeezed together in that representation. Hence, we define cyclic granularities for exploring repetitive patterns. There could potentially be many, many cyclic granularities to explore. It is essential to navigate through all the options to get a comprehensive view of the repeated patterns. This idea aligns with the notion of EDA (Tukey 1977), which emphasizes the use of multiple perspectives on data to help formulate hypotheses before proceeding to hypothesis testing. -->

<!-- introduce the specific problem  -->
<!-- ## change in data structure as a result of transitioning from linear to cyclic deconstructions -->
<!-- + Existing approaches, why we need to drill down, or why we want to -- probability distributions -->



<!-- My contribution in this thesis is to explore probability distributions induced by these multiple observations to better understand heterogeneity in repetitive behavior that can arise across temporal dependencies. -->


<!-- Summarizing the probability distribution of these multiple observations to capture both shape and uncertainty could be a potential approach to understanding the underlying distribution of these observations.  -->

<!-- Hence, the overarching research goal is to study the periodic behavior of temporal data in a structured way by studying the probability distributions by best exploiting the characteristics of time. -->




<!-- Following research objectives would facilitate the achievement of this aim -->
<!-- Research objectives -->

<!-- ## Research aims -->


<!-- The first part of the work discusses computation of all possible combinations of cyclic time granularities and a graphical mapping such that distributions of a numeric response variable is displayed across combinations of two cyclic granularities. Even analyzing the distribution of the measured variable across two cyclic granularities at once could amount to displaying many plots in search of potential  patterns. Thus, the first part of the research also introduces "harmony" to denote pairs of granularities that could be analyzed together and reduces the search from all possible options. But this approach is still overwhelming for human consumption because there would still be huge number of harmonies. Hence, the second part of the research extends this work and narrows the search further by finding pair of cyclic granularities which are informative enough and rank them according to their importance. However, to explore periodic patterns of many households, we have to resort to clustering which has been addressed in the third part of the research. Although the motivation came through the smart meter example, this is a problem that is relevant to any temporal data observed more than once per year. -->

## Visualizing probability distributions across bivariate cyclic temporal granularities {#sec:gravitas}

Chapter \@ref(ch-gravitas) describes classes of time deconstructions using linear and cyclic time granularities, which can be used to create data visualizations to explore periodicities, associations, and anomalies. It provides a formal characterization of cyclic granularities and facilitates manipulation of single- and multiple-order-up time granularities through cyclic calendar algebra, as well as providing a recommendation algorithm to check the feasibility of creating plots for any two cyclic granularities. Our proposed method is also applicable to non-temporal hierarchical granularities with an underlying ordered index. The methods are implemented in the open-source R package `gravitas` and are consistent with a tidy workflow [@wickham2016r], with probability distributions examined using the range of graphics available in `ggplot2` [@Wickham2009pk].

 
## Detecting distributional differences between temporal granularities for exploratory time series analysis {#sec:hakear}

Chapter \@ref(ch-hakear) is a natural extension of Chapter \@ref(ch-gravitas). Many displays might be built using cyclic granularities. However, only a handful of them may reveal major patterns of interest. Identifying the displays which exhibit "significant" distributional differences and plotting only these would allow for more efficient exploration. Furthermore, a few of the displays in this collection will be more engaging than others. Chapter \@ref(ch-hakear) provides a new distance metric for selecting and ranking the multiple granularities. The statistical significance of potential visual discoveries is aided by selecting a threshold for the proposed numerical distance measure. The distance measure is computed for a single or pairs of cyclic granularities, and it can be compared across different cyclic granularities as well as a collection of time series. This chapter also includes a case study using residential smart meter data from Melbourne to demonstrate how the suggested methodology may be utilized to automatically find temporal granularities with significant distributional differences. The methods are implemented in the open-source R package `hakear`.

## Clustering time series based on probability distributions across temporal granularities {#sec:gracsr}

In Chapter \@ref(ch-gracsr), we look at the problem of using clustering to discover patterns in a large number of univariate time series across multiple temporal granularities. Time series clustering research is gaining traction as more data is collected at finer temporal resolution, over longer time periods, and for a larger number of individuals/entities. Many disciplines have noisy, patchy, uneven, and asynchronous time series that make it difficult to search for similarities. We propose a method for overcoming these constraints by calculating distances between time series based on probability distributions at various temporal granularities. Because they are based on probability distributions, these distances are resistant to missing or noisy data and aid in dimension reduction. When fed into a clustering algorithm, the distances can be used to divide large data sets into small pockets of similar repetitive behaviors. These subgroups can then be analyzed separately or used as distinct prototype behaviors in classification problems. The proposed method was tested on a group of residential electricity consumers from the Australian smart meter data set to show that it can generate meaningful clusters. This chapter includes a brief review of the literature on traditional time series clustering and, more specifically, clustering residential smart meter data.



<!-- Time series clustering research is gaining a lot of importance with more and more time series data recorded at much finer temporal resolution, for a longer period of time and for a larger number of individuals/entities.Similarity searches amongst these long time series are often limited by the type of noisy, patchy, unequal and asynchronous time series common in many fields. In this paper we propose a strategy to alleviate these limitations by  clustering time series based on probability distributions across cyclic temporal granularities. Cyclic granularities are temporal deconstructions of a time period into units such as hour-of-the-day, work-day/weekend, which can be useful for measuring repetitive patterns in large univariate time series data. Thus, looking at the probability distributions across cyclic granularity leads to a method which is robust to missing or noisy data, helps in dimension reduction while ensuring small pockets of similar repetitive behaviors. The method is applied to electricity smart meter dat -->
<!-- The suggested approach was evaluated on a set of benchmark time series on residential electricity customers. The empirical results show that our approach is able to yield meaningful clusters. -->

## Thesis structure

The thesis is structured as follows. Chapter \@ref(ch-gravitas) provides details of the cyclic granularities, different classes, and computation, and also its usage in exploratory time series analysis through applications. This is implemented in the R package `gravitas`. Chapter \@ref(ch-hakear) provides guidance on how to choose significant cyclic granularities, which are likely to have interesting patterns across its categories. This is available as the R package `hakear`. Chapter \@ref(ch-gracsr) provides methods to explore heterogeneity in repetitive behavior for multiple time series over multiple cyclic granularities. This is in the developing R package `gracsr`. Chapter \@ref(ch-conclusion) summarizes the software tools developed for the work, and discusses future plans.

<!--chapter:end:Rmd//01-intro.Rmd-->

# Visualizing probability distributions across bivariate cyclic temporal granularities {#ch-gravitas}

Deconstructing a time index into time granularities can assist in exploration and automated analysis of large temporal data sets. This paper describes classes of time deconstructions using linear and cyclic time granularities. Linear granularities respect the linear progression of time such as hours, days, weeks and months. Cyclic granularities can be circular such as hour-of-the-day, quasi-circular such as day-of-the-month, and aperiodic such as public holidays. The hierarchical structure of granularities creates a nested ordering: hour-of-the-day and second-of-the-minute are single-order-up. Hour-of-the-week is multiple-order-up, because it passes over day-of-the-week. Methods are provided for creating all possible granularities for a time index. A recommendation algorithm provides an indication whether a pair of granularities can be meaningfully examined together (a "harmony"), or when they cannot (a "clash").  
 
 Time granularities can be used to create data visualizations to explore for periodicities, associations and anomalies. The granularities form categorical variables (ordered or unordered) which induce groupings of the observations. Assuming a numeric response variable, the resulting graphics are then displays of distributions compared across combinations of categorical variables.  
  
 The methods implemented in the open source R package `gravitas` are consistent with a tidy workflow, with probability distributions examined using the range of graphics available in `ggplot2`.






## Introduction

Temporal data are available at various resolutions depending on the context. Social and economic data are often collected and reported at coarse temporal scales such as monthly, quarterly or annually. With recent advancements in technology, more and more data are recorded at much finer temporal scales. Energy consumption may be collected every half an hour, energy supply may be collected every minute, and web search data might be recorded every second. As the frequency of data increases, the number of questions about the periodicity of the observed variable also increases. For example, data collected at an hourly scale can be analyzed using coarser temporal scales such as days, months or quarters. This approach requires deconstructing time in various possible ways called time granularities [@aigner2011visualization].

It is important to be able to navigate through all of these time granularities to have multiple perspectives on the periodicity of the observed data. This aligns with the notion of exploratory data analysis (EDA) [@Tukey1977-jx] which emphasizes the use of multiple perspectives on data to help formulate hypotheses before proceeding to hypothesis testing. Visualizing probability distributions conditional on one or more granularities is an indispensable tool for exploration. Analysts are expected to comprehensively explore the many ways to view and consider temporal data. However, the plethora of choices and the lack of a systematic approach to do so quickly can make the task overwhelming.

Calendar-based graphics [@wang2020calendar] are useful in visualizing patterns in the weekly and monthly structure and are helpful when checking for the effects of weekends or special days. Any temporal data at sub-daily resolution can also be displayed using this type of faceting [@Wickham2009pk] with days of the week, month of the year, or another sub-daily deconstruction of time. But calendar effects are not restricted to conventional day-of-week or month-of-year deconstructions. There can be many different time deconstructions, based on the calendar or on categorizations of time granularities.

Linear time granularities (such as hours, days, weeks and months) respect the linear progression of time and are non-repeating. One of the first attempts to characterize these granularities is due to @Bettini1998-ed. However, the definitions and rules defined are inadequate for describing non-linear granularities. Hence, there is a need to define some new time granularities that can be useful in visualizations. Cyclic time granularities can be circular, quasi-circular or aperiodic. Examples of circular granularities are hour of the day and day of the week; an example of a quasi-circular granularity is day of the month; examples of aperiodic granularities are public holidays and school holidays.

Time deconstructions can also be based on the hierarchical structure of time. For example, hours are nested within days, days within weeks, weeks within months, and so on. Hence, it is possible to construct single-order-up granularities such as second of the minute, or multiple-order-up granularities such as second of the hour. The lubridate package [@Grolemund2011-vm] provides tools to access and manipulate common date-time objects. However, most of its accessor functions are limited to single-order-up granularities.

The motivation for this work stems from the desire to provide methods to better understand large quantities of measurements on energy usage reported by smart meters in households across Australia, and indeed many parts of the world. Smart meters currently provide half-hourly use in kWh for each household, from the time they were installed, some as early as 2012. Households are distributed geographically and have different demographic properties as well as physical properties such as the existence of solar panels, central heating or air conditioning. The behavioral patterns in households vary substantially; for example, some families use a dryer for their clothes while others hang them on a line, and some households might consist of night owls, while others are morning larks. It is common to see aggregates [see @Goodwin_2012] of usage across households, such as half-hourly total usage by state, because energy companies need to plan for maximum loads on the network. But studying overall energy use hides the distribution of usage at finer scales, and makes it more difficult to find solutions to improve energy efficiency. We propose that the analysis of smart meter data will benefit from systematically exploring energy consumption by visualizing the probability distributions across different deconstructions of time to find regular patterns and anomalies. Although we were motivated by the smart meter example, the problem and the solutions we propose are practically relevant to any temporal data observed more than once per year. In a broader sense, it could be even suitable for data observed by years, decades, and centuries as might be the case in weather or astronomical data.

This work provides tools for systematically exploring bivariate granularities within the tidy workflow [@wickham2016r]. In particular, we

  * provide a formal characterization of cyclic granularities;
  * facilitate manipulation of single- and multiple-order-up time granularities through cyclic calendar algebra;
  * develop an approach to check the feasibility of creating plots or drawing inferences for any two cyclic granularities.

The remainder of the paper is organized as follows: Section&nbsp;\ref{sec:linear-time} provides some background material on linear granularities and calendar algebra for computing different linear granularities. Section&nbsp;\ref{sec:cyclic-gran} formally characterizes different cyclic time granularities by extending the framework of linear time granularities, and introducing cyclic calendar algebra for computing cyclic time granularities. The data structure for exploring the conditional distributions of the associated time series across pairs of cyclic time granularities is discussed in Section&nbsp;\ref{sec:data-structure}. Section&nbsp;\ref{sec:visualization} discusses the role of different factors in constructing an informative and trustworthy visualization. Section&nbsp;\ref{sec:application} examines how systematic exploration can be carried out for a temporal and non-temporal application. Finally, we summarize our results and discuss possible future directions in Section&nbsp;\ref{sec:discussion}.

## Linear time granularities {#sec:linear-time}

Discrete abstractions of time such as weeks, months or holidays can be thought of as "time granularities". Time granularities are **linear** if they respect the linear progression of time. There have been several attempts to provide a framework for formally characterizing time granularities, including @Bettini1998-ed which forms the basis of the work described here.

### Definitions

\begin{defn}\label{def:definition}
A \textbf{time domain} is a pair $(T; \le)$ where $T$ is a non-empty set of time instants (equivalently, moments or points) and $\le$ is a total order on $T$.
\end{defn}

\noindent The time domain is assumed to be *discrete*, and there is unique predecessor and successor for every element in the time domain except for the first and last.

\begin{defn}\label{def:index set}
The \textbf{index set}, $Z=\{z: z \in \mathbb{Z}_{\geq 0}\}$, uniquely maps the time instants to the set of non-negative integers.
\end{defn}

\begin{defn}\label{def:linear}
A \textbf{linear granularity} is a mapping $G$ from the index set, $Z$, to subsets of the time domain such that:
  (1) if $i < j$ and $G(i)$ and $G(j)$ are non-empty, then each element of $G(i)$ is less than all elements of $G(j)$; and
  (2) if $i < k < j$ and $G(i)$ and $G(j)$ are non-empty, then $G(k)$ is non-empty.
Each non-empty subset $G(i)$ is called a \textbf{granule}.
\end{defn}

\noindent This implies that the granules in a linear granularity are non-overlapping, continuous and ordered. The indexing for each granule can also be associated with a textual representation, called the label. A discrete time model often uses a fixed smallest linear granularity named by @Bettini1998-ed **bottom granularity**. \autoref{fig:linear-time} illustrates some common linear time granularities. Here, "hour" is the bottom granularity and "day", "week", "month" and "year" are linear granularities formed by mapping the index set to subsets of the hourly time domain. If we have "hour" running from $\{0, 1, \dots,t\}$, we will have "day" running from $\{0, 1, \dots, \lfloor t/24\rfloor\}$. These linear granularities are uni-directional and non-repeating.

\begin{figure}[!htbp]

{\centering \includegraphics[width=\textwidth]{img/linear-ex} 

}

\caption{(ref:linear-time)}(\#fig:linear-time)
\end{figure}

(ref:linear-time) Illustration of time domain, linear granularities and index set. Hour, day, week, month and year are linear granularities and can also be considered to be time domains. These are ordered with ordering guided by integers and hence are unidirectional and non-repeating. Hours could also be considered the index set, and a bottom granularity.

### Relativities

Properties of pairs of granularities fall into various categories.

\begin{defn}\label{def:finerthan}
A linear granularity $G$ is \textbf{finer than} a linear granularity $H$, denoted $G \preceq H$, if for each index $i$, there exists an index $j$ such that
$G(i) \subset H(j).$
\end{defn}

\begin{defn}\label{def:groupsinto}
A linear granularity $G$ \textbf{groups into} a linear granularity $H$, denoted
$G \trianglelefteq H$, if for each index $j$ there exists a (possibly infinite) subset $S$ of the integers such that $H(j) = \bigcup_{i \in S}G(i).$
\end{defn}

\noindent For example, both $day \trianglelefteq week$ and $day \preceq week$ hold, since every granule of $week$ is the union of some set of granules of day and each day is a subset of a $week$. These definitions are not equivalent. Consider another example, where $G_1$ denotes "weekend" and $H_1$ denotes "week". Then, $G_1 \preceq H_1$, but $G_1 \ntrianglelefteq H_1$. Further, with $G_2$ denoting "days" and $H_2$ denoting "business-week", $G_2 \npreceq H_2$, but $G_2 \trianglelefteq H_2$, since each business-week can be expressed as an union of some days, but Saturdays and Sundays are not subsets of any business-week.
Moreover, with  $H_3$ denoting "public holidays", $G_1 \npreceq H_3$ and $G_1 \ntrianglelefteq H_3$.  

<!-- # talk about period after definition 6 after you have explained period -->
<!-- The relationship has period 7. -->
<!-- The relationship $day \trianglelefteq month$ has a more complicated period. If leap years are ignored, each month is a grouping of the same number of days over years, hence the period of the grouping $(day, month)$ is one year. With the inclusion of leap years, the grouping period is 400 years. -->

\begin{defn}\label{def:periodic}
A granularity $G$ is \textbf{periodic} with respect to a finite granularity $H$ if:
(1) $G \trianglelefteq H$; and
(2) there exist $R$, $P \in \mathbb{Z}_+$, where $R$ is less than the number of granules of $H$, such that for all $i \in \mathbb{Z}_{\ge {0}}$ , if $H(i) = \bigcup_{j \in S}G(j)$ and $H (i + R) \neq \phi$ then $H (i + R) = \bigcup_{j \in S} G(j + P)$.
\end{defn}

If $G$ groups into $H$, it would imply that any granule $H(i)$ is the union of some granules of $G$; for example, $G(a_1), G(a_2), \dots, G(a_k)$. Condition (2) in Definition \ref{def:periodic} implies that if $H(i + R) \neq \emptyset$, then $H(i + R) = \bigcup (G(a_1 + P), G(a_2 + P), \dots, G(a_k + P))$, resulting in a "periodic" pattern of the composition of $H$ using granules of $G$. In this pattern, each granule of $H$ is shifted by $P$ granules of $G$. $P$ is called the \textbf{period} [@Bettini2000-qk].

For example, day is periodic with respect to week with $R=1$ and $P=7$, while (if we ignore leap years) day is periodic with respect to month with $R=12$ and $P=365$ as any month would consist of the same number of days across years.
Since the idea of period involves a pair of granularities, we say that the pair $(day, week)$ has period 7, while the pair $(day, month)$ has a period 365 (ignoring leap years). 

Granularities can also be periodic with respect to other granularities, _"except for a finite number of spans of time where they behave in an anomalous way"_; these are called **quasi-periodic** relationships [@Bettini2000-vy]. In a Gregorian calendar with leap years, day groups quasi-periodically into month with the exceptions of the time domain corresponding to $29^{\text{th}}$ February of any year.

\begin{defn}\label{def:order}
The \textbf{order} of a linear granularity is the level of coarseness associated with a linear granularity. A linear granularity G will have lower order than H if each granule of G is composed of lower number of granules of bottom granularity than each granule of H.
\end{defn}

With two linear granularities $G$ and $H$, if $G$ *groups into* or is *finer than* $H$ then $G$ is of lower order than $H$. For example, if the bottom granularity is hour, then granularity $day$ will have lower order than $week$ since each day consists of fewer hours than each week.

Granules in any granularity may be aggregated to form a coarser granularity. A system of multiple granularities in lattice structures is referred to as a **calendar** by @Dyreson_2000. Linear time granularities are computed through "calendar algebra" operations [@Ning_2002] designed to generate new granularities recursively from the bottom granularity. For example, due to the constant length of day and week, we can derive them from hour using
$$
  D(j) = \lfloor H(i)/24\rfloor, \qquad W(k) = \lfloor H(i)/(24*7)\rfloor,
$$
where $H$, $D$ and $W$ denote hours, days and weeks respectively.

## Cyclic time granularities {#sec:cyclic-gran}

Cyclic granularities represent cyclical repetitions in time. They can be thought of as additional categorizations of time that are not linear. Cyclic granularities can be constructed from two linear granularities, that relate periodically; the resulting cycles can be either _regular_ (**circular**), or _irregular_ (**quasi-circular**).

### Circular granularities {#sec:circular-gran-def}

\begin{defn}\label{def:circular}
A \textbf{circular granularity} $C_{B, G}$ relates linear granularity $G$ to bottom granularity $B$ if
\begin{equation} \label{eq:circular-gran}
\begin{split}
C_{B, G}(z) & = z\mod P(B, G) \quad \forall z \in \mathbb{Z}_{\geq 0} \\
\end{split}
\end{equation}
where
$z$ denotes the index set,
$B$ groups periodically into $G$ with regular mapping and period $P(B, G)$.
\end{defn}

\begin{figure}[!htb]

{\centering \includegraphics[width=\textwidth]{img/circular-ex} 

}

\caption{(ref:circular-dow)}(\#fig:circular-dow)
\end{figure}

(ref:circular-dow) Index sets for some linear and circular granularities (a). Circular granularities can be constructed by slicing the linear granularity into pieces and stacking them (b).

\autoref{fig:circular-dow} illustrates some linear and cyclical granularities. Cyclical granularities are constructed by cutting the linear granularity into pieces, and stacking them to match the cycles (as shown in b). $B, G, H$ (day, week, fortnight, respectively) are linear granularities. The circular granularity $C_{B, G}$ (day-of-week) is constructed from $B$ and $G$, while  circular granularity $C_{B, H}$ (day-of-fortnight) is constructed from $B$ and $H$. These overlapping cyclical granularities share elements from the linear granularity. Each of $C_{B , G}$ and $C_{B , H}$ consist of repeated patterns $\{0, 1, \dots, 6\}$ and $\{0, 1, \dots, 13\}$ with $P=7$ and $P=14$ respectively.

Suppose ${L}$ is a label mapping that defines a unique label for each index $\ell \in \{ 0,1,\dots, (P-1)\}$. For example, the label mapping $L$ for $C_{B, G}$ can be defined as
$$
  L: \{0,1, \dots, 6\} \longmapsto\ \{\text{Sunday}, \text{Monday}, \dots, \text{Saturday}\}.
$$

In general, any circular granularity relating two linear granularities can be expressed as
$$
  C_{G, H}(z) = \lfloor z/P(B,G) \rfloor\mod P(G,H),
$$
where $H$ is periodic with respect to $G$ with regular mapping and period $P(G,H)$. Table&nbsp;\ref{tab:definitions} shows several circular granularities constructed using minutes as the bottom granularity.

\begin{table}[ht]
\caption{Examples of circular granularities with bottom granularity minutes. Circular granularity $C_i$ relates two linear granularities, one of which groups periodically into the other with regular mapping and period $P_i$. Circular granularities can be expressed using modular arithmetic due to their regular mapping.}
\begin{center}
\begin{tabular}{lll}
\toprule
Circular granularity & Expression & Period \\
\midrule
minute-of-hour                               &
  $C_1 = z \mod 60$                     &
  $P_1 = \phantom{99}60$ \\
minute-of-day                                &
  $C_2 = z \mod 60*24$                  &
  $P_2= 1440$\\
hour-of-day                                  &
  $C_3 = \lfloor z/60\rfloor\mod 24$    &
  $P_3 = \phantom{99}24$ \\
hour-of-week                                 &
  $C_4 = \lfloor z/60\rfloor\mod 24*7$  &
  $P_4= \phantom{9}168$\\
day-of-week                                  &
  $C_5 = \lfloor z/24*60\rfloor \mod 7$ &
  $P_5= \phantom{999}7$\\
\bottomrule
\end{tabular}
\end{center}
\label{tab:definitions}
\end{table}

### Quasi-circular granularities {#sec:quasi-circular-gran-def}

A **quasi-circular** granularity cannot be defined using modular arithmetic because of the irregular mapping. However, they are still formed with linear granularities, one of which groups periodically into the other. \autoref{tab:quasi} shows some examples of quasi-circular granularities.

\begin{table}[ht]
\caption{Examples of quasi-circular granularities relating two linear granularities with irregular mapping leading to several possible period lengths.}
\centering
\begin{tabular}{lr@{~}lr@{~}r}
\toprule
Quasi-circular granularity && Possible period lengths\\
\midrule
$Q_1 =$ day-of-month && $P_1 = 31, 30, 29, 28$\\
$Q_2 =$ hour-of-month && $P_2 = 24\times 31, 24\times 30, 24\times 29, 24\times 28$\\
$Q_3 =$ day-of-year && $P_3 = 366, 365$\\
\bottomrule
\end{tabular}
\label{tab:quasi}
\end{table}

\begin{defn}\label{def:quasicircular}
A \textbf{quasi-circular granularity} $Q_{B, G'}$ is formed when bottom granularity $B$ groups periodically into linear granularity $G'$ with irregular mapping such that the granularities are given by
\begin{equation}\label{eq:quasi}
Q_{B, G'}(z) =
z - \sum_{w=0}^{k-1}\vert T_{w \mod R'} \vert, \quad \text{for}\quad z \in T_{k},
\end{equation}
where
$z$ denotes the index set,
$w$ denotes the index of $G'$,
$R'$ is the number of granules of $G'$ in each period of $(B, G')$,
$T_w$ are the sets of indices of $B$ such that $G'(w) = \bigcup_{z \in T_w}B(z)$,
and $\vert T_w \vert$ is the cardinality of set $T_w$.
\end{defn}

For example, day-of-year is quasi-periodic with either 365 or 366 granules of $B$ (days) within each granule of $G'$ (years). The pattern repeats every $4$ years (ignoring leap seconds). Hence $R'= 4$. $Q_{B, G'}$ is a repetitive categorization of time, similar to circular granularities, except that the number of granules of $B$ is not the same across different granules of $G'$.


### Aperiodic granularities {#sec:aperiodic-gran-def}

Aperiodic linear granularities are those that cannot be specified as a periodic repetition of a pattern of granules as described in Definition \ref{def:periodic}. Aperiodic cyclic granularities capture repetitions of these aperiodic linear granularities.
Examples include public holidays which repeat every year, but there is no reasonably small span of time within which their behavior remains constant. A classic example is Easter (in the Western tradition) whose dates repeat only after 5.7 million years [@Reingold2001-kf]. In Australia, if a standard public holiday falls on a weekend, a substitute public holiday will sometimes be observed on the first non-weekend day (usually Monday) after the weekend. Examples of aperiodic granularity may also include school holidays or a scheduled event. All of these are recurring events, but with non-periodic patterns. Consequently, $P_i$ (as given in \autoref{tab:quasi}) are essentially infinite for aperiodic granularities.

\begin{defn}\label{def:aperiodic}
An \textbf{aperiodic cyclic granularity} is formed when bottom granularity $B$ groups into an aperiodic linear granularity $M$ such that the granularities are given by
\begin{equation}\label{eq:aperiodic}
A_{B, M}(z) = \begin{cases}
                  i, & \text{for}\quad z \in T_{i_j} \\
                  0  & \text{otherwise},
                \end{cases}
\end{equation}
where
$z$ denotes the index set,
$T_{i_j}$ are the sets of indices of $B$ describing aperiodic linear granularities $M_{i}$ such that $M_{i}(j) = \bigcup_{z \in T_{i_j}}B(z)$, and $M = \bigcup_{i=1}^{n}M_{i}$, $n$ being the number of  aperiodic linear granularities in consideration.
\end{defn}


\begin{figure}[!htb]

{\centering \includegraphics[width=\textwidth]{img/aperiodic-ex} 

}

\caption{(ref:aperiodic-example)}(\#fig:aperiodic-example)
\end{figure}

(ref:aperiodic-example) Quasi-circular and aperiodic cyclic granularities illustrated by linear (a) and 
stacked-displays (b) progression of time. The linear display shows the granularities days ($B$), weeks ($G$), semester days ($B'$), and stages of a semester ($M$) indexed over a linear representation of time. The granules of $B'$ are only defined for days when the semester is running. Here a semester spans 18 weeks and 2 days, and consists of 6 stages. It starts with a week of orientation, followed by an in-session period (6 weeks), a break (1 week), the second half of semester (7 weeks), a 1-week study break before final exams, which spans the next 16 days. This distribution of semester days remains relatively similar for every semester. $Q_{B',M}$ with P = 128 is a quasi-circular granularity with repeating patterns, while $A_{B,M}$ is an aperiodic cyclic granularity as the placement of the semester within a year varies from year to year with no fixed start and end dates.

For example, consider the school semester shown in \autoref{fig:aperiodic-example}. Let the linear granularities $M_1$ and $M_2$ denote the teaching and non-teaching stages of the semester respectively. Both $M_1$, $M_2$ and $M = M_{1}\bigcup M_{2}$ denoting the "stages" of the semester are aperiodic with respect to days ($B$) or weeks ($G$). Hence $A_{B, M}$ denoting day-of-the-stage would be an aperiodic cyclic granularity because the placement of the semester within a year would vary across years. Here, $Q_{B', M}$ denoting semester-day-of-the-stage would be a quasi-circular granularity
since the distribution of semester days within a semester is assumed to remain constant over years. Here semester-day is denoted by "sem day" ($B'$) and its granules are only defined for the span of the semesters.

### Relativities {#sec:cyclic-calendar}

The hierarchical structure of time creates a natural nested ordering which can be used in the computation of relative pairs of granularities.

\begin{defn}\label{def:hierarchy}
The nested ordering of linear granularities can be organized into a \textbf{hierarchy table}, denoted as $H_n: (G, C, K)$, which arranges them from lowest to highest in order. It shows how the $n$ granularities relate through $K$, and how the cyclic granularities, $C$, can be defined relative to the linear granularities. Let $G_{\ell}$ and $G_{m}$ represent the linear granularity of order $\ell$ and $m$ respectively with $\ell<m$. Then $K \equiv P(\ell,m)$ represents the period length of the grouping $(G_{\ell}, G_{m})$, if $C_{G_{\ell}, G_{m}}$ is a circular granularity and $K \equiv k(\ell,m)$ represents the operation to obtain $G_{m}$ from $G_{\ell}$, if $C_{G_{\ell}, G_{m}}$ is quasi-circular.
\end{defn}

For example, \autoref{tab:tab-mayan} shows the hierarchy table for the Mayan calendar. In the Mayan calendar, one day was referred to as a kin and the calendar was structured such that 1 kin = 1 day; 1 uinal = 20 kin; 1 tun  = 18 uinal (about a year); 1 katun = 20 tun (20 years) and 1 baktun = 20 katun.

\begin{table}

\caption{(\#tab:tab-mayan)Hierarchy table for Mayan calendar with circular single-order-up granularities.}
\centering
\begin{tabular}[t]{lll}
\toprule
linear (G) & single-order-up cyclic (C) & period length/conversion operator (K)\\
\midrule
kin & kin-of-uinal & 20\\
uinal & uinal-of-tun & 18\\
tun & tun-of-katun & 20\\
katun & katun-of-baktun & 20\\
baktun & 1 & 1\\
\bottomrule
\end{tabular}
\end{table}

Like most calendars, the Mayan calendar used the day as the basic unit of time [@Reingold2001-kf]. The structuring of larger units, weeks, months, years and cycle of years, though, varies substantially between calendars. For example,  the French revolutionary calendar divided each day into 10 "hours", each "hour" into 100 "minutes" and each "minute" into 100 "seconds", the duration of which is 0.864 common seconds. Nevertheless, for any calendar, a hierarchy table can be defined. Note that it is not always possible to organize an aperiodic linear granularity in a hierarchy table. Hence, we assume that the hierarchy table consists of periodic linear granularities only, and that the cyclic granularity $C_{G(\ell),G(m)}$ is either circular or quasi-circular.

\begin{defn}\label{def:norderup}
The hierarchy table contains \textbf{multiple-order-up} granularities which are cyclic granularities that are nested within multiple levels.
A \textbf{single-order-up} is a cyclic granularity which is nested within a single level. It is a special case of multiple-order-up granularity.
\end{defn}

\noindent In the Mayan calendar (\autoref{tab:tab-mayan}), kin-of-tun or kin-of-baktun are examples of multiple-order-up granularities and single-order-up granularities are kin-of-uinal, uinal-of-tun etc.

### Computation

Following the calendar algebra of @Ning_2002 for linear granularities, we can define cyclic calendar algebra to compute cyclic granularities. Cyclic calendar algebra comprises two kinds of operations:
(1) **single-to-multiple** (the calculation of _multiple-order-up_ cyclic granularities from _single-order-up_ cyclic granularities) and (2) **multiple-to-single** (the reverse).

#### Single-to-multiple order-up {- #s2m}

Methods to obtain multiple-order-up granularity will depend on whether the hierarchy consists of all circular single-order-up granularities or a mix of circular and quasi-circular single-order-up granularities. Circular single-order-up granularities can be used recursively to obtain a multiple-order-up circular granularity using
\begin{equation} \label{eq:eq7}
C_{G_\ell,G_m}(z)
  = \sum_{i=0}^{m - \ell - 1} P(\ell, \ell+i)C_{G_{\ell+i},G_{\ell+i+1}}(z),
\end{equation}
where $\ell < m - 1$ and $P(i, i) = 1$ for $i=0,1,\dots,m-\ell-1$, and
$C_{B, G}(z)  = z\mod P(B, G)$ as per Equation \eqref{eq:circular-gran}.

For example, the multiple-order-up granularity $C_{\text{uinal},\text{katun}}$ for the Mayan calendar could be obtained using
\begin{align*}
C_{\text{uinal}, \text{baktun}}(z)  &=  C_{\text{uinal}, \text{tun}}(z) + P(\text{uinal}, \text{tun})C_{\text{tun},\text{katun}}(z) + P(\text{uinal},\text{katun})C_{\text{katun}, \text{baktun}}(z) \\
         &=  C_{\text{uinal}, \text{tun}}(z) + 18 \times{ C_{\text{tun},\text{katun}}(z)} + 18 \times 20 \times{C_{\text{katun}, \text{baktun}}(z)}
\end{align*}

where $z$ is the index of the bottom granularity $kin$.


<!--                &=  \lfloor \frac{z}{20}\rfloor \mod 18 + 18\lfloor  \frac{z}{20 \times18} \rfloor \mod 20 -->
<!--                 + 18 \times20 \lfloor \frac{z}{(20\times18\times20} \rfloor \mod 20 -->

Now consider the case where there is one quasi-circular single order-up granularity in the hierarchy table while computing a multiple-order-up quasi-circular granularity. Any multiple-order-up quasi-circular granularity $C_{\ell, m}(z)$ could then be obtained as a discrete combination of circular and quasi-circular granularities.

Depending on the order of the combination, two different approaches need to be employed leading to the following cases:

  * $C_{G_{\ell},G_{m'}}$ is circular and $C_{G_{m'},G_{m}}$ is quasi-circular
\begin{equation} \label{eq:multifromsingle-quasi1}
C_{G_\ell,G_{m}}(z) = C_{G_{\ell},G_{m'}}(z) + P(\ell, m')C_{G_{m'},G_{m}}(z)
\end{equation}

  * $C_{G_{\ell},G_{m'}}$ is quasi-circular and $C_{G_{m'},G_{m}}$ is circular
\begin{equation} \label{eq:multifromsingle-quasi2}
C_{G_\ell,G_{m}}(z)  = C_{G_{\ell},G_{m'}}(z) + \sum_{w=0}^{C_{G_{m'},G_{m}}(z) -1}(\vert T_{w} \vert)
\end{equation}
where $T_w$ is such that $G_{m'}(w) = \bigcup_{z \in T_w}G_{\ell}$ and $\vert T_w \vert$ is the cardinality of set $T_w$.

\begin{table}

\caption{(\#tab:tab-gregorian)Hierarchy table for the Gregorian calendar with both circular and quasi-circular single-order-up granularities.}
\centering
\begin{tabular}[t]{lll}
\toprule
linear (G) & single-order-up cyclic (C) & period length/conversion operator (K)\\
\midrule
minute & minute-of-hour & 60\\
hour & hour-of-day & 24\\
day & day-of-month & k(day, month)\\
month & month-of-year & 12\\
year & 1 & 1\\
\bottomrule
\end{tabular}
\end{table}

For example, the Gregorian calendar (\autoref{tab:tab-gregorian}) has day-of-month as a single-order-up quasi-circular granularity, with the other granularities being circular. Using Equations \eqref{eq:multifromsingle-quasi1} and \eqref{eq:multifromsingle-quasi2}, we then have:
$$
  C_{hour, month}(z) = C_{hour, day}(z) + P(hour, day)*C_{day, month}(z)
$$
$$
  C_{day, year}(z) = C_{day,month}(z) + \sum_{w=0}^{C_{month, year}(z)-1}(\vert T_{w} \vert),
$$
where $T_w$ is such that $month(w) = \bigcup_{z \in T_w}day(z)$.

#### Multiple-to-single order-up {- #m2s}

Similar to single-to-multiple operations, multiple-to-single operations involve different approaches for all circular single-order-up granularities and a mix of circular and quasi-circular single-order-up granularities in the hierarchy. For a hierarchy table $H_n: (G, C, K)$ with only circular single-order-up granularities and $\ell_1, \ell_2, m_1, m_2 \in {1, 2, \dots, n}$ and $\ell_2<\ell_1$ and $m_2>m_1$, multiple-order-up granularities can be obtained using Equation \eqref{eq:all-circular-multiple}.
\begin{equation} \label{eq:all-circular-multiple}
C_{G_{\ell_1}, G_{m_1}}(z) = \lfloor C_{G_{\ell_2}, G_{m_2}}(z)/P(\ell_2,\ell_1) \rfloor \mod P(\ell_1, m_1)
\end{equation}
For example, in the Mayan Calendar, it is possible to compute the single-order-up granularity tun-of-katun from uinal-of-baktun, since $C_{tun, katun}(z) = \lfloor C_{uinal, baktun}(z)/18\rfloor \mod 20$.

#### Multiple order-up quasi-circular granularities {- #quasi}

Single-order-up quasi-circular granularity can be obtained from multiple-order-up quasi-circular granularity and single/multiple-order-up circular granularity using Equations \eqref{eq:multifromsingle-quasi1} and \eqref{eq:multifromsingle-quasi2}.

## Data structure {#sec:data-structure}

Effective exploration and visualization benefit from well-organized data structures. @wang2020tsibble introduced the tidy "tsibble" data structure to support exploration and modeling of temporal data. This forms the basis of the structure for cyclic granularities. A tsibble comprises an index, optional key(s), and measured variables. An index is a variable with inherent ordering from past to present and a key is a set of variables that define observational units over time. A linear granularity is a mapping of the index set to subsets of the time domain. For example, if the index of a tsibble is days, then a linear granularity might be weeks, months or years. A bottom granularity is represented by the index of the tsibble.

\novspacing

\begin{table}

\caption{(\#tab:data-structure)The data structure for exploring periodicities in data by including cyclic granularities in the tsibble structure with index, key and measured variables.}
\centering
\begin{tabular}[t]{lllllll}
\toprule
\cellcolor[HTML]{fdf2d0}{\textbf{index}} & \cellcolor[HTML]{fdf2d0}{\textbf{key}} & \cellcolor[HTML]{fdf2d0}{\textbf{measurements}} & \cellcolor[HTML]{fdf2d0}{\textbf{$C_1$}} & \cellcolor[HTML]{fdf2d0}{\textbf{$C_2$}} & \cellcolor[HTML]{fdf2d0}{\textbf{$\cdots$}} & \cellcolor[HTML]{fdf2d0}{\textbf{$C_{N_C}$}}\\
\midrule
\cellcolor[HTML]{dbe3f1}{ } & \cellcolor[HTML]{dbe3f1}{ } & \cellcolor[HTML]{dbe3f1}{ } & \cellcolor[HTML]{fdf2d0}{ } & \cellcolor[HTML]{fdf2d0}{ } & \cellcolor[HTML]{fdf2d0}{ } & \cellcolor[HTML]{fdf2d0}{ }\\
\bottomrule
\end{tabular}
\end{table}

\vspacing

All cyclic granularities can be expressed in terms of the index set. \autoref{tab:data-structure} shows the tsibble structure (index, key, measurements) augmented by columns of cyclic granularities. The total number of cyclic granularities depends on the number of linear granularities considered in the hierarchy table and the presence of any aperiodic cyclic granularities. For example, if we have $n$ periodic linear granularities in the hierarchy table, then $n(n-1)/2$ circular or quasi-circular cyclic granularities can be constructed. Let $N_C$ be the total number of contextual circular, quasi-circular and aperiodic cyclic granularities that can originate from the underlying periodic and aperiodic linear granularities. Simultaneously encoding more than a few of these cyclic granularities when visualizing the data overwhelms human comprehension. Instead, we focus on visualizing the data split by pairs of cyclic granularities ($C_i$, $C_j$). Data sets of the form <$C_i$, $C_j$, $v$> then allow exploration and analysis of the measured variable $v$.

### Harmonies and clashes {#sec:synergy}

The way granularities are related is important when we consider data visualizations. Consider two cyclic granularities $C_i$ and $C_j$, such that $C_i$ maps index set to a set $\{A_k \mid k=1,\dots,K\}$ and $C_j$ maps index set to a set $\{B_\ell \mid \ell =1,\dots,L\}$. Here, $A_k$ and $B_\ell$ are the levels/categories corresponding to $C_i$ and $C_j$ respectively. Let $S_{k\ell}$ be a subset of the index set such that for all $s \in S_{k\ell}$, $C_i(s) = A_k$ and $C_j(s) = B_\ell$. There are $KL$ such data subsets, one for each combination of levels ($A_k$, $B_\ell$). Some of these sets may be empty due to the structure of the calendar, or because of the duration and location of events in a calendar.

\begin{defn}\label{def:clash}
A \textbf{clash} is a pair of cyclic granularities that contains empty combinations of categories.
\end{defn}

\begin{defn}\label{def:harmony}
A \textbf{harmony} is a pair of cyclic granularities that does not contain any empty combinations of its categories.
\end{defn}

Structurally empty combinations can arise due to the structure of the calendar or hierarchy. For example, let $C_i$ be day-of-month with 31 levels and $C_j$ be day-of-year with 365 levels. There will be $31\times 365=11315$ sets $S_{k\ell}$ corresponding to possible combinations of $C_i$ and $C_j$. Many of these are empty. For example, $S_{1,5}$ is empty because the first day of the month can never correspond to the fifth day of the year. Hence the pair (day-of-month, day-of-year) is a clash.

Event-driven empty combinations arise due to differences in event location or duration in a calendar. For example, let $C_i$ be day-of-week with 7 levels and $C_j$ be working-day/non-working-day with 2 levels. While potentially all of these 14 sets $S_{k\ell}$ can be non-empty (it is possible to have a public holiday on any day-of-week), in practice many of these will probably have very few observations. For example, there are few (if any) public holidays on Wednesdays or Thursdays in any given year in Melbourne, Australia.

An example of harmony is where $C_i$ and $C_j$ denote day-of-week and month-of-year respectively. So $C_i$ will have 7 levels while $C_j$ will have 12 levels, giving $12\times7=84$ sets $S_{k\ell}$. All of these are non-empty because every day-of-week can occur in every month. Hence, the pair (day-of-week, month-of-year) is a harmony.

### Near-clashes {#sec:near-clashes}

Suppose $C_i$ denotes day-of-year and $C_j$ denotes day-of-week. While any day of the week can occur on any day of the year, some combinations will be very rare. For example, the 366th day of the year will only coincide with a Wednesday approximately every 28 years on average. We refer to these as "near-clashes".

## Visualization {#sec:visualization}

The purpose is to visualize the distribution of the continuous variable ($v$) conditional on the values of two granularities, $C_i$ and $C_j$. Since $C_i$ and $C_j$ are factors or categorical variables, data subsets corresponding to each combination of their levels form a subgroup and the visualization amounts to having displays of distributions for different subgroups. The response variable $(v)$ is plotted on the y-axis and the levels of $C_i (C_j)$ on the x-axis, conditional on the levels of $C_j (C_i)$. This means, carrying out the same plot corresponding to each level of the conditioning variable. This is consistent with the widely used grammar of graphics
which is a framework to construct statistical graphics by relating the data space to the graphic space [@Wilkinson1999-nk; @Wickham2009pk].  


<!-- When a data is made up of different groups, it is often informative to compare the distribution of the variables across the groups. -->

### Data summarization

There are several ways to summarize the distribution of a data set such as estimating the empirical distribution or density of the data, or computing a few quantiles or other statistics. This estimation or summarization could be potentially misleading if it is performed on rarely occurring categories (Section&nbsp;\ref{sec:near-clashes}). Even when there are no rarely occurring events, the number of observations may vary greatly within or across each facet, due to missing observations or uneven locations of events in the time domain. In such cases, data summarization should be used with caution as sample sizes will directly affect the accuracy of the estimated quantities being displayed. 

<!-- Each plot option has parameters that need to be estimated or summarized from the data. -->
<!-- For example, for histogram, empirical distribution is estimated, for boxplot or its variation, number summaries are computed, for violin or any density-based plots, the density of the variable's distribution needs to be estimated. -->


### Display choices for univariate distributions

<!-- Plotting each point for each combination through dot plots or rug plots is not useful for our data structure since that leads to a blob of points for each subgroup. Further, they are anyway a poor choice for large datasets. Hence, -->

The basic plot choice for our data structure is one that can display distributions. For displaying the distribution of a continuous univariate variable, many options are available. Displays based on descriptive statistics include boxplots [@Tukey1977-jx] and its variants such as notched boxplots [@McGill1978-hg] or other variations as mentioned in @boxplots. They also include line or area quantile plots which can display any quantiles and not only quartiles like in a boxplot. Plots based on kernel density estimates include violin plots [@Hintze1998-zi], summary plots [@Potter2010-qc], ridge line plots [@R-ggridges], and highest density region (HDR) plots [@Hyndman1996-ft]. The less commonly used letter-value plots [@Hofmann2017-sg] is midway between boxplots and density plots. Letter values are order statistics with specific depths; for example, the median ($M$) is a letter value that divides the data set into halves. Each of the next letter values splits the remaining parts into two separate regions so that the fourths ($F$), eighths ($E$), sixteenths ($D$), etc. are obtained. They are useful for displaying the distributions beyond the quartiles especially for large data,
where boxplots mislabel data points as outliers.
One of the best approaches in exploratory data analysis is to draw a variety of plots to reveal information while keeping in mind the drawbacks and benefits of each of the plot choices. For example, boxplots obscure multimodality, and interpretation of density estimates and histograms may change depending on the bandwidth and binwidths respectively. In R package `gravitas` [@R-gravitas], boxplots, violin, ridge, letter-value, line and area quantile plots are implemented, but it is potentially possible to use any plots which can display the distribution of the data.


### Comparison across sub-groups induced by conditioning {#sec:aesthetics}

<!-- While it is often beneficial to compare distributions in this way, there are some caveats that we should be kept in mind while exploring these displays, subject to our data structure as follows: -->

#### Levels {- #levels}

The levels of cyclic granularities affect plotting choices since space and resolution may be problematic with too many levels. A potential approach could be to categorize the number of levels as low/medium/high/very high for each cyclic granularity and define some criteria based on human cognitive power, available display size and the aesthetic mappings. Default values for these categorizations could be chosen based on levels of common temporal granularities like days of the month, days of the fortnight, or days of the week.

#### Synergy of cyclic granularities {- #synergy-cyclic}

The synergy of the two cyclic granularities will affect plotting choices for exploratory analysis. Cyclic granularities that form clashes (Section \@ref(sec:synergy)) or near-clashes lead to potentially ineffective graphs. Harmonies tend to be more useful for exploring patterns. \autoref{fig:allFig}a shows the distribution of half-hourly electricity consumption through letter value plots across months of the year conditional on quarters of the year. This plot does not work because quarter-of-year clashes with month-of-year, leading to empty subsets. For example, the first quarter never corresponds to December.

\begin{figure}[!p]

\includegraphics[width=\textwidth]{figure/allFig-1} \hfill{}

\caption{(ref:allFig)}(\#fig:allFig)
\end{figure}

(ref:allFig) Distribution of energy consumption displayed on a logarithmic scale as letter value plots, illustrating harmonies and clashes, and how mappings change emphasis: **a** weekday/weekend faceted by quarter-of-year produces a harmony, **b** quarter-of-year faceted by weekday/weekend produces a harmony, **c** month-of-year faceted by quarter-of-year produces a clash, as indicated by the empty sets and white space. Placement within a facet should be done for primary comparisons. For example, arrangement in **a** makes it easier to compare across weekday type (x-axis) within a quarter (facet). It can be seen that in quarter 2, more mass occupied the lower tail on the weekends (letter value E corresponding to tail area 1/8) relative to that of the weekdays (letter value D 1/16), which corresponds to more days with lower energy use in this period. 

<!-- ; **b** across weekday/weekend faceted by quarter-of-year; **c** across quarter-of-year faceted by weekday/weekend. **a** shows a clash since there are empty combinations. **b** and **c** show harmonies since each quarter include both weekdays and weekends. It can be seen in **b** that for every quarter (mapped to facet), weekend and weekday consumption are fairly similar except for the second quarter. -->
<!-- For example, the letter value D for weekdays mostly corresponds to letter value E for weekends, implying there are lower values of energy consumption during weekdays in the second quarter. This is probably because of lower temperatures in the second quarter compared to the first quarter (summer in Australia). **c** switches the granularities mapped to the x-axis and the facets and helps to compare quarters within weekdays and weekends. For example, the lower D and E letter values are most different across different quarters for weekdays and weekends respectively. -->
<!-- For example, for weekdays the interquartile range of consumption reduces over the year, whereas this pattern is not true for weekends. -->

#### Conditioning variable {- #conditioning}

When $C_i$ is mapped to the $x$ position and $C_j$ to facets, then the $A_k$ levels are juxtaposed and each $B_\ell$ represents a group/facet. Gestalt theory suggests that when items are placed in close proximity, people assume that they are in the same group because they are close to one another and apart from other groups. Hence, in this case, the $A_k$'s are compared against each other within each group. With the mapping of $C_i$ and $C_j$ reversed, the emphasis will shift to comparing $B_\ell$ levels rather than $A_k$ levels. For example, \autoref{fig:allFig}b shows the letter value plot across weekday/weekend partitioned by quarters of the year and \autoref{fig:allFig}c shows the same two cyclic granularities with their mapping reversed. \autoref{fig:allFig}b helps us to compare weekday and weekend within each quarter and \autoref{fig:allFig}c helps to compare quarters within weekend and weekday.


## Applications {#sec:application}

### Smart meter data of Australia {#sec:smartmeter}

Smart meters provide large quantities of measurements on energy usage for households across Australia. One of the customer trials [@smart-meter] conducted as part of the Smart Grid Smart City project in Newcastle and parts of Sydney provides customer level data on energy consumption for every half hour from February 2012 to March 2014. We can use this data set to visualize the distribution of energy consumption across different cyclic granularities in a systematic way to identify different behavioral patterns.

#### Cyclic granularities search and computation {- #search}

The tsibble object `smart_meter10` from R package `gravitas` [@R-gravitas] includes the variables `reading_datetime`, `customer_id` and `general_supply_kwh` denoting the index, key and measured variable respectively. The interval of this tsibble is 30 minutes.

To identify the available cyclic time granularities, consider the conventional time deconstructions for a Gregorian calendar that can be formed from the 30-minute time index: half-hour, hour, day, week, month, quarter, half-year, year. In this example, we will consider the granularities hour, day, week and month giving six cyclic granularities "hour_day", "hour_week", "hour_month", "day_week", "day_month" and "week_month", read as "hour of the day", etc. To these, we add day-type ("wknd_wday") to capture weekend and weekday behavior. Now that we have a list of cyclic granularities to look at, we can compute them using the results in Section&nbsp;\ref{sec:cyclic-calendar}.

#### Screening and visualizing harmonies {- #visualize}

Using these seven cyclic granularities, we want to explore patterns of energy behavior. Each of these seven cyclic granularities can either be mapped to the x-axis or to facets. Choosing $2$ of the possible $7$ granularities, gives $^{7}P_2 = 42$ candidates for visualization. Harmonies can be identified among those $42$ possibilities to narrow the search. \autoref{tab:harmony-tab} shows $16$ harmony pairs after removing clashes and any cyclic granularities with more than $31$ levels, as effective exploration becomes difficult with many levels (Section&nbsp;\ref{sec:aesthetics}).


\begin{table}[!h]

\caption{(\#tab:harmony-tab)Harmonies with pairs of cyclic granularities, one mapped to facets and the other to the x-axis. Only 16 of 42 possible combinations of cyclic granularities are harmony pairs.}
\centering
\begin{tabular}[t]{llrr}
\toprule
facet variable & x-axis variable & facet levels & x-axis levels\\
\midrule
day\_week & hour\_day & 7 & 24\\
day\_month & hour\_day & 31 & 24\\
week\_month & hour\_day & 5 & 24\\
wknd\_wday & hour\_day & 2 & 24\\
hour\_day & day\_week & 24 & 7\\
\addlinespace
day\_month & day\_week & 31 & 7\\
week\_month & day\_week & 5 & 7\\
hour\_day & day\_month & 24 & 31\\
day\_week & day\_month & 7 & 31\\
wknd\_wday & day\_month & 2 & 31\\
\addlinespace
hour\_day & week\_month & 24 & 5\\
day\_week & week\_month & 7 & 5\\
wknd\_wday & week\_month & 2 & 5\\
hour\_day & wknd\_wday & 24 & 2\\
day\_month & wknd\_wday & 31 & 2\\
\addlinespace
week\_month & wknd\_wday & 5 & 2\\
\bottomrule
\end{tabular}
\end{table}

\begin{figure}[!p]

{\centering \includegraphics[width=\textwidth]{figure/bothcust-1} 

}

\caption{(ref:bothcust)}(\#fig:bothcust)
\end{figure}

(ref:bothcust) Energy consumption of a single customer shown with different distribution displays, and granularity arrangements: hour of the day; and weekday/weekend. **a** The side-by-side boxplots make the comparison between day types easier, and suggest that there is generally lower energy use on the weekend. Interestingly, this is the opposite to what might be expected. Plots **b**, **c** examine the temporal trend of consumption over the course of a day, separately for the type of day. The area quantile emphasizes time, and indicates that median consumption shows prolonged high usage in the morning on weekdays. The violin plot emphasizes subtler distributional differences across hours: morning use is bimodal.

A few harmony pairs are displayed in \autoref{fig:bothcust} to illustrate the impact of different distribution plots and reverse mapping. For each of \autoref{fig:bothcust}b and c, $C_i$ denotes day-type (weekday/weekend) and $C_j$ is hour-of-day. The geometry used for displaying the distribution is chosen as area-quantiles and violins in \autoref{fig:bothcust}b and c respectively. \autoref{fig:bothcust}a shows the reverse mapping of $C_i$ and $C_j$ with $C_i$ denoting hour-of-day and $C_j$ denoting day-type with distribution geometrically displayed as boxplots.

In \autoref{fig:bothcust}b, the black line is the median, the purple (narrow) band covers the 25th to 75th percentile, the orange (middle) band covers the 10th to 90th percentile, and the green (broad) band covers the 1st to 99th percentile. The first facet represents the weekday behavior while the second facet displays the weekend behavior; energy consumption across each hour of the day is shown inside each facet. The energy consumption is extremely skewed with the 1st, 10th and 25th percentile lying relatively close whereas 75th, 90th and 99th lying further away from each other. This is common across both weekdays and weekends. For the first few hours on weekdays, median energy consumption starts and continues to be higher for longer compared to weekends.

The same data is shown using violin plots instead of quantile plots in \autoref{fig:bothcust}c. There is bimodality in the early hours of the day for weekdays and weekends. If we visualize the same data with reverse mapping of the cyclic granularities (\autoref{fig:bothcust}a), then the natural tendency would be to compare weekend and weekday behavior within each hour and not across hours. Then it can be seen that median energy consumption for the early morning hours is higher for weekdays than weekends. Also, outliers are more prominent in the later hours of the day. All of these indicate that looking at different distribution geometry or changing the mapping can shed light on different aspects of energy behavior for the same sample.

### T20 cricket data of Indian Premier League {#sec:cricket}

Our proposed approach can be generalized to other hierarchical granularities where there is an underlying ordered index. We illustrate this with data from the sport cricket. Cricket is played with two teams of 11 players each, with each team taking turns batting and fielding. This is similar to baseball, wherein the _batsman_ and _bowler_ in cricket are analogous to a batter and pitcher in baseball. A _wicket_ is a structure with three sticks, stuck into the ground at the end of the cricket pitch behind the batsman. One player from the fielding team acts as the bowler, while another takes up the role of the _wicket-keeper_ (similar to a catcher in baseball). The bowler tries to hit the wicket with a _ball_, and the batsman defends the wicket using a _bat_. At any one time, two of the batting team and all of the fielding team are on the field. The batting team aims to score as many _runs_ as possible, while the fielding team aims to successively _dismiss_ 10 players from the batting team. The team with the highest number of runs wins the match.

Cricket is played in various formats and Twenty20 cricket (T20) is a shortened format, where the two teams have a single _innings_ each, which is restricted to a maximum of 20 _overs_. An over will consist of 6 balls (with some exceptions). A single _match_ will consist of 2 innings and a _season_ consists of several matches. Although there is no conventional time component in cricket, each ball can be thought to represent an ordering over the course of the game. Then, we can conceive a hierarchy where the ball is nested within overs, overs nested within innings, innings within matches, and matches within seasons. Cyclic granularities can be constructed using this hierarchy. Example granularities include ball of the over, over of the innings, and ball of the innings. The hierarchy table is given in \autoref{tab:hierarchy-cric}. Although most of these cyclic granularities are circular by the design of the hierarchy, in practice some granularities are aperiodic. For example, most overs will consist of 6 balls, but there are exceptions due to wide balls, no-balls, or when an innings finishes before the over finishes. Thus, the cyclic granularity ball-of-over may be aperiodic.

\begin{table}

\caption{(\#tab:hierarchy-cric)Hierarchy table for cricket where overs are nested within an innings, innings nested within a match and matches within a season.}
\centering
\begin{tabular}[t]{lll}
\toprule
linear (G) & single-order-up cyclic (C) & period length/conversion operator (K)\\
\midrule
over & over-of-inning & 20\\
inning & inning-of-match & 2\\
match & match-of-season & k(match, season)\\
season & 1 & 1\\
\bottomrule
\end{tabular}
\end{table}

The Indian Premier League (IPL) is a professional T20 cricket league in India contested by eight teams representing eight different cities in India. The IPL ball-by-ball data is provided in the `cricket` data set in the `gravitas` package for a sample of 214 matches spanning 9 seasons (2008 to 2016).

Many interesting questions could be addressed with the `cricket` data set. For example, does the distribution of total runs depend on whether a team bats in the first or second innings? The Mumbai Indians (MI) and Chennai Super Kings (CSK) appeared in the final playoffs from 2010 to 2015. Using data from these two teams, it can be observed (\autoref{fig:cricex}a) that for the team batting in the first innings there is an upward trend of runs per over, while there is no clear upward trend in the median and quartile deviation of runs for the team batting in the second innings after the first few overs. This suggests that players feel mounting pressure to score more runs as they approach the end of the first innings, while teams batting second have a set target in mind and are not subjected to such mounting pressure and therefore may adopt a more conservative run-scoring strategy.

Another question that can be addressed is if good fielding or bowling (defending) in the previous over affects the scoring rate in the subsequent over. To measure the defending quality, we use an indicator function on dismissals (1 if there was at least one wicket in the previous over, 0 otherwise). The scoring rate is measured by runs per over. \autoref{fig:cricex}b shows that no dismissals in the previous over leads to a higher median and quartile spread of runs per over compared to the case when there has been at least one dismissal in the previous over. This seems to be unaffected by the over of the innings (the faceting variable). This might be because the new batsman needs to "play himself in" or the dismissals lead the (not-dismissed) batsman to adopt a more defensive play style. Run rates will also vary depending on which player is facing the next over and when the wicket falls in the previous over. 

Here, wickets per over is an aperiodic cyclic granularity, so it does not appear in the hierarchy table. These are similar to holidays or special events in temporal data.

\begin{figure}

{\centering \includegraphics[width=\textwidth]{figure/cricex-1} \includegraphics[width=\textwidth]{figure/cricex-2} 

}

\caption{(ref:cricex)}(\#fig:cricex)
\end{figure}

(ref:cricex) Examining distribution of runs per innings, overs of the innings and number of wickets in previous innings. Plot **a** displays distribution using letter value plots. A gradual upward trend in runs per over can be seen in innings 1, which is not present in innings 2. Plot **b** shows quantile plots of runs per over across an indicator of wickets in the previous over, faceted by current over. When a wicket occurred in the previous over, the runs per over tends to be lower throughout the innings. 

## Discussion {#sec:discussion}

Exploratory data analysis involves many iterations of finding and summarizing patterns. With temporal data available at ever finer scales, exploring periodicity can become overwhelming with so many possible granularities to explore. This work provides tools to classify and compute possible cyclic granularities from an ordered (usually temporal) index. We also provide a framework to systematically explore the distribution of a univariate variable conditional on two cyclic time granularities using visualizations based on the synergy and levels of the cyclic granularities.

The `gravitas` package provides very general tools to compute and manipulate cyclic granularities, and to generate plots displaying distributions conditional on those granularities.

A missing piece in the package `gravitas` is the computation of cyclic aperiodic granularities which would require computing aperiodic linear granularities first. A few R packages including `almanac` [@R-almanac] and `gs` [@R-gs] provide the tools to create recurring aperiodic events. These functions can be used with the `gravitas` package to accommodate aperiodic cyclic granularities.

We propose producing plots based on pairs of cyclic granularities that form harmonies rather than clashes or near-clashes. A future direction of work could be to further refine the selection of appropriate pairs of granularities by identifying those for which the differences between the displayed distributions is greatest and rating these selected harmony pairs in order of importance for exploration.

## Acknowledgments {- #thanksgravitas}

The Australian authors thank the ARC Centre of Excellence for Mathematical and Statistical Frontiers [(ACEMS)](https://acems.org.au/home) for supporting this research. Thanks to [Data61 CSIRO](https://data61.csiro.au/) for partially funding Sayani's research and Dr. Peter Toscas for providing useful inputs on improving the analysis of the smart meter application. We would also like to thank Nicholas Spyrison for many useful discussions, sketching figures and feedback on the manuscript. The package `gravitas` was built during the [Google Summer of Code, 2019](https://summerofcode.withgoogle.com/archive/). More details about the package can be found at [sayani07.github.io/gravitas](https://sayani07.github.io/gravitas/). The Github repository,  [github.com/Sayani07/paper-gravitas](https://github.com/Sayani07/paper-gravitas), contains all materials required to reproduce this article and the code is also available online in the supplemental materials. This article was created with `knitr` [@knitr; @R-knitr] and `rmarkdown` [@rmarkdown; @R-rmarkdown]. 


## Supplementary materials {- #supplement-gravitas}

**Data and scripts:** Data sets and R code to reproduce all figures in this article (main.R).

\noindent
**R-package:** The ideas presented in this article have been implemented in the open-source R [@R-language] package `gravitas` [@R-gravitas], available from CRAN. The R-package facilitates manipulation of single and multiple-order-up time granularities through cyclic calendar algebra, checks feasibility of creating plots or drawing inferences for any two cyclic granularities by providing list of harmonies, and recommends possible visual summaries through factors described in the article. Version 0.1.3 of the package was used for the results presented in the article and is available on Github (https://github.com/Sayani07/gravitas).


<!--chapter:end:Rmd//02-gravitas.Rmd-->

# Detecting distributional differences between temporal granularities for exploratory time series analysis  {#ch-hakear}

 Cyclic temporal granularities, which are temporal deconstructions of a time period into units such as hour-of-the-day, work-day/weekend, can be useful for measuring repetitive patterns in large univariate time series data. The granularities feed new approaches to exploring time series data. One use is to take pairs of granularities, and make plots of response values across the categories induced by the temporal deconstruction. However, when there are many granularities that can be constructed for a time period, there will also be too many possible displays to decide which might be the more interesting to display. This work proposes a new distance metric to screen and rank the possible granularities, and hence choose the most interesting ones to plot. The distance measure is computed for a single or pairs of cyclic granularities that can be compared across different cyclic granularities and also on a collection of time series. The methods are implemented in the open-source R package [`hakear`](https://github.com/Sayani07/hakear).
 
 







## Introduction

<!-- <introducing the problem> -->
<!-- background of the problem -->

<!--Exploratory data analysis, as coined by John W. Tukey [@Tukey1977-jx] involves many iterations of finding structures and patterns that allow the data to be informative.-->

Cyclic temporal granularities [@Bettini1998-ed;@Gupta2021-gravitas] are <!--theoretical--> temporal deconstructions that define cyclic repetitions in time, e.g. hour-of-day, day-of-month, or regularly scheduled public holidays. These granularities form ordered or unordered categorical variables. An example of an ordered granularity is day-of-week, where Tuesday is always followed by Wednesday, and so on. An unordered granularity example is \DC{week type in an academic semester: orientation, break, exam or regular classes}. We can use granularities to explore patterns in univariate time series by examining the distribution of the measured variable across different categories of the cyclic granularities.



\begin{figure}[!b]

{\centering \includegraphics[width=\textwidth]{figure/onegran-new-1} 

}

\caption{A cyclic granularity can be considered to be a categorical variable, and used to break the data into subsets. Here, side-by-side boxplots overlaid on jittered dotplots explore the distribution of of energy use by a household for two different cyclic granularities: (a) hour-of-day and (b) and month-of-year. Daily peaks occur in morning and evening hours, indicating a working household, where members leave for and return from work. More volatility of usage in summer months (Jan, Feb) is probably due to air conditioner use on just some days.}(\#fig:onegran-new)
\end{figure}

As a motivating example, consider Figure \ref{fig:onegran-new} which shows electricity smart meter data plotted against two granularities (hour-of-day, month-of-year). The data was collected on a single household in Melbourne, Australia, over a six month period, and was previously used in @wang2020calendar. The categorical variable (granularity) is mapped to the x-axis, and the distribution of the response variable is displayed using both side-by-side jittered dotplots and boxplots. From panel (a) it can be seen that energy consumption is higher during the morning hours (5--8), when members in the household wake up, and again in the evening hours (17--20), possibly when members get back from work. In addition, the largest variation in energy use is in the afternoon hours (12--16), as seen in the length of the boxes. From panel (b), it is seen that the variability in energy usage is higher in Jan and Feb, probably due to the usage of air conditioners on some days. The median usage is highest in January, dips in February--April and rises again in May--June, although not to the height of January usage. This suggests that the household does not use as much electricity for heating as it does for air conditioning. A lot of households in Victoria use gas heating and hence the heater use might not be reflected in the electricity data.

Many different displays could be constructed using different granularities including day-of-week, day-of-month, weekday/weekend, etc. However, only a few might be interesting and reveal important patterns in energy usage. Determining which displays have "significant" distributional differences between categories of the cyclic granularity, and plotting only these, would make for efficient exploration.



\begin{figure}

{\centering \includegraphics[width=\textwidth]{figure/id2-new2-1} 

}

\caption{Distribution of energy consumption displayed through area quantile plots across two cyclic granularities month-of-year and hour-of-day and two households. The black line is the median, whereas the orange band covers the 25th to 75th percentile and the green band covers the 10th to 90th percentile. Difference between the 90th and 75th quantiles is less for (Jan, Feb) for the first household (a), suggesting that it is a more frequent user of air conditioners than the second household (b). Distribution of energy usage for (a) changes across both granularities, whereas for (b) daily pattern stays same irrespective of the months.}(\#fig:id2-new2)
\end{figure}

<!-- comparing one and two grans for one households -->
Exploring the distribution of the measured variable across two cyclic granularities provides more detailed information on its structure. For example, Figure \ref{fig:id2-new2}(a) shows the usage distribution across hour-of-day conditional on month-of-year across two households. It shows the hourly usage over a day does not remain the same across months. Unlike other months, the 75th and 90th percentile for all hours of the day in January are high, similar, and are not characterized by a morning and evening peak. The household in Figure \ref{fig:id2-new2}(b) has 90th percentile consumption higher in summer months relative to autumn or winter, but the 75th and 90th percentile are far apart in all months, implying that the second household resorts to air conditioning much less regularly than the first one. The differences seem to be more prominent across month-of-year (facets) than hour-of-day (x-axis) for this household, whereas they are prominent for both cyclic granularities for the first household.

<!-- The problem and its dimension -->
Are all four displays in Figures \ref{fig:onegran-new} and \ref{fig:id2-new2} useful in understanding the distributional difference in energy usage? Which ones are more useful than others? If $N_C$ is the total number of cyclic granularities of interest, the number of displays that could be potentially informative is $N_C$ when considering displays of the form in Figure \ref{fig:onegran-new}. The dimension of the problem, however, increases when considering more than one cyclic granularity. When considering displays of the form in Figure \ref{fig:id2-new2},
there are $N_C(N_C-1)$ possible pairwise plots exhaustively, with one of the two cyclic granularities acting as the conditioning variable.
This can be overwhelming for human consumption even for moderately large $N_C$. It is therefore  useful to identify those displays that are informative across at least one cyclic granularity.

<!-- Moreover, the volatility in consumption are almost equal and high for all hours in Jan, whereas variability is most in the evening hours for other months. The distribution of consumption across different hours of the day look similar for Apr, May and Jun, characterized by a distinct morning, afternoon and evening peaks and higher variability in evening hours -->

<!-- months Jan and Feb not only have a high median consumption, but also more variability, possibly due to the usage of air conditioning. The other months shows more consistent behavior with the energy consumption rising in May and June. -->

<!-- Scagnostics literature, other literature and why we need more/different --> 

This problem is similar to Scagnostics (Scatterplot Diagnostics) by @tukey1988computer, which are used to identify meaningful patterns in large collections of scatterplots. Given a set of $v$ variables, there are $p(p-1)/2$ pairs of variables, and thus the same number of possible pairwise scatterplots. Therefore, even for small $v$, the number of scatterplots can be large, and scatterplot matrices (SPLOMs) can easily run out of pixels when presenting high-dimensional data. @Dang2014-tw; @wilkinson2005graph provided potential solutions to this, where a few characterizations can be used to locate anomalies in density, shape, trend, and other features in the 2D point scatters.

In this paper, we provide a solution to narrowing down the search from $N_C(N_C-1)$ conditional distribution plots by introducing a new distance measure that can be used to detect significant distributional differences across cyclic granularities.<!-- harmonies and why it is not enough --> This work is a natural extension of our previous work [@Gupta2021-gravitas] which narrows down the search from $N_C(N_C-1)$ plots by identifying pairs of granularities that can be meaningfully examined together (a "harmony"), or when they cannot (a "clash"). However, even after excluding clashes, the list of harmonies left may be too large for exhaustive exploration. Hence, there is a need to reduce the search even further by including only those harmonies that contain useful information.

@inference; @Majumder2013-hb presented methods for statistical significance testing of visual findings using human cognition as the statistical tests. In this paper, the visual discovery of distributional differences is facilitated by choosing a threshold for the proposed numerical distance measure, eventually selecting only those cyclic granularities for which the distributional differences are sufficient to make it an interesting display.

<!-- \ref{fig:id2-new2} shows the distribution of energy consumption across hour-of-day conditional on month-of-year. \ref{fig:id2-new2}(a) shows that energy consumption is higher during the morning hours when members in the household wake up and then again higher in the evening hours possibly when members are back from work with them showing maximum variable behavior in the afternoon hours. \ref{fig:id2-new2}(b) shows the months Jan and Feb not only have a high median consumption, but also more variability, possibly due to the usage of air conditioning. The other months shows more consistent behavior with the energy consumption rising in May and June. \ref{fig:id2-new2} adds to this information, by showing that hours in Jan do not necessarily follow the same pattern across the day (due to Holidays, people visiting). Moreover, the volatility in consumption are almost equal and high for all hours in Jan, whereas variability is most in the evening hours for other months. The distribution of consumption across different hours of the day look similar for Apr, May and Jun, characterized by a distinct morning, afternoon and evening peaks and higher variability in evening hours. -->

<!-- # interaction for two households, importance could be across one --> 

<!-- Even when it is known which all interactions to look at, all of them would not be interesting and that too will vary across different households. -->

<!-- calendar plots -->
<!-- The authors show how hour-of-the-day interact with weekdays and weekends and then move on to use calendar display to show daily schedules. The calendar display has several components in it, which helps us to look at energy consumption across hour-of-day, day-of-week, week-of-month, and month-of-year at once. Some interaction of these cyclic granularities, for example, how day-of-week relates to month-of-year, could also be interpreted from this display. This is a great way for having an overview of energy consumption. However, if one wants to understand the repetitiveness in energy behavior and how they interact in greater detail, it is not easy to comprehend the interactions of all cyclic granularities from this display, due to the combination of linear and cyclic representation of time. For example, this display might not be the best to understand how hour-of-the-day or month-of-year varies across week-of-the-month as well as with each other. Furthermore, it is not clear what all interactions of cyclic granularities should be read from this display as there could be many combinations that one can look at. Moreover, there could be non-conventional cyclic granularities, which could potentially become useful depending on the context. -->

<!-- context when it could be useful is monitoring heart rates. There are devices that can detect each heartbeat and transfer the data to a receiver such as a watch or phone. These data could be available for a temporal scale as fine as a minute and it could be of interest to see regular patterns across any deconstruction of time coarser than a minute. -->



<!-- Again, look at \ref{fig:} c and d, where energy consumption for these two households are plotted against (weekend/weekday, week-of-month). Here, for both households, the pattern of energy consumption vary across different weeks of the month irrespective of the fact it is a weekday or weekend. In that respect, the harmony pair (month-of-year, hour-of-day) seems to be more informative than (weekend/weekday, week-of-month) for the first household. -->

<!-- Take an example of a data set which are observed at fine temporal scales, like that of NYC bike usage available at https://www.citibikenyc.com/system-data. We use the `nyc_bikes` data set from the R package `tsibbledata` which takes a sample of 10 bikes for the year 2018. The `start_time` and the `stop_time` are recorded to a fineness of seconds. We can look at pair of cyclic granularities (hour_day, wknd_wday) or (week_month, day_week) to see how these periodicities interact. But there could be other pairs that are important too. How to understand which pairs are sufficient to explore given the data set without losing much information about the data. -->

<!-- When we need to understand the interplay of different periodicities in a high frequency temporal datasets, we have many choices to consider. In [@wang2020tsibble] and [@wang2020calendar], periodicities are explored across hour of the day and day of the week or months. But calendar effects are not restricted to conventional day-of-week or month-of-year deconstructions. -->

<!-- If we have $n$ periodic linear granularities in the hierarchy table, then $n(n-1)/2$ circular or quasi-circular cyclic granularities could be constructed. -->

<!-- Our contributions in this paper are: -->

 <!-- * We introduce a new distance measure for detecting periodic interactions. This induces data reduction which allows for identification of patterns, if any, in the time series data. -->

 <!-- * We show that the distance metric could be used to rank the periodic patterns based on how well they capture the variation in the measured variable as they have been normalized for different number of comparisons. -->

 <!-- * We device a framework for choosing a threshold, which will result in detection of only significantly interesting periodic patterns in the time series data. -->

 <!-- * -->

 <!-- * ; -->

 <!-- * show that the proposed distance metric could be used to rank the interesting patterns across different datasets and temporal granularities since they have been normalized for relevant parameters. -->

The article is organized as follows. Section \ref{sec:computation-wpd} introduces a distance measure for detecting distributional difference in temporal granularities for a continuous univariate dependent variable. This enables identification of patterns in the time series data;
Section \ref{sec:rank-wpd} devises a selection criterion by choosing a threshold, which results in detection of only significantly interesting patterns. Section \ref{sec:simulations} provides a simulation study on the proposed methodology. Section \ref{sec:application-wpd} presents an application to residential smart meter data in Melbourne to show how the proposed methodology can be used to automatically detect temporal granularities along which distributional differences are significant.

<!-- acts as a way to automatically detect periodic patterns in time series. -->

<!-- Also, ranking the remaining harmony pairs based on how well they capture the variation in the measured variable could be potentially useful.  -->

<!-- Talk about Scagnostics: Tukey -->

## Proposed distance measure {#sec:computation-wpd}

We propose a measure called Weighted Pairwise Distances ($\wpd$) to detect distributional differences in the measured variable across cyclic granularities.

### Principle

(ref:null4by2) An example illustrating the principle of the proposed distance measure, displaying the distribution of a normally distributed variable in four panels each with two x-axis categories and three facet levels, but with different designs. Panel (a) is not interesting as the distribution of the variable does not depend on x or facet categories. Panels (b) and (c) are more interesting than (a) since there is a change in distribution either across facets (b) or x-axis (c). Panel (d) is most interesting in terms of capturing structure in the variable as the distribution of the variable changes across both facet and x-axis variable. The value of our proposed distance measure is presented for each panel, the relative differences between which will be explained later in Section \@ref(sec:ranking).

\begin{figure}[!b]

{\centering \includegraphics[width=\textwidth]{figure/null4by2-1} 

}

\caption{(ref:null4by2)}(\#fig:null4by2)
\end{figure}

The principle behind the construction of $\wpd$ is explained through a simple example in Figure&nbsp;\ref{fig:null4by2}. Each of these figures describes a panel with two x-axis categories and three facet levels, but with different designs. Figure \ref{fig:null4by2}a has all categories drawn from a standard normal distribution for each facet. It is not a particularly interesting display, as the distributions do not vary across x-axis or facet categories. Figure \ref{fig:null4by2}b has x categories drawn from the same distribution, but across facets the distribution means are three standard deviations apart. Figure \ref{fig:null4by2}c exhibits the opposite situation where distribution between the x-axis categories are three standard deviations apart, but they do not change across facets. In Figure \ref{fig:null4by2}d, the distribution varies across both facet and x-axis categories by three standard deviations.

If the panels are to be ranked in order of capturing maximum variation in the measured variable from minimum to maximum, then an obvious choice would be (a) followed by (b), (c) and then (d). It might be argued that it is not clear if (b) should precede or succeed (c) in the ranking. Gestalt theory suggests items placed at close proximity can be compared more easily, because people assume that they are in the same group and apart from other groups. With this principle in mind, Panel (b) is considered less informative compared to Panel (c) in emphasizing the distributional differences.

For displays showing a single cyclic granularity rather than pairs of granularities, we have only two design choices corresponding to no difference and significant differences between categories of that cyclic granularity.

The proposed measure $\wpd$ is constructed in such a way that it can be used to rank panels of different designs as well as test if a design is interesting. This measure is aimed to be an estimate of the maximum variation in the measured variable explained by the panel. A higher value of $\wpd$ would indicate that the panel is interesting to look at, whereas a lower value would indicate otherwise.

<!-- To compute the maximum variation of the measured variable within each panel, the distributional differences need to be measured for both within-group and between-group categories. Moreover, a tuning parameter specifying the weightage given to within-facet or between-facet categories can help to choose between designs like \ref{fig:null4by2}b and c. The maximum of all these weighted differences serves as an estimate of the maximum variation in the panel and is taken as the value of $\wpd$. -->

<!-- Larger differences imply stronger patterns, whereas small difference would imply that the underlying structure is not changing within or between group. -->

<!-- A distance measure aimed to capture the structure of the measured variable in designs like \ref{fig:null4by2}, should ideally estimate these within-group and between-group variations. -->

<!-- \noindent Intuitively, while finding a structure or measuring the strength of patterns in Figure \ref{fig:null4by2}, it makes sense to look for within-group and between-group variations. Larger variation would imply stronger patterns, whereas small variation would imply the underlying structure is not changing within or between group. -->
<!-- Thus, a distance measure aimed to capture this structure should ideally estimate these within-group and between-group variations. One of the potential ways to do this is to measure the distances between distributions of the continuous random variable measured within and between groups, weigh them basis if they are within or between groups and then take the maximum of those distances as an estimate of the maximum variation in the structure. This section starts with possible ways of characterizing distributions and computing distances between them and then describe in details how the measure $\wpd$ is defined. -->

<!-- This is similar to @ieee-irish where the authors compute the Jensen Shannon distance between two density estimates by computing percentiles and stresses the advantages to working with percentiles rather than the raw data directly in case of missing observations. Working with quantiles also ensure that unsynchronized time series could be handled. -->

<!-- Hence, with reference to the graphical design in @Gupta2021-gravitas, therefore the idea would be to rate a harmony pair higher if the variation between different levels of the x-axis variable is higher on an average across all levels of the facet variables. -->

<!-- To elaborate further, look at the examples in Figure \ref{}, where Figure \ref{}a represents the panel design with distribution of each x categories drawn from N(5, 10) distribution. It could be observed that the graph is not particularly interesting, as there is no significant change in distribution between x-axis levels or facets. Figure \ref{}b represents the same panel design with no difference in distribution of x-axis categories within a facet, but different distribution of x-axis categories for different facets. For example, if there are 4 facet levels and 2 x-axis levels, data is generated in the way as described in Table \ref{}. Figure \ref{}b exhibits an opposite situation where the x-axis within facets are different but not across facets. -->
<!-- Figure \ref{}d takes it further by varying the distribution across both facet and x-axis categories. -->


<!-- A "significant" cyclic granularity have at least two categories which are significantly different from each other. It does not tell you which categories are statistically significantly different from each other -->

<!-- in the same group would be important to bring out different patterns of the data. -->

### Notation

Let the number of cyclic granularities considered in the display be $m$. The notations and methodology are described in detail for $m=2$. But it can be easily extended to $m>2$. Consider two cyclic granularities $A$ and $B$, such that $A = \{a_j: j = 1, 2, \dots, \nx\}$ and $B = \{b_k: k = 1, 2, \dots, \nf\}$ with $A$ placed across the x-axis and $B$ across facets. Let $v = \{v_t: t = 0, 1, 2, \dots, T-1\}$ be a continuous variable observed across $T$ time points. This data structure with $\nx$ x-axis levels and $\nf$ facet levels is referred to as a $(\nx, \nf)$ panel. For example, a $(2, 3)$ panel will have cyclic granularities with two x-axis levels and three facet levels. Let the four elementary designs as described in Figure \ref{fig:null4by2} be $\Dnull$ (referred to as "null distribution") where there is no difference in distribution of $v$ for $A$ or $B$, $\Df$ denotes the set of designs where there is difference in distribution of $v$ for $B$ and not for $A$. Similarly, $\Dx$ denotes the set of designs where difference is observed only across A. Finally, $\Dfx$ denotes those designs for which difference is observed across both $A$ and $B$. We can consider a single granularity ($m = 1$) as a special case of two granularities with $\nf = 1$.

\begin{table}

\caption{(\#tab:notations)Nomenclature table}
\centering
\begin{tabular}[t]{ll}
\toprule
variable & description\\
\midrule
$N_C$ & number of cyclic granularities\\
$H_{N_C}$ & set of harmonies\\
m & number of cyclic granularities to display together\\
$\nx$ & number of x-axis categories\\
$\nf$ & number of facet categories\\
\addlinespace
$\lambda$ & tuning parameter\\
$\omega$ & increment (mean or sd)\\
$\wpd$ & raw weighted pairwise distance\\
$\wpdsub{norm}$ & normalized weighted pairwise distance\\
$\nsub{perm}$ & number of permutations for threshold/normalization\\
\addlinespace
$\nsub{sim}$ & number of simulations\\
$\wpdsub{threshold}$ & threshold for significance\\
$\Dnull$ & null design with no distributional difference across categories\\
$\Df$ & design with distributional difference only across facets categories\\
$\Dx$ & design with distributional difference only across x-axis categories\\
\addlinespace
$\Dfx$ & design with distributional difference across both facet and x-axis\\
v & continuous univariate measured variable\\
\bottomrule
\end{tabular}
\end{table}

 <!-- Thus, by CLT, ${\chi^2}_{m} \tilde{} N(m, 2m)$, which would depend on the number of discretization used to approximate the continuous distribution. -->
 <!-- Then $b_n = 1-1/n$ quantile of the normal distribution and $a_n = 1/[n*\phi(b_n)]$ where $\phi$ is the normal density function. $n$ is the number of pairwise comparisons being made. -->

<!-- The Jensen-Shanon distance between two probability distribution $p_1$ and $p_2$ is given by $$d = [D(p_1, r) + D(p_2, r)]/2 \quad where \quad r = (p_1 + p_2)/2$$ where, -->
<!-- $$D(p_1,p_2) = \int^{\infty}_{-\infty}p_1(x)log\frac{p_1(x)}{p_2(x)}\,dx$$ is the Kullback-Leibler divergence between $p_1$ and $p_2$.  -->

<!-- We call this measure of variation as Median Maximum Pairwise Distances (MMPD). -->

<!-- #### Distribution of Jensen-Shannon distances -->

<!-- Jensen-Shannon distances (JSD) are distributed as chi-squared with $m$ df where we discretize the continuous distribution with $m$ discrete values. Taking sample percentiles to approximate the integral would mean taking $m = 99$. -->
<!-- With large $m$, chi-squared is asymptotically normal by the CLT. Thus, by CLT, ${\chi^2}_{m} \tilde{} N(m, 2m)$, which would depend on the number of discretization used to approximate the continuous distribution. Then $b_n = 1-1/n$ quantile of the normal distribution and $a_n = 1/[n*\phi(b_n)]$ where $\phi$ is the normal density function. $n$ is the number of pairwise comparisons being made. -->

### Computation

The computation of the distance measure $\wpd$ for a panel involves characterizing distributions, computing distances between distributions, choosing a tuning parameter to specify the weight for different groups of distances and summarizing those weighted distances appropriately to estimate maximum variation. Furthermore, the data needs to be appropriately transformed to ensure that the value of $\wpd$ emphasizes detection of distributional differences across categories and not across different data generating processes.

<!-- but not when underlying distributions are different -->

<!-- irrespective of the distribution from which the data is generated. -->

<!-- is aimed to capture structure and patterns by estimating the maximum variation of the measured variable within a panel. $\wpd$ -->

<!-- the distributional differences need to be measured for both within-group and between-group categories. Moreover, a tuning parameter specifying the weightage given to within-facet or between-facet categories can help to choose between designs like \ref{fig:null4by2}b and c. The maximum of all these weighted differences serves as an estimate of the maximum variation in the panel and is taken as the value of $\wpd$. -->

#### Data transformation {-}

The intended aim of $\wpd$ is to capture differences in categories irrespective of the distribution from which the data is generated. Hence, as a pre-processing step, the raw data is normal-quantile transformed (NQT) [@Krzysztofowicz1997-bv], so that the transformed data follows a standard normal distribution. The empirical NQT involves the following steps:

 1. The observations of measured variable $v$ are sorted from the smallest to the largest observation $v_{(1)},\dots, v_{(n)}$.
 2. The cumulative probabilities $p_{(1)},\dots, p_{(n)}$ are estimated using $p_{(i)} = i/(n + 1)$  [@Hyndman1996-ty] so that $p_{(i)} = \text{Pr}(v \leq v_{(i)})$.
 3. Each observation $v_{(i)}$ of $v$ is transformed into $v^*(i) = \Phi^{-1}(p(i))$, with $\Phi$ denoting the standard normal distribution function.

#### Characterizing distributions {-}

Multiple observations of $v$ correspond to the subset $v_{jk} = \{s: A(s) = j, B(s) = k\}$. The number of observations might vary widely across subsets due to the structure of the calendar, missing observations or uneven locations of events in the time domain. In this paper, quantiles of $\{v_{jk}\}$ are chosen as a way to characterize distributions for the category $(a_j, b_k)$, $\forall j\in \{1, 2, \dots, \nx\}, k\in \{1, 2, \dots, \nf\}$. We use percentiles with $p = {0.01, 0.02, \dots, 0.99}$ to reduce the computational burden in summarizing distributions.

<!-- The benefit of using a non-parametric estimator is that there are less rigid assumptions made about the nature of the underlying distribution of the data. Sample quantiles could be used for estimating population quantiles in a non-parametric setup. describes the many ways of defining sample quantiles and recommends the use of median-unbiased estimator because of _desirable properties of a quantile estimator and can be defined independently of the underlying distribution._. -->

<!-- in this paper. -->

<!-- Each $v_{jk}$'s $\forall j\in \{1, 2, \dots, \nx\}, k\in \{1, 2, \dots, \nf\}\}$ are assumed to be drawn from a continuous probability distribution and have certain characteristics. Shape, central tendency, and variability are the common characteristics used to describe a distribution. -->

<!-- Mean, median or mode are generally used to describe the center of the distribution, while range, standard deviation, quantiles, standard errors and confidence intervals are used to describe variability. -->

<!-- The quantile of a distribution with probability $p$ is defined as $Q(p)=F^{-1}(p) = inf\{x: F(x) >p\}$, $0<p< 1$ where $F(x)$ is the distribution function. There are two broad approaches to quantile estimation, viz, parametric and non-parametric. The benefit of using a non-parametric estimator is that there are less rigid assumptions made about the nature of the underlying distribution of the data. Sample quantiles could be used for estimating population quantiles in a non-parametric setup. @Hyndman1996-ty describes the many ways of defining sample quantiles and recommends the use of median-unbiased estimator because of _desirable properties of a quantile estimator and can be defined independently of the underlying distribution._. The `stats::quantile()` function in @R-language could be used for practical implementation where type = 8 refers to the algorithm corresponding to the median-unbiased estimator. The default quantile chosen in this paper is percentiles computed for $p = {0.01, 0.02, \dots, 0.99}$, where for example, the $99^{th}$ percentile would be the value corresponding to $p=0.99$ and hence 99% of the observations would lie below that. -->

#### Distance between distributions {-}

<!-- One of the most important class of divergence is the f-divergence and includes measures like Kullback-Leibler divergence, Hellinger distance etc. The continuous version of f -divergence is given by -->
<!-- $$\Df(P||Q) := \int q(x)f(\frac{p(x)}{q(x)})$$, where -->
<!-- $f : [0,\infty) \rightarrow R \cup \{\infty\}$ is a continuous convex function, and $f(1) = 0$. -->

A common way to measure divergence between distributions is the Kullback-Leibler (KL) divergence [@Kullback1951-jy]. The KL divergence denoted by $D(q_1||q_2)$ is a non-symmetric measure of the difference between two probability distributions $q_1$ and $q_2$ and is interpreted as the amount of information lost when $q_2$ is used to approximate $q_1$. The KL divergence is not symmetric and hence can not be considered as a "distance" measure. The Jensen-Shannon divergence [@Menendez1997-in] based on the Kullback-Leibler divergence is symmetric and has a finite value. Hence, in this paper, the pairwise distances between the distributions of the measured variable are obtained through the square root of the Jensen-Shannon divergence, called Jensen-Shannon distance (JSD), and defined by
$$
  JSD(q_1||q_2) = \frac{1}{2}D(q_1||M) + \frac{1}{2}D(q_2||M),
$$
where $M = \frac{q_1+q_2}{2}$ and $D(q_1||q_2) := \int^\infty_{-\infty} q_1(x)f(\frac{q_1(x)}{q_2(x)})$ is the KL divergence between distributions $q_1$ and $q_2$. Other common measures of distance between distributions are Hellinger distance, total variation distance and Fisher information metric.

<!-- Furthermore, these distances are distributed as chi-squared with $m$ degrees of freedom [@Menendez1997-in], if the continuous distribution is being discretized with $m$ discrete values. Taking sample percentiles to approximate the integral would mean taking $m = 99$. As the degrees of freedom $m$ get larger, the chi-square distribution approaches the normal distribution. -->

#### Within-facet and between-facet distances {-}

\begin{figure}[!b]

{\centering \includegraphics[width=\textwidth]{/Users/sgup0008/Desktop/thesis-SG/img/dist_explain} 

}

\caption{Within and between-facet distances shown for two cyclic granularities A and B, where A is mapped to x-axis and B is mapped to facets. The dotted lines represent the distances between different categories. Panels 1) and 2) show the between-facet distances. Within-facet distances are illustrated in Panels 3) (when categories are un-ordered, shown only with respect to a1) and Panel 4) (when categories are ordered). When categories are ordered, distances should only be considered for consecutive x-axis categories. Between-facet distances are distances between different facet levels for the same x-axis category; for example, distances between {($a_1$,$b_1$) and ($a_1$, $b_2$)} or {($a_1$,$b_1$) and ($a_1$, $b_3$)}.}(\#fig:distance-explain)
\end{figure}

Pairwise distances could be within-facets or between-facets for $m = 2$. Figure \ref{fig:distance-explain} illustrates how they are defined. Pairwise distances are within-facets when $b_{k} = b_{k'}$, that is, between pairs of the form $(a_{j}b_{k}, a_{j'}b_{k})$ as shown in panel (3) of Figure \ref{fig:distance-explain}. If categories are ordered (like all temporal cyclic granularities), then only distances between pairs where $a_{j'} = (a_{j+1})$ are considered (panel (4)). Pairwise distances are between-facets when they are considered between pairs of the form $(a_{j}b_{k}, a_{j}b_{k'})$. There are a total of ${\nf \choose 2}\nx$ between-facet distances, and ${\nx \choose 2}\nf$ within-facet distances if they are unordered and $\nf(\nx-1)$ within-facet distances if they are ordered.

#### Tuning parameter {-}

For displays with $m>1$ granularities, we can use a tuning parameter to specify the relative weight given to each granularity. In general, the tuning parameters should be chosen such that $\sum_{i=1}^m \lambda_i = 1$.

Following the general principles of Gestalt theory, we wish to weight more heavily granularities that are plotted closer together. For $m=2$ we choose  $\lambda_x = \frac{2}{3}$ for the granularity on the x-axis and $\lambda_f = \frac13$ for the granularity mapped to facets, giving a relative weight of $2:1$ for within-facet to between-facet distances. No human experiment has been conducted to justify this ratio.  Specifying $\lambda_x>0.5$ will weight within-facet distances more heavily, while $\lambda_x <0.5$ would weight the between-facet distances more heavily. (See the supplements for more details.)

#### Raw distance measure {-}

The raw distance measure, denoted by $\wpdsub{raw}$, is computed after combining all the weighted distance measures appropriately. First, NQT is performed on the measured variable $v_t$ to obtain $v^*_t$ (_data transformation_). Then, for a fixed harmony pair $(A, B)$, percentiles of $v^*_{jk}$ are computed and stored in $q_{jk}$ (_distribution characterization_). This is repeated for all pairs of categories of the form $(a_{j} b_{k}, a_{j'}b_{k'}): \{a_j: j = 1, 2, \dots, \nx\}, B = \{ b_k: k = 1, 2, \dots, \nf\}$. The pairwise distances between pairs $(a_{j} b_{k}, a_{j'}b_{k'})$ denoted by $d_{(jk, j'k')} = JSD(q_{jk}, q_{j'k'})$ are computed (_distance between distributions_). The pairwise distances (_within-facet and between-facet_) are transformed using a suitable _tuning parameter_ ($0<\lambda<1$) depending on if they are within-facet($d_w$) or between-facets($d_b$) as follows:
\begin{equation}\label{def:raw-wpd}
  d*_{(j,k), (j'k')} =
    \begin{cases}
      \lambda d_{(jk), (j'k')},& \text{if } d = d_w;\\
      (1-\lambda) d_{(jk), (j'k')},       & \text{if } d = d_b.
    \end{cases}
\end{equation}
The $\wpdsub{raw}$ is then computed as
$$
  \wpdsub{raw} = \max_{j, j', k, k'}(d*_{(jk), (j'k')}) \qquad\forall j, j' \in \{1, 2, \dots, \nx\},\quad k, k' \in \{1, 2, \dots, \nf\}
$$
The statistic "maximum" is chosen to combine the weighted pairwise distances since the distance measure is aimed at capturing the maximum variation of the measured variable within a panel. The statistic "maximum" is, however, affected by the number of comparisons (resulting pairwise distances). For example, for a $(2, 3)$ panel, there are $6$ possible subsets of observations corresponding to the combinations $(a_1, b_1), (a_1, b_2), (a_1, b_3), (a_2 ,b_1), (a_2 ,b_2), (a_2, b_3)$, whereas for a $(2, 2)$ panel, there are only $4$ possible subsets $(a_1, b_1), (a_1, b_2), (a_2 ,b_1), (a_2 ,b_2)$. Consequently, the measure would have higher values for the panel $(2, 3)$ as compared to $(2, 2)$, since maximum is taken over higher number of pairwise distances.

### Adjusting for the number of comparisons

Ideally, it is desired that the proposed distance measure takes a higher value only if there is a significant difference between distributions across categories, and not because the number of categories $\nx$ or $\nf$ is high. That is, under designs like $\Dnull$, their distribution should not differ for a different number of categories. Only then could the distance measure be compared across panels with different levels. This calls for an adjusted measure, which normalizes for the different number of comparisons.

Two approaches for adjusting the number of comparisons are discussed, both of which are substantiated using simulations. The first one defines an adjusted measure $\wpdsub{perm}$ based on the permutation method to remove the effect of different comparisons. The second approach fits a model to represent the relationship between $\wpdsub{raw}$ and the number of comparisons and defines the adjusted measure ($\wpdsub{glm}$) as the residual from the model.

#### Permutation approach {-}

This method is somewhat similar in spirit to bootstrap or permutation tests, where the goal is to test the hypothesis that the groups under study have identical distributions. This method accomplishes a different goal of finding the null distribution for different groups (panels in our case) and standardizing the raw values using that distribution. The values of $\wpdsub{raw}$ are computed on many ($\nsub{perm}$) permuted data sets and stored in $\wpdsub{perm-data}$. Then $\wpdsub{perm}$ is computed as follows:
\begin{align*}
  \wpdsub{perm} = \frac{\wpdsub{raw} - \text{mean}(\wpdsub{perm-data})}{\text{sd}(\wpdsub{perm-data})}
\end{align*}
where $\text{mean}(\wpdsub{perm-data})$ and $\text{sd}(\wpdsub{perm-data})$ are the mean and standard deviation of $\wpdsub{perm-data}$ respectively. Standardizing $\wpd$ in the permutation approach ensures that the distribution of $\wpdsub{perm}$ under $\Dnull$ has zero mean and unit variance across all comparisons. While this works successfully to make the location and scale similar across different $\nx$ and $\nf$, it is computationally heavy and time consuming, and hence less user-friendly. Hence, another approach to adjustment, with potentially less computational time, is proposed.

<!-- for the $l^{th}$ panel and -->

#### Modeling approach {-}

In this approach, a Gamma generalized linear model (GLM) for $\wpdsub{raw}$ is fitted with the number of comparisons as the explanatory variable. Since, $\wpdsub{raw}$ is a Jensen-Shannon distance, it follows a Chi-square distribution [@Menendez1997-in], which is a special case of a Gamma distribution. Furthermore, the mean response is bounded, since any JSD is bounded by $1$ if a base $2$ logarithm is used [@Lin1991-pt]. Hence, by @Faraway2016-uk, an inverse link is used for the model, which is of the form $y = a+b\times\log(z) + e$, where $y = \wpdsub{raw}$, $z = (\nx \times \nf)$ is the number of groups and $e$ are idiosyncratic errors. Let $\text{E}(y) = \mu$ and $a + b\times\log(z) = g(\mu)$ where $g(\mu)= 1/\mu$ and $\hat \mu = 1/(\hat a + \hat b \log(z))$. The residuals from this model $(y-\hat y) = (y-1/(\hat a + \hat b \log(z)))$ would be expected to have no dependency on $z$. Thus, $\wpdsub{glm}$ is defined as the residuals from this model given by
$$\wpdsub{glm} = \wpdsub{raw} - 1/(\hat a + \hat b\times\log(\nx\times\nf))$$
The distribution of $\wpdsub{glm}$ under $\Dnull$ will have approximately zero mean and a constant variance (not necessarily 1).

#### Combination approach {-}

The simulation results (in Section \ref{sec:simulations}) show that the distribution of $\wpdsub{glm}$ under the null design is similar for high $\nx$ and $\nf$ (levels higher than $5$) but less so for lower values of $\nx$ and $\nf$. Hence, a combination approach is proposed where we use a permutation approach for categories with small numbers of levels, and a modeling approach for categories with higher numbers of levels. This ensures that the computational load of the permutation approach is alleviated while maintaining a similar null distribution across different categories. This approach, however, requires that the adjusted variables from the two approaches are brought to the same scale. We define $\wpdsub{glm-scaled} = \wpdsub{glm}\times \sigma^2_{\text{perm}}/\sigma^2_{\text{glm}}$ as the transformed $\wpdsub{glm}$ with a similar scale as $\wpdsub{perm}$. The adjusted measure from the combination approach, denoted by $\wpd$ is then defined as follows:
\begin{equation}
\wpd = \begin{cases}
         \wpdsub{perm},       & \text{if $\nx, \nf \le 5$};\\
         \wpdsub{glm-scaled}  & \text{otherwise}.\\
       \end{cases}
\end{equation}

<!-- ### Algorithm -->

<!-- The distance measure $\wpd$ between two cyclic granularities $A$ and $B$ is aimed to capture the strength of the structure by estimating the maximum within-group and between-group variations. Furthermore, -->

<!-- The steps employed for computing $\wpd$ is summarized as follows: -->

<!-- The values of $\wpd$ indeed depend upon the underlying distribution. This is also not desirable as the intended aim of $\wpd$ is to capture differences in categories irrespective of the distribution from which the data is generated. The steps employed for computing the distance measure is summarized as follows: -->

<!-- Hence, differences in distributions of the measured variable are computed between all pairs of categories $(a_{j} b_{k}, a_{j'}b_{k'}): j = 1, 2, \dots, \nx\}, B = \{ b_k: k = 1, 2, \dots, \nf\}$. Percentiles of $v_{jk}$ and $v_{j'k'}$ are computed and stored in $q_{jk}$ and $q_{j'k'}$ respectively. Then the pairwise distances between pairs $(a_{j} b_{k}, a_{j'}b_{k'})$ be denoted as $d_{(jk, j'k')} = JSD(q_{jk}, q_{j'k'})$ is computed. Pairwise distances could be within-facets or between-facets. Figure \ref{fig:distance-explain} illustrates how the within-facet or between-facet distances are defined. Pairwise distances are within-facets ($d_{w}$) when $b_{k} = b_{k'}$, that is, between pairs of the form $(a_{j}b_{k}, a_{j'}b_{k})$ as shown in panel (3) of Figure \ref{fig:distance-explain}. If categories are ordered (like all temporal cyclic granularities), then only distances between pairs where $a_{j'} = (a_{j+1})$ are considered (panel (4)). Pairwise distances are between-facets ($d_{b}$) when they are considered between pairs of the form $(a_{j}b_{k}, a_{j}b_{k'})$. Number of between-facet distances would be $^{\nf}C_2*\nx$ and number of within-facet distances are $\nf*(\nx-1)$ (ordered) and $^{\nx}C_2*\nf$ (un-ordered). The pairwise distances $d_{(jk, j'k')}$ are transformed using a suitable tuning parameter ($0<\lambda<1$) depending on if they are $d_{b}$ or $d_{w}$ as follows: -->



<!-- 5. Compute distribution of maximum distance ($M$) by shuffling the data $200$ times and finishing Steps1-4 in each case. Scale $M$ using the mean and sd of the distribution.

5. Use Steps 1-4 to compute maximum distance for $\forall k \in \{1, 2, \ldots, \nf\}$.

6. Compute the distance measure MMPD_raw = median $(M_1, M_2, \dots, M_K)$.

<!-- All of these distances are then aggregated by taking the maximum from these distances to obtain $\wpd$. -->

<!-- _j$ maps index set to a set $\{B_\ell \mid \ell =1,\dots,L\}$. Here, $A_k$ and $B_\ell$ are the levels/categories corresponding to $C_i$ and $C_j$ respectively. Let $S_{k\ell}$ be a subset of the index set such that for all $s \in S_{k\ell}$, $C_i(s) = A_k$ and $C_j(s) = B_\ell$. There are $KL$ such data subsets, one for each combination of levels ($A_k$, $B_\ell$). Moreover, consider that in the graphical space, $C_i$ is mapped to facets and $C_j$ is mapped to x-axis. -->

 <!-- old -->
<!-- Consider two cyclic granularities $C_i$ and $C_j$, such that $C_i$ maps index set to a set $\{A_k \mid k=1,\dots,\nf\}$ and $C_j$ maps index set to a set $\{B_\ell \mid \ell =1,\dots,L\}$. Here, $A_k$ and $B_\ell$ are the levels/categories corresponding to $C_i$ and $C_j$ respectively. Let $S_{k\ell}$ be a subset of the index set such that for all $s \in S_{k\ell}$, $C_i(s) = A_k$ and $C_j(s) = B_\ell$. There are $KL$ such data subsets, one for each combination of levels ($A_k$, $B_\ell$). Moreover, consider that in the graphical space, $C_i$ is mapped to facets and $C_j$ is mapped to x-axis. -->

<!-- The algorithm employed for computing the distance measure is summarized as follows: -->

<!-- 1. Fix harmony pair $(C_i, C_j)$. -->

<!-- 2. Fix $k$. Then there are $L$ groups corresponding to level $A_k$ of $C_i$. -->

<!-- 3. Compute $m = \binom{L}{2}$ pairwise distances between distributions of $L$ unordered levels and $m = L-1$ pairwise distances for $L$ ordered categories. -->

<!-- 4. Identify maximum within the $m$ computed distances. -->

<!-- <!-- 5. Compute distribution of maximum distance ($M$) by shuffling the data $200$ times and finishing Steps1-4 in each case. Scale $M$ using the mean and sd of the distribution. -->

<!-- 5. Use Steps 1-4 to compute maximum distance for $\forall k \in \{1, 2, \ldots, \nf\}$. -->

<!-- 6. Compute the distance measure MMPD_raw = median $(M_1, M_2, \dots, M_K)$. -->

<!-- \begin{algorithm}[!thb] -->
<!-- 	\caption{Calculation for a raw distance measure between two cyclic granularities $A = \{ a_j: j = 1, 2, \dots, \nx\}$, $B = \{ b_k: k = 1, 2, \dots, \nf\}$ with $A$ placed across x-axis and $B$ across facets.}\label{alg:scoreopt}. -->
<!-- 	\begin{algorithmic}[1] -->
<!-- 		\Procedure{RawMMPD}{$A = \{ a_j: j = 1, 2, \dots, \nx\}$, $B = \{ b_k: k = 1, 2, \dots, \nf\}$, $v = \{ v_t: t = 1, 2, \dots, T\}$}. -->
<!-- 		\For{$k=1:\nf$, $j=1:\nx$} -->
<!-- 		\State Find distances between pairs of all possible combinations of categories $(a_jb_k,a_j'b_k')$ by computing JSD between quantiles of the measured variable $q(v)$ across these combinations. -->
<!-- 		\State $d \gets JSD(\tilde{q(v)_{a_{j}b_{k}}},\tilde{q(v)_{a_{j}'b_{k}'}})$ -->
<!-- 		\If {$b_k = b_k'$} -->
<!-- 		\State $d* \gets \lambda d$ \Comment{upweight within-facet distances} -->
<!-- 		\Else -->
<!-- 		\State $d* \gets 1/\lambda d $ \Comment{downweight across-facet distances} -->
<!-- 		\EndFor -->
<!-- 		\State Set the raw distance measure as $max(d*)$ where max is taken over all $j, j', k, k'$. -->

<!-- 		\EndProcedure -->
<!-- 	\end{algorithmic} -->
<!-- \end{algorithm} -->

<!-- A stronger measure "max" is chosen for aggregating x-axis categories compared to "median" for aggregating facet categories as the measure is intended to put more importance in pointing towards distributional differences between x-axis categories than for facet categories. -->

<!-- ## Properties of $\wpd$ -->

<!-- Simulations were carried out to explore the behavior of $\wpd$ under the following factors that could potentially impact the values of $\wpd$: $\nx$, $\nf$, $\lambda$, $\omega$, $dist$ (normal/non-normal distributions with different location and scale), $ntimes$, and $designs$ and results are presented in two parts. The dependence of $\wpd$ on $\nx$ and $\nf$ under $\Dnull$ is presented here, which lays the foundation for the next section. The rest of the results that discuss the relationship of the $\wpd$ with other factors is presented in details in the Supplementary section of the paper. They show that the designs $\Df$ and $D_x$ intersect at $\lambda = 0.5$ and hence for up-weighing designs of the form $D_x$, $\lambda = 0.67$ has been considered for computation of $\wpd$ in the rest of the paper. -->

<!-- - $\nx$ (number of levels of x-axis) -->
<!-- - $\nf$ (number of levels of facets) -->
<!-- - $\lambda$ (tuning parameter) -->
<!-- - $\omega$ (increment in each panel design) -->
<!-- - $dist$ (normal/non-normal distributions with different location and scale) -->
<!-- - $n$ (sample size for each combination of categories) -->
<!-- - $designs$ ($\Dnull$, $\Df$, $D_x$ and $D_{fx}$) -->

<!-- - $\nsub{sim}$ (number of simulations) -->
<!-- - $\nsub{perm}$ (number of permutations of data)  -->

<!-- ### Simulation design{#sec:sim-study} -->

<!-- Observations are generated from a Gamma(2,1) distribution for each combination of $\nx$ and $\nf$ from the following sets: $nx = \nf = \{2, 3, 5, 7, 14, 20, 31, 50\}$ to cover a wide range of levels from very low to moderately high. Each combination is being referred to as a _panel_. That is, data is being generated for each of the panels $\{nx = 2, \nf = 2\}, \{nx = 2, \nf = 3\}, \{nx = 2, \nf = 5\}, \dots, \{nx = 50, \nf = 31\}, \{nx = 50, \nf = 50\}$. For each of the $64$ panels, $ntimes = 500$ observations are drawn for each combination of the categories. That is, if we consider the panel $\{nx = 2, \nf = 2\}$, $500$ observations are generated for each of the combination of categories from the panel, namely, $\{(1, 1), (1, 2), (2, 1), (2, 2)\}$. The values -->
<!-- of $\wpd$ is obtained for each of the panels. This design corresponds to $\Dnull$ as each combination of categories in a panel are drawn from the same distribution. Furthermore, the data is simulated for each of the panels $\nsub{sim}=200$ times, so that the distribution oaf $\wpd$ under $\Dnull$ could be observed. $wpd_{l, s}$ denotes the value of $\wpd$ obtained for the $l^{th}$ panel and $s^{th}$ simulation. -->

<!-- ### Results -->

<!-- Figure \ref{fig:raw} shows the distribution of $\wpd$ plotted across different $\nx$ and $\nf$ categories. Since under $\Dnull$, there is no difference in distributions across different categories, we expect the distance measure $\wpd$ to reflect that as well and have the same distribution across categories. But Figure \ref{fig:raw} shows that both the location and scale of the distributions change across panels. This is not desirable under $\Dnull$ as it would mean comparisons of $\wpd$ values is not appropriate across different $\nx$ and $\nf$. Figure \ref{fig:quadratic} shows how the median of $\wpd$ varies with the total number of distances $nx*\nf$ for each panel. The median increases abruptly for lower values of $nx*\nf$ and slowly for higher $nx*\nf$. -->

<!-- Furthermore, simulation results also show that the values of $\wpd$ indeed depend upon the underlying distribution. This is also not desirable as the intended aim of $\wpd$ is to capture differences in categories irrespective of the distribution from which the data is generated. -->

<!-- last writeup -->
<!-- In the linear model approach, $wpd\in R$ was assumed, whereas, $\wpd$ is a Jensen-Shannon Distance (JSD) and lies between 0 and 1 [@JSD]. Furthermore, JSD follows a Chi-square distribution, which is a special case of Gamma distribution. Therefore, a generalized linear model could be fitted instead of a linear model to allow for the response variable to follow a Gamma distribution. The inverse link is used when we know that the mean response is bounded, which is applicable in our case since $0 \leq wpd\leq 1$. -->

<!-- We fit a Gamma generalized linear model with the inverse link which is of the form:  -->
<!-- $$y_l = a+b*log(z_l) + e_l$$, where $y_l = median_m(wpd_{l, m})$, $z_l$ is the $l^{th}$ panel and $e_l$ are idiosyncratic errors. Let $E(y) = \mu$ and $a + b*log(z) = g(\mu)$ where $g$ is the link function. Then $g(\mu)= 1/\mu$ and $\hat \mu = 1/(\hat a + \hat b log(z))$. The residuals from this model $(y-\hat y) = (y-1/(\hat a + \hat b log(z)))$ would be expected to have no dependency on $z$. Thus, $\wpdsub{glm}$ is chosen as the residuals from this model and is defined as: -->
<!-- $\wpdsub{glm} = wpd - 1/(\hat a + \hat b*log(nx*\nf))$. -->

<!-- which is more approximate than exact but still has the similar accuracy when compared to the permutation approach. -->

<!-- use later -->
<!-- The measure $\wpd$ could potentially lead to comparison of the measure across different panels and also help distinguishing the interesting panels from a data set. We discuss two approaches for normalization, both of which are substantiated using simulations. -->

<!-- ## Methodology -->

<!-- The transformed ${wpd}$ which is normalized for the values of $\nx$ and $\nf$ is denoted by $\wpd$. Two approaches have been employed - the first one involves a permutation method to make the distribution of the transformed $\wpd$ similar for different comparisons and the second one fits a model to represent the relationship between the two variables and defines $\wpd$ as the residual of the model. -->

<!-- ### Notations -->

<!-- Let $\{nx_{i}, i = 1, 2, \dots, nx\}$, $\{\nf_{j}, j = 1, 2, \dots, \nf\}$ be the set of x-axis and facet categories respectively. Each combination of $nx_{i}$ and $\nf_{j}$is being referred to as a _panel_. Then the total number of panel is $nx*\nf$. Let the total number of pairwise distances that could result in each panel be $\{z_k, k = 1, 2 , \dots, nx*\nf\}$. Here, $\{z_1 = nx_1*\nf_1\}$, $\{z_2 = nx_2*\nf_2\}$ and $\{z_k = nx*\nf\}$. Now, let $\{x_{k,l}, k = 1, 2, \dots, nx*\nf, l = 1, 2, \dots, \nsub{sim}\}$ denote the values of $\wpdsub{raw}$ obtained from the simulation study for $k^{th}$ panel in the $i^{th}$ simulation. Hence, for each of those $k$ panel, we have $\nsub{sim}$ values of $\wpdsub{raw}$. -->

<!-- use soon -->
<!-- ## Properties -->

<!-- This section reports the results of a simulation study that was carried out to evaluate the behavior of $\wpd$ under different designs and other potential factors. The behavior of $\wpd$ is explored in designs where there is in fact difference in distribution between facet categories ($\Df$) or across x-categories ($D_x$) or both ($D_{fx}$). Using $\omega = \{1, 2, \dots, 10\}$ and $\lambda = seq(from = 0.1, to = 0.9, by = 0.05)$, observations are drawn from a N(0,1) distribution for each combination of $\nx$ and $\nf$ from the following sets: $nx = \nf = \{2, 3, 5, 7, 14, 20, 31, 50\}$. $ntimes = 500$ is assumed for this setup as well. Furthermore, to generate different distributions across different combination of facet and x levels, the following method is deployed - suppose the distribution of the combination of first levels of $x$ and $facet$ category is $N(\mu,\sigma)$ and $\mu_{jk}$ denotes the mean of the combination $(a_jb_k)$, then $\mu_{j.} = \mu + j\omega$ (for design $D_x$) and $\mu_{.k} = \mu + k\omega$ (for design $\Df$). -->

<!-- The tabulated values and graphical representations of the simulation results are provided in the Supplementary paper. The learning from the simulations are as follows: The values of $\wpd$ is least for $\Dnull$, followed by -->
<!-- $\Df$, $D_x$ and $D_{fx}$. This is a desirable result since the measure $\wpd$ was designed such that this relationship holds. Furthermore, the distribution of the measure $\wpd$ does not change for different facet and x categories. The distribution of $\wpd$ looks similar with at least the mean and standard of the distributions being uniform across panels. This means $\wpd$ could be used to measure differences in distribution across panels. Also, note that since the data is processed using normal-quantile-transform, this measure is independent of the initial distribution of the underlying data and hence is also comparable across different data sets. This is valid for the case when sample size $ntimes$ for each combination of categories is at least 30 and $\nsub{perm}$ used for computing $\wpd$ is at least 100. More detailed results about the properties of $\wpd$ could be found in the Supplementary paper. -->

## Ranking and selection of cyclic granularities {#sec:rank-wpd}

A cyclic granularity is referred to as "significant" if there is a significant distributional difference of the measured variable between different categories of the harmony. In this section, a selection criterion to choose significant harmonies is provided, thereby eliminating all harmonies that exhibit non-significant differences in the measured variable. The distance measure $\wpd$ is used as a test statistic to test the null hypothesis that no harmony/cyclic granularity is significant. We select only those harmonies/cyclic granularities for which the test fails. They are then ranked based on how well they capture variation in the measured variable.

<!-- . Randomization tests (permutation tests) generates a random distribution by re-ordering the observed data and allows us to test if the observed data is significantly different from any random distribution. -->
<!-- the harmonies are not significant under null hypothesis and then test if our hypotheses are true: -->

<!--  $H_0$: harmonies are not significant -->
<!--  $H_1$: at least one harmony is significant -->

<!-- we provide a method to select important harmonies by eliminating all harmonies for which patterns are not significant by employing randomization test. Randomization tests (permutation tests) generates a random distribution by re-ordering our observed data and allow to test if the observed data is significantly different from any random distribution. Complete randomness in the measured variable indicates that the process follows a homogeneous underlying distribution over the whole time series, which essentially implies there is no interesting distinction across any different categories of the cyclic granularities. -->

### Selection

<!-- Under the null hypothesis, the harmonies are not significant. -->

A threshold (and consequently a selection criterion) is chosen using the notion of randomization tests [@edgington2007randomization]. The data is permuted several times and $\wpd$ is computed for each of the permuted data sets to obtain the sampling distribution of $\wpd$ under the null hypothesis. If the null hypothesis is true, then $\wpd$ obtained from the original data set would be a likely value in the sampling distribution. But in case the null hypothesis is not true, then it is less probable that $\wpd$ obtained for the original data will be from the same distribution. This idea is utilized to come up with a threshold for selection, denoted by $\wpdsub{threshold}$, defined as the $99^{th}$ percentile of the sampling distribution. A harmony is selected if the value of $\wpd$ for that harmony is greater than the chosen threshold. The detailed algorithm for choosing a threshold and selection procedure (for two cyclic granularities) is listed as follows:

<!-- The selection criterion is stated as follows: for $(i \in {1, 2, \dots, H_{N_C}})$, select $H_i$ if $wpd_i > \wpdsub{threshold}$, else reject. -->

- **Input:** All harmonies of the form $\{(A, B),~ A = \{ a_j: j = 1, 2, \dots, \nx\},~ B = \{ b_k: k = 1, 2, \dots, \nf\}\}$, $\forall (A, B) \in H_{N_C}$.

- **Output:** Harmony pairs $(A, B)$ for which $\wpd$ is significant.

1. For each harmony pair $(A, B) \in H_{N_C}$, the following steps are taken.

    a. Given the measured variable; $\{v_t: t=0, 1, 2, \dots, T-1\}$, $\wpd$ is computed and is represented by $\wpd^{A, B}_{obs}$.
    b. For $i=1,\dots,M$, randomly permute the original time series: $\{v_t^{i}: t=0, 1, 2, \dots, T-1\}$ and compute $\wpd^{A, B}_{i}$ from $\{v_t^{i}\}$.
    c. Define $\wpdsub{sample} = \{\wpd^{A, B}_{1}, \dots, \wpd^{A, B}_{M}\}$.

2. Stack the $\wpdsub{sample}$ vectors as $\wpdsub{sample}^{\text{all}}$ and compute its $p = 100(1-\alpha)$ percentiles as $\wpdsub{threshold\emph{p}}$.
3. If $\wpd^{A, B}_{obs} > \wpdsub{threshold\emph{p}}$, harmony pair $(A, B)$ is selected at the $1-p/100$ level, otherwise rejected.
4. Harmonies selected using the $99^{th}$, $95^{th}$ and $90^{th}$ thresholds are tagged as \*\*\*, \*\*, \* respectively.

### Ranking {#sec:ranking}

The distribution of $\wpd$ is expected to be similar for all harmonies under the null hypothesis, since they have been adjusted for different number of categories for the harmonies or underlying distribution of the measured variable. Hence, the values of $\wpd$ for different harmonies are comparable and can be used to rank the significant harmonies. A higher value of $\wpd$ for a harmony indicates that higher maximum variation in the measured variable is captured through that harmony.










Figure \ref{fig:null4by2} also presents the results of $\wpd$ from the illustrative designs in Section \ref{sec:computation-wpd}. The value of $\wpd$ under null design (a) is the least, followed by (b), (c) and (d). This aligns with the principle of $\wpd$, which is expected to have lowest value for null designs and highest for designs of the form $\Dfx$ (d). Moreover, note the relative differences in $\wpd$ values between (b) and (c). The value of the tuning parameter $\lambda$ is set to 2/3, which gives greater emphasis to differences in x-axis categories than facets.

Again consider Figures \ref{fig:onegran-new}(a) and \ref{fig:onegran-new}(b) with a $\wpd$ value of 20.5 and 145 respectively. This is because there is a more gradual increase across hours of the day than across months of the year. If the order of categories is ignored, the resulting $\wpd$ values are $97.8$ and $161$ respectively, because differences between any hours of the day tend to be larger than differences only between consecutive hours. Similarly, Figures \ref{fig:id2-new2}(a) and (b) have $\wpd$ values of $110.79$ and $125.82$ respectively. The ranking implies that the distributional differences are more prominent for the second household, as is also seen from the bigger fluctuations in the $90^{th}$ percentile than for the first household.

<!-- ## Choosing a threshold -->

<!-- A randomization test involves calculating a test statistic, randomly shuffling the data and calculating the test statistic several times to obtain a distribution of the test statistic. But we will use this procedure to obtain a threshold such that harmony pairs with a $\wpd$ value higher than this threshold will only be considered significant. The process of choosing a threshold is described as follows: -->

<!-- to test if there is any interesting pattern captured by the harmonies, which essentially implies if $\wpd$ is significantly different from zero. The percentages of times the $\wpd$ obtained from the permuted data is greater than or equal to the observed $\wpd$ is the p-value. The randomization test is described as follows: -->

<!-- <!-- We can remove the harmonies for which no interesting patterns are observed through a randomization permutation method. --> 
<!-- Essentially, the assumption is that under the null hypothesis, there is no difference in categories between the pair of cyclic granularities in the chosen harmony. This method is based on the generation of randomly chosen reassignments (permutations) of the data across different cyclic granularities and the computation of -->
<!-- $\wpd$ for each of these reassignments. -->

<!-- **Assumption:** random permutation of the data keeping the categories of the cyclic granularities constant. -->

<!-- Complete randomness in the measured variable indicates that the process follows a homogeneous underlying distribution over the whole time series, which essentially implies there is no interesting distinction across any different categories of the cyclic granularities. We can remove the harmonies for which no interesting patterns are observed through a randomization permutation method. Essentially, the assumption is that under the null hypothesis, there is no difference in categories between the pair of cyclic granularities in the chosen harmony. This method is based on the generation of randomly chosen reassignments (permutations) of the data across different cyclic granularities and the computation of -->
<!-- $\wpd$ for each of these reassignments. The percentages of times the theoretical distribution greater than or equal to the respective observed $\wpd$ values are calculated and are used to obtain the P value. The procedure for the permutation test is: -->

<!-- **Assumption:** random permutation of the data keeping the categories of the cyclic granularities constant. -->

<!-- 14, 20, 31, 50\}$. For each of the $64$ panels, $ntimes = 500$ observations are drawn for each combination of the categories. The values of $\wpd$ is obtained for each of the panels for the designs $\Dnull$, $D_x$, $\Df$ and $D_{fx}$. $\wpdsub{threshold}$ is computed from all of these panels together and the number of times a harmony pair $(A, B) \in H_{N_C}$ is selected when in fact it was of the design $\Dnull$ is noted. This entire process is repeated for several null data sets to see the number of times any harmony pair $(A, B) \in H_{N_C}$ is selected under null. -->

<!-- ### Results -->

### Simulations {#sec:simulations}

Simulations were carried out to explore the behavior of $\wpd$ as $\nx$ and $\nf$ were varied, in order to compare and evaluate different normalization approaches. More detailed simulation results, including an exploration of the size and power of the proposed test, are contained in the supplements.

#### Simulation design {-}

\noindent $m=1$

\noindent Observations were generated from a N(0,1) distribution for $\nx \in \{2, 3, 5, 7, 9, 14, 17, 20, 24, 31, 42, 50\}$, with $\nsub{times} = 500$ observations drawn for each combination of categories. This design corresponds to $\Dnull$. For each of the categories, there were $\nsub{sim}=200$ replications, so that the distribution of $\wpd$ under $\Dnull$ could be observed. Let $\wpd_{\ell, s}$ denote the value of $\wpd$ obtained for the $\ell^{th}$ panel and $s^{th}$ simulation.

\noindent $m=2$

\noindent Similarly, observations were generated from a N(0,1) distribution for each combination of $\nx$ and $\nf$ from $\{2, 3, 5, 7, 14, 20, 31, 50\}$, with $\nsub{times} = 500$ observations drawn for each of the $64$ combinations.

#### Results {-}

Figure \ref{fig:raw} shows that both the location and scale of the distributions change across panels. This is not desirable under $\Dnull$ as it would mean comparisons of $\wpd$ values are not appropriate across different $\nx$ and $\nf$ values. Table \ref{tab:glm-tab} gives the summary of a Gamma generalized linear model to capture the relationship between $\wpdsub{raw}$ and the number of comparisons. The intercepts and slopes are similar, independent of the underlying distributions (see supplementary paper for details) and hence the coefficients are shown for the case when observations are drawn from a N(0,1) distribution. Figure \ref{fig:dist-new-same-scale-link} shows the distribution of $\wpdsub{perm}$ and $\wpdsub{glm-scaled}$ on the same scale to show that a combination approach could be used for higher values of $\nx$ and $\nf$ to alleviate the computational time of the permutation approach.

(ref:raw) Distribution of $\wpdsub{raw}$ is plotted across different $\nx$ and $\nf$ categories under $\Dnull$ through density and rug plots. Both location (blue line) and scale (orange marks) of the distribution shifts for different panels. This is not desirable since under the null design, the distribution is not expected to capture any differences.

\begin{figure}[!htb]

{\centering \includegraphics[width=\textwidth]{figure/raw-1} 

}

\caption{(ref:raw)}(\#fig:raw)
\end{figure}

\begin{table}

\caption{(\#tab:glm-tab)Results of generalised linear model to capture the relationship between $\wpdsub{raw}$ and the number of comparisons.}
\centering
\begin{tabular}[t]{llrrrr}
\toprule
$m$ & term & estimate & std.error & statistic & p.value\\
\midrule
1 & Intercept & 26.09 & 0.54 & 48.33 & 0\\
1 & $\log(\nx \times \nf)$ & -1.87 & 0.19 & -9.89 & 0\\
2 & Intercept & 23.40 & 0.22 & 104.14 & 0\\
2 & $\log(\nx \times \nf)$ & -0.96 & 0.04 & -21.75 & 0\\
\bottomrule
\end{tabular}
\end{table}









(ref:dist-new-same-scale-link) The distributions of $\wpdsub{perm}$ and $\wpdsub{glm-scaled}$ are overlaid to compare the location and scale across different $\nx$ and $\nf$. $\wpdsub{norm}$ takes the value of $\wpdsub{perm}$ for lower levels, and $\wpdsub{glm-scaled}$ for higher levels to to alleviate the problem of computational time in permutation approaches. This is possible as the distribution of the adjusted measure looks similar for both approaches for higher levels.

\begin{figure}

{\centering \includegraphics[width=\textwidth]{figure/dist-new-same-scale-link-1} 

}

\caption{(ref:dist-new-same-scale-link)}(\#fig:dist-new-same-scale-link)
\end{figure}

These results justify our use of the permutation approach when $\nx<5$ and $\nf<5$, and the use of the GLM otherwise.
<!--
## Choosing the threshold

### Simulation design {-}
 
Observations were generated from a N(0,1) distribution for each combination of $\nx$ and $\nf$ from the following sets: $nx \in \{3, 7, 14\}$ and $\nf \in \{2, 9, 10\}$, giving nine possible panels/combinations. In the first scenario, data for all panels were simulated using the null design $\Dnull$. In other scenarios, data simulated from the panel $(14, 2)$ and $(3, 10)$ used $\Dfx$ where the means of the distributions increase by $\omega$ across both x-axis and facets, where $\omega \in \{0.5, 2, 5\}$. This allows us to examine if the proposed test is able to capture subtle differences and obvious differences when we shift from the null design.

In the last scenario, we consider the panel $(3, 2), (7,9), (14, 10)$ to be under $\Dnull$, the panels $(7, 2), (14, 9)$ to be under $\Df$. $(14, 2), (3, 10)$ under $D_x$ and the rest under $\Dvar{{null}}$. This is done to check if the consequent ranking procedure leads to designs like $D_{vary_{all}}$ to be chosen first followed by $D_{vary_{all}}$. We generate only one data set each for which these scenarios were simulated and consider this as the original data set. We generate $1000$ repetitions of this experiment with different seeds.

### Results {-}

For the first scenario, we set $\wpdsub{threshold99}$ so that the probability of rejecting each panel under $\Dnull$ is $0.01$. We have 9 tests, so the size of the tests overall is $1-0.99^9 \approx 0.09$. We also compute the proportion of times a panel is rejected when it belongs to a non-null design to obtain the power of the test. The detailed results and graphics are included in the Supplementary paper.


 -->

## Application to residential smart meter dataset {#sec:application-wpd}

The smart meter data set for eight households in Melbourne was procured by downloading the data from the energy supplier/retailer. The data has been cleaned to form a `tsibble` [@wang2020tsibble] containing half-hourly electricity consumption from July to December 2019 for each of the households. No behavioral pattern is likely to be discerned from the time plot of energy usage over the entire period, since the plot will have too many observations squeezed in a linear representation. When we zoom into the September 2019 data in Figure \ref{fig:dotplot-8}(b), some patterns are visible in terms of peaks and troughs, but we do not know if they are regular or what is their period.

(ref:dotplot-8) An ensemble plot with a heatmap (a), line plot (b), parallel coordinate plot (c) to demonstrate energy behavior of the households in different ways. Panel (b) shows the raw demand series for September to highlight the repetitive patterns of energy demand. Panel (a) shows $\wpd$ values across harmonies where a darker color indicates a higher ranking harmony. A significant harmony is shown with an asterisk. For example, ids 7 and 8 have significant patterns across (*hod*, *dow*) and (*dow*, *hod*). Panel (c) is useful for comparing households across harmonies. For example, for the harmony (*dow*-*hod*), ids 1 and 7 have the least and highest $\wpd$ respectively.

\begin{figure}

{\centering \includegraphics[width=\textwidth]{figure/dotplot-8-1} 

}

\caption{(ref:dotplot-8)}(\#fig:dotplot-8)
\end{figure}

Electricity demand, in general, has daily, weekly and annual seasonal patterns. However, it is not apparent from this view if all households have those patterns, or how strong they are in each case. It is also not clear from this view if any other periodic patterns are present in any household. We start the analysis by choosing a few harmonies, and ranking them for each of these households. The ranking and selection of significant harmonies is validated by analyzing the distribution of energy usage across significant harmonies.

<!-- if we are benefiting by moving from a linear to cyclic representation of time to . -->



#### Choosing cyclic granularities of interest and removing clashes {-}

Let $v_{i, t}$ denote the electricity demand for the $i^{th}$ household in time period $t$. The series $\{v_{i, 1},\dots,v_{i,T}\}$ is the linear granularity corresponding to half-hour since the interval of the `tsibble` is 30 minutes. We consider coarser linear granularities like hour, day, week and month from the commonly used Gregorian calendar. From the four linear granularities of hour, day, week, and month, we obtain $N_C = 4\times 3/2 = 6$ cyclic granularities:  "hour_day", "hour_week", "hour_month", "day_ week", "day_month" and "week_month". Further, we add cyclic granularity day-type ("wknd wday") to capture weekend and weekday behavior. Thus, seven cyclic granularities are considered to be of interest. The pairs of cyclic granularities ($C_{N_C}$) will have $7\times 6=42$ elements. The set of possible harmonies $H_{N_C}$ from $C_{N_C}$ are chosen by removing clashes using procedures described in @Gupta2021-gravitas. Table \ref{tab:tab-rank-8} shows $14$ harmony pairs that belong to $H_{N_C}$.






Table: (\#tab:tab-rank-8)Ranking of harmonies for the eight households with significance marked for different thresholds. Rankings are different and at most three harmonies are significant for any household. The number of harmonies to explore is reduced from 42 to 3.

|facet variable |x variable |id 1                        |id 2                        |id 3                       |id 4                        |id 5                      |id 6                       |id 7                        |id 8                       |
|:--------------|:----------|:---------------------------|:---------------------------|:--------------------------|:---------------------------|:-------------------------|:--------------------------|:---------------------------|:--------------------------|
|hod            |wdwnd      |\phantom{0}1\rlap{$^{***}$} |\phantom{0}2\rlap{$^{*}$}   |\phantom{0}1\rlap{$^{**}$} |\phantom{0}2\rlap{$^{**}$}  |\phantom{0}3\rlap{$^{}$}  |\phantom{0}1\rlap{$^{**}$} |\phantom{0}3\rlap{$^{}$}    |\phantom{0}3\rlap{$^{*}$}  |
|dom            |hod        |\phantom{0}2\rlap{$^{***}$} |\phantom{0}4\rlap{$^{}$}    |\phantom{0}3\rlap{$^{**}$} |\phantom{0}3\rlap{$^{**}$}  |\phantom{0}4\rlap{$^{}$}  |\phantom{0}3\rlap{$^{*}$}  |\phantom{0}4\rlap{$^{}$}    |\phantom{0}6\rlap{$^{}$}   |
|wdwnd          |hod        |\phantom{0}3\rlap{$^{**}$}  |10\rlap{$^{}$}              |\phantom{0}7\rlap{$^{}$}   |\phantom{0}7\rlap{$^{}$}    |\phantom{0}6\rlap{$^{}$}  |\phantom{0}8\rlap{$^{}$}   |\phantom{0}8\rlap{$^{}$}    |10\rlap{$^{}$}             |
|hod            |wom        |\phantom{0}4\rlap{$^{}$}    |\phantom{0}9\rlap{$^{}$}    |\phantom{0}6\rlap{$^{}$}   |\phantom{0}5\rlap{$^{}$}    |\phantom{0}5\rlap{$^{}$}  |\phantom{0}5\rlap{$^{}$}   |\phantom{0}5\rlap{$^{}$}    |\phantom{0}5\rlap{$^{}$}   |
|wom            |wdwnd      |\phantom{0}5\rlap{$^{}$}    |14\rlap{$^{}$}              |14\rlap{$^{}$}             |10\rlap{$^{}$}              |12\rlap{$^{}$}            |\phantom{0}9\rlap{$^{}$}   |12\rlap{$^{}$}              |13\rlap{$^{}$}             |
|hod            |dow        |\phantom{0}6\rlap{$^{}$}    |\phantom{0}1\rlap{$^{***}$} |\phantom{0}2\rlap{$^{**}$} |\phantom{0}1\rlap{$^{***}$} |\phantom{0}1\rlap{$^{*}$} |\phantom{0}2\rlap{$^{**}$} |\phantom{0}2\rlap{$^{**}$}  |\phantom{0}1\rlap{$^{**}$} |
|wdwnd          |wom        |\phantom{0}7\rlap{$^{}$}    |12\rlap{$^{}$}              |13\rlap{$^{}$}             |\phantom{0}8\rlap{$^{}$}    |\phantom{0}7\rlap{$^{}$}  |\phantom{0}7\rlap{$^{}$}   |10\rlap{$^{}$}              |12\rlap{$^{}$}             |
|dow            |hod        |\phantom{0}8\rlap{$^{}$}    |\phantom{0}3\rlap{$^{}$}    |\phantom{0}4\rlap{$^{**}$} |\phantom{0}4\rlap{$^{**}$}  |\phantom{0}2\rlap{$^{}$}  |\phantom{0}4\rlap{$^{*}$}  |\phantom{0}1\rlap{$^{***}$} |\phantom{0}2\rlap{$^{**}$} |
|hod            |dom        |\phantom{0}9\rlap{$^{}$}    |\phantom{0}7\rlap{$^{}$}    |10\rlap{$^{}$}             |13\rlap{$^{}$}              |10\rlap{$^{}$}            |10\rlap{$^{}$}             |\phantom{0}9\rlap{$^{}$}    |\phantom{0}4\rlap{$^{}$}   |
|wom            |dow        |10\rlap{$^{}$}              |\phantom{0}6\rlap{$^{}$}    |\phantom{0}8\rlap{$^{}$}   |\phantom{0}9\rlap{$^{}$}    |\phantom{0}8\rlap{$^{}$}  |\phantom{0}6\rlap{$^{}$}   |\phantom{0}7\rlap{$^{}$}    |\phantom{0}9\rlap{$^{}$}   |
|dow            |wom        |11\rlap{$^{}$}              |\phantom{0}5\rlap{$^{}$}    |\phantom{0}9\rlap{$^{}$}   |11\rlap{$^{}$}              |11\rlap{$^{}$}            |12\rlap{$^{}$}             |\phantom{0}6\rlap{$^{}$}    |\phantom{0}7\rlap{$^{}$}   |
|wom            |hod        |12\rlap{$^{}$}              |\phantom{0}8\rlap{$^{}$}    |\phantom{0}5\rlap{$^{}$}   |\phantom{0}6\rlap{$^{}$}    |\phantom{0}9\rlap{$^{}$}  |11\rlap{$^{}$}             |11\rlap{$^{}$}              |\phantom{0}8\rlap{$^{}$}   |
|dom            |wdwnd      |13\rlap{$^{}$}              |13\rlap{$^{}$}              |11\rlap{$^{}$}             |12\rlap{$^{}$}              |14\rlap{$^{}$}            |14\rlap{$^{}$}             |14\rlap{$^{}$}              |14\rlap{$^{}$}             |
|wdwnd          |dom        |14\rlap{$^{}$}              |11\rlap{$^{}$}              |12\rlap{$^{}$}             |14\rlap{$^{}$}              |13\rlap{$^{}$}            |13\rlap{$^{}$}             |13\rlap{$^{}$}              |11\rlap{$^{}$}             |

 <!-- The paper also shows how the number of harmonies could be identified among by removing clashes. This section shows how we could refine the search further by only looking at significant harmonies. -->

#### Selecting and ranking harmonies for all households {-}

$\wpd_{i}$ is computed on $v_{i, t}$ for all harmony pairs $\in H_{N_C}$ and for each household $i \in \{1, 2, \dots, 8\}$. The harmony pairs are then arranged in descending order and highlighted with `***`, `**` and `*` corresponding to the $99^{th}$, $95^{th}$ and $90^{th}$ percentile threshold. Table \ref{tab:tab-rank-8} shows the rank of the harmonies for different households. The rankings are different for different households, which is a reflection of their varied behaviors. Most importantly, there are at most three harmonies that are significant for any household. This is a huge reduction in the number of potential harmonies to explore.

#### Detecting patterns not apparent from linear display {-}

Figure \ref{fig:dotplot-8} helps to compare households through the heatmap (a) across harmony pairs. Here *dom*, *dow*, *wdwnd* are abbreviations for day-of-month, day-of-week, weekday/weekend and so on. The colors represent the value of $\wpd$. Darker cells correspond to higher values of $\wpd$. Those with `*` correspond to $\wpd$ values above $\wpdsub{threshold95}$.

We can now see some patterns that were not discernible in Panel (b), including:

  1. id 7 and 8 have the same significant harmonies despite having very different total energy usage.
  2. id 6 and 7 differ in the sense that for id 6, the difference in patterns is only during weekday/weekends, whereas for id 7 all or few other days of the week are also important. This might be due to their flexible work routines or different day-off.
  3. There are no significant periodic patterns for id 5 when we fix the threshold to $\wpdsub{threshold95}$.

Note that the $\wpd$ values are computed over the entire range, but the linear display in (b) is only for September, with the major and minor x-axis corresponding to weeks and days respectively.

<!-- Households id 2 and 3 are similar, in terms of linear display and heatmap, which implies that similar periodic behavior can stem from households with different demographics. -->


#### Comparing households and validating rank of harmonies {-}

According to Figure \ref{fig:dotplot-8}(c), for the harmony pair (*dow*-*hod*), household id 7 has the greatest value of $\wpd$, while id 1 has the least. From Table \ref{tab:tab-rank-8} it can be seen that the harmony pair (*dow*, *hod*) is important for id 7; however, it has been labeled as an inconsequential pair for id 1. The distribution of energy demand for both of these households, with *dow* as the facet and *hod* on the x-axis, may help explain the choice. Figure \ref{fig:gravitas-plot-8} demonstrates that for id 7, the median (black) and quartile deviation (orange) of energy consumption fluctuates for most hours of the day and days of the week, while for id 1, daily patterns are more consistent within weekdays and weekends. As a result, for id 1, it is more appropriate to examine the distributional difference solely across (*dow*, *wdwnd*), which has been rated higher in Table \ref{tab:tab-rank-8}.

<!-- Although the differences might seem significant at first, with closer inspection it could be seen that the scale of the demand is lower in this case and hence the differences are not large enough to cross the threshold for significance. -->

<!-- Figure \ref{fig:gravitas-plot-8} is used to show -->
<!-- if this selection and ranking of harmony pairs makes sense for this household. Panel a) of Figure \ref{fig:gravitas-plot-8} shows the distribution of energy demand with weekday/weekend as the x-axis and hour-of-day as the facets and helps to compare the weekend/weekday patterns for different hours of the day. It could be observed that the difference between weekend and weekday is the highest from 15 to 19 hours of the day. Panel b) shows the distribution of energy demand with the variables swapped and helps to compare the daily patterns within weekday and weekend. It could be observed that the daily pattern is similar for weekdays and weekends with a morning and evening peak. However, the difference between morning and evening peaks are higher for weekends. Since $\wpd$ is designed to put more weightage on within-facet differences for $\lambda>0.5$, it makes sense that the pair (hod, wdwnd) has been ranked higher than (wdwnd, hod). Panel c) shows the distribution of energy demand with weekday/weekend as the x-axis and week-of-month as the facets. Although the differences might seem significant at first, with closer inspection it could be seen that the scale of the demand is lower in this case and hence the differences are not large enough to cross the threshold for significance. -->

(ref:gravitas-plot-8) Comparing distribution of energy demand shown for household id 1 (a) and 7 (b) across *hod* in x-axis and *dow* in facets through quantile area plots. The value of $\wpd$ in Table 3 suggests that the harmony pair (*dow*, *hod*) is significant for household id 7, but not for id 1. This implies that distributional differences are captured more by this harmony for id 7, which is apparent from this display with more fluctuations across median and 75th percentile for different hours of the day and day of week. For id 1, patterns look similar within weekdays and weekends. Here, the median is represented by the black line, the orange area corresponds to quartile deviation and the green area corresponds to area between $10^{th}$ and $90^{th}$ quantile.


\begin{figure}[!htb]

{\centering \includegraphics[width=\textwidth]{figure/gravitas-plot-8-1} 

}

\caption{(ref:gravitas-plot-8)}(\#fig:gravitas-plot-8)
\end{figure}

<!-- From Figure \ref{fig:gravitas-plot-8-id5}, it could further be observed that id5 has only one significant harmony (*hod*, *dow*). Apparently, $(hod, wnd/wday)$ which is an important harmony for most households is not important for this one. -->

## Discussion

Exploratory data analysis involves many iterations of finding and summarizing patterns. With temporal data available at finer scales, exploring time series has become overwhelming with so many possible granularities to explore. A common solution is to aggregate and look at the patterns across the usual granularities such as hour-of-day or day-of-week, but there is no way to know the “interesting” granularities a priori. A huge number of displays need to be analyzed or we might end up missing informative granularities. This work refines the search for informative granularities by identifying those for which the differences between the displayed distributions are greatest and rating them in order of importance of capturing maximum variation.

The significant granularities across different datasets (individuals/subjects) do not imply similar patterns across different datasets. They simply mean that maximum distributional differences are being captured across those granularities. A future direction of work is to be able to explore and compare many individuals/subjects together for similar patterns across significant granularities.

<!-- patterns are interesting and others are not. The subjects for which behaviors across a group of cyclic granularities are similar, could be grouped together as their periodic behavior is similar. to group subjects -->

## Acknowledgments {-}

The authors thank the ARC Centre of Excellence for Mathematical and Statistical Frontiers [(ACEMS)](https://acems.org.au/home) for supporting this research. Sayani Gupta was partially funded by [Data61 CSIRO](https://data61.csiro.au/) during her PhD. The Github repository, [github.com/Sayani07/paper-hakear](https://github.com/Sayani07/paper-hakear), contains all materials required to reproduce this article and the code is also available online in the supplemental materials. This article was created with R [@R-language], `knitr` [@knitr; @R-knitr] and `rmarkdown` [@rmarkdown; @R-rmarkdown]. Graphics are produced with `ggplot2` [@Wickham2009pk].

## Supplementary materials {- #supplement-hakear}

**Data and scripts:** Data sets and R code to reproduce all figures in this article (main.R).

\noindent
**Supplementary paper:** All simulation tables, graphics and and R codes to reproduce the supplementary paper (paper-supplementary.Rmd, paper-supplementary.pdf).

\noindent
**R-package:** The open-source R package `hakear` is available on Github (https://github.com/Sayani07/hakear) to implement ideas presented in this paper.


<!--chapter:end:Rmd//03-hakear.Rmd-->

# Clustering time series based on probability distributions across temporal granularities {#ch-gracsr}

Clustering is a potential approach for organizing large collections of time series into small homogeneous groups, but a difficult step is determining an appropriate metric to measure similarity between time series. The similarity metric needs to be capable of accommodating long, noisy, and asynchronous time series and also capture cyclical patterns. In this paper, two approaches for measuring distances between time series are presented, based on probability distributions over cyclic temporal granularities. Both are compatible with a variety of clustering algorithms. Cyclic granularities like hour-of-the-day, work-day/weekend, and month-of-the-year, are useful for finding repeated patterns in the data. Measuring similarity based on probability distributions across cyclic granularities serves two purposes: (a) characterizing the inherent temporal data structure of long, unequal-length time series in a manner robust to missing or noisy data; (b) small pockets of similar repeated behaviors can be captured. This approach is capable of producing useful clusters, as demonstrated on validation data designs and a sample of residential smart meter records.





















## Introduction

<!-- time series clustering and its challenges -->
Time series clustering is the process of unsupervised partitioning of $n$ time series data into $k$ ($k<n$) meaningful groups such that homogeneous time series are grouped together based on a certain similarity measure. The time series features, length of time series, representation technique, and, of course, the purpose of clustering time series all influence the suitable similarity measure or distance metric to a meaningful level. The three primary methods of time series clustering [@liao2005clustering] are algorithms that operate directly with distances or raw data points in the time or frequency domain (distance-based), with features derived from raw data (feature-based), or indirectly with models constructed from raw data (model-based). The efficacy of distance-based techniques is highly dependent on the distance measure utilized. Defining an appropriate distance measure for the raw time series may be a difficult task since it must take into account noise, variable lengths of time series, asynchronous time series, different scales, and missing data. Commonly used distance-based similarity measures as suggested by a review of time series clustering approaches [@Aghabozorgi2015-ct] are Euclidean, Pearson's correlation coefficient and related distances, Dynamic Time Warping (DTW), Autocorrelation, Short time series distance, Piecewise regularization, cross-correlation between time series, or a symmetric version of the Kullback–Liebler distances [@liao2007clustering] but on vector time series data. Among these alternatives, Euclidean distances have high performance but need the same length of data over the same period, resulting in information loss regardless of whether it is on raw data or a smaller collection of features. DTW works well with time series of different lengths [@corradini2001dynamic], but it is incapable of handling missing observations. Surprisingly, probability distributions, which may reflect the inherent temporal structure of a time series, have not been considered in determining time series similarity.

\noindent This work is motivated by a need to cluster a large collection of residential smart meter data, so that customers can be grouped into similar energy usage patterns. These can be considered to be univariate time series of continuous values which are available at fine temporal scales. These time series data are long (with more and more data collected at finer resolutions), are asynchronous, with varying time lengths for different houses and sporadic missing values. Using probability distributions is a natural way to analyze these types of data because they are robust to uneven length, missing data, or noise. This paper proposes two approaches for obtaining pairwise similarities based on Jensen-Shannon distances between probability distributions across a selection of cyclic granularities. Cyclic temporal granularities [@Gupta2021-hakear], which are temporal deconstructions of a time period into units such as hour-of-the-day or work-day/weekend, can measure repetitive patterns in large univariate time series data. The resulting clusters are expected to group customers that have similar repetitive behaviors across cyclic granularities. The benefits of this approach are as follows.

- When using probability distributions, data does not have to be the same length or observed during the exact same time period (unless there is a structural pattern).

<!-- (for example, some customers observed only for holidays, others observed over non holidays or year of observation is different which can lead to technological advancement.) -->

- Jensen-Shannon distances evaluate the distance between two distributions rather than raw data, which is less sensitive to missing observations and outliers than other conventional distance methods.

- While most clustering algorithms produce clusters similar across just one temporal granularity, this technique takes a broader approach to the problem, attempting to group observations with similar distributions across all interesting cyclic granularities.

- It is reasonable to define a time series based on its degree of trend and seasonality, and to take these characteristics into account while clustering it. The modification of the data structure by taking into account probability distributions across cyclic granularities assures that there is no trend and that seasonal variations are handled independently. As a result, there is no need to de-trend or de-seasonalize the data before applying the clustering method. For similar reasons, there is no need to exclude holiday or weekend routines.


<!--_Background and motivation_-->


The primary application of this work is data from the Smart Grid, Smart City (SGSC) project (2010–2014) available through @smart-meter. Half-hourly measurements of usage for more than 13,000 electricity smart meter customers are provided from October 2011 to March 2014. <!--Larger data sets include greater uncertainty about customer behavior due to growing variety of customers.--> Customers vary in size, location, and amenities such as solar panels, central heating, and air conditioning. The behavioral patterns differ amongst customers due to many temporal dependencies. Some customers use a dryer, while others dry their clothes on a line. Their weekly usage profile may reflect this. They may vary monthly, with some customers using more air conditioners or heaters than others, while having equivalent electrical equipment and weather circumstances. Some customers are night owls, while others are morning larks. Daily energy usage varies depending on whether customers stay home or work away from home. Age, lifestyle, family composition, building attributes, weather, availability of diverse electrical equipment, among other factors, make the task of properly segmenting customers into comparable energy behavior complex. The challenge is to be able to cluster consumers into these types of expected patterns, and other unexpected patterns, using only their energy usage history [@Ushakova2020-rl]. <!--To safeguard the customers' privacy, it is probable that such information is not accessible. Also, energy suppliers may not always update client information, such as property features, in a timely manner.--> There is a growing need to have methods that can examine the energy usage heterogeneity observed in smart meter data and what are some of the most common power consumption patterns.


<!-- _Related work_-->

There is an extensive body of literature focused on time series clustering related to smart meter data. @Tureczek2017-pb conducted a systematic study of over $2100$ peer-reviewed papers on smart meter data analytics. <!--None of the $34$ articles chosen for their emphasis use Australian smart meter data.--> The most often used algorithm is $k$-means [@Rhodes2014-it]. $k$-means can be made to perform better by explicitly incorporating time series features such as correlation or cyclic patterns rather than performing it on raw data. To reduce dimensionality, several studies use principal component analysis (PCA) or factor analysis to pre-process smart-meter data before clustering [@Ndiaye2011-pf]. PCA eliminates correlation patterns and decreases feature space, but loses interpretability. Other algorithms utilized in the literature include $k$-means variants, hierarchical clustering, and greedy $k$-medoids. Time series data, such as smart meter data, are not well-suited to any of the techniques mentioned in @Tureczek2017-pb. Only one study [@ozawa2016determining] identified time series characteristics by first conducting a Fourier transformation, to convert data from time to frequency domain, followed by $k$-means to cluster by greatest frequency. @Motlagh2019-yj suggested that the time feature extraction is limited by the type of noisy, patchy, and unequal time series common in residential customers and addresses model-based clustering by transforming the series into other objects such as structure or set of parameters which can be more easily characterized and clustered. @chicco2010renyi addressed information theory-based clustering such as Shannon or Renyi entropy and its variations. @Melnykov2013-sp discussed how outliers, noisy observations and scattered observations can complicate estimating mixture model parameters and hence the partitions. None of these methods focuses on exploring heterogeneity in repetitive patterns based on the dynamics of multiple temporal dependencies using probability distributions, which forms the basis of the methodology reported here.

<!-- Given none of the methods use probability distributions across cyclic granularity to explore heterogeneity in different repetitive behaviors, we present a way to do that by presenting similarity through probability distributions across cyclic granularities.  -->


This paper is organized as follows. Section \ref{sec:methodology} provides the clustering methodology. Section \ref{sec:validation} shows data designs to validate our methods. Section \ref{sec:application-gracsr} discusses the application of the method to a subset of the real data. Finally, we summarize our results and discuss possible future directions in Section \ref{sec:discussion-gracsr}.



<!-- A typical clustering technique includes the following steps: (a) establishing distance (dissimilarity) and similarity through feature or model extraction and selection; and (b) selecting the clustering algorithm design. Distance measures could be time-domain based or frequency-domain (fast Fourier transform). -->



<!-- A model-based clustering works by transforming the series into other other objects such as structure or set of parameters which can be more easily characterized and clustered [@Motlagh2019-yj]. [@chicco2010renyi] addresses information theory-based clustering such as Shannon or Renyi entropy and its variations. The essential temporal characteristics of the curves are defined or extracted using feature-based clustering. -->






<!-- This work -->
<!-- This is similar to a stochastic approach [@Motlagh2019-yj) to clustering, which proposes interpreting electricity demand as a random process and extracting time series characteristics, or a model of the series, to enable unsupervised clustering. Unsupervised clustering is only as good as the features that are extracted/selected or the distance metrics that were utilized. Well-designed additional features may collect characteristics that default features cannot. Based on the underlying structure of the temporal data, this article offers new distance metric and features for clustering and applies them to actual smart-meter data. Firstly, the distance metric is based on probability distribution, which in our knowledge is the first attempt to cluster smart meter data using probability distributions. These recorded time series are asynchronous, with varying time lengths for different houses and missing observations. Taking probability distributions helps to deal with such data, while helping with dimension reduction in one hand but not losing too much information due to aggregation. Secondly, we recognise that most clustering algorithms only provide hourly energy profiles during the day, but this approach provides a wider approach to the issue, seeking to group consumers with similar shapes over all important cyclic granularities. Since cyclic granularities are considered instead of linear granularities, clustering would group customers that have similar repetitive behavior across more than one cyclic granularities across which patterns are expected to be significant.  -->


<!-- common similarity measures -->

<!-- electricity data structure -->

<!-- common similarity measures used there -->


<!-- lit review -->




## Clustering methodology {#sec:methodology}


<!-- In contrast to models, a feature-based strategy is used to explicitly define or automatically extract the curves’ key time features, for instance by application of PCA on the daily curves [ --


<!-- Most papers discussed in Tureczek2017-pb fail to accept smart meter readings as time series data, a data type which contains a temporal component. The omission of the essential time series features in the analysis leads to the application of methods that are not designed for handling temporal components. K-Means ignores autocorrelation, unless the input data is pre-processed. The clusters identified in the papers are validated by a variety of indices, with the most prevalent -->
<!-- being the cluster dispersion index (CDI) [22–24], the Davies–Bouldin index (DBI) [25,26] and the mean index adequacy (MIA) [8,13]. -->


<!-- The data set solely contains readings from smart meters and no information about the consumers' specific physical, geographical, or behavioral attributes. As a result, no attempt is made to explain why consumption varies. Instead, this work investigates how much energy usage heterogeneity can be found in smart meter data and what some of the most common electricity use patterns are. -->

<!-- Because of this importance, countless approaches to estimate time series similarity have been proposed.  -->


<!--The foundation of our method is unsupervised clustering algorithms based exclusively on the time series data. The proposed methods leverage the intrinsic data structure hidden within cyclic temporal granularities.--> The existing work on clustering probability distributions assumes we have independent and identically distributed samples $f_1(v),\dots,f_n(v)$, where $f_i(v)$ denotes the distribution from observation $i$ over some random variable $v = \{v_t: t = 0, 1, 2, \dots, T-1\}$ observed across $T$ time points. <!--So going back to the smart meter example, $f_i(v)$ is the distribution of customer $i$ and $v$ is electricity demand.--> In our approach, instead of considering the probability distributions of the linear time series, we compare them across different categories of a cyclic granularity. We can consider categories of an individual cyclic granularity ($A$) or combination of categories for two interacting granularities ($A, B$) to have a distribution, where $A$ and $B$ are defined as $A = \{a_j: j=1, 2, \dots J\}$ and $B = \{b_k: k = 1, 2, \dots K\}$. For example, let us consider two cyclic granularities, $A$ and $B$, representing hour-of-day and day-of-week, respectively. Then $A = \{0, 1, 2, \dots, 23\}$ and $B = \{Mon, Tue, Wed, \dots, Sun\}$. In case individual granularities are considered, there are $J = 24$ distributions of the form $f_{i,j}(v)$ or $K = 7$ distributions of the form $f_{i,k}(v)$ for each customer $i$. In case of interaction, $J \times K=168$ distributions of the form $f_{i,j, k}(v)$ could be conceived for each customer $i$. Hence clustering these customers is equivalent to clustering these collections of conditional univariate probability distributions. Towards this goal, the next step is to decide how to measure distances between collections of univariate probability distributions. <!--There are multiple ways to measure similarities depending on the aim of the analysis. <!--This paper focuses on looking at the (dis) similarity between underlying distributions that may have resulted in different patterns across different cyclic temporal granularities, that eventually have resulted in the (dis) similarity between time series. --> Here, we describe two approaches for finding distances between time series. Both of these approaches may be useful in a practical context, and produce very different but equally useful customer groupings. The distances can be supplied to any usual clustering algorithm, including $k$-means or hierarchical clustering, to group observations into a smaller more homogeneous collection. <!--These clusters may be commonly associated with real-world data segmentation. However, since the data is unlabeled a priori, more information is required to corroborate this.--> The flow of the procedures is illustrated in Figure \ref{fig:flowchart} and is further described in the following subsections.


\begin{figure}

{\centering \includegraphics[width=1\linewidth]{img/flowchart} 

}

\caption{Flow chart illustrating the pipeline for our method for clustering time series.}(\#fig:flowchart)
\end{figure}

### Selecting granularities

@Gupta2021-hakear provide a distance measure ($\wpd$) for determining the significance of a cyclic granularity, and a ranking of multiple cyclic granularities. (This extends to harmonies, pairs of granularities that might interact with each other.) We define "significant" granularities as those with significant distributional differences across at least one category. The reason for subsetting granularities in this way is that clustering algorithms perform badly in the presence of nuisance variables. Granularities that do not have some difference between categories are likely to be nuisance variables. It should be noted that all of the time series in a collection may not have the same set of significant granularities.<!--This is the approach for managing this generate a subset ($S_c$) of significant granularities across a collection of time series:--> This is the approach for generating a subset ($S_c$) of significant granularities across a collection of time series:

 (a) Remove granularities from the comprehensive list that are not significant for any time series.

 (b) Select only the granularities that are significant for the majority of time series.

<!--There will be time series in both cases where one or a few selected granularities are boring. Even in that case, having this group of observations with no interesting patterns at a granularity that regularly discovers patterns may be valuable. If, on the other hand, the granularities under examination are truly important for a group of data, unique patterns may be identified while clustering them.-->



### Data transformation

The shape and scale of the distribution of the measured variable (e.g. energy usage) affects distance calculations. Skewed distributions need to be symmetrized. Scales of individuals need to be standardized, because clustering is to select similar patterns, not magnitude of usage. (Organizing individuals based on magnitude can be achieved simply by sorting on a statistic like the average value across time.) <!--Observations often have a somewhat skewed time series distribution and their ranges might vary greatly. Statistical transformations are employed to bring all of them to the same range or normalize each observation. This is important because we are not interested in trivial clusters that vary in magnitudes, but rather in uncovering comparable patterns of distributional differences between categories.--> For the JS-based approaches, two data transformation techniques are recommended, normal-quantile transform (NQT) and robust scaling (RS).<!-- NQT is used in @Gupta2021-hakear prior sorting granularities.-- $\wpd$, which is the foundation of wpd-based distances. XX wpd was not yet introduced--> While @Gupta2021-hakear already use NQT when computing $\wpd$, it could be useful to standardize it for the selected set of significant granularities prior to computing the distances.
    
- RS: The normalized $i^{th}$ observation is denoted by $v_{norm} = \frac{v_t - q_{0.5}}{q_{0.75}-q_{0.25}}$, where $v_t$ is the actual value at the $t^{th}$ time point and $q_{0.25}$, $q_{0.5}$ and $q_{0.75}$ are the $25^{th}$, $50^{th}$ and $75^{th}$ percentiles of the time series for the $i^{th}$ observation. Note that $v_{norm}$ has zero mean and median, but otherwise the shape does not change.

- NQT: The raw data for all observations is individually transformed [@Krzysztofowicz1997-bv], so that the transformed data follows a standard normal distribution. NQT will symmetrize skewed distributions. <!--As a result, determining which raw distribution was used is difficult using the modified distribution.--> A drawback is that any multimodality will be concealed. This should be checked prior to applying NQT. 

<!-- NQT, however, is an useful transformation which often improves the clustering performance. Since our variables are cyclic granularities which are derived from linear granularities, NQT ensures that the conditional distribution across each variable is also normalised. -->


<!-- is not a problem for implementation of this methodology as distributions are characterized by quantiles and the order of the quantiles is reserved under NQT. -->


<!-- First, the original data is ranked in ascending order and the probabilities $P(Y<=y(i)) = i/(n+1)$ are attached to $y(i)$, in terms of their ranking order. A NQT based transformation is applied by computing from a standard normal distribution a variable $\eta(i)$, which corresponds to the same probability $P(\eta< \eta(i)) = i/n+1$.By doing this, the new variables $\eta(i)$ will be marginally distributed according to standard Normal, N(0,1). -->



<!-- It is worth noting that when studying these similarities, a variety of objectives may be pursued. One objective could be to group time series with similar shapes over all relevant cyclic granularities. In this scenario, the variation in customers within each group is in magnitude rather than shape, while the variation between groups is only in shape. There -->
<!-- are distance measures are used for shape-based clustering [Ding et al. 2008; Wang -->
<!-- et al. 2013] and many more but none of them look at the probability distributions while computing similarity. Moreover, most distance measures offer similar shape across just one dimension. For example, we often see "similar" daily energy profiles across hours of the day, but we suggest a broader approach to the problem, aiming to group consumers with similar distributional shape across all significant cyclic granularities. Another purpose of clustering could be to group customers that have similar differences in patterns across all major cyclic granularities, capturing similar jumps across categories regardless of the overall shape. For example, in the first goal, similar shapes across hours of the day will be grouped together, resulting in customers with similar behavior across all hours of the day, whereas in the second goal, any similar big-enough jumps across hours of the day will be clubbed together, regardless of which hour of the day it is. Both of these objectives may be useful in a practical context and, depending on the data set, may or may not propose the same customer classification. Depending on the goal of clustering, the distance metric for defining similarity would be different. These distance metrics could be fed into a clustering algorithm to break large data sets into subgroups that can then be analyzed separately. These clusters may be commonly associated with real-world data segmentation. However, since the data is unlabeled a priori, more information is required to corroborate this. This section presents the work flow of the methodology:  -->

### Data pre-preprocessing

Computationally in R, the data is assumed to be a "tsibble object" [@wang2020tsibble] equipped with an index variable representing inherent ordering from past to present and a key variable that defines observational units over time. The measured variable for an observation is a time-indexed sequence of values. This sequence, however, could be shown in several ways. A shuffle of the raw sequence may represent hourly consumption throughout a day, a week, or a year. Cyclic granularities can be expressed in terms of the index set in the tsibble data structure.

The data object will change when cyclic granularities are computed, as multiple observations will be categorized into levels of the granularity, thus inducing multiple probability distributions. Directly computing Jensen-Shannon distances between the entire probability distributions can be computationally intensive. Thus it is recommended that quantiles are used to characterize the probability distributions. In the final data object, each category of a cyclic granularity corresponds to a list of numbers which is composed of a few quantiles.


### Distance metrics

<!-- Considering each individual or combined categories of cyclic granularities to have an underlying distribution lead to a collection of conditional distributions for each customer $i$. -->


The total (dis) similarity between each pair of customers is obtained by combining the distances between the collections of conditional distributions. This needs to be done in a way such that the resulting metric is a distance metric, and could be fed into the clustering algorithm. Two types of distance metrics are considered:

<!-- The choice of distance measures is a critical step in clustering. It defines how the similarity of two elements (x, y) is calculated. -->

#### JS-based distances

This distance metric considers two time series to be similar if the distributions of each category of an individual cyclic granularity or combination of categories for interacting cyclic granularities are similar. In this study, the distribution for each category is characterized using deciles (can potentially consider any list of quantiles), and the distances between distributions are calculated using the Jensen-Shannon distances [@Menendez1997-in], which are symmetric and thus could be used as a distance measure.


The sum of the distances between two observations $x$ and $y$ in terms of a cyclic granularity $A$ is defined as $$S^A_{x,y} = \sum_{j\in A} D(x_j,y_j)$$ where $D$ is the Jensen-Shannon distances, $x_j$ is the set of quantiles over the values filtered by $j^{th}$ level of granularity $A$ for observation $x$ (similar for $y$).

The sum of the distances between two observations $x$ and $y$ in terms of a pair of cyclic granularities $(A, B)$ is defined as $$S^{A,B}_{x,y} = \sum_{(j, k) \in (A, B)} D(x_{jk},y_{jk})$$ $x_{jk}$ is the set of quantiles over the values filtered by the combination of $j^{th}$ level of granularity $A$ and $k^{th}$ level of granularity $B$ for observation $x$ (similar for $y$). 

After determining the distance between two series in terms of one granularity, we must combine them to produce a distance based on all significant granularities. When combining distances from individual $L$ cyclic granularities $C_l$ with $n_l$ levels, $$S_{x, y} = \sum_{l \in L}S^{C_l}_{x,y}/n_l$$ is employed, which is also a distance metric since it is the sum of JS distances. This approach is expected to yield groups, such that the variation in observations within each group is in magnitude rather than distributional pattern, while the variation between groups is only in distributional pattern across categories.



#### wpd-based distances

We compute weighted pairwise distances $\wpd$ [@Gupta2021-hakear] for all considered granularities for all observations. $\wpd$ is designed to capture the maximum variation in the measured variable explained by an individual cyclic granularity or their interaction. It is estimated by the maximum pairwise distances between distributions across consecutive categories normalized by appropriate parameters. A higher value of $\wpd$ indicates that some interesting patterns are expected, whereas a lower value would indicate otherwise.

Once we have chosen $\wpd$ as a relevant feature for characterizing the distributions across one cyclic granularity, we have to decide how we combine differences between the multiple features (corresponding to multiple granularities) into a single number. The Euclidean distance between them is chosen, with the granularities acting as variables and $\wpd$ representing the value under each variable. With this approach, we should expect the observations with similar $\wpd$ values to be clustered together. Thus, this approach is useful for grouping observations that have a similar significance of patterns across different granularities. Similar significance does not imply a similar pattern, which is where this technique varies from JS-based distances, which detect differences in patterns across categories.





<!-- Consider a harmony table consisting of many harmonies, each of the form $(A, B)$, such that $A = \{ a_j: j = 1, 2, \dots, J\}$ and $B = \{ b_k: k = 1, 2, \dots, K\}$. Each household consists of a $J*K$ distributions one harmony. We compute the distributional difference -->
<!-- between $(A, B)$ for the $s^{th}$ household using $wpd_{{s}}(A,B)$. -->
<!-- $wpd_{{s}}(A,B)$ denotes the normalized weighted-pairwise distributional distances between $(A, B)$ and is a feature which measures distributional difference between harmonies. If we have $H_{N_C}$ harmonies in the harmony table, then for each household we have a vector of $wpd_{{s}}$ of $H_{N_C}$ elements with each element corresponding to one harmony. We aim to have pockets of customers showing similar periodic behavior by considering $\wpd$ values for different harmonies and some time series features. The features should also characterize probability distributions of different household. -->


<!-- grouping probability distributions across a harmony. This clustering algorithm is adopted to remove or appropriately adjust for auto correlation and unequal length in the data. The method could be further extended by clustering probability distributions conditional on one or more cyclic granularities. The following are some of the advantages of our proposed method. -->




### Clustering

#### Number of clusters

Determining the number of clusters is typically a difficult task. Many metrics have been defined for choosing clusters. Most metrics for choosing the optimal number of clusters are based on comparing distances between observations within a class to those distances between observations between classes, which makes the assumption that there are some separated clusters. Some common procedures include the gap statistic [@tibshirani2001estimating], average silhouette width [@rousseeuw1987silhouettes], Dunn index [@Dunn1973-gk] and the separation index (*sindex*) [@Hennig2019-eq; @Hennig2014-ah]. These are constructed by balancing within-cluster homogeneity and between-cluster separation. 


All of the common approaches can give contradictory suggestions for the optimal number of clusters, particularly when the data does not naturally break into groups, or in the presence of nuisance variables (no contribution to clustering) or nuisance observations (inlying and outlying observations falling between clusters). There is no one best metric, which is perhaps a reason why so many metrics exist. 

In this work, we have chosen to use *sindex*. It is a very simple but effective metric. This is computed by averaging the smallest 10% of inter-cluster distances. It is relatively robust to nuisance observations. The value of *sindex* always decreases, and sharp drops in value indicate candidates for the optimal number of clusters. The number of clusters corresponding to the value **before the drop** is the recommendation. 

<!--determined by calculating the distance between each observation and the cluster to which it does not belong. Then, the least 10% of these distances are averaged to only evaluate places near the cluster edges.-->


#### Algorithm

With a way to obtain pairwise distances, any clustering algorithm can be employed that supports the given distance metric as input. A good comprehensive list of algorithms can be found in @Xu2015-ja based on traditional ways like partition, hierarchy, or more recent approaches like distribution, density, and others. We employ agglomerative hierarchical clustering in conjunction with Ward's linkage. Hierarchical cluster techniques fuse neighboring points sequentially to form bigger clusters, beginning with a full pairwise distance matrix. The distance between clusters is described using a "linkage technique". This agglomerative approach successively merges the pair of clusters with the shortest between-cluster distance using Ward's linkage method.

<!--Hierarchical algorithms are one of the most widely used, can operate with data of any shape, has reasonable scalability, and the number of clusters is not needed as a parameter.-->


#### Characterization of clusters

Cluster characterization is an important final stage of a cluster analysis. The primary purpose is to compare the homogeneity within a cluster to the heterogeneity of clusters. This can be done numerically, by tabulating cluster means and standard deviations [@dasu2005grouping], and visually using methods for graphics multivariate data. @Cook2007-qe provide visual examples using both tours [@asimov1985grand] and parallel coordinate plots [@wegman1990hyperdimensional]. Dimension reduction techniques like principal component analysis [@jolliffe2016principal], multidimensional scaling (MDS) [@borg2005modern], t-distributed stochastic neighbor embedding (t-SNE) [@van2008visualizing] and linear discriminant analysis (LDA) [@fisher1936use] are also useful. <!-- A Parallel Coordinates Plot features parallel axes for each variable and each axis is linked by lines. Changing the axes may reveal patterns or relationships between variables for categorical variables. However, for categories with cyclic temporal granularities, preserving the underlying ordering of time is more desirable. Displaying cluster statistics is useful when we have larger problems and it is difficult to read the parallel coordinate plots due to congestion. All of MDS, PCA and t-SNE use a distance or dissimilarity matrix to construct a reduced-dimension space representation, but their goals are diverse. Multidimensional scaling [@borg2005modern) seeks to maintain the distances between pairs of data points, with an emphasis on pairings of distant points in the original space. The t-SNE embedding will compress data points that are close in high-dimensional space. Tour is a collection of interpolated linear projections of multivariate data into lower-dimensional space. The cluster characterization approach varies depending on the distance metric used. Parallel coordinate plots, scatter plot matrices, MDS or PCA are potentially useful ways to characterize clusters using wpd-based distances. For JS-based distances, plotting cluster statistics is beneficial for characterization and variable importance could be displayed through parallel coordinate plots. -->



<!-- CUT IT SHORT -->

<!-- (a) _Parallel coordinate plots_ [@wegman1990hyperdimensional) are often used to visualize high-dimensional and multivariate data, allowing visual grouping and pattern detection. A Parallel Coordinates Plot features parallel axes for each variable. Each axis is linked by lines. Changing the axes may reveal patterns or relationships between variables for categorical variables. However, for categories with cyclic temporal granularities, preserving the underlying ordering of time is more desirable. -->

<!-- (b) _Scatterplot matrix_ contains pairwise scatter plots of the p variables. Pairwise scatter plots are useful for figuring out how variables relate to each other and how factors determine the clustering. -->

<!-- (c) _Displaying cluster statistics_ are useful when we have larger problems and it is difficult to read the Parallel coordinate plots due to congestion. [@dasu2005grouping) -->

<!-- (d) _MDS, PCA and t-SNE_ While all of them use a distance or dissimilarity matrix to construct a reduced-dimension space representation, their goals are diverse. PCA seeks to retain data variance. Multidimensional scaling [@borg2005modern) seeks to maintain the distances between pairs of data points, with an emphasis on pairings of distant points in the original space. t-SNE, on the other hand, is concerned with preserving neighborhood data points. The t-SNE embedding will compress data points which are close in high-dimensional space. -->

<!-- (e) _Tour_ is a collection of interpolated linear projections of multivariate data into lower-dimensional space. As a result, the viewer may observe the high-dimensional data's shadows from a low-dimensional perspective. -->

<!-- The cluster characterization approach varies depending on the distance metric used. Parallel coordinate plots, scatter plot matrices, MDS or PCA are potentially useful ways to characterize clusters using wpd-based distances. For JS-based distances, plotting cluster statistics is beneficial for characterization and variable importance could be displayed through parallel coordinate plots. -->


<!-- This part of the work uses R packages `GGally` [@R-GGally), `Rtsne` [@R-tsne), `ggplot2` (Wickham2009pk), `tour` [@wickham2011tourr), `stats` [@R-language). -->


<!-- - A random sample of the original data is taken for clustering analysis and includes missing and noisy observations (detailed description in Appendix) -->

<!-- - All harmonies are computed for each customer in the sample. -->
<!-- Cyclic granularities which are clashes for all customers in the sample are removed. -->

<!-- - It is worth noting that a number of other solutions may be considered at the pre-processing stage of the method. We have considered a) Normal-Quantile Transform and b) Robust transformation. -->

<!-- - Two methods are considered for computing dissimilarity between two customers. The first one involves computing according to one granularity is computed as the sum of the JS distances between distribution of all the categories of the granularity. When we consider more than one granularity, we consider the sum of the average distances for all the granularity so that the combined metric is also a distance. -->

<!-- - Given the scale of dissimilarity among the energy readings, the model chooses optimal number of clusters -->

<!-- - Once clusters have been allocated, the groups are explored visually. -->

<!-- - Results are reported and compared. -->

<!-- Two methods are used for computing distances between subjects and then hierarchical clustering algorithm is used. -->

<!-- The existing work on clustering probability distributions assumes we have an iid sample $f_1(v),\dots,f_n(v)$, where $f_i(v)$ denotes the probability distribution from observation $i$ over some random variable $v = \{v_t: t = 0, 1, 2, \dots, T-1\}$ observed across $T$ time points. In our work, we are using $i$ as denoting a customer and the underlying variable as the electricity demand. So $f_i(v)$ is the distribution of household $i$ and $v$ is electricity demand. -->

<!-- We want to cluster distributions of the form $f_{i,j,k}(v)$, where $i$ and $j$ denote -->

<!-- Consider a harmony table consisting of many harmonies, each of the form $(A, B)$, such that $A = \{ a_j: j = 1, 2, \dots, J\}$ and $B = \{ b_k: k = 1, 2, \dots, K\}$. Each household consists of a $J*K$ distributions one harmony. We compute the distributional difference -->
<!-- between $(A, B)$ for the $s^{th}$ household using $wpd_{{s}}(A,B)$. -->
<!-- $wpd_{{s}}(A,B)$ denotes the normalized weighted-pairwise distributional distances between $(A, B)$ and is a feature which measures distributional difference between harmonies. If we have $H_{N_C}$ harmonies in the harmony table, then for each household we have a vector of $wpd_{{s}}$ of $H_{N_C}$ elements with each element corresponding to one harmony. We aim to have pockets of customers showing similar periodic behavior by considering $\wpd$ values for different harmonies and some time series features. The features should also characterize probability distributions of different household. -->


<!-- ### Notations -->

<!-- Consider an iid sample $f_1(v),\dots,f_n(v)$, where $f_i(v)$ denotes the probability distribution from observation $i$ over some random variable $v = \{v_t: t = 0, 1, 2, \dots, T-1\}$ observed across $T$ time points. In our work, we are using $i$ as denoting a household and the underlying variable as the electricity demand. Further consider a cyclic granularity of the form $B = \{ b_k: k = 1, 2, \dots, K\}$. Each customer consists of collection of probability distributions. -->


<!-- So $f_i(v)$ is the distribution of household $i$ and $v$ is electricity demand. We want to cluster distributions of the form $f_{i,j,k}(v)$, where $i$ and $j$ denote $i^{th}$ and $j^{th}$ customer respectively. -->



<!-- a harmony table consisting of many harmonies, each of the form $(A, B)$, such that $A = \{ a_j: j = 1, 2, \dots, J\}$ and $B = \{ b_k: k = 1, 2, \dots, K\}$. -->




<!-- ### A single or pair of granularities together (change names) -->

<!-- The methodology can be summarized in the following steps: -->

<!-- - _Pre-processing step_ -->

<!-- Robust scaling method or NQT used for each customer. -->


<!-- - _NQT_ -->


<!-- - _Treatment to outliers_ -->






<!-- ### Many granularities together (change names) -->

<!-- The methodology can be summarized in the following steps: -->


<!-- 1. Compute quantiles of distributions across each category of the cyclic granularity -->
<!-- 2. Compute JS distance between customers for each each category of the cyclic granularity -->
<!-- 3. Total distance between customers computed as sum of JS distances for all hours -->
<!-- 4. Cluster using this distance with hierarchical clustering algorithm (method "Ward.D") -->

<!-- _Pro:_  -->
<!-- - distance metric makes sense to group different shapes together  -->
<!-- - simulation results look great on typical designs  -->
<!-- _Cons:_   -->
<!-- - Can only take one granularity at once   -->
<!-- - Clustering a big blob of points together whereas the aim is to groups these big blob into smaller ones   -->

<!-- ### Multiple-granularities -->

<!-- _Description:_   -->

<!-- Choose all significant granularities and compute wpd for all these granularities for all customers. Distance between customers is taken as the euclidean distances between them with the granularities being the variables and wpd being the value under each variable for which Euclidean distance needs to be measured.   -->
<!-- _Pro:_   -->
<!-- - Can only take many granularities at once -->
<!-- - can apply variable selection PCP and other interesting clustering techniques -->
<!-- - simulation results look great on typical designs -->
<!-- - splitting the data into similar sized groups   -->
<!-- _Cons:_   -->
<!-- - distance metric does not make sense to split the data into similar shaped clusters  -->

## Validation {#sec:validation}

To validate our clustering methods, we have created several different data designs containing different granularity features. <!--to see where one method works better than the other and where they might give us the same outcome --or the effect of missing data <!---and trends on the proposed methods.--> There are three circular granularities $g1$, $g2$ and $g3$ with categories denoted by $\{g10,g11\}$, $\{g20, g21, g22\}$ and $\{g30, g31, g32, g33, g34\}$ and levels $n_{g_1}=2$, $n_{g_2}=3$ and $n_{g_3}=5$. These categories could be integers or some more meaningful labels. For example, the granularity "day-of-week" could be either represented by $\{0, 1, 2, \dots, 6\}$ or $\{Mon, Tue, \dots, Sun\}$. Here categories of $g1$, $g2$ and $g3$ are represented by $\{0, 1\}$, $\{0, 1, 2\}$ and $\{0, 1, 2, 3, 4\}$ respectively. A continuous measured variable $v$ of length $T$ indexed by $\{0, 1, \dots T-1\}$ is simulated such that it follows the structure across $g1$, $g2$ and $g3$. We constructed independent replications of all data designs $R = \{25, 250, 500\}$ to investigate if our proposed clustering method can discover distinct designs in small, medium, and big numbers of series. All designs employ $T=\{300, 1000, 5000\}$ sample sizes to evaluate small, medium, and large-sized series. Variations in method performance may be due to different jumps between categories. So a mean difference of $\mu = \{1, 2, 5\}$ between categories is considered. The performance of the approaches varies with the number of granularities which has interesting patterns across its categories. So three scenarios are considered to accommodate that. Table \ref{tab:range-parameter} shows the range of parameters considered for each scenario.


<!-- ```{r range-parameter, fig.cap="The range of parameters used for the validation study, for the three different scenarios, number of simulations for each design, differences between means across granularities and series lengths. "} -->
<!-- knitr::include_graphics("img/simulation_table.png") -->
<!-- ``` -->


\begin{table}

\caption{(\#tab:range-parameter)The range of parameters used for the validation study, for the three different scenarios, number of simulations ($R$) for each design, differences between means ($\mu$) across granularities and series lengths ($T$).}
\centering
\begin{tabular}[t]{l|r|l|l|l}
\hline
scenario & designs & R & $\mu$ & T\\
\hline
S1 & 5 &  &  & \\
\cline{1-2}
S2 & 4 &  &  & \\
\cline{1-2}
S3 & 4 & \multirow{-3}{*}{\raggedright\arraybackslash 25, 250, 500} & \multirow{-3}{*}{\raggedright\arraybackslash 1, 2, 5} & \multirow{-3}{*}{\raggedright\arraybackslash 300, 1000, 5000}\\
\hline
\end{tabular}
\end{table}





<!-- The results for $T=300$ and $R=25$ is shown, that means we have $25$ time series each with length $300$. The rest of the results could be found in the supplementary paper. -->


### Data generation

<!-- An ARMA (p,q) process is used to generate series, where $p$ and $q$ are selected at random such that the series is stationary. The various designs on $g1$, $g2$, and $g3$ are introduced by adding matching designs to this series' innovations. The innovations are considered to have a normal distribution, although they follow the same pattern as the designs. To eliminate the effect of starting values, the first 500 observations in each series are discarded. -->

Each category or combination of categories from $g1$, $g2$ and $g3$ are assumed to come from the same distribution, a subset of them from the same distribution, a subset of them from separate distributions, or all from different distributions, resulting in various data designs. As the methods ignore the linear progression of time, there is little value in adding time dependency to the data generating process. The data type is set to be "continuous," and the setup is assumed to be Gaussian. When the distribution of a granularity is "fixed", it means distributions across categories do not vary and are considered to be from N (0,1). $\mu$ alters in the "varying" designs, leading to varying distributions across categories.


<!-- An ARMA (p,q) process is used to generate series, where $p$ and $q$ are selected at random such that the series is stationary. The various designs on $g1$, $g2$, and $g3$ are introduced by adding matching designs to this series' innovations. The innovations are considered to have a normal distribution, although they follow the same pattern as the designs. To eliminate the effect of starting values, the first 500 observations in each series are discarded. -->

<!-- It is often reasonable to construct a time series using properties such as trend, seasonality, and auto-correlation. However, when examining distributions across categories of cyclic granularities, these time series features are lost or addressed independently by considering seasonal fluctuations through cyclic granularities. Because the time span during which an entity is observed in order to ascertain its behavior is not very long, -->
<!-- the behavior of the entity will not change drastically and hence the time series can be assumed to remain stationary throughout the observation period. If the observation period is very long (for e.g more than 3 years), property, physical or geographical attributes might change leading to a non-stationary time series. But such a scenario is not considered here and the resulting clusters are assumed to be time invariant in the observation period.  -->


### Data designs

#### Individual granularities

**Scenario 1 (S1) - All granularities significant:**
<!-- Consider a case where all the three granularities $g1$, $g2$ and $g3$ would be responsible for making the designs distinct. That would mean, the pattern for each of $g1$, $g2$ and $g3$ will change for at least one design. We consider a situation with all the null cases corresponding to no difference in distribution across categories, that is, all categories follow the same distribution N(0,1). -->
Consider the instance where $g1$, $g2$, and $g3$ all contribute to design distinction. This means that each granularity will have significantly different patterns at least across one of the designs to be clustered. In Table \ref{tab:tab-dist-design} various distributions across categories are considered (top) which lead to different designs (bottom). Figure \ref{fig:plot-3gran-new} shows the simulated variable's linear (left) and cyclic (right) representations for each of these five designs. The structural difference in the time series variable is impossible to discern from the linear view, with all of them looking very similar. The shift in structure may be seen clearly in the distribution of cyclic granularities. The following scenarios use solely graphical displays across cyclic granularities to highlight distributional differences in categories.

**Scenario 2 (S2) - Few significant granularities:**
This is the case where one granularity will remain the same across all designs. We consider the case where the distribution of $v$ varies across $g2$ levels for all designs, across $g3$ levels for a few designs, and $g1$ does not vary across designs. The proposed design is shown in Figure \ref{fig:gran2and1-clubbed}(right).

**Scenario 3 (S3) - Only one significant granularity:**
Only one granularity is responsible for identifying the designs in this case. This is depicted in Figure \ref{fig:gran2and1-clubbed} (right) where only $g3$ affects the designs significantly.







\begin{table}[!h]
\caption{(\#tab:tab-dist-design)For S1, distributions of different categories when they vary (displayed on top). If distributions are fixed, they are set to N(0, 1). The various distributions across categories result in five designs (displayed below).}

\centering
\begin{tabular}[t]{ll}
\toprule
granularity & Varying distributions\\
\midrule
g1 & g10 \textasciitilde{} N(0, 1), g11 \textasciitilde{} N(2, 1)\\
g2 & g21 \textasciitilde{} N(2, 1), g22 \textasciitilde{} N(1, 1), g23 \textasciitilde{} N(0, 1)\\
g3 & g31 \textasciitilde{} N(0, 1), g32 \textasciitilde{} N(1, 1), g33 \textasciitilde{} N(2, 1),
                      g34 \textasciitilde{} N(1, 1), g35 \textasciitilde{} N(0, 1)\\
\bottomrule
\end{tabular}
\centering
\begin{tabular}[t]{llll}
\toprule
design & g1 & g2 & g3\\
\midrule
design-1 & fixed & fixed & fixed\\
design-2 & vary & fixed & fixed\\
design-3 & fixed & vary & fixed\\
design-4 & fixed & fixed & vary\\
design-5 & vary & vary & vary\\
\bottomrule
\end{tabular}
\end{table}









\begin{figure}

{\centering \includegraphics[width=1\linewidth]{figure/plot-3gran-new-1} 

}

\caption{The linear (left) and cyclic (right) representation is shown under scenario S1 using line plots and boxplots respectively. Each row represents a design. Distributions of categories across $g1$, $g2$ and $g3$ change across at least one design as can be observed in the cyclic representation. It is not possible to comprehend these structural differences in patterns just by looking at or considering the linear representation.}(\#fig:plot-3gran-new)
\end{figure}

















\begin{figure}

{\centering \includegraphics[width=\textwidth]{figure/gran2and1-clubbed-1} 

}

\caption{ Boxplots showing distributions of categories across different designs (rows) and granularities (columns) for scenarios S2 and S3. In S2, $g2$, $g3$ change across at least one design but $g1$ remains constant. Only $g3$ changes across different designs in S3.}(\#fig:gran2and1-clubbed)
\end{figure}

#### Interaction of granularities

The proposed methods could be extended when two granularities of interest interact and we want to group subjects based on the interaction of the two granularities. Consider a group that has a different weekday and weekend behavior in the summer but not in the winter. This type of combined behavior across granularities can be discovered by evaluating the distribution across combinations of categories for different interacting granularities (weekend/weekday and month-of-year in this example). As a result, in this scenario, we analyze a combination of categories generated from different distributions. Display of design and related results can be found in supplementary material.




<!-- When two granularities of interest interact, the connection between a granularity and the measured variable is determined by the value of the other interacting granularity. This happens when the effects of the two granularities on the measured variable are not additive. For simplicity, consider a case with just two interacting granularities $g1$ and $g2$ of interest. As opposed to the last case, where we could play with the distribution of $5$ individual categories, with interaction we can play with the distribution of $6$ combination of categories. Consider $4$ designs in Figure \ref{fig:} where different distributions are assumed for different designs to get some distinction across designs. For example, in application, think about the scenario when customers need to grouped basis their joint behavior across hour-of-day and month-of-year. -->

<!-- | Granularity type                                                   	| # Significant 	| # Replications 	| -->
<!-- |--------------------------------------------------------------------	|---------------	|----------------	| -->
<!-- | **Individual**  <br><br># obs: 300, 500, 2000  <br># clusters: 4/5 	| 1/2/3         	| 25, 100, 200   	| -->
<!-- | **Interaction**  <br><br># obs: 500, 2000  <br># clusters: 4       	| 2           	| 25, 100, 200   	| -->


### Visual exploration of results

All of the approaches were fitted to each data design and to each combination of the considered parameters. The formed clusters have to match the design, be well separated, and have minimal intra-cluster variation. <!--It is possible to study these desired clustering traits visually in a more comprehensive way than just looking at index values.--> MDS and parallel coordinate graphs are used to demonstrate the findings, as well as an index value plot to provide direction on the number of clusters. JS-based approaches corresponding to NQT and RS are referred to as JS-NQT and JS-RS respectively. In the following plots, results for JS-NQT are reported, and results with JS-RS or wpd-based distances are in the supplementary material.

Figure \ref{fig:sindex-plot-validation} shows *sindex* plotted against the number of clusters ($k$) for the range of mean differences (rows) under the different scenarios (columns). This can be used to determine the number of clusters for each scenario. When *sindex* for each scenario are examined, it appears that $k = \{5, 4, 4\}$ is justified for scenarios S1, S2, and S3, respectively, given the sharp decrease in *sindex* from that value of $k$. Thus, the number of clusters corresponds to the number of designs that were originally considered in each scenario.

Figure \ref{fig:mds-plot-validation} shows separation of our clusters. It can be observed that in all scenarios and for different mean differences, clusters are separated. However, the separation increases with an increase in mean differences across scenarios. This is intuitive because, as the difference between categories increases, it gets easier for the methods to correctly distinguish the designs.

Figure \ref{fig:parcoord-sim} depicts a parallel coordinate plot with the vertical bar showing total inter-cluster distances with regard to granularities $g1$, $g2$, and $g3$ for all simulation settings and scenarios. So one line in the figure shows the inter-cluster distances for one simulation setting and scenarios vary across facets. The lines are not colored by group since the purpose is to highlight the contribution of the factors to categorization rather than class separation. Panel S1 shows that no variable stands out in the clustering, but the following two panels show that {g1} and {g1, g2} have very low inter-cluster distances, meaning that they did not contribute to the clustering. It is worth noting that these facts correspond to our original assumptions when developing the scenarios, which incorporate distributional differences over three (S1), two (S2), and one (S3) significant granularities. Hence, Figure \ref{fig:parcoord-sim} (S1), (S2), and (S3) validate the construction of scenarios (S1), (S2), and (S3) respectively.

The JS-RS and wpd-based methods perform worse for $nT=300$, then improve for higher $nT$ evaluated in the study. However, a complete year of data is the minimum requirement to capture distributional differences in winter and summer profiles, for example. Even if the data is only available for a month, $nT$ with half-hourly data is expected to be at least $1000$. As a result, as long as the performance is promising for higher $nT$, this is not a challenge.

In our study sample, the method JS-NQT outperforms the method JS-RS for smaller differences between categories. More testing, however, would be needed to to be confident in this conclusion.





\begin{figure}

{\centering \includegraphics[width=\textwidth]{figure/sindex-plot-validation-1} 

}

\caption{Choosing optimal cluster number across the range of scenarios and mean differences used in the validation study, using the cluster separation index (sindex) for the JS-NQT. S1 has a sharp decrease in sindex from 5 to 6, whereas S2 and S3 have a decrease from 4 to 5, especially when mean difference is large, providing the recommended number of clusters to be 5, 4, 4, respectively. This precisely reflects the structure in designs that we would hope the clustering could recover.}(\#fig:sindex-plot-validation)
\end{figure}





\begin{figure}

{\centering \includegraphics[width=\textwidth]{figure/mds-plot-validation-1} 

}

\caption{MDS summary plots to illustrate the cluster separation for the range of mean differences (rows) under the different scenarios (columns). It can be observed that clusters become more compact and separated for higher mean differences between categories across all scenarios. Between scenarios, separation is least prominent corresponding to Scenario (S3) where only one granularity is responsible for distinguishing the clusters.}(\#fig:mds-plot-validation)
\end{figure}


\begin{figure}

{\centering \includegraphics[width=\textwidth]{figure/parcoord-sim-1} 

}

\caption{Exploring the contribution of granularities in the clustering for scenarios S1, S2, S3, using parallel coordinate plots. Inter-cluster distances are displayed vertically. All three granularities $g1$, $g2$, and $g3$ have high inter-cluster distances for S1, suggesting all are important. In S2 $g1$ and in S3 both $g1$ and $g2$ have smaller inter-cluster distances, indicating that they did not contribute to clustering.}(\#fig:parcoord-sim)
\end{figure}






<!-- "Parallel coordinate plots are used to identify key variables for classification. Each line in this figure represents a formed group for a certain set of simulated parameters. (a), (b) and (c) corresponds to Scenarios (a), (b) and (c). The cyclic granularities $g1$, $g2$, and $g3$ are plotted on the x-axis, while total inter-cluster distances are plotted on the y-axis. Each line in this figure represents a group for a certain configuration of the various parameters ($nT$, $diff$, $R$). (c) demonstrates that intercluster distances are only significant for the variable $g3$, meaning that the clusters formed are primarily due to $g3$. This corresponds to our design in Scenario (S3), in which distributional differences were implemented solely for g3. (b) demonstrates the role of g2 and g3 in clustering. (a) depicts a heterogeneous pattern of inter-cluster distances across variables, indicating that no single variable is to account for the clusters." -->

<!-- A confusion table can come alive with linked brushing, so that mismatches and agreements between methods can be explored. -->





<!-- starts getting better with increasing difference and get worse with increasing number of replications. Length of series do not show to have any effect on the performance of the methods. It does not depend on if time series is ar or arma. -->


<!-- - confusion matrix could be used for showing results if proper labeling is used -->

<!-- - write about features that we have spiked into the data set -->
<!-- - write about you incorporated noise -->
<!-- - What is the additional structure you can incorporate that will lead to failing of method1 and method2? -->
<!-- - And both gives the same result? Basically say when method 1 works better than method 2 and vice versa! -->

<!-- - -->

## Application {#sec:application-gracsr}

Clustering with the new distances is illustrated on the smart meter energy usage for a sample of customers from @smart-meter. The full data contains half-hourly general supply in kWh for 13,735 customers, resulting in 344,518,791 observations in total.<!--In most cases, electricity data is expected to have multiple seasonal patterns like daily, weekly or annual. We do not learn about these repetitive behaviors from the linear view because too many measurements all squeezed in that representation. Hence we transition into looking at cyclic granularities, that can potentially provide more insight on their repetitive behavior.--> The raw data for these consumers is of unequal length, with varying starting and end dates. Additionally, there were missing values in many series. (The supplementary material contains details from checking for systematic missingness.) Because our proposed methods evaluate probability distributions rather than raw data, these data issues are not problematic, unless there is any systematic structure related to granularities.

Huge data sets present more complications for clustering. Clustering algorithms work well when there are well-separated clusters, with no nuisance variables or nuisance observations. When converting a series to granularities, many variables (each level of a granularity) are generated, possibly creating a slew of nuisance variables. Some customers may have a mix of energy use patterns, which could be considered nuisance observations located between major clusters. For this reason, we have chosen to select a small group of customers with relatively distinct and different patterns in order to illustrate the clustering more simply. Figure \ref{fig:hod-ind-group-png} shows the distribution across *hod*, *moy* and *wnwd* for the set of $24$ customers used to illustrate clustering. The customers are displayed in two columns of $12$ for space reasons. Each row, of each column, represents the profile of a single customer across different variables. Each customer is associated with an identifier of the form [$a$-$b$], where $a \in \{1, 2, \dots, 24\}$ represents the customer-prototype id and $b \in \{1, 2, \dots, 5\}$ indicates the label of the prototype in which a customer was placed. This is often a good approach to tackling a big analysis task, to start with a simpler task. The approach, however, is applicable to all customers.

As a result, we dissect the larger problem and test our solutions on a small sample of prototype customers. To do this, data is first filtered to generate a small sample, and then significant cyclic granularities (variables) for them are chosen (as described in Section \ref{sec:datafilter}). The sample set is subsequently examined along all dimensions of interest, to ensure that they reveal some patterns across at least one specified variable (as described in Section \ref{sec:prototype}). <!--By grouping the prototypes using our methods in Section \ref{sec:clustering} and assessing their meaning, the study hopes to unravel some of the heterogeneities observed in energy usage data.--> Because the data does not contain additional customer characteristics, we cannot explain why consumption varies, but can only identify how it varies. 


### Data filtering and variable selection {#sec:datafilter}

The steps for customer filtering and variable selection were:

1. Choose a smaller subset of randomly selected $600$ customers with no implicit missing values for 2013.
2. Obtain $\wpd$ for all cyclic granularities considered for these customers. It was found that *hod* (hour-of-day), *moy* (month-of-year) and *wnwd* (weekend/weekday) are significant for most customers. We use these three granularities while clustering.
3. Remove customers whose data for an entire category of *hod*, *moy* or *wnwd* is empty. For example, a customer who does not have data for an entire month is excluded because their monthly behavior cannot be analyzed.
4. Remove customers whose energy consumption is 0 in all deciles. These are the clients whose consumption is likely to remain essentially flat and with no intriguing repeated patterns that we are interested in studying.


### Selecting prototypes {#sec:prototype}

<!-- Why instance selection -->


It is common to filter data prior to fitting a supervised classification model using instance selection [@olvera2010review] which removes observations that might impede the model building. For clustering, this is analogous to identifying and removing nuisance observations. Prototype selection is more severe than instance selection, because only a handful of cases is selected. @Cutler1994-hm proposed a method called archetypal analysis which has inspired this approach but the procedure we have used follows @Fan2021-bq. First, dimension reduction such as t-SNE, MDS or PCA is used to project the data into a 2D space. Second, a few "anchor" customers far apart in 2D space are selected. Additional close neighbors to the anchors are selected. To check the selections relative to the full set of variables, we used a tour linked to a t-SNE layout using the R package `liminal` [@R-liminal]. This ensured that the final sample of clustered customers were also far apart in the high-dimensional space. (See the supplementary materials for further details.)<!--Few of these customers have similar distribution across *moy* and some are similar in their *hod* distribution. -->









<!-- - can be done in several ways -->

<!-- - show raw data of moy, hod, wkndwday patterns for them -->


<!-- # ```{r hod-moy-wkndwday plots, fig.cap="The distribution across moy, hod and wkndwday for the selected designs. Few are similar in their hod pattern, while others are similar in *moy* behavior. Some customers have distinct behavior as compared to all other customers.For example, although patterns across wkndwday do not look distinctly different for most customers, there is one household for whom weekend behavior is standing out from the rest."} -->
<!-- # ``` -->



### Clustering results {#sec:clustering}

Clustering of the $24$ prototypes <!--and the data subset consisting of $353$ customers--> was conducted with all three distances, JS-NQT, JS-RS and WPD, and is summarized in Figures \ref{fig:opt-cluster-tsne-jsd}, \ref{fig:groups-4and5} and \ref{fig:summary-plot-wpd}. The t-SNE visualization suggests that there are four well-separated clusters. It is possible that because the representation is only 2D, the fifth group from the original prototype selection is distinctly different in high dimensions. The *sindex* plots for the three methods indicate some disagreement: JS-NQT suggests $3$, JS-RS suggests $2$ or $5$ and WPD suggests $3$ or $5$. JS-RS would appear to match the original prototypes with the five cluster solution, but it actually differs. Even though the *sindex* for JS-NQT suggests three clusters, the five cluster solution more closely matches the original prototypes. The WPD clustering provides a different grouping of the customers, and even though it disagrees with the original prototypes it is a useful grouping.<!--While plotting the clusters, RS-based scaling is applied to each customer, so that no customer dominates the shape of the summarized clusters because of magnitude. The plotting scales are not displayed since we want to emphasize comparable patterns rather than scales. The idea is that a customer in a cluster may have low total energy usage, but their behavior may be quite similar to a customer with high usage with respect to distributional pattern or significance across cyclic granularities.-->

<!--### JS-NQT and JS-RS-->

<!--Figure \ref{fig:opt-cluster-tsne-jsd} The *sindex* is plotted against the number of clusters for clustering based on JS-based (JS-NQT) distances in Figure \ref{fig:opt-cluster-tsne-jsd}. The *sindex* falls from around $0.56$ when $k=3$ to $0.54$ when $k$ increases. In our study, we use $k=4, 5$.--> Figure \ref{fig:groups-4and5} displays the summarized distributions across $4$ and $5$ clusters from JS-NQT clustering in (a) and (b) respectively, and helps to characterize each cluster. In the quantile plots the line represents the median, and the region shows the area between the $25^{th}$ and $75^{th}$ percentiles. The only difference between the four and five cluster solution is that A-4 divides further into B-4 and B-5. This additional division makes a clearer clustering, because it resolves the heterogeneity in *moy* creating a group (B-5, customers 1-3) which has a winter peak in usage, and a group (B-4, customers 16-20) which has a start of the year peak in usage. B-2 (customers 4-9) and B-1 (customers 21-24) have distinctive *hod* patterns but are both heterogeneous in *moy* and *wnwd*. B-3 (customers 10-15) has peak usage at the end of the year, but is heterogeneous on *hod* and *wnwd*. This clustering almost agrees with the clusters visible in the t-SNE plot.

<!-- A-4 is subdivided into B-4 and B-5, each of which has a distinct shape across *moy* and *wnwd*. B-4 (id: 16-20) is distinguished by higher energy consumption in the first few months of the year and greater variability in weekend usage. B-5 (id: 1-3) has higher consumption in the middle of the year (winter months) and a similar weekday-weekend inter-quartile range. When $k = 4$ is used, these two groups merge to form A-4, which has a *moy* profile of higher usage in both the beginning and middle of the year, which is not representative of the individuals. It may be worthwhile to compare Figures \ref{fig:opt-cluster-tsne-jsd} and \ref{fig:hod-ind-group-png} to see if the summarized distributions across groups accurately characterized the groupings. If it has, then the majority of the members of the group should have a similar profile.-->


<!-- Our methodology is useful for grouping similar distributions over *hod* and *moy* and they are placed closely for easy comparison. Few groups have mixed patterns across *hod* and *moy*, but few have all customers in the group having a similar profile.-->

<!--  It appears that the aim of grouping comparable distributions across categories of multiple temporal dependencies has been accomplished to some extent. -->





















































(ref:hod-ind-group-png) The distribution of electricity demand across individual customers over three granularities *hod*, *moy*, and *wnwd* are shown for the $24$ selected customers using quantile and box plots. They are split into batches of $12$ in (a) and (b), with each row in (a) or (b) representing a customer. The number indicates a unique customer id and a prototype id.

\begin{figure}

{\centering \includegraphics[width=\textwidth]{img/ind-groups-24-design} 

}

\caption{(ref:hod-ind-group-png)}(\#fig:hod-ind-group-png)
\end{figure}

(ref:opt-cluster-tsne-jsd) Clustering summaries: (a) t-SNE computed on the $24$ selected customers, and (b) separation index (*sindex*) for $2$-$10$ clusters using JS-NQT, JS-RS and WPD. Various choices in number of clusters would be recommended. Four clusters are visible in t-SNE, although it might hide a fifth cluster because dimension reduction to 2D may be insufficient to see the difference. JS-NQT suggests $3$, JS-RS suggests $2$ or $5$ and WPD suggests $3$ or $5$.

\begin{figure}

{\centering \includegraphics[width=\textwidth]{figure/opt-cluster-tsne-jsd-1} 

}

\caption{(ref:opt-cluster-tsne-jsd)}(\#fig:opt-cluster-tsne-jsd)
\end{figure}

<!-- It appears that the aim of grouping comparable distributions over considered variables has been accomplished to some extent. -->















<!-- The widest within-cluster gap (widest_gap) is high till $k=5$ and then have a gradual decrease. Number of -->



<!-- GroupA and Group B may represent different set of customers as they are groupings based on different number of clusters. -->

(ref:groups-4and5) Summary plots for four and five clusters from JS-NQT showing the distribution of electricity demand combined for all members over *hod*, *moy*, and *wnwd*. Groups A-1 (customers 21-24), A-2 (customers 4-9), and A-3 (customers 10-15) profiles correspond to Groups B-1, B-2, and B-3, respectively. Cluster A-4 splits into B-4 (customers 16-20) and B-5 (customers 1-3) to produce the five clusters, which better resolves the *moy* distribution.


\begin{figure}

{\centering \includegraphics[width=\textwidth]{figure/groups-4and5-1} 

}

\caption{(ref:groups-4and5)}(\#fig:groups-4and5)
\end{figure}

(ref:summary-plot-wpd) Summary plots for three (a) and five (b) clusters from WPD approach showing the $\wpd$ values of each customers across *hod*, *moy*, and *wnwd* through a parallel coordinate plot. P-1 and P-2 groups correspond to Q-1 and Q-2, respectively. Cluster P-3 is subdivided into Q-3, Q-4, and Q-5. P-1 (customers 16, 18) is distinguished by high $\wpd$ on *wnwd* values. P-2 has lower $\wpd$ than *moy* and *wnwd* for *hod*. P-3 operates in the opposite way as P-2, with larger $\wpd$ for *hod* in comparison to *moy* and *wnwd*. For the $5$ cluster solution, this group is divided into Q-3, Q-4, Q-5, which are distinct due to their different relative significance of *moy* and *wnwd*.

\begin{figure}

{\centering \includegraphics[width=\textwidth]{figure/summary-plot-wpd-1} 

}

\caption{(ref:summary-plot-wpd)}(\#fig:summary-plot-wpd)
\end{figure}

\begin{table}

\caption{(\#tab:summary-table-wpd)Summary table from WPD clusters showing median $wpd$ values ($moy$, $hod$, $wnwd$), cluster size ($nobs$) and the list of the customer-prototype id for each cluster with $3$ and $5$ number of clusters ($k$). It is to be noted that $P-1$, $Q-1$ and $P-2$, $Q-2$ are identical and $P-3$ gets split into $Q-3$, $Q-4$ and $Q-5$.}
\centering
\begin{tabular}[t]{r|l|r|r|r|r|l}
\hline
k & group & nobs & moy & hod & wnwd & customer-prototype id\\
\hline
 & P-1 & 2 & 66.7 & -2.7 & 39.4 & 18, 16\\
\cline{2-7}
 & P-2 & 9 & 129.0 & -0.4 & 12.7 & 12, 9, 17, 2, 19, 13, 20, 10, 11\\
\cline{2-7}
\multirow{-3}{*}{\raggedleft\arraybackslash 3} & P-3 & 13 & 14.9 & 24.5 & 4.4 & 8, 22, 23, 24, 14, 15, 3, 1, 4, 21, 5, 6, 7\\
\cline{1-7}
 & Q-1 & 2 & 66.7 & -2.7 & 39.4 & 18, 16\\
\cline{2-7}
 & Q-2 & 9 & 129.0 & -0.4 & 12.7 & 12, 9, 17, 2, 19, 13, 20, 10, 11\\
\cline{2-7}
 & Q-3 & 4 & 88.2 & 29.4 & 2.6 & 22, 14, 4, 6\\
\cline{2-7}
 & Q-4 & 4 & 10.1 & 32.1 & 4.2 & 23, 21, 5, 7\\
\cline{2-7}
\multirow{-5}{*}{\raggedleft\arraybackslash 5} & Q-5 & 5 & 14.9 & 11.9 & 4.6 & 8, 24, 15, 3, 1\\
\hline
\end{tabular}
\end{table}


<!--### WPD-->

<!-- In Figure \ref{fig:opt-cluster-tsne-jsd}, the *sindex* is plotted against the number of clusters for clustering based on wpd-based distances. The scales are very different from those used in JS-based clustering. The *sindex* falls from around $20$ when $k=3$ to $19$ when $k=4$ and further decreases to $17.5$ with $k>7$. Thus, there are few choices available for the number of clusters. Figure \ref{fig:summary-plot-wpd} whos the wpd values for the three different granularities of the 24 customers, coloured by the 6 cluster solution. XXX REWORK THIS WHEN NEW CLUSTER RESULTS ARE SHOWN<!--The groupings with $k=4$ is shown through a parallel coordinate plot with the three significant cyclic granularities. The variables are sorted according to their separation across classes (rather than their overall variation between classes). This means that *moy* is the most important variable in distinguishing the groups, followed by *hod* and *wnwd*. The two customers (id: 16 and 18) that have significant differences between *wnwd* categories (in Figure \ref{fig:hod-ind-group-png}) stand out from the rest of the customers. Group-4 has a higher $\wpd$ for *hod* than *moy* or *wnwd*. Group-2 and Group-3 have the most distinct patterns across *moy*. Group-1 is a mixed group that has strong patterns on at least one of the three variables. When $k=3$ is used, Group-2 and Group-3 are merged because their patterns relative to *moy*, *hod* and *wnwd* are the same. The findings vary from JS-based clustering, yet it is a helpful grouping.-->

Figure \ref{fig:summary-plot-wpd} shows the $\wpd$ values of the $24$ customers over *hod*, *moy*, and *wnwd* , colored by $3$ (a) and $5$ (b) clusters from WPD clustering using a parallel coordinate plot. The variables ($\wpd$ for different granularities) are 
standardized prior to clustering using WPD. In the display, the variables are sorted according to their separation across groups. This means that *wnwd* is the most important variable in distinguishing the groups, followed by *hod* and *moy* for both (a) and (b).
Groups P-1 and P-2 correspond to Q-1 and Q-2 respectively. Cluster P-3 splits into Q-3, Q-4 and Q-5. Customers 16 and 18 are characterized by unusual high values of $\wpd$ on *wnwd* compared to the rest of the customers and hence form a group. This could again be verified from Figure \ref{fig:hod-ind-group-png}, where these were the only two customers with a difference in their *wnwd* behavior. They are represented by P-1. P-2 has lower $\wpd$ for *hod* than *moy* and *wnwd*. P-3 behaves opposite to P-2 with higher $\wpd$ for *hod* compared to *moy* and *wnwd*. These are the customers who have some significant pattern across *hod* and this can again be validated by looking at Figure \ref{fig:hod-ind-group-png}. For a $5$ cluster solution, this group gets split into Q-3, Q-4 and Q-5 characterizing different relative significance of *moy* and *wnwd*. For example, Q-4 and Q-5 have almost no pattern across *moy*, but Q-3 has a *moy* pattern and thus it is reasonable to split them. The patterns could be different, but they are significant. Q-4 and Q-5 are separated because of their different significance of *hod*. All of these can also be verified from Table \ref{tab:summary-table-wpd} which shows the cluster summaries with members and median values of $\wpd$ for the three variables. 

<!-- *hod* has very low median values of $\wpd$ for the members in the cluster, implying at least half of these customers do not have an interesting *hod* pattern. -->





<!-- # ```{r summary-plot-wpd, fig.cap="Summary of the four cluster results from WPD, shown as a parallel coordinate plot and table. XXX TO BE REVISED WHEN DIFFERENT CLUSTER SOLUTION IS SHOWN", message=FALSE, warning=FALSE, fig.height=6} -->
<!-- # #include_graphics("img/summary-plot-wpd.png") -->
<!-- # ``` -->

In summary, none of the methods captured the five original prototypes exactly. JS-NQT was almost identical, but WPD produced quite a different grouping. This is quite a reasonable result and illustrates both the difficulties of clustering to obtain a particularly expected grouping and the ability to learn unexpected patterns in the data. It is possible that the JS-based distances were distracted by the presence of nuisance variables, levels of the granularities that do not contribute to clustering. This would also be supported by the results of the validation study, where clustering was less effective in S2 and S3, where only some granularities had differences between levels. Clustering using WPD is expected to produce quite different results because it will group only by overall value of a granularity, not a particular pattern. A cluster summary like Figure \ref{fig:groups-4and5} is not possible because there may be different but equally interesting patterns (e.g. high evening *hod* and high daytime *hod*) in the same cluster. Simply it provides information that across a collection of customers this specific cluster has interesting patterns in a granularity (e.g. *hod*). One would need to post-process the cluster to separate specific patterns.  

<!--Things become far more complicated when we consider a larger data set with more uncertainty, as they do with any clustering problem. Summarizing distributions across clusters with varied or outlying customers can result in a shape that does not represent the group. Furthermore, combining heterogeneous customers may result in similar-looking final clusters that are not effective for visually differentiating them. It is also worth noting that the weekend/weekday behavior in the given case does not characterize most selected customers. This, however, will not be true for all of the customers in the data set. If more extensive prototype selection is used, resulting in more comprehensive prototypes in the data set, this method might be used to classify the entire data set into these prototype behaviors. However, the goal of this section was to have a few customers that have significant patterns over one or more cyclic granularities, apply our methodology to cluster them, and demonstrate that the method produces useful clusters.-->



## Discussion {#sec:discussion-gracsr}

We offer two approaches for calculating pairwise distances between time series based on probability distributions over multiple cyclic granularities at once. Depending on the goal of the clustering, these distance metrics, when fed into a hierarchical clustering algorithm using Ward's linkage, yield meaningful clusters. Probability distributions provide an intuitive method to characterize noisy, patchy, long, and unequal-length time series data. Distributions over cyclic granularities help to characterize the formed clusters in terms of their repeating behavior over these cyclic granularities. Furthermore, unlike earlier efforts that group customers based on behavior across only one cyclic granularity (such as hour-of-day), our method is more comprehensive in detecting clusters with repeated patterns at all relevant granularities. <!--The JS-based approaches are faster than wpd-based approaches.-->

There are a few areas to extend this research. First, larger data sets with more uncertainty complicate matters, as is true for any clustering task. Characterizing clusters with varied or outlying customers can result in a shape that does not represent the group. Moreover, integrating heterogeneous consumers may result in visually identical end clusters, which are potentially not useful. Hence, a way of appropriately scaling it up to many customers such that anomalies are removed before clustering would be useful for bringing forth meaningful, compact and separated clusters. Secondly, we have assumed the time series to be stationary, and hence the distributions are assumed to remain constant for the observation period. In reality, however, it might change. For the smart meter example, the distribution for a customer moving to a different house or changing electrical equipment can change drastically. Our current approach cannot detect these dynamic changes. Thirdly, it is possible that for a few customers, data for some categories from the list of considered significant granularities are missing. In our application, we have removed those customers and done the analysis but the metrics used should be able to incorporate those customers with such structured missingness. Finally, $\wpd$ is computationally heavy even under parallel computation. Future work can make the computations more efficient so that they are easily scalable to a large number of customers. Moreover, experiments can also be run with non-hierarchy based clustering algorithms to verify if these distances work better with other algorithms.


## Acknowledgments {-}

The authors thank the ARC Centre of Excellence for Mathematical and Statistical Frontiers [(ACEMS)](https://acems.org.au/home) for supporting this research. Sayani Gupta was partially funded by [Data61 CSIRO](https://data61.csiro.au/) during her PhD. The Monash eResearch Centre and eSolutions-Study Support Services supported this research in part through the resource usage of the MonARCH HPC Cluster. The Github repository, [github.com/Sayani07/paper-gracsr](https://github.com/Sayani07/paper-gracsr), contains all materials required to reproduce this article and the code is also available online in the supplementary materials. This article was created with R [@R-language], `knitr` [@knitr; @R-knitr] and `rmarkdown` [@rmarkdown; @R-rmarkdown]. Graphics are produced with `ggplot2` [@Wickham2009pk] and `GGally` [@R-GGally].

## Supplementary materials {- #supplement-gracsr}

**Data and scripts:** Data sets and R codes to reproduce all figures in this article ([validation.R](https://github.com/Sayani07/paper-gracsR/blob/master/script/validation.R), [application.R](https://github.com/Sayani07/paper-gracsR/blob/master/script/application.R))

\noindent
**Supplementary paper:** Additional graphics and R codes to reproduce it  
([paper-supplementary.pdf](https://github.com/Sayani07/paper-gracsR/blob/master/paper-supplementary.pdf), [paper-supplementary.Rmd](https://github.com/Sayani07/paper-gracsR/blob/master/paper-supplementary.Rmd), [supplementary.R](https://github.com/Sayani07/paper-gracsR/blob/master/script/supplementary.R))

\noindent
**R-package:** To implement the ideas provided in this research, the open-source R package `gracsr` is available on Github (https://github.com/Sayani07/gracsr).

<!--chapter:end:Rmd//04-gracsr.Rmd-->

# Conclusion {#ch-conclusion}

This thesis presents methods for visualizing and analyzing distributions of large temporal data by deconstructing time into temporal granularities. This chapter summarizes the thesis content, outlines the original contributions, software development, and possible directions for future research.

## Original contributions

<!-- In this thesis, I present a systematic approach for visualizing and analyzing distributions of large temporal data by leveraging time characteristics. The building blocks of this framework are presented in each chapter. Chapter \@ref(ch-gravitas) contains tools for computing all cyclic granularities, as well as a recommendation system for selecting pairs of granularities that can be effectively analyzed together. These temporal granularities can be used to generate data visualizations to look for patterns, associations, and anomalies. But it is difficult to decide which of the various granularities to display when there are many options. Chapter \@ref(ch-hakear) presents a methodology for selecting displays that are interesting, such that differences between displayed distributions are greatest. The methodology also ranks the displays in order of priority for capturing the most variation. The ideas in Chapters \@ref(ch-gravitas) and \@ref(ch-hakear) can be used for studying patterns in individual time series or comparing a few time series together. This is extended in Chapter \@ref(ch-gracsr) to allow for the exploration of distributions for multiple time series at the same time using unsupervised clustering. The clustering methodology produces small groups of time series with similar distributions over multiple granularities. This technique is more robust to noisy, patchy, and uneven length time series because it makes use of probability distributions. -->

Exploratory time series analysis entails numerous iterations of identifying and summarizing temporal dependencies. It is common practice to divide time into years, months, weeks, days, and so on in order to make inferences at both finer and coarser scales. In the literature, the formalization of these temporal deconstructions (granularities) is limited to linear time granularities such as hours, days, weeks, and months that respect the linear progression of time and are non-repeating. Cyclic granularities that are repeating in nature are useful for finding patterns in temporal data. They can be circular, quasi-circular, or aperiodic in nature. Hour-of-the-day and day-of-the-week are examples of circular granularities; the day-of-the-month is an example of a quasi-circular granularity; and public holidays and school holidays are examples of aperiodic granularities. Additionally, time deconstructions can be based on a time hierarchy. Thus, single-order-up granularities such as second of minute or multiple-order-up granularities such as second of hour can be envisioned. The definitions and rules defined in the literature for linear granularities are insufficient for describing various types of cyclic granularities. 

Chapter \@ref(ch-gravitas) provides a formal characterization of cyclic granularities as well as tools for classifying and computing potential cyclic granularities from an ordered temporal index. It also allows for the manipulation of single- and multiple-order-up time granularities via cyclic calendar algebra. The approach is generalizable to non-temporal hierarchical granularities with an ordered index. Visualizing probability distributions conditional on one or more cyclic granularities is a powerful exploration tool. However, there may be too many cyclic granularities to look at manually for comprehensive exploration, and not all pairs of granularities can be effectively explored together. Chapter \@ref(ch-gravitas) also provides a recommendation on whether a pair of granularities can be meaningfully plotted or analyzed together (a "harmony") or when they cannot (a "clash" or "near-clash").

Cyclic granularities could be used to create a wide range of displays. And, when there are numerous granularities to choose from, deciding which one to display can be difficult. Moreover, only a few of them may be useful in revealing major patterns. In Chapter \@ref(ch-hakear), the search for informative granularities is facilitated by selecting "significant" granularities. A cyclic granularity is referred to as "significant" if there is a significant distributional difference of the measured variable between different categories. Chapter \@ref(ch-hakear) defines a distance measure to quantify these distributional differences. A higher value of the distance measure for a cyclic granularity or harmony implies that they could be interesting for further investigation, whereas a low value indicates that nothing noteworthy is unfolding. A threshold and, consequently, a selection criterion are chosen using a permutation test such that cyclic granularities with significant values of the distance measure are selected. In addition, the distance metric has been appropriately adjusted, allowing it to be compared not only across cyclic granularities with different numbers of categories but also across a set of time series. As a result, it can also be used to rank the displays according to their ability to capture the greatest amount of variation across one or multiple time series.

The ideas in Chapters \@ref(ch-gravitas) and \@ref(ch-hakear) can be used for studying patterns in individual time series or comparing a few time series together. This is extended in Chapter \@ref(ch-gracsr) to allow for the exploration of distributions for multiple time series at the same time using unsupervised clustering. In the time series clustering literature, probability distributions across cyclic granularities have not been considered in determining similarity. However, such a similarity measure can be useful for characterizing the inherent temporal data structure of long, unequal-length time series in a way that is resistant to missing or noisy data while allowing for the detection of similar repeated patterns. Chapter \@ref(ch-gracsr) proposes two approaches for calculating distances between time series based on probability distributions across cyclic granularities. The first approach considers two time series to be similar if the distributions of each category of one or more cyclic granularities are similar. The second approach considers two time series to be similar if they have a similar significance of patterns across different granularities. A similar significance does not imply a similar pattern, which is where this technique varies from the former. When the distances from these approaches are fed into a hierarchical clustering algorithm, they yield small groups of time series with similar distributions or significance over multiple granularities. Our method is capable of producing useful clusters for both approaches, as demonstrated by testing on a range of validation data designs and a sample of residential smart meter consumers.



<!-- is more comprehensive in recognizing clusters with recurring patterns over many important granularities and -->



## Software development



This thesis focuses on translating research approaches into open source R packages for reproducibility and ease of use in other applications. So a significant amount of work has been devoted to the development of R packages  `gravitas`, `hakear`, and `gracsr`, each of which corresponds to a chapter presented in this thesis.

<!-- Figure \@ref(fig:software-ghcommits) gives an overview of my Git commits to these repositories, and Figure \@ref(fig:software-downloads) shows the daily downloads of the packages from the RStudio mirror (one of 90 CRAN mirrors) since they were available on CRAN. -->

<!-- ```{r software-ghcommits, fig.cap = "(ref:software-ghcommits-cap)"} -->
<!-- include_graphics("img/pkg-commits.png") -->
<!-- ``` -->

<!-- (ref:software-ghcommits-cap) Patterns of my package development effort during my PhD years based on Git commits to three repositories, sugrrants, tsibble, and mists. Scatter plots of weekly totals are overlaid with a loess smoother. The **sugrrants** package was the first project with much initial energy, followed by small constant maintenance. The **tsibble** package has been a major project with ongoing constant development and bursts of effort in response to users' feedback. The **mists** package has been a recent intense project. -->

<!-- # ```{r software-downloads, fig.height = 3, fig.cap = "(ref:software-downloads-cap)"} -->
<!-- # ``` -->
<!-- #  -->
<!-- # (ref:software-downloads-cap) Impact of these works (sugrrants and tsibble) as measured by daily downloads (on square root scale) from the RStudio mirror since they landed on CRAN. The **tsibble** package has an increasing trend, suggesting the steady adoption of the new data structure. -->

### gravitas

The `gravitas` package provides very general tools to compute and manipulate cyclic granularities and generate plots displaying distributions conditional on those granularities. The functions `search_gran()`, `create_gran()`, `harmony()`, `gran_advice()` and `prob_plot()` provides the entire workflow for an analyst to systematically explore large quantities of temporal data across different harmonies (pairs of granularities that can be analyzed together). This package was developed as part of my internship at Google Summer of Code, 2019. It has been on CRAN since January 2020. The website (<https://sayani07.github.io/gravitas>) includes full documentation and two vignettes about the package usage. There has been a total of 12K downloads from the RStudio mirror dating from 2020-11-01 to 2021-11-01. This package supplements the paper corresponding to Chapter \@ref(ch-gravitas), which has won the ACEMS Business Analytics Award 2021. The package can be generalized to non-temporal applications for which a hierarchical structure can be construed similar to time. 

### hakear

The R package `hakear` (https://github.com/Sayani07/hakear) provides tools for selecting and sorting significant cyclic granularities. The function `wpd()` computes the weighted pairwise distances ($\wpd$) for each cyclic granularity or pair of granularities, and the function `select_harmonies()` chooses those with significant patterns and ranks them from highest to lowest $\wpd$. This package is reliant on parallel processing using multiple multi-core computers for faster computation of $\wpd$. The selected harmonies can be plotted using package `gravitas` for potentially interesting displays. Currently, `hakear` implements ideas presented in Chapter \@ref(ch-hakear), but it will be integrated with `gravitas` in the future to explore distributions of a smaller number of time series.

### gracsr

The R package `gracsr` (https://github.com/Sayani07/gracsr) has functions for exploring a large number of time series using the clustering methodology described in Chapter \@ref(ch-gracsr). The workflow begins with the function `scale_gran()`, which may be used to scale individual series using NQT/RS. The distances for the JS and WPD approaches are computed in the second phase of the workflow using `dist_gran()`/`dist_ wpd()`. The distances can then be used to do clustering with `clust_gran()`. The package has received a grant (AUD 3000) as part of the ACEMS Business Analytics Prize towards polishing the functions and preparing it for CRAN.


### Computational resources
Simulation studies were carried out to study the behavior of $\wpd$, build the normalization method as well as compare and evaluate different normalization approaches in Chapter \@ref(ch-hakear). In Chapter \@ref(ch-gracsr), our methods were tested on several data designs with different parameters to evaluate their performance. $\wpd$ is computationally heavy for cyclic granularities with smaller levels. JS and WPD approaches are also computationally intensive when run on large number of customers. Hence, most of the scripts used to run these studies use parallel processing for better computational speed. R version 4.0.1 (2020-06-06) is utilized on the platform x86 64-apple-darwin17.0 (64-bit) operating on macOS Mojave 10.14.6, as well as the High Performance Computing (HPC) resources provided by [MonARCH](https://docs.monarch.erc.monash.edu/MonARCH/aboutMonArch.html).

<!-- The package contains functions for implementing the complete clustering methodology with choices of scaling and distance metrics discussed in the chapter. It -->


## Limitations and future ideas

We address several limitations of the current framework that might serve as natural next steps for this work and some potential short- and long-term aims in future ideas.

### Limitations

The time series are observed over a short period of time (1--3 years) in the motivating example of this research, and they are assumed to be stationary. But it is possible that the distributions change over time, even over a short period. For the smart meter example, the distribution for a customer moving to a different house or changing electrical equipment may change drastically. To detect these dynamic changes, non-stationarity in time series has to be incorporated while visualizing distributions and also computing distances for two non-stationary time series or one stationary and another non-stationary time series.

Additionally, it is possible that data for a whole category of cyclic granularity is unavailable or that there are insufficient observations to compute distributions. For example, a customer may not have data for a particular day of the week or month throughout their observation period. While visualizing probability distributions across categories in Chapters \@ref(ch-gravitas) and \@ref(ch-hakear), this can be indicated by displaying dot plots instead of summarizing distributions. But the distances in Chapter \@ref(ch-gracsr) can not handle missing observations if they are structured like this. We would like to be able to think about designing a distance metric that can incorporate customers with structural missingness and also comprehend its implications while visually characterizing them. Another related direction is how to manage the swarm of nuisance variables produced by transforming the time series to cyclic granularities. Because the design of our existing framework treats each level of granularity as a variable, it is important to identify levels of granularity that do not contribute to clustering and remove them from the distance metric in order to improve the performance of the clustering algorithm.

### Future ideas

The standard methodologies provide contradicting recommendations for the optimal number of clusters when there are nuisance variables (no contribution to clustering) or nuisance observations (inlying and outlying observations falling between clusters). As a result, a more realistic short-term goal should be to test the persistence of clustering solutions in the presence of nuisance variables for the chosen number of clusters. This can be done by introducing slightly different samples into the study and observing how the clustering methods handle increased heterogeneity.

Another possible direction is to reduce computational time so that the proposed methods are easily scalable to many customers. The distance measure, $\wpd$ is computationally heavy even under parallel computation. Moreover, while computing distances between time series, the proposed methods compute all possible pairwise distances, which acts as a computational barrier. Faster nearest-neighbor search algorithms can be employed here to decrease the computational load.

A longer-term goal would be to create a similar framework for visualizing and analyzing multivariate time series data. With multiple time series available for each observation, the complexity of efficient exploration and visualization grows exponentially. In this case, conditional distributions include not only temporal dependency but also variables and their dependencies. This adds to the already high-dimensional data structures that result from studying distributions. This big problem can be tackled by first incorporating time’s inherent characteristics while visualizing one or a few multivariate time series data. Unsupervised clustering can then be used to group multiple time series across multiple time granularities and variables. This is a method similar to the one used in this thesis for dealing with univariate time series.


<!-- There are a few areas to extend this research. The first two explore catering to time series data, where the design of our framework is limited. Handling non-stationary time series data or data with structured missingness, for example. Second, scaling up for more uncertainty is beneficial since huge data sets with greater uncertainty may have more difficulties than a few time series. It is essential to recognize the difficulties of analyzing clustering results in such a setting. To that end, it would be interesting to investigate how to deal with anomalies or determine the significance of the clusters. A third possibility is to focus on expanding the scope of the framework to include multivariate time series data. -->


<!-- **Non-stationary time series** -->


<!-- **Structured missingness** -->



<!-- **Anomalies** -->

<!-- Characterizing clusters with varied or outlying customers can result in patterns that do not represent the group. Moreover, integrating heterogeneous consumers may result in visually identical end clusters, which are potentially not useful. An appropriate anomaly detection approach could be applied to filter the anomalies, and then a decision has to be made whether the anomalies should form a group or be assigned to the closest group through a classification method. -->


<!-- ### Comparing our clustering method with other benchmark methods -->

<!-- Testing needs to be carried out with non-hierarchy based clustering methods to see whether the proposed distances perform better with other algorithms. It would also be helpful to know how much more value we obtain from considering distributions rather than aggregating time series data. In the case of smart meters, this could be accomplished by comparing the results while validating them against external data such as weather conditions, socioeconomic or other demographic factors of those households. -->


<!-- ### Extend methods to other fields -->

<!-- We propose solutions that are realistically applicable to any temporal data collected more than once per year and may be implemented in a variety of settings. We could simply verify its utility in the context of smart meter electricity data. With the availability of several open-source benchmark data sets, it is vital to evaluate how well the methodologies perform across a variety of fields. -->




<!--chapter:end:Rmd//05-conclusion.Rmd-->



<!--chapter:end:Rmd//99-references.Rmd-->

