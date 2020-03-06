# Course 3: Functional Programming in R
This repository will house all the materials for the third course in the data science specialization using R, developed for the UO COE.

<a rel="license" href="http://creativecommons.org/licenses/by-nc/4.0/"><img alt="Creative Commons License" style="border-width:0" src="https://i.creativecommons.org/l/by-nc/4.0/88x31.png" /></a><br />This work is licensed under a <a rel="license" href="http://creativecommons.org/licenses/by-nc/4.0/">Creative Commons Attribution-NonCommercial 4.0 International License</a>.

# Syllabus
## EDLD 610: Functional Programming in R (CRN: 35699; 3 credit hours)
* **Term: Spring, 2019**
* **Time: M/W 10-11:20** 
* **Classroom: 115 Lokey Education** 
* **Instructor:** Daniel Anderson, PhD
	+ *email:* (preferred contact method) [daniela@uoregon.edu](mailto:daniela@uoregon.edu)
	+ *phone:* 541-346-3317
	+ *office:* 175 Lokey
	+ *office hours*: 

# Course Overview
This is the third course in the *data science in educational research* specialization. 
This course builds upon the content covered in the first two courses, with a
specific focus on becoming a better programmer and improving workflows in 
statistical computing with R. Students will continue to work with the [tidyverse](https://www.tidyverse.org) 
suite of packages, with a emphasis on [{purrr}](https://purrr.tidyverse.org) for
functional programming. At its core, functional programming is a technique to
iterate a function over a vector, or set of vectors, to complete repetitive tasks.
We will compare and contrast {purrr} functions with base R approaches, including
`for` loops and the `apply` family of functions. Functional programming helps 
reduce redundancies in code, making it more efficient and, often, more readable.
The course will also cover writing custom functions, which can also help in
completing repetitive tasks, but can also extend the functionality of R, and is
a key component of functional programming. The course concludes with a brief 
introduction to [shiny](https://shiny.rstudio.com/) for building interactive 
applications which, although somewhat outside of the scope of functional 
programming, requires using and writing functions.

## Student Learning objectives
By the end of this course, students should be able to:

* Understand and be able to describe the differences in R's data structures 
  (including the four main vector types, data frames, lists) and when each is 
  most appropriate for a given task
* Explore `purrr::map` and its variants, how they relate to base R functions, 
  and why the {purrr} variants are often preferable.
* Work with lists and list columns using `purrr::nest` and `purrr:unnest`
* Fit multiple (basic) models using the [{broom}](https://broom.tidyverse.org) 
  package with {purrr} 
* Convert repetitive tasks into functions 
* Understand elements of good functions, and things to avoid
* Write effective and clear functions to continue with the mantra of "Don't Repeat Yourself"

# Course Reading List And Other Resources
All course readings are freely available online or will be provided by the
instructor. 

## Books (required)
1. Wickham, H. (2019). *[Advanced R [second edition]](https://adv-r.hadley.nz)*
2. Wickham, H. and Grolemund, G. (2017). *[R for data science](http://r4ds.had.co.nz)*. Sebastopol, CA: O'Reilly. 
	+ Freely available online in the link above. A full-color paper copy is also [available from Amazon](http://amzn.to/2aHLAQ1) for (currently) \$18.17.
3. Bryan, J. (2018). *[Happy git and GitHub for the useR](http://happygitwithr.com)*.


# Weekly Schedule (Topics, Assignments, and Readings)
| **Week**    | **Date** | **Theme**                                      | **Assignment Assigned** | **Assignment Due**                            | **Reading**                                                                                                                                   |
| :---------- | :------- | :--------------------------------------------- | :---------------------- | :-------------------------------------------- | :-------------------------------------------------------------------------------------------------------------------------------------------- |
| 1a          | 4/1/19   | Intro to course                                | Final Project           |                                               | [Adv R: Intro to Functional Programming](https://adv-r.hadley.nz/fp.html); [Adv R: 3.1, 3.2, 3.5 ](https://adv-r.hadley.nz/vectors-chap.html) |
| 1b          | 4/3/19   | Data Types 1                                   |                         |                                               | [Adv R: 5.3](https://adv-r.hadley.nz/control-flow.html#loops)                                                                                 |
| 2a          | 4/8/19   | Iteration 1                                    |                         |                                               |                                                                                                                                               |
| 2b          | 4/10/19  | Lab 1                                          | Lab 1                   | Groups for Final Project Finalized            | [Adv R: 9.1 to 9.3](https://adv-r.hadley.nz/functionals.html)                                                                                 |
| 3a          | 4/15/19  | Iteration 2                                    |                         |                                               |                                                                                                                                               |
| 3b          | 4/17/19  | Lab 2                                          | Lab 2                   | Lab 1                                         | [Adv R: 3.6 ](https://adv-r.hadley.nz/vectors-chap.html); [r4ds: 25](https://r4ds.had.co.nz/many-models.html)                                 |
| 4a          | 4/22/19  | List columns                                   | Midterm                 |                                               | https://www.tidyverse.org/articles/2018/01/fs-1.0.0/                                                                                          |
| 4b          | 4/24/19  | Batch load data, and map_df                    |                         | Lab 2                                         | [Adv R: 9.4 to 9.6](https://adv-r.hadley.nz/functionals.html)                                                                                 |
| 5a          | 4/29/19  | Parallel and other types of iterations         |                         |                                               |                                                                                                                                               |
| 5b          | 5/1/19   | Work day                                       |                         | Final Project Outline                         | [r4ds: 19](http://r4ds.had.co.nz/functions.html)                                                                                              |
| 6a          | 5/6/19   | Writing functions 1                            |                         | Midterm                                       | https://swcarpentry.github.io/r-novice-inflammation/02-func-R/                                                                                |
| 6b          | 5/8/19   | Lab 3                                          | Lab 3                   |                                               | [Adv R: 6.1 to 6.4](https://adv-r.hadley.nz/functions.html)                                                                                   |
| 7a          | 5/13/19  | Writing functions 2                            |                         |                                               | *optional* [Adv R: 6.5 to 6.8](https://adv-r.hadley.nz/functions.html)                                                                        |
| 7b          | 5/15/19  | Lab 4                                          | Lab 4                   | Lab 3                                         | [Parts](https://shiny.rstudio.com/articles/basics.html); [Build](https://shiny.rstudio.com/articles/build.html);                              |
| 8a          | 5/20/19  | Shiny 1                                        |                         |                                               |                                                                                                                                               |
| 8b          | 5/22/19  | Work day                                       |                         | Lab 4                                         |                                                                                                                                               |
| 9a          | 5/27/19  | Shiny 2                                        | Peer review             | Final Project Draft                           |                                                                                                                                               |
| 9b          | 5/29/19  | Work day                                       |                         |                                               |                                                                                                                                               |
| 10a         | 6/3/19   | Measuring & improving performance              |                         | Peer review                                   |                                                                                                                                               |
| 10b         | 6/5/19   | Topics beyond the course (package development) |                         |                                               |                                                                                                                                               |
| Finals Week | 6/12/19  |                                                |                         | Final Project                                 |                                                                                                                                               |


# Assignments 
## Labs (40 points; 16%)
There are 4 labs during the course. All labs will be scored on a "best
honest effort" basis, which generally implies zero or full credit (i.e., the
assignment was or was not fully completed). However, labs may require
students complete specific portions before moving on to the next sections. If
you find yourself stuck and unable to proceed, **please contact the instructor
for help rather than submitting incomplete work**. Contacting the instructor
is part of the "best honest effort", and can result in full credit for an 
assignment even if the the work is not fully complete. **If the assignment is
not complete, and the student has not contacted the instructor for help or
visited office hours, it is likely to result is a partial credit score or a 
zero**. Labs submitted late will be docked by 30% (3 points).

## Extra Credit
Up to 5 points extra credit can be earned for completing a DataCamp module 
during both the first and last 5 weeks of the term.

## Midterm (50 points; 20%)
As may be obvious from the weekly structure, this course is rather split into
iterations, and writing/building functions. The final project should incorporate
both elements (as described below), but we will also have a midterm that 
focuses primarily on iteration (perhaps with some data type/coercion involved
as well). More details will be available later in the term, but you are likely
to be asked to rewrite code to accomplish a similar task using more efficient
and/or readable approaches. 

## Final Project (150 points; 60%)

The final project in this class is a group project, requiring students use a 
publicly available “real world” dataset, collaborate through *git*/GitHub, and
demonstrate the use of functional programming and writing custom functions. 
**Students who do not have access to data should plan to meet with the 
instructor as soon as possible so a dataset can be provided**. If making your 
data publicly available is a problem for you, please contact the 
instructor as soon as possible. We can work together to either find a dataset 
that will work for you, or simulate a dataset that is similar to the data 
you'd like to work with in reality (and then all your code should work for the 
real dataset, but you can share the simulated data with your classmates). 
Students are required to work in groups of 2-4 people. The final assignment is 
assigned during the first class, and **groups must be finalized by the end of 
Week 2** (at which point students who have not self-selected into groups will 
be assigned by the instructor).

The specific format of the final project is open, but generally will be one of
the following:

* Brief, reproducible, APA manuscript
* Dashboard (flex dashboard or shiny dashboard) 
* Shiny application
* Blog post(s) 

Alternative formats are also allowable, but should be discussed and approved by
the instructor.

There are **two basic approaches** you may take for the final project. 

1. Write code that builds toward a cumulative product (e.g., a data product for 
   a client, a research article, or similar).

2. A tutorial (or series of tutorials) on functional programming and writing 
   functions in R. 

The former would likely be best suited to one of the first three options listed
above, while the former is best suited for a blog post or, perhaps preferably,
a series of blog posts. Note that you could also easily combine formats - e.g., 
a blog post that includes a shiny app or a dashboard.

### Outline (10 points; 5%; **Due May 1**)
At the end of Week 5, you must include a proposal of your final project. It 
should specifically identify:

* The chosen format(s)
* Whether the purpose will primarily be instructive (tutorial-style) or 
  substantive
* Description of the data source (must be publicly available)
* Lingering questions (is there anything that gives you pause in terms of how
  your final project will address all the grading requirements)

### Draft Data Preparation Script (20 points)
##### Due by 10:00 am, May 27
By the beginning of Week 9, you must have a draft of your final project
complete, with the vast majority, if not all, of the components listed in the
grading criteria present. This script is expected to still be a work in 
progress and it is expected there will still be pieces you are working on. But
the majority should be there. This script will be distributed to your peers 
for feedback.

### Peer Review of Data Preparation Script (20 points)
##### Due by 10:00 am, June 3
Following the submission of the data preparation scripts, you will be assigned 
to review your peers code. The purpose of this exercise is to learn from each 
other. Programming is an immensely open-ended enterprise and there are lots of 
winding paths that all ultimately end up at the same destination. During your 
peer review, you must note the following three elements.

1. At least three areas of strength

2. At least one thing you learned from reviewing their script

3. At least one and no more than three areas for improvement 

To conduct your review, you are required to fork the group's repository, 
provide a comprehensive review with commits on your fork, then submit a pull
request so the group can easily see the changes you made and choose whether to
merge them or not.

Making your code publicly available can feel daunting. The purpose of this 
portion of the final project is to help us all learn from each other, not to 
denigrate. **Under no circumstances will negative comments be tolerated**. Any 
comments that could be perceived as negative, and outside the scope of the 
code, will result in an immediate score of zero. Be constructive in your 
feedback. Be kind. We are all learning.

### Final product grading (100 points)
##### Due by 11:59 PM, June 12
Below are the required components of the final project and the points
associated with each component. Note that it is assumed that in order to meet 
all requirements of the final projects some the use cases may be a bit 
artificial. This is completely acceptable. It is important to recognize that
this is a final project for a class, not a "real" product, and these elements
must be present to demonstrate that you learned the content.

* No code is used repetitively (no more than twice) [10 points]
* More than one variant of `purrr::map` is used [10 points]
* At least one {purrr} function outside the basic `map` family (`walk_*`, 
  `reduce`, `modify_*`, etc.) [10 points]
* At least one instance of parallel iteration (e.g., `map2_*`, `pmap_*`) [10 points]
* At least one use case of `purrr::nest %>% mutate()` [10 points]
* At least two custom functions [20 points; 10 points each]
	+ Each function must be "pure" unless it is clear that it should have side 
	  effects (e.g., a plotting function)
  + Each function must do exactly one thing
  + The functions **may** replicate the behavior of a base function - as noted 
    above this is about practicing the skills you learn in class
* Code is fully reproducible and housed on GitHub [10 points]
* No obvious errors in chosen output format [10 points]
* Deployed on the web and shareable through a link [10 points]

I will investigate the commits made by different authors when evaluating the
final project. If it is obvious that one person did not utilize GitHub, and
instead added all of their contributions through a single or only a few 
commits, I will dock points from that individual. There should be numerous
commits by each author, and they should be roughly even in terms of
contribution activity (which GitHub has metrics to track, both in terms of the
number of commits as well as the number of lines modified).

# Grading Components
|  **Lower percent** |**Lower point range**  | **Grade** | **Upper point range**  | **Upper percent**|
|  :------: | :------   | :-:| :-------: | ----:|
|  0.97     | (242 pts) | A+ | (250 pts) |      |
|  0.93     | (232 pts) | A  | (242 pts) | 0.97 |
|  0.90     | (225 pts) | A- | (232 pts) | 0.93 |
|  0.87     | (218 pts) | B+ | (225 pts) | 0.90 |
|  0.83     | (208 pts) | B  | (218 pts) | 0.87 |
|  0.80     | (200 pts) | B- | (208 pts) | 0.83 |
|  0.77     | (192 pts) | C+ | (200 pts) | 0.80 |
|  0.73     | (182 pts) | C  | (192 pts) | 0.77 |
|  0.70     | (175 pts) | C- | (182 pts) | 0.73 |
|           |           | F  | (175 pts) | 0.70 |

# Student Engagement Inventory
Graduate: 1 credit hour = 40 hours of student engagement (3 credit hours = 120 hours of student engagement) 

| **Educational activity** | **Hours student engaged** | **Explanatory comments (if any):**                                                                               |
| :----------------------- | :-----------------------: | :--------------------------------------------------------------------------------------------------------------- |
| Course attendance        |            26.5           | 20 meetings, at 80 minutes per meeting                                                                           |
| Assigned readings        |            30.5           | Weekly readings are assigned, and are expected to take roughly as long to complete as the in-seat time per week. |
| Final project            |            35             | Final project, as described above                                                                                |
| Homeworks                |            28             | 4 Labs, at approximately five hours per lab spent out of class (20 hours), plus the midterm at approximately 8 hours|
| Total hours:             |            120            |                                                                                                                  |

# Attendance and Absence Guidelines
Attendance at all class and discussion groups is expected and required, as described above.

Students must contact the instructor in case of illness or emergencies that preclude attending class sessions. Messages can be left on the instructor's voice mail or e-mail at any time of the day or night, prior to class. If no prior arrangements have been made before class time, the absence will be unexcused. Excused absences will involve make-up assignments, with make-up assignment procedures to be discussed in class on the first day. 

If you are unable to complete an assignment due to a personal and/or family emergency, you should contact your instructor or discussion leader as soon as possible. On a case-by-case basis, the instructor will determine whether the emergency qualifies as an excused absence. 

# Expected Classroom Behavior
Students are expected to participate in classroom activities. If use of digital devices or engagement in other non-class activities during class for purposes not regarding classroom activities is critically necessary, the student should engage in these activities while on break, or check with the instructor to arrange for a 10-minute break for unusual circumstances, but only if critically necessary. 

# Course Policies 
## Diversity, Equity and Inclusion
It is the policy of the University of Oregon to support and value equity and diversity and to provide inclusive learning environments for all students.  To do so requires that we: 

* respect the dignity and essential worth of all individuals. 
* promote a culture of respect throughout the University community. 
* respect the privacy, property, and freedom of others. 
* reject bigotry, discrimination, violence, or intimidation of any kind. 
* practice personal and academic integrity and expect it from others. 
* promote the diversity of opinions, ideas and backgrounds which is the lifeblood of the university.

In this course, class discussions, projects/activities and assignments will challenge students to think critically about and be sensitive to the influence, and intersections, of race, ethnicity, nationality, documentation status, language, religion, gender, socioeconomic background, physical and cognitive ability, sexual orientation, and other cultural identities and experiences. Students will be encouraged to develop or expand their respect and understanding of such differences.

Maintaining an inclusive classroom environment where all students feel able to talk about their cultural identities and experiences, ideas, beliefs, and values will not only be my responsibility, but the responsibility of each class member as well. Behavior that disregards or diminishes another student will not be permitted for any reason. This means that no racist, ableist, transphobic, xenophobic, chauvinistic or otherwise derogatory comments will be allowed. It also means that students must pay attention and listen respectfully to each other’s comments.

## Documented Disability
Appropriate accommodations will be provided for students with documented disabilities. If you have a documented disability and require accommodation, arrange to meet with the course instructor within the first two weeks of the term. The documentation of your disability must come in writing from the Accessible Education Center in the Office of Academic Advising and Student Services.  Disabilities may include (but are not limited to) neurological impairment, orthopedic impairment, traumatic brain injury, visual impairment, chronic medical conditions, emotional/psychological disabilities, hearing impairment, and learning disabilities. For more information on Accessible Education Center, please see http://aec.uoregon.edu 

## Mandatory Reporting of Child Abuse
UO employees, including faculty, staff, and GEs, are mandatory reporters of child abuse. This statement is to advise you that that your disclosure of information about child abuse to a UO employee may trigger the UO employee’s duty to report that information to the designated authorities. Please refer to the following links for detailed information about mandatory reporting:
http://hr.uoregon.edu/policies-leaves/general-information/mandatory-reporting-child-abuse-and-neglect

## Reporting Title IX Experiences
Any student who has experienced sexual assault, relationship violence, sex or gender-based bullying, stalking, and/or sexual harassment may seek resources and help at safe.uoregon.edu. To get help by phone, a student can also call either the UO’s 24-hour hotline at 541-346-7244 [SAFE], or the non-confidential Title IX Coordinator at 541-346-8136. From the SAFE website, students may also connect to Callisto, a confidential, third-party reporting site that is not a part of the university. 

Students experiencing any other form of prohibited discrimination or harassment can find information at aaeo.uoregon.edu or contact the non-confidential AAEO office at 541-346-3123 or the Dean of Students Office at 541-346-3216 for help. As UO policy has different reporting requirements based on the nature of the reported harassment or discrimination, additional information about reporting requirements for discrimination or harassment unrelated to sexual assault, relationship violence, sex or gender based bullying, stalking, and/or sexual harassment is available at http://aaeo.uoregon.edu/content/discrimination-harassment 

Specific details about confidentiality of information and reporting obligations of employees can be found at https://titleix.uoregon.edu. 

## Academic Misconduct Policy
All students are subject to the regulations stipulated in the UO Student Conduct Code http://conduct.uoregon.edu). This code represents a compilation of important regulations, policies, and procedures pertaining to student life. It is intended to inform students of their rights and responsibilities during their association with this institution, and to provide general guidance for enforcing those regulations and policies essential to the educational and research missions of the University.  

## Conflict Resolution 
Several options, both informal and formal, are available to resolve conflicts for students who believe they have been subjected to or have witnessed bias, unfairness, or other improper treatment. 

It is important to exhaust the administrative remedies available to you including discussing the conflict with the specific individual, contacting the Department Head, or within the College of Education, you can contact the Associate Dean for Academic Affairs and Equity (Krista Chronister, 346-2415, kmg@uoregon.edu). Outside the College, you can contact: 
* UO Bias Response Team: 346-3216 http://bias.uoregon.edu/whatbrt.htm 
* Affirmative Action and Equal Opportunity: 346-3123 http://aaeo.uoregon.edu/

## Grievance Policy
A student or group of students of the College of Education may appeal decisions or actions pertaining to admissions, programs, evaluation of performance and program retention and completion. Students who decide to file a grievance should follow the student grievance procedure, or alternative ways to file a grievance outlined in the Student Grievance Policy (https://education.uoregon.edu/academics/student-grievance) or enter search: student grievance.

## In Case of Inclement Weather 
In the event the University operates on a curtailed schedule or closes, UO media relations will notify the Eugene-Springfield area radio and television stations as quickly as possible. In addition, a notice regarding the university’s schedule will be posted on the UO main home page (in the “News” section) at http://www.uoregon.edu. Additional information is available at http://hr.uoregon.edu/policy/weather.html.

If an individual class must be canceled due to inclement weather, illness, or other reason, a notice will be posted on Canvas or via email. During periods of inclement weather, please check Canvas and your email rather than contact department personnel. Due to unsafe travel conditions, departmental staff may be limited and unable to handle the volume of calls from you and others.

## Course Incomplete Policy
Students are expected to be familiar with university policy regarding grades of “incomplete” and the time line for completion. For details on the policy and procedures regarding incompletes, Please see: https://education.uoregon.edu/academics/incompletes-courses
