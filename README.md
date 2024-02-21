# Administration aid tool

## Initial observations

To first use the tool, please, log in using:
login: admin
password: admin1

The tool is in portuguese because is the language my sister speaks 😌.

I also know that keeping data in .csv files (mainly logins and passwords) are not good practice, but remember that this tool was developed to only one or two people use and there is no need for scalability. Speaking of, loading every database when the tool is starting is not good practice as well, but again, this tool was developed for a small amount of data.

## Content

1. [Overall look](#1-overall-look)
2. [Functions embed](#2-functions-embed)
3. [Design](#3-design)
4. [Structure](#4-structure)
5. [Libraries used](#5-libraries-used)
6. [Results](#6-results)
7. [Server side](#7-server-side)
8. [Final considerations](#8-final-considerations)

---

## 1. Overall look

The objective of this tool is to design a tool to help my sister out on her **pet-shop and grooming business**. She is starting now, so the tool is for small and emerging business. This tool will be hosted in a **home server** and will only work on her wifi's network, providing safety and reducing her costs. It will be developed using **Shiny framework and R language** to improve my skills on those. This administration aid tool will have:

* Inventory control;
* Selling control & visiting checkpoint;
* Calendar/Schedules;
* Cash book;
* Prices simulation;
* Clients and plans catalogue;
* Invoice control for taxes;
* Additional security step using login and password;

All the data is stored in .csv files because it the project was personally developed and it works locally.

## 2. Functions embed

###  Inventory Control

This section has 2 tables: one with the historical of all the products bought by her and other with the inventory itself. In each one there is a modal to add the products. If the product bought is already in the inventory, the amounts is increased. It is possible to edit and remove rows in both tables. When it is done in the buying table, it reflects on the inventory table.

### Clients and plans catalogue

On this section, there are 2 tables: one to add clients, with names, birthdays, and contact information; and another to create her grooming plans with selling and cost prices, name and plans description.

### Selling control & visiting checkpoint

This section has 3 tables:

* Selling product: she selects products from the inventory and adds to the shopping cart. Then she can edit the value and the quantity bought and ends the sell.
* Selling plan: she selects a client and a plan. The number of remaining visits is altered in the clients' table.
* Visit checkpoint: she selects a client to checkpoint a visit. The number of remaining visits is altered in the client's table.

### Calendar/Schedules

This is an outside widget that allows her to schedule events. It shows the all the week with the hours of the day as google calendar. When creating an event, she has to add the name, date and hour, location, description,  if the event is all day or not, and the cards' background color. It is possible to remove or edit an event by clicking on the card.

 It gets the birthdays from the clients table and creates them as events for the next 10 years. If the dates are edited or the client removed, so it happens to the calendar's events.

 ### Cash book

 It shows a monthly money enter based on the sells table, a savings, a monthly expenses and the value in the inventory cards. It has a time-series line graph with sells vs buying. Lastly, it has a donut graph with the category of the sold products.

 ### Prices simulation

 In this section, she can simulates the selling prices for services and products, based on desired profit, taxes, and buying costs. To simulate services products, she can add more n more products used in that service and input the amount used and price per unit for a higher accuracy.

 ### Invoice control

It provides a .word document containing a table with all the products, the value and the invoice number of the products bought in the input year.

### Login

As a security step (not necessary though), I included a login function. In the users database, there is a column indicating the permission of the user. If it is an admin, it can see the [cashbook](#cash-book) and the [invoice control](#invoice-control) pages.


## 3. Design

The design was primarily made on a pptx, present in this [link](Design.pptx). The colors selected was thought based on a mix of happy (since dealing with animals), and cleaning (grooming activities). The fonts used were thought based on something more fluid and extrovert. The icons were obtained from PPT itself.

In the tool, the style was set using this [css](./shiny-app/www/styles.css) file.

## 4. Structure

The tool structure consists on a main script containing the [Ui and server side](./shiny-app/app.R), other scripts in the folder [pages](./shiny-app/pages/), and the data in the folder [data](./shiny-app/data/). The scripts on pages folder has functions containing Ui and back-end. The main script load the functions while loading using the function "source()". It also loads the data using the function "read.csv()" when starts. The overall look is as follow:

+- app.R

+- pages

+--- files.R containing Ui and backend functions for each [functions embed topic](#2-functions-embed).

+- data

+--- .csv files containing the tables used in the tool.

## 5. Libraries Used

| Libraries | Function |
|--- | --- |
| shiny, shinydashboard, shinyWidgets, shinythemes, shinyjs, shinycssloaders and shinyDatetimePickers | Libraries used to render the shiny application, with dashboard style, different themes, allows to use javaScript, render a loader and a date-time widget |
| DT | Render and perform operations on tables |
| tidyverse, dplyr | Used to perform operations in data frames |
| rmarkdown | Used to created the .word document for the report |
| lubridate | Used to handle with dates |
| ggplot2 | Used to create graphs |
| colourpicker | Pick colors for the calendar label |
| toastui | Calendar widget |

## 6. Results

Below there is a screen recording using the tool.

[![Check it out](Record.mov)](https://github.com/GustavoSantiago113/Administration-Help-Tool/assets/104911112/a7ae8c7d-acff-45d3-a958-1369e2d59ec1)

As shown, it is possible to see that the user can login, and perform some operations. To add data to the tables, the user clicks and opens a modal, then the user fills the form and presses to add. To remove, the user has to click on the desired row and click on delete. To edit, user have to double click in the desired cell, insert the new value and click outside the table. Some tables allow to remove more than one row, while others allow just one row at a time.

The calendar section successfully creates events. However, it has a bug that does not automatically render the event in the correct time, being necessary to refresh the page. 

The simulations' section allows to add more than one product and instantly returns the price.

The clients' dates appear in the calendar as events and are editable.

The cashbook section shows the all the desired data, even when there is a deficit on the savings, return as a negative value.

The login section blocks any unwanted user and successfully allows the usage of correct users. It also applies the filter, allowing only admins to visualize the cashbook and invoice control sections.

The invoice control section generates the .word document, available to download after a loading widget.

Regarding the layout, sometimes the page does not render completely, being necessary to minimize and maximize the page. (I don't know why this happens).

## 7. Server side

The server hardware is an old Aspire F5 computer with 8GB RAM and an Intel i5 processor. To run the application I used Docker with the configurations present on the [dockerfile](Dockerfile).

The tool was imported using git, an image created and a container set to run when docker is running. I also set docker to run when the computer starts.

## 8. Final considerations

This was my first attempt to build something with data connections, personal touches (name, picture and permissions), develop something to work on server, and I could give some help to my sister. The tool is not perfect, with some rendering and login problems, but was enough for me to learn more about this topic.

Next steps, if necessary, are to:
* Figure out the rendering problems;
* Remove the necessity to login every time the page is refreshed;
* Build an own calendar widget to handle the data adding issues;
* Change the database to NoSQL or SQL structure, instead of .csv;

**If you are interested in building this kind of thing for your company or organization, reach me out. The format, inputs and database can be changed to fit your needs.**
