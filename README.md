# Learning GitHub for Growth Analyses

The primary purpose of this repository is to help us learn and practice using version control and GitHub, R and Rstudio, and Machine Learning.In addition, this is a common workspace to share code manuscripts and other interesting stuff to help us develop and collaborate on our project for analyzing children growth data.

## Objectives

- Familiarize with version control concepts.
- Learn how to use GitHub.
- Improve collaboration and efficiency.

## Getting started 

### "Clone" (copy) the repository to your computer

 - At the top right corner of this page you will see a green button saying "Clone or download". 
 - After clicking this button, you will be given a series of options to copy the contents of the repository locally in your computer. The simplest option for you is the "Open in Desktop". 
 - This step requires to have the Github application installed in your computer. If you do not have the application, install it from here: https://desktop.github.com/. 
 - Once you have installed the application, you can return here and click the "Clone or download" button and then follow the instructions of the application. When the repository is cloned in your desktop you can find it in the directory that the application has created for you. Alternatively, you can open the repository locally by selecting Repository->Show in Explorer from the menu of the application.
 - From there, you can start working in Rstudio by setting this as your working directory in Rstudio. Go to Rstudio and from the menu go Session->Set Working Directory->Choose Directory... and pick the directory of the cloned repository. At the right bottom of the Rstudio screen in the "File" pane, choose More->Go To Working Directory. Now, you should be able to see the contents of the repository in your Rstudio screen. 

### Make your first commit!

- In the code directory of this project, there is a R file called "hello_world.R". Open it in your Rstudio and add a greeting under the last line.
- This is your first change to the project. If you open the GitHub application, you will see that it has detected a changed file. You will need to commit this change so that it becomes visible and available to everybody else in the project.
- The screen of the application is split in two parts. The right part contains the changes in the selected file. The left top part contains all the files that have been changed. You can select a file to see the changes in the right part. Under the list of changes there is a text field with your picture from GitHub. Add a short commit message there such as "Update hello_world.R". You can add more descriptive information in the text field below.
- Once you have added all the messages and selected all files that you want to commit to the repository, you can hit the blue button that says "Commit to master".
- You are not done yet! This action committed your changes to your local copy, but not to the actual repository! For this you will need to also hit the blue button "Push origin" that will appear in the next screen.
- Attention! While you edit your code, somebody else may have edited the same files and have already pushed the changes to the repository, the GitHub application will not let you push your changes until you have pulled the previous changes. Follow the instructions of the application to first pull the latest version of the repository and then push your changes.
- Congratulations! You just made your first contribution to a collaborative project! You can see your version in your browser and in the GitHub application as well.

### Collaborate through issues

- Issues are a good way to track your tasks, communicate your work organizations with your team and coordinate your activities to fix bugs or provide enhancements. 
- For example, when you find a problem in a R script that somebody else in the team is developping, instead of sending an email or an instant message (which is not recorded and may get lost), you can create an issue. 
- In the issue, you specify a) a short description of the problem, issue, feature, b) the file that may be associated with this problem, c) the priority of this issue (if it is critical and needs to be fixed ASAP), d) a developer that will be responsible for this issue. 
- Another example is the following. After a meeting you decide that you need to develop some new features. You create issues for this features and this way you can plan the development work.
- Once the associated task is completed, you can close the issue and this notifies the rest of the team that this task is completed and ready to be used (without the need of an email). 
- In the end the history of commits and issues constitutes the history of the project, where you can track a) everything that has happened, b) who did what, c) what problems occured during the project (so that you can avoid them) and others.

