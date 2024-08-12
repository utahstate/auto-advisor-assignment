# auto-advisor-assignment
Ellucian Banner Custom package to automatically assign advisors for program, campus, attributes, etc.
Oracle Objects have been lable labled by type. (Table, Trigger etc)
All of the JSON objects are for the pagebuilder page.

There are a couple of known bugs
1- When changing student information in SGASTDN, the Advisor rules are not getting re-evaluated.
2- When somebody changes a past term, there are occasions where the advior from the past term is 
  being assigned to the student and removing the current advisor from the more current term from 
  the student.
