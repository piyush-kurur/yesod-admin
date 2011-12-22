Yesod Admin
-----------

The aim is to provide a admin interface to persistent data types. I am
happy to receive your comments/suggestion/patches. In case you want to
report a bug or demand a feature please open an issue on github.

http://github.com/piyush-kurur/yesod-admin

Here are few things I would like of an admin site.

  1. Easy and efficient creation of admin interfaces: The user should
     be able to easily create an admin site for the objects of
     interest. This will invariably require some template haskell.

  2. Good haddock documentation: Always a work in progress

  3. As much as possible stick to the Yesod philosophy (type safety
     etc.)
  
  4. Simple yet flexible themeing options: If the site has to look
     pretty there should be lots of thought on the underlying
     CSS/JavaScripts etc.  I would like to separate, as much as
     possible, the logic and the presentation. The module Y.A.Render
     is an attempt at this.

There are some side goals as well. Although template haskell would be
the recommended approach, I would like to have a simplified and well
documented base types and classes. This way it would be easy for
developers to do nonstandard things with its type. Besides, I think
this would force developers to think more about the types and classed
defined.


TODO
----

The site is hardly complete. However, I think the right design is there.

1. Currently the site is capable of just listing the objects. The
   other crud operations have to be implemented.

2. A good form interface. I currently have no clue on how to go about
   doing this.

3. Admin actions: One should be able to define entity specific
   actions. The right place to launch those actions are from the
   object listing. As a first step one should be able to select
   objects and delete them.

4. Cleaning up the default rendering. One needs to test the CSS with
   different browsers and also clean up some mess that are preset
   there

5. Better documentation.
