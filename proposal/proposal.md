Project Proposal
================

Who are the members of your team?
---------------------------------

Alex Curtiss, Seth Hovestol, Izaak Weiss

What basic problem will your project try to solve?
--------------------------------------------------

More than ever, modern websites are a complex application requiring coordination between client and server continuously, rather than the standard HTTP Request/Response protocol. To create such a website requires two codebases which must seamlessly communicate with each other, and which are often written in different languages. We want to explore the idea of a programming language that is written as one codebase that compiles to both a server and a client-side program, automating network calls as a stage during compilation. 

Define the problem that you will solve as concretely as possible. Provide a scope of expected and potential results. Give a few example programs that exhibit the problem that you are trying to solve.
-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

We want a single language that automatically generates code for network calls between a client and server. The foundational features of our language will be (1) creating a foreign function interface for both frontend and backend javascript, and (2) identifying locations requiring a network call 

What is the general approach that you intend to use to solve the problem?
-------------------------------------------------------------------------

We want to write a language allow for both server-side and client-side computation, determines what network calls are needed to tie the two together as neatly as possible, and generates both client- and server-side Javascript. 

Why do you think that approach will solve the problem? What resources (papers, book chapters, etc.) do you plan to base your solution on? Is there one in particular that you plan to follow?
---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

We have yet to find any other attempts at what we’re doing. Our project is somewhat inspired by Elm, a web programming framework that abstracts away direct manipulation of HTML using the DOM, which we’ve found to make web programming significantly less painful. However, network calls remained difficult in Elm, and our goal is to similarly abstract them away.

Some libraries attempt to provide similar abilities; such as https://pythonhosted.org/Pyro4/intro.html, which is a library for turning python function calls into networked communication. While this abstracts network calls in a similar manner, it does not automate the process nor does it run in the browser. We also believe that by making this behaviour a product of the compiler should let us automatically identify locations to insert network calls, thus avoiding the need for manual annotation.

We also have drawn parallels to garbage collection; once a purely manual affair, garbage collection is now almost entirely automated in everyday programming, using various methods from a full Python, Java, or Go style garbage collection to simpler C++ smart pointers. Why not attempt to do the same for network calls?

What about your solution will be similar? What will be diﬀerent?
----------------------------------------------------------------

The language builds off the groundwork laid by Node.js, which allowed the same language to be used on both the client and server. We will expand on this by allowing both sides of the website to be combined into one set of source files. Additionally, we hope to drastically reduce the amount of code needed to communicate between the client and server.

How do you plan to demonstrate your idea? Will you use your course compiler. If so, what specific changes do you plan to make to it (e.g., what passes need to be changed or added)?
------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

We plan on writing a brand new compiler capable of turning a source file in our language into two javascript files. Then using node, we will serve one of the sources to browsers, and use the other to serve data to that page given requests.

How will you evaluate your idea? What will be the measurement for success?
--------------------------------------------------------------------------

We’ll be happy if we can compile a website which allows for the access and retrieval of data on the backend based on some user input on the frontend. A high, and maybe unattainable, goal would be to allow streams of data to the frontend based on user input on the front end (both a udp structure and tcp structure). 

