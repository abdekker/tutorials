### Markdown
Markdown is a system for writing content for the web (https://en.wikipedia.org/wiki/Markdown)

This tutorial has been adapted from:  
    - https://www.markdowntutorial.com  
    - https://www.markdownguide.org/basic-syntax/  

In Linux, the Atom text editor has a core package called **markdown-preview**  
To view how Markdown text will render, toggle with Ctrl + Shift + M.

View this file in a standard ASCII editor to understand the examples.

###### Headings
Six levels are supported from h1 (largest) to h6 (smallest).  
Put one hash ( \# ) for each level before text eg. \#\#\# Heading 3  

###### Paragraphs and line breaks
Paragraph: Separate text with one or more blank lines  
Line break: End line with two or more spaces (not required for headings)

###### Bold
Nest text with double asterisk ( \*\* ) or underscore ( \_\_ ): Next word is **bold**. Nice!

###### Italics
Nest text with single asterisk ( \* ) or underscore ( \_ ): Next word is in *italics*. Cool!

###### Bold and Italics
Nest text with triple asterisk ( \*\*\* ) or underscore ( \_\_\_ ): Very ***important*** note!!

###### Blockquotes
Place a greater than ( \> ) before text (see example below)
> This is an example of a simple blockquote

> This example has a nested blockquote on the next line...
>> ...and here is the nested blockquote

###### List (ordered)
To create a numbered list, add line numbers followed by a period. There is an example below.

1. List 1
2. List 2
3. List 3

###### List (unordered)
Unordered (or bullet) lists have a single dash ( \- ), asterisk ( \* ) or plus ( \+ ) symbol in front of items.

+ Bullet 1
+ Bullet 2
+ Bullet 3

###### List with inserted elements
Indent the inserted item with four or more spaces
1. Item 1
2. Item 2

    Inserted paragraph after the second list item

3. ...and the list continues with item 3

This example embeds an image in a list
1. Open an image of the Linux mascot.
2. Marvel at its beauty.

    ![Tux, the Linux mascot](./Tux-small.png)

3.  Close the file.

###### Tables
Tables are defined using three (or more) rows. Trailing spaces are ignored.
+ 1st row: Use pipe (|) characters to define columns and provide titles
+ 2nd row: Use one or more hyphen (-) characters, separated by pipes (|)
+ 3rd+ row: Provide text for each row in the table

Column 1 | Column 2 | Really Long Column Name
-- | -- | --
This row has | text in all | columns
2nd column is blank | | Text for 3rd column
3rd column is missing | Text for 2nd column

###### Code samples
Nest code samples with tick marks ( \` ): Here is a function `auto sum(auto a, auto b)`

###### Links
Enclose the link text in square brackets followed by the URL in parentheses.  
An excellent search engine is [Duck Duck Go](https://duckduckgo.com).

Text enclosed in angle brackets ( \<\> ) also becomes a link...  
<fake@example.com>  
<https://bbc.co.uk>

###### Displaying special characters
Use a backslash ( \\ ) to escape special characters.

### And that's the end...
