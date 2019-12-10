## Markdown
Markdown is a system for writing content for the web (https://en.wikipedia.org/wiki/Markdown)

This tutorial has been adapted from:
* https://www.markdowntutorial.com
* https://www.markdownguide.org/basic-syntax/

The Atom text editor has a core package called **markdown-preview**.  
To view how Markdown text will render, toggle with Ctrl + Shift + M.

Similarly, Visual Studio Code has a built-in preview mode.  
The default keybinging is Ctrl + Shift + V (changed this to be the same as Atom).

Alternatively, view this file in a standard ASCII editor to understand the examples.  

#### Headings
Six levels are supported from h1 (largest) to h6 (smallest).  
Put one hash ( \# ) for each level before text eg. \#\#\# Heading 3

#### Line breaks
Line break: End line with two or more spaces (not required for headings)

#### Bold
Nest text with double asterisk ( \*\* ) or underscore ( \_\_ ): Next word is **bold**. Nice!

#### Italics
Nest text with single asterisk ( \* ) or underscore ( \_ ): Next word is in *italics*. Cool!

#### Bold and Italics
Nest text with triple asterisk ( \*\*\* ) or underscore ( \_\_\_ ): Very ***important*** note!!

#### Blockquotes
Place a greater than ( \> ) before text (see example below)  
> This is an example of a simple blockquote

> This example has a nested blockquote on the next line...
>> ...and here is the nested blockquote

#### List (ordered)
To create a numbered list, add line numbers followed by a period. There is an example below.

1. List 1
2. List 2
3. List 3

#### List (unordered)
Unordered (or bullet) lists have a single dash ( \- ), asterisk ( \* ) or plus ( \+ ) symbol in front of items.

+ Bullet 1
+ Bullet 2
+ Bullet 3

#### List with inserted elements
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

#### Tables
Tables are defined using three (or more) rows. Trailing spaces are ignored.

+ 1st row: Use pipe (|) characters to define columns and provide titles
+ 2nd row: Use one or more hyphen (-) characters, separated by pipes (|)
+ 3rd+ row: Provide text for each row in the table

Column 1 | Column 2 | Really Long Column Title
-- | -- | --
This row has | text in all | columns
2nd column is blank | | Text for 3rd column
3rd column is missing | Text for 2nd column

Column widths are automatically calculated from contents. Note that table rendering varies widely between Markdown viewers.

#### Displaying special characters
Use a backslash ( \\ ) to escape special characters.

#### Code samples
Nest code samples with tick marks ( \` ). Here is a function declaration: `auto sum(auto a, auto b);`

Enclose language-specific code between two sets of triple tick marks. The top-most set of ticks is followed by the language tag, as in the example below.  
\`\`\`c++  
CODE SAMPLE  
\`\`\`

Example `C++` code (use "c", "cpp" or "c++")
```c++
typedef struct
{
    int32_t x;
    int32_t y;
    int32_t z;
} Point;
```

Example `Pascal` code (use "pas" or "pascal")
```pas
if (not isProblem) then
    begin
    CreateProblem();
    RunAway();
    end;
```

#### Links
Enclose the link text in square brackets followed by the URL in parentheses. An excellent search engine is [Duck Duck Go](https://duckduckgo.com).

Text enclosed in angle brackets ( \<\> ) also becomes a link...  
<fake@example.com>  
<https://bbc.co.uk>

### And that's the end...
