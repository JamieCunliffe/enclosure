# Enclosure
Emacs port of surround.vim

This adds the ability to add/change/delete pairs of brackets (or arbitrary strings) around expressions.

# Examples
Note: | denotes cursor position

With the example string "(defun somefunc()(interac|tive))"

Running `enclosure-change` with change string being ( and with string being & would give "(defun somefunc()&interactive&)

Running `enclosure-delete` with the delete char being ( would give "(defun somefunc()interactive)"

Running `enclosure-thing-at-point` with the string being ^ would give "(defun somefunc()(^interactive^))"

# Functions
`enclosure-change` - Prompts for a string search for and then a string to replace that surrounding pair with

`enclosure-delete` - Deletes the specified surrounding pair

`enclosure-region` - Adds a surrounding pair to the region

`enclosure-thing-at-point` - Adds a surrounding pair to the `thing-at-point`

# Customisation
The variable `enclosure-chars` is where the pair matching is taken from, by default this is
```
(defvar enclosure-chars
  '((:beginning "(" :end ")")
    (:beginning "{" :end "}")
    (:beginning "[" :end "]")
    (:beginning "<" :end ">")))
```
More pairs can be added by customizing the enclosure-chars variable, for example:

* If you write bash scripts you could add ${ } as a pair with the following

```
(setq enclosure-chars (append enclosure-chars '((:beginning "${" :end "}"))))
```

* If you use Scala with doobie you could add the sql""" """ as a pair by adding the following to your config:

```
(setq enclosure-chars (append enclosure-chars '((:beginning "sql\"\"\"" :end "\"\"\""))))
```
