# Enclosure
Emacs port of surround.vim

# Functions
`enclosure-change` - Prompts for a string search for and then a string to replace that surrounding pair with

`enclosure-delete` - Deletes the specified surrounding pair

`enclosure-region` - Adds a surrounding pair to the region

`enclosure-thing-at-point` - Adds a surrounding pair to the `thing-at-point`

# Customisation
The variable `enclosure--chars` is where the pair matching is taken from, by default this is
```
(defvar enclosure--chars
  '((:beginning "(" :end ")")
    (:beginning "{" :end "}")
    (:beginning "[" :end "]")
    (:beginning "<" :end ">")))
```
More pairs can be added for, instance, if you use Scala with doobie you could add the sql""" """ as a pair by adding the following to your config:
```
(setq enclosure--chars (append enclosure--chars '((:beginning "sql\"\"\"" :end "\"\"\""))))
```
And then sql""" would be correctly matched when used as a pair.
