let true = \x.\y.x in 
let false = \x.\y.y in 
let if = \b.\l.\r.(b l) r in
if true then false else true;

