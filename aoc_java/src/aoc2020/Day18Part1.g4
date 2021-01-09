grammar Day18Part1;

// Slightly silly parser where '*' and '+' have the same priority, parsed
// left-to-right.

expr
 : expr binop=('*'|'+')  expr # binopGRP
 | NUMBER         # literalGRP
 | '(' expr ')'   # parenGRP
 ;

NUMBER    : [0-9]+;
SPACE     : [ \t\r\n] -> skip;
