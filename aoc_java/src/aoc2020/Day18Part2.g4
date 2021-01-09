grammar Day18Part2;

// Slightly silly parser, where addition has higher precedence than 
// multiplication.

expr
 : expr '+' expr # addopGRP
 | expr '*' expr # mulopGRP
 | NUMBER        # literalGRP
 | '(' expr ')'  # parenGRP
 ;

NUMBER    : [0-9]+;
SPACE     : [ \t\r\n] -> skip;
