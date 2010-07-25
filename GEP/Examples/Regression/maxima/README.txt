Maxima client README
--------------------

In the regression example, the output of the code is often something
similar to:

INFIX : ((((((((((y/y)+x)-((y-x)+(y/y)))-y)-((((x+x)-(x-x))-((y-y)+x))+(y*(y-y))
))+y)+((((((((y+x)/x)-((y/x)-(x/x)))*(x-x))+y)*x)*x)-x))+(((y*(x+y))/x)*x))+(y-x
))+x)

This is less-than-informative if what we really were looking for was a
polynomial written in a form that we are used to reading:

                                 2    2
(%o1)                           y  + x  y + x y

The code in this directory can be used to allow a client to send the
raw infix-printed string representing an expression tree produced by the
regression example to a server that will use Maxima to reduce the
expression to a simpler polynomial form.
