( simple modules )

comment ~
Modules allow us to remove the clutter of private words in a module.
We have two choices:
1. Just unlink the private headers
2. Save the memory by putting them in another location

Alternately we can just use vocabularies.

Let's try the super simple model from Dewey Val Schorre FD 2/5

internal
: private-word ;
: something ;

external
: public-word private-word ;

: something something ;

module
~

: internal  current @ @ ;
: external  align here ;
: module    ! ;

\ But this requires all external words together
\ How about we elevate selective words (from Hoffmann modules)
\ : export ( <name> -- )
\     >in @ ' swap >in ! create , does> @ execute ;
