# haskell-adhoc

Various stuff I try to write in Haskell while learning this
beautiful language.

Haskell rocks!


:hoogle Monad

:seti -XNoMonomorphismRestriction

:load Dir.Module  => load compiled if exists (only gives exports)
:load *Dir.Module => load interpreted ;* (opens full scope)
:show modules

:show imports => show what's in scope

:browse Module - show module exports
:browse *Module - shows all stuff (imported there too)*

:show bindings -> list bound variables in scope

:set stop :list => print prog listing on hitting a breakpoint

:delete * => delete all breakpoints
