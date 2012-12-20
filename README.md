run `make` to build three variants of Increase and dump the core.

The code depends on three CPP flags.

  <none> -- this most straightforward code incurs mutually recursive
  dictinaries, which prevent inlining.

  FAST -- avoids the problematic mutual recursion at the cost of a simple
  additional instance.

  FORCE-SC -- if FAST is on, this forces SpecConstr.  For our little example at
  least, seems to merely elimate some casts.

Current Task
------------

Even with -DFORCE-SC, the core dumped by -DFAST -ddump-simpl still seems to
scream for SpecConstr. I'm now trying to make that optimization happen at the
right time.
