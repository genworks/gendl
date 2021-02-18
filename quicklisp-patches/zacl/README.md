# ZACL

ZACL is for running
[AllegroServe](https://github.com/franzinc/allegroserve/) on
non-AllegroCL Lisps. The goal is to run AllegroServe sources with as
few modifications as possible, so that such a modified version of
aserve will continue to be able to track future versions of [official
aserve](https://github.com/franzinc/allegroserve/). That means
*occasionally* resorting to grody one-off hacks instead of elegant
beautiful gracefulness. 

Where possible, ACL functionality is mimicked with portability
libraries like [UIOP](http://quickdocs.org/UIOP/),
[bordeaux-threads](http://quickdocs.org/bordeaux-threads/),
[cl-ppcre](http://weitz.de/cl-ppcre/),
[USOCKET](https://common-lisp.net/project/usocket/), etc. Where
necessary, implementation-specific hacks are used to fill in the gaps.

For example, Allegro CL's non-standard `#+(version>= ...)` expression
is sprinkled throughout aserve code. To work around the problem
without changing the code, zacl installs a readtable that overrides
the standard `#+` and `#-` readers to check for these expressions and
work around them.

Occasionally, quirks in aserve's source may be very difficult to work
around. In those cases, a patch or pull request can be [sent to Franz]
(https://github.com/franzinc/aserve/). Ideally, the [official
aserve](https://github.com/franzinc/aserve/) will eventualy get to
where it will work with ZACL unmodified.

But as long as there are discrepancies between what works with zacl
and the [official Franz version of
aserve](https://github.com/franzinc/aserve/) (as is the case
currently), a zacl-compatible fork will be available
[here](https://github.com/genworks/aserve/tree/zacl-compatible).  You
can check it out like so:

    git clone -b master https://github.com/genworks/aserve.git

## Usage

The `aserve` system definition name conflicts in Quicklisp with a
legacy [attempt to port
AllegroServe](https://sourceforge.net/p/portableaserve/git/ci/master/tree/)

Therefore, the [zacl-compatible
AllegroServe](https://github.com/genworks/aserve/tree/zacl-compatible)
has its system name as  `zaserve`. This `zaserve` system definition
includes a dependency on ZACL, so you can load it simply by invoking

    (ql:quickload :zaserve)

Of course, if for some reason you only want the `zacl` system itself,
you can load that with

    (ql:quickload :zacl)


## Limitations

**Only works on Clozure CL and SBCL**. Support for other
implementations may be added if there is sufficient
demand. Contributions of ports to other implementations are of course
welcome

**Does not support CGI**. A victim of time constraints. The author and
maintainers do not currently need CGI. CGI functionality requires
non-trivial support for managing subprocesses, and I am not sure if
e.g. UIOP's subprocess management API is sufficient.

**Lightly tested**. Our applications seem to work. Yours might not. If
you have trouble with ZACL and zaserve, feel free to create an
issue[here](https://gitlab.common-lisp.net/zbeane/zacl/issues) and/or
[here](https://github.com/genworks/aserve/tree/zacl-compatible) and
the maintainers will see them.

**Possibly slow**. Using generic functions and portability layers will
be slower than running aserve natively on Allegro CL. There aren't
many ways around it. If you do have a specific performance issue when
running with ZACL that you would like to see ZACL try to address, then
please [open an
Issue](https://gitlab.common-lisp.net/zbeane/zacl/issues) or [get in
touch](mailto:xach@xach.com).

With that said, AllegroServe will always run best on its native
platform, which is [Allegro
CL](https://franz.com/products/allegro-common-lisp/). So if you want
to run AllegroServe for mission-critical or performance-critical
applications, and with a guarantee of a supported, trouble-free
experience, then you may want to consider investing [Allegro
CL](https://franz.com/products/allegro-common-lisp/) for your
AllegroServe needs. This option also comes with the benefit of
dedicated, world-class [support from Franz
Inc. engineers](https://franz.com).


