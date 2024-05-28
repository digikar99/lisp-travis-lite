# Motivation

[cl-travis](https://github.com/lispci/cl-travis) depends on [cim](https://github.com/sionescu/CIM) which is [no longer supported](https://keens.github.io/blog/2017/01/29/deprecating_cim/). In addition, I personally found cl-travis more complex than necessary. So, here's another attempt at a lighter version of it.

Agreeing with Keen, the core developer behind CIM, I'm also maintaining a run.py, which should hopefully turn out easier to maintain.

roswell is another alternative - and I'm using roswell binaries below! Hopefully, this repository is easier to maintain than roswell for the common implementations used below. Also, roswell is much slower than cl-travis. lisp-travis-lite aims to provide the speed of cl-travis: near instant installs.

## Support Matrix

### run.sh

Only quicklisp. No clpm.

TODO: Enable ecl for macosx

| Impl \ OS (arch) | linux (x86\_64) | linux (arm64) | macosx (x86\_64) | macosx (arm64) |
|------------------|-----------------|---------------|------------------|----------------|
| sbcl             | ✓               | ✓             | ✓                | ✓              |
| allegro          | ✓               | ✗             | ✗                | ✓              |
| ccl              | ✓               | ✗             | ✗                | ✗              |
| ecl              | ✓               | ✗             | ✗                | ✗              |
| abcl             | ✓               | ✗             | ✗                | ✗              |

### run.py

Both quicklisp and clpm, except on abcl.

TODO: Port arm64 support to CCL.

| Impl \ OS (arch) | linux (x86\_64) | linux (arm64) | macosx (x86\_64) | macosx (arm64) |
|------------------|-----------------|---------------|------------------|----------------|
| sbcl             | ✓               | ✗             | ✓                | ✓              |
| ccl              | ✓               | ✗             | ✗                | ✗              |
| ecl              | ✓               | ✗             | ✗                | ✗              |
| abcl             | ✓               | ✗             | ✗                | ✗              |


## Dependencies

Common:

- quicklisp
- If you are using run.py: clpm
- ecl_bin
- abcl repositories

Linux:

- roswell binaries for sbcl and ccl
- wget

MacOS:

- brew
- wget

