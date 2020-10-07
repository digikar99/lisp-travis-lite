# Motivation

While using CI for [py4cl2](https://github.com/digikar99/py4cl2), over the span of a year, I encountered about 2-3 breakages that effectively consumed my 4-5 working days. Initially, CI was set up for only SBCL and CCL using cl-travis. However, at some point, a newer version of CCL was required - therefore, ROS was used. ROS turned out to be very slow for ECL.

Even as of my current writing, [numcl is still failing over SBCL brought from cl-travis](https://travis-ci.org/github/digikar99/py4cl2/jobs/733744556). In fact, numcl and Java-ABCL compatibility have been the two major pain points. numcl loads good on my local system; so does ABCL with Java version 8.

## Should I use lisp-travis-lite?

If ROS or cl-travis can get your job done well, go with them! If not, either raise an issue here, or fork/local-copy this and use for your own purposes.

The system itself is incredibly simple, and solely depends on 

- [sbcl-images](https://github.com/digikar99/sbcl-images)
- [ccl-images](https://github.com/digikar99/ccl-images)
- ABCL jar files 
- Packaged ECL, courtesy of https://cdn.cddr.org/

No CIM. Nothing heavy like ROS. Of course, that "lite"ness gets you no support for Windows, or anything else beyond linux-x64-(sbcl/ccl/abcl/ecl) in fact!

