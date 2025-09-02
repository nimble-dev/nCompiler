These are old tests.
In setting up CI testing using GitHub actions, we used setup-r-dependencies.
This aggressively scans all directories doing static code analysis of .R files
to discover hidden dependencies, for example that may be in testing only.
This causes problems for nCompiler because it does things like dynamically generate
and build packages on the fly, and then check them. So it looks like there are
package dependencies where there aren't. We have worked around this in the
current test suite. However, in retaining these old tests for reference
(eventually they will be removed), we simply renamed the files so they
don't have a .R extension. The new extension is .R_noscan to indicate
they were renamed in order to avoid static code analysis scanning.
