# Should install $LISP
# Should output $LISP --version
# Should install quicklisp by default
# Should install clpm by default
# Should push current directory to ql:*local-project-directories* if quicklisp is installed

# source run.sh to be able to run LISP
# Implementations should support --eval and --load arguments

import sys
import os
import stat
from subprocess import run

LISP = os.getenv("LISP").lower()
HOME = os.getenv("HOME")
PATH = os.getenv("PATH")
OS = os.getenv("OS")
DRY_RUN  = (False if os.getenv("DRY_RUN") is None else True)
if OS.startswith("ubuntu"):
	PLATFORM = "x86-64-linux"
	CCL_PLATFORM = "linuxx86"
elif OS in ["macos-11", "macos-12", "macos-13"]:
	PLATFORM = "x86-64-darwin"
	CCL_PLATFORM = "darwinx86"
elif OS.startswith("macos"):
	PLATFORM = "arm64-darwin"
	CCL_PLATFORM = "darwinarm"
else:
	raise Exception("Unknown OS: " + OS)

IGNORE_BACKTRACE = (False if os.getenv("IGNORE_BACKTRACE") is None else True)

LISP_INIT_FILE = {
	"sbcl": ".sbclrc",
	"ccl" : ".ccl-init.lisp",
	"ecl" : ".eclrc",
	"abcl": ".abclrc"
}[LISP]

def ensure_string(obj):
	if type(obj) == bytes:
		return obj.decode()
	elif type(obj) == str:
		return obj
	else:
		return str(obj)

def install_cl(cl_command):
	print("cl_command:", cl_command)
	bin_dir = os.path.join(HOME, "bin")
	cl_file = os.path.join(HOME, "bin", "cl")
	print("cl_file", cl_file)

	# PART 0: Make the directories
	print(PATH)
	# run(["mkdir", "-p", bin_dir], check=True)
	os.makedirs(bin_dir, exist_ok=True)
	os.chmod(bin_dir, 0o777)
	os.listdir(bin_dir)

	# PART 1: Prepare the cl file
	with open(cl_file, "w") as cl:
		cl.writelines([
			"#!/bin/bash\n",
			" ".join([cl_command, '"$@"', "--eval", "'(quit)'"])
		])

	os.chmod(cl_file, 0o777)
	print("Executable", cl_file, "?", os.access(cl_file, os.X_OK))
	print("Readable", cl_file, "?", os.access(cl_file, os.R_OK))

	with open(cl_file, "r") as cl: print(cl.read())
	cl_proc = run([cl_file, "--eval", "(quit)"], capture_output=True)
	print(
		ensure_string(cl_proc.stderr),
		ensure_string(cl_proc.stdout)
	)

	# PART 2: Install quicklisp
	install_quicklisp()

	# Part 3: Install clpm
	if LISP != "abcl": install_clpm()

	# Part 4: Print backtrace and ensure quitting
	with open(cl_file, "w") as cl:
		cl.write("#!/bin/bash\n")
		if IGNORE_BACKTRACE:
			cl.write(
				" ".join([
					cl_command,
					"--load", HOME+"/quicklisp/setup.lisp",
					"--eval", """'(setf *debugger-hook*
										(lambda (c h)
										  (declare (ignore c h))
										  (uiop:quit 1)))'""",
					'"$@"', "--eval", "'(quit)'"])
			)
		else:
			print("Avoiding trivial-backtrace...")
			cl.write(
				" ".join([
					cl_command,
					"--load", HOME+"/quicklisp/setup.lisp",
					"--eval", '\'(ql:quickload "trivial-backtrace" :silent t)\'',
					"--eval", """'(setf *debugger-hook*
										(lambda (c h)
										  (declare (ignore h))
										  (trivial-backtrace:print-backtrace c)
										  (uiop:quit 1)))'""",
					'"$@"', "--eval", "'(quit)'"])
			)
	os.chmod(cl_file, 0o777)
	print("Executable", cl_file, "?", os.access(cl_file, os.X_OK))
	print("Readable", cl_file, "?", os.access(cl_file, os.R_OK))


def install_quicklisp():
	print("Installing quicklisp...")
	os.chdir(HOME)
	run([
		"wget",
		"https://beta.quicklisp.org/quicklisp.lisp",
		"-O",
		HOME + "/quicklisp.lisp"
	])

	# Below we remove a prompt to append quicklisp autoload code to the implementations init file
	# Also, some implementations like ABCL necessitate an explicit one-by-one form evaluation
	cl_proc = run([
		"cl",
		"--load", "quicklisp.lisp",
		"--eval", "(quicklisp-quickstart:install)",
		"--eval", "(in-package :ql-impl-util)",
		"--eval", """
		  (defun add-to-init-file (&optional implementation-or-file)
			"Add forms to the Lisp implementations init file that will load
		  quicklisp at CL startup."
			(let ((init-file (suitable-lisp-init-file implementation-or-file)))
			  (unless init-file
				(error "Do not know how to add to init file for your implementation."))
			  (setf init-file (merge-pathnames init-file (user-homedir-pathname)))
			  (format *query-io* "~&I will append the following lines to ~S:~%"
					  init-file)
			  (write-init-forms *query-io* :indentation 2)
			  (with-open-file (stream init-file
									  :direction :output
									  :if-does-not-exist :create
									  :if-exists :append)
			  (write-init-forms stream))
			  init-file))
		""",
		"--eval", "(progn (add-to-init-file) (cl-user::quit))"
	], check=True)

	print("Successfully installed quicklisp!")

def install_clpm():
	print("Installing clpm...")

	# Instructions can be found at https://www.clpm.dev/tutorial/tutorial.html

	# Download CLPM
	os.chdir(HOME)
	run(["wget", "https://files.clpm.dev/clpm/clpm-0.4.1-linux-amd64.tar.gz"], check=True)
	run(["tar", "xf", "clpm-0.4.1-linux-amd64.tar.gz"], check=True)

	# Setup CLPM
	os.chdir("clpm-0.4.1-linux-amd64")
	run(["sudo", "sh", "./install.sh"], check=True)

	# Setup ASDF to find CLPM
	os.chdir(HOME)
	clpm_asdf_config_dir  = HOME + "/.config/common-lisp/source-registry.conf.d/"
	clpm_asdf_config_file = HOME + "/.config/common-lisp/source-registry.conf.d/20-clpm-client.conf"
	os.makedirs(clpm_asdf_config_dir)
	clpm_proc = run(["clpm", "client", "source-registry.d"], capture_output=True)
	with open(clpm_asdf_config_file, "w") as f:
		print(clpm_proc.stdout)
		f.write(ensure_string(clpm_proc.stdout))

	# Setup init file to load clpm-client
	clpm_config = """
;;; Use CLPM with default configuration.
;;;
;;; Generated by CLPM 0.4.1

(require "asdf")
#-clpm-client
(when (asdf:find-system "clpm-client" t)
  ;; Load the CLPM client if we can find it.
  (asdf:load-system "clpm-client")
  (when (uiop:symbol-call :clpm-client '#:active-context)
	;; If started inside a context (i.e., with `clpm exec` or `clpm bundle exec`),
	;; activate ASDF integration
	(uiop:symbol-call :clpm-client '#:activate-asdf-integration)))
	"""
	with open(LISP_INIT_FILE, "a") as cl_init:
		cl_init.write(clpm_config)

	with open(LISP_INIT_FILE, "r") as cl_init:
		print(cl_init.read())

	print("Successfully installed clpm!")

def prepare_sbcl():
	SBCL_VERSION = "2.3.11"
	SBCL_DIR = "-".join(["sbcl", SBCL_VERSION, PLATFORM])
	LISP_URL = "https://github.com/roswell/sbcl_bin/releases/download/{0}/{1}-binary.tar.bz2".format(
		SBCL_VERSION, SBCL_DIR
	)
	print("Downloading {0} from {1}...".format(LISP, LISP_URL))
	if not DRY_RUN:
		run(["wget", LISP_URL, "-O", "{0}.tar.bz2".format(SBCL_DIR)])
		run(["tar", "-xf", "{0}.tar.bz2".format(SBCL_DIR)])
		run(["ls", "-l", SBCL_DIR])
	print("Done.")
	install_cl("bash {0}/{1}/run-sbcl.sh --dynamic-space-size 4096".format(
		os.getcwd(),
		SBCL_DIR
	))



def prepare_ccl():
	CCL = "ccl-1.12.2-{}".format(CCL_PLATFORM)
	LISP_URL = "https://github.com/roswell/ccl_bin/releases/download/1.12.2/{0}.tar.gz".format(CCL)
	print("Downloading CCL from", LISP_URL)
	if not DRY_RUN:
		run(["wget", LISP_URL, "-O", "{0}.tar.gz".format(CCL)])
		run(["tar", "-xzf", "{0}.tar.gz".format(CCL)])
		run(["ls", "-l"])
	print("Done.")
	os.chmod("{0}/ccl/lx86cl64".format(os.getcwd()), 0o777)
	install_cl("{0}/ccl/lx86cl64".format(os.getcwd()))



def prepare_ecl():
	ECL_VERSION="23.9.9"
	LISP_URL = "https://github.com/digikar99/ecl_bin/releases/download/{0}/ecl-{0}-x86-64-linux-binary.tar.gz".format(ECL_VERSION)
	print("Downloading ECL from", LISP_URL)
	if not DRY_RUN:
		run(["wget", "--no-check-certificate", LISP_URL, "-O", "ecl-{0}.tar.gz".format(ECL_VERSION)])
		run(["sudo", "tar", "-C", "/", "-xzf", "ecl-{0}.tar.gz".format(ECL_VERSION)])
		run(["ls", "-l", "/usr/local/bin"])
	run(["sudo", "chmod", "+x", "/usr/local/bin/ecl"])
	# os.chmod("/usr/local/bin/ecl", 0o777)
	print("Done.")
	install_cl("/usr/local/bin/ecl")


def prepare_abcl():
	ABCL_VERSION="1.9.2"
	LISP_URL="https://abcl.org/releases/{0}/abcl-bin-{0}.tar.gz".format(ABCL_VERSION)
	print("Downloading ABCL from", LISP_URL)
	if not DRY_RUN:
		run(["wget", LISP_URL, "-O", "abcl-bin-{0}.tar.gz".format(ABCL_VERSION)])
		run(["tar", "-xzf", "abcl-bin-{0}.tar.gz".format(ABCL_VERSION)])
		run(["ls", "-l"])
	print("Done.")
	install_cl("java -jar {0}/abcl-bin-{1}/abcl.jar".format(
		os.getcwd(),
		ABCL_VERSION
	))


if __name__ == "__main__":
	os.chdir(HOME)
	print("Currently in", os.getcwd())
	prepare_function = globals()["prepare_"+LISP]
	prepare_function()
