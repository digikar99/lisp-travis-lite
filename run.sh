#!/bin/bash

# Should install $LISP
# Should output $LISP --version
# Should install quicklisp by default
# Should push current directory to ql:*local-project-directories* if quicklisp is installed

# source run.sh to be able to run LISP
# Implementations should support --eval and --load arguments

DRY_RUN=$1 # dry run if at least one argument is supplied

prepare_sbcl(){
    LISP_URL="https://gitlab.com/digikar99/sbcl-images/-/raw/master/sbcl-2.0.9-linux-x86_64.image"
    echo Downloading $LISP from $LISP_URL...
    if [ -z $DRY_RUN ] ; then
        wget "$LISP_URL" -O lisp
    fi
    echo Downloaded
    alias cl="$PWD/lisp"
}

prepare_abcl(){
    ABCL_VERSION="1.7.1" # TODO: check if already defined
    LISP_URL="https://abcl.org/releases/$ABCL_VERSION/abcl-bin-$ABCL_VERSION.tar.gz"
    echo Downloading $LISP from $LISP_URL...
    if [ -z $DRY_RUN ] ; then
        wget "$LISP_URL" -O "abcl-bin-$ABCL_VERSION.tar.gz"
        tar -xzf "abcl-bin-$ABCL_VERSION.tar.gz"
    fi
    echo Downloaded
    alias cl="java -jar $PWD/abcl-bin-$ABCL_VERSION/abcl.jar"
}

install_quicklisp(){
    echo Installing quicklisp...
    wget "https://beta.quicklisp.org/quicklisp.lisp" -O "$HOME/quicklisp.lisp"
    cd $HOME
    # Below we remove a prompt to append quicklisp autoload code to the implementations init file
    cl --load '(load "quicklisp.lisp")' --eval \
       '(progn
          (quicklisp-quickstart:install)
          (in-package :ql-impl-util)
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
          (add-to-init-file))'
    echo Successfully installed quicklisp!
}

cd $HOME
echo Currently in $(pwd)...
prepare_"$LISP"
cl --eval '(quit)' # print (potentially) version information and quit
install_quicklisp

