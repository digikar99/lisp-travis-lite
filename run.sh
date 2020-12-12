#!/bin/bash

# Should install $LISP
# Should output $LISP --version
# Should install quicklisp by default
# Should push current directory to ql:*local-project-directories* if quicklisp is installed

# source run.sh to be able to run LISP
# Implementations should support --eval and --load arguments

DRY_RUN=$1 # dry run if at least one argument is supplied

install_cl(){
    mkdir "$HOME/bin"
    echo $PATH
    cl_file="$HOME/bin/cl"
    ls -l "$HOME/bin"
    echo "#!/bin/bash" > "$cl_file"
    echo "$1" '"$@"' " --eval '(quit)'" >> "$cl_file"
    chmod +x "$cl_file"
    cat "$cl_file"
}

prepare_sbcl(){
    LISP_URL="https://archive.org/download/sbcl-2.0.9-linux-x86_64/sbcl-2.0.9-linux-x86_64.image"
    echo Downloading $LISP from $LISP_URL...
    if [ -z $DRY_RUN ] ; then
        wget "$LISP_URL" -O lisp
        ls -l
        chmod +x ./lisp
    fi
    echo Downloaded
    install_cl "$PWD/lisp --dynamic-space-size 4096 --non-interactive"
}

prepare_ccl(){
    CCL="ccl-1.12-linux-x86_64"
    LISP_URL="https://archive.org/download/$CCL/$CCL.image.tar.gz"
    echo Downloading $LISP from $LISP_URL...
    if [ -z $DRY_RUN ] ; then
        wget "$LISP_URL" -O lisp.tar.gz
        tar -xzf "lisp.tar.gz"
        ls -l
        chmod +x ./$CCL.image
    fi
    echo Downloaded
    install_cl "$PWD/$CCL.image -b"
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
    install_cl "java -jar $PWD/abcl-bin-$ABCL_VERSION/abcl.jar"
}

prepare_ecl(){
    ECL_VERSION="20.4.24"
    LISP_URL="https://cdn.cddr.org/ci/ecl-$ECL_VERSION-linux-amd64.tar.gz"
    echo Downloading $LISP from $LISP_URL...
    if [ -z $DRY_RUN ] ; then
        wget --no-check-certificate "$LISP_URL" -O "ecl-$ECL_VERSION.tar.gz"
        sudo tar -C / -xzf "ecl-$ECL_VERSION.tar.gz"
    fi
    echo Downloaded
    install_cl "/usr/local/bin/ecl"
}

install_quicklisp(){
    echo Installing quicklisp...
    wget "https://beta.quicklisp.org/quicklisp.lisp" -O "$HOME/quicklisp.lisp"
    cd $HOME
    # Below we remove a prompt to append quicklisp autoload code to the implementations init file
    # Also, some implementations like ABCL necessitate an explicit one-by-one form evaluation
    cl --load "quicklisp.lisp" --eval '(quicklisp-quickstart:install)' \
       --eval '(in-package :ql-impl-util)' --eval \
       '(defun add-to-init-file (&optional implementation-or-file)
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
            init-file))' --eval '(progn (add-to-init-file) (cl-user::quit))' \
                && echo Successfully installed quicklisp!
}

cd $HOME
echo Currently in $(pwd)...
prepare_"$LISP"
cl --eval '(quit)' # print (potentially) version information and quit
install_quicklisp
cl="$cl --load $HOME/quicklisp/setup.lisp"

