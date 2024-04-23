#!/bin/bash

# Should install $LISP
# Should output $LISP --version
# Should install quicklisp by default
# Should install clpm by default
# Should push current directory to ql:*local-project-directories* if quicklisp is installed

# source run.sh to be able to run LISP
# Implementations should support --eval and --load arguments

DRY_RUN=$1 # dry run if at least one argument is supplied

case $OS in
    ubuntu*)
        PLATFORM="x86-64-linux"
        CCL_PLATFORM="linuxx86"
        ;;
    macos-11 | macos-12 | macos-13)
        PLATFORM="x86-64-darwin"
        CCL_PLATFORM="darwinx86"
        ;;
    macos*)
        PLATFORM="arm64-darwin"
        CCL_PLATFORM="darwinarm"
        ;;
    *) echo "Unknown OS: " $OS
       exit 1
       ;;
esac

install_cl(){
    mkdir "$HOME/bin"
    echo $PATH
    cl_file="$HOME/bin/cl"
    ls -l "$HOME/bin"
    echo "#!/bin/bash" > "$cl_file"
    echo "$1" '"$@"' " --eval '(quit)'" >> "$cl_file"
    chmod +x "$cl_file"
    cat "$cl_file"
    echo "PATH=$HOME/bin:\$PATH" >> $HOME/.bashrc
    cl --eval '(quit)' # print (potentially) version information and quit
    install_quicklisp

    # Load quicklisp by default
    cl_file="$HOME/bin/cl"
    echo "#!/bin/bash" > "$cl_file"
    # https://stackoverflow.com/questions/3601515/how-to-check-if-a-variable-is-set-in-bash
    if [ -z "${IGNORE_BACKTRACE+ignore_backtrace}" ]; then
        echo "$1 --load $HOME/quicklisp/setup.lisp \\
             --eval '(ql:quickload \"trivial-backtrace\")' \\
             --eval '(setf *debugger-hook*
                           (lambda (c h)
                             (declare (ignore h))
                             (trivial-backtrace:print-backtrace c)
                              (uiop:quit 1)))'" \
                                  '"$@"' " --eval '(uiop:quit)'" >> "$cl_file"
    else
        # trivial-backtrace in the January 2021 quicklisp distribution does not load on CCL
        echo Skipping trivial-backtrace
        echo "$1 --load $HOME/quicklisp/setup.lisp \\
             --eval '(setf *debugger-hook*
                           (lambda (c h)
                             (declare (ignore c h))
                              (uiop:quit 1)))'" \
                                  '"$@"' " --eval '(uiop:quit)'" >> "$cl_file"
    fi
    chmod +x "$cl_file"
    cat "$cl_file"
}

prepare_sbcl(){
    SBCL_VERSION="2.4.3"
    echo "Installing SBCL on " $OS
    case $OS in
        ubuntu* | macos-11 | macos-12 | macos-13)
            SBCL_DIR="sbcl-$SBCL_VERSION-$PLATFORM"
            LISP_URL="https://github.com/roswell/sbcl_bin/releases/download/$SBCL_VERSION/$SBCL_DIR-binary.tar.bz2"
            echo Downloading $LISP from $LISP_URL...
            if [ -z $DRY_RUN ] ; then
                wget "$LISP_URL" -O "$SBCL_DIR.tar.bz2"
                tar -xf "$SBCL_DIR.tar.bz2"
                ls -l "$SBCL_DIR"
            fi
            echo Downloaded
            install_cl "bash $PWD/$SBCL_DIR/run-sbcl.sh --dynamic-space-size 4096"
            ;;
        macos*)
            brew install bash
            brew install sbcl
            install_cl "bash $(which sbcl) --dynamic-space-size 2048"
            ;;
    esac
}

prepare_ccl(){
    CCL="ccl-1.12.2-$CCL_PLATFORM"
    LISP_URL="https://github.com/roswell/ccl_bin/releases/download/1.12.2/$CCL.tar.gz"
    echo Downloading $LISP from $LISP_URL...
    if [ -z $DRY_RUN ] ; then
        wget "$LISP_URL" -O $CCL.tar.gz
        tar -xzf "$CCL.tar.gz"
        ls -l
    fi
    echo Downloaded
    install_cl "$PWD/ccl/lx86cl64"
}

prepare_abcl(){
    ABCL_VERSION="1.9.2" # TODO: check if already defined
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
    ECL_VERSION="23.9.9"
    LISP_URL="https://github.com/digikar99/ecl_bin/releases/download/$ECL_VERSION/ecl-$ECL_VERSION-x86-64-linux-binary.tar.gz"
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
