

# Should install $LISP
# Should output $LISP --version
# Should install quicklisp by default
# Should install clpm by default
# Should push current directory to ql:*local-project-directories* if quicklisp is installed

# source run.sh to be able to run LISP
# Implementations should support --eval and --load arguments

DRY_RUN=$1 # dry run if at least one argument is supplied

if [ -z ${SBCL_DYNAMIC_SPACE_SIZE} ]; then
    SBCL_DYNAMIC_SPACE_SIZE=4096
fi

case $(uname -m) in
    x86_64) ARCH="x86-64" ;;
    arm64|aarch64) ARCH="arm64";;
    *) ARCH=$(uname -m) ;;
esac

if [ -n "$TRAVIS" ]; then
    case $TRAVIS_OS_NAME in
        linux)
            PLATFORM="$ARCH-linux"
            case $ARCH in
                x86-64) CCL_PLATFORM="linuxx86" ;;
                arm64|aarch64) CCL_PLATFORM="linuxarm" ;;
            esac
            ;;
        osx)
            # Do Nothing; let it brew
            PLATFORM="$ARCH-darwin"
            ;;
        *)
            echo "Unhandled OS: ": $os
            exit 1
            ;;
    esac
else # Github actions
    case $OS in
        macos-14)
            PLATFORM="arm64-darwin"
            CCL_PLATFORM="darwinarm"
            ACL_PLATFORM="macarm64.64"
            ACL_SUFFIX="macos-arm64.dmg"
            ;;
        ubuntu*)
            PLATFORM="$ARCH-linux"
            CCL_PLATFORM="linuxx86"
            ACL_PLATFORM="linuxamd64.64"
            ACL_SUFFIX="linux-x64.tbz2"
            ;;
        macos*)
            PLATFORM="$ARCH-darwin"
            CCL_PLATFORM="darwinx86"
            ACL_PLATFORM="macosx86-64.64"
            ACL_SUFFIX="macos-x64.dmg"
            ;;
        windows*)
            PLATFORM="$ARCH-windows"
            ;;
        *) echo "Unknown OS: " $OS
           exit 1
           ;;
    esac
fi

case $LISP in
    acl|allegro)
        EVALOPT="-e"
        LOADOPT="-L"
        ;;
    *)
        EVALOPT="--eval"
        LOADOPT="--load"
        ;;
esac

case $LISP in
    acl|allegro)
        QUITOPT="--kill"
        ;;
    *)
        QUITOPT="--eval '(uiop:quit)'"
        ;;
esac

install_cl(){
    # Create a 'cl' script
    mkdir "/usr/local/bin"
    echo $PATH
    cl_file="/usr/local/bin/cl"
    ls -l "/usr/local/bin"
    echo "#!$SHELL" > "$cl_file"
    echo "$1" '"$@"' " $QUITOPT" >> "$cl_file"
    chmod +x "$cl_file"
    cat "$cl_file"
    cl $QUITOPT # print (potentially) version information and quit
    install_quicklisp

    # Load quicklisp by default
    cl_file="/usr/local/bin/cl"
    echo "#!$SHELL" > "$cl_file"

    # Argument processor: replace --load with $LOADOPT, --eval with $EVALOPT, --quit with $QUITOPT
    echo "processed_args=()" >> "$cl_file"
    echo "for var in "'"$@"' >> "$cl_file"
    echo "do" >> "$cl_file"
    echo 'case $var in' >> "$cl_file"
    echo '--load|-l) var="'$LOADOPT'" ;;' >> "$cl_file"
    echo '--eval|-e) var="'$EVALOPT'" ;;' >> "$cl_file"
    echo '--quit|-q) var="'$QUITOPT'" ;;' >> "$cl_file"
    echo "esac" >> "$cl_file"
    echo 'processed_args+=("${var}")' >> "$cl_file"
    echo "done" >> "$cl_file"
    echo 'var="${var@Q}"' >> "$cl_file"
    echo 'echo ${processed_args[@]}' >> "$cl_file"

    # https://stackoverflow.com/questions/3601515/how-to-check-if-a-variable-is-set-in-bash
    case $LISP in
        allegro|acl)
            echo "$1 --batch --backtrace-on-error $LOADOPT $HOME/quicklisp/setup.lisp" \
                 '"${processed_args[@]}"' " $QUITOPT" >> "$cl_file"
            ;;
        *)
            echo "$1 $LOADOPT $HOME/quicklisp/setup.lisp \\
                     $EVALOPT '(setf *debugger-hook*
                               (lambda (c h)
                                 (declare (ignore h))
                                 (uiop:print-condition-backtrace c)
                                 (uiop:quit 1)))'" \
                              '"${processed_args[@]}"' " $QUITOPT" >> "$cl_file"
            ;;
    esac

    chmod +x "$cl_file"
    cat "$cl_file"
}

prepare_sbcl(){
    SBCL_VERSION="2.4.11"
    echo "Installing SBCL on " $PLATFORM
    case $PLATFORM in
        x86-64-darwin | *linux)
            SBCL_DIR="sbcl-$SBCL_VERSION-$PLATFORM"
            LISP_URL="https://github.com/roswell/sbcl_bin/releases/download/$SBCL_VERSION/$SBCL_DIR-binary.tar.bz2"
            echo Downloading $LISP from $LISP_URL...
            if [ -z $DRY_RUN ] ; then
                wget "$LISP_URL" -O "$SBCL_DIR.tar.bz2"
                tar -xf "$SBCL_DIR.tar.bz2"
                ls -l "$SBCL_DIR"
            fi
            echo Downloaded
            install_cl "$SHELL $PWD/$SBCL_DIR/run-sbcl.sh --dynamic-space-size $SBCL_DYNAMIC_SPACE_SIZE"
            ;;
        *darwin)
            brew install sbcl
            install_cl "$(which sbcl) --dynamic-space-size $SBCL_DYNAMIC_SPACE_SIZE"
            ;;
        *windows)
            pacman --noconfirm -S mingw-w64-x86_64-sbcl wget
            install_cl "$(which sbcl) --dynamic-space-size $SBCL_DYNAMIC_SPACE_SIZE"
            ;;
    esac
}

prepare_ccl(){
    case $PLATFORM in
        *linux)
            CCL="ccl-1.12.2-$CCL_PLATFORM"
            LISP_URL="https://github.com/roswell/ccl_bin/releases/download/1.12.2/$CCL.tar.gz"
            echo Downloading $LISP from $LISP_URL...
            if [ -z $DRY_RUN ] ; then
                wget "$LISP_URL" -O $CCL.tar.gz
                tar -xzf "$CCL.tar.gz"
                ls -l ccl*
            fi
            echo Downloaded
            case $ARCH in
                x86-64) install_cl "$PWD/ccl/lx86cl64" ;;
                arm64) install_cl "$PWD/ccl/armcl" ;;
            esac
            ;;
        *darwin)
            brew install clozure-cl
            install_cl "$(which ccl)"
            ;;
    esac
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
    ECL_VERSION="24.5.10"
    LISP_URL="https://github.com/digikar99/ecl_bin/releases/download/$ECL_VERSION/ecl-$ECL_VERSION-$ARCH-linux-binary.tar.gz"
    echo Downloading $LISP from $LISP_URL...
    if [ -z $DRY_RUN ] ; then
        wget --no-check-certificate "$LISP_URL" -O "ecl-$ECL_VERSION.tar.gz"
        sudo tar -C / -xzf "ecl-$ECL_VERSION.tar.gz"
    fi
    echo Downloaded
    install_cl "/usr/local/bin/ecl"
}

# Allegro CL
prepare_acl(){
    ACL_VERSION="11.0"
    LISP_URL="https://franz.com/ftp/pub/acl"$ACL_VERSION"express/$ACL_PLATFORM/acl"$ACL_VERSION"express-$ACL_SUFFIX"
    echo Downloading $LISP from $LISP_URL...
    if [ -z $DRY_RUN ] ; then
        case $PLATFORM in
            *darwin)
                wget --no-check-certificate "$LISP_URL" -O "acl-$ACL_VERSION.dmg"
                sudo hdiutil attach "acl-$ACL_VERSION.dmg"
                ls -l /Volumes/AllegroCL64express/
                sudo cp -R /Volumes/AllegroCL64express/AllegroCL64express.app /Applications/

                # echo "ls -R /Applications/AllegroCL64express.app/"
                # ls -R /Applications/AllegroCL64express.app/

                acl_cmd="/Applications/AllegroCL64express.app/Contents/Resources/alisp -I /Applications/AllegroCL64express.app/Contents/Resources/alisp.dxl"
                ;;
            *linux)
                wget --no-check-certificate "$LISP_URL" -O "acl-$ACL_VERSION.tbz2"
                sudo tar jxf "acl-$ACL_VERSION.tbz2" -C /usr/local/
                ls -l /usr/local/
                acl_cmd="/usr/local/acl"$ACL_VERSION"express.64/alisp -I /usr/local/acl"$ACL_VERSION"express.64/alisp.dxl"
                ;;
        esac
    fi
    install_cl "$acl_cmd"
    echo Downloaded
}

install_quicklisp(){
    echo Installing quicklisp...
    wget "https://beta.quicklisp.org/quicklisp.lisp" -O "$HOME/quicklisp.lisp"
    cd $HOME
    # Below we remove a prompt to append quicklisp autoload code to the implementations init file
    # Also, some implementations like ABCL necessitate an explicit one-by-one form evaluation
    cl $LOADOPT "quicklisp.lisp" $EVALOPT '(quicklisp-quickstart:install)' \
       $EVALOPT '(in-package :ql-impl-util)' $EVALOPT \
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
            init-file))' $EVALOPT '(add-to-init-file)' $QUITOPT \
                && echo Successfully installed quicklisp!
}

cd $HOME
echo Currently in $(pwd)...
prepare_"$LISP"
