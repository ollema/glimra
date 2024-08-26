#!/bin/bash

UNAME_S=$(uname -s)
case "$UNAME_S" in
    Linux)
        OS="linux"
        SOURCE_EXT="so"
        TARGET_EXT="so"
        ;;
    Darwin)
        OS="macos"
        SOURCE_EXT="dylib"
        TARGET_EXT="so"
        ;;
    CYGWIN*|MINGW32*|MSYS*|MINGW*)
        OS="windows"
        SOURCE_EXT="dll"
        TARGET_EXT="dll"
        ;;
    *)
        echo "Unsupported OS: $UNAME_S"
        exit 1
        ;;
esac

UNAME_M=$(uname -m)
case "$UNAME_M" in
    x86_64)
        ARCH="x86_64"
        ;;
    arm64|aarch64)
        ARCH="aarch64"
        ;;
    *)
        echo "Unsupported architecture: $UNAME_M"
        exit 1
        ;;
esac

SOURCE_PATH="native/libglans/target/release/liblibglans.${SOURCE_EXT}"

TARGET_PATH="./priv/lib/libglans-${OS}-${ARCH}.${TARGET_EXT}"

echo -e "\nbuilding libglans for for ${OS} (${ARCH})...\n"

pushd native/libglans > /dev/null || { echo "failed to enter directory native/libglans"; exit 1; }
cargo build --release || { echo "cargo build failed"; popd > /dev/null; exit 1; }
popd > /dev/null

if [ ! -f "$SOURCE_PATH" ]; then
    echo "$SOURCE_PATH not found!"
    exit 1
fi

cp "$SOURCE_PATH" "$TARGET_PATH" || { echo "failed to copy source to target path"; exit 1; }

echo -e "\nbuild completed:\n\n${TARGET_PATH}\n"
