
# ascii-transformer

# Introduction

This is an application that converts an mp4 video into ascii characters.

# Usage

This assumes that Haskell with stack is properly installed on your device.

First, clone the repository and navigate to the project directory:

    $ git clone https://github.com/Sahaj0312/ascii-transformer.git

There are already a couple test videos in the directory but you can upload mp4 files of your choosing to the main directory.

Next, build the project:

    $ stack build

Then, execute the application (replace 'test.mp4' with a mp4 file name of your choice).

    $ stack exec ascii-transformer-exe test.mp4
