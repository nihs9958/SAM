# CMAKE generated file: DO NOT EDIT!
# Generated by "Unix Makefiles" Generator, CMake Version 3.7

# Delete rule output on recipe failure.
.DELETE_ON_ERROR:


#=============================================================================
# Special targets provided by cmake.

# Disable implicit rules so canonical targets will work.
.SUFFIXES:


# Remove some rules from gmake that .SUFFIXES does not remove.
SUFFIXES =

.SUFFIXES: .hpux_make_needs_suffix_list


# Suppress display of executed commands.
$(VERBOSE).SILENT:


# A target that is always out of date.
cmake_force:

.PHONY : cmake_force

#=============================================================================
# Set environment variables for the build.

# The shell in which to execute make rules.
SHELL = /bin/sh

# The CMake executable.
CMAKE_COMMAND = /usr/local/bin/cmake

# The command to remove a file.
RM = /usr/local/bin/cmake -E remove -f

# Escaping for special characters.
EQUALS = =

# The top-level source directory on which CMake was run.
CMAKE_SOURCE_DIR = /Users/elgood/Code/eclipse/Streaming-c++/SAM

# The top-level build directory on which CMake was run.
CMAKE_BINARY_DIR = /Users/elgood/Code/eclipse/Streaming-c++/SAM

# Include any dependencies generated for this target.
include CMakeFiles/SimpleSum.dir/depend.make

# Include the progress variables for this target.
include CMakeFiles/SimpleSum.dir/progress.make

# Include the compile flags for this target's objects.
include CMakeFiles/SimpleSum.dir/flags.make

CMakeFiles/SimpleSum.dir/ExecutableSrc/TestSimpleSum.cpp.o: CMakeFiles/SimpleSum.dir/flags.make
CMakeFiles/SimpleSum.dir/ExecutableSrc/TestSimpleSum.cpp.o: ExecutableSrc/TestSimpleSum.cpp
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/Users/elgood/Code/eclipse/Streaming-c++/SAM/CMakeFiles --progress-num=$(CMAKE_PROGRESS_1) "Building CXX object CMakeFiles/SimpleSum.dir/ExecutableSrc/TestSimpleSum.cpp.o"
	/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/c++   $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -o CMakeFiles/SimpleSum.dir/ExecutableSrc/TestSimpleSum.cpp.o -c /Users/elgood/Code/eclipse/Streaming-c++/SAM/ExecutableSrc/TestSimpleSum.cpp

CMakeFiles/SimpleSum.dir/ExecutableSrc/TestSimpleSum.cpp.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing CXX source to CMakeFiles/SimpleSum.dir/ExecutableSrc/TestSimpleSum.cpp.i"
	/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/c++  $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -E /Users/elgood/Code/eclipse/Streaming-c++/SAM/ExecutableSrc/TestSimpleSum.cpp > CMakeFiles/SimpleSum.dir/ExecutableSrc/TestSimpleSum.cpp.i

CMakeFiles/SimpleSum.dir/ExecutableSrc/TestSimpleSum.cpp.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling CXX source to assembly CMakeFiles/SimpleSum.dir/ExecutableSrc/TestSimpleSum.cpp.s"
	/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/c++  $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -S /Users/elgood/Code/eclipse/Streaming-c++/SAM/ExecutableSrc/TestSimpleSum.cpp -o CMakeFiles/SimpleSum.dir/ExecutableSrc/TestSimpleSum.cpp.s

CMakeFiles/SimpleSum.dir/ExecutableSrc/TestSimpleSum.cpp.o.requires:

.PHONY : CMakeFiles/SimpleSum.dir/ExecutableSrc/TestSimpleSum.cpp.o.requires

CMakeFiles/SimpleSum.dir/ExecutableSrc/TestSimpleSum.cpp.o.provides: CMakeFiles/SimpleSum.dir/ExecutableSrc/TestSimpleSum.cpp.o.requires
	$(MAKE) -f CMakeFiles/SimpleSum.dir/build.make CMakeFiles/SimpleSum.dir/ExecutableSrc/TestSimpleSum.cpp.o.provides.build
.PHONY : CMakeFiles/SimpleSum.dir/ExecutableSrc/TestSimpleSum.cpp.o.provides

CMakeFiles/SimpleSum.dir/ExecutableSrc/TestSimpleSum.cpp.o.provides.build: CMakeFiles/SimpleSum.dir/ExecutableSrc/TestSimpleSum.cpp.o


# Object files for target SimpleSum
SimpleSum_OBJECTS = \
"CMakeFiles/SimpleSum.dir/ExecutableSrc/TestSimpleSum.cpp.o"

# External object files for target SimpleSum
SimpleSum_EXTERNAL_OBJECTS =

bin/SimpleSum: CMakeFiles/SimpleSum.dir/ExecutableSrc/TestSimpleSum.cpp.o
bin/SimpleSum: CMakeFiles/SimpleSum.dir/build.make
bin/SimpleSum: /usr/local/lib/libzmq.dylib
bin/SimpleSum: lib/libSamLib.a
bin/SimpleSum: /usr/local/lib/libzmq.dylib
bin/SimpleSum: CMakeFiles/SimpleSum.dir/link.txt
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --bold --progress-dir=/Users/elgood/Code/eclipse/Streaming-c++/SAM/CMakeFiles --progress-num=$(CMAKE_PROGRESS_2) "Linking CXX executable bin/SimpleSum"
	$(CMAKE_COMMAND) -E cmake_link_script CMakeFiles/SimpleSum.dir/link.txt --verbose=$(VERBOSE)

# Rule to build all files generated by this target.
CMakeFiles/SimpleSum.dir/build: bin/SimpleSum

.PHONY : CMakeFiles/SimpleSum.dir/build

CMakeFiles/SimpleSum.dir/requires: CMakeFiles/SimpleSum.dir/ExecutableSrc/TestSimpleSum.cpp.o.requires

.PHONY : CMakeFiles/SimpleSum.dir/requires

CMakeFiles/SimpleSum.dir/clean:
	$(CMAKE_COMMAND) -P CMakeFiles/SimpleSum.dir/cmake_clean.cmake
.PHONY : CMakeFiles/SimpleSum.dir/clean

CMakeFiles/SimpleSum.dir/depend:
	cd /Users/elgood/Code/eclipse/Streaming-c++/SAM && $(CMAKE_COMMAND) -E cmake_depends "Unix Makefiles" /Users/elgood/Code/eclipse/Streaming-c++/SAM /Users/elgood/Code/eclipse/Streaming-c++/SAM /Users/elgood/Code/eclipse/Streaming-c++/SAM /Users/elgood/Code/eclipse/Streaming-c++/SAM /Users/elgood/Code/eclipse/Streaming-c++/SAM/CMakeFiles/SimpleSum.dir/DependInfo.cmake --color=$(COLOR)
.PHONY : CMakeFiles/SimpleSum.dir/depend

