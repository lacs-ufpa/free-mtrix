/*   
  Free-mtrix - Free cultural selection and social behavior experiments.   
  Copyright (C) 2016-2017 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.   

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License   
  along with this program. If not, see <http://www.gnu.org/licenses/>.   
*/

# Manual

## How to install?

### Linux

   1. Install libzmq.so (see http://zeromq.org/intro:get-the-software), v3.2.5, 64 or 32 bits, on your system.

   2. Done!

### Windows

   1. Download and install Visual C++ Redistributable for Visual Studio 2015. You may find it here: https://www.microsoft.com/en-us/download/details.aspx?id=48145

   2. Compile or download a compiled version of the program. You can find some ready to use compiled versions of the program at the following places:
   
   - [The Thais' Experiment (with the Graphical User Interface traslated to English)](https://github.com/lacs-ufpa/free-mtrix/files/2975876/experiment_runner_windows_32bits_v0.2.0.14-30-ga48cff1.zip)
   - [The release page (PT-BR Graphical User Interface)](https://github.com/lacs-ufpa/free-mtrix/releases)
   - In the [issues page](https://github.com/lacs-ufpa/free-mtrix/issues)

   3. Done!

`OBS. 1`: Remember that to run an experiment you will need to write or receive a `Configuration File` containing the configuration of your experiment. Then you will be able to load this configuration file and start the data gathering protocol.


`OBS. 2`: If necessary, copy the `libzmq.dll` (v3.2.5, 32bits, dynamic, compiled with VS 2015) library to the same folder as the `experiment_runner.exe` file. This library is the dependency we need to the network communication.


## How to setup?

   - You have at least two options.
      1. Running from different folders in one machine (default for debugging).
      2. Running from different machines in a local network.

   - If you have a firewall enabled, you must create a firewall rule adding the program as an exception.

   - You will need an instance of the program running as Server (researcher) and others running as Clients (players). You will need at least two clients.

   - Clients must connect to the IP address of the Server; just create a file named 'IP' in the root folder of each client containing the Server IP on the first line.

## How to compile?

   1. Install Lazarus RAD IDE (v1.6.2+) Free Pascal compiler and sources (v3.0.0+): http://lazarus-ide.org/.
   2. Compile and install libzmq (v3.2.5) on your system: http://zeromq.org/intro:get-the-software.

   Note: If windows, all sofware must be compiled for 32bits, otherwise, you must write 64bits support yourself.

## Known Issues

**`A referral was returned from the server`**

   - Windows Only Error.
   - Solution 1: Right click at the `.exe` and select properties. In the tab compatibility, check the checkbox "Run as administrator".
