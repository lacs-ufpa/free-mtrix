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

   2. Copy the `libzmq.dll` (v3.2.5, 32bits, dynamic, compiled with VS 2015) library to the same folder as the `experiment_runner.exe` file. 

   3. Done!

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
