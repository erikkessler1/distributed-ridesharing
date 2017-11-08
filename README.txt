Erik Kessler and Kevin Persons
CS 339 Final Project: Distributed Ride Sharing

The purpose of this part of the project is to simulate a simple
1-D world that we can use to test the correctness and effectiveness of
our distributed, peer-to-peer ride-sharing application.

NOTE: The visulizer and command prompt requires that the terminal
      window be 132x43.

Building:
	make

Running:
	scala -cp bin Simulator [OPTION]... [# OF PEERS]
          -v  Verbose logging
          -r  Randomly initialize a list of 10 known peers for each node
	  -f  Initialize with peer lists that contain all peers in the network

Sample Usage:
       -> scala -cp bin Simulator -r
         - Creates world with 100 peers

       -> s
       	 - Shows the peers on the world

       -> s 100 10
         - Runs the simulation for 100 steps with a delay of 10ms between steps

       -> f 50
       	 - Focus on peer 50

       -> r
         - View the number of rides served over the number requested
