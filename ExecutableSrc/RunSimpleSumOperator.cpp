/*
 * TestTopK.cpp
 *
 *  Created on: Dec 25, 2016
 *      Author: elgood
 */

#define DEBUG 1

#include <string>
#include <vector>
#include <stdlib.h>
#include <iostream>
#include <chrono>

#include <boost/program_options.hpp>

#include "ReadSocket.h"
#include "ZeroMQPushPull.hpp"
#include "SimpleSum.hpp"
#include "Netflow.hpp"

using std::string;
using std::vector;
using std::cout;
using std::endl;

namespace po = boost::program_options;

using namespace sam;
using namespace std::chrono;

int main(int argc, char** argv) {

#ifdef DEBUG
  cout << "DEBUG: At the beginning of main" << endl;
#endif

	// The ip to read the nc data from.
	string ip;

	// The port to read the nc data from.
	int ncPort;

	// The number of nodes in the cluster
	int numNodes;

	// The node id of this node
	int nodeId;

	// The prefix to the nodes
	string prefix;

	// The starting port number
	int startingPort;

	// The high-water mark
	long hwm;

  // The length of the input queue
  int queueLength;

  // How many simultaneous operators
  int nop;

  // The total number of elements in a sliding window
  int N;

	time_t timestamp_sec1, timestamp_sec2;

  po::options_description desc("Allowed options");
  desc.add_options()
    ("help", "help message")
    ("ip", po::value<string>(&ip)->default_value("localhost"), 
      "The ip to receive the data from nc")
    ("ncPort", po::value<int>(&ncPort)->default_value(9999), 
      "The port to receive the data from nc")
    ("numNodes", po::value<int>(&numNodes)->default_value(1), 
      "The number of nodes involved in the computation")
    ("nodeId", po::value<int>(&nodeId)->default_value(0), 
      "The node id of this node")
    ("prefix", po::value<string>(&prefix)->default_value("node"), 
      "The prefix common to all nodes")
    ("startingPort", po::value<int>(&startingPort)->default_value(10000), 
      "The starting port for the zeromq communications")
    ("hwm", po::value<long>(&hwm)->default_value(10000), 
      "The high water mark (how many items can queue up before we start "
      "dropping)")
    ("queueLength", po::value<int>(&queueLength)->default_value(10000),
      "We fill a queue before sending things in parallel to all consumers."
      "  This controls the size of that queue.")
    ("nop", po::value<int>(&nop)->default_value(1),
      "The number of simultaneous operators")
    ("N", po::value<int>(&N)->default_value(10000),
      "The total number of elements in a sliding window")
  ;

  po::variables_map vm;
  po::store(po::parse_command_line(argc, argv, desc), vm);
  po::notify(vm);

  if (vm.count("help")) {
    std::cout << desc << std::endl;
    return 1;
  }

#ifdef DEBUG
  cout << "Options" << endl;
  cout << "numNodes " << numNodes << endl;
  cout << "nodeId " << nodeId << endl;
#endif


	ReadSocket receiver(ip, ncPort);
#ifdef DEBUG
  cout << "DEBUG: main created receiver " << endl;
#endif

  vector<string> hostnames(numNodes);
  vector<std::size_t> ports(numNodes);

  if (numNodes == 1) {
    hostnames[0] = "127.0.0.1";
    ports[0] = startingPort;
  } else {
    for (int i = 0; i < numNodes; i++) {
      hostnames[i] = prefix + boost::lexical_cast<string>(i);
      ports[i] = (startingPort + i);  
    }
  }

  auto consumer = std::make_shared<ZeroMQPushPull>(queueLength,
                               numNodes, 
                               nodeId, 
                               hostnames, 
                               ports, 
                               hwm);

#ifdef DEBUG
  cout << "DEBUG: main created consumer " << endl;
#endif

  receiver.registerConsumer(consumer);

  auto featureMap = std::make_shared<FeatureMap>();
  for (int i = 0; nop > 0 && i < nop; i++) {
    string identifier = "simplesum" + boost::lexical_cast<string>(i);
    auto sum = std::make_shared<SimpleSum<size_t, Netflow, 14, DestIp>>
                    (N, nodeId, featureMap, identifier);
    consumer->registerConsumer(sum); 
  }

  

  if (!receiver.connect()) {
    std::cout << "Couldn't connected to " << ip << ":" << ncPort << std::endl;
    return -1;
  }

  milliseconds ms1 = duration_cast<milliseconds>(
    system_clock::now().time_since_epoch()
  );
  receiver.receive();
  milliseconds ms2 = duration_cast<milliseconds>(
    system_clock::now().time_since_epoch()
  );
	std::cout << nodeId << " Seconds " 
    << static_cast<double>(ms2.count() - ms1.count()) / 1000 << std::endl;


}
