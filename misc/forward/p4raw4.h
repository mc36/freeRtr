pthread_t threadSock;
if (pthread_create(&threadSock, NULL, (void*) & doSockLoop, NULL)) err("error creating socket thread");
pthread_t threadStat;
if (pthread_create(&threadStat, NULL, (void*) & doStatLoop, NULL)) err("error creating status thread");
pthread_t threadRaw[maxPorts];
for (int i=0; i < dataPorts; i++) {
    if (pthread_create(&threadRaw[i], NULL, (void*) & doIfaceLoop, &ifaceId[i])) err("error creating port thread");
}

doMainLoop();
