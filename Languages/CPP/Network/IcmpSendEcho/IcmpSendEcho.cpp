#include <iostream>
#include <stdio.h>

#include <string>
#include <cstring>

#include <winsock2.h>
#include <iphlpapi.h>
#include <icmpapi.h>
#include <ws2tcpip.h>

#pragma comment(lib, "iphlpapi.lib")
#pragma comment(lib, "ws2_32.lib")

/* According to recent versions of the MSVC compiler (since C++11), the functions listed below are
considered "unsafe" and result in compiler warning C4996. The recommended updates are listed.
* inet_addr -> inet_pton
* inet_ntoa -> inet_ntop

Three ways to resolve this:
1) Update the methods as per the recommendations
2) Define "_WINSOCK_DEPRECATED_NO_WARNINGS" to disable deprecated API warnings (in project settings)
3) Disable the warning using "#pragma warning(disable : 4996)" (obviously not ideal!) */

/*
IcmpSendEcho examples are based on:
	https://docs.microsoft.com/en-us/windows/win32/api/icmpapi/nf-icmpapi-icmpsendecho
but adapted to give functionality similar to the command-line "ping" utility:
	Pinging 192.168.2.191 with 32 bytes of data:
	Reply from 192.168.2.191: bytes=32 time=102ms TTL=64
	Reply from 192.168.2.191: bytes=32 time=630ms TTL=64
	Reply from 192.168.2.191: bytes=32 time=237ms TTL=64
	Reply from 192.168.2.191: bytes=32 time=142ms TTL=64

	Ping statistics for 192.168.2.191:
		Packets: Sent = 4, Received = 4, Lost = 0 (0% loss),
	Approximate round trip times in milli-seconds:
		Minimum = 102ms, Maximum = 630ms, Average = 277ms
*/

int TryIcmpSendEcho_v1(const char *address, const uint32_t cuTries = 1)
{
	// Send an ICMP echo request to the specified IPv4 address
	// This version uses the deprecated methods "inet_addr", "inet_ntoa" and "gethostbyname"

	unsigned long ipAddr = INADDR_NONE;
	ipAddr = inet_addr(address);
	if (ipAddr == INADDR_NONE)
	{
		// Could be a web address instead of a IPv4 numeric address
		bool bError = true;
		hostent* hp = gethostbyname(address);
		if (hp)
		{
			memcpy(&ipAddr, hp->h_addr, hp->h_length);
			bError = false;
		}

		if (bError)
		{
			printf("Usage: app IpAddress\n");
			return 1;
		}
	}
    
	HANDLE hIcmpFile = IcmpCreateFile();
	if (hIcmpFile == INVALID_HANDLE_VALUE) {
		printf("\tUnable to open handle.\n");
		printf("IcmpCreatefile returned error: %ld\n", GetLastError() );
		return 1;
	}

	char SendData[32] = "data";
	LPVOID pReplyBuffer = NULL;

	DWORD dwReplySize = sizeof(ICMP_ECHO_REPLY) + sizeof(SendData);
	pReplyBuffer = (VOID*) malloc(dwReplySize);
	if (pReplyBuffer == NULL) {
		printf("\tUnable to allocate memory\n");
		return 1;
	}    

	printf("Using IcmpSendEcho to ping %s with %d bytes of data (%d time/s):\n",
		address, sizeof(SendData), cuTries);
	uint32_t uTry = 1;
	uint32_t uSuccess = 0;
	uint32_t uFailed = 0;
	DWORD dwEchoReturn = 0;
	DWORD dwTotalResponses = 0;		// Stats
	ULONG ulRoundTripTotal = 0;
	ULONG ulRoundTripMin = ULONG_MAX;
	ULONG ulRoundTripMax = 0;
	ULONG ulReplyAddress = 0;
	PICMP_ECHO_REPLY pEchoReply = nullptr;
	struct in_addr ReplyAddr;
	while (uTry <= cuTries)
	{
		dwEchoReturn = IcmpSendEcho(hIcmpFile, ipAddr, SendData, sizeof(SendData), 
			NULL, pReplyBuffer, dwReplySize, 1000);
		if (dwEchoReturn != 0) {
			// Success
			uSuccess++;
			dwTotalResponses += dwEchoReturn;

			// Capture timing and other statistics
			pEchoReply = (PICMP_ECHO_REPLY)pReplyBuffer;
			ulRoundTripTotal += pEchoReply->RoundTripTime;
			if (pEchoReply->RoundTripTime < ulRoundTripMin)
				ulRoundTripMin = pEchoReply->RoundTripTime;
			if (pEchoReply->RoundTripTime > ulRoundTripMax)
				ulRoundTripMax = pEchoReply->RoundTripTime;

			if (ulReplyAddress == 0)
				ulReplyAddress = pEchoReply->Address;
			ReplyAddr.S_un.S_addr = pEchoReply->Address;
			printf("  Reply from %s: time=%ldms status=%ld\n",
				inet_ntoa(ReplyAddr), pEchoReply->RoundTripTime, pEchoReply->Status);
		}
		else {
			// Error
			uFailed++;

			// Retrieve the error message, probably 11010 (IP_REQ_TIMED_OUT). This error code is
			// not WSA_QOS_ADMISSION_FAILURE! This list of IP errors codes comes from IPExport.h:
			/*	#define IP_STATUS_BASE              11000
				#define IP_SUCCESS                  0
				#define IP_BUF_TOO_SMALL            (IP_STATUS_BASE + 1)
				#define IP_DEST_NET_UNREACHABLE     (IP_STATUS_BASE + 2)
				#define IP_DEST_HOST_UNREACHABLE    (IP_STATUS_BASE + 3)
				#define IP_DEST_PROT_UNREACHABLE    (IP_STATUS_BASE + 4)
				#define IP_DEST_PORT_UNREACHABLE    (IP_STATUS_BASE + 5)
				#define IP_NO_RESOURCES             (IP_STATUS_BASE + 6)
				#define IP_BAD_OPTION               (IP_STATUS_BASE + 7)
				#define IP_HW_ERROR                 (IP_STATUS_BASE + 8)
				#define IP_PACKET_TOO_BIG           (IP_STATUS_BASE + 9)
				#define IP_REQ_TIMED_OUT            (IP_STATUS_BASE + 10)
				#define IP_BAD_REQ                  (IP_STATUS_BASE + 11)
				#define IP_BAD_ROUTE                (IP_STATUS_BASE + 12)
				#define IP_TTL_EXPIRED_TRANSIT      (IP_STATUS_BASE + 13)
				#define IP_TTL_EXPIRED_REASSEM      (IP_STATUS_BASE + 14)
				#define IP_PARAM_PROBLEM            (IP_STATUS_BASE + 15)
				#define IP_SOURCE_QUENCH            (IP_STATUS_BASE + 16)
				#define IP_OPTION_TOO_BIG           (IP_STATUS_BASE + 17)
				#define IP_BAD_DESTINATION          (IP_STATUS_BASE + 18)*/
			DWORD dwErrorFailure = GetLastError();
			pEchoReply = (PICMP_ECHO_REPLY)pReplyBuffer;
			wchar_t bufferError[200];
			DWORD dwErrorSize = sizeof(bufferError);
			GetIpErrorString(pEchoReply->Status, bufferError, &dwErrorSize);
			wprintf(L"  Error: %d, %s\n", dwErrorFailure, bufferError);
		}

		uTry++;
		if (uTry <= cuTries)
			Sleep(1000);
	}

	// Print statistics
	printf("\n");
	if (ulReplyAddress > 0) {
		// We received at least one response!
		printf("IcmpSendEcho statistics from %s:\n", inet_ntoa(ReplyAddr));
	}
	else {
		// No responses received
		printf("IcmpSendEcho statistics from %s:\n", address);
	}
	printf("  Packets: Sent = %d, Received = %d, Lost = %d\n",
		cuTries, uSuccess, (cuTries - uSuccess));
	printf("Approximate round trip times in milliseconds:\n");
	ULONG ulRoundTripAverage = 0;
	if (uSuccess > 0)
		ulRoundTripAverage = (ulRoundTripTotal / uSuccess);

	printf("  Minimum = %ldms, Maximum = %ldms, Average = %ldms\n\n",
		ulRoundTripMin, ulRoundTripMax, ulRoundTripAverage);

	// Close handle
	IcmpCloseHandle(hIcmpFile);
	return 0;
}

int TryIcmpSendEcho_v2(const char *address, const uint32_t cuTries = 1)
{
	// Send an ICMP echo request to the specified IPv4 address
	// This version uses updated methods:
	// * "inet_pton" (for "inet_addr")
	// * "inet_ntoa" (for "inet_ntoa")
	// * "getaddrinfo" (for "gethostbyname")

	// TODO
	return 0;
}

int __cdecl main(int argc, char **argv)
{
	// Validate the parameters
	// Note: The 1st parameter is the full path to the application
	if (argc != 2) {
		printf("Expected 1 parameter, but received %d\nUsage: app IpAddress\n", (argc - 1));
		return 1;
	}

	// Send an IPv4 ICMP echo request
	(void)TryIcmpSendEcho_v1(argv[1], 4);
	//(void)TryIcmpSendEcho_v1("192.168.2.192", 10);

	// Host names are not implemented yet...
	//(void)TryIcmpSendEcho_v1("www.google.com");

	return 0;
}
