#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Header files needed for socket programming
#include <netinet/in.h>
#include <sys/socket.h>
#include <sys/types.h>

int main() {
  // Open index.html
  FILE *html_data;
  html_data = fopen("index.html", "r");

  // Read the contents for index.html
  char response_data[1024];
  fgets(response_data, 1024, html_data);

  // Concatenate HTTP header with content of index.html
  char http_header[2048] = "HTTP/1.1 200 OK\r\n\n";
  strcat(http_header, response_data);

  // Create a socket
  int server_socket = socket(AF_INET, SOCK_STREAM, 0);

  // Define the address
  struct sockaddr_in server_address;
  server_address.sin_family = AF_INET;
  server_address.sin_port = htons(9000);
  server_address.sin_addr.s_addr = INADDR_ANY;

  // Bind the address to the socket
  bind(server_socket, (struct sockaddr *)&server_address,
       sizeof(server_address));

  listen(server_socket, 5);

  // Define and accept the client. Then respond with the http_header
  int client_socket;
  while (1) {
    client_socket = accept(server_socket, NULL, NULL);
    send(client_socket, http_header, sizeof(http_header), 0);
    close(client_socket);
  }

  return 0;
}
