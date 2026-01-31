# AdaModbus TLS (Desktop)

TLS transport for desktop systems using AWS (Ada Web Server) with OpenSSL.

## Building

```bash
cd tls
alr pin adamodbus --use=..
alr build
```

## Usage

```ada
with Ada_Modbus.Transport.TLS;

Config := (
   CA_File     => "ca.crt",
   Cert_File   => "client.crt",
   Key_File    => "client.key",
   Verify_Peer => True
);

Connect (Connection, "192.168.1.100", 802, Config, Result);
Send_Frame (Connection, Request, Result);
Receive_Frame (Connection, Response, Length, Timeout, Result);
Disconnect (Connection);
```

## Requirements

- AWS (Ada Web Server)
- OpenSSL
