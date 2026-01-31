# AdaModbus TLS (Desktop)

TLS transport using AWS (Ada Web Server) with OpenSSL backend.

## Usage

```bash
cd tls
alr pin adamodbus --use=..
alr build
```

## API

```ada
with Ada_Modbus.Transport.TLS;

Connect (Connection, "192.168.1.100", 802, Config, Result);
Send_Frame (Connection, Request, Result);
Receive_Frame (Connection, Response, Length, Timeout, Result);
Disconnect (Connection);
```

## Configuration

```ada
Config := (
   CA_File     => "ca.crt",
   Cert_File   => "client.crt",
   Key_File    => "client.key",
   Verify_Peer => True
);
```

## Requirements

- AWS (Ada Web Server)
- OpenSSL
