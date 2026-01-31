# AdaModbus TLS (Embedded)

TLS transport for embedded systems using mbedTLS.

## Building

```bash
cd tls_mbed
alr pin adamodbus --use=..
alr build
```

## Features

- Client and server modes
- Certificate-based and PSK authentication
- Hardware RNG integration for true entropy
- TLS session resumption for fast reconnects
- ZFP/Light runtime compatible

## Usage

```ada
with Ada_Modbus.Transport.TLS_Mbed;

--  Certificate authentication
Config := (
   Mode            => Auth_Certificate,
   CA_Certificate  => CA_Cert'Address,
   CA_Cert_Len     => CA_Cert'Length,
   Verify_Peer     => True,
   others          => <>
);

--  Or PSK authentication
Config := (
   Mode         => Auth_PSK,
   PSK          => Key'Address,
   PSK_Len      => Key'Length,
   PSK_Identity => Identity'Address,
   PSK_Id_Len   => Identity'Length,
   others       => <>
);

--  Client
Connect (Connection, "192.168.1.100", 802, Config, Result);

--  Server
Listen (Server, 802, Config, Result);
Accept_Connection (Server, Connection, Timeout, Result);
```

## Memory Usage

Approximately 50-80 KB Flash and 15-20 KB RAM per TLS session.
