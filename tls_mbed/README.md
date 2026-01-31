# AdaModbus TLS (Embedded)

TLS transport using mbedTLS for MCU targets.

## Usage

```bash
cd tls_mbed
alr pin adamodbus --use=..
alr build
```

## Features

- Client and Server modes
- Certificate and PSK authentication
- Hardware RNG integration
- Session resumption
- ZFP/Light runtime compatible

## API

```ada
with Ada_Modbus.Transport.TLS_Mbed;

--  Client
Connect (Connection, "192.168.1.100", 802, Config, Result);

--  Server
Listen (Server, 802, Config, Result);
Accept_Connection (Server, Connection, Timeout, Result);
```

## Configuration

```ada
--  Certificate mode
Config := (
   Mode            => Auth_Certificate,
   CA_Certificate  => CA_Cert'Address,
   CA_Cert_Len     => CA_Cert'Length,
   Verify_Peer     => True,
   others          => <>
);

--  PSK mode
Config := (
   Mode         => Auth_PSK,
   PSK          => Key'Address,
   PSK_Len      => Key'Length,
   PSK_Identity => Identity'Address,
   PSK_Id_Len   => Identity'Length,
   others       => <>
);
```

## Memory

- ~50-80 KB Flash
- ~15-20 KB RAM per session
